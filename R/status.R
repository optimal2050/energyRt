# =============================================================================#
# status.R — what state a saved object is in.
#
# Seal and mark state could be written for every saved object but never read
# back: `seal_scenario()`, `mark_delete()` and their siblings had no reader, so
# "is this sealed, is it marked, and can I unseal or delete it?" meant opening
# YAML by hand or reading aggregate counts off `summary(load_registry())`.
#
# The state lives in the manifests, and this reads them. Not the registry: the
# CSV caches `sealed` and `marked_delete` but not `outputs_sealed`, and it
# holds no lineage at all, so two of the questions it cannot answer.
#
# Lineage is the part that makes the answer actionable. A scenario carries two
# independent records of where it came from -- `base:`, which is provenance,
# and one `datasets:` entry per table it actually borrows, which is the
# coupling. Only the second becomes a dependency edge (.dep_edges()), so only
# the second decides whether unsealing or deleting the base is refused. A
# scenario built on a base whose tables all fell under .param_ref_min_rows()
# has the first and not the second, and nothing about it is at risk.
# =============================================================================#

#' @include save-objects.R save-scenario.R registry.R utils.R
NULL

.status_cols <- function() {
  tibble(type = character(0), name = character(0),
         sealed = logical(0), outputs_sealed = logical(0),
         sealed_at = character(0),
         marked = logical(0), importance = numeric(0),
         marked_at = character(0),
         runs = integer(0), solved = integer(0),
         size_mb = numeric(0), format = character(0), ondisk = logical(0),
         base = character(0), shares = integer(0), used_by = integer(0),
         lineage_warn = character(0), path = character(0))
}

# Runs of a stored scenario, counted from `run.yml` rather than from a loaded
# object. `scenario_runs()` needs a `scenario`, and loading every one of them
# to count its runs would cost far more than the store walk itself. Both
# layouts .run_dirs() knows are covered: runs/<solve>/ and
# runs/<variant>/<solve>/.
.status_runs_at <- function(path) {
  root <- fp(path, "runs")
  if (!dir.exists(root)) return(c(runs = 0L, solved = 0L))
  ry <- character(0)
  for (d1 in list.dirs(root, recursive = FALSE)) {
    if (file.exists(fp(d1, "run.yml"))) {
      ry <- c(ry, fp(d1, "run.yml"))
      next
    }
    for (d2 in list.dirs(d1, recursive = FALSE)) {
      if (file.exists(fp(d2, "run.yml"))) ry <- c(ry, fp(d2, "run.yml"))
    }
  }
  if (!length(ry)) return(c(runs = 0L, solved = 0L))
  st <- vapply(ry, function(f) {
    r <- tryCatch(yaml::read_yaml(f), error = function(e) list())
    as.character(r$status %||% "unknown")
  }, character(1), USE.NAMES = FALSE)
  c(runs = length(st), solved = sum(st == "solved"))
}

# One pass over the four store roots, reading each manifest once. Both the
# lifecycle fields and the reference edges come off that one read;
# `.store_dep_index()` makes the same walk for the edges alone, so calling it
# as well would read every manifest twice.
.status_walk <- function() {
  kinds <- .entry_kinds()
  out <- list()
  for (tp in names(kinds)) {
    root <- tryCatch(kinds[[tp]]$root(), error = function(e) "")
    if (!length(root) || !nzchar(root) || !dir.exists(root)) next
    for (d in list.dirs(root, recursive = FALSE)) {
      mf <- tryCatch(yaml::read_yaml(fp(d, kinds[[tp]]$manifest)),
                     error = function(e) NULL)
      if (is.null(mf)) next
      out[[length(out) + 1L]] <- list(
        type = tp, name = as.character(mf$name %||% basename(d)),
        path = .fix_path(d), mf = mf)
    }
  }
  out
}

# Every reference edge in the project, from a walk already made.
.status_edges <- function(walk) {
  rows <- list()
  for (w in walk) {
    for (e in .dep_edges(w$type, w$mf)) {
      rows[[length(rows) + 1L]] <- tibble(
        type = w$type, name = w$name, via = as.character(e$via),
        target = as.character(e$target))
    }
  }
  if (!length(rows)) {
    tibble(type = character(0), name = character(0), via = character(0),
           target = character(0))
  } else {
    bind_rows(rows)
  }
}

# One status row from an already-read manifest.
#
# The lifecycle fields go through .registry_lifecycle() rather than being read
# raw, because `unmark_delete()` writes only `marked_delete = FALSE` and leaves
# `delete_importance` and `marked_at` behind in the YAML. The registry masks
# that by NA-ing the importance of an unmarked entry; reading raw here would
# make this verb contradict load_registry() on the same object.
.status_row <- function(type, name, path, mf, edges = NULL, known = NULL,
                        sizes = FALSE) {
  lc <- .registry_lifecycle(mf)
  rr <- .status_runs_at(path)
  # the tables this entry borrows: the coupling, and what a refusal is built
  # from. `base:` alone is provenance.
  borrowed <- Filter(function(e) {
    identical(as.character(e$source %||% ""), "scenario") &&
      nzchar(as.character(e$scenario %||% ""))
  }, mf$datasets %||% list())
  from <- unique(vapply(borrowed, function(e) as.character(e$scenario),
                        character(1), USE.NAMES = FALSE))
  base_nm <- as.character(mf$base$name %||% "")

  lineage <- !is.null(edges)
  # who depends on THIS entry: the edge's `via` is the kind of thing it points
  # at, so a model's dependents are the scenarios built on it, a repository's
  # are the models holding it, and a scenario's are the scenarios borrowing its
  # tables. The same test .dep_of() applies, so this agrees with
  # list_dependents() for every kind rather than only for scenarios.
  used <- if (!lineage || !nrow(edges)) character(0) else {
    unique(edges$name[edges$via == type & edges$target == name &
                        edges$name != name])
  }
  warn <- character(0)
  if (lineage && length(from)) {
    gone <- setdiff(from, known)
    if (length(gone)) {
      warn <- c(warn, paste0("borrows tables from a missing scenario: ",
                             .preview_chr(gone)))
    }
  }
  if (lineage && nzchar(base_nm) && !base_nm %in% known) {
    warn <- c(warn, "base folder missing")
  }

  tibble(
    type = type, name = name,
    sealed = lc$sealed,
    outputs_sealed = if (identical(type, "scenario")) {
      .scenario_seal_state(mf)$outputs
    } else {
      NA
    },
    sealed_at = lc$sealed_at,
    marked = lc$marked_delete,
    importance = lc$delete_importance,
    marked_at = lc$marked_at,
    runs = as.integer(rr[["runs"]]),
    solved = as.integer(rr[["solved"]]),
    size_mb = if (isTRUE(sizes)) {
      round(dir_size(path, missing = "zero") / 1024^2, 2)
    } else {
      NA_real_
    },
    format = as.character(mf$format %||% ""),
    # On-disk or in-memory, read from the store rather than from the object: an
    # on-disk scenario keeps its parameter tables under modInp/parameters/,
    # while an in-memory one carries them inside scen.RData. isOnDisk() agrees
    # where both can be asked, but it needs the loaded object, so it cannot
    # answer for a name or during the project walk -- which is the whole point
    # of reading it here.
    ondisk = dir.exists(fp(path, "modInp", "parameters")),
    base = base_nm,
    shares = length(borrowed),
    used_by = if (lineage) length(used) else NA_integer_,
    lineage_warn = paste(warn, collapse = "; "),
    path = path)
}

# The edge table and the known scenario names, or NULLs when lineage is off.
.status_context <- function(lineage) {
  if (!isTRUE(lineage)) return(list(edges = NULL, known = NULL))
  walk <- .status_walk()
  list(edges = .status_edges(walk), walk = walk,
       known = vapply(Filter(function(w) identical(w$type, "scenario"), walk),
                      function(w) w$name, character(1), USE.NAMES = FALSE))
}

#' Seal, mark, runs, storage and lineage of a saved object
#'
#' @description
#' What state a saved object is in: whether it is sealed, whether it is queued
#' for deletion, what runs it holds, what it costs on disk, and what it shares
#' with other scenarios. Reads the manifests, so the answer does not depend on
#' the registry being up to date.
#'
#' @details
#' With an object, a name, or nothing:
#'
#' * `get_status(scen)`, `get_status(mod)`, `get_status(repo)` — that object.
#' * `get_status("REF_sub")`, `get_status("merra_cf", type = "dataset")` — the
#'   named entry, resolved in its store. A bare name present in more than one
#'   store is refused rather than guessed; name the `type`.
#' * `get_status()` — every saved model, repository, dataset and scenario.
#'
#' A **run** carries no seal and no deletion mark — those verbs act on the four
#' entry kinds, not on a run — so runs appear as the `runs` and `solved` counts
#' of the scenario that holds them.
#'
#' @section Lineage:
#' A derived scenario records its origin twice, and the two can disagree.
#' `base` is the name from `base:` in the manifest: **provenance**, written by
#' `interpolate_model(base = )`. `shares` counts the tables it actually borrows
#' from another scenario: the **coupling**, and the only one of the two that
#' blocks [unseal_scenario()] or deletion.
#'
#' `used_by` is the other direction, for every kind of entry and not only
#' scenarios -- the scenarios built on a model, the models holding a
#' repository, the scenarios borrowing a scenario's tables. It counts the same
#' edges [list_dependents()] walks, so a non-zero `used_by` is what a refusal
#' will cite.
#'
#' `base` set with `shares == 0` is therefore the ordinary safe case — the base
#' was not sealed when the derivation was built, or every table fell under
#' `getOption("energyRt.param_ref_min_rows")` and was copied. `lineage_warn`
#' is for the cases that are not safe: a borrowed table whose source scenario
#' is gone, or a recorded base that no longer exists.
#'
#' @param x a saved object, a name, or `NULL` for every saved object.
#' @param type `"model"`, `"repository"`, `"dataset"` or `"scenario"`, when `x`
#'   is a name that needs disambiguating. A dataset is always named this way:
#'   [load_dataset()] returns an unclassed payload, so there is nothing to
#'   dispatch on.
#' @param sizes measure folder sizes with [dir_size()]. Defaults to `TRUE` for
#'   one object and `FALSE` for a whole project, where walking a large store
#'   would dominate the call.
#' @param lineage resolve `used_by` and check for broken references. Needs the
#'   store walk; with `FALSE`, `used_by` is `NA` and nothing is checked.
#' @param ... unused.
#'
#' @return A tibble of class `en_status`, one row per object.
#' @seealso [seal_scenario()], [mark_delete()], [delete_marked()],
#'   [list_dependents()], [load_registry()]
#' @export
#' @examples
#' \dontrun{
#' get_status()                              # the whole project
#' get_status(scen)
#' get_status("merra_cf", type = "dataset")
#'
#' st <- get_status()
#' st[st$marked, c("type", "name", "importance")]   # what delete_marked() sees
#' st[nzchar(st$lineage_warn), ]                    # broken references
#' }
get_status <- function(x = NULL, type = NULL, sizes = NULL, lineage = TRUE,
                       ...) {
  # `get_status(sizes = TRUE)` leaves `x` missing, and UseMethod() dispatches a
  # missing first argument to the default method rather than on its NULL
  # default -- so route the whole-project form here instead of relying on it.
  if (missing(x) || is.null(x)) {
    return(get_status.NULL(NULL, type = type, sizes = sizes,
                           lineage = lineage, ...))
  }
  UseMethod("get_status")
}

#' @rdname get_status
#' @export
get_status.NULL <- function(x = NULL, type = NULL, sizes = NULL,
                            lineage = TRUE, ...) {
  sizes <- sizes %||% FALSE
  walk <- .status_walk()
  if (!length(walk)) return(.status_class(.status_cols()))
  edges <- if (isTRUE(lineage)) .status_edges(walk) else NULL
  known <- if (isTRUE(lineage)) {
    vapply(Filter(function(w) identical(w$type, "scenario"), walk),
           function(w) w$name, character(1), USE.NAMES = FALSE)
  }
  if (!is.null(type)) {
    walk <- Filter(function(w) w$type %in% type, walk)
    if (!length(walk)) return(.status_class(.status_cols()))
  }
  rows <- lapply(walk, function(w) {
    .status_row(w$type, w$name, w$path, w$mf, edges = edges, known = known,
                sizes = sizes)
  })
  out <- bind_rows(rows)
  out <- out[order(match(out$type, names(.entry_kinds())), out$name), ,
             drop = FALSE]
  .status_class(out)
}

# The shared tail of every single-object method.
.status_one <- function(type, name, path, sizes, lineage) {
  mf <- .entry_manifest(type, path)
  ctx <- .status_context(lineage)
  .status_class(.status_row(type, name, path, mf, edges = ctx$edges,
                            known = ctx$known, sizes = sizes %||% TRUE))
}

#' @rdname get_status
#' @export
get_status.scenario <- function(x, type = NULL, sizes = NULL, lineage = TRUE,
                                ...) {
  e <- .entry_resolve("scenario", x)
  .status_one("scenario", e$name, e$path, sizes, lineage)
}

#' @rdname get_status
#' @export
get_status.model <- function(x, type = NULL, sizes = NULL, lineage = TRUE,
                             ...) {
  e <- .entry_resolve("model", x)
  .status_one("model", e$name, e$path, sizes, lineage)
}

#' @rdname get_status
#' @export
get_status.repository <- function(x, type = NULL, sizes = NULL,
                                  lineage = TRUE, ...) {
  e <- .entry_resolve("repository", x)
  .status_one("repository", e$name, e$path, sizes, lineage)
}

#' @rdname get_status
#' @export
get_status.character <- function(x, type = NULL, sizes = NULL, lineage = TRUE,
                                 ...) {
  if (length(x) != 1L) {
    stop("get_status() takes one name at a time; call get_status() with no ",
         "argument for every saved object.", call. = FALSE)
  }
  kinds <- names(.entry_kinds())
  if (!is.null(type)) {
    type <- match.arg(type, kinds)
    e <- .entry_resolve(type, x)
    return(.status_one(type, e$name, e$path, sizes, lineage))
  }
  # a bare name: look in every store and refuse an ambiguous answer rather
  # than report whichever kind happens to be checked first
  hit <- Filter(Negate(is.null), lapply(kinds, function(tp) {
    p <- tryCatch(.entry_resolve(tp, x), error = function(err) NULL)
    if (is.null(p)) NULL else list(type = tp, name = p$name, path = p$path)
  }))
  if (!length(hit)) {
    stop("'", x, "' was not found in any store (model, repository, dataset ",
         "or scenario). Run refresh_registry() to rescan, or save it first.",
         call. = FALSE)
  }
  if (length(hit) > 1L) {
    stop("'", x, "' names ",
         paste(vapply(hit, function(h) h$type, character(1)),
               collapse = " and a "),
         ". Name one with get_status(\"", x, "\", type = ).", call. = FALSE)
  }
  h <- hit[[1]]
  .status_one(h$type, h$name, h$path, sizes, lineage)
}

#' @rdname get_status
#' @export
get_status.default <- function(x, type = NULL, sizes = NULL, lineage = TRUE,
                               ...) {
  stop("get_status() takes a model, repository or scenario object, a name, ",
       "or nothing for the whole project; got ", class(x)[1],
       ". A dataset is named: get_status(\"<name>\", type = \"dataset\").",
       call. = FALSE)
}

.status_class <- function(x) {
  structure(x, class = unique(c("en_status", class(x))))
}

# "sealed", "inputs only", "open" -- a scenario's two flags read as one state,
# since outputs can never be sealed while the inputs are open
# (.scenario_seal_state()).
.status_seal_label <- function(sealed, outputs) {
  if (!isTRUE(sealed)) return("open")
  if (is.na(outputs)) return("sealed")
  if (isTRUE(outputs)) "sealed (inputs + outputs)" else "sealed (inputs only)"
}

#' @rdname get_status
#' @method print en_status
#' @export
print.en_status <- function(x, ...) {
  if (!nrow(x)) {
    cat("No saved objects in this project.\n")
    return(invisible(x))
  }
  if (nrow(x) == 1L) {
    cat(x$type, " '", x$name, "'\n", sep = "")
    cat("  seal      ", .status_seal_label(x$sealed, x$outputs_sealed),
        if (nzchar(x$sealed_at)) paste0("  ", x$sealed_at) else "", "\n",
        sep = "")
    cat("  marked    ",
        if (isTRUE(x$marked)) {
          paste0("yes (importance ", x$importance, ")",
                 if (nzchar(x$marked_at)) paste0("  ", x$marked_at) else "")
        } else {
          "no"
        }, "\n", sep = "")
    if (identical(x$type, "scenario")) {
      cat("  runs      ", x$runs, " (", x$solved, " solved)\n", sep = "")
    }
    cat("  storage   ",
        if (is.na(x$size_mb)) "not measured (sizes = TRUE)" else
          paste0(format(x$size_mb, big.mark = ","), " MB"),
        if (nzchar(x$format)) paste0("  ", x$format) else "",
        if (isTRUE(x$ondisk)) "  on disk" else "", "\n", sep = "")
    if (nzchar(x$base) || x$shares > 0 || isTRUE(x$used_by > 0)) {
      if (nzchar(x$base)) cat("  base      ", x$base, "\n", sep = "")
      # "provenance only" is a statement about a RECORDED base whose tables
      # were copied. An entry with no base and nothing borrowed has no such
      # line to print.
      if (x$shares > 0) {
        cat("  shares    ", x$shares, " table(s) borrowed\n", sep = "")
      } else if (nzchar(x$base)) {
        cat("  shares    none - provenance only, nothing is coupled\n")
      }
      if (!is.na(x$used_by)) {
        cat("  used by   ", x$used_by, " scenario(s)",
            if (x$used_by > 0) " - blocks unseal and delete" else "", "\n",
            sep = "")
      }
    }
    if (nzchar(x$lineage_warn)) cat("  ! ", x$lineage_warn, "\n", sep = "")
    return(invisible(x))
  }

  cat("energyRt saved objects: ", nrow(x), "\n", sep = "")
  d <- data.frame(
    type = x$type, name = x$name,
    seal = vapply(seq_len(nrow(x)),
                  function(i) .status_seal_label(x$sealed[i],
                                                 x$outputs_sealed[i]),
                  character(1)),
    marked = ifelse(x$marked, paste0("yes (", x$importance, ")"), ""),
    runs = ifelse(x$runs > 0, paste0(x$solved, "/", x$runs), ""),
    base = x$base,
    shares = ifelse(x$shares > 0, as.character(x$shares), ""),
    used_by = ifelse(is.na(x$used_by) | x$used_by == 0, "",
                     as.character(x$used_by)),
    stringsAsFactors = FALSE)
  if (!all(is.na(x$size_mb))) d$size_mb <- x$size_mb
  # a column no row has anything to say in is padding, and padding is what
  # pushes the measured sizes onto a wrapped line
  keep <- vapply(d, function(v) any(nzchar(as.character(v))), logical(1))
  keep[c("type", "name", "seal")] <- TRUE
  print(d[, keep, drop = FALSE], row.names = FALSE)
  w <- which(nzchar(x$lineage_warn))
  for (i in w) {
    cat("! ", x$type[i], " '", x$name[i], "': ", x$lineage_warn[i], "\n",
        sep = "")
  }
  if (all(is.na(x$size_mb))) {
    cat("Sizes not measured; get_status(sizes = TRUE) walks the stores.\n")
  }
  # the manifests are the truth, so a behind registry is worth one line: other
  # readers of the CSV would still see the old state
  if (isTRUE(tryCatch(.registry_is_stale(), error = function(e) FALSE))) {
    cat("! the registry CSV is behind the stores - refresh_registry()\n")
  }
  invisible(x)
}

utils::globalVariables(c("outputs_sealed", "lineage_warn"))
