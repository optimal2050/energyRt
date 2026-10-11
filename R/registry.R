# =============================================================================#
# registry.R — persisted project registry of models, scenarios, and runs.
#
# One CSV file per project (default `energyRt_registry.csv` at the project
# root, a sibling of the `scenarios/` and `models/` stores) indexes what is
# saved on disk: one row per model, scenario, or run. The on-disk manifests
# (`scenario.yml`, `model.yml`, `run.yml`) are the source of truth; the CSV is
# a rebuildable index — `refresh_registry()` rescans the stores and reconciles.
#
# The previous implementation (a wrapper over the CRAN `registry` package,
# in-memory only) is archived in `drafts/registry-cran-wrapper.R`; its public
# names live on as deprecated shims at the bottom of this file.
# =============================================================================#

# Canonical column set. Character throughout for CSV round-trip stability,
# except the three lifecycle columns typed by `.registry_coerce()` — they are
# filtered on (`reg[reg$marked_delete, ]`), not merely displayed.
.registry_cols <- c(
  "type",             # "model" | "repository" | "scenario" | "run" | "dataset"
  "name",             # object name; runs: "<variant>/<solve>"
  "hash",             # model content hash (model rows), "" otherwise
  # A scenario's name may repeat across a project -- 48 countries can all be
  # called "BY25A_F4" -- so the model is what tells two rows apart by hand,
  # and what `getScenario(name, model =)` resolves on.
  "model",            # scenario rows: name of the referenced/embedded model
  "model_hash",       # scenario rows: hash of that model
  "path",             # relative to the registry file's directory
  "parent",           # runs: scenario name; "" otherwise
  "variant",          # runs: own-problem variant label; "" = base problem
  "created", "updated",  # UTC timestamps, "%Y-%m-%dT%H:%M:%SZ"
  "energyRt_version",
  # Lifecycle, mirrored from each entry's own manifest — which remains the
  # source of truth (`.entry_manifest_update()`). Cached here so the CSV
  # answers "sealed?" and "marked for deletion?" on its own, for any reader,
  # without walking a tree of YAML. `sealed` and `marked_delete` are
  # independent: nothing guards a mark on a sealed entry, which is why
  # `delete_marked()` has a `skipped_sealed` action.
  "sealed", "sealed_at",
  "marked_delete", "delete_importance", "marked_at",
  "status",           # run rows only: `run.yml` status
  "memo"
)

# Columns that are not character. Everything absent from this list is.
.registry_col_types <- list(
  sealed = logical(0), marked_delete = logical(0),
  delete_importance = numeric(0)
)

.registry_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Paths are compared, not displayed: separator form and a trailing slash are
# not differences, and on Windows case is not either. Used as the registry's
# upsert key, so two spellings of one folder must not become two rows.
.registry_path_key <- function(p) {
  p <- gsub("[/]+", "/", chartr("\\", "/", as.character(p)))
  p <- sub("/+$", "", p)
  if (.Platform$OS.type == "windows") tolower(p) else p
}

# a path relative to the registry file's directory when possible (portability)
.registry_rel_path <- function(p, reg_file = get_registry_file()) {
  p <- gsub("[\\/]+", "/", p)
  rn <- gsub("[\\/]+", "/", normalizePath(dirname(reg_file), winslash = "/",
                                          mustWork = FALSE))
  pn <- gsub("[\\/]+", "/", normalizePath(p, winslash = "/", mustWork = FALSE))
  if (startsWith(pn, paste0(rn, "/"))) substring(pn, nchar(rn) + 2L) else p
}

# Resolve a scenario NAME to its folder: registry row first (path relative
# to the registry file's directory), then a scan of the scenarios store for
# a manifest (or layout-2 class marker + dir name) matching the name.
# Scenarios are not content-addressed, so resolution is by name only; two
# candidate folders for one name is an error, never a silent pick.
.scenario_resolve <- function(name, registry = NULL, model = NULL) {
  is_scen_dir <- function(d) {
    file.exists(fp(d, "scenario.yml")) || file.exists(fp(d, "class"))
  }
  reg <- registry %||% tryCatch(load_registry(), error = function(e) NULL)
  if (!is.null(reg)) {
    hit <- find_in_registry(reg, type = "scenario", name = name)
    if (!is.null(model)) {
      hit <- hit[hit$model %in% as.character(model), , drop = FALSE]
    }
    if (nrow(hit)) {
      p <- gsub("[\\/]+", "/", fp(dirname(get_registry_file()), hit$path))
      # Rows whose folder is gone are not candidates -- a stale row must not
      # make a resolvable name look ambiguous.
      keep <- vapply(p, is_scen_dir, logical(1), USE.NAMES = FALSE)
      p <- p[keep]
      # Taking p[1] here is what rendered one country's report from another
      # country's folder: the name is a label and may repeat, so more than one
      # live candidate is a refusal, not a choice.
      if (length(p) > 1L) {
        mods <- unique(hit$model[keep])
        mods <- mods[nzchar(mods)]
        stop("Scenario '", name, "' matches ", length(p),
             " folders in the registry: ",
             .preview_chr(basename(p)),
             if (length(mods)) paste0("\n  models: ", .preview_chr(mods)),
             "\n  Name one with getScenario(..., model = ) or load it by ",
             "path with load_scenario().", call. = FALSE)
      }
      if (length(p) == 1L) return(p)
    }
  }
  root <- get_scenarios_path()
  if (!dir.exists(root)) return(NULL)
  cand <- character(0)
  for (d in list.dirs(root, recursive = FALSE)) {
    if (startsWith(basename(d), ".")) next
    mf <- fp(d, "scenario.yml")
    nm <- if (file.exists(mf)) {
      tryCatch(yaml::read_yaml(mf)$name %||% basename(d),
               error = function(e) NULL)
    } else if (file.exists(fp(d, "class"))) {
      basename(d)   # layout 2: no manifest, the folder name is the name
    } else {
      NULL
    }
    if (identical(nm, name)) cand <- c(cand, gsub("[\\/]+", "/", d))
  }
  if (length(cand) > 1L) {
    stop("Scenario '", name, "' matches ", length(cand), " folders in '",
         root, "': ", paste(basename(cand), collapse = ", "),
         ". Load one with an explicit path.")
  }
  if (length(cand) == 1L) cand else NULL
}

.registry_empty <- function() {
  .registry_class(as_tibble(setNames(
    lapply(.registry_cols, \(x) .registry_col_types[[x]] %||% character(0)),
    .registry_cols
  )))
}

# Type the lifecycle columns after a read. The CSV comes back all-character
# for round-trip stability, and an empty cell means "not marked" — so a column
# back-filled for an older registry must read FALSE, never NA, or every
# pre-existing project would look marked-or-unknown.
.registry_coerce <- function(reg) {
  for (nm in c("sealed", "marked_delete")) {
    v <- reg[[nm]]
    reg[[nm]] <- if (is.logical(v)) {
      !is.na(v) & v
    } else {
      toupper(trimws(as.character(v))) %in% c("TRUE", "T", "YES", "1")
    }
  }
  v <- trimws(as.character(reg[["delete_importance"]]))
  v[!nzchar(v)] <- NA_character_
  reg[["delete_importance"]] <- suppressWarnings(as.numeric(v))
  reg
}

# the registry tibble carries an S3 subclass so generics (`add()`) can
# dispatch on it; tibble/dplyr verbs drop extra classes, so every function
# returning a registry re-attaches it
.registry_class <- function(reg) {
  class(reg) <- unique(c("en_registry", class(reg)))
  reg
}

#' Project registry of models, scenarios, and runs
#'
#' @description
#' The registry is a per-project CSV file (see [get_registry_file()]) indexing
#' the saved models, scenarios, and runs: one row each, with type, name,
#' content hash (models), path, parent (runs), timestamps, and a memo.
#' It is a rebuildable index — the on-disk manifests are the source of truth,
#' and `refresh_registry()` reconstructs the registry by rescanning the
#' `scenarios/` and `models/` stores.
#'
#' * `newRegistry()` constructs a new, empty registry. Creating one is
#'   explicit: `load_registry()` does NOT invent a registry for you.
#' * `load_registry()` reads the registry file into a tibble, and errors when
#'   the file does not exist — naming `newRegistry()`. (Object stores create
#'   their own registry on first write, so `save_model()` and friends work in
#'   a fresh project without any ceremony.)
#' * `save_registry()` writes the tibble back.
#' * `add_to_registry()` inserts or updates one row (keyed by `type` + `name` +
#'   `parent`) and returns the updated tibble; it does not write the file.
#' * `find_in_registry()` filters by any combination of `type`, `name`, `hash`,
#'   and `parent` (exact matches; `NULL` = no filter).
#' * `refresh_registry()` rescans the stores under `root` and rebuilds rows
#'   from what is actually on disk, preserving `created`/`memo` of surviving
#'   entries; with `write = TRUE` (default) the result is saved.
#'
#' @param file character, path to the registry CSV
#'   (default [get_registry_file()]).
#' @param reg a registry tibble as returned by `load_registry()`.
#' @param type character: `"model"`, `"repository"`, `"scenario"`, or `"run"`.
#' @param name character, object name (for runs: `"<variant>/<solve>"`).
#' @param path character, object directory, relative to the registry file's
#'   directory.
#' @param hash character, model content hash (from `model_hash()`, arriving
#'   with the model store); `""` for scenario/run rows.
#' @param model_hash character, for scenario rows: hash of the referenced or
#'   embedded model.
#' @param parent character, for run rows: the scenario name.
#' @param memo character, optional free-form note.
#' @param root character, project root to rescan (the directory holding the
#'   `scenarios/` and `models/` stores).
#' @param write logical, save the refreshed registry to `file`.
#' @param lazy logical. `FALSE` (default) always rescans — a full scan is
#'   around 0.3 s for ~80 manifests, too cheap to risk serving a stale table.
#'   `TRUE` skips the scan when the registry is already up to date with the
#'   stores, for callers that refresh repeatedly. The check compares the CSV's
#'   own modification time, which changes without the content changing (a
#'   `git checkout`, a file copy, cloud sync, saving from Excel), so a registry
#'   copied in from elsewhere can read as fresh.
#' @param sealed,sealed_at,marked_delete,delete_importance,marked_at Lifecycle
#'   state, mirrored from the entry's own manifest — which stays the source of
#'   truth. Cached in the registry so the CSV answers "sealed?" and "marked
#'   for deletion?" on its own. Set by [seal_model()] and [mark_delete()]
#'   rather than passed by hand.
#' @param status character, for run rows: the solve status from `run.yml`.
#' @param variant character, for run rows: the variant the run belongs to.
#' @param obj the object a registry row is derived from.
#' @param ... further columns to set on the registry row.
#'
#' @return `load_registry()`, `add_to_registry()`, `find_in_registry()`, and
#'   `refresh_registry()` return the registry tibble; `save_registry()`
#'   returns `file` invisibly.
#'
#' @rdname registry
#' @export
#' @examples
#' reg <- newRegistry()
#' reg <- add_to_registry(reg, "scenario", "BASE", path = "scenarios/BASE")
#' find_in_registry(reg, type = "scenario")
newRegistry <- function() .registry_empty()

#' @rdname registry
#' @export
load_registry <- function(file = get_registry_file()) {
  if (!file.exists(file)) {
    stop("No registry at '", file, "'.\n",
         "  Create one with `newRegistry()` (then `save_registry()`), or ",
         "rebuild it from the stores with `refresh_registry()`.",
         call. = FALSE)
  }
  reg <- utils::read.csv(file, colClasses = "character",
                         stringsAsFactors = FALSE) |> as_tibble()
  missing_cols <- setdiff(.registry_cols, names(reg))
  for (m in missing_cols) reg[[m]] <- ""
  .registry_class(.registry_coerce(reg[, .registry_cols]))
}

# Load the registry, or start a fresh one when the project has none yet.
# The INTERNAL counterpart of load_registry(): the object stores register
# what they write, and must work in a project whose first save is happening
# right now. Public `load_registry()` stays strict on purpose — creating a
# registry is `newRegistry()`, never a side effect of reading.
#' @noRd
.registry_open <- function(file = get_registry_file()) {
  if (!file.exists(file)) return(.registry_empty())
  load_registry(file)
}

#' @rdname registry
#' @export
save_registry <- function(reg, file = get_registry_file()) {
  stopifnot(is.data.frame(reg))
  reg <- as_tibble(reg)[, .registry_cols]
  dir_ <- dirname(file)
  if (!dir.exists(dir_)) dir.create(dir_, recursive = TRUE)
  utils::write.csv(reg, file, row.names = FALSE, na = "")
  invisible(file)
}

#' @rdname registry
#' @export
add_to_registry <- function(reg, type, name, path,
                         hash = "", model = "", model_hash = "", parent = "",
                         variant = "", memo = "",
                         sealed = FALSE, sealed_at = "",
                         marked_delete = FALSE,
                         delete_importance = NA_real_, marked_at = "",
                         status = "") {
  type <- match.arg(type, c("model", "repository", "scenario", "run",
                            "dataset"))
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  now <- .registry_now()
  # One row per DIRECTORY, keyed on (type, path).
  #
  # The key used to be (type, name, parent, variant), which cannot represent a
  # name that repeats: a scenario's folder is {scenario}-{model}-{calendar}-
  # {horizon} while its @name is just {scenario}, so 48 countries saved as
  # "BY25A_F4" collapsed into one row whose path each save overwrote, and a
  # later lookup by name loaded whichever folder had been written last. Run
  # rows collapsed with them, keyed on `parent = scen@name`.
  #
  # `path` is unique by construction, and for models, repositories and
  # datasets it is 1:1 with the name anyway (a store entry updates in place
  # and an occupied name is left alone), so for those this is a no-op.
  ii <- which(reg$type == type & .registry_path_key(reg$path) ==
                .registry_path_key(path))
  row <- tibble(
    type = type, name = name, hash = hash,
    model = as.character(model), model_hash = model_hash,
    path = path, parent = parent, variant = variant,
    created = if (length(ii)) reg$created[ii[1]] else now,
    updated = now,
    energyRt_version = as.character(utils::packageVersion("energyRt")),
    # lifecycle is never carried from the previous row: the manifest on disk
    # is the authority, so a rescan that no longer sees a mark must clear it
    sealed = isTRUE(sealed), sealed_at = as.character(sealed_at),
    marked_delete = isTRUE(marked_delete),
    delete_importance = as.numeric(delete_importance),
    marked_at = as.character(marked_at),
    status = as.character(status),
    memo = if (nzchar(memo) || !length(ii)) memo else reg$memo[ii[1]]
  )
  if (length(ii)) reg <- reg[-ii, ]
  .registry_class(bind_rows(reg, row))
}

#' @details
#' `add()` dispatches on a loaded registry, so `add(reg, "scenario", "BASE",
#' path = "scenarios/BASE")` is `add_to_registry()` under the generic shared
#' with models and repositories.
#'
#' @rdname registry
#' @method add en_registry
#' @export
add.en_registry <- function(obj, ...) add_to_registry(obj, ...)

#' @details
#' `summary()` reports the registry rather than listing it: counts by type,
#' how many entries are sealed or queued for deletion, how the runs ended,
#' and — because the lifecycle columns are a cache of each entry's manifest —
#' a warning when the stores have moved on since the file was written.
#'
#' @param object a loaded registry.
#' @param ... unused.
#' @rdname registry
#' @method summary en_registry
#' @export
summary.en_registry <- function(object, ...) {
  f <- get_registry_file()
  cat("energyRt registry: ", f, "\n", sep = "")
  tb <- table(factor(object$type,
                     levels = c("model", "repository", "scenario", "run",
                                "dataset")))
  tb <- tb[tb > 0]
  cat("Entries: ", nrow(object),
      if (length(tb)) paste0("  (", paste(names(tb), unname(tb),
                                          collapse = ", "), ")") else "",
      "\n", sep = "")

  n_seal <- sum(object$sealed, na.rm = TRUE)
  mk <- which(object$marked_delete)
  if (n_seal || length(mk)) {
    imp <- object$delete_importance[mk]
    it <- table(ifelse(is.na(imp), "unset", as.character(imp)))
    cat("Lifecycle: ", n_seal, " sealed, ", length(mk),
        " marked for deletion",
        if (length(mk)) paste0(" (importance ",
                               paste(names(it), unname(it), sep = ": ",
                                     collapse = ", "), ")") else "",
        "\n", sep = "")
  }

  rs <- object$status[object$type == "run" & nzchar(object$status)]
  if (length(rs)) {
    rt <- table(rs)
    cat("Runs: ", paste(unname(rt), names(rt), collapse = ", "), "\n", sep = "")
  }

  # the lifecycle columns are a cache; say so when it is behind
  if (isTRUE(tryCatch(.registry_is_stale(f), error = function(e) FALSE))) {
    cat("! the stores have changed since this registry was written",
        " — refresh_registry()\n", sep = "")
  }
  invisible(as_tibble(list(type = names(tb), n = unname(as.integer(tb)))))
}

#' @rdname registry
#' @export
find_in_registry <- function(reg, type = NULL, name = NULL, hash = NULL,
                          parent = NULL, variant = NULL) {
  if (!is.null(type)) reg <- reg[reg$type %in% type, ]
  if (!is.null(name)) reg <- reg[reg$name %in% name, ]
  if (!is.null(hash)) {
    # accept full or short (prefix) hashes
    reg <- reg[startsWith(reg$hash, hash[1]) & nzchar(reg$hash), ]
  }
  if (!is.null(parent)) reg <- reg[reg$parent %in% parent, ]
  if (!is.null(variant) && !is.null(reg[["variant"]])) {
    reg <- reg[reg$variant %in% variant, ]
  }
  .registry_class(reg)
}

# An absolute path: a drive letter, a leading slash, or a UNC backslash. Built
# from the character code so this source carries no escape sequences.
.registry_is_abs <- function(p) {
  bs <- rawToChar(as.raw(92L))
  grepl("^[A-Za-z]:", p) | startsWith(p, "/") | startsWith(p, bs)
}

# Store roots from the options may be absolute (e.g. redirected to a temp
# dir); only prefix `root` onto relative ones.
.registry_root_join <- function(p, root = ".") {
  if (.registry_is_abs(p)) gsub("[/]+", "/", chartr("\\", "/", p)) else fp(root, p)
}

# Every manifest the registry is built from, as absolute paths. Directory
# listings and globs only — no YAML is parsed, which is what keeps the
# freshness check sub-millisecond against ~0.3 s for a full rescan.
.registry_manifest_paths <- function(root = ".") {
  out <- character(0)
  scen_root <- .registry_root_join(get_scenarios_path(), root)
  if (dir.exists(scen_root)) {
    for (d in list.dirs(scen_root, recursive = FALSE)) {
      # dot-prefixed dirs are scratch by convention, as in the scan below
      if (startsWith(basename(d), ".")) next
      # layout 3 has scenario.yml; layout 2 only the `class` marker
      mk <- if (file.exists(fp(d, "scenario.yml"))) "scenario.yml" else "class"
      if (file.exists(fp(d, mk))) out <- c(out, fp(d, mk))
      out <- c(out, Sys.glob(fp(d, "runs", "*", "run.yml")),
               Sys.glob(fp(d, "runs", "*", "*", "run.yml")))
    }
  }
  for (st in list(c(get_models_path(), "model.yml"),
                  c(get_repositories_path(), "repository.yml"),
                  c(get_datasets_path(), "dataset.yml"))) {
    r <- .registry_root_join(st[1], root)
    if (!dir.exists(r)) next
    for (d in list.dirs(r, recursive = FALSE)) {
      if (file.exists(fp(d, st[2]))) out <- c(out, fp(d, st[2]))
    }
  }
  gsub("[/]+", "/", chartr("\\", "/", out))
}

# The lifecycle fields of one manifest, as `add_to_registry()` arguments.
# NULL-safe, because a layout-2 scenario folder has only a `class` marker and
# no manifest to read. `delete_importance` is NA unless the entry is actually
# marked — 0 would read as "marked at the least valuable tier".
.registry_lifecycle <- function(mf) {
  list(sealed = isTRUE(mf$sealed),
       sealed_at = as.character(mf$sealed_at %||% ""),
       marked_delete = isTRUE(mf$marked_delete),
       delete_importance = if (isTRUE(mf$marked_delete)) {
         as.numeric(mf$delete_importance %||% 0)
       } else {
         NA_real_
       },
       marked_at = as.character(mf$marked_at %||% ""))
}

# Mirror one entry's lifecycle into its registry row, straight after a mark or
# a seal, so `load_registry()` does not lag until the next rescan. Called by
# `.mark_set()` and `.seal_set()`.
#
# Writing the CSV here also leaves it newer than the manifest that just
# changed, which is exactly the `refresh_registry(lazy = TRUE)` freshness
# condition — so this keeps that check consistent rather than defeating it.
#
# A project with no registry yet is left alone: creating one is `newRegistry()`,
# never a side effect of marking something.
.registry_sync_entry <- function(type, path, name) {
  f <- get_registry_file()
  if (!file.exists(f)) return(invisible(FALSE))
  reg <- tryCatch(load_registry(f), error = function(e) NULL)
  if (is.null(reg)) return(invisible(FALSE))
  mf <- .entry_manifest(type, path)
  reg <- do.call(add_to_registry, c(
    list(reg, type, name, path = .registry_rel_path(path, reg_file = f),
         hash = as.character(mf$hash %||% ""),
         model = as.character(mf$model$name %||% "")[1],
         model_hash = as.character(mf$model$hash %||% "")),
    .registry_lifecycle(mf)))
  ok <- tryCatch({
    save_registry(reg, f)
    TRUE
  }, error = function(e) {
    # the manifest is already written and is the source of truth, so this is a
    # stale cache rather than lost state -- say which verb fixes it
    warning("Written to the manifest, but the registry could not be updated (",
            conditionMessage(e), ") - run refresh_registry().", call. = FALSE)
    FALSE
  })
  invisible(ok)
}

# Is the registry CSV behind the stores? Used by `refresh_registry(lazy =
# TRUE)` and by the drift line in `summary()`; never by the default refresh.
#
# The path-set comparison (step 2) is not redundant with the mtimes. Modified
# time alone cannot see a DELETED entry — nothing is newer, so the CSV keeps
# listing a folder that is gone — nor a RENAMED or copied directory, because a
# move preserves the manifest's mtime and the entry appears with an old
# timestamp.
#
# `>=` rather than `>`: some filesystems carry 1-2 s timestamp granularity, so
# a manifest written in the same second as the CSV would read as older.
#
# Limitation, which is why this is opt-in: the comparison is against the CSV's
# OWN mtime, and that changes without the content changing (git checkout, file
# copy, cloud sync, saving from Excel). A CSV copied from another machine looks
# newer than every manifest and so reads as fresh. Making this the default
# would mean replacing the comparison with a stored scan fingerprint.
.registry_is_stale <- function(file = get_registry_file(), root = ".") {
  if (!file.exists(file)) return(TRUE)
  mf <- .registry_manifest_paths(root)
  reg <- tryCatch(load_registry(file), error = function(e) NULL)
  if (is.null(reg)) return(TRUE)
  # .registry_rel_path() takes one path at a time (its startsWith test sits in
  # an `if`), so map rather than pass the vector
  on_disk <- unique(vapply(dirname(mf), .registry_rel_path, "",
                           reg_file = file, USE.NAMES = FALSE))
  known <- unique(reg$path[nzchar(reg$path)])
  if (length(setdiff(on_disk, known)) || length(setdiff(known, on_disk))) {
    return(TRUE)
  }
  if (!length(mf)) return(FALSE)
  any(file.mtime(mf) >= file.mtime(file))
}

#' @rdname registry
#' @export
refresh_registry <- function(root = ".", file = get_registry_file(),
                             write = TRUE, lazy = FALSE) {
  if (isTRUE(lazy) && !.registry_is_stale(file, root)) {
    if (isVerbose()) {
      message("Registry is up to date with the stores; not rescanned ",
              "(lazy = TRUE).")
    }
    return(.registry_open(file))
  }
  old <- .registry_open(file)
  rel_to_reg <- function(p) .registry_rel_path(p, reg_file = file)
  root_join <- function(p) .registry_root_join(p, root)
  # Carry `created` and `memo` forward from the previous registry. Matched on
  # PATH, like the upsert: matching on name would hand one folder's created
  # date and memo to another once a name repeats.
  keep_created <- function(reg, type, path) {
    k <- .registry_path_key(path)
    hit <- old[old$type == type & .registry_path_key(old$path) == k, ,
               drop = FALSE]
    if (nrow(hit)) {
      ii <- which(reg$type == type & .registry_path_key(reg$path) == k)
      reg$created[ii] <- hit$created[1]
      if (!nzchar(reg$memo[ii])) reg$memo[ii] <- hit$memo[1]
    }
    reg
  }

  reg <- .registry_empty()

  # scenarios: any first-level directory in the scenarios store carrying the
  # `class` marker file (layout >= 2) or a `scenario.yml` manifest (layout 3)
  scen_root <- root_join(get_scenarios_path())
  if (dir.exists(scen_root)) {
    for (d in list.dirs(scen_root, recursive = FALSE)) {
      # dot-prefixed dirs are scratch by convention (e.g. levcost mini-models)
      if (startsWith(basename(d), ".")) next
      manifest <- fp(d, "scenario.yml")
      class_file <- fp(d, "class")
      nm <- NULL
      model_hash <- ""
      model_name <- ""
      # reset per iteration: `mf` outlives the loop body, so a layout-2 folder
      # would otherwise be registered with the previous scenario's lifecycle
      mf <- NULL
      if (file.exists(manifest)) {
        mf <- tryCatch(yaml::read_yaml(manifest), error = \(e) NULL)
        if (!is.null(mf)) {
          nm <- mf$name %||% basename(d)
          model_hash <- mf$model$hash %||% ""
          model_name <- as.character(mf$model$name %||% "")[1]
        }
      } else if (file.exists(class_file) &&
                 any(grepl("scenario", readLines(class_file, warn = FALSE)))) {
        nm <- basename(d)
      }
      if (is.null(nm)) next
      reg <- do.call(add_to_registry, c(
        list(reg, "scenario", nm, path = rel_to_reg(d),
             model = model_name, model_hash = model_hash),
        .registry_lifecycle(mf)))
      reg <- keep_created(reg, "scenario", rel_to_reg(d))
      # runs (layout 3): base runs at runs/<solve>/run.yml, variant runs at
      # runs/<variant>/<solve>/run.yml
      run_ymls <- c(Sys.glob(fp(d, "runs", "*", "run.yml")),
                    Sys.glob(fp(d, "runs", "*", "*", "run.yml")))
      for (ry in run_ymls) {
        rr <- tryCatch(yaml::read_yaml(ry), error = \(e) NULL)
        if (is.null(rr)) next
        run_dir <- dirname(ry)
        parent_dir <- basename(dirname(run_dir))
        # plain solve label in `name`; the own-problem variant (if any) in
        # its own column — never a compound "variant/solve" name
        run_variant <- if (identical(parent_dir, "runs")) "" else parent_dir
        solve_lbl <- basename(run_dir)
        # runs carry no seal or deletion mark — those verbs act on the four
        # entry types, not on a run — so only `status` is recorded here
        reg <- add_to_registry(reg, "run", solve_lbl,
                            path = rel_to_reg(run_dir),
                            parent = nm, variant = run_variant,
                            status = as.character(rr$status %||% ""))
        reg <- keep_created(reg, "run",
                            rel_to_reg(run_dir))
      }
    }
  }

  # models and repositories: content-addressed stores of <name>@<hash8>/
  # folders, each with a manifest (layout 3)
  stores <- list(
    list(type = "model", root = root_join(get_models_path()),
         manifest = "model.yml"),
    list(type = "repository", root = root_join(get_repositories_path()),
         manifest = "repository.yml"),
    list(type = "dataset", root = root_join(get_datasets_path()),
         manifest = "dataset.yml")
  )
  for (st in stores) {
    if (!dir.exists(st$root)) next
    for (d in list.dirs(st$root, recursive = FALSE)) {
      manifest <- fp(d, st$manifest)
      if (!file.exists(manifest)) next
      mf <- tryCatch(yaml::read_yaml(manifest), error = \(e) NULL)
      if (is.null(mf)) next
      nm <- mf$name %||% sub("@.*$", "", basename(d))
      reg <- do.call(add_to_registry, c(
        list(reg, st$type, nm, path = rel_to_reg(d),
             hash = mf$hash %||% ""),
        .registry_lifecycle(mf)))
      reg <- keep_created(reg, st$type, rel_to_reg(d))
    }
  }

  if (write) save_registry(reg, file)
  reg
}
