# =========================================================================== #
# portability.R -- getting a stored scenario into shape to move, share, or
# shrink.
#
# Four user-facing functions over one folder:
#
#   scenario_artifacts()   what each solve left on disk, and what it costs
#   drop_solver_outputs()  remove the part of it that regenerates
#   strip_user_info()      remove the machine it was made on
#   prepare_for_sharing()  all of the above, into a copy
#
# Two invariants run through the file, and both exist because getting them
# wrong is silent rather than loud.
#
# 1. `output/` is deletable IF AND ONLY IF `modOut/` exists.
#
#    They are not the same thing. `output/` is the SOLVER'S raw dump and is
#    what `read_solution()` reads -- exclusively (read.R:111 and on).
#    `modOut/` is the IMPORTED solution, a parquet store written only by
#    `save_scenario()`. Once it exists, `load_scenario()` rebases onto it and
#    never looks at `output/` again (paths_rebase.R:24-40), so the raw dump is
#    redundant; until then it is the only copy.
#
#    `run.yml$status == "solved"` does NOT establish that: it records that the
#    solution reached memory, not that it reached disk. The only sound test is
#    whether the parquet store is there.
#
# 2. Stripping must not touch structural bookkeeping.
#
#    A blanket `@misc <- list()` -- which is what `.strip_volatile()` does for
#    the model store -- is wrong here:
#
#      @misc$onDisk       every path rebase is guarded on it being non-empty,
#                         so clearing it makes the rebase a SILENT NO-OP; it is
#                         also the "moved vs never written" discriminator whose
#                         loss returns every result as zero rows with no error
#                         (arrow.R:904-920). It holds class/dim/length/size --
#                         nothing identifying.
#      @misc$inMemory     gates the whole lazy-read path
#      @misc$run,$variant the modOut rebase root is derived from them
#      @misc$has_base     .variant_swap() reads it
#      @misc$sourceCode_default  without it the scenario loads with 13 missing
#                         template blocks and cannot be re-solved
#      ref name/hash      the model/dataset resolve by them; only $path goes
#      per-parameter misc dropIfEmpty, prune, fold_info, rem_col, nValues are
#                         semantics, not bookkeeping
#
# Two audiences want opposite things from the same fields, hence `scope`. On a
# shared team drive `run.yml`'s `hostname` and `user` are provenance -- who ran
# this, and where. On the way out of the team they are a leak.
#
# The stripping half works on the SAVED FOLDER, not on an in-memory object: it
# copies the tree (or edits it in place) and rewrites what is inside. That is
# deliberate -- `save_scenario(path = )` cannot write a copy of an already
# stored scenario, see `.sui_copy_tree()`.
# =========================================================================== #

#' @include arrow.R runs.R utils.R
NULL

# -- what a solve left behind, and dropping what regenerates --------------- #

# Is this scenario sealed? Read from the manifest, like the seal guard does --
# a scenario with no manifest yet has nothing to protect.
.art_sealed <- function(scen) {
  mfp <- fp(scen@path, "scenario.yml")
  if (length(mfp) != 1L || !file.exists(mfp)) return(FALSE)
  mf <- tryCatch(yaml::read_yaml(mfp), error = function(e) NULL)
  isTRUE(mf$sealed)
}

# Bytes held by a set of files and/or directories.
#
# The reported scratch size is the size of exactly the paths deletion would
# remove, rather than a separate subtract-the-keepers calculation. Two
# definitions of "the scratch" would drift apart, and the first sign of it
# would be a size that does not match what was freed.
.art_paths_size <- function(paths) {
  if (!length(paths)) return(0)
  total <- 0
  for (p in paths) {
    total <- total + if (dir.exists(p)) {
      dir_size(p, missing = "zero")
    } else if (file.exists(p)) {
      sum(file.size(p), na.rm = TRUE)
    } else 0
  }
  total
}

# Classify one run directory: what is there, what it costs, what may go.
.art_run_info <- function(scen, variant, solve, dir, has_record) {
  solver_dir <- .run_solver_dir(dir)
  mod_out <- fp(dir, "modOut")
  imported <- dir.exists(fp(mod_out, "variables"))
  rec <- if (has_record) {
    tryCatch(yaml::read_yaml(fp(dir, "run.yml")), error = function(e) NULL)
  }
  status <- if (has_record) rec$status %||% "unknown" else "no-record"

  list(
    variant = variant, solve = solve, dir = dir,
    has_record = has_record, status = status, imported = imported,
    solution_mb = dir_size(mod_out, missing = "zero") / 1024^2,
    scratch_mb = .art_paths_size(.art_scratch_paths(dir, imported)) / 1024^2
  )
}

# The solver's output directory for a run, whichever layout wrote it.
.art_output_dir <- function(solver_dir, run_dir) {
  d <- fp(solver_dir, "output")
  if (dir.exists(d)) return(d)
  fp(run_dir, "output")
}

# Everything in a run folder that regenerates, as absolute paths. `output/` is
# included only when the solution has been imported.
.art_scratch_paths <- function(dir, imported) {
  solver_dir <- .run_solver_dir(dir)
  ff <- list.files(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  keep <- c(fp(dir, "run.yml"), fp(dir, "modOut"))
  if (!imported) keep <- c(keep, .art_output_dir(solver_dir, dir))
  if (!identical(solver_dir, gsub("[\\/]+", "/", dir))) {
    # layout 3/S2: the scratch is one level down, and the run folder itself
    # holds only the record and the solution
    ff <- c(setdiff(ff, keep),
            list.files(solver_dir, full.names = TRUE, all.files = TRUE,
                       no.. = TRUE))
    if (!imported) ff <- setdiff(ff, .art_output_dir(solver_dir, dir))
    return(unique(gsub("[\\/]+", "/", setdiff(ff, keep))))
  }
  unique(gsub("[\\/]+", "/", setdiff(ff, keep)))
}

# Why this row is a clean-up candidate, or "" when it is not.
#
# The ACTIVE run is suggested like any other. That differs from
# `drop_scenario_run()`, which refuses it -- rightly, because it removes the
# whole run. Here only the regenerable scratch goes; the record and the
# imported solution stay, so the run remains listed and readable. Excluding it
# would skip the commonest case of all: solve, save, reclaim the solver files.
.art_suggest <- function(info, active, sealed) {
  if (sealed) return("")
  if (!info$has_record) {
    return("no run.yml -- a crash before the record")
  }
  if (info$status %in% c("failed", "interrupted")) {
    return(paste0(info$status, " solve; only the record is worth keeping"))
  }
  if (identical(info$status, "running")) {
    return("stale 'running' record -- the process did not finish")
  }
  if (info$imported && info$scratch_mb > 0) {
    return("solution imported; the solver scratch is redundant")
  }
  ""
}

#' Solver artifacts of a stored scenario
#'
#' @description
#' Lists what each solve left on disk — whether its solution was imported, what
#' the scratch costs, and which rows are safe to clean up. Read-only.
#'
#' @details
#' The `scratch_mb` column counts the regenerable part: the generated model
#' source, the exchange `input/`, solver logs, and `output/` **only when the
#' solution has been imported** into `modOut/`. Until that has happened
#' `output/` is the only copy of the solution and is never counted as scratch,
#' whatever `status` says — `status == "solved"` records that the solution
#' reached memory, not disk.
#'
#' `suggest` is a reason string, empty when the row is not a candidate. A
#' sealed scenario suggests nothing. The **active** run is suggested like any
#' other — unlike [drop_scenario_run()], cleaning scratch keeps the record and
#' the solution, so the run stays listed and readable.
#'
#' @param scen a scenario object.
#'
#' Rows for the scenario-level derived tiers are listed too, distinguished by
#' `kind`: `"reports"` for rendered reports and `"levcost"` for cached levcost
#' results. They sit beside `runs/` rather than inside it, and each has its own
#' remover — [clear_report_cache()] and [clear_levcost_cache()] — so
#' [drop_solver_outputs()], a run cleaner, leaves them alone.
#'
#' @return a tibble: `kind`, `run`, `variant`, `solve`, `status`, `imported`,
#'   `scratch_mb`, `solution_mb`, `active`, `sealed`, `has_record`, `path`,
#'   `suggest`.
#' @seealso [drop_solver_outputs()], [scenario_runs()],
#'   [clear_report_cache()]
#' @export
scenario_artifacts <- function(scen) {
  stopifnot(is(scen, "scenario"))
  empty <- tibble(
    kind = character(0),
    run = character(0), variant = character(0), solve = character(0),
    status = character(0), imported = logical(0), scratch_mb = numeric(0),
    solution_mb = numeric(0), active = logical(0), sealed = logical(0),
    has_record = logical(0), path = character(0), suggest = character(0)
  )
  sealed <- isTRUE(.art_sealed(scen))
  act_v <- .run_variant(scen)
  act_s <- scen@misc$run %||% ""

  rd <- .run_dirs(scen)
  rows <- lapply(seq_len(nrow(rd)), function(i) {
    info <- .art_run_info(scen, rd$variant[i], rd$solve[i], rd$dir[i],
                          rd$has_record[i])
    active <- identical(info$variant, act_v) && identical(info$solve, act_s)
    tibble(
      kind = "run",
      run = .run_id(info$variant, info$solve),
      variant = info$variant, solve = info$solve,
      status = info$status, imported = info$imported,
      scratch_mb = round(info$scratch_mb, 3),
      solution_mb = round(info$solution_mb, 3),
      active = active, sealed = sealed, has_record = info$has_record,
      path = info$dir,
      suggest = .art_suggest(info, active, sealed)
    )
  })
  bind_rows(c(list(empty), rows, .art_derived_rows(scen, sealed)))
}

# The scenario-level derived tiers: rendered reports and cached levcost
# results. They sit beside `runs/`, not inside it, so `.art_scratch_paths()`
# never sees them -- without a row here they are absent from the disk picture
# and nothing accounts for them.
#
# `kind` keeps them out of `drop_solver_outputs()`, which is a RUN cleaner:
# each tier has its own remover (`clear_report_cache()`,
# `clear_levcost_cache()`) and its own rule for what is safe to lose.
.art_derived_rows <- function(scen, sealed) {
  own <- .object_store_dir(scen)
  if (is.null(own)) return(list())
  spec <- list(
    reports = "rendered reports -- regenerable from the object and template",
    levcost = "cached levcost results -- recomputable")
  out <- list()
  for (nm in names(spec)) {
    d <- fp(own, nm)
    if (!dir.exists(d)) next
    mb <- dir_size(d, missing = "zero") / 1024^2
    out[[length(out) + 1L]] <- tibble(
      kind = nm, run = NA_character_, variant = NA_character_,
      solve = NA_character_, status = NA_character_, imported = NA,
      scratch_mb = round(mb, 3), solution_mb = 0,
      active = FALSE, sealed = sealed, has_record = NA,
      path = gsub("[\\/]+", "/", d),
      suggest = if (sealed || mb == 0) "" else spec[[nm]])
  }
  out
}

#' Delete the regenerable part of a solve
#'
#' @description
#' Removes the generated model source, the exchange `input/`, solver logs and —
#' only when the solution has been imported — the solver's raw `output/`. The
#' run record and the imported solution are kept, so the run stays listed and
#' readable. **Dry-run by default.**
#'
#' @details
#' This is what [drop_scenario_run()] cannot do: that removes the whole run.
#'
#' A run whose solution was never imported keeps its `output/` whatever is
#' asked, because that directory is the only copy — see [scenario_artifacts()].
#'
#' Only runs are touched. Rendered reports and cached levcost results are
#' scenario-level, not part of a run; [clear_report_cache()] and
#' [clear_levcost_cache()] remove those.
#'
#' @param scen a scenario object.
#' @param runs character, run identifiers (`"<solve>"` or `"<variant>/<solve>"`)
#'   or `NULL` for every run `scenario_artifacts()` suggests.
#' @param dry_run logical. `TRUE` (default) lists what would go without
#'   removing it.
#' @param verbose logical.
#'
#' @return a tibble of candidates with an `action` column (`would_delete`,
#'   `deleted`, `kept_not_imported`, `nothing_to_do`, `skipped_sealed`) and
#'   `freed_mb`. Returned visibly on a dry run, invisibly otherwise.
#' @seealso [scenario_artifacts()], [drop_scenario_run()]
#' @export
drop_solver_outputs <- function(scen, runs = NULL, dry_run = TRUE,
                                verbose = TRUE) {
  stopifnot(is(scen, "scenario"))
  # a RUN cleaner: the scenario-level derived tiers have their own removers
  art <- scenario_artifacts(scen)
  art <- art[art$kind == "run", , drop = FALSE]
  art$action <- character(nrow(art))
  art$freed_mb <- numeric(nrow(art))
  if (!nrow(art)) {
    if (verbose) message("No runs in scenario '", scen@name, "'.")
    return(invisible(art))
  }
  sel <- if (is.null(runs)) art[nzchar(art$suggest), ] else {
    unknown <- setdiff(runs, art$run)
    if (length(unknown)) {
      stop("No such run", if (length(unknown) > 1) "s" else "", ": ",
           paste(unknown, collapse = ", "), "\n  Available runs:\n",
           .run_list_hint(scen), call. = FALSE)
    }
    art[art$run %in% runs, ]
  }
  if (!nrow(sel)) {
    if (verbose) message("Nothing to clean in scenario '", scen@name, "'.")
    return(invisible(sel))
  }

  sel$action <- NA_character_
  sel$freed_mb <- 0

  for (i in seq_len(nrow(sel))) {
    if (sel$sealed[i]) { sel$action[i] <- "skipped_sealed"; next }
    paths <- .art_scratch_paths(sel$path[i], sel$imported[i])
    if (!length(paths)) {
      sel$action[i] <- if (sel$imported[i]) "nothing_to_do" else
        "kept_not_imported"
      next
    }
    sel$freed_mb[i] <- sel$scratch_mb[i]
    if (dry_run) { sel$action[i] <- "would_delete"; next }
    unlink(paths, recursive = TRUE, force = TRUE)
    sel$action[i] <- "deleted"
  }

  if (!dry_run && any(sel$action == "deleted")) {
    tryCatch(refresh_registry(), error = function(e) {
      warning("Could not refresh the registry: ", conditionMessage(e),
              "\n  Run refresh_registry() when convenient.", call. = FALSE)
    })
  }

  if (verbose) {
    if (dry_run) {
      message("Dry run - nothing deleted. ",
              sum(sel$action == "would_delete"), " of ", nrow(sel),
              " run(s) would free ",
              round(sum(sel$freed_mb[sel$action == "would_delete"]), 2),
              " MB; drop_solver_outputs(dry_run = FALSE) to remove.")
    } else {
      message("Freed ", round(sum(sel$freed_mb[sel$action == "deleted"]), 2),
              " MB from ", sum(sel$action == "deleted"), " run(s).")
    }
  }
  if (dry_run) sel else invisible(sel)
}

# -- removing the machine, and copying a scenario out ---------------------- #

# Copy a scenario folder.
#
# Not `save_scenario(path = )`: for a scenario whose data is already on disk
# that writes only the six-file shell and leaves the parquet stores behind
# (`arrow.R:281`, "The data walk is skipped"), so the result looks like a
# scenario and has no data. A file-level copy is also faster and preserves
# exactly what is there.
.sui_copy_tree <- function(from, to) {
  if (!dir.exists(from)) {
    stop("Scenario directory '", from, "' does not exist.", call. = FALSE)
  }
  if (dir.exists(to)) unlink(to, recursive = TRUE, force = TRUE)
  dir.create(to, recursive = TRUE, showWarnings = FALSE)
  ff <- list.files(from, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  ok <- file.copy(ff, to, recursive = TRUE, copy.date = TRUE)
  if (!all(ok)) {
    stop("Could not copy ", sum(!ok), " item(s) into '", to, "'.",
         call. = FALSE)
  }
  gsub("[\\/]+", "/", to)
}

# Fields of `@misc` that are derived or cached, and identify a machine or a
# session rather than the model.
.SUI_MISC_DROP <- c("dirsize", "time.log", "approxim", "dropped_data")

# The current user's home directory and login, as strings to scan for.
.sui_identity <- function() {
  home <- tryCatch(gsub("[\\/]+", "/", normalizePath(path.expand("~"),
                                                     winslash = "/",
                                                     mustWork = FALSE)),
                   error = function(e) "")
  user <- tryCatch(unname(Sys.info()["user"]), error = function(e) "")
  list(home = home[nzchar(home)], user = user[nzchar(user)])
}

# Clear the machine-local parts of an in-memory scenario. Paths are left alone
# here -- see the file header.
.sui_strip_object <- function(scen, scope, keep_solver) {
  # The solver command line is the one absolute path the load-time rebase does
  # not reconstruct, and every backend regenerates it when empty.
  if (is.list(scen@settings@solver)) {
    for (k in c("cmdline", "inc1", "inc2", "inc3", "inc4", "inc5",
                "code_files")) {
      if (!is.null(scen@settings@solver[[k]])) scen@settings@solver[[k]] <- ""
    }
    if (!keep_solver) {
      for (k in setdiff(names(scen@settings@solver),
                        c("name", "lang", "cmdline"))) {
        scen@settings@solver[[k]] <- NULL
      }
    }
  }

  for (k in .SUI_MISC_DROP) scen@misc[[k]] <- NULL
  lv <- grep("^levcost", names(scen@misc), value = TRUE)
  for (k in lv) scen@misc[[k]] <- NULL

  # A recorded run derives its directory from `@path`; only an EXTERNAL run
  # has these as its sole pointer to the results, so keep them then.
  if (nzchar(scen@misc$run %||% "")) {
    scen@misc$solver.dir <- NULL
    scen@misc$tmp.dir <- NULL
  }

  # Keep the name and hash of every reference -- they are what resolves it.
  for (k in c("model_ref", "repo_ref")) {
    if (!is.null(scen@misc[[k]]$path)) scen@misc[[k]]$path <- NULL
  }
  if (is.list(scen@misc$dataset_ref)) {
    for (i in seq_along(scen@misc$dataset_ref)) {
      if (!is.null(scen@misc$dataset_ref[[i]]$path)) {
        scen@misc$dataset_ref[[i]]$path <- NULL
      }
    }
  }

  if (identical(scope, "share") && !keep_solver) {
    # user-authored solver templates and constraint code can carry anything
    scen@modInp@user_constraints <- list()
    scen@modInp@user_costs <- character()
  }
  scen
}

# Rewrite a SAVED scen.RData: apply the object-level strip and blank the
# machine paths. Blanking the paths is safe because
# `.scenario_rebase_paths()` rebuilds each of them from the folder being read.
.sui_scrub_rdata <- function(dir, scope = "share", keep_solver = FALSE) {
  f <- fp(dir, "scen.RData")
  if (!file.exists(f)) return(FALSE)
  e <- new.env(parent = emptyenv())
  nm <- tryCatch(load(f, envir = e), error = function(err) NULL)
  if (is.null(nm) || !length(nm)) return(FALSE)
  obj <- get(nm[1], envir = e)
  if (!is(obj, "scenario")) return(FALSE)

  obj <- .sui_strip_object(obj, scope, keep_solver)
  obj@path <- ""
  blank <- function(x) {
    if (isS4(x) && .hasSlot(x, "misc") && !is.null(x@misc$path)) {
      x@misc$path <- ""
    }
    x
  }
  obj@modInp <- blank(obj@modInp)
  for (p in names(obj@modInp@parameters)) {
    obj@modInp@parameters[[p]] <- blank(obj@modInp@parameters[[p]])
  }
  if (isS4(obj@modOut)) {
    obj@modOut <- blank(obj@modOut)
    for (v in names(obj@modOut@variables)) {
      obj@modOut@variables[[v]] <- blank(obj@modOut@variables[[v]])
    }
  }
  if (isS4(obj@model)) obj@model <- blank(obj@model)

  assign(nm[1], obj, envir = e)
  save(list = nm[1], file = f, envir = e, compress = "xz")
  TRUE
}

# Remove the named keys from every run.yml under `dir`.
.sui_scrub_run_yml <- function(dir, keys) {
  ff <- list.files(dir, pattern = "^run\\.yml$", recursive = TRUE,
                   full.names = TRUE)
  n <- 0L
  for (f in ff) {
    mf <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (is.null(mf)) next
    hit <- intersect(keys, names(mf))
    if (!length(hit)) next
    for (k in hit) mf[[k]] <- ""
    tryCatch({ yaml::write_yaml(mf, f); n <- n + 1L },
             error = function(e) invisible(NULL))
  }
  n
}

# Blank the cmdline row of every solver.csv (and its extension-less sibling).
.sui_scrub_solver_csv <- function(dir) {
  ff <- c(list.files(dir, pattern = "^solver\\.csv$", recursive = TRUE,
                     full.names = TRUE),
          list.files(dir, pattern = "^solver$", recursive = TRUE,
                     full.names = TRUE))
  ff <- ff[!dir.exists(ff)]
  n <- 0L
  for (f in ff) {
    d <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE),
                  error = function(e) NULL)
    if (is.null(d) || !all(c("name", "value") %in% names(d))) next
    i <- d$name %in% c("cmdline", "solver.dir", "tmp.dir")
    if (!any(i)) next
    d$value[i] <- ""
    tryCatch({ utils::write.csv(d, f, row.names = FALSE); n <- n + 1L },
             error = function(e) invisible(NULL))
  }
  n
}

# Drop the path fields from scenario.yml, keeping name and hash.
.sui_scrub_manifest <- function(dir) {
  f <- fp(dir, "scenario.yml")
  if (!file.exists(f)) return(FALSE)
  mf <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
  if (is.null(mf)) return(FALSE)
  if (!is.null(mf$model$path)) mf$model$path <- NULL
  if (is.list(mf$datasets)) {
    for (i in seq_along(mf$datasets)) {
      if (!is.null(mf$datasets[[i]]$path)) mf$datasets[[i]]$path <- NULL
    }
  }
  tryCatch({ yaml::write_yaml(mf, f); TRUE }, error = function(e) FALSE)
}

# Files under `dir` that still contain the home directory or user name.
#
# A scan, not an allowlist: GAMS listings embed absolute paths and the next
# backend may too. Note what it CANNOT see -- `scen.RData` is xz-compressed,
# so a byte search will not find a path inside it. That object is checked by
# reading it back, not by scanning (see the tests).
.sui_scan_identity <- function(dir, id) {
  pats <- c(id$home, id$user)
  pats <- pats[nzchar(pats)]
  if (!length(pats)) return(character(0))
  ff <- list.files(dir, recursive = TRUE, full.names = TRUE, all.files = TRUE,
                   no.. = TRUE)
  ff <- ff[!dir.exists(ff)]
  hit <- character(0)
  raw_pats <- lapply(pats, charToRaw)
  for (f in ff) {
    sz <- file.size(f)
    if (is.na(sz) || sz == 0 || sz > 50e6) next
    # Byte search, not readLines(): the tree is mostly parquet, and decoding
    # that as text warns once per batch about invalid strings. Bytes also
    # find a path embedded in a binary payload, which is the point.
    buf <- tryCatch(readBin(f, "raw", n = sz), error = function(e) raw(0))
    if (!length(buf)) next
    if (any(vapply(raw_pats, function(rp) length(grepRaw(rp, buf)) > 0,
                   logical(1)))) {
      hit <- c(hit, gsub("[\\/]+", "/", f))
    }
  }
  hit
}

#' Remove machine and user information from a stored scenario
#'
#' @description
#' Clears the absolute paths, solver command line and — with
#' `scope = "share"` — the host/user provenance from a scenario, so the folder
#' can move between machines or leave the team.
#'
#' @details
#' `scope = "store"` removes only what breaks cross-machine use: the stored
#' absolute paths and the solver command line, which is the one path the
#' load-time rebase does not reconstruct and which every backend regenerates
#' when empty. Provenance is kept, because on a shared drive "who ran this" is
#' the point.
#'
#' `scope = "share"` (default) additionally clears `hostname` and `user` from
#' every `run.yml`, the save-history `logfile.csv`, and user-authored
#' constraint code.
#'
#' Structural bookkeeping is never touched — in particular `@misc$onDisk`,
#' which is not user-identifying and whose loss would silently return every
#' result as zero rows.
#'
#' **Rendered reports are reported, not cleaned.** A report can embed an
#' absolute path or a user name inside a PDF or DOCX, where byte-level
#' scrubbing would corrupt the file; such files show up in `remaining`. Remove
#' them with [clear_report_cache()] — they re-render — or let
#' [prepare_for_sharing()] drop them, which it does by default.
#'
#' Passing `path` writes a cleaned copy and leaves the original working.
#' `path = NULL` edits in place, which is irreversible, so it is a dry run
#' unless `confirm = TRUE`.
#'
#' @param scen a scenario object.
#' @param path target directory for a cleaned copy, or `NULL` to edit in place.
#' @param scope `"share"` (default) or `"store"` — see Details.
#' @param keep_solver logical, keep the solver settings and any user-authored
#'   template code. `FALSE` by default.
#' @param confirm logical, required to edit in place.
#' @param verbose logical.
#'
#' @return invisibly, a list with `path`, `scope`, `run_yml` (records scrubbed),
#'   `solver_csv`, and `remaining` — files that still contain the home
#'   directory or user name after the pass, which should be empty.
#' @seealso [prepare_for_sharing()], [scenario_artifacts()]
#' @export
strip_user_info <- function(scen, path = NULL, scope = c("share", "store"),
                            keep_solver = FALSE, confirm = FALSE,
                            verbose = TRUE) {
  from <- if (is(scen, "scenario")) scen@path else as.character(scen)
  stopifnot(length(from) == 1L, nzchar(from))
  scope <- match.arg(scope)
  in_place <- is.null(path)

  if (!dir.exists(from)) {
    stop("No stored scenario at '", from, "'. Save it first: a scenario is ",
         "stripped on disk, not in memory.", call. = FALSE)
  }

  if (in_place && !isTRUE(confirm)) {
    if (verbose) {
      message("Dry run - nothing changed. Stripping '", from, "' in place is ",
              "irreversible: it clears
  the solver command line, the stored ",
              "paths",
              if (identical(scope, "share")) " and the run provenance" else "",
              ".
  Pass confirm = TRUE, or path = <dir> to write a cleaned ",
              "copy instead.")
    }
    return(invisible(list(path = from, scope = scope, run_yml = 0L,
                          solver_csv = 0L, remaining = character(0),
                          dry_run = TRUE)))
  }

  out <- if (in_place) gsub("[\\/]+", "/", from) else
    .sui_copy_tree(from, path)

  .sui_scrub_rdata(out, scope, keep_solver)
  .sui_scrub_manifest(out)
  keys <- c("cmdline", "solver")
  if (identical(scope, "share")) keys <- c(keys, "hostname", "user")
  n_run <- .sui_scrub_run_yml(out, keys)
  n_csv <- .sui_scrub_solver_csv(out)
  if (identical(scope, "share")) unlink(fp(out, "logfile.csv"), force = TRUE)

  remaining <- .sui_scan_identity(out, .sui_identity())

  if (verbose) {
    message("Stripped '", out, "' (scope = ", scope, "): ",
            n_run, " run record(s), ", n_csv, " solver file(s).")
    if (length(remaining)) {
      message("  Still carrying the home directory or user name:
   ",
              paste(utils::head(remaining, 5), collapse = "
   "),
              if (length(remaining) > 5)
                paste0("
   ... and ", length(remaining) - 5, " more"))
    }
  }
  invisible(list(path = out, scope = scope, run_yml = n_run,
                 solver_csv = n_csv, remaining = remaining, dry_run = FALSE))
}

#' Prepare a stored scenario for sharing
#'
#' @description
#' Copies a scenario to a new directory, drops the regenerable solver scratch,
#' and removes the machine it was made on — in one call. The original is left
#' untouched.
#'
#' @details
#' Runs [drop_solver_outputs()] then [strip_user_info()] on the copy, and
#' reports anything it could not make portable rather than producing a folder
#' that fails on the recipient's machine.
#'
#' Two things it does **not** do, deliberately. It will not write in place:
#' the point is an artifact beside the working original. And it does not
#' re-save through `save_scenario(path = )`, which for an already-stored
#' scenario writes only the shell and leaves the data behind.
#'
#' A scenario that *references* a model or dataset store rather than embedding
#' it cannot be made self-contained by copying alone; those references are
#' listed in `unresolved` and the recipient will need the store too.
#'
#' @param scen a scenario object, or the path to a stored scenario.
#' @param path target directory. Required.
#' @param scope `"share"` (default) or `"store"`, passed to
#'   [strip_user_info()].
#' @param keep_runs logical, keep the solver scratch. `FALSE` by default.
#' @param keep_reports logical, keep rendered reports. `FALSE` by default:
#'   they re-render from the object and the template, and are the one artifact
#'   that can carry an absolute path or a user name inside a binary (PDF,
#'   DOCX) where scrubbing is not reliable — so they are removed, not cleaned.
#' @param keep_solver logical, keep the solver settings.
#' @param verbose logical.
#'
#' @return invisibly, a list: `path`, `mb_before`, `mb_after`, `freed_mb`,
#'   `remaining` (files still carrying the home directory or user name) and
#'   `unresolved` (references that will not travel).
#' @seealso [strip_user_info()], [drop_solver_outputs()]
#' @export
prepare_for_sharing <- function(scen, path, scope = c("share", "store"),
                                keep_runs = FALSE, keep_reports = FALSE,
                                keep_solver = FALSE, verbose = TRUE) {
  scope <- match.arg(scope)
  if (missing(path) || is.null(path) || !nzchar(path)) {
    stop("`path` is required: prepare_for_sharing() writes a copy and never ",
         "edits in place.", call. = FALSE)
  }
  from <- if (is(scen, "scenario")) scen@path else as.character(scen)
  if (!dir.exists(from)) {
    stop("No stored scenario at '", from, "'.", call. = FALSE)
  }
  mb_before <- dir_size(from, missing = "zero") / 1024^2

  out <- .sui_copy_tree(from, path)

  freed <- 0
  if (!keep_runs) {
    # Rebase the object onto the COPY by hand.
    #
    # Not `getScenario()`: it resolves by NAME through the registry and the
    # scenarios store, which both still point at the original -- so cleaning
    # "the copy" would delete the source's solver files instead. Nothing in
    # `drop_solver_outputs()` reads anything but `@path`, `@name` and the
    # active-run fields, so a rebased copy of the object is enough.
    obj <- if (is(scen, "scenario")) scen else NULL
    if (is.null(obj)) {
      e <- new.env(parent = emptyenv())
      nm <- tryCatch(load(fp(out, "scen.RData"), envir = e),
                     error = function(err) NULL)
      if (!is.null(nm) && length(nm)) obj <- get(nm[1], envir = e)
    }
    if (is(obj, "scenario")) {
      obj@path <- out
      d <- suppressMessages(drop_solver_outputs(obj, dry_run = FALSE,
                                                verbose = FALSE))
      freed <- sum(d$freed_mb[d$action == "deleted"], na.rm = TRUE)
    }
  }

  # Rendered reports go rather than get scrubbed: byte-replacing a user name
  # inside a PDF or DOCX corrupts the container, and they re-render anyway.
  if (!keep_reports) {
    rep_dir <- fp(out, "reports")
    if (dir.exists(rep_dir)) {
      freed <- freed + dir_size(rep_dir, missing = "zero") / 1024^2
      unlink(rep_dir, recursive = TRUE, force = TRUE)
    }
  }

  r <- strip_user_info(out, path = NULL, scope = scope,
                       keep_solver = keep_solver, confirm = TRUE,
                       verbose = FALSE)

  # References that a copy cannot carry: the store they point at is elsewhere.
  unresolved <- character(0)
  mf <- tryCatch(yaml::read_yaml(fp(out, "scenario.yml")),
                 error = function(e) NULL)
  if (!is.null(mf)) {
    if (identical(mf$model$source %||% "", "ref")) {
      unresolved <- c(unresolved,
                      paste0("model '", mf$model$name %||% "?", "'"))
    }
    for (ds in mf$datasets %||% list()) {
      if (identical(ds$source %||% "", "ref")) {
        unresolved <- c(unresolved,
                        paste0("dataset '", ds$name %||% "?", "'"))
      }
    }
  }

  mb_after <- dir_size(out, missing = "zero") / 1024^2
  if (verbose) {
    message("Prepared '", out, "' for sharing (scope = ", scope, "): ",
            round(mb_before, 2), " MB -> ", round(mb_after, 2), " MB.")
    if (length(r$remaining)) {
      message("  WARNING - still carries the home directory or user name:\n   ",
              paste(utils::head(r$remaining, 5), collapse = "\n   "))
    }
    if (length(unresolved)) {
      message("  NOT self-contained - these are referenced, not embedded, ",
              "and will not travel:\n   ",
              paste(unresolved, collapse = "\n   "),
              "\n  Re-save with embed_model = TRUE / embed_datasets = TRUE ",
              "before sharing.")
    }
  }
  invisible(list(path = out, mb_before = mb_before, mb_after = mb_after,
                 freed_mb = freed, remaining = r$remaining,
                 unresolved = unresolved))
}
