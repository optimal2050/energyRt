# =============================================================================#
# import_solution.R -- choosing a solver attempt and making it stick.
#
# `read_solution()` populates `@modOut` and `save_scenario()` writes it; with
# neither called, a solved run leaves `output/` behind and no `modOut/`. With
# several attempts there is no table to choose from either: `run.yml`'s
# `objective` is computed from the IN-MEMORY solution at finish time
# (`.run_record_finish()`), so it is NA for exactly the runs that were never
# imported.
#
# It is recoverable without importing anything: `output/log.csv` carries the
# solution status, and the objective is one small `output/vObjective.<ext>`
# file whose extension comes from that run's own `solver.csv`. Peeking is what
# makes an informed choice possible -- nothing here ranks or auto-selects.
#
# Nothing in this file writes to `output/`.
# =============================================================================#

#' @include read.R
NULL

# Status and objective of one run, read from its `output/` without importing.
# Never errors: a corrupt or half-written dump degrades the row to NA.
.peek_run_solution <- function(run_dir) {
  out <- list(solver_status = NA_character_, objective = NA_real_,
              has_output = FALSE, output_mb = 0)
  solver_dir <- tryCatch(.run_solver_dir(run_dir), error = function(e) NULL)
  if (is.null(solver_dir)) return(out)
  odir <- .art_output_dir(solver_dir, run_dir)
  if (!dir.exists(odir)) return(out)
  out$has_output <- TRUE
  out$output_mb <- round(dir_size(odir, missing = "zero") / 1024^2, 3)

  # solution status: `log.csv` records it as a numeric code, 1 == optimal --
  # the same reading `read_solution()` makes when it sets `@stage`.
  lg <- tryCatch(utils::read.csv(fp(odir, "log.csv"), stringsAsFactors = FALSE),
                 error = function(e) NULL)
  if (!is.null(lg) && all(c("parameter", "value") %in% names(lg))) {
    v <- lg$value[lg$parameter == "solution status"]
    out$solver_status <- if (!length(v)) {
      "no status"
    } else if (identical(as.character(v[1]), "1")) {
      "optimal"
    } else {
      paste0("not optimal (", v[1], ")")
    }
  }

  # objective: one tiny file, whose extension follows the run's own
  # import_format -- not the scenario's, which may differ or be empty.
  imf <- tolower(tryCatch({
    m <- .read_solver_meta(solver_dir)
    x <- m$value[m$name == "import_format"]
    if (length(x)) as.character(x[1]) else ""
  }, error = function(e) ""))
  ext <- if (imf == "parquet") ".parquet" else
    if (imf %in% c("feather", "ipc", "arrow")) ".arrow" else ".csv"
  f <- fp(odir, paste0("vObjective", ext))
  if (file.exists(f)) {
    d <- tryCatch(
      if (identical(ext, ".csv")) {
        utils::read.csv(f, stringsAsFactors = FALSE)
      } else {
        as.data.frame(.read_exchange_table(f))
      }, error = function(e) NULL)
    if (!is.null(d) && nrow(d)) {
      col <- if ("value" %in% names(d)) "value" else names(d)[ncol(d)]
      out$objective <- suppressWarnings(as.numeric(d[[col]][1]))
    }
  }
  out
}

#' What solutions does this scenario have, and which is worth importing?
#'
#' @description
#' One row per run, joining what the run record holds with what can be read
#' back from a run's raw `output/` without importing it. Read-only: it writes
#' nothing, not even the objectives it recovers.
#'
#' @details
#' `run.yml`'s `objective` is computed from the in-memory solution when the
#' solve finishes, so it is `NA` for any run whose solution was never imported
#' — which is exactly the set you need to compare after several attempts.
#' This recovers it per run from the single `vObjective` file in `output/`,
#' and `objective_src` says which reading you are looking at: `"record"` from
#' `run.yml`, `"output"` peeked from the dump, `NA` when neither is available.
#'
#' `solver_status` likewise comes from the run's `output/log.csv` and is the
#' solver's own verdict, independent of `status`, which is what the driver
#' recorded. The two disagree exactly when a solve finished but was never read
#' back.
#'
#' Nothing here ranks the runs or picks one. Objectives are comparable only
#' within one problem: a `"<variant>/<solve>"` run solves a *different* problem
#' (its own `modInp`), so compare within a `variant`, not across.
#'
#' @param scen a scenario object.
#' @param peek logical; `FALSE` skips reading `output/` and leaves
#'   `solver_status` / peeked objectives as `NA`. Faster on a scenario with
#'   many large runs.
#'
#' @return a tibble: `run`, `variant`, `solve`, `status`, `solver_status`,
#'   `objective`, `objective_src`, `solver_name`, `lang`, `solver`,
#'   `duration_sec`, `imported`, `has_output`, `output_mb`, `active`.
#' @seealso [import_solution()], [scenario_runs()], [scenario_artifacts()]
#' @examples
#' \dontrun{
#' scenario_solutions(scen)                      # what is there
#' import_solution(scen, "julia_highs_barrier")  # make one of them stick
#' }
#' @export
scenario_solutions <- function(scen, peek = TRUE) {
  stopifnot(is(scen, "scenario"))
  empty <- tibble(
    run = character(0), variant = character(0), solve = character(0),
    status = character(0), solver_status = character(0),
    objective = numeric(0), objective_src = character(0),
    solver_name = character(0), lang = character(0), solver = character(0),
    duration_sec = numeric(0), imported = logical(0), has_output = logical(0),
    output_mb = numeric(0), active = logical(0))

  rd <- .run_dirs(scen)
  if (!nrow(rd)) return(empty)
  act_v <- .run_variant(scen)
  act_s <- scen@misc$run %||% ""

  rows <- lapply(seq_len(nrow(rd)), function(i) {
    dir <- rd$dir[i]
    rec <- if (isTRUE(rd$has_record[i])) {
      tryCatch(yaml::read_yaml(fp(dir, "run.yml")), error = function(e) NULL)
    } else {
      NULL
    }
    pk <- if (isTRUE(peek)) .peek_run_solution(dir) else
      list(solver_status = NA_character_, objective = NA_real_,
           has_output = dir.exists(.art_output_dir(.run_solver_dir(dir), dir)),
           output_mb = NA_real_)

    obj <- suppressWarnings(as.numeric(rec$objective %||% NA_real_))
    src <- if (!is.na(obj)) "record" else if (!is.na(pk$objective)) "output" else
      NA_character_
    if (is.na(obj)) obj <- pk$objective

    tibble(
      run = .run_id(rd$variant[i], rd$solve[i]),
      variant = rd$variant[i], solve = rd$solve[i],
      status = as.character(rec$status %||% if (isTRUE(rd$has_record[i]))
        "unknown" else "no-record"),
      solver_status = pk$solver_status,
      objective = obj, objective_src = src,
      solver_name = as.character(rec$solver_name %||% NA_character_),
      lang = as.character(rec$lang %||% NA_character_),
      solver = as.character(rec$solver %||% NA_character_),
      duration_sec = suppressWarnings(as.numeric(rec$duration_sec %||% NA_real_)),
      imported = dir.exists(fp(dir, "modOut", "variables")),
      has_output = pk$has_output,
      output_mb = pk$output_mb,
      active = identical(rd$variant[i], act_v) &&
        identical(rd$solve[i], act_s))
  })
  bind_rows(c(list(empty), rows))
}

# The run's own solver identity, from the `solver.csv` it was solved with.
# `cmdline` is deliberately excluded: it is an absolute, machine-local path
# that every backend regenerates when empty, and `strip_user_info()` clears it
# on purpose. Restoring it would put a username back into a saved object.
.solver_identity <- function(solver_dir) {
  m <- tryCatch(.read_solver_meta(solver_dir), error = function(e) NULL)
  if (is.null(m) || !all(c("name", "value") %in% names(m))) return(NULL)
  keep <- c("name", "lang", "solver", "method", "import_format",
            "export_format")
  out <- list()
  for (k in keep) {
    v <- m$value[m$name == k]
    if (length(v) && nzchar(as.character(v[1]))) out[[k]] <- as.character(v[1])
  }
  if (!length(out)) NULL else out
}

#' Import a run's solution and make it stick
#'
#' @description
#' Reads the solution of one run into `modOut` and saves it, so the scenario
#' keeps it after the session ends. This is the step between a finished solve
#' and a scenario that still has its results tomorrow: `solve_scenario()` never
#' saves, and `read_solution()` alone leaves the solution in memory.
#'
#' @details
#' Pick the run with [scenario_solutions()], which shows each attempt's status,
#' objective, solver and backend. Nothing is chosen for you.
#'
#' The solver's `output/` is left alone. Importing makes it *removable* — it is
#' no longer the only copy — but removing it is [drop_solver_outputs()]'s job,
#' and `cleanup = FALSE` here means this function never does it.
#'
#' **Why this is a verb rather than two calls.** When `read_solution()` cannot
#' read `output/variable_list.csv` it returns the scenario *unchanged*,
#' discarding the run switch. Saving that would write an empty `modOut/`, which
#' flips the run to "imported" and would let the solution's only copy be
#' cleaned up. This refuses to save a read that produced no variables.
#'
#' @param scen a scenario object.
#' @param run character, the run to import (`"<solve>"` or
#'   `"<variant>/<solve>"`).
#' @param ... passed to [read_solution()].
#' @param restore_solver logical; take the run's solver identity (name, lang,
#'   solver, method, formats) from its `solver.csv`. Never the command line.
#' @param save logical; write the result with [save_scenario()]. `FALSE`
#'   returns the scenario with the solution in memory only — which is the
#'   situation this function exists to end, so it is on by default.
#' @param cleanup logical; remove the run's regenerable solver files
#'   afterwards. Off by default.
#' @param verbose logical.
#'
#' @return the scenario, invisibly, with `modOut` populated and `run` active.
#' @seealso [scenario_solutions()], [read_solution()], [drop_solver_outputs()]
#' @export
import_solution <- function(scen, run, ..., restore_solver = TRUE,
                            save = TRUE, cleanup = FALSE, verbose = TRUE) {
  stopifnot(is(scen, "scenario"))
  if (missing(run) || !length(run) || !nzchar(run[1])) {
    stop("`run` is required: name the run to import.\n  Available runs:\n",
         .run_list_hint(scen), call. = FALSE)
  }
  run <- as.character(run[1])
  id <- .parse_run_id(run, scen)
  run_dir <- tryCatch(.run_dir(scen, id$variant, id$solve),
                      error = function(e) NULL)
  if (is.null(run_dir) || !dir.exists(run_dir)) {
    stop("No run '", run, "' in scenario '", scen@name, "'.\n",
         "  Available runs:\n", .run_list_hint(scen), call. = FALSE)
  }

  out <- read_solution(scen, run = run, echo = FALSE, ...)

  # The guard. `read_solution()` hands back the ORIGINAL object when the
  # dump cannot be read, so "no variables" is the signal that nothing was
  # imported -- and saving then would write an empty store over a run whose
  # `output/` is still the only copy.
  nfilled <- sum(vapply(out@modOut@variables, function(v) {
    d <- tryCatch(get_data_slot(v), error = function(e) NULL)
    !is.null(d) && nrow(d) > 0
  }, logical(1)))
  if (nfilled == 0L) {
    stop("Nothing was imported from run '", run, "': its output/ produced no ",
         "variables, so the scenario was left untouched.\n",
         "  scenario_solutions() shows what each run holds; the solver's ",
         "output/ has not been modified.", call. = FALSE)
  }

  if (isTRUE(restore_solver)) {
    sid <- .solver_identity(.run_solver_dir(run_dir))
    if (!is.null(sid)) {
      for (k in names(sid)) out@settings@solver[[k]] <- sid[[k]]
      # regenerated by every backend; never carried between machines
      out@settings@solver$cmdline <- NULL
    }
  }

  if (isTRUE(save)) out <- save_scenario(out, verbose = FALSE)

  if (isTRUE(cleanup)) {
    tryCatch(drop_solver_outputs(out, runs = run, dry_run = FALSE,
                                 verbose = verbose),
             error = function(e) {
               warning("Imported, but the clean-up failed: ",
                       conditionMessage(e), call. = FALSE)
             })
  }

  if (isTRUE(verbose)) {
    obj <- tryCatch(get_variable(out, "vObjective")$value[1],
                    error = function(e) NA_real_)
    message("Imported run '", run, "' of '", out@name, "': ", nfilled,
            " variables", if (!is.na(obj)) paste0(", objective ",
                                                  format(obj, digits = 10)),
            if (isTRUE(save)) "; saved." else
              "; NOT saved (save = FALSE).")
  }
  invisible(out)
}
