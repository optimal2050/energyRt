# =============================================================================#
# promote_solution.R -- the scenario's chosen solution, stored beside modInp.
#
# A solution lives in the run that produced it,
# `runs/[<variant>/]<solve>/modOut/`, and the shell's `@modOut` points there.
# Deleting `runs/` therefore BRICKS the scenario: the paths survive the rebase
# and every read fails with "On-disk data expected but not found".
#
# Promoting copies the chosen run's store to `<scenario>/modOut/`, a sibling of
# `modInp/`, and clears the active run so the rebase resolves there. After
# that the run tree is scratch and can be dropped.
#
# `modOut.yml` travels with the store because `run.yml` is the ONLY home of the
# solve's provenance -- solver, timings, host, the version that produced it --
# and dies with the run folder. It is written LAST, so its presence is also
# what distinguishes a complete store from a half-written one.
# =============================================================================#

#' @include read.R
NULL

.MODOUT_MANIFEST <- "modOut.yml"

.modout_manifest_path <- function(store_dir) fp(store_dir, .MODOUT_MANIFEST)

.modout_manifest_read <- function(store_dir) {
  f <- .modout_manifest_path(store_dir)
  if (!file.exists(f)) return(NULL)
  tryCatch(yaml::read_yaml(f), error = function(e) NULL)
}

# A promoted store is one carrying the manifest. That is the discriminator
# `upgrade_scenario_layout()` needs: a top-level `modOut/` WITHOUT it is the
# layout-2 artifact the upgrader migrates into a run, and one WITH it is a
# deliberate promotion that must be left alone.
.modout_is_promoted <- function(store_dir) {
  !is.null(.modout_manifest_read(store_dir))
}

# Written last, once the payload is in place.
.modout_manifest_write <- function(store_dir, run_id = "", rec = NULL,
                                   stage = "solved", objective = NA_real_,
                                   format = get_storage_format()) {
  vdir <- fp(store_dir, "variables")
  nms <- if (dir.exists(vdir)) {
    sort(basename(list.dirs(vdir, recursive = FALSE)))
  } else {
    character(0)
  }
  mf <- list(
    class = "modOut",
    format = format,
    from_run = as.character(run_id %||% ""),
    promoted = .registry_now(),
    stage = as.character(stage %||% ""),
    objective = if (is.na(objective)) NULL else as.numeric(objective),
    variables = as.list(nms),
    energyRt_version = as.character(utils::packageVersion("energyRt"))
  )
  # Provenance of the SOLVE, copied from the run record before it can be
  # deleted. Row counts are deliberately not recorded: reading every variable
  # to count them would cost as much as the promotion itself, and the
  # manifest-written-last rule already carries completeness.
  if (!is.null(rec)) {
    mf$solver <- list(name = rec$solver_name %||% "", lang = rec$lang %||% "",
                      solver = rec$solver %||% "")
    mf$solved <- list(
      started = rec$started %||% "", finished = rec$finished %||% "",
      duration_sec = rec$duration_sec %||% "",
      mem_mb = rec$mem_mb %||% "", peak_mb = rec$peak_mb %||% "")
    mf$solved_with_version <- rec$energyRt_version %||% ""
    mf$hostname <- rec$hostname %||% ""
    mf$user <- rec$user %||% ""
  }
  mf <- mf[!vapply(mf, is.null, logical(1))]
  yaml::write_yaml(mf, .modout_manifest_path(store_dir))
  invisible(mf)
}

# Move a staged store into place. The previous one is set aside first and
# removed only once the new one has landed, so an interrupted swap leaves one
# of the two, never neither.
.store_swap_in <- function(staging, final) {
  prev <- paste0(final, ".prev")
  unlink(prev, recursive = TRUE, force = TRUE)
  if (dir.exists(final) && !file.rename(final, prev)) {
    stop("Could not replace the store at '", final,
         "': something is holding the folder open. The new one is staged at '",
         staging, "'.", call. = FALSE)
  }
  if (!file.rename(staging, final)) {
    if (dir.exists(prev)) file.rename(prev, final)
    stop("Could not move the staged store into '", final, "'.", call. = FALSE)
  }
  unlink(prev, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

#' Make a run's solution the scenario's own
#'
#' @description
#' Copies an imported run's solution to `<scenario>/modOut/`, beside `modInp/`,
#' and clears the active run so the scenario reads from there. After this the
#' `runs/` tree is scratch: it can be dropped without losing the solution.
#'
#' @details
#' Until a solution is promoted it lives only inside the run that produced it,
#' and deleting that run leaves the scenario pointing at a store that is no
#' longer there — every read then fails. Promotion is what makes the run tree
#' disposable.
#'
#' The solve's provenance travels with the store in `modOut.yml` — solver,
#' objective, stage and timings — because `run.yml` is its only other home and
#' goes with the run folder. The manifest is written last, so its presence
#' also marks the store as complete.
#'
#' Runs are untouched: promoting copies, it does not move. Use
#' [drop_scenario_run()] or [drop_solver_outputs()] afterwards to reclaim the
#' space, and [scenario_solutions()] to choose which run to promote.
#'
#' @section Variants cannot be promoted:
#' An own-problem variant carries its own `modInp`, settings and manifest
#' inside `runs/<variant>/`. Its results belong to a different problem from
#' the scenario's, so lifting only the solution would pair variant results
#' with base-problem parameters. Promote a base-problem run, or keep the
#' variant's run folder.
#'
#' @param scen a scenario object.
#' @param run character, the run to promote (`"<solve>"`). `NULL` (default)
#'   takes the scenario's active run.
#' @param save logical; write the scenario shell afterwards, so a reload
#'   resolves to the promoted store. On by default — without it the store
#'   exists and nothing points at it.
#' @param verbose logical.
#'
#' @return the scenario, invisibly, reading from `<scenario>/modOut/`.
#' @seealso [import_solution()], [scenario_solutions()], [drop_scenario_run()]
#' @examples
#' \dontrun{
#' scen <- import_solution(scen, "glpk", promote = TRUE)
#' drop_scenario_run(scen, "glpk", force = TRUE)   # the run is now scratch
#' }
#' @export
promote_solution <- function(scen, run = NULL, save = TRUE, verbose = TRUE) {
  stopifnot(is(scen, "scenario"))
  # `@misc$run` is the SOLVE label only; the variant lives beside it, so
  # defaulting from the active run has to rejoin the two or a variant-active
  # scenario resolves to a base run that does not exist.
  run <- as.character(
    run %||% .run_id(.run_variant(scen), scen@misc$run %||% ""))
  if (!nzchar(run)) {
    stop("No run to promote: the scenario has no active run and none was ",
         "named.\n  scenario_solutions() lists what it has.", call. = FALSE)
  }
  id <- .parse_run_id(run, scen)
  if (nzchar(id$variant %||% "")) {
    stop("Run '", run, "' belongs to variant '", id$variant,
         "', an own problem: its modInp, settings and manifest live in ",
         "runs/", id$variant, "/, and its results are not comparable with ",
         "the scenario's own problem.\n  Promote a base-problem run, or keep ",
         "that variant's run folder.", call. = FALSE)
  }
  run_dir <- tryCatch(.run_dir(scen, "", id$solve), error = function(e) NULL)
  if (is.null(run_dir) || !dir.exists(run_dir)) {
    stop("No run '", run, "' in scenario '", scen@name, "'.\n",
         "  Available runs:\n", .run_list_hint(scen), call. = FALSE)
  }
  src <- fp(run_dir, "modOut")
  if (!dir.exists(fp(src, "variables"))) {
    stop("Run '", run, "' has no imported solution to promote.\n",
         "  import_solution(scen, \"", run, "\") reads it from the solver's ",
         "output/ first.", call. = FALSE)
  }

  final <- fp(scen@path, "modOut")
  staging <- paste0(final, ".incoming")
  unlink(staging, recursive = TRUE, force = TRUE)
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  .sui_copy_tree(src, staging)
  # a stale manifest copied from the source would claim the wrong provenance
  unlink(.modout_manifest_path(staging), force = TRUE)

  rec <- tryCatch(yaml::read_yaml(fp(run_dir, "run.yml")),
                  error = function(e) NULL)
  obj <- tryCatch(as.numeric(rec$objective %||% NA_real_),
                  error = function(e) NA_real_)
  .modout_manifest_write(staging, run_id = run, rec = rec,
                         stage = rec$stage %||% "solved", objective = obj)
  .store_swap_in(staging, final)

  # Point the object at the promoted store and clear the active run, which is
  # what makes the load-time rebase resolve to `<scenario>/modOut`.
  mo <- .modout_from_store(
    final,
    sets = tryCatch(scen@modOut@sets, error = function(e) list()),
    stage = rec$stage %||% "solved")
  if (!is.null(mo)) scen@modOut <- mo
  scen@misc$run <- ""
  scen@misc$variant <- NULL

  if (isTRUE(save)) scen <- save_scenario(scen, verbose = FALSE)
  if (isTRUE(verbose)) {
    message("Promoted run '", run, "' to '", final, "'. The runs/ tree is now ",
            "scratch: drop_scenario_run() or drop_solver_outputs() reclaim it.")
  }
  invisible(scen)
}
