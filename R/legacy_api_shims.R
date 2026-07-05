# =============================================================================#
# legacy_api_shims.R
#
# The public legacy names `interpolate_model()` / `solve_model()` /
# `solve_scenario()` and the `interpolate` / `solve` S4 methods, repurposed as
# thin wrappers over the NEW mapping pipeline (`interp_mod()` / `solve_mod()` /
# `solve_scen()`). The legacy implementations are archived in
# `depreciated/R/` (interpolate.R, obj2modInp.R, add2set.R, interpolate2.R,
# solve_legacy.R).
#
# Semantics are kept close to the legacy ones: `interpolate*()` returns an
# interpolated scenario; `solve*()` interpolates (if needed) and solves, reusing
# the shared write/run/read framework in R/solve.R. Because they are built on the
# new engine, edge-case behaviour can differ (the tests / utopia vignette are
# being migrated separately).
#
# Legacy in-memory/unfolded behaviour is the default (`ondisk = FALSE`,
# `fold = FALSE`) so the writers see explicit parameter rows; callers can override
# via `...`.
# =============================================================================#

#' Deprecated: `interp_mod()` is now [interpolate_model()]
#'
#' @description
#' `interp_mod()` was the working name of the new interpolation pipeline; it has
#' been renamed to [interpolate_model()] (same engine). This alias keeps the
#' original `interp_mod()` defaults (`ondisk = TRUE`, `fold = TRUE`) for backward
#' compatibility and issues a one-time deprecation message.
#' @inheritParams interpolate_model
#' @return an interpolated scenario (see [interpolate_model()]).
#' @keywords internal
#' @noRd
interp_mod <- function(mod, name = NULL, ...,
                       desc = NULL, ondisk = TRUE, overwrite = FALSE,
                       fold = TRUE, sparse = TRUE, prune = TRUE,
                       validate = TRUE, code = NULL,
                       verbose = getOption("energyRt.verbose", FALSE)) {
  .Deprecated("interpolate_model")
  interpolate_model(mod, name = name, ..., desc = desc, ondisk = ondisk,
                    overwrite = overwrite, fold = fold, sparse = sparse,
                    prune = prune, validate = validate, code = code,
                    verbose = verbose)
}

setMethod("interpolate", signature(object = "model"),
  function(object, ...) interpolate_model(object, ...))

setMethod("interpolate", signature(object = "scenario"),
  function(object, ...) interpolate_model(object, ...))

#' Solve a model or scenario (legacy names; new pipeline)
#'
#' `solve_model()` is the "do everything" entry point: it interpolates a model
#' via [interpolate_model()] and solves it (or, given an un-interpolated
#' scenario, interpolates it first), then routes to [solve_mod()] / [solve_scen()].
#' `solve_scenario()` **expects an already-interpolated scenario** and only solves
#' it via [solve_scen()]; it does **not** re-interpolate (an un-interpolated
#' scenario is an error pointing to `solve_model()` / `interpolate_model()`).
#' @param obj a `model` or `scenario`.
#' @param ... passed to [solve_mod()] / [solve_scen()].
#' @seealso [solve_mod()], [solve_scen()]
#' @rdname solve_model
#' @export
solve_model <- function(obj, ...) {
  if (inherits(obj, "scenario")) {
    # convenience: interpolate an un-interpolated scenario before solving. An
    # already-interpolated scenario is solved as-is (its build knobs preserved).
    if (!isTRUE(obj@status$interpolated)) {
      obj <- interpolate_model(obj@model, name = obj@name)
    }
    return(do.call(solve_scen, c(list(obj), list(...))))
  }
  do.call(solve_mod, c(list(obj), list(...)))
}

#' @rdname solve_model
#' @export
solve_scenario <- function(obj, ...) {
  # Expects an interpolated scenario: delegate straight to solve_scen(), which
  # errors (pointing to solve_model() / interpolate_model()) if it is not
  # interpolated. Never silently re-interpolates (which would use default build
  # args, not the scenario's original sparse/fold/prune settings).
  do.call(solve_scen, c(list(obj), list(...)))
}

.solve_model_method   <- function(a, b, ...) solve_model(a, ...)
.solve_scenario_method <- function(a, b, ...) solve_scenario(a, ...)

setMethod("solve", signature(a = "model", b = "character"), .solve_model_method)
setMethod("solve", signature(a = "model", b = "missing"),   .solve_model_method)
setMethod("solve", signature(a = "scenario", b = "character"), .solve_scenario_method)
setMethod("solve", signature(a = "scenario", b = "missing"),   .solve_scenario_method)
setMethod("solve", signature(a = "missing", b = "missing"), function(...) {
  arg <- list(...)
  if (is.null(arg$obj)) return(do.call(NextMethod, arg))
  if (is(arg$obj, "scenario")) return(do.call(solve_scenario, arg))
  if (is(arg$obj, "model"))    return(do.call(solve_model, arg))
  NextMethod(arg)
})
