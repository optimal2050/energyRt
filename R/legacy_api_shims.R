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

#' Interpolate a model (legacy name; new pipeline)
#'
#' Thin wrapper over [interp_mod()] preserving the legacy `interpolate_model()`
#' entry point. Returns an interpolated scenario.
#' @param object a `model` (or `scenario`, whose `@model` is re-interpolated).
#' @param ... passed to [interp_mod()].
#' @seealso [interp_mod()]
#' @export
interpolate_model <- function(object, ...) {
  args <- list(...)
  if (is.null(args$ondisk)) args$ondisk <- FALSE
  if (is.null(args$fold))   args$fold   <- FALSE
  if (inherits(object, "scenario")) object <- object@model
  do.call(interp_mod, c(list(object), args))
}

setMethod("interpolate", signature(object = "model"),
  function(object, ...) interpolate_model(object, ...))

setMethod("interpolate", signature(object = "scenario"),
  function(object, ...) interpolate_model(object, ...))

#' Solve a model or scenario (legacy names; new pipeline)
#'
#' `solve_model()` interpolates a model (or routes a scenario to
#' `solve_scenario()`) and solves it via [solve_mod()]. `solve_scenario()` solves
#' an interpolated scenario via [solve_scen()] (interpolating first if needed).
#' @param obj a `model` or `scenario`.
#' @param ... passed to [solve_mod()] / [solve_scen()].
#' @seealso [solve_mod()], [solve_scen()]
#' @rdname solve_model
#' @export
solve_model <- function(obj, ...) {
  if (inherits(obj, "scenario")) return(solve_scenario(obj, ...))
  do.call(solve_mod, c(list(obj), list(...)))
}

#' @rdname solve_model
#' @export
solve_scenario <- function(obj, ...) {
  if (inherits(obj, "scenario") && !isTRUE(obj@status$interpolated)) {
    obj <- interpolate_model(obj@model, name = obj@name)
  }
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
