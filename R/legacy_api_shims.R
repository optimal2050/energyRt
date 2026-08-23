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
                       verbose = isVerbose()) {
  # `old = ` explicitly: the default derives it from the CALL, and under
  # do.call() that deparses the whole function body into the warning
  .Deprecated("interpolate_model", old = "interp_mod")
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

# Registry shims ==============================================================#
# The in-memory registry (a wrapper over the CRAN `registry` package, archived
# in drafts/registry-cran-wrapper.R) is replaced by the persisted tibble+CSV
# registry in R/registry.R. These shims keep the old exported names alive with
# deprecation messages; none of them were usable in released versions (the old
# register() contained a leftover browser() call), so the mappings are best-
# effort conveniences, not exact behavior ports.

#' Deprecated registry API
#'
#' @description
#' The in-memory registry has been replaced by a persisted, file-backed
#' registry — see [registry_load()], [registry_add()], [registry_refresh()],
#' and [get_registry_file()]. These names are deprecated:
#' * `newRegistry()` -> [registry_load()]
#' * `register()` -> [registry_add()] + [registry_save()]
#' * `get_registry()` -> [registry_load()]
#' * `getScenario()` -> [load_scenario()] (path resolved via the registry)
#' * `get_entry()` -> [registry_find()]
#' * `set_default_registry()` / `use_registry()` -> [set_registry_file()]
#' * `which_registry()` -> [get_registry_file()]
#'
#' @param name character, object name.
#' @param obj object to register (scenario, model, or repository).
#' @param registry ignored (the registry is now a file, not an object).
#' @param env,... ignored, kept for backward compatibility.
#' @param obj_name,env_name ignored, kept for backward compatibility.
#'
#' @keywords internal
#' @rdname registry-deprecated
#' @export
newRegistry <- function(...) {
  .Deprecated("registry_load")
  registry_load()
}

#' @rdname registry-deprecated
#' @export
register <- function(obj, registry = NULL, name = NULL, ...) {
  .Deprecated("registry_add")
  nm <- name %||% obj@name
  type <- if (is(obj, "scenario")) "scenario" else
          if (is(obj, "model")) "model" else
          stop("Only scenario and model objects can be registered; got ",
               class(obj)[1])
  path_ <- tryCatch(obj@path, error = function(e) "")
  reg <- registry_load()
  reg <- registry_add(reg, type, nm, path = path_)
  registry_save(reg)
  invisible(reg)
}

#' @rdname registry-deprecated
#' @export
get_registry <- function() {
  .Deprecated("registry_load")
  registry_load()
}

#' @rdname registry-deprecated
#' @export
getScenario <- function(name, registry = NULL, ...) {
  .Deprecated("load_scenario")
  reg <- registry_load()
  hit <- registry_find(reg, type = "scenario", name = name)
  if (!nrow(hit)) {
    stop("Scenario '", name, "' is not in the registry (",
         get_registry_file(), "). Run registry_refresh() to rescan, or use ",
         "load_scenario() with an explicit path.")
  }
  load_scenario(fp(dirname(get_registry_file()), hit$path[1]), env = NULL)
}

#' @rdname registry-deprecated
#' @export
get_entry <- function(name, registry = NULL, ...) {
  .Deprecated("registry_find")
  registry_find(registry_load(), name = name)
}

#' @rdname registry-deprecated
#' @export
get_entry_object <- function(name, registry = NULL, ...) {
  .Deprecated("load_scenario")
  getScenario(name)
}

#' @rdname registry-deprecated
#' @export
registry.exists <- function(name, env = NULL) {
  .Deprecated("registry_find")
  nrow(registry_find(registry_load(), name = name)) > 0L
}

#' @rdname registry-deprecated
#' @export
registry_exists <- registry.exists

#' @rdname registry-deprecated
#' @export
set_default_registry <- function(obj_name = NULL, env_name = NULL) {
  .Deprecated("set_registry_file")
  invisible(NULL)
}

#' @rdname registry-deprecated
#' @export
use_registry <- set_default_registry

#' @rdname registry-deprecated
#' @export
which_registry <- function() {
  .Deprecated("get_registry_file")
  list(file = get_registry_file())
}
