# =============================================================================#
# legacy_api_shims.R -- THE deprecation layer.
#
# Every deprecated public name lives here and nowhere else, so the sunset is a
# single-file delete. Each shim forwards to its replacement and warns through
# `.dep090()`, which states the removal version once and identically.
#
# ADDING a deprecation: move the old name here, give it a `...` signature
# forwarding to the replacement, `@rdname energyRt-deprecated`, and one row in
# the table in that topic's description.
#
# AT v0.90: delete this file, drop the Collate entry, re-document, and move the
# NEWS block from "Deprecations" to "Breaking changes". Nothing else should
# need touching -- keep it that way by never putting LIVE code in this file.
# =============================================================================#

# One voice for the whole layer. `old=` is passed explicitly: the default
# derives it from the CALL, which under do.call() deparses the entire function
# body into the message.
#' @noRd
.dep090 <- function(old, new = NULL) {
  msg <- paste0("`", old, "()` is deprecated and won't be available starting ",
                "energyRt v0.90")
  msg <- if (is.null(new)) paste0(msg, ".") else
    paste0(msg, "; use `", new, "` instead.")
  .Deprecated(msg = msg, old = old)
}

#' Deprecated functions
#'
#' @description
#' These names still work but are scheduled for removal in **energyRt v0.90**;
#' each warns and forwards to its replacement. Update calls to the right-hand
#' column.
#'
#' | deprecated | use instead |
#' |---|---|
#' | `solve_mod()` | [solve_model()] |
#' | `solve_scen()` | [solve_scenario()] |
#' | `register()` | [add_to_registry()] + [save_registry()] |
#' | `get_registry()` | [load_registry()] |
#' | `get_entry()` | [find_in_registry()] |
#' | `get_entry_object()` | [getScenario()] (or [getModel()] / [getRepository()]) |
#' | `find_registry()` | [find_in_registry()] |
#' | `registry_exists()`, `registry.exists()` | `file.exists(get_registry_file())` — they now report whether the project has a registry; `name` is ignored |
#' | `set_default_registry()`, `use_registry()` | [set_registry_file()] |
#' | `which_registry()` | [get_registry_file()] |
#' | `tech_designer()` | [process_designer()] |
#' | `tech_from_spec()` | [process_from_spec()] |
#' | `tech_to_spec()` | [process_to_spec()] |
#' | `tech_spec_code()` | [process_spec_code()] |
#' | `tech_spec_issues()` | [process_spec_issues()] |
#' | `read_techspec()`, `read_procspec()` | [read_process_spec()] |
#' | `write.sc()` | [write_sc()] |
#' | `make_scenario_dirname()` | naming is automatic; override with `set_path_builder(scenario_dir = )` |
#' | `levcost_by_variant(x, what)` | `levcost(x, by_variant = what)` |
#' | `get_data()` | [getData()] |
#' | `get_units()` | [getUnits()] |
#'
#' @param ... arguments passed on to the replacement function.
#' @param x a `levcost_variants` result (for `levcost_by_variant()`).
#' @param what which per-variant table to stack: `"levcost"`, `"npv"` or
#'   `"components"`.
#' @param obj object to register (scenario or model).
#' @param registry ignored (the registry is a file, not an object).
#' @param name character, object name.
#' @param env,obj_name,env_name ignored, kept for backward compatibility.
#'
#' @return whatever the replacement returns.
#' @name energyRt-deprecated
#' @keywords internal
NULL

# -- solve / interpolate ------------------------------------------------------#

#' `interp_mod()` was the working name of the interpolation pipeline, renamed
#' to [interpolate_model()]. Keeps the original `interp_mod()` defaults
#' (`ondisk = TRUE`, `fold = TRUE`); not exported.
#' @keywords internal
#' @noRd
interp_mod <- function(mod, name = NULL, ...,
                       desc = NULL, ondisk = TRUE, overwrite = FALSE,
                       fold = TRUE, sparse = TRUE, prune = TRUE,
                       validate = TRUE, code = NULL,
                       verbose = isVerbose()) {
  .dep090("interp_mod", "interpolate_model")
  interpolate_model(mod, name = name, ..., desc = desc, ondisk = ondisk,
                    overwrite = overwrite, fold = fold, sparse = sparse,
                    prune = prune, validate = validate, code = code,
                    verbose = verbose)
}

#' @rdname energyRt-deprecated
#' @export
solve_mod <- function(...) {
  .dep090("solve_mod", "solve_model")
  solve_model(...)
}

#' @rdname energyRt-deprecated
#' @export
solve_scen <- function(...) {
  .dep090("solve_scen", "solve_scenario")
  solve_scenario(...)
}

# -- registry -----------------------------------------------------------------#
# The in-memory registry (a wrapper over the CRAN `registry` package, archived
# in drafts/registry-cran-wrapper.R) was replaced by the persisted tibble+CSV
# registry in R/registry.R. None of these were usable in released versions (the
# old register() carried a leftover browser()), so they are best-effort
# conveniences, not exact behavior ports.
#
# NOTE: `newRegistry()` is NOT deprecated and does not belong here. It is the
# live constructor -- the only way to make a registry, since `load_registry()`
# refuses to invent one -- and lives in R/registry.R with the other verbs.
# The query shims below use the internal `.registry_open()` so that asking
# about a project with no registry answers "no", rather than erroring.

#' @rdname energyRt-deprecated
#' @export
register <- function(obj, registry = NULL, name = NULL, ...) {
  .dep090("register", "add_to_registry")
  nm <- name %||% obj@name
  type <- if (is(obj, "scenario")) "scenario" else
          if (is(obj, "model")) "model" else
          stop("Only scenario and model objects can be registered; got ",
               class(obj)[1])
  path_ <- tryCatch(obj@path, error = function(e) "")
  reg <- .registry_open()
  reg <- add_to_registry(reg, type, nm, path = path_)
  save_registry(reg)
  invisible(reg)
}

#' @rdname energyRt-deprecated
#' @export
get_registry <- function(...) {
  .dep090("get_registry", "load_registry")
  load_registry()
}

#' @rdname energyRt-deprecated
#' @export
get_entry <- function(name, registry = NULL, ...) {
  .dep090("get_entry", "find_in_registry")
  find_in_registry(.registry_open(), name = name)
}

#' @rdname energyRt-deprecated
#' @export
get_entry_object <- function(name, registry = NULL, ...) {
  .dep090("get_entry_object", "getScenario")
  getScenario(name)
}

#' @rdname energyRt-deprecated
#' @export
find_registry <- function(...) {
  .dep090("find_registry", "find_in_registry")
  find_in_registry(...)
}

# These answered "is NAME registered" in the in-memory era. They now report
# whether the project HAS a registry at all -- `name` is accepted and ignored.
# For the name query use
# `nrow(find_in_registry(load_registry(), name = )) > 0`.
#' @rdname energyRt-deprecated
#' @export
registry.exists <- function(name, env = NULL) {
  .dep090("registry.exists", "file.exists(get_registry_file())")
  file.exists(get_registry_file())
}

#' @rdname energyRt-deprecated
#' @export
registry_exists <- function(name, env = NULL) {
  .dep090("registry_exists", "file.exists(get_registry_file())")
  file.exists(get_registry_file())
}

#' @rdname energyRt-deprecated
#' @export
set_default_registry <- function(obj_name = NULL, env_name = NULL) {
  .dep090("set_default_registry", "set_registry_file")
  invisible(NULL)
}

#' @rdname energyRt-deprecated
#' @export
use_registry <- function(obj_name = NULL, env_name = NULL) {
  .dep090("use_registry", "set_registry_file")
  invisible(NULL)
}

#' @rdname energyRt-deprecated
#' @export
which_registry <- function(...) {
  .dep090("which_registry", "get_registry_file")
  list(file = get_registry_file())
}

# -- process specs (the `tech_*` generation) ----------------------------------#

#' @rdname energyRt-deprecated
#' @export
tech_designer <- function(...) {
  .dep090("tech_designer", "process_designer")
  process_designer(...)
}

#' @rdname energyRt-deprecated
#' @export
tech_from_spec <- function(...) {
  .dep090("tech_from_spec", "process_from_spec")
  process_from_spec(...)
}

#' @rdname energyRt-deprecated
#' @export
tech_to_spec <- function(...) {
  .dep090("tech_to_spec", "process_to_spec")
  process_to_spec(...)
}

#' @rdname energyRt-deprecated
#' @export
tech_spec_code <- function(...) {
  .dep090("tech_spec_code", "process_spec_code")
  process_spec_code(...)
}

#' @rdname energyRt-deprecated
#' @export
tech_spec_issues <- function(...) {
  .dep090("tech_spec_issues", "process_spec_issues")
  process_spec_issues(...)
}

#' @rdname energyRt-deprecated
#' @export
read_techspec <- function(...) {
  .dep090("read_techspec", "read_process_spec")
  read_process_spec(...)
}

#' @rdname energyRt-deprecated
#' @export
read_procspec <- function(...) {
  .dep090("read_procspec", "read_process_spec")
  read_process_spec(...)
}

# -- misc ---------------------------------------------------------------------#

#' @rdname energyRt-deprecated
#' @export
write.sc <- function(...) {
  .dep090("write.sc", "write_sc")
  write_sc(...)
}

#' @rdname energyRt-deprecated
#' @export
levcost_by_variant <- function(x, what = c("levcost", "npv", "components")) {
  .dep090("levcost_by_variant", "levcost(by_variant = )")
  levcost(x, by_variant = match.arg(what))
}

#' @rdname energyRt-deprecated
#' @export
get_data <- function(...) {
  .dep090("get_data", "getData")
  getData(...)
}

#' @rdname energyRt-deprecated
#' @export
get_units <- function(...) {
  .dep090("get_units", "getUnits")
  getUnits(...)
}

# Scenario folders are named by `.scenario_dir_name()`; this export never was
# the live implementation and produced different names. It now delegates to the
# real one (the previous body is archived in
# drafts/make_scenario_dirname-legacy.R). `sep` is accepted and ignored.
#' @rdname energyRt-deprecated
#' @param scen scenario object.
#' @param model_name,calendar_name,horizon_name character, defaults taken from
#'   `scen`.
#' @param prefix,suffix character, added around the derived name.
#' @param sep ignored, kept for backward compatibility.
#' @export
make_scenario_dirname <- function(
    scen,
    name = scen@name,
    model_name = scen@model@name,
    calendar_name = scen@settings@calendar@name,
    horizon_name = scen@settings@horizon@name,
    prefix = NULL,
    suffix = NULL,
    sep = "_") {
  .dep090("make_scenario_dirname",
          "set_path_builder(scenario_dir = ) (naming is automatic)")
  nm <- .scenario_dir_name(name, model_name, calendar_name, horizon_name)
  .path_slug(prefix, nm, suffix)
}
