#' An S4 class to represent scenario, an interpolated and/or solved model.
#' @name class-scenario
#' @aliases scenario
#'
#' @slot name `r get_slot_doc("scenario", "name")`
#' @slot desc `r get_slot_doc("scenario", "desc")`
#' @slot model `r get_slot_doc("scenario", "model")`
#' @slot settings `r get_slot_doc("scenario", "settings")`
#' @slot modInp `r get_slot_doc("scenario", "modInp")`
#' @slot modOut `r get_slot_doc("scenario", "modOut")`
#' @slot status `r get_slot_doc("scenario", "status")`
#' @slot inMemory `r get_slot_doc("scenario", "inMemory")`
#' @slot path `r get_slot_doc("scenario", "path")`
#' @slot misc `r get_slot_doc("scenario", "misc")`
#'
#' @include class-modOut.R class-settings.R
#' @family class scenario
#'
#' @seealso `interpolate()`, `solve()`, `add_to_registry()`, `summary()`, `newScenario()`
#'
#' @export
setClass("scenario",
  representation(
    name = "character",
    desc = "character",
    model = "model",
    # subset = list(),
    settings = "settings",
    modInp = "modInp",
    modOut = "modOut",
    # source = "list", # Model/scenario source code
    status = "list",
    inMemory = "logical", # reserved
    path = "character", # reserved
    misc = "list"
  ),
  prototype(
    # `character()`, not NULL: the slots are typed `character`, so NULL made
    # every scenario fail `validObject()` on sight
    name = character(),
    desc = character(),
    model = NULL,
    # subset = list(),
    settings = NULL,
    modInp = NULL,
    modOut = NULL,
    # source = list(), # Model source
    # solver = list(),
    status = list(
      interpolated = FALSE,
      script = FALSE,
      optimal = FALSE
    ),
    inMemory = TRUE,
    path = character(),
    misc = list()
  ),
  S3methods = FALSE
)

setMethod("initialize", "scenario", function(.Object, ...) {
  .refuse_initialize_dots("scenario", "newScenario", ...)
  .Object
})

#' Generate a new scenario object
#'
#' @param name `r get_slot_doc("scenario", "name")`
#' @param path `r get_slot_doc("scenario", "path")`
#' @param model the `model` object the scenario is built from; `NULL` leaves
#'   it unset, to be assigned later.
#' @param ... any model objects or settings to be assigned to the scenario.
#' @param env_name,registry,replace deprecated and ignored — the in-memory
#'   registry has been replaced by the persisted project registry (see
#'   [load_registry()]); scenarios are registered on [save_scenario()].
#'
#' @return A new scenario object.
#' @export
#'
#' @examples
#' # It is suggested to use `interpolate(model)` or `solve(model)` to create a new scenario.
newScenario <- function(
    name,
    model = NULL,
    path = fp(get_scenarios_path(), name),
    ...,
    env_name = NULL,
    registry = NULL,
    replace = NULL
) {
  if (!is.null(env_name) || !is.null(registry) || !is.null(replace)) {
    .say("newScenario(): the `env_name`, `registry`, and `replace` ",
            "arguments are deprecated and ignored; scenarios are registered ",
            "in the project registry on save_scenario().")
  }
  # The name becomes a solver symbol and a directory name, so it is validated
  # here as every other object's is.
  .assert_object_name(name, what = "scenario")

  scen <- new("scenario")
  scen@name <- name
  # A scenario returned by this constructor is VALID: the typed slots carry
  # empty objects of their declared classes rather than NULL. `modInp` and
  # `modOut` are built here instead of in the prototype because an empty one
  # costs 270 ms and 88 ms respectively.
  scen@desc <- character()
  scen@model <- new("model")
  scen@settings <- new("settings")
  scen@modInp <- new("modInp")
  scen@modOut <- new("modOut")
  if (!is.null(path)) {
    scen@path <- path
  }
  if (!is.null(model)) {
    # `model` is the second positional parameter, so a stray object passed
    # positionally lands here rather than in `...`. Say so, instead of letting
    # the S4 slot assignment report a type mismatch the caller did not write.
    if (!methods::is(model, "model")) {
      stop("newScenario(): `model` must be a `model` object, not a '",
           class(model)[1], "'. A model object (commodity, technology, ...) ",
           "belongs in the MODEL -- build it with `newModel()` and pass that.",
           call. = FALSE)
    }
    scen@model <- model
  }

  # `...` was documented as "model objects or settings to assign" and then
  # ignored, so `desc` and `misc` passed there vanished. Read what is
  # meaningful and refuse the rest rather than dropping it.
  arg <- list(...)
  if (length(arg) > 0) {
    nms <- names(arg)
    if (is.null(nms)) nms <- rep("", length(arg))
    for (i in seq_along(arg)) {
      v <- arg[[i]]
      if (identical(nms[i], "desc")) {
        scen@desc <- as.character(v)
      } else if (identical(nms[i], "misc")) {
        if (!is.list(v)) stop('"misc" must be a named list', call. = FALSE)
        scen@misc <- v
      } else if (methods::is(v, "settings")) {
        scen@settings <- v
      } else if (methods::is(v, "model")) {
        scen@model <- v
      } else {
        stop("newScenario(): unused argument",
             if (nzchar(nms[i])) paste0(" `", nms[i], "`") else
               paste0(" of class '", class(v)[1], "'"),
             ". A scenario takes `desc`, `misc`, a `model` or a `settings` ",
             "object; model objects are added to the MODEL, and scenarios ",
             "are normally created by `interpolate_model()`.", call. = FALSE)
      }
    }
  }
  scen
}

# summary <- function(...) UseMethod("summary")

summary.scenario <- function(object, ...) {
  scen <- object
  cat("Scenario:", scen@name, "\n")
  cat("desc:", scen@desc, "\n")
  if (!is.null(scen@model)) cat("Model:", scen@model@name, "\n")
  cat("Interpolated:", scen@status$interpolated, "\n")
  cat("path:", scen@path, "\n")
  if (scen@status$interpolated) {
    if (!is.null(scen@modOut) && scen@modOut@stage == "solved") {
      cat("Solution status: ", ifelse(scen@status$optimal, "", "NOT "), "optimal\n", sep = "")
      vObj <- getData(scen, "vObjective", merge = TRUE)
      cat("vObjective: ", vObj$value, "\n")
      # `vDummyCost` has never existed -- the variables are `vDummyImportCost`
      # and `vDummyExportCost`. `NULL$value` is NULL and `sum(NULL)` is 0, so
      # this line has silently never reported anything, and dummy flows (which
      # mask infeasibility) have been invisible in every `summary()`.
      dum <- sum(scen@modOut@variables$vDummyImportCost$value,
                 scen@modOut@variables$vDummyExportCost$value, na.rm = TRUE)
      if (abs(dum) > 0) {
        cat("Dummy import/export costs: ", dum, "\n")
      }
    } else if (is.null(scen@modOut)) { # not solved
      cat("Solution status: not solved\n")
    } else {
      status <- try(scen@modOut@stage)
      if (is(status, "try-error")) status <- "unknown"
      cat("Solution status:", status,"\n")
    }
  }
  cat("Size:", size(scen),"\n")
}
#' Summarize a model or scenario
#'
#' Prints a concise summary of a [model][class-model] or
#' [scenario][class-scenario] object: name, description, contents and — for a
#' solved scenario — the solution status and size.
#'
#' @param object a `model` or `scenario` object.
#' @param ... additional arguments (currently unused).
#' @return The `object`, invisibly; called for its printed summary.
#' @rdname summary
#' @export
setMethod("summary", signature(object = "scenario"),
          definition = summary.scenario)
# setMethod("summary", "scenario", summary.scenario)

## show ####
#' @method show scenario
#' @export
#' @family show print summary
setMethod("show", "scenario", function(object) summary(object))

# @export
# setMethod("setTimeSlices", signature(obj = "scenario"), function(obj, ...) {
#   obj@model@settings@timeslice <- .setTimeSlices(...)
#   obj
# })

#' @export
setMethod("setHorizon", signature(obj = "scenario"),
  function(obj, ...) {
    args <- list(...)
    has_h <- sapply(args, function(x) inherits(x, "horizon"))
    if (any(has_h)) {
      if (sum(has_h) > 1) stop("Only one horizon object is allowed.")
      obj@settings@horizon <- args[[which(has_h)]]
    }
    if (!is.null(args$period) || !is.null(args$intervals)) {
      if (is.null(args$period) && is.null(args$intervals)) {
          stop("Both 'period' and 'intervals' parameters must be provided.")
      }
      obj@settings <- setHorizon(obj@settings, args$period, args$intervals)
    }
    obj
  }
)


#' @export
setMethod("getHorizon", signature(obj = "scenario"), function(obj) {
  # getHorizon(obj@model)
  obj@settings@horizon
})


