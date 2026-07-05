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
#' @seealso `interpolate()`, `solve()`, `register()`, `summary()`, `newScenario()`
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
    name = NULL,
    desc = NULL,
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
  .Object
})

#' Generate a new scenario object
#'
#' @param name `r get_slot_doc("scenario", "name")`
#' @param path `r get_slot_doc("scenario", "path")`
#' @param ... any model objects or settings to be assigned to the scenario.
#' @param env_name character. Name of the environment to register the scenario. Default is ".scen". Used only if registry is provided. (in development)
#' @param registry optional registry object to register the scenario. (in development)
#' @param replace logical. If TRUE, replace the entry of the scenario in the registry if the entry already exists. (in development)
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
    env_name = ".scen",
    registry = get_registry(),
    replace = FALSE
) {
  # browser()
  scen <- new("scenario")
  scen@name <- name
  if (!is.null(path)) {
    scen@path <- path
  }
  if (!is.null(registry)) {
    if (registry$has_entry(name, ...)) {
      if (replace) {
        registry$delete_entry(name, ...)
      } else {
        cat("Scenario ", name,
            " already exists in the registry.\n")
        return(invisible(FALSE))
      }
    }
    register(scen, registry, ..., env = env_name)
    cat("Scenario ", name, " created in ", env_name,
        " environment and registered.\n")
    return(invisible(TRUE))
  } else {
    return(scen)
  }
}

# summary <- function(...) UseMethod("summary")

summary.scenario <- function(object, ...) {
  # browser()
  scen <- object
  cat("Scenario:", scen@name, "\n")
  cat("desc:", scen@desc, "\n")
  if (!is.null(scen@model)) cat("Model:", scen@model@name, "\n")
  cat("Interpolated:", scen@status$interpolated, "\n")
  cat("path:", scen@path, "\n")
  if (scen@status$interpolated) {
    if (!is.null(scen@modOut) && scen@modOut@stage == "solved") {
      cat("Solution status: ", ifelse(scen@status$optimal, "", "NOT "), "optimal\n", sep = "")
      # browser()
      vObj <- getData(scen, "vObjective", merge = TRUE)
      cat("vObjective: ", vObj$value, "\n")
      dum <- sum(scen@modOut@variables$vDummyCost$value)
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
#' @name summary
#' @rdname summary
#' @method summary scenario
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
#   obj@model@settings@slice <- .setTimeSlices(...)
#   obj
# })

#' @export
setMethod("setHorizon", signature(obj = "scenario"),
  function(obj, ...) {
    args <- list(...)
    has_h <- sapply(args, function(x) inherits(x, "horizon"))
    # browser()
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

# @export
# setMethod(
#   "setHorizon",
#   signature(obj = "scenario", horizon = "horizon"),
#   function(obj, horizon) {
#     # obj@model <- setHorizon(obj@model, period, intervals)
#     browser()
#     obj
#   }
# )

#' @export
setMethod("getHorizon", signature(obj = "scenario"), function(obj) {
  # getHorizon(obj@model)
  obj@settings@horizon
  # browser()
})

# .modelCode <- list(
#   GAMS = readLines("gams/energyRt.gms"),
#   JuMP = readLines("julia/energyRt.jl"),
#   JuMPOutput = readLines("julia/energyRtOutput.jl"),
#   PYOMOConcrete = readLines("pyomo/energyRtConcrete.py"),
#   PYOMOConcreteOutput = readLines("pyomo/energyRtConcreteOutput.py"),
#   PYOMOAbstract = readLines("pyomo/energyRtAbstract.py"),
#   PYOMOAbstractOutput = readLines("pyomo/energyRtAbstractOutput.py"),
#   GLPK = readLines("glpk/energyRt.mod"),
#   GAMS_output = readLines("gams/output.gms"),
#   checkGAMS = readLines("gams/check.gms"),
#   checkJULIA = readLines("julia/check.jl"),
#   checkPYOMO = readLines("pyomo/check.py"),
#   checkGLPK = readLines("glpk/check.mod")
# )

# @method add scenario
# @rdname add
#
# @return scenario object with added objects.
# @export
# setMethod(
#   "add",
#   "scenario",
#   function(obj, ..., overwrite = FALSE, repo_name = NULL) {
#     stop("'add' method is not implemented yet for scenario.\n",
#          "You can add objects to a new scenario on 'interpolation' stage.")
#   }
# )
