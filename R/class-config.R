# class-config ###############################################################
#' An S4 class to represent default model configuration.
#'
#' @description
#' Config class is used to represent the default model configuration.
#' It is stored in the model object and is used to initialize the
#' scenario settings.
#'
#' @name class-config
#'
#' @slot name `r get_slot_doc("config", "name")`
#' @slot desc `r get_slot_doc("config", "desc")`
#' @slot region `r get_slot_doc("config", "region")`
#' @slot calendar `r get_slot_doc("config", "calendar")`
#' @slot horizon `r get_slot_doc("config", "horizon")`
#' @slot discount `r get_slot_doc("config", "discount")`
#' @slot discountFirstYear `r get_slot_doc("config", "discountFirstYear")`
#' @slot optimizeRetirement `r get_slot_doc("config", "optimizeRetirement")`
#' @slot defVal `r get_slot_doc("config", "defVal")`
#' @slot interpolation `r get_slot_doc("config", "interpolation")`
#' @slot debug `r get_slot_doc("config", "debug")`
#' @slot misc `r get_slot_doc("config", "misc")`
#'
#' @include class-calendar.R class-horizon.R
#' @rdname class-config
#' @family class config settings scenario model
#'
#' @export
setClass("config",
  representation(
    name = "character",
    desc = "character",
    region = "character",
    # year = "numeric", # move to horizon
    calendar = "calendar",
    horizon = "horizon", # change to class
    # slice = "slice", #
    # yearFraction = "data.frame",
    discount = "data.frame",
    discountFirstYear = "logical",
    optimizeRetirement = "logical",
    defVal = "data.frame",
    interpolation = "data.frame",
    debug = "data.frame",
    misc = "list"
  ),
  prototype(
    name = "default",
    desc = "model configuration",
    debug = data.frame(
      comm = character(),
      region = character(),
      year = integer(),
      slice = character(),
      dummyImport = numeric(),
      dummyExport = numeric(),
      stringsAsFactors = FALSE
    ),
    discount = data.frame(
      region = character(),
      year = integer(),
      discount = numeric(), # deprecated
      wacc = numeric(), # weighted average cost of capital
      sdr = numeric(), # social discount rate
      stringsAsFactors = FALSE
    ),
    region = NULL,
    # year = as.numeric(2005:2050),
    horizon = new("horizon"),
    calendar = newCalendar(),
    # slice = new("slice"),
    discountFirstYear = FALSE,
    optimizeRetirement = FALSE,
    defVal = data.frame(),
    interpolation = data.frame(),
    # defVal = as.data.frame(.defVal, stringsAsFactors = FALSE),
    # interpolation = as.data.frame(.defInt, stringsAsFactors = FALSE),
    # yearFraction = data.frame(
    #   year = as.numeric(NA),
    #   fraction = as.numeric(1),
    #   stringsAsFactors = FALSE
    # ),
    misc = list()
  ),
  S3methods = FALSE
)
setMethod("initialize", "config", function(.Object, ...) {
  # browser()
  if (!exists(".defVal") || !exists(".modInp") || !exists(".defInt")) {
    load("R/sysdata.rda")
  }
  # if (!is.null()) # add import from .defInt
  .Object@defVal <- as.data.frame(.defVal, stringsAsFactors = FALSE)
  .Object@interpolation <- as.data.frame(.defInt, stringsAsFactors = FALSE)
  .Object
})

# @export
# setGeneric("setTimeSlices", function(obj, ...) standardGeneric("setTimeSlices"))

# @export
# setMethod("setTimeSlices", signature(obj = "config"), function(obj, ...) {
#   obj@slice <- .setTimeSlices(...)
#   obj
# })

# setGeneric("setCalendar", function(obj, ...) standardGeneric("setCalendar"))

## setCalendar ###############################################################
#' @export
setMethod("setCalendar", signature(obj = "config"), function(obj, ...) {
  obj@calendar <- newCalendar(...) ## ToDo: add check for fractional data
  obj
})

# setGeneric("setHorizon",
#            function(obj, horizon, intervals) standardGeneric("setHorizon"))

## setHorizon ###############################################################
#' @param horizon a new horizon object to be set.
#' @method setHorizon config
#'
#' @rdname newHorizon
#'
#' @export
setMethod(
  "setHorizon", signature(obj = "config"), function(obj, period, ...) {
    # browser()
    # obj@horizon <- milestoneYears(start, interval)
    # obj@year <- min(obj@horizon@intervals$start):max(obj@horizon@intervals$end)
    obj@horizon <- newHorizon(period = period, ...)
    obj
  }
)
# setGeneric("getHorizon", function(obj) standardGeneric("getHorizon"))

## getHorizon ###############################################################
#' @export
setMethod("getHorizon", signature(obj = "config"), function(obj) obj@horizon)

#' @rdname newHorizon
#' @family update config
#' @method update config
#' @export
setMethod("update", "config", function(object, ..., warn_nodata = TRUE) {
  # browser()
  # !!! add no-data check for warning
  cf <- .data2slots("config", object, ..., warn_nodata = FALSE)
  cf@calendar <- .data2slots("calendar", cf@calendar, ...,
                             ignore_args = c("name", "desc", "misc"),
                             warn_nodata = FALSE)
  cf@horizon <-  .data2slots("horizon", cf@horizon, ...,
                             ignore_args = c("name", "desc", "misc"),
                             warn_nodata = FALSE)
  cf
})



# setGeneric("milestoneYears",
#            function(start, interval) standardGeneric("milestoneYears"))
#
# setMethod("milestoneYears",
#           signature(start = "numeric", interval = "numeric"),
#           function(start, interval) {
#             browser()
#   if (interval[1] != 1) stop("setMileStoneYears: first interval have to be 1")
#   mlst <- data.frame(
#     start = start + cumsum(c(0, interval[-length(interval)])),
#     mid = rep(NA, length(interval)),
#     end = start + cumsum(interval) - 1
#   )
#   mlst[, "mid"] <- trunc(.5 * (mlst[, "start"] + mlst[, "end"]))
#   mlst
# })


