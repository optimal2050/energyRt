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
#' @slot currency `r get_slot_doc("config", "currency")`
#' @slot region `r get_slot_doc("config", "region")`
#' @slot calendar `r get_slot_doc("config", "calendar")`
#' @slot geoscale `r get_slot_doc("config", "geoscale")`
#' @slot horizon `r get_slot_doc("config", "horizon")`
#' @slot discount `r get_slot_doc("config", "discount")`
#' @slot discountFirstYear `r get_slot_doc("config", "discountFirstYear")`
#' @slot optimizeRetirement `r get_slot_doc("config", "optimizeRetirement")`
#' @slot defVal `r get_slot_doc("config", "defVal")`
#' @slot interpolation `r get_slot_doc("config", "interpolation")`
#' @slot debug `r get_slot_doc("config", "debug")`
#' @slot variant_prefix `r get_slot_doc("config", "variant_prefix")`
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
    currency = "character",
    region = "character",
    calendar = "calendar",
    # Optional `geoscales::GeoScale`. Typed "ANY" rather than "Geoscale"
    # because `geoscales` is a Suggests dependency (it is not on CRAN, and
    # energyRt targets CRAN), so the class does not exist when this file is
    # sourced at build time. `setValidity("config")` below enforces the type.
    geoscale = "ANY",
    horizon = "horizon", # change to class
    discount = "data.frame",
    discountFirstYear = "logical",
    optimizeRetirement = "logical",
    defVal = "data.frame",
    interpolation = "data.frame",
    debug = "data.frame",
    variant_prefix = "character",
    misc = "list"
  ),
  prototype(
    name = "default",
    desc = "model configuration",
    # Unset claims nothing, which is how every model written before this slot
    # behaves. See `.model_currency()`.
    currency = character(),
    debug = data.frame(
      comm = character(),
      region = character(),
      year = integer(),
      timeslice = character(),
      dummyImport = numeric(),
      dummyExport = numeric(),
      stringsAsFactors = FALSE
    ),
    # The model's two rates, by region and year. `wacc` annuitises investment,
    # `sdr` discounts the objective. There is deliberately no `discount` column:
    # `discount` is an argument only, expanded into equal wacc/sdr by
    # `.discount_arg_to_rates()` (R/discount_rates.R) before it reaches here.
    discount = data.frame(
      region = character(),
      year = integer(),
      wacc = numeric(), # weighted average cost of capital
      sdr = numeric(), # social discount rate
      stringsAsFactors = FALSE
    ),
    # `character()`, not NULL: the slot is typed "character", so NULL made
    # every `config` -- and so every `model` -- fail `validObject()` on sight.
    # Every reader goes through `as.character()`, which treats the two
    # identically, so this is a validity fix with no behavioural half.
    region = character(),
    horizon = new("horizon"),
    calendar = newCalendar(),
    geoscale = NULL,
    discountFirstYear = FALSE,
    optimizeRetirement = FALSE,
    # Prefixes inserted into the names of expanded technology variants, e.g.
    # WIND + vintage 2030 + cluster "best" -> "WIND_VIN2030_CLbest". Model-wide,
    # because these become solver set-member names. Kept in sync with
    # `.variant_prefix_default()` (R/variants.R) by a unit test.
    variant_prefix = c(vintage = "_VIN", cluster = "_CL"),
    defVal = data.frame(),
    interpolation = data.frame(),
    misc = list()
  ),
  S3methods = FALSE
)
# `geoscale` is typed "ANY" (see the slot comment above), so its type contract
# is stated here. `geoscales` need not be installed: an S7 object carries a
# plain character class attribute, so `is_geoscale()` is a pure attribute test.
#
# `config`'s `initialize` method does not validate, so this runs only when
# `validObject()` is called explicitly -- but a default `config` now passes it
# (see `region` in the prototype above).
setValidity("config", function(object) {
  if (!methods::.hasSlot(object, "geoscale")) return(TRUE)
  gs <- object@geoscale
  if (is.null(gs) || is_geoscale(gs)) return(TRUE)
  paste0(
    "`config@geoscale` must be a `geoscales::GeoScale` object or NULL, not ",
    class(gs)[1], "."
  )
})

#' Is this a geoscales::GeoScale?
#'
#' Type test that does not require `geoscales` to be installed -- an S7 object
#' carries its class attribute either way. Accepts both the installed class
#' name and the unqualified one used when the package is sourced rather than
#' installed.
#'
#' @param x Any object.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @examples
#' energyRt:::is_geoscale(NULL)
#' @keywords internal
is_geoscale <- function(x) {
  # `geoscales` 0.6 renamed the class Geoscale -> GeoScale (and rebased it on
  # nestedscales::NestedScale). The old spellings stay in the list so a model
  # serialized before the rename still loads.
  inherits(x, c("geoscales::GeoScale", "GeoScale",
                "geoscales::GeoScale", "Geoscale"))
}

setMethod("initialize", "config", function(.Object, ...) {
  .refuse_initialize_dots("config", "newModel", ...)
  if (!exists(".defVal") || !exists(".modInp") || !exists(".defInt")) {
    load("R/sysdata.rda")
  }
  .Object@defVal <- as.data.frame(.defVal, stringsAsFactors = FALSE)
  .Object@interpolation <- as.data.frame(.defInt, stringsAsFactors = FALSE)
  .Object
})

## setCalendar ###############################################################
#' @rdname setGeoscale
#' @export
setMethod("setCalendar", signature(obj = "config"), function(obj, ...) {
  obj@calendar <- newCalendar(...)
  obj
})

## setGeoscale / getGeoscale ##################################################
#' @param geoscale a `geoscales::GeoScale` object, or `NULL` to clear it.
#' @rdname setGeoscale
#' @export
setMethod("setGeoscale", signature(obj = "config"), function(obj, geoscale,
                                                             ...) {
  # The argument is checked here rather than by validating the whole object,
  # so the message names `geoscale` instead of reporting the first invalid
  # slot it happens to find.
  if (!is.null(geoscale) && !is_geoscale(geoscale)) {
    stop("`geoscale` must be a `geoscales::GeoScale` object or NULL, not ",
         class(geoscale)[1], ".", call. = FALSE)
  }
  obj@geoscale <- geoscale
  obj
})

#' @rdname setGeoscale
#' @export
setMethod("getGeoscale", signature(obj = "config"), function(obj, ...) {
  if (!methods::.hasSlot(obj, "geoscale")) return(NULL)
  obj@geoscale
})


## setHorizon ###############################################################
#' @param horizon a new horizon object to be set.
#' @method setHorizon config
#'
#' @rdname newHorizon
#'
#' @export
setMethod(
  "setHorizon", signature(obj = "config"), function(obj, period, ...) {
    obj@horizon <- newHorizon(period = period, ...)
    obj
  }
)

## getHorizon ###############################################################
#' @export
setMethod("getHorizon", signature(obj = "config"), function(obj) obj@horizon)

#' @rdname newHorizon
#' @family update config
#' @method update config
#' @export
setMethod("update", "config", function(object, ..., warn_nodata = TRUE) {
  # `discount` is an argument, not a column: a bare number (or a `discount`
  # column) is expanded into equal `wacc`/`sdr` here, and partial or mixed rate
  # rows are rejected, so `.data2slots()` and everything downstream only ever
  # see the two rates the model actually uses.
  args <- .discount_args(list(...))
  cf <- do.call(.data2slots,
                c(list("config", object), args, list(warn_nodata = FALSE)))
  # Normalise `variant_prefix`: merge a partial override over the previous value
  # and validate. Doing it here (rather than relying on `.data2slots`) also
  # re-materialises the slot on a config serialised before it existed.
  cf@variant_prefix <- .variant_prefix_merge(args[["variant_prefix"]],
                                             .variant_prefix(object))
  cf@calendar <- do.call(.data2slots,
                         c(list("calendar", cf@calendar), args,
                           list(ignore_args = c("name", "desc", "misc"),
                                warn_nodata = FALSE)))
  cf@horizon <- do.call(.data2slots,
                        c(list("horizon", cf@horizon), args,
                          list(ignore_args = c("name", "desc", "misc"),
                               warn_nodata = FALSE)))
  cf
})





