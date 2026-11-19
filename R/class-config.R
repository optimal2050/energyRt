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
#' @slot units `r get_slot_doc("config", "units")`
#' @slot region `r get_slot_doc("config", "region")`
#' @slot calendar `r get_slot_doc("config", "calendar")`
#' @slot geoscale `r get_slot_doc("config", "geoscale")`
#' @slot horizon `r get_slot_doc("config", "horizon")`
#' @slot discount `r get_slot_doc("config", "discount")`
#' @slot objweight `r get_slot_doc("config", "objweight")`
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
    units = "data.frame",
    region = "character",
    calendar = "calendar",
    # Optional `geoscales::GeoScale`. Typed "ANY" because it is an S7 class
    # with no S4 definition to name here; `setValidity("config")` below
    # enforces the type.
    geoscale = "ANY",
    horizon = "horizon", # change to class
    discount = "data.frame",
    objweight = "data.frame",
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
    # The model's generic units: what everything is expressed in, and the
    # declared fallback for an object that states nothing. Same column names as
    # an object's `@units`, so the two can be compared directly. Unset claims
    # nothing, which is how every model written before the slot behaves. See
    # `.model_currency()`.
    units = data.frame(
      comm = character(),
      capacity = character(),
      activity = character(),
      use = character(),
      currency = character(),
      stringsAsFactors = FALSE
    ),
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
    # The region half of the objective weight: how much a region's costs count
    # in the objective, beside `horizon@intervals$weight` (the year half). The
    # two MULTIPLY into `pObjWeight[region, year]`, which is why they are two
    # tables and not one -- a single table cannot say "2035 at 0.5 and R2 at
    # 0.6" without the user spelling out 0.3 for the cell. Deliberately NOT a
    # column of `discount`: `cfg@discount` feeds `levcost()`'s own discounting
    # as well as the objective, so a weight placed there would corrupt every
    # levelised cost while the objective still looked right.
    objweight = data.frame(
      region = character(),
      year = integer(),
      weight = numeric(),
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
    # One row, no columns: empty means "no override", and the single row keeps
    # `config@interpolation$pTechAf.up <- "inter"` working, which a 0-row frame
    # rejects. Keys are PARAMETER names, with `.lo` / `.up` for a bound.
    defVal = data.frame(row.names = 1L),
    interpolation = data.frame(row.names = 1L),
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
  inherits(x, c("geoscales::GeoScale", "GeoScale", "Geoscale"))
}

setMethod("initialize", "config", function(.Object, ...) {
  .refuse_initialize_dots("config", "newModel", ...)
  # `@defVal` and `@interpolation` are OVERRIDE tables and start empty: a
  # parameter's default and interpolation rule are declared once, in
  # data-raw/parameters.yml. Seeding them with the whole catalogue (as every
  # version up to 0.91 did) made an untouched table indistinguishable from a
  # deliberate one, and forced a second copy of every default to diff against.
  .Object
})

## setCalendar ###############################################################
#' @rdname setCalendar
#' @export
setMethod("setCalendar", signature(obj = "config"),
          function(obj, calendar = NULL, years = NULL, ...) {
  .set_calendar_on(obj, calendar, years, ...)
})

# The one implementation behind the three methods.
#
# `calendar` takes a calendar OBJECT -- which is what `calendars$d365_h24` and
# `calendar_samples()` hand you, and what `timeslices.R` has documented all
# along -- or is left out, in which case `...` builds one through
# `newCalendar()`, the only thing this used to accept.
#
# With `years`, the calendar is not the model's: it is registered in
# `misc$calendars` and named on those milestone rows of the horizon, which is
# the per-year assignment. Validated at once, because a mismatch found here
# names the call that made it.
#' @noRd
.set_calendar_on <- function(obj, calendar = NULL, years = NULL, ...) {
  cal <- if (methods::is(calendar, "calendar")) calendar else {
    if (!is.null(calendar)) {
      stop("`calendar` must be a calendar object (see `calendars`), or left ",
           "out to build one from the remaining arguments.", call. = FALSE)
    }
    newCalendar(...)
  }
  if (is.null(years)) {
    obj@calendar <- cal
    return(obj)
  }
  years <- as.integer(years)
  if (!length(years) || anyNA(years)) {
    stop("`years` must be one or more milestone years.", call. = FALSE)
  }
  nm <- cal@name
  if (!length(nm) || is.na(nm) || !nzchar(nm)) {
    stop("a calendar assigned to particular years needs a name, which is how ",
         "the horizon refers to it: newCalendar(name = ...) or ",
         "calendar_samples(..., name = ...).", call. = FALSE)
  }
  iv <- data.table::as.data.table(obj@horizon@intervals)
  if (!nrow(iv)) {
    stop("the horizon has no intervals, so there are no years to assign to.",
         call. = FALSE)
  }
  mid <- as.integer(iv$mid)
  unknown <- setdiff(years, mid)
  if (length(unknown)) {
    stop("not a milestone year of this horizon: ",
         paste(unknown, collapse = ", "), "
The milestones are ",
         paste(mid, collapse = ", "), ".", call. = FALSE)
  }
  if (is.null(iv[["calendar"]])) iv$calendar <- NA_character_
  iv$calendar[mid %in% years] <- nm
  obj@horizon@intervals <- iv
  reg <- if (is.list(obj@misc$calendars)) obj@misc$calendars else list()
  reg[[nm]] <- cal
  obj@misc$calendars <- reg
  .check_year_calendars(obj)
  obj
}

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
# A calendar's structure is DERIVED from its timetable: the shares, the frame
# ranks and families, the ancestry and both successor relations are all built by
# `newCalendar()`. `update()` forwards every `...` name into the calendar's
# slots, so these were accepted by construction -- and setting one wrote it
# while the rest went on describing the previous calendar, with the new
# timetable then validated against that stale structure. They are outputs, not
# inputs; the calendar is replaced whole instead.
.config_derived_calendar_slots <- c(
  "timetable", "timeslice_share", "timeframe_rank", "timeslices_in_frame",
  "timeslice_family", "timeslice_ancestry", "next_in_timeframe", "next_in_year")

setMethod("update", "config", function(object, ..., warn_nodata = TRUE) {
  derived <- intersect(names(list(...)), .config_derived_calendar_slots)
  if (length(derived)) {
    stop("update(config): ", paste0("`", derived, "`", collapse = ", "), " ",
         if (length(derived) == 1L) "is" else "are",
         " derived from the calendar's timetable and cannot be set here.
",
         "  Build the calendar and attach it instead: ",
         "setCalendar(obj, newCalendar(timetable = ..., name = ...)).",
         call. = FALSE)
  }
  if (any(.horizon_derived_from_period %in% names(list(...)))) {
    stop("update(config): `period` derives the horizon's `intervals`, so it ",
         "cannot be set here -- the milestones would go on naming the old ",
         "years.
  Build the horizon and attach it instead: ",
         "setHorizon(obj, period = ...).", call. = FALSE)
  }
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





