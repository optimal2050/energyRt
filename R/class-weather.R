#' S4 class to represent weather factors
#' 
#' @name class-weather
#'
#' @inherit newWeather description
#' @inherit newWeather details
#'
#' @md
#' @slot name `r get_slot_doc("weather", "name")`
#' @slot desc `r get_slot_doc("weather", "desc")`
#' @slot unit `r get_slot_doc("weather", "unit")`
#' @slot region `r get_slot_doc("weather", "region")`
#' @slot timeframe `r get_slot_doc("weather", "timeframe")`
#' @slot defVal `r get_slot_doc("weather", "defVal")`
#' @slot weather `r get_slot_doc("weather", "weather")`
#' @slot misc `r get_slot_doc("weather", "misc")`
#'
#' @include class-export.R
#' @family class weather data
#' @rdname class-weather
#'
#' @export
#'
setClass("weather",
  representation(
    name = "character",
    desc = "character",
    unit = "character",
    region = "character",
    timeframe = "character",
    defVal = "numeric",
    weather = "data.frame",
    misc = "list"
  ),
  prototype(
    name = "",
    desc = "",
    unit = as.character(NA),
    region = character(),
    timeframe = character(),
    defVal = 0.,
    weather = data.frame(
      region = character(), #
      year = integer(),
      slice = character(),
      wval = numeric(),
      stringsAsFactors = FALSE
    ),
    misc = list()
  ),
  S3methods = FALSE
)
setMethod("initialize", "weather", function(.Object, ...) {
  .Object
})


#' Create new weather object
#'
#' @description
#' `weather` is a data-carrying class with exogenous shocks
#' used to influence operation of processes in the model.
#'
#' @details
#' Weather factors are separated from the model parameters
#' and can be added or replaced for different scenarios.
#' !!!Additional details...
#'
#' @md
#' @param name `r get_slot_doc("weather", "name")`
#' @param desc `r get_slot_doc("weather", "desc")`
#' @param unit `r get_slot_doc("weather", "unit")`
#' @param region `r get_slot_doc("weather", "region")`
#' @param timeframe `r get_slot_doc("weather", "timeframe")`
#' @param defVal `r get_slot_doc("weather", "defVal")`
#' @param weather `r get_slot_doc("weather", "weather")`
#'
#' @return weather object with given specifications.
#' @export
#' @rdname newWeather
#' @family weather
#'
#' @examples
#' \dontrun{
#' 
#' # use/make time resolution of the model: timetalbe
#' ttbl <- make_timetable(tsl_levels$d365_h24)
#' ttbl
#' 
#' WSOL <- newWeather(
#'   name = "WSOL",
#'   desc = "Horiontal solar PV capacity factor",
#'   timeframe = "HOUR",
#'   defVal = 0.,
#'   weather = data.frame(
#'     region = "R1",
#'     year = 2015, # 
#'     slice = ttbl$slice,
#'     wval = runif(length(ttbl$slice), 0., 1) # use your data
#'   )
#' )
#' }
newWeather <- function(
    name = "",
    desc = "",
    unit = as.character(NA),
    region = character(),
    timeframe = character(),
    defVal = 0.,
    weather = data.frame(),
    misc = list(),
    ...) {
  .data2slots(
    "weather",
    name,
    desc = desc,
    unit = unit,
    region = region,
    timeframe = timeframe,
    defVal = defVal,
    weather = weather,
    misc = misc,
    ...
    )
  }

#' @param object object of class export
#'
#' @param ... slot-names with data to update (see `newWeather`)
#'
#' @rdname newTechnology
#' @family update weather
#' @method update weather
#' @export
setMethod("update", "weather", function(object, ...) {
  .data2slots("weather", object, ...)
})

