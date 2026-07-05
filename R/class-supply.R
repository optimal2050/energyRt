#' An S4 class to represent a supply of a commodity
#'
#' @slot name `r get_slot_doc("supply", "name")`
#' @slot desc `r get_slot_doc("supply", "desc")`
#' @slot commodity `r get_slot_doc("supply", "commodity")`
#' @slot unit `r get_slot_doc("supply", "unit")`
#' @slot weather `r get_slot_doc("supply", "weather")`
#' @slot reserve `r get_slot_doc("supply", "reserve")`
#' @slot availability `r get_slot_doc("supply", "availability")`
#' @slot region `r get_slot_doc("supply", "region")`
#' @slot misc `r get_slot_doc("supply", "misc")`
#'
#' @include class-demand.R
#'
#' @return supply object with given specifications.
#' @export
setClass("supply",
  representation(
    name = "character",
    desc = "character",
    commodity = "character",
    unit = "character",
    weather = "data.frame",
    reserve = "data.frame",
    availability = "data.frame",
    region = "character",
    misc = "list"
  ),
  prototype(
    name = "",
    desc = "",
    commodity = "",
    unit = "",
    weather = data.frame(
      weather = character(),
      wava.lo = numeric(),
      wava.up = numeric(),
      wava.fx = numeric(),
      stringsAsFactors = FALSE
    ),
    reserve = data.frame(
      region = character(),
      res.lo = numeric(),
      res.up = numeric(),
      res.fx = numeric(),
      stringsAsFactors = FALSE
    ),
    availability = data.frame(
      region = character(),
      year = integer(),
      slice = character(),
      ava.lo = numeric(),
      ava.up = numeric(),
      ava.fx = numeric(),
      cost = numeric(),
      stringsAsFactors = FALSE
    ),
    region = character(),
    misc = list()
  ),
  S3methods = FALSE
)

setMethod("initialize", "supply", function(.Object, ...) {
  .Object
})

#' @title Constructor for supply object.
#' @name newSupply
#' @description
#' Creates an instance of the `supply` class and 
#' initializes it with the given data and parameters.
#'
#' @details
#' The `supply` class is used to add a domestic source of a commodity 
#' to the model, with given reserves, availability, and costs.
#'
#' @md
#' @param name `r get_slot_doc("supply", "name")`
#' @param desc `r get_slot_doc("supply", "desc")`
#' @param commodity `r get_slot_doc("supply", "commodity")`
#' @param unit `r get_slot_doc("supply", "unit")`
#' @param weather `r get_slot_doc("supply", "weather")`
#' @param reserve `r get_slot_doc("supply", "reserve")`
#' @param availability `r get_slot_doc("supply", "availability")`
#' @param region `r get_slot_doc("supply", "region")`
#' @param misc `r get_slot_doc("supply", "misc")`
#'
#' @rdname newSupply
#' @order 1
#' @family supply process
#'
#' @return supply object with given specifications.
#' @export
#'
#' @examples
#' SUP_COA <- newSupply(
#'    name = "SUP_COA",
#'    desc = "Coal supply",
#'    commodity = "COA",
#'    unit = "PJ",
#'    reserve = data.frame(
#'       region = c("R1", "R2", "R3"),
#'       res.up = c(2e5, 1e4, 3e6) # total reserves/deposits
#'    ),
#'    availability = data.frame(
#'       region = c("R1", "R2", "R3"),
#'       year = NA_integer_,
#'       slice = "ANNUAL",
#'       ava.up = c(1e3, 1e2, 2e2), # annual availability
#'       cost = c(10, 20, 30) # cost of the resource (currency per unit)
#'    ),
#'    region = c("R1", "R2", "R3")
#'  )
#' class(SUP_COA)
#' # draw(SUP_COA)
newSupply <- function(
  name = "",
  desc = "",
  commodity = character(),
  unit = character(),
  weather = data.frame(),
  reserve = data.frame(),
  availability = data.frame(),
  region = character(),
  misc = list(),
  ...
  ) {
  .data2slots(
    "supply",
    name,
    desc = desc,
    commodity = commodity,
    unit = unit,
    weather = weather,
    reserve = reserve,
    availability = availability,
    region = region,
    misc = misc,
    ...
    )
}

#' Update supply object
#' @rdname sypply
#' @family supply update
#' @export
setMethod('update', signature(object = 'supply'), function(object, ...) {
# update.supply <- function(obj, ...) {
  .data2slots("supply", object, ...)
})
