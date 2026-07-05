#' An S4 class to represent a commodity
#' 
#' @name class-commodity
#'
#' @description
#' A commodity is a good or service that is produced and consumed in the model.
#' The commodity class is used to store information about the commodity.
#' All processes in the model operate on commodities, i.e. they either generate,
#' produce, consume, transform, store, or transport commodities.
#' The creation of a commodity object is done with the `newCommodity` function.
#'
#' @md
#' @slot name `r get_slot_doc("commodity", "name")`
#' @slot desc `r get_slot_doc("commodity", "desc")`
#' @slot limtype `r get_slot_doc("commodity", "limtype")`
#' @slot timeframe `r get_slot_doc("commodity", "timeframe")`
#' @slot unit `r get_slot_doc("commodity", "unit")`
#' @slot emis `r get_slot_doc("commodity", "emis")`
#' @slot agg `r get_slot_doc("commodity", "agg")`
#' @slot misc `r get_slot_doc("commodity", "misc")`
#'
#' @rdname class-commodity
#' @family class, commodity
#' @include class-calendar.R
#'
setClass("commodity",
  representation(
    name = "character", # Short name
    desc = "character", # Details
    limtype = "factor",
    timeframe = "character",
    unit = "character",
    emis = "data.frame", # Emission factors
    agg = "data.frame", # Aggregation parameter
    misc = "list"
  ),
  prototype(
    name = character(),
    desc = character(),
    limtype = factor("LO", levels = c("FX", "UP", "LO")),
    timeframe = character(),
    unit = character(),
    agg = data.frame(
      comm = character(),
      unit = character(),
      agg = numeric(),
      stringsAsFactors = FALSE
    ),
    emis = data.frame(
      comm = character(),
      unit = character(),
      emis = numeric(),
      stringsAsFactors = FALSE
    ),
    misc = list()
  ),
  S3methods = FALSE
)

setMethod("initialize", "commodity", function(.Object, ...) {
  .Object
})

#' Create new commodity object
#'
#' @md
#' @param name `r get_slot_doc("commodity", "name")`
#' @param desc `r get_slot_doc("commodity", "desc")`
#' @param limtype `r get_slot_doc("commodity", "limtype")`
#' @param timeframe `r get_slot_doc("commodity", "timeframe")`
#' @param unit `r get_slot_doc("commodity", "unit")`
#' @param agg `r get_slot_doc("commodity", "agg")`
#' @param emis `r get_slot_doc("commodity", "emis")`
#' @param misc `r get_slot_doc("commodity", "misc")`
#'
#' @return commodity object
#' @export
#'
#' @rdname newCommodity
#'
#' @family commodity
#'
#' @examples
#' newCommodity(name = "ELC", desc = "Electricity")
newCommodity <- function(
    name = "",
    desc = "",
    limtype = "LO",
    timeframe = character(),
    unit = character(),
    agg = data.frame(),
    emis = data.frame(),
    misc = list()) {
  .data2slots(
    "commodity",
    name,
    desc = desc,
    limtype = limtype,
    timeframe = timeframe,
    unit = unit,
    agg = agg,
    emis = emis,
    misc = misc
  )
}


#' @method update commodity
#' @family commodity update
#' @export
setMethod("update", signature(object = "commodity"), function(object, ...) {
  # update.supply <- function(obj, ...) {
  .data2slots("commodity", object, ...)
})
