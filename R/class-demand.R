#' An S4 class to declare a demand in the model
#' 
#' @name class-demand
#'
#' @md
#' @slot name `r get_slot_doc("demand", "name")`
#' @slot desc `r get_slot_doc("demand", "desc")`
#' @slot commodity `r get_slot_doc("demand", "commodity")`
#' @slot unit `r get_slot_doc("demand", "unit")`
#' @slot dem `r get_slot_doc("demand", "dem")`
#' @slot region `r get_slot_doc("demand", "region")`
#' @slot misc `r get_slot_doc("demand", "misc")`
#'
#' @include class-commodity.R
#' @rdname class-demand
#'
#' @export
setClass("demand",
  representation(
    name = "character",
    desc = "character",
    commodity = "character",
    unit = "character",
    dem = "data.frame",
    region = "character",
    misc = "list"
  ),
  prototype(
    name = "",
    desc = "",
    unit = "",
    region = character(),
    dem = data.frame(
      region = character(),
      year = integer(),
      slice = character(),
      dem = numeric(),
      stringsAsFactors = FALSE
    ),
    misc = list()
  ),
  S3methods = FALSE
)

setMethod("initialize", "demand", function(.Object, ...) {
  .Object
})

#' Create new demand object
#'
#' @param name `r get_slot_doc("demand", "name")`
#' @param desc `r get_slot_doc("demand", "desc")`
#' @param commodity `r get_slot_doc("demand", "commodity")`
#' @param unit `r get_slot_doc("demand", "unit")`
#' @param dem `r get_slot_doc("demand", "dem")`
#' @param region `r get_slot_doc("demand", "region")`
#' @param misc `r get_slot_doc("demand", "misc")`
#'
#' @rdname newDemand
#' @order 1
#' @return demand object with given specifications.
#' @export
#'
#' @examples
#' DSTEEL <- newDemand(
#'  name = "DSTEEL",
#'  desc = "Steel demand",
#'  commodity = "STEEL",
#'  unit = "Mt",
#'  dem = data.frame(
#'     region = "UTOPIA", # NA for every region
#'     year = c(2020, 2030, 2050),
#'     slice = "ANNUAL",
#'     dem = c(100, 200, 300)
#'  ),
#'  region = "UTOPIA", # optional, to narrow the specification of the demand
#'  )
#'  class(DSTEEL)
#'  draw(DSTEEL)
#'
newDemand <- function(
    name = "",
    desc = character(),
    commodity = character(),
    unit = character(),
    dem = data.frame(),
    region = character(),
    misc = list(),
    ...)
{
  .data2slots("demand", name,
    desc = desc,
    commodity = commodity,
    unit = unit,
    dem = dem,
    region = region,
    misc = misc,
    ...
  )
}

#' Update data in a demand object
#'
#' @name update
#' @param object demand object
#'
#' @rdname newDemand
#' @order 2
#' @family demand update
#' @keywords demand update
#' @exportMethod update
setMethod("update", signature(object = "demand"), function(object, ...) {
  .data2slots("demand", object, ...)
})

