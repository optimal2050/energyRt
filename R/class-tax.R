#' An S4 class to represent a commodity tax
#'
#' @name tax-class
#' @inherit newTax description
#'
#' @slot name `r get_slot_doc("tax", "name")`
#' @slot desc `r get_slot_doc("tax", "desc")`
#' @slot comm `r get_slot_doc("tax", "comm")`
#' @slot region `r get_slot_doc("tax", "region")`
#' @slot defVal `r get_slot_doc("tax", "defVal")`
#' @slot tax `r get_slot_doc("tax", "tax")`
#' @slot misc `r get_slot_doc("tax", "misc")`
#'
#' @include class-weather.R
#' @family class constraint policy
#' @rdname class-tax
#' @export
setClass("tax",
  representation(
    name = "character", #
    desc = "character", #
    comm = "character", #
    region = "character", #
    defVal = "numeric", #
    tax = "data.frame", #
    misc = "list"
  ),
  # !!! add slot "tax" = data.frame(comm, year, slice, tax)
  # !!! add slot @variable = factor("output", "balance")
  prototype(
    name = "", # Short name
    comm = "",
    desc = "",
    region = character(), #
    defVal = 0, #
    tax = data.frame(
      region = character(),
      year = integer(),
      slice = character(),
      inp = numeric(),
      out = numeric(),
      bal = numeric(),
      stringsAsFactors = FALSE
    ),
    # ! Misc
    misc = list()
  ),
  S3methods = FALSE
)

# setGeneric("newTax", function(name, ...) standardGeneric("newTax"))
#' @title Create a new tax object
#' @name newTax
#'
#' @description
#' Taxes are used to represent the financial levy imposed on production,
#' consumption, or balance of a commodity.
#'
#' @param name `r get_slot_doc("tax", "name")`
#' @param desc `r get_slot_doc("tax", "desc")`
#' @param comm `r get_slot_doc("tax", "comm")`
#' @param region `r get_slot_doc("tax", "region")`
#' @param defVal `r get_slot_doc("tax", "defVal")`
#' @param tax `r get_slot_doc("tax", "tax")`
#' @param misc `r get_slot_doc("tax", "misc")`
#'
#' @return An object of class `tax`
#' @family class constraint policy
#' @rdname newTax
#' @export
#'
#' @examples
#' CO2TAX <- newTax(
#'  name = "CO2TAX",
#'  desc = "Tax on net CO2 emissions",
#'  comm = "CO2",
#'  region = "R1",
#'  defVal = 0,
#'  tax = data.frame(
#'  # region = "R1", # not required when @region is set
#'  year = c(2030, 2040, 2050),
#'  bal =  c(10, 50, 200) # $10, $50, $200 per ton, will be interpolated
#'  # out = ... use to tax output commodity
#'  # inp = ... use to tax input commodity
#'    ),
#'  misc = list(
#'   source = "https://www.example.com/tax"
#'   )
#'  )
newTax <- function(
  name,
  desc = "",
  comm = "",
  region = character(),
  defVal = 0,
  tax = data.frame(),
  misc = list(),
  ...
  ) {
  .data2slots(
    "tax",
    name,
    desc = desc,
    comm = comm,
    region = region,
    defVal = defVal,
    tax = tax,
    misc = misc,
    ...
    )
}

# setMethod("newTax", signature(name = "character"), function(name, ...) {
#   .data2slots("tax", name, ...)
# })
