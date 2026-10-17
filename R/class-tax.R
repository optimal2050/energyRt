#' An S4 class to represent a commodity tax
#'
#' @name tax-class
#' @inherit newTax description
#'
#' @slot name `r get_slot_doc("tax", "name")`
#' @slot desc `r get_slot_doc("tax", "desc")`
#' @slot comm `r get_slot_doc("tax", "comm")`
#' @slot geoframe `r get_slot_doc("tax", "geoframe")`
#' @slot region `r get_slot_doc("tax", "region")`
#' @slot process `r get_slot_doc("tax", "process")`
#' @slot process_ `r get_slot_doc("tax", "process_")`
#' @slot classes `r get_slot_doc("tax", "classes")`
#' @slot vintage `r get_slot_doc("tax", "vintage")`
#' @slot vintage_ `r get_slot_doc("tax", "vintage_")`
#' @slot cluster `r get_slot_doc("tax", "cluster")`
#' @slot cluster_ `r get_slot_doc("tax", "cluster_")`
#' @slot variable `r get_slot_doc("tax", "variable")`
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
    geoframe = "character",
    region = "character", #
    process = "character", #
    process_ = "character", #
    classes = "character", #
    vintage = "character", #
    vintage_ = "character", #
    cluster = "character", #
    cluster_ = "character", #
    variable = "character", #
    defVal = "numeric", #
    tax = "data.frame", #
    misc = "list"
  ),
  prototype(
    name = "", # Short name
    comm = "",
    desc = "",
    geoframe = character(),
    region = character(), #
    process = character(), #
    process_ = character(), #
    classes = character(), #
    vintage = character(), #
    vintage_ = character(), #
    cluster = character(), #
    cluster_ = character(), #
    variable = character(), #
    defVal = 0, #
    tax = data.frame(
      region = character(),
      year = integer(),
      timeslice = character(),
      process = character(),
      comm = character(),
      inp = numeric(),
      out = numeric(),
      bal = numeric(),
      act = numeric(),
      cap = numeric(),
      ncap = numeric(),
      rate = numeric(),
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
#' @param geoframe `r get_slot_doc("tax", "geoframe")`
#' @param region `r get_slot_doc("tax", "region")`
#' @param process `r get_slot_doc("tax", "process")`
#' @param process_ `r get_slot_doc("tax", "process_")`
#' @param classes `r get_slot_doc("tax", "classes")`
#' @param vintage `r get_slot_doc("tax", "vintage")`
#' @param vintage_ `r get_slot_doc("tax", "vintage_")`
#' @param cluster `r get_slot_doc("tax", "cluster")`
#' @param cluster_ `r get_slot_doc("tax", "cluster_")`
#' @param variable `r get_slot_doc("tax", "variable")`
#' @param defVal `r get_slot_doc("tax", "defVal")`
#' @param tax `r get_slot_doc("tax", "tax")`
#' @param misc `r get_slot_doc("tax", "misc")`
#'
#' @param ... further named slots of the class, set on the new object.
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
  geoframe = character(),
  region = character(),
  process = character(),
  process_ = character(),
  classes = character(),
  vintage = character(),
  vintage_ = character(),
  cluster = character(),
  cluster_ = character(),
  variable = character(),
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
    geoframe = geoframe,
    region = region,
    process = process,
    process_ = process_,
    classes = classes,
    vintage = vintage,
    vintage_ = vintage_,
    cluster = cluster,
    cluster_ = cluster_,
    variable = variable,
    defVal = defVal,
    tax = tax,
    misc = misc,
    ...
    )
}

# setMethod("newTax", signature(name = "character"), function(name, ...) {
#   .data2slots("tax", name, ...)
# })
