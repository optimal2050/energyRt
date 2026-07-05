#' An S4 class to represent a commodity subsidy
#'
#' @name subsidy-class
#'
#' @inherit newSubsidy description
#'
#' @md
#' @slot name `r get_slot_doc("sub", "name")`
#' @slot desc `r get_slot_doc("sub", "desc")`
#' @slot comm `r get_slot_doc("sub", "comm")`
#' @slot region `r get_slot_doc("sub", "region")`
#' @slot defVal `r get_slot_doc("sub", "defVal")`
#' @slot sub `r get_slot_doc("sub", "sub")`
#' @slot misc `r get_slot_doc("sub", "misc")`
#'
#' @export
#' @family class constraint policy
#'
#' @include class-tax.R
#' @rdname class-subsidy
#'
setClass("sub",
  representation(
    # General information
    name = "character",
    desc = "character",
    comm = "character",
    region = "character",
    defVal = "numeric",
    sub = "data.frame",
    misc = "list"
  ),
  # !!! Rename to "subsidy"
  # !!! add slot "sub" = data.frame(comm, year, slice, sub)
  # !!! rename sub -> subsidy
  # !!! add slot @variable = factor("output", "balance")
  prototype(
    name = "", # Short name
    comm = "",
    desc = "",
    region = character(), #
    defVal = 0, #
    sub = data.frame(
      region = character(),
      year = integer(),
      slice = character(),
      inp = numeric(),
      out = numeric(),
      bal = numeric(),
      stringsAsFactors = FALSE
    ),
    misc = list()
  ),
  S3methods = FALSE
)

# setGeneric("newSub", function(name, ...) standardGeneric("newSub"))

#' @title Create a new subsidy object
#' @name newSubsidy
#'
#' @description
#' Subsidies are used to represent the financial support provided to
#' production, consumption, or balance of a commodity.
#'
#' @param name `r get_slot_doc("sub", "name")`
#' @param desc `r get_slot_doc("sub", "desc")`
#' @param comm `r get_slot_doc("sub", "comm")`
#' @param region `r get_slot_doc("sub", "region")`
#' @param defVal `r get_slot_doc("sub", "defVal")`
#' @param sub `r get_slot_doc("sub", "sub")`
#' @param misc `r get_slot_doc("sub", "misc")`
#'
#' @return An object of class `sub`
#' @family class constraint policy
#' @export
#' @aliases newSubsidy
#' @rdname newSubsidy
#'
#' @examples
#' SUB_BIO <- newSub(
#'  name = "SUB_BIO", # used in sets
#'  desc = "Biofuel consumption subsidy", # for own reference
#'  comm = "BIO", # must match the commodity name in the model
#'  region = "R1", # region where the subsidy is applied
#'  defVal = 0, # default value
#'  sub = data.frame(
#'     # region = "R1",
#'     year = 2025:2030,
#'     inp = 0.9 # subsidy rate
#'    )
#'  )
newSub <- function(
  name,
  desc = "",
  comm = "",
  region = character(),
  defVal = 0,
  sub = data.frame(),
  misc = list(),
  ...
  ) {
  .data2slots(
    "sub",
    name,
    desc = desc,
    comm = comm,
    region = region,
    defVal = defVal,
    sub = sub,
    misc = misc,
    ...
    )
}

#' @export
#' @noRd
newSubsidy <- newSub

# setMethod("newSub", signature(name = "character"), function(name, ..., value = NULL) {
#   .data2slots("sub", name, ...)
# })
