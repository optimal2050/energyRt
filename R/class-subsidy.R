#' An S4 class to represent a commodity subsidy
#'
#' @name subsidy-class
#'
#' @inherit newSubsidy description
#'
#' @md
#' @slot name `r get_slot_doc("subsidy", "name")`
#' @slot desc `r get_slot_doc("subsidy", "desc")`
#' @slot comm `r get_slot_doc("subsidy", "comm")`
#' @slot geoframe `r get_slot_doc("subsidy", "geoframe")`
#' @slot region `r get_slot_doc("subsidy", "region")`
#' @slot process `r get_slot_doc("subsidy", "process")`
#' @slot process_ `r get_slot_doc("subsidy", "process_")`
#' @slot classes `r get_slot_doc("subsidy", "classes")`
#' @slot vintage `r get_slot_doc("subsidy", "vintage")`
#' @slot vintage_ `r get_slot_doc("subsidy", "vintage_")`
#' @slot cluster `r get_slot_doc("subsidy", "cluster")`
#' @slot cluster_ `r get_slot_doc("subsidy", "cluster_")`
#' @slot variable `r get_slot_doc("subsidy", "variable")`
#' @slot defVal `r get_slot_doc("subsidy", "defVal")`
#' @slot subsidy `r get_slot_doc("subsidy", "subsidy")`
#' @slot misc `r get_slot_doc("subsidy", "misc")`
#'
#' @export
#' @family class constraint policy
#'
#' @include class-tax.R
#' @rdname class-subsidy
#'
setClass("subsidy",
  representation(
    # General information
    name = "character",
    desc = "character",
    comm = "character",
    geoframe = "character",
    region = "character",
    process = "character",
    process_ = "character",
    classes = "character",
    vintage = "character",
    vintage_ = "character",
    cluster = "character",
    cluster_ = "character",
    variable = "character",
    defVal = "numeric",
    subsidy = "data.frame",
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
    subsidy = data.frame(
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
      stg.cap = numeric(),
      stg.ncap = numeric(),
      inp.cap = numeric(),
      inp.ncap = numeric(),
      out.cap = numeric(),
      out.ncap = numeric(),
      rate = numeric(),
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
#' @param name `r get_slot_doc("subsidy", "name")`
#' @param desc `r get_slot_doc("subsidy", "desc")`
#' @param comm `r get_slot_doc("subsidy", "comm")`
#' @param geoframe `r get_slot_doc("subsidy", "geoframe")`
#' @param region `r get_slot_doc("subsidy", "region")`
#' @param process `r get_slot_doc("subsidy", "process")`
#' @param process_ `r get_slot_doc("subsidy", "process_")`
#' @param classes `r get_slot_doc("subsidy", "classes")`
#' @param vintage `r get_slot_doc("subsidy", "vintage")`
#' @param vintage_ `r get_slot_doc("subsidy", "vintage_")`
#' @param cluster `r get_slot_doc("subsidy", "cluster")`
#' @param cluster_ `r get_slot_doc("subsidy", "cluster_")`
#' @param variable `r get_slot_doc("subsidy", "variable")`
#' @param defVal `r get_slot_doc("subsidy", "defVal")`
#' @param subsidy `r get_slot_doc("subsidy", "subsidy")`
#' @param misc `r get_slot_doc("subsidy", "misc")`
#'
#' @return An object of class `subsidy`
#' @family class constraint policy
#' @export
#' @param ... further named slots of the class, set on the new object.
#' @rdname newSubsidy
#'
#' @examples
#' SUB_BIO <- newSubsidy(
#'  name = "SUB_BIO", # used in sets
#'  desc = "Biofuel consumption subsidy", # for own reference
#'  comm = "BIO", # must match the commodity name in the model
#'  region = "R1", # region where the subsidy is applied
#'  defVal = 0, # default value
#'  subsidy = data.frame(
#'     # region = "R1",
#'     year = 2025:2030,
#'     inp = 0.9 # subsidy rate
#'    )
#'  )
newSubsidy <- function(
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
  subsidy = data.frame(),
  misc = list(),
  ...
  ) {
  .data2slots(
    "subsidy",
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
    subsidy = subsidy,
    misc = misc,
    ...
    )
}


# setMethod("newSub", signature(name = "character"), function(name, ..., value = NULL) {
#   .data2slots("sub", name, ...)
# })
