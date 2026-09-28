#' An S4 class to declare a demand in the model
#' 
#' @name class-demand
#'
#' @md
#' @slot name `r get_slot_doc("demand", "name")`
#' @slot desc `r get_slot_doc("demand", "desc")`
#' @slot commodity `r get_slot_doc("demand", "commodity")`
#' @slot unit `r get_slot_doc("demand", "unit")`
#' @slot demand `r get_slot_doc("demand", "demand")`
#' @slot cluster `r get_slot_doc("demand", "cluster")`
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
    demand = "data.frame",
    cluster = "data.frame",
    region = "character",
    misc = "list"
  ),
  prototype(
    name = "",
    desc = "",
    unit = "",
    region = character(),
    demand = data.frame(
      cluster = character(),
      region = character(),
      year = integer(),
      timeslice = character(),
      demand = numeric(),
      stringsAsFactors = FALSE
    ),
    # Declaration of the demand's parallel parts. For demand a cluster is a
    # SUB-REGIONAL SHARE: the object carries one coarse total and each cluster
    # takes a fixed fraction of it in one finer region. Variant expansion mints
    # one `dem` set member per cluster, and `pDemand` is indexed by `dem`, so
    # the parts are summed back by `eqDemInp` -- the coarse figure is the
    # AGGREGATE of its children and stays the number you edit.
    #
    # Only `dem.share.fx`, and deliberately so. A `lo`/`up` split would need
    # the LP to CHOOSE where load sits, and there is no variable indexed by
    # `dem` for such a bound to act on: `pDemand` sits on the right-hand side
    # of an equality. Fixed is what the data supports.
    #
    # Shares are fractions of THIS object's total and must sum to 1, so one
    # demand object is one coarse region -- several zones are several objects,
    # the same way one `lossTranches()` object is one line.
    cluster = data.frame(
      cluster = character(),
      desc = character(),
      region = character(),
      dem.share.fx = numeric(),
      order = integer(),
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
#' @param demand `r get_slot_doc("demand", "demand")`
#' @param cluster `r get_slot_doc("demand", "cluster")`
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
#'  demand = data.frame(
#'     region = "TOPIA", # NA for every region
#'     year = c(2020, 2030, 2050),
#'     timeslice = "ANNUAL",
#'     demand = c(100, 200, 300)
#'  ),
#'  region = "TOPIA", # optional, to narrow the specification of the demand
#'  )
#'  class(DSTEEL)
#'  draw(DSTEEL)
#'
newDemand <- function(
    name = "",
    desc = character(),
    commodity = character(),
    unit = character(),
    demand = data.frame(),
    cluster = data.frame(),
    region = character(),
    misc = list(),
    ...)
{
  .data2slots("demand", name,
    desc = desc,
    commodity = commodity,
    unit = unit,
    demand = demand,
    cluster = cluster,
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

