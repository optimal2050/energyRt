# Class trade ####
#' An S4 class to represent inter-regional trade
#'
#' @name class-trade
#'
#' @inherit newTrade details
#'
#' @md
#' @slot name `r get_slot_doc("trade", "name")`
#' @slot desc `r get_slot_doc("trade", "desc")`
#' @slot commodity `r get_slot_doc("trade", "commodity")`
#' @slot routes `r get_slot_doc("trade", "routes")`
#' @slot trade `r get_slot_doc("trade", "trade")`
#' @slot aux `r get_slot_doc("trade", "aux")`
#' @slot aeff `r get_slot_doc("trade", "aeff")`
#' @slot invcost `r get_slot_doc("trade", "invcost")`
#' @slot fixom `r get_slot_doc("trade", "fixom")`
#' @slot varom `r get_slot_doc("trade", "varom")`
#' @slot capacity `r get_slot_doc("trade", "capacity")`
#' @slot vintage `r get_slot_doc("trade", "vintage")`
#' @slot cap2act `r get_slot_doc("trade", "cap2act")`
#' @slot optimizeRetirement `r get_slot_doc("trade", "optimizeRetirement")`
#' @slot misc `r get_slot_doc("trade", "misc")`
#'
#' @include class-storage.R
#'
#' @export
#'
setClass("trade",
  representation(
    # General information
    name = "character", # Short name
    desc = "character", # Details
    commodity = "character", # Vector if NULL that
    routes = "data.frame",
    # Performance parameters
    trade = "data.frame",
    aux = "data.frame", #
    aeff = "data.frame", #  Commodity efficiency
    invcost = "data.frame",
    fixom = "data.frame", # !!!ToDo: add fixom
    varom = "data.frame", # !!!ToDO: add varom
    vintage = "data.frame",
    # stock = "data.frame", # !!!ToDo: deprecate (move to @capacity)
    capacity = "data.frame", # !!!ToDo: not implemented yet
    cap2act = "numeric", #
    optimizeRetirement = "logical", # !!!ToDo: add early retirement
    misc = "list"
  ),
  # Default values and structure of slots
  prototype(
    # General information
    name = "", # name in sets
    desc = "",
    commodity = NULL, #
    routes = data.frame(
      src = character(),
      dst = character(),
      stringsAsFactors = FALSE
    ),
    trade = data.frame(
      vintage = character(),
      src = character(),
      dst = character(),
      year = integer(),
      timeslice = character(),
      ava.up = numeric(),
      ava.fx = numeric(),
      ava.lo = numeric(),
      # cost = numeric(), # !!!ToDo: move to varom
      # markup = numeric(), # !!!ToDo: move to varom
      teff = numeric(),
      stringsAsFactors = FALSE
    ),
    fixom = data.frame(
      vintage = character(),
      region = character(),
      year = integer(),
      fixom = numeric(),
      stringsAsFactors = FALSE
    ),
    varom = data.frame(
      vintage = character(),
      src = character(),
      dst = character(),
      year = integer(),
      timeslice = character(),
      varom = numeric(),
      markup = numeric(),
      stringsAsFactors = FALSE
    ),
    # `wacc` overrides the model-wide rate when annuitising this corridor;
    # `payback` shortens the cost-recovery period below `@vintage$olife`; `eac`
    # supplies the annuity directly, bypassing both.
    invcost = data.frame(
      vintage = character(),
      region = character(),
      year = integer(),
      invcost = numeric(),
      wacc = numeric(),
      payback = numeric(),
      eac = numeric(),
      retcost = numeric(),
      stringsAsFactors = FALSE
    ),
    # Lifespan / vintage table, replacing the former `start`/`end`/`olife` slots
    # (whose ToDo notes asked for exactly this consistency with the other
    # processes). One row per vintage; `start`/`end` = user-defined window
    # (NA side = unbounded).
    # `region` and `cluster` are carried for a uniform shape across classes but
    # are unused here: trade has no `@region` slot -- its scope comes from the
    # route endpoints -- and no cluster dimension, since `@routes` already
    # provides multiplicity.
    vintage = data.frame(
      vintage = character(),
      region = character(),
      cluster = character(),
      start = integer(),
      end = integer(),
      olife = integer(),
      stringsAsFactors = FALSE
    ),
    capacity = data.frame(
      vintage = character(),
      # region = character(),
      year = integer(),
      stock = numeric(),
      cap.lo = numeric(),
      cap.up = numeric(),
      cap.fx = numeric(),
      ncap.lo = numeric(),
      ncap.up = numeric(),
      ncap.fx = numeric(),
      ret.lo = numeric(),
      ret.up = numeric(),
      ret.fx = numeric(),
      stringsAsFactors = FALSE
    ),
    aux = data.frame(
      acomm = character(),
      unit = character(),
      stringsAsFactors = FALSE
    ),
    # Auxiliary commodity parameters
    aeff = data.frame(
      vintage = character(),
      acomm = character(),
      src = character(),
      dst = character(),
      year = integer(),
      timeslice = character(),
      csrc2aout = numeric(),
      csrc2ainp = numeric(),
      cdst2aout = numeric(),
      cdst2ainp = numeric(),
      stringsAsFactors = FALSE
    ),
    cap2act = 1, #
    optimizeRetirement = FALSE,
    misc = list()
  ),
  S3methods = FALSE
)

setMethod("initialize", "trade", function(.Object, ...) {
  .Object
})


#' Create new trade object
#'
#' @description Constructor for trade object.
#'
#' @details Trade objects are used to represent inter-regional exchange in the model.
#' Without trade, every region is isolated and can only use its own resources.
#' The class defines trade routes, efficiency, costs,
#' and other parameters related to the process. Number of routes per trade object is not
#' limited. One trade object can have a part or entire trade network of the model.
#' However, it has a distinct name and all the routs will be optimized together.
#' Create separate trade objects to optimize different parts of the trade network
#' (aka transmission lines).
#'
#' @md
#' @param name `r get_slot_doc("trade", "name")`
#' @param desc `r get_slot_doc("trade", "desc")`
#' @param commodity `r get_slot_doc("trade", "commodity")`
#' @param routes `r get_slot_doc("trade", "routes")`
#' @param trade `r get_slot_doc("trade", "trade")`
#' @param fixom `r get_slot_doc("trade", "fixom")`
#' @param varom `r get_slot_doc("trade", "varom")`
#' @param invcost `r get_slot_doc("trade", "invcost")`
#' @param vintage `r get_slot_doc("trade", "vintage")`
#' @param olife deprecated, use the `olife` column of `vintage`.
#' @param start deprecated, use the `start` column of `vintage`.
#' @param end deprecated, use the `end` column of `vintage`.
#' @param capacity `r get_slot_doc("trade", "capacity")`
#' @param aux `r get_slot_doc("trade", "aux")`
#' @param aeff `r get_slot_doc("trade", "aeff")`
#' @param cap2act `r get_slot_doc("trade", "cap2act")`
#' @param optimizeRetirement `r get_slot_doc("trade", "optimizeRetirement")`
#' @param misc `r get_slot_doc("trade", "misc")`
#'
#' @return trade object with given specifications.
#' @export
#' @rdname newTrade
#' @family trade process constructor
#' @examples
#' PIPELINE1 <- newTrade(
#'   name = "PIPELINE1",
#'   desc = "Some transport pipeline",
#'   commodity = "OIL",
#'   routes = data.frame(
#'     src = c("R1", "R2"),
#'     dst = c("R2", "R3")
#'   ),
#'   trade = data.frame(
#'     src = c("R1", "R2"),
#'     dst = c("R2", "R3"),
#'     teff = c(0.99, 0.98)
#'   ),
#'   olife = list(olife = 60)
#' )
#' draw(PIPELINE1)
#'
#' PIPELINE2 <- newTrade(
#'   name = "PIPELINE2",
#'   desc = "Some transport pipeline",
#'   commodity = "OIL",
#'   routes = data.frame(
#'     src = c("R1", "R1", "R2", "R3"),
#'     dst = c("R2", "R3", "R3", "R2")
#'   ),
#'   trade = data.frame(
#'     src = c("R1", "R1", "R2", "R3"),
#'     dst = c("R2", "R3", "R3", "R2"),
#'     teff = c(0.912, 0.913, 0.923, 0.932)
#'   ),
#'   aux = data.frame(
#'     acomm = c("ELC", "CH4"),
#'     unit = c("MWh", "kt")
#'   ),
#'   aeff = data.frame(
#'     acomm = c("ELC", "CH4", "ELC", "CH4"),
#'     src = c("R1", "R1", "R2", "R3"),
#'     dst = c("R2", "R2", "R3", "R2"),
#'     csrc2ainp = c(.5, NA, .3, NA),
#'     cdst2ainp = c(.4, NA, .6, NA),
#'     csrc2aout = c(NA, .1, NA, .2)
#'   ),
#'   olife = list(olife = 60)
#' )
#' draw(PIPELINE2, node = "R1")
#' draw(PIPELINE2, node = "R2")
#' draw(PIPELINE2, node = "R3")
newTrade <- function(
    name = "",
    desc = "",
    commodity = character(),
    routes = data.frame(),
    trade = data.frame(),
    fixom = data.frame(),
    varom = data.frame(),
    invcost = data.frame(),
    olife = data.frame(),
    # `-Inf` / `Inf` mean "no restriction"; `.tech_lifespan_args()` treats an
    # all-infinite bound as absent, so these produce no `vintage` row.
    start = data.frame(start = -Inf, stringsAsFactors = FALSE),
    end = data.frame(end = Inf, stringsAsFactors = FALSE),
    vintage = data.frame(),
    capacity = data.frame(),
    aux = data.frame(),
    aeff = data.frame(),
    cap2act = 1,
    optimizeRetirement = FALSE,
    misc = list(),
    ...
) {
  args <- list(
    desc = desc,
    commodity = commodity,
    routes = routes,
    trade = trade,
    fixom = fixom,
    varom = varom,
    invcost = invcost,
    vintage = vintage,
    olife = olife,
    start = start,
    end = end,
    capacity = capacity,
    aux = aux,
    aeff = aeff,
    cap2act = cap2act,
    optimizeRetirement = optimizeRetirement,
    misc = misc,
    ...
  )
  args <- .trade_removed_args(args)
  args <- .tech_lifespan_args(args)
  do.call(.data2slots, c(list("trade", name), args))
}



# setMethod("newTrade", signature(name = "character"),
#           function(name, ..., source = NULL, destination = NULL, avaUpDef = Inf) {
#   trd <- .data2slots("trade", name, ...)
#   if (avaUpDef != Inf) {
#     trd@trade[nrow(trd@trade) + 1, ] <- NA
#     trd@trade[nrow(trd@trade), "ava.up"] <- avaUpDef
#   }
#   if (is.null(source) != is.null(destination)) {
#     stop('Inconsistency of source/destination data "', trd@name, '"')
#   }
#   if (!is.null(source) && !is.null(list(...)$routes)) {
#     stop('Inconsistency of source/destination with routes data "', trd@name, '"')
#   }
#   if (!is.null(source)) {
#     trd@routes <- merge(
#       data.frame(src = source, stringsAsFactors = FALSE),
#       data.frame(dst = destination, stringsAsFactors = FALSE)
#     )
#     trd@routes <- trd@routes[trd@routes$src != trd@routes$dst, , drop = FALSE]
#   }
#   trd
# })

## Methods ####################################################################
#' Update trade object
#' @rdname update
#' @name update
#'
#' @family trade update
#' @keywords trade update
#' @method trade update
#' @export
setMethod("update", signature(object = "trade"), function(object, ...) {
  args <- .trade_removed_args(list(...))
  args <- .tech_lifespan_args(args)
  do.call(.data2slots, c(list("trade", object), args))
})
