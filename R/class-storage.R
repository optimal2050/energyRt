# Class storage ####
#' An S4 class to represent storage type of technological process.
#'
#' @inherit newStorage description
#' @inherit newStorage details
#'
#' @md
#' @slot name `r get_slot_doc("storage", "name")`
#' @slot desc `r get_slot_doc("storage", "desc")`
#' @slot commodity `r get_slot_doc("storage", "commodity")`
#' @slot aux `r get_slot_doc("storage", "aux")`
#' @slot region `r get_slot_doc("storage", "region")`
#' @slot start `r get_slot_doc("storage", "start")`
#' @slot end `r get_slot_doc("storage", "end")`
#' @slot olife `r get_slot_doc("storage", "olife")`
#' @slot capacity `r get_slot_doc("storage", "capacity")`
#' @slot charge `r get_slot_doc("storage", "charge")`
#' @slot seff `r get_slot_doc("storage", "seff")`
#' @slot af `r get_slot_doc("storage", "af")`
#' @slot aeff `r get_slot_doc("storage", "aeff")`
#' @slot fixom `r get_slot_doc("storage", "fixom")`
#' @slot varom `r get_slot_doc("storage", "varom")`
#' @slot invcost `r get_slot_doc("storage", "invcost")`
#' @slot fullYear `r get_slot_doc("storage", "fullYear")`
#' @slot cap2stg `r get_slot_doc("storage", "cap2stg")`
#' @slot weather `r get_slot_doc("storage", "weather")`
#' @slot optimizeRetirement `r get_slot_doc("storage", "optimizeRetirement")`
#' @slot misc `r get_slot_doc("storage", "misc")`
#'
#' @include class-technology.R
#'
#' @rdname class-storage
#' @family process storage
#'
#' @export
setClass("storage",
  representation(
    name = "character",
    desc = "character",
    commodity = "character", # !!! ToDo: add units
    aux = "data.frame", #
    region = "character",
    start = "data.frame",
    end = "data.frame",
    olife = "data.frame", #
    # stock = "data.frame", #
    capacity = "data.frame", #
    charge = "data.frame", #
    seff = "data.frame", #
    af = "data.frame", # Availability of the resource with prices
    aeff = "data.frame", #  Commodity efficiency
    fixom = "data.frame", #
    varom = "data.frame", #
    invcost = "data.frame",
    fullYear = "logical",
    cap2stg = "numeric", # cap2stg cinp
    weather = "data.frame", # weather condisions multiplier
    optimizeRetirement = "logical",
    misc = "list" #
  ),
  prototype(
    name = "",
    desc = "",
    commodity = "",
    start = data.frame(
      region = character(),
      start = integer(),
      stringsAsFactors = FALSE
    ),
    end = data.frame(
      region = character(),
      end = integer(),
      stringsAsFactors = FALSE
    ),
    olife = data.frame(
      region = character(),
      # year = integer(), # add year to distinguish vintages
      olife = integer(),
      stringsAsFactors = FALSE
    ),
    charge = data.frame(
      region = character(),
      year = integer(),
      slice = character(),
      charge = numeric(),
      stringsAsFactors = FALSE
    ),
    seff = data.frame(
      region = character(),
      year = integer(),
      slice = character(),
      stgeff = numeric(),
      inpeff = numeric(),
      outeff = numeric(),
      stringsAsFactors = FALSE
    ),
    aux = data.frame(
      acomm = character(),
      unit = character(),
      stringsAsFactors = FALSE
    ),
    aeff = data.frame(
      acomm = character(),
      region = character(),
      year = integer(),
      slice = character(),
      stg2ainp = numeric(),
      cinp2ainp = numeric(),
      cout2ainp = numeric(),
      stg2aout = numeric(),
      cinp2aout = numeric(),
      cout2aout = numeric(),
      cap2ainp = numeric(),
      cap2aout = numeric(),
      ncap2ainp = numeric(),
      ncap2aout = numeric(),
      ncap2stg = numeric(),
      stringsAsFactors = FALSE
    ),
    af = data.frame(
      region = character(),
      year = integer(),
      slice = character(),
      af.lo = numeric(),
      af.up = numeric(),
      af.fx = numeric(),
      cinp.up = numeric(),
      cinp.fx = numeric(),
      cinp.lo = numeric(),
      cout.up = numeric(),
      cout.fx = numeric(),
      cout.lo = numeric(),
      stringsAsFactors = FALSE
    ),
    fixom = data.frame(
      region = character(),
      year = integer(),
      fixom = numeric(),
      stringsAsFactors = FALSE
    ),
    varom = data.frame(
      region = character(),
      year = integer(),
      slice = character(),
      inpcost = numeric(),
      outcost = numeric(),
      stgcost = numeric(),
      stringsAsFactors = FALSE
    ),
    invcost = data.frame(
      region = character(),
      year = integer(),
      invcost = numeric(),
      wacc = numeric(),
      eac = numeric(),
      retcost = numeric(),
      stringsAsFactors = FALSE
    ),
    # stock = data.frame(
    #   region = character(),
    #   year = integer(),
    #   stock = numeric(),
    #   stringsAsFactors = FALSE
    # ),
    capacity = data.frame(
      region = character(),
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
    cap2stg = 1,
    fullYear = TRUE,
    region = character(),
    weather = data.frame(
      weather = character(),
      waf.lo = numeric(),
      waf.up = numeric(),
      waf.fx = numeric(),
      wcinp.lo = numeric(),
      wcinp.fx = numeric(),
      wcinp.up = numeric(),
      wcout.lo = numeric(),
      wcout.fx = numeric(),
      wcout.up = numeric(),
      stringsAsFactors = FALSE
    ),
    optimizeRetirement = FALSE,
    misc = list()
  ),
  S3methods = FALSE
)

setMethod("initialize", "storage", function(.Object, ...) {
  .Object
})

#' Create new storage object
#'
#' @description Storage type of technological processes with accumulating capacity of a commodity.
#'
#' @details
#' Storage can be used in combination with other processes, such as
#' technologies, supply, or demand to represent complex technological chains,
#' demand or supply technologies with time-shift.
#' Operation of storage includes accumulation, storing, and release
#' of the stored commodity. The storing cycle operates on the ordered
#' time-slices of the commodity timeframe. The cycle is looped either
#' on an annual basis (last time-slice of a year follows the first time
#' slice of the same year) or within the parent time-frame (for example,
#' when commodity time-frame is "HOUR" and the parent time-frame is "DAY" then
#' the storage cycle will be a calendar day).
#'
#' @param name `r get_slot_doc("storage", "name")`
#' @param desc `r get_slot_doc("storage", "desc")`
#' @param commodity `r get_slot_doc("storage", "commodity")`
#' @param aux `r get_slot_doc("storage", "aux")`
#' @param region `r get_slot_doc("storage", "region")`
#' @param start `r get_slot_doc("storage", "start")`
#' @param end `r get_slot_doc("storage", "end")`
#' @param olife `r get_slot_doc("storage", "olife")`
#' @param charge `r get_slot_doc("storage", "charge")`
#' @param seff `r get_slot_doc("storage", "seff")`
#' @param aeff `r get_slot_doc("storage", "aeff")`
#' @param af `r get_slot_doc("storage", "af")`
#' @param fixom `r get_slot_doc("storage", "fixom")`
#' @param varom `r get_slot_doc("storage", "varom")`
#' @param invcost `r get_slot_doc("storage", "invcost")`
#' @param capacity `r get_slot_doc("storage", "capacity")`
#' @param cap2stg `r get_slot_doc("storage", "cap2stg")`
#' @param fullYear `r get_slot_doc("storage", "fullYear")`
#' @param weather `r get_slot_doc("storage", "weather")`
#' @param optimizeRetirement `r get_slot_doc("storage", "optimizeRetirement")`
#' @param misc `r get_slot_doc("storage", "misc")`
#' @return storage object
#'
#' @name newStorage
#' @family storage process
#' @rdname storage
#' @export
#' @examples
#' STG1 <- newStorage(
#'   name = "STG1",
#'   desc = "Storage description",
#'   commodity = "electricity",
#'   region = "R1",
#'   start = data.frame(region = "R1", start = 0),
#'   end = data.frame(region = "R1", end = 1),
#'   olife = data.frame(region = "R1", olife = 20),
#'   charge = data.frame(
#'     # region = "R1",
#'     year = 2020,
#'     # slice = "HOUR",
#'     charge = 0.1
#'   ),
#'   seff = data.frame(
#'     # region = "R1",
#'     # year = 2020,
#'     # slice = "HOUR",
#'     stgeff = 0.999,
#'     inpeff = 0.9,
#'     outeff = 0.9
#'   ),
#'   aeff = data.frame(
#'     acomm = "electricity",
#'     region = "R1",
#'     year = 2020,
#'     # slice = "HOUR",
#'     stg2ainp = 0.9,
#'     cinp2ainp = 0.1,
#'     cout2ainp = 0.2,
#'     stg2aout = 0.9,
#'     cinp2aout = 0.9,
#'     cout2aout = 0.9,
#'     cap2ainp = 0.9,
#'     cap2aout = 0.9,
#'     ncap2ainp = 0.9,
#'     ncap2aout = 0.9,
#'     ncap2stg = 0.9
#'   ),
#'   af = data.frame(
#'     region = "R1", year = 2020, slice = "HOUR",
#'     af.lo = 0.9, af.up = 0.9, af.fx = 0.9, cinp.up = 0.9,
#'     cinp.fx = 0.9, cinp.lo = 0.9, cout.up = 0.9,
#'     cout.fx = 0.9, cout.lo = 0.9
#'   ),
#'   fixom = data.frame(region = "R1", year = 2020, fixom = 0.9),
#'   varom = data.frame(
#'     region = "R1", year = 2020, slice = "HOUR",
#'     inpcost = 0.9, outcost = 0.9, stgcost = 0.9
#'   ),
#'   invcost = data.frame(
#'     region = "R1", year = 2020, invcost = 0.9,
#'     wacc = 0.9, retcost = 0.9
#'   ),
#'   capacity = data.frame(
#'     region = "R1", year = 2020, stock = 0.9,
#'     cap.lo = 0.9, cap.up = 0.9, cap.fx = 0.9, ncap.lo = 0.9,
#'     ncap.up = 0.9, ncap.fx = 0.9, ret.lo = 0.9, ret.up = 0.9,
#'     ret.fx = 0.9
#'   ),
#'   cap2stg = 1,
#'   fullYear = TRUE,
#'   weather = data.frame(
#'     weather = "sunny",
#'     waf.lo = 0.9,
#'     waf.up = 0.9,
#'     waf.fx = 0.9, wcinp.lo = 0.9,
#'     wcinp.fx = 0.9, wcinp.up = 0.9, wcout.lo = 0.9, wcout.fx = 0.9,
#'     wcout.up = 0.9
#'   ),
#'   optimizeRetirement = FALSE,
#'   misc = list()
#'   )
newStorage <- function(
    name = "",
    desc = "",
    commodity = character(),
    aux = data.frame(),
    region = character(),
    start = data.frame(),
    end = data.frame(),
    olife = data.frame(),
    charge = data.frame(),
    seff = data.frame(),
    aeff = data.frame(),
    af = data.frame(),
    fixom = data.frame(),
    varom = data.frame(),
    invcost = data.frame(),
    capacity = data.frame(),
    cap2stg = 1,
    fullYear = TRUE,
    weather = data.frame(),
    optimizeRetirement = FALSE,
    misc = list(),
    ...
    ) {
  .data2slots(
    "storage",
    name,
    desc = desc,
    commodity = commodity,
    aux = aux,
    region = region,
    start = start,
    end = end,
    olife = olife,
    charge = charge,
    seff = seff,
    aeff = aeff,
    af = af,
    fixom = fixom,
    varom = varom,
    invcost = invcost,
    capacity = capacity,
    cap2stg = cap2stg,
    fullYear = fullYear,
    weather = weather,
    optimizeRetirement = optimizeRetirement,
    misc = misc,
    ...)
}


#' @param object storage object.
#'
#' @rdname update
#' @name update
#'
#' @family storage update
#' @keywords storage update
#' @export
setMethod("update", signature(object = "storage"),
          function(object, ...) {
  # update.storage <- function(obj, ...) {
  .data2slots("storage", object, ...)
})
