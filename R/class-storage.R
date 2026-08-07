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
#' @slot cluster `r get_slot_doc("storage", "cluster")`
#' @slot vintage `r get_slot_doc("storage", "vintage")`
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
    cluster = "data.frame",
    vintage = "data.frame",
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
    cap2stg = "data.frame", # energy-to-power ratio; varies by cluster (duration)
    weather = "data.frame", # weather condisions multiplier
    optimizeRetirement = "logical",
    misc = "list" #
  ),
  prototype(
    name = "",
    desc = "",
    commodity = "",
    # Cluster declaration -- see the `technology` class for the full rationale.
    # For storage the motivating case is site-constrained capacity: pumped-hydro
    # head classes, CAES caverns, or duration classes via per-cluster `cap2stg`.
    cluster = data.frame(
      cluster = character(),
      desc = character(),
      region = character(),
      order = integer(),
      stringsAsFactors = FALSE
    ),
    # Lifespan / vintage table, replacing the former `start`/`end`/`olife` slots
    # (which is what the old `# year = integer(), # add year to distinguish
    # vintages` note here was reaching for). One row per (vintage, region,
    # cluster); `start`/`end` default to the vintage year.
    vintage = data.frame(
      vintage = character(),
      region = character(),
      cluster = character(),
      start = integer(),
      end = integer(),
      olife = integer(),
      stringsAsFactors = FALSE
    ),
    charge = data.frame(
      vintage = character(),
      cluster = character(),
      region = character(),
      year = integer(),
      slice = character(),
      charge = numeric(),
      stringsAsFactors = FALSE
    ),
    seff = data.frame(
      vintage = character(),
      cluster = character(),
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
      vintage = character(),
      cluster = character(),
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
      vintage = character(),
      cluster = character(),
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
      vintage = character(),
      cluster = character(),
      region = character(),
      year = integer(),
      fixom = numeric(),
      stringsAsFactors = FALSE
    ),
    varom = data.frame(
      vintage = character(),
      cluster = character(),
      region = character(),
      year = integer(),
      slice = character(),
      inpcost = numeric(),
      outcost = numeric(),
      stgcost = numeric(),
      stringsAsFactors = FALSE
    ),
    # `wacc` overrides the model-wide rate when annuitising this storage;
    # `payback` shortens the cost-recovery period below `@vintage$olife`; `eac`
    # supplies the annuity directly, bypassing both.
    invcost = data.frame(
      vintage = character(),
      cluster = character(),
      region = character(),
      year = integer(),
      invcost = numeric(),
      wacc = numeric(),
      payback = numeric(),
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
      vintage = character(),
      cluster = character(),
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
    # Energy-to-power ratio. A data.frame (not a scalar) so it can differ by
    # cluster: 1h / 4h / 8h duration classes of the same battery. The scalar
    # shorthand `cap2stg = 4` still works via `.data2slots()`, because the slot
    # carries a column of its own name.
    cap2stg = data.frame(
      vintage = character(),
      cluster = character(),
      region = character(),
      year = integer(),
      cap2stg = numeric(),
      stringsAsFactors = FALSE
    ),
    fullYear = TRUE,
    region = character(),
    weather = data.frame(
      vintage = character(),
      cluster = character(),
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
#' @param cluster `r get_slot_doc("storage", "cluster")`
#' @param vintage `r get_slot_doc("storage", "vintage")`
#' @param start deprecated, use the `start` column of `vintage`.
#' @param end deprecated, use the `end` column of `vintage`.
#' @param olife deprecated, use the `olife` column of `vintage`.
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
#'     wacc = 0.09, payback = 10, retcost = 0.9
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
    cluster = data.frame(),
    vintage = data.frame(),
    cap2stg = 1,
    fullYear = TRUE,
    weather = data.frame(),
    optimizeRetirement = FALSE,
    misc = list(),
    ...
    ) {
  args <- list(
    desc = desc,
    commodity = commodity,
    aux = aux,
    region = region,
    cluster = cluster,
    vintage = vintage,
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
  # fold the deprecated `start`/`end`/`olife` arguments into `vintage`
  args <- .tech_lifespan_args(args)
  do.call(.data2slots, c(list("storage", name), args))
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
  # fold the deprecated `start`/`end`/`olife` arguments into `vintage` before
  # they reach `.data2slots()`, which no longer knows those slot names
  args <- .tech_lifespan_args(list(...))
  do.call(.data2slots, c(list("storage", object), args))
})
