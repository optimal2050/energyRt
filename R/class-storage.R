# Class storage ####
#' An S4 class to represent storage type of technological process.
#'
#' @inherit newStorage description
#' @inherit newStorage details
#'
#' @md
#' @slot name `r get_slot_doc("storage", "name")`
#' @slot desc `r get_slot_doc("storage", "desc")`
#' @slot aux `r get_slot_doc("storage", "aux")`
#' @slot region `r get_slot_doc("storage", "region")`
#' @slot cluster `r get_slot_doc("storage", "cluster")`
#' @slot vintage `r get_slot_doc("storage", "vintage")`
#' @slot capacity `r get_slot_doc("storage", "capacity")`
#' @slot input `r get_slot_doc("storage", "input")`
#' @slot output `r get_slot_doc("storage", "output")`
#' @slot storage `r get_slot_doc("storage", "storage")`
#' @slot startLevel `r get_slot_doc("storage", "startLevel")`
#' @slot seff `r get_slot_doc("storage", "seff")`
#' @slot af `r get_slot_doc("storage", "af")`
#' @slot aeff `r get_slot_doc("storage", "aeff")`
#' @slot fixom `r get_slot_doc("storage", "fixom")`
#' @slot varom `r get_slot_doc("storage", "varom")`
#' @slot invcost `r get_slot_doc("storage", "invcost")`
#' @slot fullYear `r get_slot_doc("storage", "fullYear")`
#' @slot duration `r get_slot_doc("storage", "duration")`
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
    input = "data.frame",    # what fills the store   (the "charger" side)
    output = "data.frame",   # what it releases       (the "discharger" side)
    storage = "data.frame",  # what it HOLDS -- the level's commodity
    aux = "data.frame", #
    region = "character",
    cluster = "data.frame",
    vintage = "data.frame",
    # stock = "data.frame", #
    capacity = "data.frame", #
    startLevel = "data.frame", #
    seff = "data.frame", #
    af = "data.frame", # Availability of the resource with prices
    aeff = "data.frame", #  Commodity efficiency
    fixom = "data.frame", #
    varom = "data.frame", #
    invcost = "data.frame",
    fullYear = "logical",
    duration = "data.frame", # energy-to-power ratio; varies by cluster (duration)
    weather = "data.frame", # weather condisions multiplier
    optimizeRetirement = "logical",
    misc = "list" #
  ),
  prototype(
    name = "",
    desc = "",
    # Cluster declaration -- see the `technology` class for the full rationale.
    # For storage the motivating case is site-constrained capacity: pumped-hydro
    # head classes, CAES caverns, or duration classes via per-cluster `duration`.
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
    # cluster); `start`/`end` = user-defined window (NA side = unbounded).
    vintage = data.frame(
      vintage = character(),
      region = character(),
      cluster = character(),
      start = integer(),
      end = integer(),
      olife = integer(),
      stringsAsFactors = FALSE
    ),
    # Energy added to the level ONCE PER CYCLE, at the first timeslice of the
    # cycle. There is deliberately NO `timeslice` column: the slice is derived
    # from the calendar and `@fullYear`, which is what removes the wildcard trap
    # the old per-timeslice `@charge` had. Which cycle depends on `@fullYear`:
    # once a year when TRUE, once per parent timeframe when FALSE.
    # The three commodity ROLES -- the ONLY place a storage's commodities live.
    # There is no `@commodity` slot: `newStorage(commodity = )` is still accepted
    # and fills whichever roles were not named, at construction, so the object
    # never carries two answers to "what does this consume". Same treatment
    # `@start`/`@end`/`@olife` got when they merged into `@vintage`.
    # `comm` only for now: capacity and cost columns arrive with the variables
    # that read them, so the class never advertises a column no equation
    # consumes (cf. storage retirement -- collected, mapped and declared with no
    # equation in any backend).
    input = data.frame(comm = character(), stringsAsFactors = FALSE),
    output = data.frame(comm = character(), stringsAsFactors = FALSE),
    storage = data.frame(comm = character(), stringsAsFactors = FALSE),
    startLevel = data.frame(
      vintage = character(),
      cluster = character(),
      region = character(),
      year = integer(),
      startLevel = numeric(),
      stringsAsFactors = FALSE
    ),
    seff = data.frame(
      vintage = character(),
      cluster = character(),
      region = character(),
      year = integer(),
      timeslice = character(),
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
      timeslice = character(),
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
      timeslice = character(),
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
      timeslice = character(),
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
    # shorthand `duration = 4` still works via `.data2slots()`, because the slot
    # carries a column of its own name.
    duration = data.frame(
      vintage = character(),
      cluster = character(),
      region = character(),
      year = integer(),
      duration = numeric(),
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

# `@charge` -> `@startLevel` (v0.80) ------------------------------------------
#
# Renamed twice, and the second rename changed the semantics deliberately.
#
# `@charge` was documented as "pre-charged level at the beginning of the
# operational cycle" but was implemented as an additive term in the level
# balance at EVERY timeslice the user gave a row for. Since the balance is
# cyclic there is no "beginning", so a row with no `timeslice` was broadcast to
# every slice by the usual NA wildcard -- 8760 unpriced injections on an hourly
# calendar where one was meant. That was the form the shipped example used.
#
# `@startLevel` keeps the additive behaviour but takes the slice out of the
# user's hands: it is added ONCE PER CYCLE at the first timeslice of the cycle,
# derived from the calendar and `@fullYear`. Dropping the `timeslice` column is
# what removes the wildcard trap -- there is no longer anything to leave unset.
#
# It is free to the model, by design and unavoidably: a store that ends a cycle
# below where it started has consumed an endowment nobody paid for. PyPSA has
# the same property (`state_of_charge_initial`). Being additive, the level at
# the first slice is `startLevel + carried-over`, i.e. >= startLevel, not equal
# to it; the model may end the cycle empty and make it exact.
#
# Hydro inflow does NOT belong here -- use a `supply` (weather-driven, with
# `ava.up` so spilling is free) feeding the storage.
.storage_deprecated_args <- function(args, name = "") {
  # `newStorage()` defaults both formals to `data.frame()`, so "supplied" has to
  # mean "carries rows" -- `!is.null()` alone is TRUE for every call. Same
  # reasoning as the `nonempty` filter in `.tech_lifespan_args()`.
  .given <- function(x) {
    if (is.null(x)) return(FALSE)
    if (is.data.frame(x)) return(nrow(x) > 0L)
    if (is.list(x)) return(length(x) > 0L)
    length(x) > 0L
  }
  # `commodity` is an ARGUMENT, not a slot. Fill whichever roles were not named;
  # an explicitly named role always wins. Doing this at construction (rather
  # than leaving `@commodity` as a fallback) is what stops an object carrying
  # two answers to "what does this consume" -- and what makes `update()`
  # unambiguous.
  if (!is.null(args$commodity)) {
    cm <- as.character(args$commodity)
    cm <- cm[!is.na(cm) & nzchar(cm)]
    if (length(cm)) {
      for (role in c("input", "output", "storage")) {
        if (!.given(args[[role]]) ||
            !("comm" %in% names(args[[role]]))) {
          args[[role]] <- data.frame(comm = cm, stringsAsFactors = FALSE)
        }
      }
    }
    args$commodity <- NULL
  }

  # `charge`/`inflow` -> `startLevel` (v0.80). The value column is renamed and any
  # `timeslice` column is dropped with a warning: the slice is now derived, so a
  # user-supplied one would be silently ignored otherwise.
  for (old_nm in c("charge", "inflow")) {
    if (!.given(args[[old_nm]])) { args[[old_nm]] <- NULL; next }
    if (.given(args$startLevel)) {
      stop("Supply either `startLevel` or the deprecated `", old_nm,
           "`, not both. `", old_nm, "` was renamed to `startLevel`.")
    }
    .en_deprecate_once(
      paste0("storage@", old_nm),
      paste0("`storage@", old_nm, "` was renamed to `@startLevel` in energyRt ",
             "v0.80 and is now added ONCE PER CYCLE at the first timeslice, not ",
             "at every timeslice given. For a weather-driven inflow profile use ",
             "a `supply` feeding the storage. This warning is shown once per ",
             "session.")
    )
    x <- args[[old_nm]]
    if (is.data.frame(x) || is.list(x)) {
      if (old_nm %in% names(x)) names(x)[names(x) == old_nm] <- "startLevel"
      if ("timeslice" %in% names(x)) {
        warning("storage", if (nzchar(name)) paste0(" '", name, "'") else "",
                ": the `timeslice` column of `@", old_nm, "` is ignored -- ",
                "`@startLevel` is applied at the first timeslice of each cycle.",
                call. = FALSE)
        x[["timeslice"]] <- NULL
      }
    }
    args$startLevel <- x
    args[[old_nm]] <- NULL
  }

  # `cap2stg` -> `duration` (v0.80). Same number, clearer name: it is the ratio of
  # storing capacity to (dis)charging capacity, i.e. how long the store runs at
  # its rated output. `maps.R` had already flagged the rename ("to be renamed to
  # duration"). The column keeps the slot's own name so the `duration = 6` scalar
  # shorthand keeps working through `.data2slots()`.
  if (.given(args$cap2stg)) {
    if (.given(args$duration)) {
      stop("Supply either `duration` or the deprecated `cap2stg`, not both. ",
           "`cap2stg` was renamed to `duration` in energyRt v0.80.")
    }
    .en_deprecate_once(
      "storage@cap2stg",
      paste0("`storage@cap2stg` was renamed to `@duration` in energyRt v0.80; the ",
             "supplied `cap2stg` is accepted and renamed. Please update your data. ",
             "This warning is shown once per session.")
    )
    x <- args$cap2stg
    if ((is.data.frame(x) || is.list(x)) && "cap2stg" %in% names(x)) {
      names(x)[names(x) == "cap2stg"] <- "duration"
    }
    args$duration <- x
  }
  args$cap2stg <- NULL

  args
}

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
#' time-timeslices of the commodity timeframe. The cycle is looped either
#' on an annual basis (last time-timeslice of a year follows the first time
#' timeslice of the same year) or within the parent time-frame (for example,
#' when commodity time-frame is "HOUR" and the parent time-frame is "DAY" then
#' the storage cycle will be a calendar day).
#'
#' @param name `r get_slot_doc("storage", "name")`
#' @param desc `r get_slot_doc("storage", "desc")`
#' @param commodity convenience shorthand: the commodity used for whichever of
#'   `input`, `output` and `storage` do not name their own. A battery is
#'   `commodity = "ELC"` and nothing else; a hydrogen store is
#'   `commodity = "H2"` with `input`/`output` naming `"ELC"`. There is no
#'   `@commodity` slot -- this is folded into the three roles at construction,
#'   so the object never carries two answers.
#' @param aux `r get_slot_doc("storage", "aux")`
#' @param region `r get_slot_doc("storage", "region")`
#' @param cluster `r get_slot_doc("storage", "cluster")`
#' @param vintage `r get_slot_doc("storage", "vintage")`
#' @param start deprecated, use the `start` column of `vintage`.
#' @param end deprecated, use the `end` column of `vintage`.
#' @param olife deprecated, use the `olife` column of `vintage`.
#' @param input `r get_slot_doc("storage", "input")`
#' @param output `r get_slot_doc("storage", "output")`
#' @param storage `r get_slot_doc("storage", "storage")`
#' @param startLevel `r get_slot_doc("storage", "startLevel")`
#' @param seff `r get_slot_doc("storage", "seff")`
#' @param aeff `r get_slot_doc("storage", "aeff")`
#' @param af `r get_slot_doc("storage", "af")`
#' @param fixom `r get_slot_doc("storage", "fixom")`
#' @param varom `r get_slot_doc("storage", "varom")`
#' @param invcost `r get_slot_doc("storage", "invcost")`
#' @param capacity `r get_slot_doc("storage", "capacity")`
#' @param duration `r get_slot_doc("storage", "duration")`
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
#'   startLevel = data.frame(
#'     # region = "R1",
#'     year = 2020,
#'     startLevel = 0.1
#'   ),
#'   seff = data.frame(
#'     # region = "R1",
#'     # year = 2020,
#'     # timeslice = "HOUR",
#'     stgeff = 0.999,
#'     inpeff = 0.9,
#'     outeff = 0.9
#'   ),
#'   aeff = data.frame(
#'     acomm = "electricity",
#'     region = "R1",
#'     year = 2020,
#'     # timeslice = "HOUR",
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
#'     region = "R1", year = 2020, timeslice = "HOUR",
#'     af.lo = 0.9, af.up = 0.9, af.fx = 0.9, cinp.up = 0.9,
#'     cinp.fx = 0.9, cinp.lo = 0.9, cout.up = 0.9,
#'     cout.fx = 0.9, cout.lo = 0.9
#'   ),
#'   fixom = data.frame(region = "R1", year = 2020, fixom = 0.9),
#'   varom = data.frame(
#'     region = "R1", year = 2020, timeslice = "HOUR",
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
#'   duration = 1,
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
    input = data.frame(),
    output = data.frame(),
    storage = data.frame(),
    startLevel = data.frame(),
    seff = data.frame(),
    aeff = data.frame(),
    af = data.frame(),
    fixom = data.frame(),
    varom = data.frame(),
    invcost = data.frame(),
    capacity = data.frame(),
    cluster = data.frame(),
    vintage = data.frame(),
    duration = 1,
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
    input = input,
    output = output,
    storage = storage,
    startLevel = startLevel,
    seff = seff,
    aeff = aeff,
    af = af,
    fixom = fixom,
    varom = varom,
    invcost = invcost,
    capacity = capacity,
    duration = duration,
    fullYear = fullYear,
    weather = weather,
    optimizeRetirement = optimizeRetirement,
    misc = misc,
    ...)
  # fold the deprecated `start`/`end`/`olife` arguments into `vintage`
  args <- .tech_lifespan_args(args)
  # fold the deprecated `charge` into `inflow`, and warn on a wildcard injection
  args <- .storage_deprecated_args(args, name)
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
  args <- .storage_deprecated_args(args, tryCatch(object@name, error = function(e) ""))
  do.call(.data2slots, c(list("storage", object), args))
})
