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
#' @slot inp2out `r get_slot_doc("storage", "inp2out")`
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
    inp2out = "data.frame",  # charge-to-discharge power ratio
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
    #
    # A role slot DECLARES; it does not parameterise. `comm`, `unit` and
    # `cap2act` are invariant -- a charger consumes ELC in MW whatever the
    # vintage, region or year -- so the slot carries no key columns at all.
    # Everything that varies lives in `@capacity`, `@invcost` and `@fixom`,
    # under a `inp.`/`out.`/`stg.` prefix naming the part it belongs to:
    # `inp.cap.up`, `stg.invcost`, `out.fixom`. That is one home per quantity,
    # which is what stops the same number being reachable two ways.
    #
    # The charging side. A dedicated charger rated differently from the
    # discharger -- an EV drawing 7 kW but delivering 100 kW to the motor -- is
    # `inp.cap.fx = 7` next to `out.cap.fx = 100`.
    input = data.frame(
      comm = character(),
      # Unit of `comm` on this side, as on `technology@input`/`@output`. Purely
      # descriptive -- carried for reporting and `convert()`, never interpolated.
      # The COMMODITY's unit, not the capacity's: capacity follows `cap2act`.
      unit = character(),
      # Capacity -> ANNUAL flow, exactly as `technology@cap2act`. The flow bound
      # is `cinp.up * cap2act * cap * pTimesliceShare[s]`, so `cap` is a RATE and
      # means the same physical thing on any calendar. Defaults to 8760 (hours in
      # a year), which makes `cap` "commodity per hour" -- and leaves an hourly
      # full-year model numerically unchanged, because 8760 * (1/8760) = 1.
      #
      # Invariant, which is why it belongs here: a conversion factor between two
      # units does not vary by vintage, region or year.
      cap2act = numeric(),
      stringsAsFactors = FALSE
    ),
    # The discharging side. `comm` is what the store releases; the capacity and
    # cost columns are accepted here for symmetry with `@input`/`@storage` and are
    # FOLDED at construction into `@capacity`/`@invcost`/`@fixom`, which is where
    # the output side has always lived. Writing `output = list(invcost = 12144)`
    # and `invcost = 12144` are therefore the same statement -- supplying both is
    # an error rather than a silent winner.
    output = data.frame(
      comm = character(),
      # Unit of `comm` on this side, as on `technology@input`/`@output`. Purely
      # descriptive -- carried for reporting and `convert()`, never interpolated.
      # The COMMODITY's unit, not the capacity's: capacity follows `cap2act`.
      unit = character(),
      # Capacity -> ANNUAL flow, exactly as `technology@cap2act`. The flow bound
      # is `cinp.up * cap2act * cap * pTimesliceShare[s]`, so `cap` is a RATE and
      # means the same physical thing on any calendar. Defaults to 8760 (hours in
      # a year), which makes `cap` "commodity per hour" -- and leaves an hourly
      # full-year model numerically unchanged, because 8760 * (1/8760) = 1.
      #
      # Invariant, which is why it belongs here: a conversion factor between two
      # units does not vary by vintage, region or year.
      cap2act = numeric(),
      stringsAsFactors = FALSE
    ),
    # The storing side. `comm` is what the store HOLDS; every other column is
    # that part's OWN capacity and economics, measured in ENERGY (e.g. MWh) --
    # NOT power. This is what `@duration` used to stand in for: a battery's
    # energy cost had to be hand-multiplied into its per-MW number because there
    # was nowhere else to put it.
    #
    # A part carrying nothing but `comm` gets NO capacity variable: naming a
    # commodity is metadata, not data. That rule is what keeps a storage written
    # the old way byte-identical -- see `.storage_part_has_data()`.
    storage = data.frame(
      comm = character(),
      # Unit of `comm` on this side, as on `technology@input`/`@output`. Purely
      # descriptive -- carried for reporting and `convert()`, never interpolated.
      # The COMMODITY's unit, not the capacity's: capacity follows `cap2act`.
      unit = character(),
      stringsAsFactors = FALSE
    ),
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
      out.fixom = numeric(),
      inp.fixom = numeric(),
      stg.fixom = numeric(),
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
      out.invcost = numeric(),
      out.wacc = numeric(),
      out.payback = numeric(),
      out.eac = numeric(),
      out.retcost = numeric(),
      inp.invcost = numeric(),
      inp.wacc = numeric(),
      inp.payback = numeric(),
      inp.eac = numeric(),
      inp.retcost = numeric(),
      stg.invcost = numeric(),
      stg.wacc = numeric(),
      stg.payback = numeric(),
      stg.eac = numeric(),
      stg.retcost = numeric(),
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
      # discharging side
      out.stock = numeric(),
      out.cap.lo = numeric(),
      out.cap.up = numeric(),
      out.cap.fx = numeric(),
      out.ncap.lo = numeric(),
      out.ncap.up = numeric(),
      out.ncap.fx = numeric(),
      out.ret.lo = numeric(),
      out.ret.up = numeric(),
      out.ret.fx = numeric(),
      # charging side
      inp.stock = numeric(),
      inp.cap.lo = numeric(),
      inp.cap.up = numeric(),
      inp.cap.fx = numeric(),
      inp.ncap.lo = numeric(),
      inp.ncap.up = numeric(),
      inp.ncap.fx = numeric(),
      inp.ret.lo = numeric(),
      inp.ret.up = numeric(),
      inp.ret.fx = numeric(),
      # stored energy side
      stg.stock = numeric(),
      stg.cap.lo = numeric(),
      stg.cap.up = numeric(),
      stg.cap.fx = numeric(),
      stg.ncap.lo = numeric(),
      stg.ncap.up = numeric(),
      stg.ncap.fx = numeric(),
      stg.ret.lo = numeric(),
      stg.ret.up = numeric(),
      stg.ret.fx = numeric(),
      stringsAsFactors = FALSE
    ),
    # Energy-to-power ratio. A data.frame (not a scalar) so it can differ by
    # cluster: 1h / 4h / 8h duration classes of the same battery. The scalar
    # shorthand `duration = 4` still works via `.data2slots()`, because the slot
    # carries a column of its own name.
    # `duration` = storing capacity / output capacity, in hours. Now a BOUND:
    # `.fx` ties the two (an N-hour battery), `.lo`/`.up` let the model choose
    # the ratio. The bare `duration` column is kept as the scalar shorthand
    # (`duration = 6`) and is normalised to `duration.fx` at construction --
    # `.pack_bounds_long()` reads only the `.lo`/`.up`/`.fx` suffixes, so a bare
    # column alone would be silently ignored.
    duration = data.frame(
      vintage = character(),
      cluster = character(),
      region = character(),
      year = integer(),
      duration = numeric(),
      duration.lo = numeric(),
      duration.up = numeric(),
      duration.fx = numeric(),
      stringsAsFactors = FALSE
    ),
    # inp2out = input capacity / output capacity, dimensionless. The charger
    # rating relative to the discharger. Same bound semantics as `duration`:
    # `.fx` ties them (one bidirectional inverter), `.lo`/`.up` let the model
    # size them apart. Default [1, 1] -- symmetric, which is what a storage
    # without a separate charger has always been.
    inp2out = data.frame(
      vintage = character(),
      cluster = character(),
      region = character(),
      year = integer(),
      inp2out = numeric(),
      inp2out.lo = numeric(),
      inp2out.up = numeric(),
      inp2out.fx = numeric(),
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


# Reject the pre-refactor column spellings ---------------------------------
#
# `.storage_deprecated_args()` runs inside `newStorage()`/`update()` only, so a
# DESERIALISED object bypasses it entirely. Every `.rds` written before the
# `inp.`/`out.`/`stg.` prefixes existed carries `cap.up` in `@capacity` and keys
# plus capacity in `@input` -- columns no mapping now reads. Without this check
# such a model loads clean and solves with the store simply absent: no error, no
# warning, a silently different answer. Validity is the only place that catches
# it, because it is the only thing that runs on `readRDS()`.
.storage_decl_cols <- c("comm", "unit", "cap2act")
.storage_key_cols  <- c("vintage", "cluster", "region", "year")
.storage_part_cols <- local({
  q <- c("stock", outer(c("cap", "ncap", "ret"), c("lo", "up", "fx"),
                        paste, sep = "."))
  list(capacity = as.vector(outer(c("out", "inp", "stg"), q, paste, sep = ".")),
       invcost  = as.vector(outer(c("out", "inp", "stg"),
                                  c("invcost", "wacc", "payback", "eac", "retcost"),
                                  paste, sep = ".")),
       fixom    = paste0(c("out", "inp", "stg"), ".fixom"))
})

setValidity("storage", function(object) {
  bad <- character()
  for (role in c("input", "output", "storage")) {
    ok <- if (role == "storage") .storage_decl_cols[1:2] else .storage_decl_cols
    x <- setdiff(names(slot(object, role)), ok)
    if (length(x)) bad <- c(bad, sprintf(
      "@%s holds the declaration only (%s); found %s", role,
      paste(ok, collapse = ", "), paste(x, collapse = ", ")))
  }
  for (nm in names(.storage_part_cols)) {
    ok <- c(.storage_key_cols, .storage_part_cols[[nm]])
    x <- setdiff(names(slot(object, nm)), ok)
    if (length(x)) bad <- c(bad, sprintf(
      "@%s: unknown column%s %s -- parameters are prefixed inp./out./stg. now",
      nm, if (length(x) > 1) "s" else "", paste(x, collapse = ", ")))
  }
  if (length(bad)) paste0("storage '", object@name, "': ",
                          paste(bad, collapse = "; ")) else TRUE
})

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
        if (!.given(args[[role]])) {
          # nothing supplied for this role -- it is exactly the default
          args[[role]] <- data.frame(comm = cm, stringsAsFactors = FALSE)
        } else if (!("comm" %in% names(args[[role]]))) {
          # Data supplied WITHOUT a commodity, e.g. `storage = list(invcost =)`
          # next to `commodity = "ELC"`. Replacing the frame here (as this did)
          # silently threw that data away -- and it is precisely the shape the
          # documented examples use, so a battery priced per MWh lost its energy
          # cost without a word. Add the commodity to the supplied rows instead.
          x <- as.data.frame(args[[role]])
          args[[role]] <- if (length(cm) == 1L) {
            cbind(comm = cm, x, stringsAsFactors = FALSE)
          } else {
            # several default commodities: every supplied row applies to each
            merge(data.frame(comm = cm, stringsAsFactors = FALSE), x)
          }
        }
      }
    }
    args$commodity <- NULL
  }

  # `cap2act` at the top level applies to BOTH power sides.
  #
  # It was never a formal and never a slot, so it fell through `...` into
  # `.data2slots()` and was dropped without a word -- the PyPSA converter passes
  # `cap2act = 8760` believing it does something, and was saved only by 8760
  # already being the default. Now that `cap2act` is the declaration slots' own
  # content, give the shorthand a meaning: fill whichever power side did not
  # name one. `@storage` never gets it -- energy is energy at any resolution.
  if (!is.null(args$cap2act)) {
    c2a <- args$cap2act
    for (role in c("input", "output")) {
      x <- args[[role]]
      if (is.null(x) || !.given(x)) {
        args[[role]] <- data.frame(cap2act = c2a, stringsAsFactors = FALSE)
      } else {
        x <- as.data.frame(x)
        if (!("cap2act" %in% names(x))) x$cap2act <- c2a
        args[[role]] <- x
      }
    }
    args$cap2act <- NULL
  }

  # A role slot DECLARES: `comm`, `unit`, `cap2act` and nothing else. Every
  # parameter lives in `@capacity`/`@invcost`/`@fixom` under an `inp.`/`out.`/
  # `stg.` prefix, so there is exactly one home per quantity and no fold.
  #
  # This USED to accept `output = list(cap.up = )` and quietly move it. That
  # convenience was the second route this class was refactored to remove, so it
  # is now an error that names the column to write instead -- silence here would
  # mean a capacity that no mapping reads and a store that is simply absent.
  .DECL <- c("comm", "unit", "cap2act")
  .PREFIX <- c(input = "inp", output = "out", storage = "stg")
  .HOME <- c(stock = "capacity", cap = "capacity", ncap = "capacity",
             ret = "capacity", invcost = "invcost", wacc = "invcost",
             payback = "invcost", eac = "invcost", retcost = "invcost",
             fixom = "fixom")
  for (role in c("input", "output", "storage")) {
    if (!.given(args[[role]])) next
    x <- args[[role]]
    extra <- setdiff(names(as.data.frame(x)), .DECL)
    # `@storage` holds ENERGY on both sides, so it has no cap2act to convert.
    if (role == "storage") extra <- union(extra, intersect(names(as.data.frame(x)), "cap2act"))
    if (!length(extra)) next
    pre <- .PREFIX[[role]]
    home <- unname(.HOME[sub("[.](lo|up|fx)$", "", extra)])
    known <- !is.na(home)
    msg <- paste0("storage", if (nzchar(name)) paste0(" '", name, "'") else "",
                  ": `", role, "` holds the declaration only (",
                  paste(if (role == "storage") .DECL[1:2] else .DECL,
                        collapse = ", "), ").")
    # A misplaced parameter gets the column to write; an unrecognised name is a
    # different mistake and saying "move `frobnicate` to `frobnicate`" helps
    # nobody -- name the two cases separately.
    if (any(known)) {
      msg <- paste0(msg, "
  Move ",
                    paste0("`", extra[known], "`", collapse = ", "), " to ",
                    paste(unique(paste0("`", home[known], "$", pre, ".",
                                        extra[known], "`")), collapse = ", "),
                    ".")
    }
    if (any(!known)) {
      msg <- paste0(msg, "
  Not a storage parameter: ",
                    paste0("`", extra[!known], "`", collapse = ", "), ".")
    }
    stop(msg, call. = FALSE)
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

  # `duration` is a BOUND now, and `.pack_bounds_long()` reads only the
  # `.lo`/`.up`/`.fx` suffixes. A bare `duration` column -- which is what both
  # the `duration = 6` scalar shorthand and every pre-v0.80 `cap2stg` produce --
  # would therefore be read as nothing at all, silently untying the store's
  # energy from its power. Normalise it to `duration.fx`, which is what "an
  # N-hour battery" has always meant. An explicit bound always wins.
  # `duration` and `inp2out` are both RATIO bounds and normalise identically.
  # `.pack_bounds_long()` reads only the `.lo`/`.up`/`.fx` suffixes, so a bare
  # column -- which is what the scalar shorthand and every pre-v0.80 `cap2stg`
  # produce -- would be read as nothing at all, silently untying the two
  # capacities it is supposed to link.
  .norm_ratio <- function(x, nm) {
    if (is.null(x)) return(x)
    fx <- paste0(nm, ".fx"); lo <- paste0(nm, ".lo"); up <- paste0(nm, ".up")
    if (is.numeric(x) && is.null(dim(x)) && is.null(names(x))) {
      # an unnamed number means the ratio is FIXED
      out <- data.frame(as.numeric(x), stringsAsFactors = FALSE)
      names(out) <- fx
      return(out)
    }
    if (is.list(x) && !is.data.frame(x)) x <- as.data.frame(x, stringsAsFactors = FALSE)
    if (!is.data.frame(x)) return(x)
    # An absent column must read as a column of NAs, not NULL: `!is.na(NULL)` is
    # `logical(0)` and `TRUE & logical(0)` is `logical(0)`, which silently
    # collapses the whole vector and moves nothing.
    col <- function(d, k) if (is.null(d[[k]])) rep(NA_real_, nrow(d)) else as.numeric(d[[k]])
    if (nm %in% names(x)) {
      if (is.null(x[[fx]])) x[[fx]] <- NA_real_
      bare  <- !is.na(x[[nm]])
      given <- !is.na(col(x, fx)) | !is.na(col(x, lo)) | !is.na(col(x, up))
      move <- bare & !given
      x[[fx]][move] <- x[[nm]][move]
      x[[nm]][move] <- NA_real_
      if (all(is.na(x[[nm]]))) x[[nm]] <- NULL
    }
    # A ONE-SIDED range means the other side is open, not the default. The
    # parameter defaults to [1, 1] so a storage saying nothing keeps the legacy
    # tie; that must not leak into a deliberate `.up = 8`, which would otherwise
    # also impose `.lo = 1`.
    if (any(c(lo, up) %in% names(x))) {
      if (is.null(x[[lo]])) x[[lo]] <- NA_real_
      if (is.null(x[[up]])) x[[up]] <- NA_real_
      one_sided <- is.na(col(x, fx)) & xor(is.na(x[[lo]]), is.na(x[[up]]))
      x[[lo]][one_sided & is.na(x[[lo]])] <- 0
      x[[up]][one_sided & is.na(x[[up]])] <- Inf
    }
    x
  }
  if (.given(args$duration)) args$duration <- .norm_ratio(args$duration, "duration")
  if (.given(args$inp2out))  args$inp2out  <- .norm_ratio(args$inp2out, "inp2out")
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
#' @param inp2out `r get_slot_doc("storage", "inp2out")`
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
#'   fixom = data.frame(region = "R1", year = 2020, out.fixom = 0.9),
#'   varom = data.frame(
#'     region = "R1", year = 2020, timeslice = "HOUR",
#'     inpcost = 0.9, outcost = 0.9, stgcost = 0.9
#'   ),
#'   invcost = data.frame(
#'     region = "R1", year = 2020, out.invcost = 0.9,
#'     out.wacc = 0.09, out.payback = 10, out.retcost = 0.9
#'   ),
#'   capacity = data.frame(
#'     region = "R1", year = 2020, out.stock = 0.9,
#'     out.cap.lo = 0.9, out.cap.up = 0.9, out.cap.fx = 0.9, out.ncap.lo = 0.9,
#'     out.ncap.up = 0.9, out.ncap.fx = 0.9, out.ret.lo = 0.9, out.ret.up = 0.9,
#'     out.ret.fx = 0.9
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
    duration = NULL,
    inp2out = NULL,
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
    inp2out = inp2out,
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
