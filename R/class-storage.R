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
#' @slot inp2stg `r get_slot_doc("storage", "inp2stg")`
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
    inp2stg = "data.frame",  # charge-to-storing capacity ratio (charging C-rate)
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
      # is `inp.af.up * cap2act * cap * pTimesliceShare[s]`, so `cap` is a RATE and
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
      # is `inp.af.up * cap2act * cap * pTimesliceShare[s]`, so `cap` is a RATE and
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
      # --- capacity couplings, PER PART --------------------------------
      # Each multiplies its own part's capacity variable (cap = standing,
      # ncap = newly built). The pre-part-aware `cap2ainp`/`ncap2ainp` names
      # coupled ONLY the discharger and are refused with a rename hint --
      # they read as "the" capacity, which a three-part storage does not have.
      inp.cap2ainp = numeric(),
      inp.cap2aout = numeric(),
      inp.ncap2ainp = numeric(),
      inp.ncap2aout = numeric(),
      stg.cap2ainp = numeric(),
      stg.cap2aout = numeric(),
      stg.ncap2ainp = numeric(),
      stg.ncap2aout = numeric(),
      out.cap2ainp = numeric(),
      out.cap2aout = numeric(),
      out.ncap2ainp = numeric(),
      out.ncap2aout = numeric(),
      # --- end of life -------------------------------------------------
      # `pho2a*` fires when capacity reaches the END OF ITS LIFE, `ret2a*` when
      # it is retired EARLY. They are separate because the recovery differs:
      # a plant scrapped early is largely intact, a worn-out one is not.
      #
      # Both multiply the per-year retirement/phase-out FLOW, so the charge
      # lands ONCE -- in the milestone where the capacity disappears -- exactly
      # as the `*.ncap2a*` couplings land once at construction. Multiplying a
      # standing quantity instead would recur every year.
      #
      # TIMESLICE WARNING, inherited deliberately from `*.ncap2a*`: the term
      # carries NO `pTimesliceShare`, and `vTechAInp`/`vTechAOut` enter a
      # PER-TIMESLICE balance. A coefficient given with `timeslice = NA`
      # therefore applies in every slice and the annual total comes out
      # multiplied by the slice count -- 8760x on an hourly calendar. Give a
      # per-slice value, or name one slice.
      pho2ainp = numeric(),
      pho2aout = numeric(),
      ret2ainp = numeric(),
      ret2aout = numeric(),
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
      inp.af.up = numeric(),
      inp.af.fx = numeric(),
      inp.af.lo = numeric(),
      out.af.up = numeric(),
      out.af.fx = numeric(),
      out.af.lo = numeric(),
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
    # inp2stg = input capacity / storing capacity, 1/hours -- a charging
    # C-rate. Unlike the two ratios above there is NO binding default: the
    # constraint exists only where a bound is declared (and only for storages
    # whose charging AND storing parts both carry capacity variables), so a
    # storage saying nothing keeps its charger and reservoir untied.
    inp2stg = data.frame(
      vintage = character(),
      cluster = character(),
      region = character(),
      year = integer(),
      inp2stg = numeric(),
      inp2stg.lo = numeric(),
      inp2stg.up = numeric(),
      inp2stg.fx = numeric(),
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
      inp.waf.lo = numeric(),
      inp.waf.fx = numeric(),
      inp.waf.up = numeric(),
      out.waf.lo = numeric(),
      out.waf.fx = numeric(),
      out.waf.up = numeric(),
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

# v0.80: `cinp.*` / `cout.*` (and their weather twins) were availability factors
# wearing a flow name. The bound the model builds is
# `cinp.up * cap2act * cap * pTimesliceShare[s]` -- a per-capacity availability
# factor, which is exactly what `af` means everywhere else, and the name read as
# a flow limit in commodity units. Renamed onto the same `inp.`/`out.` prefixes
# the cost and capacity slots already carry. A hard break, so the old name is an
# error rather than a silent no-op -- but one that names its replacement.
.storage_renamed_cols <- c(
  stats::setNames(paste0("inp.af.",  c("lo", "up", "fx")),
                  paste0("cinp.",  c("lo", "up", "fx"))),
  stats::setNames(paste0("out.af.",  c("lo", "up", "fx")),
                  paste0("cout.",  c("lo", "up", "fx"))),
  stats::setNames(paste0("inp.waf.", c("lo", "up", "fx")),
                  paste0("wcinp.", c("lo", "up", "fx"))),
  stats::setNames(paste0("out.waf.", c("lo", "up", "fx")),
                  paste0("wcout.", c("lo", "up", "fx"))))

setValidity("storage", function(object) {
  bad <- character()
  for (role in c("input", "output", "storage")) {
    ok <- if (role == "storage") .storage_decl_cols[1:2] else .storage_decl_cols
    x <- setdiff(names(slot(object, role)), ok)
    if (length(x)) bad <- c(bad, sprintf(
      "@%s holds the declaration only (%s); found %s", role,
      paste(ok, collapse = ", "), paste(x, collapse = ", ")))
  }
  for (nm in c("af", "weather")) {
    hit <- intersect(names(slot(object, nm)), names(.storage_renamed_cols))
    if (length(hit)) bad <- c(bad, sprintf(
      "@%s: %s renamed -- %s", nm,
      if (length(hit) > 1) "columns" else "column",
      paste(sprintf("`%s` is now `%s`", hit, .storage_renamed_cols[hit]),
            collapse = ", ")))
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
# below where it started has consumed an endowment nobody paid for. Being
# additive, the level at
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
  # Not a formal and not a slot, so it would otherwise fall through `...` into
  # `.data2slots()` and be dropped silently. `cap2act` is the declaration slots'
  # own content, so the shorthand fills whichever power side did not name one.
  # `@storage` never gets it -- energy is energy at any resolution.
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
  if (.given(args$inp2stg))  args$inp2stg  <- .norm_ratio(args$inp2stg, "inp2stg")
  args$cap2stg <- NULL

  # The pre-part-aware aux capacity couplings coupled ONLY the discharger.
  # HARD BREAK (no aliases): refuse them with the rename, so a model meaning
  # "per unit of energy capacity" cannot silently charge per unit of power.
  if (.given(args$aeff) && is.data.frame(args$aeff)) {
    legacy <- intersect(names(args$aeff),
                        c("cap2ainp", "cap2aout", "ncap2ainp", "ncap2aout"))
    if (length(legacy)) {
      stop("Storage '", name, "': aeff column(s) ",
           paste0("`", legacy, "`", collapse = ", "),
           " are renamed -- they coupled only the DISCHARGER capacity. Use ",
           paste0("`out.", legacy, "`", collapse = ", "),
           " for that meaning, or `inp.`/`stg.` prefixes to couple the ",
           "charger / reservoir capacity instead.", call. = FALSE)
    }
  }

  .check_storage_ratios(args, name)
}

# Bring a `storage` serialized before the `inp2stg` slot existed up to the
# current class definition. Old objects lack the slot, so accessing `@inp2stg`
# errors ("no slot of name ..."); add it (S4 slots are attributes) with the
# prototype empty frame. Same device as `.upgrade_summand()`.
.upgrade_storage_inp2stg <- function(s) {
  if (!methods::is(s, "storage")) return(s)
  if (!("inp2stg" %in% names(attributes(s)))) {
    attr(s, "inp2stg") <- methods::new("storage")@inp2stg
  }
  s
}

# Upgrade every storage in a model (in each repository) so legacy models
# serialized before the slot interpolate cleanly. Called once at the top of
# interp_mod(), beside .upgrade_model_summands().
.upgrade_model_storages <- function(mod) {
  for (i in seq_along(mod@data)) {
    rp <- mod@data[[i]]
    if (!methods::is(rp, "repository")) next
    objs <- rp@data
    hit <- FALSE
    for (j in seq_along(objs)) {
      o <- objs[[j]]
      if (methods::is(o, "storage") &&
          !("inp2stg" %in% names(attributes(o)))) {
        objs[[j]] <- .upgrade_storage_inp2stg(o)
        hit <- TRUE
      }
    }
    if (hit) mod@data[[i]]@data <- objs
  }
  mod
}

# Cross-check the three capacity ratios: `inp2out` = inp/out, `inp2stg` =
# inp/stg, `duration` = stg/out. Any two determine the third
# (inp2stg == inp2out / duration), so a fixed triangle must satisfy the
# identity, and a triangle constrained on all three edges is over-determined.
# Runs on the normalised args (bounds in `.lo`/`.up`/`.fx` columns); a key
# column that is absent or NA acts as a wildcard when matching rows across
# the three slots.
.check_storage_ratios <- function(args, name) {
  keys <- c("vintage", "cluster", "region", "year")
  rows <- function(x) if (is.data.frame(x) && nrow(x) > 0) x else NULL
  dur <- rows(args$duration)
  i2o <- rows(args$inp2out)
  i2s <- rows(args$inp2stg)
  if (is.null(dur) || is.null(i2o) || is.null(i2s)) return(args)

  kv <- function(d, i, k) if (k %in% names(d)) d[[k]][i] else NA
  compat <- function(a, ia, b, ib) {
    for (k in keys) {
      va <- kv(a, ia, k); vb <- kv(b, ib, k)
      if (!is.na(va) && !is.na(vb) &&
          as.character(va) != as.character(vb)) return(FALSE)
    }
    TRUE
  }
  col <- function(d, nm) if (nm %in% names(d)) as.numeric(d[[nm]]) else
    rep(NA_real_, nrow(d))
  # A ratio side counts as CONSTRAINING only when it can bind: an opened side
  # (lo <= 0, or up >= 1e6 -- the "switched off" convention) is not a third
  # edge of the triangle, or every storage opening duration + inp2out around
  # a C-rate would warn.
  bounded <- function(d, nm) {
    lo <- col(d, paste0(nm, ".lo"))
    up <- col(d, paste0(nm, ".up"))
    fx <- col(d, paste0(nm, ".fx"))
    (!is.na(fx)) | (!is.na(lo) & lo > 0) | (!is.na(up) & up < 1e6)
  }

  over <- FALSE
  for (id in seq_len(nrow(dur))) {
    for (io in seq_len(nrow(i2o))) {
      if (!compat(dur, id, i2o, io)) next
      for (is_ in seq_len(nrow(i2s))) {
        if (!compat(dur, id, i2s, is_) || !compat(i2o, io, i2s, is_)) next
        f_d <- col(dur, "duration.fx")[id]
        f_o <- col(i2o, "inp2out.fx")[io]
        f_s <- col(i2s, "inp2stg.fx")[is_]
        if (!is.na(f_d) && !is.na(f_o) && !is.na(f_s)) {
          if (f_d > 0 &&
              abs(f_s - f_o / f_d) >
                1e-6 * max(abs(f_s), abs(f_o / f_d), 1e-12)) {
            stop("Storage '", name, "': fixed capacity ratios are ",
                 "inconsistent -- inp2stg.fx (", f_s, ") != inp2out.fx / ",
                 "duration.fx (", f_o, " / ", f_d, " = ",
                 signif(f_o / f_d, 8), "). Any two of the three ratios ",
                 "determine the third.", call. = FALSE)
          }
          next  # a consistent fixed triangle is exact, not over-determined
        }
        if (bounded(dur, "duration")[id] && bounded(i2o, "inp2out")[io] &&
            bounded(i2s, "inp2stg")[is_]) over <- TRUE
      }
    }
  }
  if (over) {
    warning("Storage '", name, "': all three capacity ratios (duration, ",
            "inp2out, inp2stg) are constrained on overlapping keys. Any two ",
            "determine the third; inconsistent ranges will surface as an ",
            "infeasible model.", call. = FALSE)
  }
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
#' @param inp2stg `r get_slot_doc("storage", "inp2stg")`
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
#'     out.cap2ainp = 0.9,
#'     out.cap2aout = 0.9,
#'     stg.ncap2ainp = 0.9,
#'     out.ncap2aout = 0.9,
#'     ncap2stg = 0.9
#'   ),
#'   af = data.frame(
#'     region = "R1", year = 2020, timeslice = "HOUR",
#'     af.lo = 0.9, af.up = 0.9, af.fx = 0.9, inp.af.up = 0.9,
#'     inp.af.fx = 0.9, inp.af.lo = 0.9, out.af.up = 0.9,
#'     out.af.fx = 0.9, out.af.lo = 0.9
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
#'     waf.fx = 0.9, inp.waf.lo = 0.9,
#'     inp.waf.fx = 0.9, inp.waf.up = 0.9, out.waf.lo = 0.9, out.waf.fx = 0.9,
#'     out.waf.up = 0.9
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
    inp2stg = NULL,
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
    inp2stg = inp2stg,
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
