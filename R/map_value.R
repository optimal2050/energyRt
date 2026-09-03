# =========================================================================== #
# map_value.R  —  value-derived mapping builders (family "value")
#
# One `map_<Name>(scen, fmp) -> scen` per value mapping. A value map's domain is
# the set of points where its source p* parameter(s) carry a defined value (see
# value_on_window / weather_map in mapping_helpers.R), optionally intersected with
# a lifespan window. Registered in `.value_builders` and dispatched by
# build_mappings() ahead of the legacy recipe_value().
#
# The regular maps are table-backed: `.value_map_def` / `.weather_map_def` are the
# single source of truth for each map's source parameter(s) + window / bound
# types. `.value_map_def` is ALSO consumed by interp.R `.param_value_maps()` to
# trim value parameters to the domain of the maps that index them, so it is real
# shared metadata (build + trim), kept co-located with this family.
#
# To add a value mapping: add its row to the table (or write a bespoke builder),
# a thin `map_<Name>`, and an entry to `.value_builders`; declare it in
# modInp.yml (type: map, dimSets) + maps.R.
# =========================================================================== #

# Regular value maps: source = interpolated p* parameter(s) supplying the value
# domain; window = lifespan window map to intersect (NULL = none); gate = optional
# scenario settings flag that must be TRUE.
.value_map_def <- list(
  # technology
  mTechInv      = list(source = "pTechInvcost", window = "mTechNew"),
  mTechFixom    = list(source = "pTechFixom",   window = "mTechSpan"),
  mTechVarom    = list(source = "pTechVarom",   window = "mTechSpan"),
  mTechRetCost  = list(source = "pTechRetCost", window = NULL,
                       gate = "optimizeRetirement"),
  # One retirement-cost domain per storage, spanning the three parts -- the
  # cost equation sums them into a single `vStorageRetCost`, as eqStorageEac
  # does for the annuity.
  mStorageRetCost = list(source = c("pStorageOutRetCost", "pStorageInpRetCost",
                                    "pStorageStgRetCost"),
                         window = NULL, gate = "optimizeRetirement"),
  mTradeRetCost   = list(source = "pTradeRetCost", window = NULL,
                         gate = "optimizeRetirement"),
  # storage
  # Spans the three parts, like `mStorageRetCost` above and for the same reason:
  # `eqStorageFixom` sums all three into a single `vStorageFixom`, with the
  # storing and charging terms as guarded sums INSIDE the equation. Sourced from
  # the out part alone, a storage priced only on its reservoir got no equation at
  # all and its fixed O&M silently left the objective.
  mStorageFixom = list(source = c("pStorageOutFixom", "pStorageInpFixom",
                                  "pStorageStgFixom"),
                       window = "mStorageSpan"),
  mStorageVarom = list(source = c("pStorageCostInp", "pStorageCostOut",
                                  "pStorageCostStore"),
                       window = "mStorageSpan"),
  # STRUCTURE FOLLOWS DATA. `mStorageStgCap` is the set of (stg, region, year)
  # where the storing part carries data of its own -- a stock, a bound, or a
  # price. ONLY there does `vStorageStgCap` exist; elsewhere the af equations
  # fall back to `duration * vStorageOutCap`, which is the previous model.
  #
  # `@storage$comm` is deliberately NOT a source: naming a commodity is
  # metadata, not data. Were it included, every storage would acquire an energy
  # capacity variable and `inp.af.up`/`out.af.up` defaulting to .inf would let the
  # LP drive it to zero -- a priced-but-unbounded capacity vanishing silently.
  #
  # `pStorage*Eac` IS a source, alongside its `pStorage*Invcost` sibling. Both
  # are a price on the part, so both are data by the same rule. Omitting the eac
  # produced exactly the failure the line above warns about, only from the other
  # direction: `mStorage*Eac` still built an objective term, so the LP carried a
  # capacity variable that was priced but appeared in no constraint, and drove it
  # to zero. An `eac`-financed charging or storing part was silently free.
  mStorageStgCap = list(source = c("pStorageStgStock", "pStorageStgCap",
                                   "pStorageStgNewCap", "pStorageStgInvcost",
                                   "pStorageStgEac", "pStorageStgFixom"),
                        window = "mStorageSpan"),
  mStorageStgNew = list(source = c("pStorageStgStock", "pStorageStgCap",
                                   "pStorageStgNewCap", "pStorageStgInvcost",
                                   "pStorageStgEac", "pStorageStgFixom"),
                        window = "mStorageNew"),
  # The charging part, same structure-follows-data gate as the storing part.
  mStorageInpCap = list(source = c("pStorageInpStock", "pStorageInpCap",
                                   "pStorageInpNewCap", "pStorageInpInvcost",
                                   "pStorageInpEac", "pStorageInpFixom"),
                        window = "mStorageSpan"),
  mStorageInpNew = list(source = c("pStorageInpStock", "pStorageInpCap",
                                   "pStorageInpNewCap", "pStorageInpInvcost",
                                   "pStorageInpEac", "pStorageInpFixom"),
                        window = "mStorageNew"),
  mStorageInpFixom = list(source = "pStorageInpFixom", window = "mStorageSpan"),
  mStorageInpEac   = list(source = "pStorageInpEac",   window = "mStorageNew"),
  mStorageStgFixom = list(source = "pStorageStgFixom", window = "mStorageSpan"),
  mStorageStgEac   = list(source = "pStorageStgEac",   window = "mStorageNew"),
  # trade
  mTradeInv     = list(source = "pTradeInvcost", window = "mTradeNew"),
  mTradeEac     = list(source = "pTradeEac",     window = "mTradeNew"),
  mTradeFixom   = list(source = "pTradeFixom",   window = "mTradeSpan"),
  # supply
  mvSupCost     = list(source = "pSupCost",      window = NULL),
  mvSupReserve  = list(source = "pSupReserve",   window = NULL)
)

# Weather-availability membership maps: select bound `types` from the source
# bounds parameter ("Up" -> up/fx, "Lo" -> lo/fx) and project onto the map dims.
.weather_map_def <- list(
  mTechWeatherAfUp      = list(source = "pTechWeatherAf",      types = c("up", "fx")),
  mTechWeatherAfLo      = list(source = "pTechWeatherAf",      types = c("lo", "fx")),
  mTechWeatherAfsUp     = list(source = "pTechWeatherAfs",     types = c("up", "fx")),
  mTechWeatherAfsLo     = list(source = "pTechWeatherAfs",     types = c("lo", "fx")),
  mTechWeatherAfcUp     = list(source = "pTechWeatherAfc",     types = c("up", "fx")),
  mTechWeatherAfcLo     = list(source = "pTechWeatherAfc",     types = c("lo", "fx")),
  mStorageWeatherAfUp   = list(source = "pStorageWeatherAf",   types = c("up", "fx")),
  mStorageWeatherAfLo   = list(source = "pStorageWeatherAf",   types = c("lo", "fx")),
  mStorageWeatherInpAfUp = list(source = "pStorageWeatherInpAf", types = c("up", "fx")),
  mStorageWeatherInpAfLo = list(source = "pStorageWeatherInpAf", types = c("lo", "fx")),
  mStorageWeatherOutAfUp = list(source = "pStorageWeatherOutAf", types = c("up", "fx")),
  mStorageWeatherOutAfLo = list(source = "pStorageWeatherOutAf", types = c("lo", "fx")),
  mSupWeatherUp         = list(source = "pSupWeather",         types = c("up", "fx")),
  mSupWeatherLo         = list(source = "pSupWeather",         types = c("lo", "fx"))
)

.value_std     <- function(scen, name, fmp) {
  d <- .value_map_def[[name]]
  value_on_window(scen, name, source = d$source, window = d$window,
                  gate = d$gate, fmp = fmp)
}
.value_weather <- function(scen, name, fmp) {
  d <- .weather_map_def[[name]]
  weather_map(scen, name, source = d$source, types = d$types, fmp = fmp)
}

# -- per-mapping entry points (thin, table-backed) ------------------------- #
map_mTechInv      <- function(scen, fmp) .value_std(scen, "mTechInv", fmp)
map_mTechFixom    <- function(scen, fmp) .value_std(scen, "mTechFixom", fmp)
map_mTechVarom    <- function(scen, fmp) .value_std(scen, "mTechVarom", fmp)
map_mTechRetCost  <- function(scen, fmp) .value_std(scen, "mTechRetCost", fmp)
map_mStorageRetCost <- function(scen, fmp) .value_std(scen, "mStorageRetCost", fmp)
map_mTradeRetCost   <- function(scen, fmp) .value_std(scen, "mTradeRetCost", fmp)
map_mStorageFixom <- function(scen, fmp) .value_std(scen, "mStorageFixom", fmp)
map_mStorageStgCap   <- function(scen, fmp) .value_std(scen, "mStorageStgCap", fmp)
map_mStorageInpCap   <- function(scen, fmp) .value_std(scen, "mStorageInpCap", fmp)
map_mStorageInpNew   <- function(scen, fmp) .value_std(scen, "mStorageInpNew", fmp)
map_mStorageInpFixom <- function(scen, fmp) .value_std(scen, "mStorageInpFixom", fmp)
map_mStorageInpEac   <- function(scen, fmp) .value_std(scen, "mStorageInpEac", fmp)
map_mStorageStgNew   <- function(scen, fmp) .value_std(scen, "mStorageStgNew", fmp)
map_mStorageStgFixom <- function(scen, fmp) .value_std(scen, "mStorageStgFixom", fmp)
map_mStorageStgEac   <- function(scen, fmp) .value_std(scen, "mStorageStgEac", fmp)

# The COMPLEMENT of mStorageStgCap within the storage's operating span: where the
# storing part has no data and therefore no capacity variable, so the af bounds
# use `duration * vStorageOutCap` instead. Built as a set difference rather than
# a value domain, hence the bespoke builder. Must run AFTER map_mStorageStgCap
# (see the .value_builders order below).
# `have` is the part's capacity domain; the complement within the operating span
# is where that part has no variable and the linking ratio is inlined onto
# vStorageOutCap instead. Must run AFTER the map it complements.
.map_no_part_cap <- function(scen, name, have, fmp) {
  span <- .gds(scen, "mStorageSpan")
  if (is.null(span) || !nrow(span)) return(scen)
  span <- as.data.frame(span)
  h <- .gds(scen, have)
  out <- if (is.null(h) || !nrow(h)) span else
    dplyr::anti_join(span, as.data.frame(h), by = c("stg", "region", "year"))
  .set_map(scen, name, as.data.frame(out), fmp)
}
map_mStorageNoStgCap <- function(scen, fmp)
  .map_no_part_cap(scen, "mStorageNoStgCap", "mStorageStgCap", fmp)
map_mStorageNoInpCap <- function(scen, fmp)
  .map_no_part_cap(scen, "mStorageNoInpCap", "mStorageInpCap", fmp)

# The INTERSECTION of the charging and storing capacity domains: the inp2stg
# ratio links vStorageInpCap to vStorageStgCap, so its constraint can only
# exist where BOTH variables do. There is no inlining fallback (nothing to
# inline the ratio onto), so an unpriced part simply drops the link. Must run
# AFTER both parents (see the .value_builders order below).
map_mStorageInpStgCap <- function(scen, fmp) {
  inp <- .gds(scen, "mStorageInpCap")
  stg <- .gds(scen, "mStorageStgCap")
  if (is.null(inp) || !nrow(inp) || is.null(stg) || !nrow(stg)) return(scen)
  out <- dplyr::inner_join(as.data.frame(inp), as.data.frame(stg),
                           by = c("stg", "region", "year"))
  .set_map(scen, "mStorageInpStgCap", as.data.frame(out), fmp)
}
map_mStorageVarom <- function(scen, fmp) .value_std(scen, "mStorageVarom", fmp)
map_mTradeInv     <- function(scen, fmp) .value_std(scen, "mTradeInv", fmp)
map_mTradeEac     <- function(scen, fmp) .value_std(scen, "mTradeEac", fmp)
map_mTradeFixom   <- function(scen, fmp) .value_std(scen, "mTradeFixom", fmp)
map_mvSupCost     <- function(scen, fmp) .value_std(scen, "mvSupCost", fmp)
map_mvSupReserve  <- function(scen, fmp) .value_std(scen, "mvSupReserve", fmp)

map_mTechWeatherAfUp      <- function(scen, fmp) .value_weather(scen, "mTechWeatherAfUp", fmp)
map_mTechWeatherAfLo      <- function(scen, fmp) .value_weather(scen, "mTechWeatherAfLo", fmp)
map_mTechWeatherAfsUp     <- function(scen, fmp) .value_weather(scen, "mTechWeatherAfsUp", fmp)
map_mTechWeatherAfsLo     <- function(scen, fmp) .value_weather(scen, "mTechWeatherAfsLo", fmp)
map_mTechWeatherAfcUp     <- function(scen, fmp) .value_weather(scen, "mTechWeatherAfcUp", fmp)
map_mTechWeatherAfcLo     <- function(scen, fmp) .value_weather(scen, "mTechWeatherAfcLo", fmp)
map_mStorageWeatherAfUp   <- function(scen, fmp) .value_weather(scen, "mStorageWeatherAfUp", fmp)
map_mStorageWeatherAfLo   <- function(scen, fmp) .value_weather(scen, "mStorageWeatherAfLo", fmp)
map_mStorageWeatherInpAfUp <- function(scen, fmp) .value_weather(scen, "mStorageWeatherInpAfUp", fmp)
map_mStorageWeatherInpAfLo <- function(scen, fmp) .value_weather(scen, "mStorageWeatherInpAfLo", fmp)
map_mStorageWeatherOutAfUp <- function(scen, fmp) .value_weather(scen, "mStorageWeatherOutAfUp", fmp)
map_mStorageWeatherOutAfLo <- function(scen, fmp) .value_weather(scen, "mStorageWeatherOutAfLo", fmp)
map_mSupWeatherUp         <- function(scen, fmp) .value_weather(scen, "mSupWeatherUp", fmp)
map_mSupWeatherLo         <- function(scen, fmp) .value_weather(scen, "mSupWeatherLo", fmp)

# Regions named in an object's own data slots. Empty when the object names
# none, and empty when any row leaves `region` NA: an NA row is a wildcard
# meaning every region, so the caller falls back to the full span.
.obj_data_regions <- function(obj, slots) {
  out <- character(0)
  for (sl in slots) {
    if (!.hasSlot(obj, sl)) next
    d <- methods::slot(obj, sl)
    if (!is.data.frame(d) || nrow(d) == 0 || !"region" %in% names(d)) next
    r <- as.character(d$region)
    if (any(is.na(r))) return(character(0))
    out <- c(out, r)
  }
  unique(out[nzchar(out)])
}

# -- bespoke value maps ----------------------------------------------------- #
# mSupSpan: (sup, region) operational span of each supply object (its own regions,
# defaulting to all model regions when unspecified).
map_mSupSpan <- function(scen, fmp) {
  # A supply spans the regions its COMMODITY is balanced at: the atoms for a
  # finest-level commodity (the flat case, unchanged), the level's members
  # for a commodity with a coarse `@geoframe`. Supply is a STRICT-level class
  # (check_levels.R): a nation-balanced commodity MUST be supplied at the
  # nation -- and the old blanket atom intersection silently DELETED exactly
  # that shape, leaving the balance with a free costless vOutTot cell
  # (energy from nowhere, objective 0, model feasible).
  atoms <- .model_regions(scen)
  comm_reg <- .comm_region_df(scen)   # NULL when no geoscale is attached
  res <- apply_to_scenario_data(
    scen = scen, classes = "supply", as_list = TRUE,
    func = function(obj) {
      allowed <- atoms
      if (!is.null(comm_reg)) {
        m <- comm_reg$region[comm_reg$comm == obj@commodity]
        if (length(m) > 0) allowed <- m
      }
      # The span must follow the DECLARATION, and a supply may be scoped
      # either by the `@region` slot or by the `region` column of its own
      # data. Reading the slot alone spanned a data-scoped supply across
      # every region, and those cells carry no `pSupCost` / `pSupAva` row --
      # so they were free and unbounded, and the solver drew from them in
      # preference to every priced cell (objective 0, model feasible).
      regs <- as.character(obj@region)
      regs <- regs[!is.na(regs)]
      if (length(regs) == 0) regs <- .obj_data_regions(obj, c("supply",
                                                              "reserve"))
      if (length(regs) == 0) regs <- allowed
      regs <- regs[regs %in% allowed]
      if (length(regs) == 0) return(NULL)
      out <- list()
      out[[obj@name]] <- data.frame(sup = obj@name, region = regs,
                                    stringsAsFactors = FALSE)
      out
    }
  )
  if (length(res) == 0) return(scen)
  .set_map(scen, "mSupSpan", dplyr::bind_rows(res), fmp)
}

# mTechRetirement: technologies with retirement optimisation enabled.
map_mTechRetirement <- function(scen, fmp) {
  if (!isTRUE(scen@settings@optimizeRetirement)) return(scen)
  techs <- .retirement_objects(scen, "technology")
  if (length(techs) == 0) return(scen)
  .set_map(scen, "mTechRetirement",
           data.frame(tech = techs, stringsAsFactors = FALSE), fmp)
}

# mTaxCost / mSubCost: (comm, region, year) domains where a tax / subsidy applies,
# aggregated over timeslice from the three cost components (inp/out/bal). NA region in
# the source means "all regions" and is expanded to every model region.
.policy_cost_map <- function(scen, name, sources, fmp) {
  p <- scen@modInp@parameters[[name]]
  if (is.null(p)) return(scen)
  set <- p@dimSets
  tx <- lapply(sources, function(sp) {
    sp_par <- scen@modInp@parameters[[sp]]
    if (is.null(sp_par)) return(NULL)
    sd <- get_data_slot(sp_par)
    if (is.null(sd) || nrow(sd) == 0) return(NULL)
    as.data.frame(sd)
  })
  df <- .reduce_sect_merge_unique(tx, set)
  if (is.null(df) || nrow(df) == 0) return(scen)
  regions <- scen@modInp@sets[["region"]]
  if ("region" %in% set && length(regions) > 0 && anyNA(df$region)) {
    na_rows <- df[is.na(df$region), setdiff(colnames(df), "region"), drop = FALSE]
    expanded <- merge0(na_rows,
                       data.frame(region = regions, stringsAsFactors = FALSE))
    df <- dplyr::distinct(dplyr::bind_rows(df[!is.na(df$region), , drop = FALSE],
                                           expanded))
  }
  .set_map(scen, name, df, fmp)
}
map_mTaxCost <- function(scen, fmp)
  .policy_cost_map(scen, "mTaxCost", c("pTaxCostInp", "pTaxCostOut", "pTaxCostBal"), fmp)
map_mSubCost <- function(scen, fmp)
  .policy_cost_map(scen, "mSubCost", c("pSubCostInp", "pSubCostOut", "pSubCostBal"), fmp)

# -- registry for the value family ----------------------------------------- #
.value_builders <- list(
  mTechInv      = map_mTechInv,
  mTechFixom    = map_mTechFixom,
  mTechVarom    = map_mTechVarom,
  mTechRetCost  = map_mTechRetCost,
  mStorageRetCost = map_mStorageRetCost,
  mTradeRetCost = map_mTradeRetCost,
  mStorageFixom = map_mStorageFixom,
  mStorageStgCap = map_mStorageStgCap,
  mStorageInpCap = map_mStorageInpCap,
  mStorageNoInpCap = map_mStorageNoInpCap,   # AFTER mStorageInpCap
  mStorageInpNew = map_mStorageInpNew,
  mStorageInpFixom = map_mStorageInpFixom,
  mStorageInpEac = map_mStorageInpEac,
  mStorageNoStgCap = map_mStorageNoStgCap,   # AFTER mStorageStgCap: complement
  mStorageInpStgCap = map_mStorageInpStgCap, # AFTER both parents: intersection
  mStorageStgNew = map_mStorageStgNew,
  mStorageStgFixom = map_mStorageStgFixom,
  mStorageStgEac = map_mStorageStgEac,
  mStorageVarom = map_mStorageVarom,
  mTradeInv     = map_mTradeInv,
  mTradeEac     = map_mTradeEac,
  mTradeFixom   = map_mTradeFixom,
  mvSupCost     = map_mvSupCost,
  mvSupReserve  = map_mvSupReserve,
  mTechWeatherAfUp      = map_mTechWeatherAfUp,
  mTechWeatherAfLo      = map_mTechWeatherAfLo,
  mTechWeatherAfsUp     = map_mTechWeatherAfsUp,
  mTechWeatherAfsLo     = map_mTechWeatherAfsLo,
  mTechWeatherAfcUp     = map_mTechWeatherAfcUp,
  mTechWeatherAfcLo     = map_mTechWeatherAfcLo,
  mStorageWeatherAfUp   = map_mStorageWeatherAfUp,
  mStorageWeatherAfLo   = map_mStorageWeatherAfLo,
  mStorageWeatherInpAfUp = map_mStorageWeatherInpAfUp,
  mStorageWeatherInpAfLo = map_mStorageWeatherInpAfLo,
  mStorageWeatherOutAfUp = map_mStorageWeatherOutAfUp,
  mStorageWeatherOutAfLo = map_mStorageWeatherOutAfLo,
  mSupWeatherUp         = map_mSupWeatherUp,
  mSupWeatherLo         = map_mSupWeatherLo,
  mSupSpan        = map_mSupSpan,
  mTechRetirement = map_mTechRetirement,
  mTaxCost        = map_mTaxCost,
  mSubCost        = map_mSubCost
)
