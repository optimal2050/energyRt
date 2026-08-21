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
  mStorageFixom = list(source = "pStorageOutFixom", window = "mStorageSpan"),
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
  # capacity variable and `cinp.up`/`cout.up` defaulting to .inf would let the
  # LP drive it to zero -- a priced-but-unbounded capacity vanishing silently.
  mStorageStgCap = list(source = c("pStorageStgStock", "pStorageStgCap",
                                   "pStorageStgNewCap", "pStorageStgInvcost",
                                   "pStorageStgFixom"),
                        window = "mStorageSpan"),
  mStorageStgNew = list(source = c("pStorageStgStock", "pStorageStgCap",
                                   "pStorageStgNewCap", "pStorageStgInvcost",
                                   "pStorageStgFixom"),
                        window = "mStorageNew"),
  # The charging part, same structure-follows-data gate as the storing part.
  mStorageInpCap = list(source = c("pStorageInpStock", "pStorageInpCap",
                                   "pStorageInpNewCap", "pStorageInpInvcost",
                                   "pStorageInpFixom"),
                        window = "mStorageSpan"),
  mStorageInpNew = list(source = c("pStorageInpStock", "pStorageInpCap",
                                   "pStorageInpNewCap", "pStorageInpInvcost",
                                   "pStorageInpFixom"),
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
  mStorageWeatherCinpUp = list(source = "pStorageWeatherCinp", types = c("up", "fx")),
  mStorageWeatherCinpLo = list(source = "pStorageWeatherCinp", types = c("lo", "fx")),
  mStorageWeatherCoutUp = list(source = "pStorageWeatherCout", types = c("up", "fx")),
  mStorageWeatherCoutLo = list(source = "pStorageWeatherCout", types = c("lo", "fx")),
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
map_mStorageWeatherCinpUp <- function(scen, fmp) .value_weather(scen, "mStorageWeatherCinpUp", fmp)
map_mStorageWeatherCinpLo <- function(scen, fmp) .value_weather(scen, "mStorageWeatherCinpLo", fmp)
map_mStorageWeatherCoutUp <- function(scen, fmp) .value_weather(scen, "mStorageWeatherCoutUp", fmp)
map_mStorageWeatherCoutLo <- function(scen, fmp) .value_weather(scen, "mStorageWeatherCoutLo", fmp)
map_mSupWeatherUp         <- function(scen, fmp) .value_weather(scen, "mSupWeatherUp", fmp)
map_mSupWeatherLo         <- function(scen, fmp) .value_weather(scen, "mSupWeatherLo", fmp)

# -- bespoke value maps ----------------------------------------------------- #
# mSupSpan: (sup, region) operational span of each supply object (its own regions,
# defaulting to all model regions when unspecified).
map_mSupSpan <- function(scen, fmp) {
  # Declared regions only: a supply sits at the finest level, never at a
  # geoscale nation or zone.
  regions <- .model_regions(scen)
  res <- apply_to_scenario_data(
    scen = scen, classes = "supply", as_list = TRUE,
    func = function(obj) {
      regs <- obj@region
      if (length(regs) == 0 || all(is.na(regs))) regs <- regions
      regs <- regs[regs %in% regions]
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
  mStorageWeatherCinpUp = map_mStorageWeatherCinpUp,
  mStorageWeatherCinpLo = map_mStorageWeatherCinpLo,
  mStorageWeatherCoutUp = map_mStorageWeatherCoutUp,
  mStorageWeatherCoutLo = map_mStorageWeatherCoutLo,
  mSupWeatherUp         = map_mSupWeatherUp,
  mSupWeatherLo         = map_mSupWeatherLo,
  mSupSpan        = map_mSupSpan,
  mTechRetirement = map_mTechRetirement,
  mTaxCost        = map_mTaxCost,
  mSubCost        = map_mSubCost
)
