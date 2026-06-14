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
  # storage
  mStorageFixom = list(source = "pStorageFixom", window = "mStorageSpan"),
  mStorageVarom = list(source = c("pStorageCostInp", "pStorageCostOut",
                                  "pStorageCostStore"),
                       window = "mStorageSpan"),
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
map_mStorageFixom <- function(scen, fmp) .value_std(scen, "mStorageFixom", fmp)
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
  regions <- scen@modInp@sets[["region"]]
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
  techs <- .retirement_techs(scen)
  if (length(techs) == 0) return(scen)
  .set_map(scen, "mTechRetirement",
           data.frame(tech = techs, stringsAsFactors = FALSE), fmp)
}

# mTaxCost / mSubCost: (comm, region, year) domains where a tax / subsidy applies,
# aggregated over slice from the three cost components (inp/out/bal). NA region in
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
  mStorageFixom = map_mStorageFixom,
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
