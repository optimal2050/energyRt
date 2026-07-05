# =========================================================================== #
# testing-models.R
#
# A modular catalog of small energyRt test models for exercising the
# interpolation / mapping-parameter pipeline (interp_mod + mapping_engine) and,
# eventually, the full solve path. The file is organised in three sections,
# mirroring the energyRt modelling workflow:
#
#   A. OBJECTS    - every commodity / technology / supply / ... is defined
#                   separately. Each object isolates a feature that some
#                   mapping recipe must handle (emissions, aux commodities,
#                   weather, trade, taxes, ...).
#   B. REPOSITORY - all objects are combined into `repo_testing_model`, a
#                   single catalog from which the individual test models draw.
#   C. MODELS     - `tm_*()` builder functions assemble a subset of the catalog
#                   plus configuration into a ready-to-interpolate model. The
#                   tiers are cumulative: each adds one group of features.
#
# Tiers (cumulative):
#   tm_core()    base + storage + trade            (R1/R2, ANNUAL+4 seasons)
#   tm_flows()   tm_core + aux + emissions + aggregate
#   tm_policy()  tm_flows + tax + subsidy + import + export
#   tm_weather() tm_policy + weather-dependent availability
#
# The builders return an UNSOLVED, UN-INTERPOLATED model object.
#
# Usage:
#   devtools::load_all(".")
#   source("data-raw/testing-models.R")
#   mod <- tm_core()
#
# NOTE: For now the catalog lives as editable code here. Once the S4 classes
# and modInp structure stabilise, `repo_testing_model` can be promoted to a
# package data object (data/repo_testing_model.rda) via usethis::use_data() in
# data-raw/DATASET.R, and the builders can pull from the lazy-loaded repo.
# =========================================================================== #

# --------------------------------------------------------------------------- #
# Shared calendar: ANNUAL split into 4 seasonal slices.
# --------------------------------------------------------------------------- #
.tm_calendar <- function() {
  tt <- make_timetable(
    struct = list(
      ANNUAL = "ANNUAL",
      SEASON = c("WIN", "SPR", "SUM", "AUT")
    )
  )
  newCalendar(timetable = tt, name = "cal_test")
}

.tm_regions  <- c("R1", "R2")
.tm_horizon  <- function() newHorizon(2020:2050, intervals = 10)

# =========================================================================== #
# A. OBJECTS
# =========================================================================== #

# --- Commodities ----------------------------------------------------------- #

# Coal: primary fuel, annual resolution. Plain variant (no emissions), used by
# tm_core.
tm_COA <- newCommodity(
  name = "COA",
  desc = "Coal (primary fuel)",
  timeframe = "ANNUAL"
)

# Coal variant carrying a CO2 emission factor so that burning it generates
# emissions (feature: emissions / mEmsFuelTot). Used from tm_flows onward,
# where the CO2 commodity is present. Replaces tm_COA.
tm_COA_emis <- newCommodity(
  name = "COA",
  desc = "Coal (primary fuel, emits CO2)",
  timeframe = "ANNUAL",
  emis = data.frame(comm = "CO2", unit = "kt/PJ", emis = 0.09)
)

# Electricity: the main carrier, seasonal resolution.
tm_ELC <- newCommodity("ELC", desc = "Electricity", timeframe = "SEASON")

# Water: auxiliary input commodity for cooling (feature: aux-conversion).
tm_WAT <- newCommodity("WAT", desc = "Cooling water (auxiliary)",
                       timeframe = "SEASON")

# Waste heat: auxiliary OUTPUT commodity, used to exercise the trade aux-output
# chain (mTradeIr*2Aout / mvTradeIrAOut). Seasonal resolution.
tm_HET <- newCommodity("HET", desc = "Waste heat (auxiliary)",
                       timeframe = "SEASON")

# CO2: emission commodity (feature: emissions).
tm_CO2 <- newCommodity("CO2", desc = "Carbon dioxide", timeframe = "ANNUAL")

# GHG: aggregate commodity bundling CO2 (feature: aggregate / mAggOut).
tm_GHG <- newCommodity(
  name = "GHG",
  desc = "Aggregate greenhouse gas (CO2-equivalent)",
  timeframe = "ANNUAL",
  agg = data.frame(comm = "CO2", unit = "kt", agg = 1)
)

# --- Supply ---------------------------------------------------------------- #

# Coal supply, available in R1 only (region-restricted via availability).
tm_SUP_COA <- newSupply(
  name = "SUP_COA",
  commodity = "COA",
  availability = data.frame(region = "R1", cost = 1)
)

# Coal supply variant with a finite availability upper bound (feature:
# mSupAvaUp). Replaces tm_SUP_COA in the tm_io tier only, so the core tiers are
# unaffected.
tm_SUP_COA_cap <- newSupply(
  name = "SUP_COA",
  commodity = "COA",
  availability = data.frame(region = "R1", cost = 1, ava.up = 500)
)

# Coal supply variant with weather-dependent availability (feature: weather
# map mSupWeatherUp). Replaces tm_SUP_COA in the tm_weather tier.
tm_SUP_COA_weather <- newSupply(
  name = "SUP_COA",
  commodity = "COA",
  availability = data.frame(region = "R1", cost = 1),
  weather = data.frame(weather = "WWIN", wava.up = 1)  # -> mSupWeatherUp
)

# --- Technologies ---------------------------------------------------------- #

# Coal power plant: COA -> ELC, available in R1. Used by tm_core (without aux)
# and tm_flows (with the auxiliary water input declared on tm_ECOA_aux).
tm_ECOA <- newTechnology(
  name = "ECOA",
  input = list(comm = "COA"),
  output = list(comm = "ELC"),
  invcost = data.frame(region = "R1", invcost = 1000),
  olife = list(olife = 30),
  cap2act = 1
)

# Coal plant variant WITH an auxiliary cooling-water input (feature:
# aux-conversion maps mTechAct2AInp etc.). Consumes WAT proportional to
# activity. Replaces tm_ECOA in the tm_flows tier and above.
tm_ECOA_aux <- newTechnology(
  name = "ECOA",
  input = list(comm = "COA"),
  output = list(comm = "ELC"),
  aux = data.frame(acomm = "WAT", unit = "Mm3"),
  aeff = rbind(
    # activity- and capacity-driven water use (single-commodity maps
    # mTechAct2AInp / mTechCap2AInp).
    data.frame(acomm = "WAT", comm = NA_character_,
               act2ainp = 0.02, cap2ainp = 0.10, cinp2ainp = NA_real_),
    # coal-input-driven water use (two-commodity map mTechCinp2AInp:
    # comm = WAT, comm.1 = COA).
    data.frame(acomm = "WAT", comm = "COA",
               act2ainp = NA_real_, cap2ainp = NA_real_, cinp2ainp = 0.50)
  ),
  invcost = data.frame(region = "R1", invcost = 1000),
  olife = list(olife = 30),
  cap2act = 1
)

# Wind plant: -> ELC, available in R1. tm_core variant (no weather).
tm_EWIN <- newTechnology(
  name = "EWIN",
  output = list(comm = "ELC"),
  invcost = data.frame(region = "R1", invcost = 1500),
  olife = list(olife = 25),
  cap2act = 1
)

# Wind plant variant with weather-dependent availability (feature: weather
# maps mTechWeatherAf*). Replaces tm_EWIN in the tm_weather tier.
tm_EWIN_weather <- newTechnology(
  name = "EWIN",
  output = list(comm = "ELC"),
  weather = data.frame(
    weather = c("WWIN", "WWIN"),
    comm    = c(NA, "ELC"),
    waf.up  = c(1,  NA),    # -> mTechWeatherAfUp
    waf.lo  = c(0.1, NA),   # -> mTechWeatherAfLo
    wafs.up = c(0.8, NA),   # -> mTechWeatherAfsUp
    wafc.up = c(NA, 0.9)    # -> mTechWeatherAfcUp  (commodity-specific)
  ),
  invcost = data.frame(region = "R1", invcost = 1500),
  olife = list(olife = 25),
  cap2act = 1
)

# --- Demand ---------------------------------------------------------------- #

# Electricity demand in both regions (R2 served via trade).
tm_DEM_ELC <- newDemand(
  name = "DEM_ELC",
  commodity = "ELC",
  dem = data.frame(
    region = c("R1", "R1", "R2", "R2"),
    slice  = c("WIN", "SUM", "WIN", "SUM"),
    dem    = c(10, 8, 6, 5)
  )
)

# --- Storage --------------------------------------------------------------- #

# Electricity storage.
tm_STG_ELC <- newStorage(
  name = "STG_ELC",
  desc = "Electricity storage",
  commodity = "ELC",
  invcost = list(invcost = 50),
  olife = list(olife = 15)
)

# Storage variant carrying auxiliary commodity flows (feature: storage
# aux-conversion maps mStorageStg2AInp / mStorageStg2AOut / mStorageCap2AInp).
# The store consumes cooling water (WAT) proportional to capacity and stored
# energy and releases waste heat (HET) proportional to the stored energy.
# Replaces tm_STG_ELC from the tm_flows tier onward.
tm_STG_ELC_aux <- newStorage(
  name = "STG_ELC",
  desc = "Electricity storage with auxiliary flows",
  commodity = "ELC",
  aux = data.frame(
    acomm = c("WAT", "HET"),
    unit  = c("Mm3", "TJ")
  ),
  aeff = rbind(
    data.frame(acomm = "WAT", stg2ainp = 0.02, cap2ainp = 0.05,
               stg2aout = NA_real_),
    data.frame(acomm = "HET", stg2ainp = NA_real_, cap2ainp = NA_real_,
               stg2aout = 0.01)
  ),
  invcost = list(invcost = 50),
  olife = list(olife = 15)
)

# Storage variant adding weather-dependent availability on top of the aux flows
# (feature: storage weather maps mStorageWeatherAfUp / CinpUp / CoutUp).
# Replaces tm_STG_ELC_aux in the tm_weather tier.
tm_STG_ELC_weather <- newStorage(
  name = "STG_ELC",
  desc = "Electricity storage with auxiliary flows and weather availability",
  commodity = "ELC",
  aux = data.frame(
    acomm = c("WAT", "HET"),
    unit  = c("Mm3", "TJ")
  ),
  aeff = rbind(
    data.frame(acomm = "WAT", stg2ainp = 0.02, cap2ainp = 0.05,
               stg2aout = NA_real_),
    data.frame(acomm = "HET", stg2ainp = NA_real_, cap2ainp = NA_real_,
               stg2aout = 0.01)
  ),
  weather = data.frame(
    weather  = "WWIN",
    waf.up   = 1,    # -> mStorageWeatherAfUp
    wcinp.up = 0.9,  # -> mStorageWeatherCinpUp
    wcout.up = 0.8   # -> mStorageWeatherCoutUp
  ),
  invcost = list(invcost = 50),
  olife = list(olife = 15)
)

# --- Trade ----------------------------------------------------------------- #

# Inter-regional electricity trade R1 -> R2 (feature: trade-Ir chain).
tm_TRD_ELC <- newTrade(
  name = "TRD_ELC",
  commodity = "ELC",
  routes = data.frame(src = "R1", dst = "R2"),
  invcost = list(invcost = 100),
  olife = list(olife = 30)
)

# Trade variant carrying auxiliary commodities (feature: trade aux chain
# mTradeIrAInp/AOut, mTradeIr*2Ainp/Aout, mvTradeIrAInp/AOut). The transmission
# line consumes cooling water (WAT) at BOTH ends (csrc2ainp at the source,
# cdst2ainp at the destination) and produces waste heat (HET) at the
# destination (cdst2aout). Replaces tm_TRD_ELC from tm_flows onward.
tm_TRD_ELC_aux <- newTrade(
  name = "TRD_ELC",
  commodity = "ELC",
  routes = data.frame(src = "R1", dst = "R2"),
  aux = data.frame(
    acomm = c("WAT", "HET"),
    unit  = c("Mm3", "TJ")
  ),
  aeff = rbind(
    data.frame(acomm = "WAT", csrc2ainp = 0.01, cdst2ainp = 0.02,
               cdst2aout = NA_real_),
    data.frame(acomm = "HET", csrc2ainp = NA_real_, cdst2ainp = NA_real_,
               cdst2aout = 0.03)
  ),
  invcost = list(invcost = 100),
  olife = list(olife = 30)
)

# --- Import / Export to the Rest of the World ------------------------------ #

# Coal import to R2 (feature: import maps mImport / mImportRow).
tm_IMP_COA <- newImport(
  name = "IMP_COA",
  desc = "Coal import from RoW (R2)",
  commodity = "COA",
  imp = data.frame(region = "R2", price = 2, imp.up = 100)
)

# Electricity export from R1 (feature: export maps mExport / mExportRow).
tm_EXP_ELC <- newExport(
  name = "EXP_ELC",
  desc = "Electricity export to RoW (R1)",
  commodity = "ELC",
  exp = data.frame(region = "R1", price = 3, exp.up = 50)
)

# --- Tax / Subsidy --------------------------------------------------------- #

# Tax on net CO2 balance in both regions (feature: mTaxCost).
tm_TAX_CO2 <- newTax(
  name = "TAX_CO2",
  desc = "Carbon tax on net CO2",
  comm = "CO2",
  tax = data.frame(year = c(2020, 2050), bal = c(10, 100))
)

# Subsidy on electricity output (feature: mSubCost).
tm_SUB_ELC <- newSub(
  name = "SUB_ELC",
  desc = "Electricity output subsidy (R1)",
  comm = "ELC",
  region = "R1",
  sub = data.frame(year = c(2020, 2050), out = c(1, 0.5))
)

# --- Weather --------------------------------------------------------------- #

# Wind availability profile (feature: weather). Seasonal capacity factor.
tm_WWIN <- newWeather(
  name = "WWIN",
  desc = "Wind availability factor",
  timeframe = "SEASON",
  weather = data.frame(
    region = rep(.tm_regions, each = 4),
    year   = 2020L,
    slice  = rep(c("WIN", "SPR", "SUM", "AUT"), times = 2),
    wval   = c(0.5, 0.3, 0.2, 0.4, 0.5, 0.3, 0.2, 0.4)
  )
)

# =========================================================================== #
# B. REPOSITORY
# =========================================================================== #
# Full catalog of every object. Individual models draw a subset of these.
# (Both ECOA / EWIN variants are kept as separate objects but NOT both put in a
# single model, since they share a name; each builder picks the variant it
# needs.)
repo_testing_model <- newRepository(
  "repo_testing_model",
  desc = "Catalog of feature-isolating objects for energyRt test models",
  # commodities
  tm_COA, tm_ELC, tm_WAT, tm_HET, tm_CO2, tm_GHG,
  # supply / demand
  tm_SUP_COA, tm_DEM_ELC,
  # technologies (core variants; *_aux / *_weather variants added per model)
  tm_ECOA, tm_EWIN,
  # storage / trade
  tm_STG_ELC, tm_TRD_ELC,
  # import / export
  tm_IMP_COA, tm_EXP_ELC,
  # policy
  tm_TAX_CO2, tm_SUB_ELC,
  # weather
  tm_WWIN
)

# =========================================================================== #
# C. MODELS (cumulative tiers)
# =========================================================================== #

# Internal helper: assemble a repository from objects + standard config.
.tm_model <- function(name, desc, ...) {
  repo <- newRepository(paste0("repo_", name), ...)
  newModel(
    name = name,
    desc = desc,
    repo = repo,
    calendar = .tm_calendar(),
    region = .tm_regions,
    horizon = .tm_horizon(),
    discount = 0.05
  )
}

#' tm_core: base + storage + trade.
#'
#' Two regions, ANNUAL + 4 seasons, coal supply in R1, coal & wind generation
#' in R1, electricity demand in both regions, electricity storage, and R1->R2
#' electricity trade. Exercises the membership / calendar / lifespan / core
#' filter recipes and the full trade-Ir chain.
tm_core <- function() {
  .tm_model(
    "tm_core",
    "Base test model: supply + tech + demand + storage + trade",
    tm_COA, tm_ELC,
    tm_SUP_COA, tm_ECOA, tm_EWIN, tm_DEM_ELC,
    tm_STG_ELC, tm_TRD_ELC
  )
}

#' tm_flows: tm_core + aux commodities + emissions + aggregate.
#'
#' Adds an auxiliary cooling-water input on the coal plant (aux-conversion
#' maps), a CO2 emission factor on coal (emission maps), and a GHG aggregate
#' commodity (aggregate map). Uses the tm_ECOA_aux variant in place of tm_ECOA.
tm_flows <- function() {
  .tm_model(
    "tm_flows",
    "tm_core + aux water input + CO2 emissions + GHG aggregate",
    tm_COA_emis, tm_ELC, tm_WAT, tm_HET, tm_CO2, tm_GHG,
    tm_SUP_COA, tm_ECOA_aux, tm_EWIN, tm_DEM_ELC,
    tm_STG_ELC_aux, tm_TRD_ELC_aux
  )
}

#' tm_io: tm_flows + import + export (no tax / subsidy).
#'
#' Adds coal import to R2 (with a finite imp.up) and electricity export from R1
#' (with a finite exp.up), and switches coal supply to the capped variant with
#' a finite ava.up. Isolates the import / export filter maps (mImportRow,
#' mImportRowUp, mExportRow, mExportRowUp, meq*RowLo, *RowCumUp) and mSupAvaUp
#' from the tax / subsidy value maps (which remain in tm_policy).
tm_io <- function() {
  .tm_model(
    "tm_io",
    "tm_flows + coal import + ELC export + capped coal supply",
    tm_COA_emis, tm_ELC, tm_WAT, tm_HET, tm_CO2, tm_GHG,
    tm_SUP_COA_cap, tm_ECOA_aux, tm_EWIN, tm_DEM_ELC,
    tm_STG_ELC_aux, tm_TRD_ELC_aux,
    tm_IMP_COA, tm_EXP_ELC
  )
}

#' tm_policy: tm_flows + tax + subsidy + import + export.
#'
#' Adds a carbon tax, an electricity output subsidy, coal import to R2, and
#' electricity export from R1. Exercises the cost / tax / subsidy / import /
#' export filter and value maps.
tm_policy <- function() {
  .tm_model(
    "tm_policy",
    "tm_flows + carbon tax + ELC subsidy + coal import + ELC export",
    tm_COA_emis, tm_ELC, tm_WAT, tm_HET, tm_CO2, tm_GHG,
    tm_SUP_COA, tm_ECOA_aux, tm_EWIN, tm_DEM_ELC,
    tm_STG_ELC_aux, tm_TRD_ELC_aux,
    tm_IMP_COA, tm_EXP_ELC,
    tm_TAX_CO2, tm_SUB_ELC
  )
}

#' tm_weather: tm_policy + weather-dependent availability.
#'
#' Adds a wind availability profile and switches the wind plant to its
#' weather-driven variant. Exercises the weather maps.
tm_weather <- function() {
  .tm_model(
    "tm_weather",
    "tm_policy + weather-dependent wind availability",
    tm_COA_emis, tm_ELC, tm_WAT, tm_HET, tm_CO2, tm_GHG,
    tm_SUP_COA_weather, tm_ECOA_aux, tm_EWIN_weather, tm_DEM_ELC,
    tm_STG_ELC_weather, tm_TRD_ELC_aux,
    tm_IMP_COA, tm_EXP_ELC,
    tm_TAX_CO2, tm_SUB_ELC,
    tm_WWIN
  )
}
