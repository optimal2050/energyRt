## data-raw/utopia_modules.R
## Build the packaged `utopia_modules` -- a kit of energyRt building blocks and
## scenario levers for the UTOPIA teaching model, mirroring the structure of
## IDEEA::ideea_modules. Region layouts (reg1/reg3/reg7) are assembled on the
## base `utopia_s4h24` calendar by the local `build_utopia()` below (the same
## explicit steps the "UTOPIA I: building the model" vignette walks through).
##
## Naming conventions (see the vignette's "Conventions" section):
##   * set elements (regions, commodities, technologies) are UPPER-CASE, so the
##     model behaves identically on case-sensitive (GLPK/Pyomo/JuMP) and
##     case-insensitive (GAMS) backends;
##   * supply objects  -> SUP_*  ; renewable resources -> RES_* ;
##     storage -> STG_* ; trade -> TRD_* (bi-directional TBD_*) ;
##     export -> EXP_*  ; import -> IMP_*.
##
## Run: pkgload::load_all(".") ; source("data-raw/utopia_modules.R") ; devtools::document()

if (!isNamespaceLoaded("energyRt")) library(energyRt)
library(usethis)

# Renewable-target lever: a minimum renewable (SOL/WIN/HYD) generation floor that
# grows by year, expressed as a share of national electricity demand. Single term
# (renewable output >= floor) so it binds cleanly.
res_share_lever <- function(years, growth, annual_demand, nreg,
                            share = c(0.15, 0.50)) {
  ren <- c("ESOL", "EWIN", "EHYD")
  tot <- annual_demand * growth * nreg                 # ~national demand, PJ
  sh  <- seq(share[1], share[2], length.out = length(years))
  newConstraint(name = "RES_SHARE", eq = ">=",
    for.each = data.frame(year = years, comm = "ELC"),
    term1 = list(variable = "vTechOut", for.sum = list(tech = ren)),
    rhs = data.frame(year = years, rhs = sh * tot), defVal = 0)
}

# Assemble a complete UTOPIA electricity kit for `regions` on `calendar`.
build_utopia <- function(regions = paste0("R", 1:3),
                         calendar = "utopia_s4h24",
                         annual_demand = 100,
                         years = c(2020, 2030, 2040, 2050),
                         demand_growth = c(1, 1.2, 1.4, 1.6)) {
  stopifnot(is.character(regions), length(regions) > 0)
  cal  <- calendars[[calendar]]
  if (is.null(cal)) stop("unknown calendar '", calendar, "'")
  prof <- utopia_profiles(regions, calendar = calendar)
  mg   <- function(eur_kw) convert("EUR/kW", "MEUR/GW", eur_kw)   # -> MEUR/GW

  # ---- commodities ----
  COA <- newCommodity("COA", timeframe = "ANNUAL",
    emis = data.frame(comm = "CO2", unit = "kt/PJ", emis = 95))
  GAS <- newCommodity("GAS", timeframe = "ANNUAL",
    emis = data.frame(comm = "CO2", unit = "kt/PJ", emis = 56))
  BIO <- newCommodity("BIO", timeframe = "ANNUAL")
  NUC <- newCommodity("NUC", timeframe = "ANNUAL")
  SOL <- newCommodity("SOL", timeframe = "HOUR")
  WIN <- newCommodity("WIN", timeframe = "HOUR")
  HYD <- newCommodity("HYD", timeframe = "HOUR")
  ELC <- newCommodity("ELC", timeframe = "HOUR")
  CO2 <- newCommodity("CO2", timeframe = "ANNUAL")
  repo_comm <- newRepository("utopia_comm",
    COA, GAS, BIO, NUC, SOL, WIN, HYD, ELC, CO2)

  # ---- regional endowments (drive inter-regional + rest-of-world trade) ------
  # coal is mined in R1/R4/R7/R10, gas produced in R2/R5/R8/R11, hydro flows in
  # R3/R6/R9; biomass and nuclear fuel are available everywhere. Regions without
  # a domestic fuel import it (IMP_*) or buy electricity over the grid (TBD_*).
  coal_regs  <- intersect(regions, paste0("R", c(1, 4, 7, 10)))
  gas_regs   <- intersect(regions, paste0("R", c(2, 5, 8, 11)))
  hydro_regs <- intersect(regions, paste0("R", c(3, 6, 9)))

  # ---- supply (fuels SUP_*, free renewable resources RES_*; EUR/GJ = MEUR/PJ) ----
  sup <- function(nm, comm, cost, reg = regions) newSupply(nm, commodity = comm,
    availability = data.frame(region = reg, cost = cost))
  supplies <- list()
  if (length(coal_regs) > 0) supplies$SUP_COA <- sup("SUP_COA", "COA", 2.5, coal_regs)
  if (length(gas_regs)  > 0) supplies$SUP_GAS <- sup("SUP_GAS", "GAS", 6.0, gas_regs)
  supplies$SUP_BIO <- sup("SUP_BIO", "BIO", 8.0)
  supplies$SUP_NUC <- sup("SUP_NUC", "NUC", 0.9)
  supplies$RES_SOL <- sup("RES_SOL", "SOL", 0)
  supplies$RES_WIN <- sup("RES_WIN", "WIN", 0)
  if (length(hydro_regs) > 0)
    supplies$RES_HYD <- sup("RES_HYD", "HYD", 0, hydro_regs)
  repo_supply <- newRepository("utopia_supply", supplies)

  # ---- rest-of-world trade: fuel imports at a premium, capped ELC exports ----
  IMP_COA <- newImport("IMP_COA", desc = "Coal import from the rest of the world",
    commodity = "COA", unit = "PJ",
    imp = data.frame(region = regions, price = 3.5))
  IMP_GAS <- newImport("IMP_GAS", desc = "LNG import from the rest of the world",
    commodity = "GAS", unit = "PJ",
    imp = data.frame(region = regions, price = 9.0))
  # export price below new-build LCOE (never build-to-export); exp.up paced by
  # slice-shares (~10 PJ/yr/region -- a bare exp.up would be read PER SLICE and
  # the model would front-load the whole reserve into the base year)
  sl <- as.data.frame(cal@slice_share)
  EXP_ELC <- newExport("EXP_ELC", desc = "Electricity export to the rest of the world",
    commodity = "ELC", unit = "PJ", reserve = 300,
    exp = merge(data.frame(region = regions, price = 5.0),
                data.frame(slice = sl$slice, exp.up = 10 * sl$share)))

  # ---- weather ----
  wobj <- function(res) newWeather(res, timeframe = "HOUR",
    weather = prof$weather[prof$weather$resource == res, c("region", "slice", "wval")])
  WSOL <- wobj("WSOL"); WWIN <- wobj("WWIN"); WHYD <- wobj("WHYD")

  # ---- demand ----
  share <- as.data.frame(cal@slice_share)[, c("slice", "share")]
  d0 <- merge(prof$demand, share, by = "slice"); d0$w <- d0$load * d0$share
  dem_rows <- do.call(rbind, lapply(seq_along(years), function(i) {
    do.call(rbind, lapply(regions, function(r) {
      dr <- d0[d0$region == r, ]
      data.frame(region = r, year = years[i], slice = dr$slice,
                 dem = annual_demand * demand_growth[i] * dr$w / sum(dr$w))
    }))
  }))
  DEM_ELC <- newDemand("DEM_ELC", commodity = "ELC", dem = dem_rows)

  # ---- technologies (names are UPPER-CASE set elements; no prefix required) ----
  # Existing stock is a DECLINING path (base year -> 0 at `ret_year`): the fleet
  # retires on a schedule -- ~20y for solar/wind, ~30y thermal, 40y nuclear,
  # 50y hydro. Stocks follow the endowments (coal plants near the mines, gas
  # plants on the gas, hydro only in hydro regions). `start`/`end` bound the
  # years new capacity can be built.
  by <- min(years)
  stk <- function(gw_by_region, ret_year) {
    gw <- gw_by_region[gw_by_region > 0]
    if (length(gw) == 0) return(data.frame())
    data.frame(region = rep(names(gw), 2),
               year   = rep(c(by, ret_year), each = length(gw)),
               stock  = c(unname(gw), rep(0, length(gw))))
  }
  gw_rule <- function(base, bonus_regs, bonus) {
    stats::setNames(ifelse(regions %in% bonus_regs, bonus, base), regions)
  }
  thermal <- function(nm, comm, eff, inv, fixom, ...) newTechnology(nm,
    input = list(comm = comm, combustion = 1), output = list(comm = "ELC"),
    ceff = data.frame(comm = comm, cinp2use = eff), cap2act = 31.536, fixom = fixom,
    invcost = list(invcost = inv), olife = 30L, start = 2010L,
    optimizeRetirement = TRUE, ...)
  ECOA <- thermal("ECOA", "COA", 0.40, mg(2000), 55, end = 2030L,  # no new coal > 2030
    capacity = stk(gw_rule(3, coal_regs, 8), by + 30))
  EGAS <- thermal("EGAS", "GAS", 0.58, mg(900), 25,
    capacity = stk(gw_rule(1, gas_regs, 6), by + 30))
  ENUC <- newTechnology("ENUC", input = list(comm = "NUC"), output = list(comm = "ELC"),
    ceff = data.frame(comm = "NUC", cinp2use = 0.35), af = data.frame(af.lo = 0.7),
    cap2act = 31.536, fixom = 120, invcost = list(invcost = mg(8000)),
    olife = 50L, start = 2025L,                       # licensing/construction lead
    capacity = stk(gw_rule(2, character(0), 2), by + 40))
  vre <- function(nm, comm, wname, inv, fixom, olife = 25L, start = 2015L, ...)
    newTechnology(nm,
      input = list(comm = comm), output = list(comm = "ELC"),
      ceff = data.frame(comm = comm, cinp2use = 1),
      weather = list(weather = wname, comm = comm, waf.up = 1),
      cap2act = 31.536, fixom = fixom, invcost = list(invcost = inv),
      olife = olife, start = start, ...)
  ESOL <- vre("ESOL", "SOL", "WSOL", mg(650), 12,
              capacity = stk(gw_rule(1, character(0), 1), by + 20))
  EWIN <- vre("EWIN", "WIN", "WWIN", mg(1300), 35,
              capacity = stk(gw_rule(1, character(0), 1), by + 20))
  EHYD <- vre("EHYD", "HYD", "WHYD", mg(3000), 45,
              capacity = stk(gw_rule(0, hydro_regs, 12), by + 50),
              start = 2010L, end = 2010L, olife = 60L)  # window closed: legacy only
  EBIO <- newTechnology("EBIO", desc = "Biomass power plant (carbon-neutral)",
    input  = list(comm = "BIO", combustion = 1), output = list(comm = "ELC"),
    ceff   = data.frame(comm = "BIO", cinp2use = 0.35),
    cap2act = 31.536, fixom = 60, invcost = list(invcost = mg(2200)),
    olife = 30L, start = 2025L, optimizeRetirement = TRUE)

  # ---- storage (STG_*, 4-hour battery, ~200 EUR/kWh energy) ----
  STG_BTR <- newStorage("STG_ELC", commodity = "ELC", olife = 20L,
    invcost = list(invcost = convert("EUR/kWh", "MEUR/PJ", 200)),
    cap2stg = 4, seff = data.frame(inpeff = 0.95, outeff = 0.95))

  # ---- interregional trade (TBD_*, bi-directional links along the chain) ----
  trades <- list()
  if (length(regions) > 1) {
    for (i in seq_len(length(regions) - 1)) {
      r1 <- regions[i]; r2 <- regions[i + 1]
      trades[[length(trades) + 1L]] <- newTrade(
        name = paste("TBD_ELC", r1, r2, sep = "_"),
        desc = paste("Bidirectional transmission line", r1, "-", r2),
        commodity = "ELC",
        routes = data.frame(src = c(r1, r2), dst = c(r2, r1)),
        trade  = data.frame(src = c(r1, r2), dst = c(r2, r1), teff = 0.97),
        capacity = data.frame(stock = 1),
        capacityVariable = TRUE,
        invcost = data.frame(region = c(r1, r2), invcost = 350),
        olife = list(olife = 50))
    }
  }

  # ---- base repository (order mirrors the UTOPIA I vignette) ----
  repo <- newRepository("utopia",
    COA, GAS, BIO, NUC, SOL, WIN, HYD, ELC, CO2,
    repo_supply,
    IMP_COA, IMP_GAS, EXP_ELC,
    WSOL, WWIN, WHYD,
    ECOA, EGAS, ENUC, ESOL, EWIN, EHYD, EBIO, STG_BTR)
  for (trd in trades) repo <- add(repo, trd)
  repo <- add(repo, DEM_ELC)

  # ---- scenario levers (pre-built objects) ----
  # CO2_CAP starts near BASE 2020 emissions (~5,000 kt/region) and tightens to a
  # deep cut. CT_CO2: costs are MEUR, emissions kt -> 20-80 EUR/t = 0.02-0.08.
  nreg <- length(regions)
  CO2_CAP <- newConstraint(name = "CO2_CAP", eq = "<=",
    for.each = data.frame(year = years, comm = "CO2"),
    term1 = list(variable = "vEmsFuelTot"),
    rhs = data.frame(year = range(years), rhs = c(5000, 2500) * nreg),
    defVal = Inf)
  CT_CO2 <- newTax(name = "CT_CO2", comm = "CO2",
    tax = data.frame(year = range(years), bal = c(0.02, 0.08)))
  RES_SHARE <- res_share_lever(years, demand_growth, annual_demand, nreg)
  NO_NEW_NUC <- newConstraint(name = "NO_NEW_NUC", eq = "<=",
    for.each = data.frame(year = years, tech = "ENUC"),
    term1 = list(variable = "vTechNewCap"),
    rhs = 0, defVal = 0)
  # forced early retirement of the fossil fleets: a declining per-region ceiling
  # on installed coal and gas capacity (vTechCap is per region, so the ceiling
  # applies region by region; optimizeRetirement lets the model pick which units
  # go). The binding effect is on GAS: without it the model builds gas out to
  # ~7-17 GW/region as the flexible VRE backstop; coal already phases out on
  # economics. A full fossil exit is infeasible here (must-run nuclear + limited
  # transmission need some firm fossil capacity for peak), so the schedule keeps
  # a firm-backup floor while cutting system CO2 ~40%.
  # for.each = full year x tech grid (expand.grid); rhs = declining ceiling.
  # NOTE: scope for.each to 2030+ only -- including the 2020 milestone (even with
  # an unbound Inf ceiling) makes the LP infeasible via rhs interpolation.
  EARLY_RET <- newConstraint(name = "EARLY_RET", eq = "<=",
    for.each = expand.grid(year = c(2030, 2040, 2050), tech = c("ECOA", "EGAS"),
                           stringsAsFactors = FALSE),
    term1 = list(variable = "vTechCap"),
    rhs = data.frame(
      tech = rep(c("ECOA", "EGAS"), each = 3),
      year = rep(c(2030, 2040, 2050), 2),
      rhs  = c(12, 10, 8,    # coal ceiling (GW/region), declining
                7,  6, 5)),  # gas ceiling  (GW/region), declining
    defVal = Inf)

  list(regions = regions, calendar = calendar, repo = repo,
       repo_comm = repo_comm, repo_supply = repo_supply,
       DEM_ELC = DEM_ELC,
       WSOL = WSOL, WWIN = WWIN, WHYD = WHYD,
       ECOA = ECOA, EGAS = EGAS, ENUC = ENUC, ESOL = ESOL, EWIN = EWIN,
       EHYD = EHYD, EBIO = EBIO, STG_BTR = STG_BTR,
       CO2_CAP = CO2_CAP, CT_CO2 = CT_CO2, RES_SHARE = RES_SHARE,
       NO_NEW_NUC = NO_NEW_NUC, EARLY_RET = EARLY_RET)
}

base_horizon <- newHorizon(period = 2020:2050, intervals = c(1, 10, 10, 10),
                           mid_is_end = TRUE, name = "base",
                           desc = "2020-2050, milestones 2020/2030/2040/2050")

utopia_modules <- list(
  info = paste("UTOPIA teaching-model modules: a kit of energyRt building blocks",
               "(commodity/supply repositories, weather, technologies, storage, a",
               "ready base repository `$repo`) and scenario levers (CO2_CAP, CT_CO2,",
               "RES_SHARE, NO_NEW_NUC), for region layouts under `$electricity`.",
               "Built by the UTOPIA I vignette's explicit steps; mirrors",
               "IDEEA::ideea_modules."),
  maps      = utopia$map,
  calendars = calendars[c("utopia_annual", "utopia_seasons",
                          "utopia_s4h24", "utopia_m12h24")],
  horizons  = list(base = base_horizon),
  electricity = list(
    reg1 = build_utopia("R1",                calendar = "utopia_s4h24"),
    reg3 = build_utopia(paste0("R", 1:3),    calendar = "utopia_s4h24"),
    reg7 = build_utopia(paste0("R", 1:7),    calendar = "utopia_s4h24")
  )
)

# sanity: each config has a base repo + the four levers
for (cfg in names(utopia_modules$electricity)) {
  k <- utopia_modules$electricity[[cfg]]
  stopifnot(methods::is(k$repo, "repository"),
            all(c("CO2_CAP", "CT_CO2", "RES_SHARE", "NO_NEW_NUC",
                  "EARLY_RET") %in% names(k)))
}
message("utopia_modules configs: ",
        paste(names(utopia_modules$electricity), collapse = ", "))

usethis::use_data(utopia_modules, overwrite = TRUE)
