## data-raw/topia_modules.R
## Build the packaged `topia_modules` -- a kit of energyRt building blocks and
## scenario levers for the TOPIA teaching model, mirroring the structure of
## IDEEA::ideea_modules. Region layouts (R1/R3/R7/R11) are assembled on the
## base `s4_h24` calendar by the local `build_topia()` below (the same
## explicit steps the "TOPIA I: building the model" vignette walks through).
##
## Naming conventions (see the vignette's "Conventions" section):
##   * set elements (regions, commodities, technologies) are UPPER-CASE, so the
##     model behaves identically on case-sensitive (GLPK/Pyomo/JuMP) and
##     case-insensitive (GAMS) backends;
##   * supply objects  -> SUP_*  ; renewable resources -> RES_* ;
##     storage -> STG_* ; trade -> TRD_* (bi-directional TBD_*) ;
##     export -> EXP_*  ; import -> IMP_*.
##
## Run: pkgload::load_all(".") ; source("data-raw/topia_modules.R") ; devtools::document()

if (!isNamespaceLoaded("energyRt")) library(energyRt)
library(usethis)

# Renewable-target lever: a minimum renewable (SOL/WIN/HYD) generation floor that
# grows by year, expressed as a share of national electricity demand. Single term
# (renewable output >= floor) so it binds cleanly.
res_share_lever <- function(years, growth, annual_demand, nreg,
                            ren = c("ESOL", "EWIN", "EHYD"),
                            share = c(0.15, 0.50)) {
  tot <- annual_demand * growth * nreg                 # ~national demand, PJ
  sh  <- seq(share[1], share[2], length.out = length(years))
  newConstraint(name = "RES_SHARE", eq = ">=",
    for.each = data.frame(year = years, comm = "ELC"),
    term1 = list(variable = "vTechOut", for.sum = list(tech = ren)),
    rhs = data.frame(year = years, rhs = sh * tot), defVal = 0)
}

# Assemble a complete TOPIA electricity kit for `regions` on `calendar`.
# ── TOPIA's own calendars ────────────────────────────────────────────────────
# energyRt ships GENERIC calendars only (data-raw/calendars.R). These two are
# TOPIA's, so they travel with the model instead: `topia_seasons` in
# `$calendars`, the symmetric unit calendars in `$unit$calendars`.
#
# `topia_seasons` is a STYLISED teaching structure, not a catalog calendar: its
# seasons are equal quarters rather than real day counts, and the daypart split
# varies by season (long winter nights, long summer days), which encodes a
# latitude. The catalog's `s4_hp3` is the regular twin -- day-proportional
# seasons, a uniform 12/8/4 split -- and carries no such assumption.
topia_seasons <- newCalendar(
  make_timetable(list(
    SEASON = list(
      WIN = list(1 / 4, HOUR = list(DAY =  9 / 24, NGT = 12 / 24, PK = 3 / 24)),
      SPR = list(1 / 4, HOUR = list(DAY = 11 / 24, NGT = 11 / 24, PK = 2 / 24)),
      SUM = list(1 / 4, HOUR = list(DAY = 12 / 24, NGT =  9 / 24, PK = 3 / 24)),
      FAL = list(1 / 4, HOUR = list(DAY = 11 / 24, NGT = 11 / 24, PK = 2 / 24))
    )
  )),
  name = "topia_seasons",
  desc = "TOPIA: 4 seasons x 3 dayparts (DAY/NIGHT/PEAK), 12 timeslices"
)

# Every share is a power of 1/4, so weights are exactly 4 (and 16) and the unit
# kits' objectives come out as small integers. No catalog entry can serve this:
# timescales' `s4` is day-proportional (0.2466/0.2521/0.2521/0.2493).
unit_calendars <- list(
  unit_s4 = newCalendar(
    make_timetable(list(SEASON = paste0("S", 1:4))),
    name = "unit_s4",
    desc = "Unit calendar: 4 equal seasons (share 1/4 each, weight 4)"
  ),
  unit_s4h4 = newCalendar(
    make_timetable(list(
      SEASON = paste0("S", 1:4),
      HOUR   = paste0("H", 1:4)
    )),
    name = "unit_s4h4",
    desc = "Unit calendar: 4 seasons x 4 hours, 16 timeslices (share 1/16 each)"
  )
)

# name -> object for the builders below: the shipped generic list plus TOPIA's
topia_calendars <- c(calendars,
                     list(topia_seasons = topia_seasons),
                     unit_calendars)

# ── resource geography ───────────────────────────────────────────────────────
# Each resource is CONCENTRATED rather than spread evenly: one zone dominates
# coal, another gas, and hydro and nuclear sit in named regions. That is what
# gives the mid-level fuel corridors a direction and the regions distinct
# prices -- with an endowment in every region nothing would ever flow.
#
# Coal and gas are balanced at the ZONE (`commodity@geoframe`), so their
# endowments and rest-of-world imports are declared AT the zone: `@region` on a
# strict class (supply / import / export / trade / demand / storage) must match
# the commodity's own level. Hydro is an hourly region-level resource, and
# nuclear is restricted by the plant's own `@region`.
#
# Caps are per region OF THE ZONE, so a layout that reaches further east gets a
# proportionally larger pool; costs are USD/GJ, the same unit as the fuels'
# import prices.
.TOPIA_COAL      <- c(WEST = 150, CENTRAL = 40)     # PJ/yr per region of zone
.TOPIA_COAL_COST <- c(WEST = 1.8, CENTRAL = 2.8)    # USD/GJ
.TOPIA_GAS       <- c(EAST = 120, WEST = 25)
.TOPIA_GAS_COST  <- c(EAST = 4.0, WEST = 6.0)

# Where the dispatchable fleets and the free resources sit. A region absent
# from a table gets `base` (thermal) or nothing at all (hydro, nuclear).
.TOPIA_NUC_REGIONS <- "C1"
.TOPIA_NUC_GW      <- c(C1 = 4)                     # base-year nuclear, GW
.TOPIA_HYD_GW      <- c(C4 = 12, E1 = 6, C1 = 3)    # legacy reservoir hydro
# C4 dominates the hydro; the small C1 plant is what keeps the three-region
# layout (and the vignette built on it) able to show an hourly resource
.TOPIA_COAL_GW     <- c(W1 = 8, W2 = 4, C5 = 3)     # elsewhere 1 GW
.TOPIA_GAS_GW      <- c(E2 = 6, E3 = 4, E1 = 2, C6 = 2, W2 = 2)

build_topia <- function(regions = .topia_regions[1:3],
                         calendar = "s4_h24",
                         annual_demand = 100,
                         years = c(2025, 2030, 2040, 2050),
                         demand_growth = c(1, 1.1, 1.3, 1.5)) {
  stopifnot(is.character(regions), length(regions) > 0)
  cal  <- topia_calendars[[calendar]]
  if (is.null(cal)) stop("unknown calendar '", calendar, "'")
  prof <- topia_profiles(regions, calendar = calendar)
  mg   <- function(usd_kw) convert("USD/kW", "MUSD/GW", usd_kw)   # -> MUSD/GW

  # the layout's place in the hierarchy: which zones it spans, and how many of
  # each zone's regions it carries (the endowments scale with that)
  zone_of <- stats::setNames(geo$zone, geo$region)
  zones   <- unique(unname(zone_of[regions]))
  zcount  <- table(unname(zone_of[regions]))

  # ---- commodities ----
  # COA and GAS are pooled inside a zone and move between zones on the
  # mid-level corridors below; ELC is balanced at the finest level, so
  # electricity trades region to region.
  COA <- newCommodity("COA", timeframe = "ANNUAL", geoframe = "zone",
    emis = data.frame(comm = "CO2", unit = "kt/PJ", emis = 95))
  GAS <- newCommodity("GAS", timeframe = "ANNUAL", geoframe = "zone",
    emis = data.frame(comm = "CO2", unit = "kt/PJ", emis = 56))
  BIO <- newCommodity("BIO", timeframe = "ANNUAL")
  NUC <- newCommodity("NUC", timeframe = "ANNUAL")
  SOL <- newCommodity("SOL", timeframe = "HOUR")
  WIN <- newCommodity("WIN", timeframe = "HOUR")
  HYD <- newCommodity("HYD", timeframe = "HOUR")
  ELC <- newCommodity("ELC", timeframe = "HOUR")
  CO2 <- newCommodity("CO2", timeframe = "ANNUAL")
  repo_comm <- newRepository("topia_comm",
    COA, GAS, BIO, NUC, SOL, WIN, HYD, ELC, CO2)

  # which concentrated resources this layout actually reaches
  nuc_regs   <- intersect(regions, .TOPIA_NUC_REGIONS)
  hyd_regs   <- intersect(regions, names(.TOPIA_HYD_GW))
  coal_zones <- intersect(zones, names(.TOPIA_COAL))
  gas_zones  <- intersect(zones, names(.TOPIA_GAS))

  # ---- supply: finite domestic endowments, free renewable resources ----------
  # `ava.up` is what makes the geography bite: a region can exhaust its own
  # fuel, and then pays either a corridor (zone trade) or the rest of the world
  # (IMP_*). The pre-0.92 kits declared unbounded supplies, so no fuel ever
  # flowed and every region faced the same price.
  sup_zone <- function(nm, comm, desc, caps, costs, z) {
    if (length(z) == 0) return(NULL)
    newSupply(nm, desc = desc, commodity = comm, region = z,
      supply = data.frame(region = z, cost = unname(costs[z]),
                          ava.up = unname(caps[z]) * as.integer(zcount[z])))
  }
  sup_reg <- function(nm, comm, desc, cost, reg, ava = NULL) {
    if (length(reg) == 0) return(NULL)
    d <- data.frame(region = reg, cost = cost)
    if (!is.null(ava)) d$ava.up <- ava
    newSupply(nm, desc = desc, commodity = comm, region = reg, supply = d)
  }
  supplies <- list(
    SUP_COA = sup_zone("SUP_COA", "COA", "Domestic coal, pooled within a zone",
                       .TOPIA_COAL, .TOPIA_COAL_COST, coal_zones),
    SUP_GAS = sup_zone("SUP_GAS", "GAS", "Domestic gas, pooled within a zone",
                       .TOPIA_GAS, .TOPIA_GAS_COST, gas_zones),
    SUP_BIO = sup_reg("SUP_BIO", "BIO", "Biomass residues, collected locally",
                      8.0, regions, ava = 15),
    SUP_NUC = sup_reg("SUP_NUC", "NUC", "Nuclear fuel assemblies",
                      0.9, nuc_regs),
    RES_SOL = sup_reg("RES_SOL", "SOL", "Solar resource", 0, regions),
    RES_WIN = sup_reg("RES_WIN", "WIN", "Wind resource", 0, regions),
    RES_HYD = sup_reg("RES_HYD", "HYD", "River inflow to the reservoirs",
                      0, hyd_regs))
  supplies <- Filter(Negate(is.null), supplies)
  repo_supply <- newRepository("topia_supply", supplies)

  # ---- rest-of-world trade: fuel imports at a premium, capped ELC exports ----
  # The fuel imports are the unbounded backstop above every domestic cap, and
  # they are declared at the ZONE because that is where the fuels are balanced.
  IMP_COA <- newImport("IMP_COA", desc = "Coal import from the rest of the world",
    commodity = "COA", unit = "PJ", region = zones,
    import = data.frame(region = zones, price = 3.5))
  IMP_GAS <- newImport("IMP_GAS", desc = "LNG import from the rest of the world",
    commodity = "GAS", unit = "PJ", region = zones,
    import = data.frame(region = zones, price = 9.0))
  # export price below new-build LCOE (never build-to-export); exp.up paced by
  # timeslice-shares (~10 PJ/yr/region -- a bare exp.up would be read PER TIMESLICE and
  # the model would front-load the whole reserve into the base year)
  #
  # Restricted to the timeslices ELC is BALANCED at. `@timeslice_share` carries every
  # level (ANNUAL = 1, then day-proportional seasonal/hourly shares), so an unfiltered
  # `10 * share` also wrote an ANNUAL 10 and four seasonal 2.5 bounds. Those
  # read as a nested cap and are not one: only `mExportRowUp` timeslices are read,
  # and it holds the HOUR level alone, so the 20 coarse rows per region were
  # written and silently ignored.
  sl <- as.data.frame(cal@timeslice_share)
  sl <- sl[sl$timeslice %in% as.character(cal@timeframes[["HOUR"]]), , drop = FALSE]
  EXP_ELC <- newExport("EXP_ELC", desc = "Electricity export to the rest of the world",
    commodity = "ELC", unit = "PJ", reserve = 300,
    export = merge(data.frame(region = regions, price = 5.0),
                data.frame(timeslice = sl$timeslice, exp.up = 10 * sl$share)))

  # ---- weather ----
  wobj <- function(res, desc) newWeather(res, desc = desc, timeframe = "HOUR",
    weather = prof$weather[prof$weather$resource == res, c("region", "timeslice", "wval")])
  WSOL <- wobj("WSOL", "Solar capacity factor by timeslice and region")
  WWIN <- wobj("WWIN", "Wind capacity factor by timeslice and region")
  WHYD <- wobj("WHYD", "Hydro inflow factor by timeslice and region")

  # ---- demand ----
  share <- as.data.frame(cal@timeslice_share)[, c("timeslice", "share")]
  d0 <- merge(prof$demand, share, by = "timeslice"); d0$w <- d0$load * d0$share
  dem_rows <- do.call(rbind, lapply(seq_along(years), function(i) {
    do.call(rbind, lapply(regions, function(r) {
      dr <- d0[d0$region == r, ]
      data.frame(region = r, year = years[i], timeslice = dr$timeslice,
                 demand = annual_demand * demand_growth[i] * dr$w / sum(dr$w))
    }))
  }))
  DEM_ELC <- newDemand("DEM_ELC", desc = "Electricity demand", commodity = "ELC",
    demand = dem_rows)

  # ---- technologies (names are UPPER-CASE set elements; no prefix required) ----
  # Three ways an existing fleet leaves the system, and the choice carries the
  # modelling story:
  #
  #   stk_keep()    a stock declared in the base year and nothing more. It
  #                 survives to its `olife`, which for hydro and nuclear is
  #                 beyond the horizon -- those plants are refurbished, not
  #                 retired, and nothing in the model forces them out.
  #   stk_decline() a MODERATE exogenous path: the ceiling falls to `share` of
  #                 the base-year fleet by the last milestone. Coal ages out
  #                 faster than gas. Combined with `optimizeRetirement = TRUE`
  #                 it is a ceiling, not a schedule: the model closes a plant
  #                 earlier when the economics say so, and the path only stops
  #                 an ageing fleet from standing forever.
  #   nothing       solar and wind carry no legacy stock worth tracking.
  #
  # The fleets follow the geography above: the coal plants stand on the coal,
  # the gas plants on the gas, hydro and nuclear only where they exist.
  # `start`/`end` bound the years new capacity can be built.
  by <- min(years)
  ey <- max(years)
  stk_keep <- function(gw_by_region) {
    gw <- gw_by_region[gw_by_region > 0]
    if (length(gw) == 0) return(data.frame())
    # BOTH ends, at the same value: `stock` is an exogenous series, and a row
    # in the base year alone leaves every later milestone at the parameter's
    # default of zero -- the fleet would simply disappear after 2025.
    data.frame(region = rep(names(gw), 2),
               year   = rep(c(by, ey), each = length(gw)),
               stock  = rep(unname(gw), 2))
  }
  stk_decline <- function(gw_by_region, share) {
    gw <- gw_by_region[gw_by_region > 0]
    if (length(gw) == 0) return(data.frame())
    data.frame(region = rep(names(gw), 2),
               year   = rep(c(by, ey), each = length(gw)),
               stock  = c(unname(gw), unname(gw) * share))
  }
  # a fleet table -> GW per region of THIS layout, `base` where unnamed
  fleet <- function(tbl, base = 0) stats::setNames(
    ifelse(regions %in% names(tbl), tbl[regions], base), regions)
  thermal <- function(nm, comm, desc, eff, inv, fixom, ...) newTechnology(nm,
    desc = desc,
    input = list(comm = comm, combustion = 1), output = list(comm = "ELC"),
    ceff = data.frame(comm = comm, cinp2use = eff), cap2act = 31.536, fixom = fixom,
    invcost = list(invcost = inv), olife = 30L, start = 2010L,
    optimizeRetirement = TRUE, ...)
  ECOA <- thermal("ECOA", "COA", "Coal-fired power plant, subcritical",
    0.40, mg(2000), 55, end = 2030L,                  # no new coal after 2030
    capacity = stk_decline(fleet(.TOPIA_COAL_GW, 1), 0.3))
  EGAS <- thermal("EGAS", "GAS", "Gas-fired combined-cycle plant",
    0.58, mg(900), 25,
    capacity = stk_decline(fleet(.TOPIA_GAS_GW, 1), 0.6))
  ENUC <- if (length(nuc_regs) > 0) newTechnology("ENUC",
    desc = "Nuclear power plant",
    input = list(comm = "NUC"), output = list(comm = "ELC"),
    ceff = data.frame(comm = "NUC", cinp2use = 0.35), af = data.frame(af.lo = 0.7),
    cap2act = 31.536, fixom = 120, invcost = list(invcost = mg(8000)),
    olife = 50L, start = 2030L,                       # licensing/construction lead
    region = nuc_regs,
    capacity = stk_keep(fleet(.TOPIA_NUC_GW)))
  vre <- function(nm, comm, wname, desc, inv, fixom, olife = 25L, start = 2015L, ...)
    newTechnology(nm, desc = desc,
      input = list(comm = comm), output = list(comm = "ELC"),
      ceff = data.frame(comm = comm, cinp2use = 1),
      weather = list(weather = wname, comm = comm, waf.up = 1),
      cap2act = 31.536, fixom = fixom, invcost = list(invcost = inv),
      olife = olife, start = start, ...)
  ESOL <- vre("ESOL", "SOL", "WSOL", "Solar photovoltaic plant", mg(650), 12,
              capacity = stk_decline(fleet(c(), 1), 0))
  EWIN <- vre("EWIN", "WIN", "WWIN", "Onshore wind farm", mg(1300), 35,
              capacity = stk_decline(fleet(c(), 1), 0))
  EHYD <- if (length(hyd_regs) > 0)
    vre("EHYD", "HYD", "WHYD", "Reservoir hydropower plant", mg(3000), 45,
        region = hyd_regs,
        capacity = stk_keep(fleet(.TOPIA_HYD_GW)),
        start = 2010L, end = 2010L, olife = 60L)  # window closed: legacy only
  EBIO <- newTechnology("EBIO", desc = "Biomass power plant (carbon-neutral)",
    input  = list(comm = "BIO", combustion = 1), output = list(comm = "ELC"),
    ceff   = data.frame(comm = "BIO", cinp2use = 0.35),
    cap2act = 31.536, fixom = 60, invcost = list(invcost = mg(2200)),
    olife = 30L, start = 2030L, optimizeRetirement = TRUE)

  # ---- storage (STG_*, 4-hour battery on a declining cost path) --------------
  # UNITS, which the pre-0.92 object got wrong: `cap2act` makes the power
  # capacity mean the same thing as a technology's (GW -> PJ/yr), and
  # `@duration` is reservoir energy per unit of POWER capacity, not hours --
  # `vStorageStgCap = pStorageDuration * vStorageOutCap` with no conversion of
  # its own, so 4 hours of a GW discharger is 4 * 0.0036 PJ. The reservoir's
  # cost belongs on `stg.invcost`, which is measured in energy; `out.invcost`
  # prices the discharger per GW. Put on the wrong slot the battery reads as a
  # free reservoir behind a dear converter.
  STG_ELC <- newStorage("STG_ELC",
    desc = "Lithium-ion battery, four-hour reservoir",
    commodity = "ELC", olife = 20L, cap2act = 31.536,
    duration = 4 * 0.0036,
    invcost = data.frame(year = years,
      stg.invcost = convert("USD/kWh", "MUSD/PJ", c(200, 140, 90, 60)),
      out.invcost = convert("USD/kW",  "MUSD/GW", c(150, 130, 110, 90))),
    seff = data.frame(inpeff = 0.95, outeff = 0.95))

  # ---- trade: electricity at the finest level, fuels at the mid level -------
  # ELC moves region to region along the canonical west-to-east chain; COA and
  # GAS move ZONE to zone, because that is where they are balanced. A corridor
  # inside one zone would be a no-op: both ends would be the same balance.
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
        # `capacityVariable = TRUE` used to be passed here; the slot was removed
        # because a trade's capacity is ALWAYS a decision variable now, so TRUE
        # is simply the behaviour
        invcost = data.frame(region = c(r1, r2), invcost = 350),
        # A wheeling charge on the flow, per direction. Without it a built
        # corridor is free to use and only the 3% `teff` loss discourages
        # trade, so regional prices converge further than they should.
        # `markup` is deliberately unset: it is a transfer between the two
        # regions, not a resource cost, and cancels in the objective.
        varom = data.frame(src = c(r1, r2), dst = c(r2, r1),
                           varom = convert("USD/MWh", "MUSD/PJ", 2)),
        olife = list(olife = 50))
    }
  }
  zchain <- intersect(c("WEST", "CENTRAL", "EAST"), zones)
  for (i in seq_len(max(0, length(zchain) - 1))) {
    z1 <- zchain[i]; z2 <- zchain[i + 1]
    trades[[length(trades) + 1L]] <- newTrade(
      name = paste("TBD_COA", z1, z2, sep = "_"),
      desc = paste("Coal rail corridor", z1, "-", z2),
      commodity = "COA",
      routes = data.frame(src = c(z1, z2), dst = c(z2, z1)),
      trade  = data.frame(src = c(z1, z2), dst = c(z2, z1), teff = 0.99),
      capacity = data.frame(stock = 60),
      invcost = data.frame(region = c(z1, z2), invcost = 20),
      varom = data.frame(src = c(z1, z2), dst = c(z2, z1), varom = 0.3),
      olife = list(olife = 40))
    trades[[length(trades) + 1L]] <- newTrade(
      name = paste("TBD_GAS", z1, z2, sep = "_"),
      desc = paste("Gas pipeline", z1, "-", z2),
      commodity = "GAS",
      routes = data.frame(src = c(z1, z2), dst = c(z2, z1)),
      trade  = data.frame(src = c(z1, z2), dst = c(z2, z1), teff = 0.98),
      capacity = data.frame(stock = 40),
      invcost = data.frame(region = c(z1, z2), invcost = 60),
      varom = data.frame(src = c(z1, z2), dst = c(z2, z1), varom = 0.2),
      olife = list(olife = 40))
  }

  # ---- base repository (order mirrors the TOPIA I vignette) ----
  repo <- newRepository("topia",
    COA, GAS, BIO, NUC, SOL, WIN, HYD, ELC, CO2,
    repo_supply,
    IMP_COA, IMP_GAS, EXP_ELC,
    WSOL, WWIN, WHYD,
    ECOA, EGAS, ESOL, EWIN, EBIO, STG_ELC)
  for (ob in list(ENUC, EHYD)) if (!is.null(ob)) repo <- add(repo, ob)
  for (trd in trades) repo <- add(repo, trd)
  repo <- add(repo, DEM_ELC)

  # the hierarchy this layout is solved on: the shipped nation -> zone -> region
  # table narrowed to its own regions, without geometry (the layouts' polygons
  # live in `topia$geoscales`)
  gscale <- geoscales::filter_geoscale(topia_model_geoscale, geoframe = "region",
                                       region = regions,
                                       drop_empty_geoframes = FALSE)

  # ---- scenario levers (pre-built objects) ----
  # CO2_CAP starts near BASE 2020 emissions (~5,000 kt/region) and tightens to a
  # deep cut. CT_CO2: costs are MUSD, emissions kt -> 20-80 USD/t = 0.02-0.08.
  nreg <- length(regions)
  CO2_CAP <- newConstraint(name = "CO2_CAP", eq = "<=",
    for.each = data.frame(year = years, comm = "CO2"),
    term1 = list(variable = "vEmsFuelTot"),
    rhs = data.frame(year = range(years), rhs = c(5000, 2500) * nreg),
    defVal = Inf)
  CT_CO2 <- newTax(name = "CT_CO2", comm = "CO2",
    tax = data.frame(year = range(years), bal = c(0.02, 0.08)))
  RES_SHARE <- res_share_lever(years, demand_growth, annual_demand, nreg,
    ren = c("ESOL", "EWIN", if (length(hyd_regs) > 0) "EHYD"))
  NO_NEW_NUC <- if (length(nuc_regs) > 0) newConstraint(name = "NO_NEW_NUC",
    eq = "<=",
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
  # NOTE: scope for.each to the milestones AFTER the base year -- including the
  # base-year milestone (even with an unbound Inf ceiling) makes the LP
  # infeasible via rhs interpolation.
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

  # ---- add-on modules: supply curve, resource clusters, vintages -------------
  # Shipped like the levers: NOT in `repo`. A scenario opts in with
  # `add(mod, kit$<MODULE>, overwrite = TRUE)` -- GAS_CURVE / EWIN_SITES /
  # ENUC_VINT REPLACE the flat SUP_GAS / EWIN / ENUC by name -- so the base
  # kits (and their goldens) are untouched.
  #
  # GAS_CURVE: domestic gas as a 3-step marginal-cost curve (cluster variants
  # via asSupplyCurve) around the flat zone price, at the zone level like the
  # supply it replaces.
  GAS_CURVE <- if (length(gas_zones) > 0) {
    asSupplyCurve(
      newSupply("SUP_GAS", desc = "Domestic gas, stepped marginal cost",
                commodity = "GAS", region = gas_zones,
                supply = data.frame(region = gas_zones,
                                    ava.up = unname(.TOPIA_GAS[gas_zones]) *
                                             as.integer(zcount[gas_zones]))),
      price = c(4, 8, 12))
  }
  # EWIN_SITES: wind split into two resource grades -- GOOD sites keep the
  # base capacity factor but are finite (5 GW/region), POOR sites are
  # down-rated to 60% and unlimited. The classic supply-curve-of-sites: the
  # model fills GOOD first, then pays the lower CF.
  wwin_cf <- prof$weather[prof$weather$resource == "WWIN",
                          c("region", "timeslice", "wval")]
  WWIN_GOOD <- newWeather("WWIN_GOOD", desc = "Wind CF, good sites",
                          timeframe = "HOUR", weather = wwin_cf)
  wwin_poor <- wwin_cf; wwin_poor$wval <- 0.6 * wwin_poor$wval
  WWIN_POOR <- newWeather("WWIN_POOR", desc = "Wind CF, poor sites",
                          timeframe = "HOUR", weather = wwin_poor)
  EWIN_CL <- newTechnology("EWIN",
    desc = "Wind power in two site grades (GOOD finite, POOR down-rated)",
    cluster = data.frame(cluster = c("GOOD", "POOR")),
    input = list(comm = "WIN"), output = list(comm = "ELC"),
    ceff = data.frame(comm = "WIN", cinp2use = 1),
    weather = data.frame(cluster = c("GOOD", "POOR"),
                         weather = c("WWIN_GOOD", "WWIN_POOR"),
                         comm = "WIN", waf.up = 1),
    capacity = data.frame(cluster = "GOOD", year = years, cap.up = 5),
    cap2act = 31.536, fixom = 35, invcost = list(invcost = mg(1300)),
    olife = 25L, start = 2015L)
  EWIN_SITES <- newRepository("EWIN_SITES", WWIN_GOOD, WWIN_POOR, EWIN_CL)
  # ENUC_VINT: two build vintages -- the 2040 generation is cheaper, more
  # efficient, and longer-lived. Same name as the base ENUC, same legacy stock,
  # same single nuclear region.
  ENUC_VINT <- if (length(nuc_regs) > 0) newTechnology("ENUC",
    desc = "Nuclear with two build vintages (v2025, v2040)",
    vintage = data.frame(vintage = c("v2025", "v2040"),
                         start = c(2025L, 2040L), end = c(2039L, NA),
                         olife = c(50L, 60L)),
    input = list(comm = "NUC"), output = list(comm = "ELC"),
    ceff = data.frame(vintage = c("v2025", "v2040"), comm = "NUC",
                      cinp2use = c(0.35, 0.40)),
    af = data.frame(af.lo = 0.7),
    cap2act = 31.536, fixom = 120,
    invcost = data.frame(vintage = c("v2025", "v2040"),
                         invcost = mg(c(8000, 5500))),
    region = nuc_regs,
    capacity = stk_keep(fleet(.TOPIA_NUC_GW)))

  list(regions = regions, calendar = calendar, repo = repo, geoscale = gscale,
       repo_comm = repo_comm, repo_supply = repo_supply,
       DEM_ELC = DEM_ELC,
       WSOL = WSOL, WWIN = WWIN, WHYD = WHYD,
       ECOA = ECOA, EGAS = EGAS, ENUC = ENUC, ESOL = ESOL, EWIN = EWIN,
       EHYD = EHYD, EBIO = EBIO, STG_ELC = STG_ELC,
       CO2_CAP = CO2_CAP, CT_CO2 = CT_CO2, RES_SHARE = RES_SHARE,
       NO_NEW_NUC = NO_NEW_NUC, EARLY_RET = EARLY_RET,
       GAS_CURVE = GAS_CURVE, EWIN_SITES = EWIN_SITES, ENUC_VINT = ENUC_VINT)
}

# ---- unit kit ---------------------------------------------------------------
# The "unit model": every input is 1 (efficiencies, costs, lifetimes, cap2act,
# demand per slice) on a perfectly symmetric calendar and a single-year
# horizon with discount = 0, so every solved quantity and the objective are
# small integers a reader verifies in their head. Complexity is opt-in through
# add-on modules, each changing the arithmetic by a hand-computable amount.
#
#   base repo (U1):  DEM 1/slice -> E1 activity 4, capacity 4 (peak 1 over
#                    share 1/4 at cap2act 1), fuel 4x1 -> objective 8.
#   U3: PRM exists only in the first region; the other two import ELC over
#       a unit trade chain:
#                    fuel 12 + E1 cap 12 + trade caps 8 + 4 -> objective 36.
#   SOLAR module:    a SELF-CONTAINED solar + storage model -- free resource
#                    ON in half the slices (step levels = 2), storage bridges
#                    the rest: ESUN cap 8 (2 per ON slice over share 1/4) +
#                    storage out cap 4 (discharging 1 in a share-1/4 slice at
#                    cap2act = 1, the same arithmetic as E1 above)
#                    -> objective 12. Layered onto the base
#                    (add, overwrite = TRUE) it stays idle: 12 > 8 is the
#                    merit-order lesson.
#   SUP_CURVE:       SUP_PRM as a 2-step curve (2 units @1 + 2 @2 -> fuel 6,
#                    objective 10 when it replaces the flat supply).
build_unit <- function(regions = .topia_regions[1], calendar = "unit_s4") {
  stopifnot(is.character(regions), length(regions) > 0)
  cal <- topia_calendars[[calendar]]
  if (is.null(cal)) stop("unknown calendar '", calendar, "'")
  lvl <- cal@default_timeframe
  slices <- as.character(cal@timeframes[[lvl]])

  ELC <- newCommodity("ELC", timeframe = lvl)
  PRM <- newCommodity("PRM", timeframe = "ANNUAL")
  SUN <- newCommodity("SUN", timeframe = lvl)

  # the primary resource is an ENDOWMENT of the first region only: single-
  # region kits are self-sufficient, multi-region kits must trade
  SUP_PRM <- newSupply("SUP_PRM", commodity = "PRM",
    supply = data.frame(region = regions[1], cost = 1))
  E1 <- newTechnology("E1", desc = "Unit converter PRM -> ELC",
    input = list(comm = "PRM"), output = list(comm = "ELC"),
    ceff = data.frame(comm = "PRM", cinp2use = 1),
    cap2act = 1, invcost = list(invcost = 1), olife = 1L,
    region = regions[1])
  DEM_ELC <- newDemand("DEM_ELC", commodity = "ELC",
    demand = expand.grid(region = regions, timeslice = slices, demand = 1,
                         stringsAsFactors = FALSE))

  trades <- list()
  if (length(regions) > 1) {
    for (i in seq_len(length(regions) - 1)) {
      r1 <- regions[i]; r2 <- regions[i + 1]
      trades[[length(trades) + 1L]] <- newTrade(
        name = paste("TBD_ELC", r1, r2, sep = "_"), commodity = "ELC",
        routes = data.frame(src = c(r1, r2), dst = c(r2, r1)),
        trade  = data.frame(src = c(r1, r2), dst = c(r2, r1), teff = 1),
        invcost = data.frame(region = c(r1, r2), invcost = 0.5),  # 1 per corridor
        cap2act = 1, olife = list(olife = 1))
    }
  }

  repo <- newRepository("unit", ELC, PRM, SUP_PRM, E1, DEM_ELC)
  for (trd in trades) repo <- add(repo, trd)

  # -- SOLAR module: free on/off resource (+ storage makes it bridge) --------
  prof <- topia_profile("step", levels = 2, calendar = cal, regions = regions)
  RES_SUN <- newSupply("RES_SUN", commodity = "SUN",
    supply = data.frame(region = regions, cost = 0))
  WSUN <- newWeather("WSUN", timeframe = lvl,
    weather = data.frame(region = prof$region, timeslice = prof$timeslice,
                         wval = prof$value))
  ESUN <- newTechnology("ESUN", desc = "Unit solar: free fuel, ON half the year",
    input = list(comm = "SUN"), output = list(comm = "ELC"),
    ceff = data.frame(comm = "SUN", cinp2use = 1),
    weather = list(weather = "WSUN", comm = "SUN", waf.up = 1),
    cap2act = 1, invcost = list(invcost = 1), olife = 1L)

  STG_ELC <- newStorage("STG_ELC", commodity = "ELC", olife = 1L, cap2act = 1,
    invcost = list(out.invcost = 1), duration = 1,
    seff = data.frame(inpeff = 1, outeff = 1))

  # self-contained: `newModel(data = kit$SOLAR)` is the storage-bridging model
  # on its own (objective 10); added onto the base repo the shared objects
  # replace identically and the solar chain sits idle (8 < 10)
  SOLAR <- newRepository("SOLAR", ELC, SUN, RES_SUN, WSUN, ESUN, STG_ELC,
                         DEM_ELC)

  SUP_CURVE <- asSupplyCurve(
    newSupply("SUP_PRM", desc = "Unit supply as a 2-step curve",
              commodity = "PRM",
              supply = data.frame(region = regions[1], ava.up = 4)),
    price = c(1, 2))

  list(regions = regions, calendar = calendar, repo = repo,
       DEM_ELC = DEM_ELC, E1 = E1, SUP_PRM = SUP_PRM,
       SOLAR = SOLAR, STG_ELC = STG_ELC, SUP_CURVE = SUP_CURVE)
}

# The milestone is the MIDDLE of its period, which is what `newHorizon()` does
# by default: 2025 alone is the base year, then 2026-2035, 2036-2045 and
# 2046-2055 are represented by 2030, 2040 and 2050. The pre-0.92 horizon passed
# `mid_is_end = TRUE`, so each period's costs were discounted from its LAST
# year -- about five extra years of discounting per period, against everything
# built late.
base_horizon <- newHorizon(period = 2025:2055, intervals = c(1, 10, 10, 10),
                           name = "base",
                           desc = "2025-2055, milestones 2025/2030/2040/2050")
unit_horizon <- newHorizon(period = 2025, name = "unit",
                           desc = "single-year horizon (2025) for the unit kits")

topia_modules <- list(
  info = paste("TOPIA teaching-model modules: a kit of energyRt building blocks",
               "(commodity/supply repositories, weather, technologies, storage, a",
               "ready base repository `$repo`), scenario levers (CO2_CAP, CT_CO2,",
               "RES_SHARE, NO_NEW_NUC, EARLY_RET) and add-on modules (GAS_CURVE,",
               "EWIN_SITES, ENUC_VINT), for region layouts under `$electricity`;",
               "all-unit-input models with hand-checkable integer objectives under",
               "`$unit`. Built by the TOPIA I vignette's explicit steps; mirrors",
               "IDEEA::ideea_modules."),
  # the calendars this model uses: the generic ones it borrows, plus its own
  calendars = c(calendars[c("annual", "s4_h24", "m12_h24")],
                list(topia_seasons = topia_seasons)),
  horizons  = list(base = base_horizon, unit = unit_horizon),
  # Keyed `R<n>` for an n-region layout. `R11` covers the whole map; the
  # smaller kits are PREFIXES of the canonical west-to-east order, so `R1` is
  # the westernmost region and `R3` reaches the first CENTRAL one. (The kit
  # KEYS stay `R<n>` -- they name a size, not a region.)
  electricity = list(
    R1  = build_topia(.topia_regions[1],     calendar = "s4_h24"),
    R3  = build_topia(.topia_regions[1:3],   calendar = "s4_h24"),
    R7  = build_topia(.topia_regions[1:7],   calendar = "s4_h24"),
    R11 = build_topia(.topia_regions[1:11],  calendar = "s4_h24")
  ),
  # Unit kits: `U<n>` layouts on the symmetric unit calendar; use with
  # `$horizons$unit` and `discount = 0` so the objective stays an integer.
  unit = list(
    # the unit kits' own calendars live with the kits -- `unit_s4`/`unit_s4h4`
    # are not shipped in `energyRt::calendars`
    calendars = unit_calendars,
    U1 = build_unit(.topia_regions[1],   calendar = "unit_s4"),
    U3 = build_unit(.topia_regions[1:3], calendar = "unit_s4")
  )
)

# sanity: each config has a base repo + the five levers + the add-on modules
for (cfg in names(topia_modules$electricity)) {
  k <- topia_modules$electricity[[cfg]]
  stopifnot(methods::is(k$repo, "repository"),
            all(c("CO2_CAP", "CT_CO2", "RES_SHARE", "NO_NEW_NUC",
                  "EARLY_RET") %in% names(k)),
            methods::is(k$EWIN_SITES, "repository"),
            inherits(k$geoscale, "geoscales::GeoScale"),
            # nuclear sits in C1 alone, so the nuclear objects exist from R3 up
            cfg == "R1" || methods::is(k$ENUC, "technology"),
            cfg == "R1" || methods::is(k$ENUC_VINT, "technology"),
            # every layout spans a zone with a gas endowment (WEST or EAST)
            methods::is(k$GAS_CURVE, "supply"))
}
# `unit` also carries its own `calendars` entry, which is not a kit.
for (cfg in setdiff(names(topia_modules$unit), "calendars")) {
  k <- topia_modules$unit[[cfg]]
  stopifnot(methods::is(k$repo, "repository"),
            methods::is(k$SOLAR, "repository"),
            methods::is(k$STG_ELC, "storage"),
            methods::is(k$SUP_CURVE, "supply"))
}
message("topia_modules configs: ",
        paste(names(topia_modules$electricity), collapse = ", "),
        " | unit: ", paste(names(topia_modules$unit), collapse = ", "))

# (no use_data here: data-raw/topia_assemble.R writes the single dataset)
