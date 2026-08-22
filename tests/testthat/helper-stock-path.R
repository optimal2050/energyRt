# =========================================================================== #
# Fixtures for the exogenous-stock-path suite.
#
# `capacity$stock` names the legacy fleet STILL STANDING at each milestone. The
# old formulation read the same number as the ORIGINAL endowment as well:
#
#     retiredCum(y) <= stock(y)            # stock as the original endowment
#     retStock(y)*plen = cum(y) - cum(y-1) # flow >= 0, so cum never decreases
#
# A schedule declining to zero therefore pinned retiredCum to zero in EVERY
# year, and exogenous phase-out and endogenous early retirement could not both
# be used on one process. `pXStockNew` / `pXStockSurv` split the two readings
# apart, and `eqXStockCap` carries the fleet recursively with no cumulative
# variable.
#
# Three milestones one year apart (pPeriodLen == 1) so a per-year retirement
# RATE and an absolute capacity coincide and the arithmetic below is readable.
# =========================================================================== #

sp_cal <- function() newCalendar(
  timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", HOUR = sprintf("t%02d", 1:4))),
  name = "spc")
sp_hor <- function() newHorizon(2020:2022, intervals = c(1, 1, 1))
SP_YEARS <- c(2020L, 2021L, 2022L)

# Value by milestone, as a named vector; absent (default-valued) years are
# reported as 0 so a schedule that reaches nothing reads as nothing.
sp_by_year <- function(sol, name) {
  d <- getData(sol, name, merge = TRUE)
  out <- stats::setNames(rep(0, length(SP_YEARS)), as.character(SP_YEARS))
  if (is.null(d) || nrow(d) == 0) return(out)
  out[as.character(d$year)] <- round(d$value, 6)
  out
}

# --- technology ------------------------------------------------------------ #
sp_tech <- function(stock, ret = NULL, optret = TRUE, name = "sp") {
  cap <- data.frame(region = "R1", year = SP_YEARS, stock = stock)
  if (!is.null(ret)) cap$ret.fx <- ret
  TECH <- newTechnology(
    "TECH", output = data.frame(comm = "ELC", unit = "GWh"), cap2act = 1,
    capacity = cap, fixom = data.frame(fixom = 1),
    vintage = data.frame(olife = 50L), optimizeRetirement = optret)
  newModel(
    name = name, region = "R1", horizon = sp_hor(), discount = 0,
    calendar = sp_cal(), optimizeRetirement = optret,
    repo = newRepository(
      "spr", newCommodity("ELC", timeframe = "HOUR"), TECH,
      # a costly backstop, so a shrinking fleet is never infeasible
      newSupply("BACK", commodity = "ELC", supply = data.frame(cost = 1000)),
      newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 1))))
}

# --- storage (all three parts carry a stock) -------------------------------- #
sp_storage <- function(stock, ret = NULL, optret = TRUE, name = "sps") {
  cap <- data.frame(region = "R1", year = SP_YEARS, out.stock = stock,
                    inp.stock = stock, stg.stock = stock * 4)
  if (!is.null(ret)) {
    cap$out.ret.fx <- ret; cap$inp.ret.fx <- ret; cap$stg.ret.fx <- ret * 4
  }
  STG <- newStorage(
    "STG", commodity = "ELC", capacity = cap, duration = 4,
    fixom = data.frame(out.fixom = 1), vintage = data.frame(olife = 50L),
    optimizeRetirement = optret)
  newModel(
    name = name, region = "R1", horizon = sp_hor(), discount = 0,
    calendar = sp_cal(), optimizeRetirement = optret,
    repo = newRepository(
      "spr", newCommodity("ELC", timeframe = "HOUR"), STG,
      newSupply("SUP", commodity = "ELC", supply = data.frame(cost = 1)),
      newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 1))))
}

# --- trade (capacity is region-free) ---------------------------------------- #
sp_trade <- function(stock, ret = NULL, optret = TRUE, name = "spt") {
  cap <- data.frame(year = SP_YEARS, stock = stock)
  if (!is.null(ret)) cap$ret.fx <- ret
  TRD <- newTrade(
    "TRD", commodity = "ELC", cap2act = 1, capacity = cap,
    routes = data.frame(src = "R1", dst = "R2"),
    fixom = data.frame(fixom = 1), vintage = data.frame(olife = 50L),
    optimizeRetirement = optret)
  newModel(
    name = name, region = c("R1", "R2"), horizon = sp_hor(), discount = 0,
    calendar = sp_cal(), optimizeRetirement = optret,
    repo = newRepository(
      "spr", newCommodity("ELC", timeframe = "HOUR"), TRD,
      # PRICED by region rather than restricted to one: a region-restricted
      # newSupply breaks eqSupOutTot in the Pyomo and Julia emitters, which has
      # nothing to do with the stock path.
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(region = c("R1", "R2"), cost = c(1, 1000))),
      newDemand("DEM", commodity = "ELC", region = "R2",
                demand = data.frame(demand = 1))))
}

sp_solve <- function(mod) solve_scen(interpolate_model(mod, name = mod@name))
sp_interp <- function(mod) interpolate_model(mod, name = mod@name)
