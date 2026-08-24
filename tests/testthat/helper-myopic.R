# =========================================================================== #
# Fixtures for the sequential/myopic solving suite (R/carry.R,
# R/solve_myopic.R).
#
# Unlike the stock-path fixtures (exogenous fleets), these models BUILD
# capacity: TECH has an investment cost and satisfies a demand that a costly
# backstop otherwise serves, so each solve decides vTechNewCap. One-year
# milestone intervals keep per-year rates and standing amounts identical
# (pPeriodLen = 1), so the arithmetic in assertions stays readable.
# =========================================================================== #

my_cal <- function() newCalendar(
  timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", HOUR = sprintf("t%02d", 1:4))),
  name = "myc")

MY_BACKSTOP_COST <- 1000

# A one-region build-capable model.
#   years:   annual milestones (plen = 1)
#   olife:   TECH lifetime in years (Inf allowed)
#   demand:  per-year demand level (recycled)
#   sup_res: optional cumulative reserve (res.up) on a cheap supply CHP that
#            competes with TECH (for the budget-debit test)
my_mod <- function(years = 2020:2022, olife = 50L, demand = 1,
                   invcost = 10, name = "my", cap_up = NULL,
                   sup_res = NULL) {
  cap <- NULL
  if (!is.null(cap_up)) {
    cap <- data.frame(region = "R1", year = years, cap.up = cap_up)
  }
  TECH <- newTechnology(
    "TECH",
    output = data.frame(comm = "ELC", unit = "GWh"), cap2act = 1,
    invcost = data.frame(invcost = invcost),
    capacity = cap,
    vintage = data.frame(olife = olife),
    optimizeRetirement = FALSE)
  objs <- list(
    newCommodity("ELC", timeframe = "HOUR"),
    TECH,
    newSupply("BACK", commodity = "ELC",
              supply = data.frame(cost = MY_BACKSTOP_COST)),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(year = years,
                                  demand = rep(demand, length(years)))))
  if (!is.null(sup_res)) {
    objs <- c(objs, list(
      newSupply("CHEAP", commodity = "ELC",
                supply = data.frame(cost = 0.1),
                reserve = data.frame(res.up = sup_res))))
  }
  newModel(
    name = name, region = "R1", discount = 0, calendar = my_cal(),
    horizon = newHorizon(min(years):max(years),
                         intervals = rep(1L, length(years))),
    repo = do.call(newRepository, c(list(paste0(name, "r")), objs)))
}

my_scen_path <- function(name) {
  p <- file.path(tempdir(), "myopic-suite", name)
  gsub("[\\/]+", "/", p)
}

# A storage-building model: free SUN only in slices t01-t02, flat ELC demand
# in all four slices, so PV + storage beat the costly backstop and the solve
# decides vStorage*NewCap.
my_stg_mod <- function(years = 2020:2022, olife = 50L, name = "mys") {
  tsl <- sprintf("t%02d", 1:4)
  V <- data.frame(olife = olife)
  newModel(
    name = name, region = "R1", discount = 0, calendar = my_cal(),
    horizon = newHorizon(min(years):max(years),
                         intervals = rep(1L, length(years))),
    repo = newRepository(
      paste0(name, "r"),
      newCommodity("SUN", timeframe = "HOUR"),
      newCommodity("ELC", timeframe = "HOUR"),
      newSupply("SUP_SUN", commodity = "SUN",
                supply = data.frame(timeslice = tsl,
                                    ava.up = c(2, 2, 0, 0), cost = 0)),
      newTechnology("PV", input = list(comm = "SUN"),
                    output = list(comm = "ELC"),
                    invcost = data.frame(invcost = 5), vintage = V,
                    cap2act = 1),
      newStorage("STG", commodity = "ELC", vintage = V,
                 invcost = list(out.invcost = 4, stg.invcost = 1),
                 duration = data.frame(duration.lo = 1, duration.up = 200),
                 af = data.frame(inp.af.up = 1, out.af.up = 1)),
      newSupply("BACK", commodity = "ELC",
                supply = data.frame(cost = MY_BACKSTOP_COST)),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = tsl, demand = 1))))
}

# A trade-building model: cheap supply in R1, demand in R2, a tradable link
# with an investment cost — the solve decides vTradeNewCap (region-free).
my_trd_mod <- function(years = 2020:2022, olife = 50L, name = "myt") {
  V <- data.frame(olife = olife)
  newModel(
    name = name, region = c("R1", "R2"), discount = 0, calendar = my_cal(),
    horizon = newHorizon(min(years):max(years),
                         intervals = rep(1L, length(years))),
    repo = newRepository(
      paste0(name, "r"),
      newCommodity("ELC", timeframe = "HOUR"),
      newTrade("TRD", commodity = "ELC", cap2act = 1,
               routes = data.frame(src = "R1", dst = "R2"),
               invcost = data.frame(invcost = 10),
               # explicit capacity rows put the link into the capacity
               # system (trade capacity is otherwise optional)
               capacity = data.frame(year = years, cap.up = 100),
               vintage = V,
               optimizeRetirement = FALSE),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(region = c("R1", "R2"),
                                    cost = c(1, MY_BACKSTOP_COST))),
      # NOTE the explicit region COLUMN: on the current tree a region-scoped
      # demand (@region = "R2") whose data row has region = NA interpolates
      # to ZERO pDemand rows — a pre-existing bug in the in-flight region
      # work (reproduced at HEAD), reported separately.
      newDemand("DEM", commodity = "ELC", region = "R2",
                demand = data.frame(region = "R2", demand = 1))))
}

# A vintaged technology: the "early" variant can only be built in the first
# year, the "late" one afterwards — so a myopic carry must route the first
# step's build back onto the RIGHT vintage selector.
my_vin_mod <- function(years = 2020:2022, name = "myv") {
  TECH <- newTechnology(
    "TECH",
    output = data.frame(comm = "ELC", unit = "GWh"), cap2act = 1,
    invcost = data.frame(invcost = 10),
    vintage = data.frame(vintage = c("early", "late"),
                         start = c(min(years), min(years) + 1L),
                         end = c(min(years), max(years)),
                         olife = 50L),
    optimizeRetirement = FALSE)
  newModel(
    name = name, region = "R1", discount = 0, calendar = my_cal(),
    horizon = newHorizon(min(years):max(years),
                         intervals = rep(1L, length(years))),
    repo = newRepository(
      paste0(name, "r"),
      newCommodity("ELC", timeframe = "HOUR"),
      TECH,
      newSupply("BACK", commodity = "ELC",
                supply = data.frame(cost = MY_BACKSTOP_COST)),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(demand = 1))))
}

# value of a variable by year, 0-filled over the given years
my_by_year <- function(res_or_scen, name, years) {
  d <- if (inherits(res_or_scen, "myopic")) {
    getData(res_or_scen, name, merge = TRUE)
  } else {
    suppressMessages(getData(res_or_scen, name, merge = TRUE))
  }
  out <- stats::setNames(rep(0, length(years)), as.character(years))
  if (!is.null(d) && nrow(d)) {
    agg <- tapply(d$value, as.character(d$year), sum)
    out[names(agg)] <- round(as.numeric(agg), 6)
  }
  out
}
