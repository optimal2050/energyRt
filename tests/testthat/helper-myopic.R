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
