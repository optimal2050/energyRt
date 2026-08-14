# =========================================================================== #
# Helpers for the discount-rate / EAC / payback tests.
#
# `rt_model()` is a one-region electricity model whose only investable plant is
# ECOA, so pTechEac and vTechEac are unambiguous. `capacity` forbids new capacity
# after the first milestone where asked, which pins every unit to the 2020
# vintage and makes the EAC charging window directly readable off vTechEac.
# =========================================================================== #

# Capital-recovery factor, written out independently of the package so the tests
# check the formula rather than restate it.
rt_crf <- function(d, n) {
  if (d == 0) return(1 / n)
  d * (1 + d)^n / ((1 + d)^n - 1)
}

rt_model <- function(invcost = data.frame(invcost = 1000), olife = 40L,
                     build_first_only = FALSE, name = "rt", ...) {
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SPR", "SUM", "AUT"))),
    name = paste0("cal_", name))
  cap <- if (build_first_only) {
    data.frame(year = c(2025L, 2035L, 2045L, 2051L), ncap.up = 0)
  } else {
    data.frame()
  }
  newModel(
    name = name, desc = "", calendar = cal, region = "R1",
    horizon = newHorizon(2020:2059, intervals = c(1, rep(10, 4))), ...,
    repo = newRepository(
      paste0("repo_", name),
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SEASON"),
      newSupply("SUP_COA", commodity = "COA",
                supply = data.frame(region = "R1", cost = 1)),
      newDemand("DEM_ELC", commodity = "ELC",
                demand = data.frame(region = "R1", timeslice = c("WIN", "SUM"),
                                 demand = 30)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"), invcost = invcost,
                    capacity = cap,
                    vintage = data.frame(olife = olife), cap2act = 1)
    ))
}

rt_interp <- function(mod, name = "rt") {
  suppressMessages(suppressWarnings(
    interpolate_model(mod, name = name, fold = TRUE)))
}

rt_solve <- function(scen) {
  suppressMessages(suppressWarnings(
    solve_scen(scen, solver = solver_options$glpk, wait = TRUE)))
}

# A parameter's values as a plain vector, or by year.
rt_val <- function(scen, par) {
  d <- suppressMessages(suppressWarnings(getData(scen, par, merge = TRUE)))
  if (is.null(d) || nrow(d) == 0) return(numeric())
  unique(round(d$value, 6))
}

rt_by_year <- function(scen, par) {
  d <- suppressMessages(suppressWarnings(getData(scen, par, merge = TRUE)))
  if (is.null(d) || nrow(d) == 0) return(numeric())
  stats::setNames(round(d$value, 4), as.character(d$year))
}
