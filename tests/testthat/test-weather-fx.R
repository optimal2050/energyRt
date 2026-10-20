# =========================================================================== #
# A fixed weather factor fixes the flow.
#
# The weather lower bound is a factor inside its family's lower-bound equation
# (af.lo * cap2act * vTechCap * share * prod(waf.lo * weather) <= vTechAct),
# whose domain holds only rows with a non-zero `.lo` and which is multiplied by
# that `.lo` (default 0). A technology with `waf.fx` and no `af.lo` had no lower
# bound, so `.fx` fixed nothing: the solver curtailed by producing less and the
# surplus never reached vBalance. `.weather_lo_default()` gives such a process
# `.lo = 1` on its domain; a declared `.lo` is kept and multiplies.
# =========================================================================== #

wx_model <- function(name, af_lo = NULL) {
  cal <- newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", HOUR = sprintf("h%02d", 0:3))),
                     name = paste0("cal_", name))
  hrs <- cal@timetable$timeslice
  W <- newWeather("W1", timeframe = "HOUR", region = "R1",
                  weather = data.frame(region = "R1", timeslice = hrs, wval = c(0.2, 0.9, 0.5, 0.1)))
  WIND <- newTechnology("WIND", output = list(comm = "ELC"), region = "R1", cap2act = 1,
                        weather = data.frame(weather = "W1", waf.fx = 1),
                        af = if (is.null(af_lo)) NULL else data.frame(af.lo = af_lo),
                        capacity = data.frame(region = "R1", stock = 100, cap.up = 100))
  GAS <- newTechnology("GAS", output = list(comm = "ELC"), region = "R1", cap2act = 1,
                       capacity = data.frame(region = "R1", stock = 100, cap.up = 100),
                       varom = data.frame(region = "R1", varom = 50))
  DEM <- newDemand("DEM", commodity = "ELC", region = "R1",
                   demand = data.frame(region = "R1", timeslice = hrs, demand = 10))
  newModel(name = name, calendar = cal, region = "R1", horizon = newHorizon(2025), discount = 0,
           repo = newRepository(paste0("repo_", name), newCommodity("ELC", timeframe = "HOUR"), W, WIND, GAS, DEM))
}

# hourly potential: 100 MW x weather x the slice's share (0.25)
WX_POT <- 100 * c(0.2, 0.9, 0.5, 0.1) * 0.25

wx_out <- function(scen) {
  o <- as.data.frame(getData(scen, "vTechOut", merge = TRUE))
  o <- o[o$tech == "WIND", ]
  v <- setNames(o$value, o$timeslice)[sprintf("h%02d", 0:3)]
  v[is.na(v)] <- 0
  unname(v)
}

# @covers mTechWeatherAfLo meqTechAfLo pTechAf depth=I
test_that("a technology with waf.fx gets a lower-bound row in every slice", {
  scen <- suppressMessages(suppressWarnings(interpolate_model(wx_model("wx_map"), name = "wx_map", fx_equ = FALSE, overwrite = TRUE)))
  m <- as.data.frame(.gds(scen, "meqTechAfLo"))
  expect_setequal(m$timeslice[m$tech == "WIND"], sprintf("h%02d", 0:3))
  p <- as.data.frame(.gds(scen, "pTechAf"))
  expect_true(all(p$value[p$tech == "WIND" & p$type == "lo"] == 1))
})

# @covers eqTechAfLo vBalance depth=S backends=glpk
test_that("waf.fx fixes output to the weather; the surplus goes to vBalance", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(wx_model("wx_sol"), name = "wx_sol", overwrite = TRUE)))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_equal(wx_out(scen), WX_POT, tolerance = 1e-6)
  b <- as.data.frame(getData(scen, "vBalance", merge = TRUE))
  # demand 10 per slice: 22.5 - 10 and 12.5 - 10 are spilled
  expect_equal(sum(b$value[b$comm == "ELC"]), 15, tolerance = 1e-6)
})

# @covers eqTechAfLo depth=S backends=glpk
test_that("a declared af.lo is kept and multiplies the weather bound", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(wx_model("wx_lo", af_lo = 0.5), name = "wx_lo", overwrite = TRUE)))
  scen <- .fork_solve(scen, solver_options$glpk)
  out <- wx_out(scen)
  expect_true(all(out >= 0.5 * WX_POT - 1e-6))
  expect_true(all(out <= WX_POT + 1e-6))
})
