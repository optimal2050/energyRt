# =========================================================================== #
# Feature family: weather multipliers (tech waf / wafs / wafc, supply wava).
#
# Wind model: WIND -> ELC (invcost 1000, olife 30, cap2act 1), flat demand
# 10 x 4 slices, weather factor WCF with profile (1, .5, .25, .2).
# Verified convention: act_s <= waf.up * W(s) * share * cap -- the binding
# slice is s4: rate 40 <= 0.2 * cap  ->  cap = 200, obj = 200 * 1000/30
#                                                       = 6666.667.
# All weather multipliers default to 0 (a declared weather link with no
# value shuts the process down) -- the af family's trap default.
# =========================================================================== #

wf_slices <- paste0("s", 1:4)

wf_build <- function(tech_weather, wvals = c(1, 0.5, 0.25, 0.2),
                     af = data.frame()) {
  newModel("wf",
    repo = newRepository("wf_repo",
      newCommodity("ELC", timeframe = "SL"),
      newWeather("WCF", timeframe = "SL",
                 weather = data.frame(timeslice = wf_slices, wval = wvals)),
      newTechnology("WIND", output = list(comm = "ELC"),
                    weather = tech_weather, af = af,
                    invcost = list(invcost = 1000), olife = list(olife = 30),
                    cap2act = 1),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = wf_slices, demand = 10))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = wf_slices)),
      name = "wf_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

# @covers pWeather pTechWeatherAf vTechCap depth=S backends=glpk
# @covers eqTechAfUp depth=S backends=glpk
test_that("family weather: waf.up scales the availability by the weather profile", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    wf_build(data.frame(weather = "WCF", waf.up = 1)),
    name = "wf_up", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pWeather")$value), c(1, 0.5, 0.25, 0.2))
  expect_equal(unique(ff_bound(scen, "pTechWeatherAf", "up")$value), 1)
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  # binding slice s4: rate 40 <= W(s4) * cap = 0.2 * cap -> cap 200
  expect_equal(ff_solution_sum(scen, "vTechCap"), 200, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 200 * 1000 / 30, tolerance = 1e-4)
})

# @covers pTechWeatherAf depth=S backends=glpk forks=fx
test_that("family weather: af.fx + waf.fx force must-run output at the weather profile", {
  skip_if_no_solver()
  # the weather factor MULTIPLIES the af bound; af.lo defaults to 0, so
  # forcing needs an explicit af.fx alongside waf.fx (this is the documented
  # explicit-curtailment setup: forced VRE output + free-disposal surplus)
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    wf_build(data.frame(weather = "WCF", waf.fx = 1),
             af = data.frame(af.fx = 1)),
    name = "wf_fx", overwrite = TRUE)))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  # act_s is FORCED to W(s) * share * cap; cap sized by the binding s4 as
  # before (200), so s1 produces 200*0.25*1 = 50 >> demand 10: free-disposal
  # surplus (curtailment), same capacity and objective
  expect_equal(ff_solution_sum(scen, "vTechCap"), 200, tolerance = 1e-6)
  act <- ff_solution_sum(scen, "vTechAct", by = "timeslice")
  expect_equal(act[order(timeslice)]$value, 200 * 0.25 * c(1, 0.5, 0.25, 0.2),
               tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 200 * 1000 / 30, tolerance = 1e-4)
})

# @covers pTechWeatherAfs pTechWeatherAfc pSupWeather depth=I backends=glpk
test_that("family weather: wafs / wafc / supply wava multipliers land in modInp", {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    newModel("wf2",
      repo = newRepository("r",
        newCommodity("ELC", timeframe = "SL"),
        newWeather("WCF", timeframe = "SL",
                   weather = data.frame(timeslice = wf_slices,
                                        wval = c(1, 0.5, 0.25, 0.2))),
        newSupply("SUP", commodity = "ELC",
                  supply = data.frame(cost = 1, ava.up = 100),
                  weather = data.frame(weather = "WCF", wava.up = 1, wava.lo = 0.1)),
        newTechnology("WIND", output = list(comm = "ELC"),
                      weather = data.frame(weather = c("WCF", "WCF"),
                                           comm = c(NA, "ELC"),
                                           wafs.up = c(0.8, NA),
                                           wafs.lo = c(0.1, NA),
                                           wafc.up = c(NA, 0.9)),
                      invcost = list(invcost = 1000), olife = list(olife = 30),
                      cap2act = 1),
        newDemand("DEM", commodity = "ELC",
                  demand = data.frame(timeslice = wf_slices, demand = 10))),
      calendar = newCalendar(
        timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = wf_slices)),
        name = "wf2_cal"),
      region = "R1", horizon = newHorizon(2025), discount = 0),
    name = "wf2", overwrite = TRUE)))
  expect_equal(unique(ff_bound(scen, "pTechWeatherAfs", "up")$value), 0.8)
  expect_equal(unique(ff_bound(scen, "pTechWeatherAfs", "lo")$value), 0.1)
  expect_equal(unique(ff_bound(scen, "pTechWeatherAfc", "up")$value), 0.9)
  expect_equal(unique(ff_bound(scen, "pSupWeather", "up")$value), 1)
  expect_equal(unique(ff_bound(scen, "pSupWeather", "lo")$value), 0.1)
})
