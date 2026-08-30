# Guards on name references between objects.
#
# A technology names a weather profile in a string column; the profile is a
# separate object elsewhere in the repository. Nothing reconciled the two.
# `collect_set_elements()` builds the `weather` set from the REFERENCES rather
# than from the declared objects, so a dangling name entered the set, produced
# no `pWeather` rows, and the availability link it carried disappeared -- the
# process ran unconstrained, with no error and no warning.
#
# Measured on the wind model below: capacity 200 -> 40 and objective
# 6,666.7 -> 1,333.3, which is exactly the answer with no availability limit at
# all. Regions and commodities were already guarded; weather was the gap.
#
# `@group` is a different case and is NOT guarded. Groups are collected from the
# `group` column of `@input` / `@output`, so declaring them is optional and a
# guard would reject valid models -- asserted below. What can go wrong is a
# `geff` naming a group no commodity belongs to: the group-efficiency
# constraint then spans no commodities and the problem is INFEASIBLE. Loud, but
# the solver says only "PROBLEM HAS NO PRIMAL FEASIBLE SOLUTION", with nothing
# pointing back at the typo.

dg_slices <- paste0("s", 1:4)

dg_cal <- function(nm) {
  newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SL = dg_slices)), name = nm)
}

# Wind -> ELC with a waf.up link to the WCF profile, which is present or not.
dg_weather_model <- function(with_weather) {
  objs <- list(
    newCommodity("ELC", timeframe = "SL"),
    newTechnology("WIND", output = list(comm = "ELC"),
                  weather = data.frame(weather = "WCF", waf.up = 1),
                  invcost = list(invcost = 1000), olife = list(olife = 30),
                  cap2act = 1),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(timeslice = dg_slices, demand = 10)))
  if (with_weather) {
    objs <- append(objs, list(newWeather("WCF", timeframe = "SL",
      weather = data.frame(timeslice = dg_slices,
                           wval = c(1, 0.5, 0.25, 0.2)))), after = 1L)
  }
  newModel("dgw", repo = do.call(newRepository, c("dgw_repo", objs)),
           calendar = dg_cal("dgw_cal"), region = "R1",
           horizon = newHorizon(2025), discount = 0)
}

test_that("a weather reference with no object behind it is refused", {
  expect_error(
    suppressMessages(suppressWarnings(interpolate_model(
      dg_weather_model(FALSE), name = "dgw_miss", overwrite = TRUE))),
    "weather object\\(s\\) that are not in the repository.*WCF.*WIND"
  )
})

# @covers pWeather depth=S
test_that("the guard does not disturb a model whose weather is declared", {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    dg_weather_model(TRUE), name = "dgw_ok", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pWeather")$value), c(1, 0.5, 0.25, 0.2))
})

test_that("get_weather reads the reference from any object that carries one", {
  tech <- newTechnology("T", output = list(comm = "ELC"),
                        weather = data.frame(weather = c("W1", "W2"),
                                             waf.up = 1))
  expect_setequal(get_weather(tech), c("W1", "W2"))
  # A weather object refers to no other; its own `weather` slot is the profile.
  expect_length(get_weather(newWeather("W1", timeframe = "SL",
    weather = data.frame(timeslice = dg_slices, wval = 1))), 0L)
  expect_length(get_weather(newCommodity("ELC")), 0L)
})

# Co-firing: COAL and BIOM substitute within one input group, priced apart so
# the solution is determinate.
dg_group_model <- function(declare_group, geff_group = "FUEL") {
  grp <- if (declare_group) {
    data.frame(group = "FUEL", desc = "fuel", unit = "GWh",
               stringsAsFactors = FALSE)
  } else {
    data.frame()
  }
  newModel("dgg", repo = newRepository("dgg_repo",
    newCommodity("ELC", timeframe = "SL"),
    newCommodity("COAL", timeframe = "ANNUAL"),
    newCommodity("BIOM", timeframe = "ANNUAL"),
    newSupply("SCOAL", commodity = "COAL",
              supply = data.frame(ava.up = 1e4, cost = 2)),
    newSupply("SBIOM", commodity = "BIOM",
              supply = data.frame(ava.up = 1e4, cost = 5)),
    newTechnology("PP",
      input = data.frame(comm = c("COAL", "BIOM"), group = "FUEL",
                         stringsAsFactors = FALSE),
      output = list(comm = "ELC"), group = grp,
      geff = data.frame(group = geff_group, ginp2use = 2.5,
                        stringsAsFactors = FALSE),
      invcost = list(invcost = 100), olife = list(olife = 30), cap2act = 1),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(timeslice = dg_slices, demand = 10))),
    calendar = dg_cal("dgg_cal"), region = "R1",
    horizon = newHorizon(2025), discount = 0)
}

# @covers group pTechGinp2use mTechInpGroup depth=S backends=glpk
test_that("declaring @group is optional and changes nothing", {
  skip_if_no_solver()
  obj <- vapply(c(TRUE, FALSE), function(dec) {
    scen <- suppressMessages(suppressWarnings(interpolate_model(
      dg_group_model(dec), name = paste0("dgg_", dec), overwrite = TRUE)))
    expect_equal(scen@modInp@sets$group, "FUEL")
    .fork_objective(.fork_solve(scen, solver_options$glpk))
  }, 0)
  expect_equal(obj[[1]], obj[[2]], tolerance = 1e-8)
})

test_that("a geff naming a group with no members is refused", {
  expect_error(
    suppressMessages(suppressWarnings(interpolate_model(
      dg_group_model(TRUE, geff_group = "TYPO"), name = "dgg_typo",
      overwrite = TRUE))),
    "no commodity belongs to.*PP: TYPO"
  )
})
