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

# =========================================================================== #
# PARENT-LEVEL weather read by CHILD-level processes.
#
# `pWeather` used to be indexed at the CONSUMING process's region, so a series
# had to be copied once per region that used it -- and a profile declared one
# level up multiplied the availability bound by 0 (pWeather defVal 0, and the
# factor is multiplicative), silently shutting the process down.
#
# The lookup now goes through `mWeatherRegionAt(weather, region, regionp)`:
# `regionp` is the process's own region when the weather serves it, else the
# NEAREST ancestor it does serve. One series at adm1 therefore feeds every adm2
# child, and a flat model resolves through identity rows to the old lookup.
# =========================================================================== #

wx_gs <- function() {
  geoscales::geoscale_from_leaftable(
    data.frame(adm1 = c("A1", "A1", "A2", "A2"),
               region = c("r1", "r7", "r3", "r9")),
    geoframes = c("adm1", "region"), key = "region", name = "wxpc")
}

# `wreg` is where the ONE series is declared: "A1" (the parent) or the children.
wx_model <- function(wreg, nm, tech_regions = c("r1", "r7")) {
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = paste0("wxc_", nm))
  wx <- newWeather("W_ON", region = wreg, timeframe = "SEASON",
                   weather = data.frame(
                     region = rep(wreg, each = 2),
                     timeslice = c("WIN", "SUM"), wval = c(1, 0.5)))
  objs <- list(
    newCommodity("ELC", timeframe = "SEASON"),
    newCommodity("COA", timeframe = "ANNUAL"),
    newSupply("S", commodity = "COA",
              supply = data.frame(region = tech_regions, cost = 0)),
    wx,
    newTechnology("WIND", input = list(comm = "COA"),
                  output = list(comm = "ELC"), region = tech_regions,
                  weather = data.frame(weather = "W_ON", waf.up = 1),
                  invcost = data.frame(invcost = 10),
                  vintage = data.frame(olife = 30L), cap2act = 1),
    newDemand("D", commodity = "ELC",
              demand = data.frame(region = rep(tech_regions, each = 2),
                                  timeslice = c("WIN", "SUM"), demand = 5)))
  setGeoscale(newModel(
    name = nm, calendar = cal, region = c("r1", "r7", "r3", "r9"),
    horizon = newHorizon(2025), discount = 0,
    repo = do.call(newRepository, c(list(paste0("wxr_", nm)), objs))), wx_gs())
}

wx_gd <- function(scen, nm) {
  p <- scen@modInp@parameters[[nm]]
  if (is.null(p)) return(NULL)
  d <- as.data.frame(get_data_slot(p))
  if (is.null(d) || nrow(d) == 0) NULL else d
}

test_that("a parent-level weather resolves to its child regions", {
  skip_if_not_installed("geoscales")
  scen <- suppressMessages(suppressWarnings(
    interpolate_model(wx_model("A1", "wxa"), name = "wxa", overwrite = TRUE)))
  at <- wx_gd(scen, "mWeatherRegionAt")
  expect_setequal(paste(at$weather, at$region, at$regionp),
                  c("W_ON A1 A1", "W_ON r1 A1", "W_ON r7 A1"))
  # exactly one source per (weather, region) -- two would multiply two
  # profiles into a single factor
  expect_false(any(duplicated(at[, c("weather", "region")])))
  # the series is stored ONCE, at the parent
  pw <- wx_gd(scen, "pWeather")
  expect_equal(unique(as.character(pw$region)), "A1")
  expect_equal(nrow(pw), 2L)
})

# @covers mWeatherRegionAt mWeatherRegion pWeather depth=S backends=glpk forks=geoframe
test_that("one series at the parent equals a copy per child", {
  skip_if_not_installed("geoscales")
  skip_if_no_solver()
  obj <- function(wreg, nm) {
    scen <- suppressMessages(suppressWarnings(
      interpolate_model(wx_model(wreg, nm), name = nm, overwrite = TRUE)))
    sol <- suppressMessages(suppressWarnings(
      solve_scenario(scen, solver = solver_options$glpk, wait = TRUE)))
    list(obj = sum(as.data.frame(get_data_slot(
           sol@modOut@variables[["vObjective"]]))$value),
         rows = nrow(wx_gd(scen, "pWeather")))
  }
  parent <- obj("A1", "wxp")
  twin   <- obj(c("r1", "r7"), "wxt")
  expect_gt(parent$obj, 0)
  expect_equal(parent$obj, twin$obj, tolerance = 1e-9)
  # half the data for the same answer
  expect_equal(parent$rows, 2L)
  expect_equal(twin$rows, 4L)
})

test_that("a weather with no series and no ancestor to read is refused", {
  skip_if_not_installed("geoscales")
  # r3 sits under A2; the profile is declared at A1, so that cell cannot resolve
  expect_error(
    suppressMessages(suppressWarnings(interpolate_model(
      wx_model("A1", "wxe", tech_regions = c("r1", "r7", "r3")),
      name = "wxe", overwrite = TRUE))),
    "cannot be resolved")
})

test_that("a flat model resolves through identity rows only", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(
    interpolate_model(wf_build(data.frame(weather = "WCF", waf.up = 1)),
                      name = "wxflat", overwrite = TRUE)))
  at <- wx_gd(scen, "mWeatherRegionAt")
  expect_true(!is.null(at))
  expect_true(all(at$region == at$regionp))
})

test_that("subsetting keeps a parent weather its children still need", {
  skip_if_not_installed("geoscales")
  # Judging a weather object by the sampled regions alone dropped the parent
  # profile, and `.prune_weather_refs()` then stripped the surviving child's
  # link -- leaving WIND with NO availability limit at all rather than an
  # error. Scope it against the sample PLUS its ancestors.
  mod <- wx_model("A1", "wxs")
  kept <- function(reg) {
    sm <- suppressMessages(subset_model_regions(mod, region = reg))
    list(w = names(getObjects(sm, "weather")),
         links = nrow(getObjects(sm, "technology")[["WIND"]]@weather))
  }
  r1 <- kept("r1")
  expect_equal(r1$w, "W_ON")     # A1 is an ancestor of r1
  expect_equal(r1$links, 1L)     # and the link survives with it

  # r3 sits under A2, so the A1 profile is genuinely out of scope there
  m3 <- wx_model("A1", "wxs3")
  s3 <- suppressMessages(subset_model_regions(m3, region = "r3"))
  expect_length(names(getObjects(s3, "weather")), 0L)
})

test_that("aggregation passes a target-level weather through, refuses above", {
  skip_if_not_installed("geoscales")
  # `.agg_map_codes()` keys on the FINEST leaftable column, so an object above
  # the atom layer matched nothing and aggregation died with the misleading
  # "no rows of `x` matched the Geoscale's atoms; check `key=`".
  wx_of <- function(m) getObjects(m, "weather")[["W_ON"]]

  # already AT the target level: nothing to aggregate, pass through
  a1 <- suppressMessages(
    aggregate_model_regions(wx_model("A1", "wxag1"), level = "adm1",
                            name = "wxag1c"))
  expect_equal(wx_of(a1)@region, "A1")
  expect_equal(nrow(wx_of(a1)@weather), 2L)

  # declared at the atoms: mean-aggregated, and lands on the SAME coarse model
  a2 <- suppressMessages(
    aggregate_model_regions(wx_model(c("r1", "r7"), "wxag2"), level = "adm1",
                            name = "wxag2c"))
  expect_equal(unique(as.character(wx_of(a2)@weather$region)), "A1")
  expect_setequal(a1@config@region, a2@config@region)
})

test_that("aggregating between two non-atom levels is refused", {
  skip_if_not_installed("geoscales")
  gs3 <- geoscales::geoscale_from_leaftable(
    data.frame(nation = "N1", zone = c("Z1", "Z1", "Z2", "Z2"),
               region = c("r1", "r2", "r3", "r4")),
    geoframes = c("nation", "zone", "region"), key = "region", name = "wx3")
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))), name = "wx3c")
  mod <- setGeoscale(newModel(
    name = "wx3m", calendar = cal, region = c("r1", "r2", "r3", "r4"),
    horizon = newHorizon(2025), discount = 0,
    repo = newRepository("wx3r",
      newCommodity("ELC", timeframe = "SEASON"),
      newCommodity("COA", timeframe = "ANNUAL"),
      newSupply("S", commodity = "COA",
                supply = data.frame(region = c("r1", "r2"), cost = 0)),
      newWeather("W_ON", region = "Z1", timeframe = "SEASON",
                 weather = data.frame(region = "Z1",
                                      timeslice = c("WIN", "SUM"),
                                      wval = c(1, 0.5))),
      newTechnology("WIND", input = list(comm = "COA"),
                    output = list(comm = "ELC"), region = c("r1", "r2"),
                    weather = data.frame(weather = "W_ON", waf.up = 1),
                    invcost = data.frame(invcost = 10),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newDemand("D", commodity = "ELC",
                demand = data.frame(region = rep(c("r1", "r2"), each = 2),
                                    timeslice = c("WIN", "SUM"), demand = 5)))),
    gs3)
  # zone -> nation: the profile is at neither the atoms nor the target
  expect_error(
    suppressMessages(aggregate_model_regions(mod, level = "nation",
                                             name = "wx3n")),
    "neither atoms of the geoscale nor members of")
  # zone -> zone: it is already at the target, so this is fine
  expect_s4_class(
    suppressMessages(aggregate_model_regions(mod, level = "zone",
                                             name = "wx3z")), "model")
})

test_that("levcost follows a parent-level weather down to the technology", {
  skip_if_not_installed("geoscales")
  # levcost builds a SINGLE-region mini-model at the technology's region and
  # subsets the weather to it. A profile declared at the parent matched no row,
  # so the capacity factor silently vanished and the technology looked cheaper:
  # 0.650514 with the profile at A1 against 0.867352 with a copy per child --
  # the same system, 33% apart, no warning.
  lc <- function(wreg, nm) {
    d <- suppressMessages(suppressWarnings(
      levcost(wx_model(wreg, nm), name = "WIND", region = "r1",
              verbose = FALSE)))
    d$levcost$levcost[1]
  }
  parent <- lc("A1", "wxlc1")
  twin   <- lc(c("r1", "r7"), "wxlc2")
  expect_gt(parent, 0)
  expect_equal(parent, twin, tolerance = 1e-9)
})

test_that("a parent-level weather is reported where it is STORED", {
  skip_if_not_installed("geoscales")
  skip_if_no_solver()
  # Ruling: `pWeather` is an INPUT, so it is reported at the region it is
  # declared at, not at the regions that read it. Results stay at the
  # process's own regions, so the two are easy to tell apart.
  scen <- suppressMessages(suppressWarnings(
    interpolate_model(wx_model("A1", "wxrep"), name = "wxrep",
                      overwrite = TRUE)))
  sol <- suppressMessages(suppressWarnings(
    solve_scenario(scen, solver = solver_options$glpk, wait = TRUE)))
  pw <- suppressMessages(getData(sol, "pWeather", merge = TRUE))
  expect_equal(unique(as.character(pw$region)), "A1")
  cap <- suppressMessages(getData(sol, "vTechCap", merge = TRUE))
  expect_setequal(unique(as.character(cap$region)), c("r1", "r7"))
})
