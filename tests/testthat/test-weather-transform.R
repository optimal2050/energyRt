# =========================================================================== #
# Weather transforms: one stream -> many derived series at interpolation.
#
# Fixture: a wind-SPEED stream WSPD (2, 6, 9, 12 m/s over 4 slices) with two
# transforms in misc$transform -- AF50 = speed/10 capped at 1, AF40 =
# wscale * speed/12 capped at 1 (takes the wscale link column). Two techs
# select one each. The EQUIVALENCE anchor: this model interpolates to the
# same modInp as its twin that declares the two derived series as ordinary
# precomputed weather objects under the same names.
# =========================================================================== #

wtf_slices <- paste0("s", 1:4)
wtf_speed <- c(2, 6, 9, 12)
wtf_af50 <- function(weather, ...) pmin(weather / 10, 1)
wtf_af40 <- function(weather, wscale = 1, ...) pmin(wscale * weather / 12, 1)

wtf_stream <- function() {
  w <- newWeather("WSPD", timeframe = "SL",
                  weather = data.frame(timeslice = wtf_slices,
                                       wval = wtf_speed))
  w@misc$transform <- list(AF50 = wtf_af50, AF40 = wtf_af40)
  w
}

wtf_model <- function(name, ...) {
  newModel(name,
    repo = newRepository(paste0(name, "_repo"),
      newCommodity("ELC", timeframe = "SL"),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = wtf_slices, demand = 10)),
      ...),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL",
                                               SL = wtf_slices)),
      name = paste0(name, "_cal")),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

wtf_tech <- function(name, weather) {
  newTechnology(name, output = list(comm = "ELC"), weather = weather,
                invcost = list(invcost = 1000), olife = list(olife = 30),
                cap2act = 1)
}

# transform route: links select AF50 / AF40 (the latter with a wscale arg)
wtf_transform_model <- function() {
  wtf_model("wtA", wtf_stream(),
    wtf_tech("WIND_HI", data.frame(weather = "WSPD", transform = "AF50",
                                   waf.up = 1)),
    wtf_tech("WIND_LO", data.frame(weather = "WSPD", transform = "AF40",
                                   wscale = 0.9, waf.up = 1)))
}

# precomputed twin: the SAME series declared as ordinary weather objects
# under the names materialization synthesizes
wtf_precomputed_model <- function() {
  wtf_model("wtB", wtf_stream(),
    newWeather("WSPD_AF50", timeframe = "SL",
               weather = data.frame(timeslice = wtf_slices,
                                    wval = wtf_af50(wtf_speed))),
    newWeather("WSPD_AF40", timeframe = "SL",
               weather = data.frame(timeslice = wtf_slices,
                                    wval = wtf_af40(wtf_speed, wscale = 0.9))),
    wtf_tech("WIND_HI", data.frame(weather = "WSPD_AF50", waf.up = 1)),
    wtf_tech("WIND_LO", data.frame(weather = "WSPD_AF40", waf.up = 1)))
}

wtf_interp <- function(mod, name) {
  suppressMessages(suppressWarnings(interpolate_model(
    mod, name = name, overwrite = TRUE)))
}

# @covers pWeather pTechWeatherAf depth=I backends=none
test_that("weather transforms: registry round-trip and name checks", {
  fun <- function(weather, ...) weather * 2
  expect_identical(register_weather_transform("wtf_double", fun), fun)
  w <- newWeather("W_T", timeframe = "SL",
                  weather = data.frame(timeslice = wtf_slices,
                                       wval = as.numeric(1:4)))
  expect_equal(materialize_weather(w, "wtf_double")@weather$wval,
               c(2, 4, 6, 8))
  expect_error(register_weather_transform("bad name", fun), "valid")
  expect_error(materialize_weather(w, "wtf_absent"), "Unknown weather")
})

test_that("weather transforms: materialize_weather clones with provenance", {
  w <- wtf_stream()
  d <- materialize_weather(w, "AF40", wscale = 0.9)
  expect_identical(d@name, "WSPD_AF40")
  expect_equal(d@weather$wval, wtf_af40(wtf_speed, wscale = 0.9))
  expect_equal(d@defVal, wtf_af40(w@defVal, wscale = 0.9))
  expect_null(d@misc[["transform"]])
  expect_identical(d@misc$transform_source$weather, "WSPD")
  expect_identical(d@misc$transform_source$args, list(wscale = 0.9))
  # a transform must map the series elementwise
  w@misc$transform$bad <- function(weather, ...) weather[1]
  expect_error(materialize_weather(w, "bad"), "series length")
})

# @covers pWeather pTechWeatherAf depth=I backends=none
test_that("weather transforms: transform route == precomputed twin at modInp", {
  sa <- wtf_interp(wtf_transform_model(), "wt_a")
  sb <- wtf_interp(wtf_precomputed_model(), "wt_b")
  expect_setequal(sa@modInp@sets$weather, sb@modInp@sets$weather)
  key <- function(d) d[order(d$weather, d$timeslice), ]
  expect_equal(key(as.data.frame(ff_param(sa, "pWeather"))),
               key(as.data.frame(ff_param(sb, "pWeather"))),
               ignore_attr = TRUE)
  waf <- function(s) {
    d <- as.data.frame(ff_param(s, "pTechWeatherAf"))
    d[order(d$tech, d$weather), ]
  }
  expect_equal(waf(sa), waf(sb), ignore_attr = TRUE)
})

# @covers pWeather vTechCap depth=S backends=glpk
test_that("weather transforms: both routes solve to the same objective", {
  skip_if_no_solver()
  sa <- .fork_solve(wtf_interp(wtf_transform_model(), "wt_sa"),
                    solver_options$glpk)
  sb <- .fork_solve(wtf_interp(wtf_precomputed_model(), "wt_sb"),
                    solver_options$glpk)
  expect_true(verify_solution(sa)$ok)
  expect_equal(.fork_objective(sa), .fork_objective(sb), tolerance = 1e-9)
})

test_that("weather transforms: one name = one series is enforced", {
  mod <- wtf_model("wtC", wtf_stream(),
    wtf_tech("W1", data.frame(weather = "WSPD", transform = "AF40",
                              wscale = 0.9, waf.up = 1)),
    wtf_tech("W2", data.frame(weather = "WSPD", transform = "AF40",
                              wscale = 0.8, waf.up = 1)))
  expect_error(energyRt:::materialize_weather_transforms(mod),
               "differing arguments")
  # identical args share one clone
  mod2 <- wtf_model("wtD", wtf_stream(),
    wtf_tech("W1", data.frame(weather = "WSPD", transform = "AF40",
                              wscale = 0.9, waf.up = 1)),
    wtf_tech("W2", data.frame(weather = "WSPD", transform = "AF40",
                              wscale = 0.9, waf.up = 1)))
  out <- energyRt:::materialize_weather_transforms(mod2)
  expect_identical(sum(names(getObjects(out)) == "WSPD_AF40"), 1L)
})

test_that("weather transforms: unknown names, objects, collisions are loud", {
  mod <- wtf_model("wtE", wtf_stream(),
    wtf_tech("W1", data.frame(weather = "WSPD", transform = "nope",
                              waf.up = 1)))
  expect_error(energyRt:::materialize_weather_transforms(mod),
               "Unknown weather transform")
  mod <- wtf_model("wtF", wtf_stream(),
    wtf_tech("W1", data.frame(weather = "GONE", transform = "AF50",
                              waf.up = 1)))
  expect_error(energyRt:::materialize_weather_transforms(mod),
               "unknown weather object")
  mod <- wtf_model("wtG", wtf_stream(),
    newWeather("WSPD_AF50", timeframe = "SL",
               weather = data.frame(timeslice = wtf_slices, wval = 1)),
    wtf_tech("W1", data.frame(weather = "WSPD", transform = "AF50",
                              waf.up = 1)))
  expect_error(energyRt:::materialize_weather_transforms(mod), "collides")
})

test_that("weather transforms: no transform is a strict no-op", {
  mod <- wtf_model("wtH", wtf_stream(),
    wtf_tech("W1", data.frame(weather = "WSPD", waf.up = 1)))
  expect_identical(getObjects(energyRt:::materialize_weather_transforms(mod)),
                   getObjects(mod))
})

test_that("weather transforms: aggregation refuses pending transforms", {
  mod <- wtf_transform_model()
  expect_error(energyRt:::.assert_no_weather_transforms(mod),
               "does not commute")
  expect_true(energyRt:::.assert_no_weather_transforms(
    wtf_precomputed_model()))
})
