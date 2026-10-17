# =========================================================================== #
# wafs and wafc: weather factors on the summed and the commodity availability.
#
# `wafs` multiplies `afs`: without `afs` it is an error, and the weather must
# have a value at every timeslice `afs` is set on (ANNUAL -> one value a year,
# DAY -> one a day). `wafc` acts like `waf` at the commodity's timeframe: with
# no `afc` of its own on a side the commodity gets `afc = 1` there, and the
# weather must have a value at each of the commodity's timeslices.
# =========================================================================== #

ws_cal <- function(name) {
  newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", DAY = c("d1", "d2"),
                                                       HOUR = sprintf("h%02d", 0:1))),
              name = paste0("cal_", name))
}

ws_model <- function(name, tech, weather) {
  cal <- ws_cal(name)
  hrs <- cal@timetable$timeslice
  newModel(name = name, calendar = cal, region = "R1", horizon = newHorizon(2025), discount = 0,
           repo = newRepository(paste0("repo_", name), newCommodity("ELC", timeframe = "HOUR"), weather, tech,
             newTechnology("GAS", output = list(comm = "ELC"), region = "R1", cap2act = 1,
                           capacity = data.frame(region = "R1", stock = 100, cap.up = 100),
                           varom = data.frame(region = "R1", varom = 50)),
             newDemand("DEM", commodity = "ELC", region = "R1",
                       demand = data.frame(region = "R1", timeslice = hrs, demand = 10))))
}

W_HOUR <- newWeather("WH", timeframe = "HOUR", region = "R1",
                     weather = data.frame(region = "R1", timeslice = c("d1_h00", "d1_h01", "d2_h00", "d2_h01"),
                                          wval = c(0.2, 0.9, 0.5, 0.1)))
W_DAY <- newWeather("WD", timeframe = "DAY", region = "R1",
                    weather = data.frame(region = "R1", timeslice = c("d1", "d2"), wval = c(0.5, 0.3)))

ws_tech <- function(weather, afs = NULL, wafs = NULL, wafc = NULL) {
  w <- data.frame(weather = weather)
  if (!is.null(wafs)) w[[names(wafs)]] <- wafs[[1]]
  if (!is.null(wafc)) { w$comm <- "ELC"; w[[names(wafc)]] <- wafc[[1]] }
  newTechnology("WS", output = list(comm = "ELC"), region = "R1", cap2act = 1,
                afs = afs, weather = w,
                capacity = data.frame(region = "R1", stock = 100, cap.up = 100))
}

ws_interp <- function(name, tech, weather) {
  suppressMessages(suppressWarnings(interpolate_model(ws_model(name, tech, weather), name = name, overwrite = TRUE)))
}

ws_out <- function(scen) {
  o <- as.data.frame(getData(scen, "vTechOut", merge = TRUE))
  o <- o[o$tech == "WS", ]
  v <- setNames(o$value, o$timeslice)[c("d1_h00", "d1_h01", "d2_h00", "d2_h01")]
  v[is.na(v)] <- 0
  unname(v)
}

# @covers mTechWeatherAfsUp meqTechAfsUp depth=I
test_that("wafs without afs is an error", {
  expect_error(ws_interp("ws_noafs", ws_tech("WD", wafs = list(wafs.up = 1)), W_DAY),
               "wafs.up is set without afs.up")
})

# @covers mTechWeatherAfsUp meqTechAfsUp depth=I
test_that("wafs whose weather is not at the afs timeframe is an error", {
  afs <- data.frame(timeslice = c("d1", "d2"), afs.up = 1)
  expect_error(ws_interp("ws_tf", ws_tech("WH", afs = afs, wafs = list(wafs.up = 1)), W_HOUR),
               "no value at the timeslices")
})

# @covers eqTechAfsLo eqTechAfsUp depth=S backends=glpk
test_that("wafs.fx with afs.fx fixes the daily totals to the daily weather", {
  skip_if_no_solver()
  afs <- data.frame(timeslice = c("d1", "d2"), afs.fx = 1)
  scen <- .fork_solve(ws_interp("ws_day", ws_tech("WD", afs = afs, wafs = list(wafs.fx = 1)), W_DAY),
                      solver_options$glpk)
  out <- ws_out(scen)
  # 100 MW x weather x the day's share (0.5)
  expect_equal(c(sum(out[1:2]), sum(out[3:4])), c(25, 15), tolerance = 1e-6)
})

# @covers meqTechAfcOutLo meqTechAfcOutUp pTechAfc depth=I
test_that("wafc without afc gets afc = 1 on the sides it sets", {
  scen <- ws_interp("ws_afcmap", ws_tech("WH", wafc = list(wafc.fx = 1)), W_HOUR)
  p <- .gds(scen, "pTechAfc")
  expect_setequal(as.character(p$type[p$tech == "WS"]), c("lo", "up"))
  expect_true(all(p$value[p$tech == "WS"] == 1))
  # lo == up: with fx_equ (default) the four cells are one equality row each
  expect_equal(nrow(.gds(scen, "meqTechAfcOutFx")), 4L)
  expect_null(.gds(scen, "meqTechAfcOutLo"))
  expect_null(.gds(scen, "meqTechAfcOutUp"))
})

# @covers eqTechAfcOutLo eqTechAfcOutUp depth=S backends=glpk
test_that("wafc.fx alone fixes the commodity flow to the weather", {
  skip_if_no_solver()
  scen <- .fork_solve(ws_interp("ws_afc", ws_tech("WH", wafc = list(wafc.fx = 1)), W_HOUR),
                      solver_options$glpk)
  # 100 MW x weather x the hour's share (0.25)
  expect_equal(ws_out(scen), 100 * c(0.2, 0.9, 0.5, 0.1) * 0.25, tolerance = 1e-6)
})

# @covers mTechWeatherAfcUp depth=I
test_that("wafc whose weather is not at the commodity timeframe is an error", {
  expect_error(ws_interp("ws_afctf", ws_tech("WD", wafc = list(wafc.fx = 1)), W_DAY),
               "no value at the timeslices")
})
