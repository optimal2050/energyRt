# =========================================================================== #
# fx_equ: a fixed availability bound as one equality row.
#
# A cell carried by both meqTechAfLo and meqTechAfUp whose two sides are the
# same number (af.lo == af.up and the same weather factors) is written as
# eqTechAfFx (vTechAct == ...) instead of the pair eqTechAfLo + eqTechAfUp.
# `fx_equ = FALSE` keeps the pair. The solution must not change.
# =========================================================================== #

fx_model <- function(name) {
  cal <- newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", HOUR = sprintf("h%02d", 0:3))),
                     name = paste0("cal_", name))
  hrs <- cal@timetable$timeslice
  W <- newWeather("W1", timeframe = "HOUR", region = "R1",
                  weather = data.frame(region = "R1", timeslice = hrs, wval = c(0.2, 0.9, 0.5, 0.1)))
  # weather fixed: lo == up, same factor -> Fx
  WIND <- newTechnology("WIND", output = list(comm = "ELC"), region = "R1", cap2act = 1,
                        weather = data.frame(weather = "W1", waf.fx = 1),
                        capacity = data.frame(region = "R1", stock = 100, cap.up = 100))
  # af fixed, no weather -> Fx
  NUC <- newTechnology("NUC", output = list(comm = "ELC"), region = "R1", cap2act = 1,
                       af = data.frame(af.fx = 0.05),
                       capacity = data.frame(region = "R1", stock = 100, cap.up = 100),
                       varom = data.frame(region = "R1", varom = 5))
  # af fixed but the weather differs on the two sides -> stays a Lo/Up pair
  SOL <- newTechnology("SOL", output = list(comm = "ELC"), region = "R1", cap2act = 1,
                       af = data.frame(af.fx = 1),
                       weather = data.frame(weather = "W1", waf.lo = 0.5, waf.up = 1),
                       capacity = data.frame(region = "R1", stock = 20, cap.up = 20))
  # upper bound only -> Up
  GAS <- newTechnology("GAS", output = list(comm = "ELC"), region = "R1", cap2act = 1,
                       capacity = data.frame(region = "R1", stock = 100, cap.up = 100),
                       varom = data.frame(region = "R1", varom = 50))
  DEM <- newDemand("DEM", commodity = "ELC", region = "R1",
                   demand = data.frame(region = "R1", timeslice = hrs, demand = 10))
  newModel(name = name, calendar = cal, region = "R1", horizon = newHorizon(2025), discount = 0,
           repo = newRepository(paste0("repo_", name), newCommodity("ELC", timeframe = "HOUR"),
                                 W, WIND, NUC, SOL, GAS, DEM))
}

fx_interp <- function(name, fx_equ) {
  suppressMessages(suppressWarnings(
    interpolate_model(fx_model(name), name = name, fx_equ = fx_equ, overwrite = TRUE)))
}

fx_techs <- function(scen, map) {
  m <- .gds(scen, map)
  if (is.null(m)) character(0) else sort(unique(as.character(m$tech)))
}

# @covers meqTechAfFx meqTechAfLo meqTechAfUp depth=I
test_that("fx_equ moves the cells with equal sides to meqTechAfFx", {
  on <- fx_interp("fx_on", TRUE)
  expect_equal(fx_techs(on, "meqTechAfFx"), c("NUC", "WIND"))
  expect_equal(nrow(.gds(on, "meqTechAfFx")), 8L)
  expect_equal(fx_techs(on, "meqTechAfLo"), "SOL")
  expect_equal(fx_techs(on, "meqTechAfUp"), c("GAS", "SOL"))

  off <- fx_interp("fx_off", FALSE)
  expect_null(.gds(off, "meqTechAfFx"))
  expect_equal(fx_techs(off, "meqTechAfLo"), c("NUC", "SOL", "WIND"))
  expect_equal(fx_techs(off, "meqTechAfUp"), c("GAS", "NUC", "SOL", "WIND"))
})

# @covers eqTechAfFx depth=S backends=glpk
test_that("fx_equ does not change the solution (GLPK)", {
  skip_if_no_solver()
  on <- .fork_solve(fx_interp("fx_glpk_on", TRUE), solver_options$glpk)
  off <- .fork_solve(fx_interp("fx_glpk_off", FALSE), solver_options$glpk)
  expect_equal(.fork_objective(on), .fork_objective(off), tolerance = 1e-8)
  o <- as.data.frame(getData(on, "vTechOut", merge = TRUE))
  w <- o[o$tech == "WIND", ]
  expect_equal(sum(w$value), sum(100 * c(0.2, 0.9, 0.5, 0.1) * 0.25), tolerance = 1e-6)
})

# @covers eqTechAfFx depth=S backends=julia,pyomo,mm
test_that("fx_equ gives the GLPK objective on JuMP, Pyomo and multimod", {
  skip_if_no_solver()
  ref <- .fork_objective(.fork_solve(fx_interp("fx_ref", FALSE), solver_options$glpk))
  for (b in list(list("julia", skip_if_no_julia_highs, solver_options$julia_highs),
                 list("pyomo", skip_if_no_pyomo, solver_options$pyomo_highs),
                 list("mm", skip_if_no_multimod, solver_options$mm_highs))) {
    ok <- tryCatch({ b[[2]](); TRUE }, skip = function(e) FALSE)
    if (!ok) next
    got <- .fork_objective(.fork_solve(fx_interp(paste0("fx_", b[[1]]), TRUE), b[[3]]))
    expect_equal(got, ref, tolerance = 1e-6, label = b[[1]])
  }
})
