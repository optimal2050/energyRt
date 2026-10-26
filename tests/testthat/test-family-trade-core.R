# =========================================================================== #
# Feature family: trade-core (route flow bounds, relative af, cap2act) and
# trade aux flows (aeff csrc/cdst ratios).
#
# Uses the shared tr_model() fixture (helper-trade.R): ELC priced R1 = 1,
# R2 = 100, demand 10 in R2, corridor capacity pinned with cap.fx, one year,
# discount 0. Full trade -> obj 10; no trade -> obj 1000; every unit denied
# to the corridor costs 99 extra.
# =========================================================================== #

# @covers pTradeIr vTradeIr depth=S backends=glpk
test_that("family trade-core: an absolute route bound (ava.up) caps the flow", {
  skip_if_no_solver()
  routes <- data.frame(src = "R1", dst = "R2")
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    tr_model(routes, trade = data.frame(src = "R1", dst = "R2", ava.up = 4)),
    name = "tc_ava", overwrite = TRUE)))
  expect_equal(unique(ff_bound(scen, "pTradeIr", "up")$value), 4)
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vTradeIr"), 4, tolerance = 1e-6)
  # 4 cheap imported + 6 local at 100
  expect_equal(.fork_objective(scen), 4 + 600, tolerance = 1e-6)
})

# @covers pTradeIrAf depth=S backends=glpk
test_that("family trade-core: a relative bound (af.up) caps the flow at a capacity fraction", {
  skip_if_no_solver()
  routes <- data.frame(src = "R1", dst = "R2")
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    tr_model(routes, cap = 100,
             trade = data.frame(src = "R1", dst = "R2", af.up = 0.05)),
    name = "tc_af", overwrite = TRUE)))
  expect_equal(unique(ff_bound(scen, "pTradeIrAf", "up")$value), 0.05)
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  # flow <= 100 * 1 * 0.05 = 5
  expect_equal(ff_solution_sum(scen, "vTradeIr"), 5, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 5 + 500, tolerance = 1e-6)
})

# @covers pTradeCap2Act depth=S backends=glpk
test_that("family trade-core: cap2act scales the capacity into a flow budget", {
  skip_if_no_solver()
  routes <- data.frame(src = "R1", dst = "R2")
  TRD <- newTrade("TRD", commodity = "ELC", cap2act = 0.5,
                  routes = routes,
                  capacity = data.frame(cap.fx = 10),
                  vintage = data.frame(olife = 50L),
                  trade = data.frame(src = "R1", dst = "R2", af.up = 1))
  mod <- newModel("tc2", region = c("R1", "R2"), horizon = tr_hor(),
    discount = 0, calendar = tr_cal(),
    repo = newRepository("tr_repo",
      newCommodity("ELC", timeframe = "ANNUAL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(region = c("R1", "R2"), cost = c(1, 100))),
      TRD,
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(region = "R2", demand = 10))))
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mod, name = "tc_c2a", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pTradeCap2Act")$value), 0.5)
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  # flow <= cap 10 * cap2act 0.5 * af 1 = 5
  expect_equal(ff_solution_sum(scen, "vTradeIr"), 5, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 5 + 500, tolerance = 1e-6)
})

# @covers pTradeIrCsrc2Ainp pTradeIrCdst2Ainp vTradeIrAInp depth=S backends=glpk
test_that("family trade-core: endpoint aux inputs are drawn per traded unit", {
  skip_if_no_solver()
  routes <- data.frame(src = "R1", dst = "R2")
  mk_aux <- function(aeff) {
    TRD <- newTrade("TRD", commodity = "ELC", cap2act = 1,
                    routes = routes,
                    capacity = data.frame(cap.fx = 100),
                    vintage = data.frame(olife = 50L),
                    aux = data.frame(acomm = "WAT"),
                    aeff = aeff)
    newModel("tc3", region = c("R1", "R2"), horizon = tr_hor(),
      discount = 0, calendar = tr_cal(),
      repo = newRepository("tr_repo",
        newCommodity("ELC", timeframe = "ANNUAL"),
        newCommodity("WAT", timeframe = "ANNUAL"),
        newSupply("SUP", commodity = "ELC",
                  supply = data.frame(region = c("R1", "R2"), cost = c(1, 100))),
        newSupply("SWAT", commodity = "WAT", supply = data.frame(cost = 2)),
        TRD,
        newDemand("DEM", commodity = "ELC",
                  demand = data.frame(region = "R2", demand = 10))))
  }
  # src-side aux: 0.1 WAT per unit leaving R1 -> 1 WAT at cost 2
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mk_aux(data.frame(acomm = "WAT", csrc2ainp = 0.1)),
    name = "tc_srcaux", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pTradeIrCsrc2Ainp")$value), 0.1)
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vTradeIrAInp"), 1, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 10 + 2, tolerance = 1e-6)
  # dst-side aux: 0.2 WAT per unit arriving in R2 -> 2 WAT at cost 2
  scen2 <- suppressMessages(suppressWarnings(interpolate_model(
    mk_aux(data.frame(acomm = "WAT", cdst2ainp = 0.2)),
    name = "tc_dstaux", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen2, "pTradeIrCdst2Ainp")$value), 0.2)
  scen2 <- .fork_solve(scen2, solver_options$glpk)
  expect_true(verify_solution(scen2)$ok)
  expect_equal(ff_solution_sum(scen2, "vTradeIrAInp"), 2, tolerance = 1e-6)
  expect_equal(.fork_objective(scen2), 10 + 4, tolerance = 1e-6)
})

# @covers pTradeIrCsrc2Aout pTradeIrCdst2Aout depth=I backends=glpk
test_that("family trade-core: endpoint aux-output ratios land in modInp", {
  routes <- data.frame(src = "R1", dst = "R2")
  TRD <- newTrade("TRD", commodity = "ELC", cap2act = 1,
                  routes = routes, capacity = data.frame(cap.fx = 100),
                  vintage = data.frame(olife = 50L),
                  aux = data.frame(acomm = "HET"),
                  aeff = data.frame(acomm = "HET", csrc2aout = 0.03,
                                    cdst2aout = 0.04))
  mod <- newModel("tc4", region = c("R1", "R2"), horizon = tr_hor(),
    discount = 0, calendar = tr_cal(),
    repo = newRepository("tr_repo",
      newCommodity("ELC", timeframe = "ANNUAL"),
      newCommodity("HET", timeframe = "ANNUAL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(region = c("R1", "R2"), cost = c(1, 100))),
      TRD,
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(region = "R2", demand = 10))))
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mod, name = "tc_auxout", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pTradeIrCsrc2Aout")$value), 0.03)
  expect_equal(unique(ff_param(scen, "pTradeIrCdst2Aout")$value), 0.04)
})

# A route bound over a parent timeslice: four slices, demand 10 each in R2,
# capacity 40 a year (10 a slice) -- unbounded, all 40 come from R1 at 1.
tr_afs_model <- function(trade = data.frame()) {
  sl <- paste0("s", 1:4)
  newModel("tafs", region = c("R1", "R2"), horizon = tr_hor(), discount = 0,
    calendar = newCalendar(timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", SL = sl)), name = "tafs_cal"),
    repo = newRepository("tafs_repo",
      newCommodity("ELC", timeframe = "SL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(region = c("R1", "R2"), cost = c(1, 100))),
      newTrade("TRD", commodity = "ELC", cap2act = 1,
               routes = data.frame(src = "R1", dst = "R2"),
               capacity = data.frame(cap.fx = 40),
               vintage = data.frame(olife = 50L), trade = trade),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(region = "R2", timeslice = sl, demand = 10))))
}

# @covers pTradeIrAfs meqTradeIrAfsUp meqTradeIrAfsFx depth=S backends=glpk
test_that("family trade-core: afs bounds a route's flow over a parent timeslice", {
  skip_if_no_solver()
  free <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    tr_afs_model(), name = "tafs0", overwrite = TRUE))), solver_options$glpk)
  expect_equal(.fork_objective(free), 40, tolerance = 1e-6)
  # half the year's capacity: 20 imported, 20 local at 100
  up <- suppressMessages(suppressWarnings(interpolate_model(
    tr_afs_model(data.frame(src = "R1", dst = "R2", timeslice = "ANNUAL",
                            afs.up = 0.5)), name = "tafs1", overwrite = TRUE)))
  expect_gt(NROW(get_data_slot(up@modInp@parameters$meqTradeIrAfsUp)), 0)
  up <- .fork_solve(up, solver_options$glpk)
  expect_true(verify_solution(up)$ok)
  expect_equal(ff_solution_sum(up, "vTradeIr"), 20, tolerance = 1e-6)
  expect_equal(.fork_objective(up), 20 + 2000, tolerance = 1e-6)
  # fixed: the equality map, the same flow
  fx <- suppressMessages(suppressWarnings(interpolate_model(
    tr_afs_model(data.frame(src = "R1", dst = "R2", timeslice = "ANNUAL",
                            afs.fx = 0.25)), name = "tafs2", overwrite = TRUE)))
  expect_gt(NROW(get_data_slot(fx@modInp@parameters$meqTradeIrAfsFx)), 0)
  fx <- .fork_solve(fx, solver_options$glpk)
  expect_equal(ff_solution_sum(fx, "vTradeIr"), 10, tolerance = 1e-6)
})

# @covers pTradeWeatherAf mTradeWeatherAfUp depth=S backends=glpk
test_that("family trade-core: a weather factor rates a route by slice", {
  skip_if_no_solver()
  sl <- paste0("s", 1:4)
  w <- newWeather("WLINE", timeframe = "SL", weather = data.frame(region = "R1", timeslice = sl,
                                                wval = c(1, 1, 0.5, 0.5)))
  mod <- add(tr_afs_model(), newRepository("tw_repo", w))
  trd <- mod@data[["tafs_repo"]]@data[["TRD"]]
  trd@weather <- data.frame(weather = "WLINE", src = "R1", dst = "R2", waf.up = 1)
  mod@data[["tafs_repo"]]@data[["TRD"]] <- trd
  scen <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    mod, name = "tw1", overwrite = TRUE))), solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  # 10 a slice where the series is 1, 5 where it is 0.5; the rest local at 100
  expect_equal(ff_solution_sum(scen, "vTradeIr"), 30, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 30 + 1000, tolerance = 1e-6)
})

test_that("family trade-core: wafs needs afs on the same side", {
  sl <- paste0("s", 1:4)
  w <- newWeather("WLINE", timeframe = "SL", weather = data.frame(region = "R1", timeslice = sl, wval = 1))
  mod <- add(tr_afs_model(), newRepository("tw_repo", w))
  trd <- mod@data[["tafs_repo"]]@data[["TRD"]]
  trd@weather <- data.frame(weather = "WLINE", src = "R1", dst = "R2", wafs.up = 1)
  mod@data[["tafs_repo"]]@data[["TRD"]] <- trd
  expect_error(suppressMessages(suppressWarnings(interpolate_model(
    mod, name = "tw2", overwrite = TRUE))), "without afs.up")
})
