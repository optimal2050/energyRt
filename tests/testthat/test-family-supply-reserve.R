# =========================================================================== #
# Feature family: supply-reserve (cumulative resource limits pSupReserveLo/Up).
#
# Mini-model, 1 region, ANNUAL, one year (2025), discount 0:
#   SCHEAP at cost 1, SEXP at cost 5, DEM 50.
#   res.up 30 on SCHEAP -> 30 cheap + 20 expensive: obj = 30 + 100 = 130
#   res.lo 10 on SEXP   -> 40 cheap + 10 forced expensive: obj = 40 + 50 = 90
#   (single-year horizon: cumulative == annual extraction)
# =========================================================================== #

sr_build <- function(cheap_reserve = data.frame(), exp_reserve = data.frame()) {
  newModel("sr",
    repo = newRepository("sr_repo",
      newCommodity("GAS", timeframe = "ANNUAL"),
      newSupply("SCHEAP", commodity = "GAS",
                supply = data.frame(cost = 1), reserve = cheap_reserve),
      newSupply("SEXP", commodity = "GAS",
                supply = data.frame(cost = 5), reserve = exp_reserve),
      newDemand("DEM", commodity = "GAS", demand = data.frame(demand = 50))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
      name = "sr_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

# @covers pSupReserve vSupReserve eqSupReserveUp depth=S backends=glpk
test_that("family supply-reserve: res.up caps the cumulative extraction", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    sr_build(cheap_reserve = data.frame(res.up = 30)),
    name = "sr_up", overwrite = TRUE)))
  expect_equal(unique(ff_bound(scen, "pSupReserve", "up")$value), 30)
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  so <- as.data.frame(ff_solution_sum(scen, "vSupOut", by = "sup"))
  expect_equal(so$value[so$sup == "SCHEAP"], 30, tolerance = 1e-6)
  expect_equal(so$value[so$sup == "SEXP"], 20, tolerance = 1e-6)
  rsv <- as.data.frame(ff_solution_sum(scen, "vSupReserve", by = "sup"))
  expect_equal(rsv$value[rsv$sup == "SCHEAP"], 30, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 130, tolerance = 1e-6)
})

# @covers pSupReserve depth=S backends=glpk forks=default
test_that("family supply-reserve: res.lo forces a minimum cumulative extraction", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    sr_build(exp_reserve = data.frame(res.lo = 10)),
    name = "sr_lo", overwrite = TRUE)))
  expect_equal(unique(ff_bound(scen, "pSupReserve", "lo")$value), 10)
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  so <- as.data.frame(ff_solution_sum(scen, "vSupOut", by = "sup"))
  expect_equal(so$value[so$sup == "SEXP"], 10, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 90, tolerance = 1e-6)
})
