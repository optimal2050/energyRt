# =========================================================================== #
# Feature family: dummy-debug (slack import/export at finite penalty).
#
# config@debug (data.frame comm/region/year/timeslice/dummyImport/dummyExport)
# prices the balance slack: dummyImport fills an infeasible deficit,
# dummyExport absorbs a surplus that a FX-balance commodity cannot discard.
# Defaults are Inf (slack disabled -> hard infeasibility).
#
# Analytic cases (1 region, ANNUAL + 4 slices, one year, discount 0):
#   deficit: DEM 10 at s3 with NO source, dummyImport 50 -> obj 500
#   surplus: ELC limtype FX, forced production 15 (ava.lo), DEM 10,
#            dummyExport 30 -> 5 slack units, obj 15 + 150 = 165
# =========================================================================== #

dd_slices <- paste0("s", 1:4)

dd_cal <- function() newCalendar(
  timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = dd_slices)),
  name = "dd_cal")

# @covers pDummyImportCost vDummyImport vDummyImportCost eqDummyImportCost depth=S backends=glpk
test_that("family dummy-debug: dummyImport fills an unmeetable demand at its price", {
  skip_if_no_solver()
  mod <- newModel("ddi",
    repo = newRepository("r",
      newCommodity("ELC", timeframe = "SL"),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = "s3", demand = 10))),
    calendar = dd_cal(), region = "R1", horizon = newHorizon(2025),
    discount = 0,
    debug = data.frame(comm = "ELC", dummyImport = 50))
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mod, name = "dd_imp", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pDummyImportCost")$value), 50)
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vDummyImport"), 10, tolerance = 1e-6)
  expect_equal(ff_solution_sum(scen, "vDummyImportCost"), 500, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 500, tolerance = 1e-6)
})

# @covers pDummyExportCost vDummyExport vDummyExportCost eqDummyExportCost depth=S backends=glpk
test_that("family dummy-debug: dummyExport absorbs a FX-balance surplus at its price", {
  skip_if_no_solver()
  mod <- newModel("dde",
    repo = newRepository("r",
      newCommodity("ELC", timeframe = "SL", limtype = "FX"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(timeslice = "s3", cost = 1, ava.lo = 15)),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = "s3", demand = 10))),
    calendar = dd_cal(), region = "R1", horizon = newHorizon(2025),
    discount = 0,
    debug = data.frame(comm = "ELC", dummyExport = 30))
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mod, name = "dd_exp", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pDummyExportCost")$value), 30)
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vDummyExport"), 5, tolerance = 1e-6)
  expect_equal(ff_solution_sum(scen, "vDummyExportCost"), 150, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 165, tolerance = 1e-6)
})
