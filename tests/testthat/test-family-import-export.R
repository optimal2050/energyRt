# =========================================================================== #
# Feature family: import-export-row (rest-of-world exchange).
#
# Mini-model with fully analytic economics, single region / ANNUAL / one year:
#   DEM_ELC = 100, only source is IMP_ELC at price 3  -> import cost 300
#   SUP_GAS at cost 2, EXP_GAS at price 5, exp.up 50  -> export margin 3/unit,
#                                                        bound binds: -150
#   expected objective = 100*3 + 50*2 - 50*5 = 150
# Variants: exp.fx (exact export), export reserve (cumulative cap binds).
#
# The run_family() harness adds interp-mode invariance, cross-backend parity,
# and verify_solution() on every solved cell (helper-forks.R).
# =========================================================================== #

ie_build <- function(exp_bounds = data.frame(exp.up = 50),
                     exp_reserve = data.frame(),
                     imp_reserve = data.frame()) {
  cal <- newCalendar(
    timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
    name = "ie_cal")
  newModel("ie",
    repo = newRepository("ie_repo",
      newCommodity("ELC", timeframe = "ANNUAL"),
      newCommodity("GAS", timeframe = "ANNUAL"),
      newDemand("DEM_ELC", commodity = "ELC",
                demand = data.frame(demand = 100)),
      newImport("IMP_ELC", commodity = "ELC",
                import = data.frame(price = 3, imp.up = 200),
                reserve = imp_reserve),
      newSupply("SUP_GAS", commodity = "GAS",
                supply = data.frame(cost = 2)),
      newExport("EXP_GAS", commodity = "GAS",
                export = cbind(data.frame(price = 5), exp_bounds),
                reserve = exp_reserve)),
    calendar = cal, region = "R1", horizon = newHorizon(2025), discount = 0)
}

ie_check_params <- function(scen, case_name) {
  lbl <- function(x) paste0("import-export:", case_name, " ", x)
  pi <- ff_param(scen, "pImportRowPrice")
  expect_false(is.null(pi) || nrow(pi) == 0, label = lbl("pImportRowPrice rows"))
  expect_equal(unique(pi$value), 3, label = lbl("pImportRowPrice value"))
  pe <- ff_param(scen, "pExportRowPrice")
  expect_equal(unique(pe$value), 5, label = lbl("pExportRowPrice value"))
  up <- ff_bound(scen, "pExportRow", "up")
  expect_equal(unique(up$value), 50, label = lbl("pExportRow up-bound value"))
  iu <- ff_bound(scen, "pImportRow", "up")
  expect_equal(unique(iu$value), 200, label = lbl("pImportRow up-bound value"))
}

ie_check_solution <- function(scen, backend, case_name) {
  lbl <- function(x) paste0("import-export:", backend, ":", case_name, " ", x)
  expect_equal(ff_solution_sum(scen, "vImportRow"), 100, tolerance = 1e-6,
               label = lbl("import volume"))
  expect_equal(ff_solution_sum(scen, "vExportRow"), 50, tolerance = 1e-6,
               label = lbl("export volume"))
  expect_equal(ff_solution_sum(scen, "vImportRowCost"), 300, tolerance = 1e-6,
               label = lbl("import cost"))
  expect_equal(ff_solution_sum(scen, "vExportRowCost"), -250, tolerance = 1e-6,
               label = lbl("export revenue"))
  expect_equal(.fork_objective(scen), 150, tolerance = 1e-6,
               label = lbl("objective"))
}

# @covers pImportRow pImportRowPrice pExportRow pExportRowPrice depth=X backends=glpk,julia_highs,pyomo,gams forks=fold,ondisk,sparse,default
# @covers vImportRow vExportRow vImportRowCost vExportRowCost depth=X backends=glpk,julia_highs,pyomo,gams
# @covers eqImportRowCost eqExportRowCost depth=X backends=glpk,julia_highs,pyomo,gams
run_family("import-export",
           build = ie_build,
           check_params = ie_check_params,
           check_solution = ie_check_solution)

# ---- bound-form fork: exp.fx forces the exchange exactly -------------------- #
# @covers pExportRow depth=S backends=glpk forks=fx
test_that("family import-export: exp.fx forces exact export volume", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ie_build(exp_bounds = data.frame(exp.fx = 30)), name = "ie_fx")))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vExportRow"), 30, tolerance = 1e-6)
  # 300 (import) + 30*2 (supply) - 30*5 (revenue) = 210
  expect_equal(.fork_objective(scen), 210, tolerance = 1e-6)
})

# ---- cumulative reserve fork: the horizon total caps the export ------------- #
# @covers pExportRowRes vExportRowCum eqExportRowResUp depth=S backends=glpk
test_that("family import-export: export reserve caps the cumulative volume", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ie_build(exp_bounds = data.frame(exp.up = 1000),
             exp_reserve = data.frame(res.up = 40)),
    name = "ie_res")))
  expect_equal(unique(ff_param(scen, "pExportRowRes")$value), 40)
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vExportRow"), 40, tolerance = 1e-6)
  expect_equal(ff_solution_sum(scen, "vExportRowCum"), 40, tolerance = 1e-6)
  # 300 + 40*2 - 40*5 = 180
  expect_equal(.fork_objective(scen), 180, tolerance = 1e-6)
})

# ---- import reserve reaches the model input (depth I) ----------------------- #
# @covers pImportRowRes depth=I backends=glpk
test_that("family import-export: import reserve lands in modInp", {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ie_build(imp_reserve = data.frame(res.up = 500)), name = "ie_impres")))
  expect_equal(unique(ff_param(scen, "pImportRowRes")$value), 500)
})
