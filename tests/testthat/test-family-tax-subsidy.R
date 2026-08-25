# =========================================================================== #
# Feature family: tax-subsidy (per-commodity levies / credits).
#
# Mini-model, 1 region, ANNUAL + 4 slices, one year, discount 0, NO storage
# (a lossless free storage plus an output subsidy is an unbounded money pump):
#   SUP ELC at cost 1 (any slice), DEM 10 at s3.
# Template accounting (verified):
#   vTaxCost[c,r,y]  = + sum_s tax.out * weight * vOutTot + tax.inp * vInpTot
#                      + tax.bal * vBalance
#   vSubsCost[c,r,y] = - (same with subsidy rates)   [enters vTotalCost]
# Base flows: OutTot = 10 (supply), InpTot = 10 (demand), so
#   tax out 0.5 -> obj 10 + 5 = 15;  tax inp 0.2 -> obj 12
#   subsidy out 0.3 -> obj 10 - 3 = 7 (safe: subsidy < production cost)
#   tax bal 0.5 with forced surplus 5 (ava.lo 15) -> obj 15 + 2.5 = 17.5
# =========================================================================== #

ts_slices <- paste0("s", 1:4)

ts_build <- function(levy = NULL, ava_lo = NULL) {
  sup <- if (is.null(ava_lo)) {
    newSupply("SUP", commodity = "ELC",
              supply = data.frame(cost = 1))
  } else {
    newSupply("SUP", commodity = "ELC",
              supply = data.frame(timeslice = "s3", cost = 1, ava.lo = ava_lo))
  }
  objs <- list(
    newCommodity("ELC", timeframe = "SL"),
    sup,
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(timeslice = "s3", demand = 10)))
  if (!is.null(levy)) objs <- c(objs, list(levy))
  newModel("ts",
    repo = do.call(newRepository, c(list("ts_repo"), objs)),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = ts_slices)),
      name = "ts_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

ts_solve <- function(mod, tag) {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mod, name = tag, overwrite = TRUE)))
  .fork_solve(scen, solver_options$glpk)
}

# @covers pTaxCostOut vTaxCost eqTaxCost depth=S backends=glpk
test_that("family tax-subsidy: output tax charges every produced unit", {
  skip_if_no_solver()
  scen0 <- suppressMessages(suppressWarnings(interpolate_model(
    ts_build(newTax("TAXE", comm = "ELC", tax = data.frame(out = 0.5))),
    name = "ts_tax_i", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen0, "pTaxCostOut")$value), 0.5)
  scen <- ts_solve(ts_build(newTax("TAXE", comm = "ELC",
                                   tax = data.frame(out = 0.5))), "ts_tax_out")
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vTaxCost"), 5, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 15, tolerance = 1e-6)
})

# @covers pTaxCostInp depth=S backends=glpk
test_that("family tax-subsidy: input tax charges every consumed unit", {
  skip_if_no_solver()
  scen <- ts_solve(ts_build(newTax("TAXE", comm = "ELC",
                                   tax = data.frame(inp = 0.2))), "ts_tax_inp")
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vTaxCost"), 2, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 12, tolerance = 1e-6)
})

# @covers pTaxCostBal depth=S backends=glpk
test_that("family tax-subsidy: balance tax charges the free-disposal surplus", {
  skip_if_no_solver()
  scen <- ts_solve(ts_build(newTax("TAXE", comm = "ELC",
                                   tax = data.frame(bal = 0.5)),
                            ava_lo = 15), "ts_tax_bal")
  expect_true(verify_solution(scen)$ok)
  # forced production 15, demand 10 -> surplus 5 taxed at 0.5
  expect_equal(ff_solution_sum(scen, "vTaxCost"), 2.5, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 17.5, tolerance = 1e-6)
})

# @covers pSubCostOut vSubsCost eqSubsCost depth=S backends=glpk
test_that("family tax-subsidy: output subsidy credits every produced unit", {
  skip_if_no_solver()
  scen0 <- suppressMessages(suppressWarnings(interpolate_model(
    ts_build(newSub("SUBE", comm = "ELC", subsidy = data.frame(out = 0.3))),
    name = "ts_sub_i", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen0, "pSubCostOut")$value), 0.3)
  scen <- ts_solve(ts_build(newSub("SUBE", comm = "ELC",
                                   subsidy = data.frame(out = 0.3))), "ts_sub_out")
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vSubsCost"), -3, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 7, tolerance = 1e-6)
})

# @covers pSubCostInp pSubCostBal depth=I backends=glpk
test_that("family tax-subsidy: inp and bal subsidy rates land in modInp", {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ts_build(newSub("SUBE", comm = "ELC",
                    subsidy = data.frame(inp = 0.1, bal = 0.2))),
    name = "ts_sub_i2", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pSubCostInp")$value), 0.1)
  expect_equal(unique(ff_param(scen, "pSubCostBal")$value), 0.2)
})
