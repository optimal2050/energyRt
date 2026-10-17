# =========================================================================== #
# Feature family: storage-inp2out (charger/discharger capacity-ratio link).
#
# eqStorageInp2outLo/Up: vStorageInpCap >=/<= ratio * vStorageOutCap, on
# storages that have both a charger (mStorageInpCap) and a discharger
# (mStorageOutCap) -- the domain mStorageInpOutCap.
#
# Semantics (the reservoir anchor, 2026-10-17):
#   - nothing declared      -> no link: each part sizes to its own flow
#   - one side declared     -> the other side is open (no equation)
#   - declared on a storage without both parts -> interpolation error
# Economics (inp/out.invcost 300, olife 30, discount 0, a free reservoir,
# 10 MWh charged in s1 and discharged in s3, a quarter-year slice): a part
# rated for the flow has capacity k = 10 / (8760 * 0.25) at 300 / 30 per
# unit per year, so each part costs 10 k on top of the 10 of supply.
# =========================================================================== #

si_slices <- paste0("s", 1:4)
si_k <- 10 / (8760 * 0.25)

si_build <- function(inp2out = data.frame(), inp_invcost = 300) {
  invcost <- data.frame(out.invcost = 300, stg.invcost = 0)
  if (!is.null(inp_invcost)) invcost$inp.invcost <- inp_invcost
  newModel("si",
    repo = newRepository("si_repo",
      newCommodity("ELC", timeframe = "SL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(timeslice = si_slices, cost = 1,
                                    ava.up = c(1000, 0, 0, 0))),
      newStorage("STG", commodity = "ELC",
                 olife = data.frame(olife = 30),
                 invcost = invcost, inp2out = inp2out),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = "s3", demand = 10))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = si_slices)),
      name = "si_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

# @covers pStorageInp2out depth=I backends=glpk
test_that("family storage-inp2out: the ratio bound lands in modInp with its maps", {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    si_build(inp2out = data.frame(inp2out.lo = 2)),
    name = "si_i", overwrite = TRUE)))
  expect_equal(unique(ff_bound(scen, "pStorageInp2out", "lo")$value), 2)
  expect_gte(nrow(ff_param(scen, "mStorageInp2outLo")), 1)
  # the completed up = Inf side is OPEN: no up equation may be instantiated
  expect_equal(nrow(ff_param(scen, "mStorageInp2outUp")), 0)
  # a link declared on a storage without a charger is an error
  expect_error(suppressMessages(suppressWarnings(interpolate_model(
    si_build(inp2out = data.frame(inp2out.lo = 2, inp2out.up = 3),
             inp_invcost = NULL),
    name = "si_i2", overwrite = TRUE))), "no charger")
})

# @covers pStorageInp2out vStorageInpCap vStorageOutCap depth=S backends=glpk
test_that("family storage-inp2out: without a declared ratio the parts size on their own", {
  skip_if_no_solver()
  base <- suppressMessages(suppressWarnings(interpolate_model(
    si_build(), name = "si_base", overwrite = TRUE)))
  expect_equal(nrow(ff_param(base, "mStorageInp2outLo")), 0)
  expect_equal(nrow(ff_param(base, "mStorageInp2outUp")), 0)
  base <- .fork_solve(base, solver_options$glpk)
  expect_true(verify_solution(base)$ok)
  expect_true(isTRUE(base@status$solved))
  expect_true(isTRUE(base@status$optimal))
  expect_equal(ff_solution_sum(base, "vStorageInpCap"), si_k, tolerance = 1e-6)
  expect_equal(ff_solution_sum(base, "vStorageOutCap"), si_k, tolerance = 1e-6)
  expect_equal(.fork_objective(base), 10 + 2 * 10 * si_k, tolerance = 1e-6)
})

# @covers eqStorageInp2outLo depth=S backends=glpk
test_that("family storage-inp2out: a lone lo bound oversizes the charger, up side open", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    si_build(inp2out = data.frame(inp2out.lo = 2)),
    name = "si_lo", overwrite = TRUE)))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  # band [2, Inf): cost-minimal charger is exactly 2 x OutCap
  expect_equal(ff_solution_sum(scen, "vStorageInpCap"), 2 * si_k, tolerance = 1e-6)
  expect_equal(ff_solution_sum(scen, "vStorageOutCap"), si_k, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 10 + 3 * 10 * si_k, tolerance = 1e-6)
})

# @covers eqStorageInp2outUp depth=S backends=glpk
test_that("family storage-inp2out: a lone up bound leaves the lo side open", {
  skip_if_no_solver()
  # `.up` alone: InpCap <= 3 OutCap, no floor -- each part still sizes to
  # its flow, which satisfies the bound.
  up <- suppressMessages(suppressWarnings(interpolate_model(
    si_build(inp2out = data.frame(inp2out.up = 3)),
    name = "si_up", overwrite = TRUE)))
  expect_gte(nrow(ff_param(up, "mStorageInp2outUp")), 1)
  expect_equal(nrow(ff_param(up, "mStorageInp2outLo")), 0)
  up <- .fork_solve(up, solver_options$glpk)
  expect_true(verify_solution(up)$ok)
  expect_equal(ff_solution_sum(up, "vStorageInpCap"), si_k, tolerance = 1e-4)
  expect_equal(.fork_objective(up), 10 + 2 * 10 * si_k, tolerance = 1e-4)
})
