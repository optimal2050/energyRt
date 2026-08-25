# =========================================================================== #
# Feature family: storage-inp2out (charger/discharger capacity-ratio link).
#
# eqStorageInp2outLo/Up: vStorageInpCap >=/<= ratio * vStorageOutCap, for
# storages whose charging part has its own capacity variable (mStorageInpCap).
#
# Semantics (per .norm_ratio() in class-storage.R and the structural-map fix
# of 2026-08-25, which also covers the identical `duration` hazard):
#   - nothing declared      -> binding default [1, 1]: InpCap == OutCap (the
#                              single bidirectional inverter; keeps a priced
#                              charger from silently costing nothing)
#   - one side declared     -> the OTHER side is OPEN (no equation), not the
#                              default. Previously the completed up = Inf row
#                              was dropped at write and the binding default 1
#                              resurrected: `inp2out.lo = 2` alone was
#                              INFEASIBLE (lo 2 vs up 1).
#   - `inp2out.up` alone    -> lo is open: a priced charger MAY size to zero
#                              (its invcost then costs nothing) -- deliberate,
#                              documented one-sided semantics.
# Anchor economics (inp.invcost 300, olife 30, discount 0, charge/discharge
# 10): unconstrained charger obj = 10 + 10*300/30 = 110; forcing
# InpCap = 2*OutCap adds another 100 -> obj 210.
# =========================================================================== #

si_slices <- paste0("s", 1:4)

si_build <- function(inp2out = data.frame(), inp_invcost = NULL) {
  invcost <- if (is.null(inp_invcost)) data.frame(stg.invcost = 0) else
    data.frame(inp.invcost = inp_invcost, stg.invcost = 0)
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
    si_build(inp2out = data.frame(inp2out.lo = 2), inp_invcost = 300),
    name = "si_i", overwrite = TRUE)))
  expect_equal(unique(ff_bound(scen, "pStorageInp2out", "lo")$value), 2)
  expect_gte(nrow(ff_param(scen, "mStorageInp2outLo")), 1)
  # the completed up = Inf side is OPEN: no up equation may be instantiated
  expect_equal(nrow(ff_param(scen, "mStorageInp2outUp")), 0)
  # without any charger driver (no inp costs/caps) there is no vStorageInpCap
  # and the ratio is inlined into the flow bounds instead: both maps empty
  scen2 <- suppressMessages(suppressWarnings(interpolate_model(
    si_build(inp2out = data.frame(inp2out.lo = 2, inp2out.up = 3)),
    name = "si_i2", overwrite = TRUE)))
  expect_equal(nrow(ff_param(scen2, "mStorageInp2outLo")), 0)
})

# @covers pStorageInp2out vStorageInpCap vStorageOutCap eqStorageInp2outLo depth=S backends=glpk
test_that("family storage-inp2out: default ratio ties the charger to the discharger", {
  skip_if_no_solver()
  base <- suppressMessages(suppressWarnings(interpolate_model(
    si_build(inp_invcost = 300), name = "si_base", overwrite = TRUE)))
  base <- .fork_solve(base, solver_options$glpk)
  expect_true(verify_solution(base)$ok)
  # a good solve records its status (solved had no writer before 2026-08-25)
  expect_true(isTRUE(base@status$solved))
  expect_true(isTRUE(base@status$optimal))
  # binding default [1, 1]: InpCap == OutCap == 10, charger EAC 100
  expect_equal(ff_solution_sum(base, "vStorageInpCap"), 10, tolerance = 1e-6)
  expect_equal(ff_solution_sum(base, "vStorageOutCap"), 10, tolerance = 1e-6)
  expect_equal(.fork_objective(base), 110, tolerance = 1e-6)
})

test_that("family storage-inp2out: a lone lo bound oversizes the charger, up side open", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    si_build(inp2out = data.frame(inp2out.lo = 2), inp_invcost = 300),
    name = "si_lo", overwrite = TRUE)))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  # band [2, Inf): cost-minimal charger is exactly 2 x OutCap
  expect_equal(ff_solution_sum(scen, "vStorageInpCap"), 20, tolerance = 1e-6)
  expect_equal(ff_solution_sum(scen, "vStorageOutCap"), 10, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 210, tolerance = 1e-6)
})

test_that("family storage-inp2out: a lone up bound leaves the lo side open (documented)", {
  skip_if_no_solver()
  # one-sided `.up` means lo is OPEN, not the default 1: a priced charger may
  # size to zero and its invcost then (deliberately) costs nothing
  up <- suppressMessages(suppressWarnings(interpolate_model(
    si_build(inp2out = data.frame(inp2out.up = 3), inp_invcost = 300),
    name = "si_up", overwrite = TRUE)))
  up <- .fork_solve(up, solver_options$glpk)
  expect_true(verify_solution(up)$ok)
  expect_equal(.fork_objective(up), 10, tolerance = 1e-6)
})
