# =========================================================================== #
# Feature family: storage-inp2stg (charging C-rate: charger/reservoir link).
#
# eqStorageInp2stgLo/Up: vStorageInpCap >=/<= rate * vStorageStgCap, emitted
# only where a finite bound is declared AND both parts carry capacity
# variables (mStorageInpStgCap = mStorageInpCap intersect mStorageStgCap).
#
# Semantics (deliberately unlike `duration`/`inp2out`):
#   - nothing declared     -> NO equation at all: the default is open [0, Inf],
#                             not a binding identity (a 1C default would
#                             silently bind every storage pricing both parts)
#   - one side declared    -> the other side is OPEN (`.norm_ratio()` writes
#                             0-lo / Inf-up, and the map `drop` discards them)
#   - either part unpriced -> no variable pair, no equation (no inlining
#                             fallback exists -- nothing to inline onto)
# The three ratios form a triangle (inp2stg = inp2out / duration); the
# constructor refuses a contradictory fixed triangle.
#
# Anchor economics (discount 0, olife 30, charge/discharge 10, supply cost 1,
# inp.invcost 300 -> charger EAC 10/unit, stg.invcost 30 -> reservoir EAC
# 1/unit). The fixture opens `duration` -- its structural default [1,1] would
# tie StgCap == OutCap and contradict any C-rate != 1 (with the inp2out
# default also binding, the triangle would close on itself). With the level
# bound now on StgCap (stg part priced) nothing sizes the discharger, so
# `out.cap.lo = 10` pins it; the inp2out default then sizes the charger:
# InpCap == OutCap. The level held between s1 and s3 keeps StgCap >= 10.
#   base              InpCap == OutCap == 10, StgCap 10:  10 + 100 + 10 = 120
#   inp2stg.up = 0.5  -> StgCap >= 2*InpCap = 20:         10 + 100 + 20 = 130
#   inp2stg.lo = 2    -> InpCap >= 2*StgCap -> both 20:   10 + 200 + 10 = 220
# =========================================================================== #

si2_slices <- paste0("s", 1:4)

si2_build <- function(inp2stg = NULL, inp_invcost = 300, stg_invcost = 30) {
  inv <- data.frame(stg.invcost = stg_invcost)
  if (!is.null(inp_invcost)) inv$inp.invcost <- inp_invcost
  newModel("si2",
    repo = newRepository("si2_repo",
      newCommodity("ELC", timeframe = "SL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(timeslice = si2_slices, cost = 1,
                                    ava.up = c(1000, 0, 0, 0))),
      newStorage("STG", commodity = "ELC",
                 olife = data.frame(olife = 30),
                 duration = data.frame(duration.lo = 0, duration.up = Inf),
                 capacity = data.frame(out.cap.lo = 10),
                 invcost = inv, inp2stg = inp2stg),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = "s3", demand = 10))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = si2_slices)),
      name = "si2_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

test_that("family storage-inp2stg: constructor normalisation and the ratio triangle", {
  # scalar shorthand fixes the rate
  s <- newStorage("S1", commodity = "ELC", inp2stg = 0.5)
  expect_equal(s@inp2stg$inp2stg.fx, 0.5)
  # a one-sided range opens the other side
  s <- newStorage("S2", commodity = "ELC",
                  inp2stg = data.frame(inp2stg.up = 3))
  expect_equal(s@inp2stg$inp2stg.lo, 0)
  expect_equal(s@inp2stg$inp2stg.up, 3)
  # a contradictory fixed triangle is refused: inp2out/duration = 1/2 != 1
  expect_error(
    newStorage("S3", commodity = "ELC",
               duration = 2, inp2out = 1, inp2stg = 1),
    "inconsistent")
  # a consistent fixed triangle passes silently
  expect_no_error(
    expect_no_warning(
      newStorage("S4", commodity = "ELC",
                 duration = 2, inp2out = 1, inp2stg = 0.5)))
  # three ranged edges over-determine the triangle
  expect_warning(
    newStorage("S5", commodity = "ELC",
               duration = data.frame(duration.lo = 1, duration.up = 2),
               inp2out  = data.frame(inp2out.lo = 0.5, inp2out.up = 1),
               inp2stg  = data.frame(inp2stg.lo = 0.2, inp2stg.up = 1)),
    "over-determined|determine the third")
})

# @covers pStorageInp2stg depth=I backends=glpk
test_that("family storage-inp2stg: the rate bound lands in modInp with its maps", {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    si2_build(inp2stg = data.frame(inp2stg.up = 0.5)),
    name = "si2_i", overwrite = TRUE)))
  expect_equal(unique(ff_bound(scen, "pStorageInp2stg", "up")$value), 0.5)
  expect_gte(nrow(ff_param(scen, "mStorageInp2stgUp")), 1)
  # the completed lo = 0 side is OPEN: no lo equation may be instantiated
  expect_equal(nrow(ff_param(scen, "mStorageInp2stgLo")), 0)
  # nothing declared -> no equation at all (open default, unlike inp2out)
  scen2 <- suppressMessages(suppressWarnings(interpolate_model(
    si2_build(), name = "si2_i2", overwrite = TRUE)))
  expect_equal(nrow(ff_param(scen2, "mStorageInp2stgUp")), 0)
  expect_equal(nrow(ff_param(scen2, "mStorageInp2stgLo")), 0)
  # an unpriced charging part has no vStorageInpCap: the intersection domain
  # is empty and a declared rate emits nothing (no inlining fallback)
  scen3 <- suppressMessages(suppressWarnings(interpolate_model(
    si2_build(inp2stg = data.frame(inp2stg.up = 0.5), inp_invcost = NULL),
    name = "si2_i3", overwrite = TRUE)))
  expect_equal(nrow(ff_param(scen3, "mStorageInp2stgUp")), 0)
})

# @covers pStorageInp2stg vStorageInpCap vStorageStgCap eqStorageInp2stgUp depth=S backends=glpk
test_that("family storage-inp2stg: an up rate grows the reservoir behind a slow charger", {
  skip_if_no_solver()
  base <- suppressMessages(suppressWarnings(interpolate_model(
    si2_build(), name = "si2_base", overwrite = TRUE)))
  base <- .fork_solve(base, solver_options$glpk)
  expect_true(verify_solution(base)$ok)
  expect_equal(ff_solution_sum(base, "vStorageStgCap"), 10, tolerance = 1e-6)
  expect_equal(.fork_objective(base), 120, tolerance = 1e-6)

  scen <- suppressMessages(suppressWarnings(interpolate_model(
    si2_build(inp2stg = data.frame(inp2stg.up = 0.5)),
    name = "si2_up", overwrite = TRUE)))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  # InpCap == OutCap == 10 (inp2out default) and InpCap <= 0.5 * StgCap
  expect_equal(ff_solution_sum(scen, "vStorageInpCap"), 10, tolerance = 1e-6)
  expect_equal(ff_solution_sum(scen, "vStorageStgCap"), 20, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 130, tolerance = 1e-6)
})

test_that("family storage-inp2stg: a lone lo rate oversizes the charger, up side open", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    si2_build(inp2stg = data.frame(inp2stg.lo = 2)),
    name = "si2_lo", overwrite = TRUE)))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  # StgCap >= 10 (must hold the cycle), so InpCap >= 20; inp2out drags OutCap
  expect_equal(ff_solution_sum(scen, "vStorageInpCap"), 20, tolerance = 1e-6)
  expect_equal(ff_solution_sum(scen, "vStorageStgCap"), 10, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 220, tolerance = 1e-6)
})

test_that("family storage-inp2stg: the slot is equivalent to the explicit constraint", {
  skip_if_no_solver()
  # the same physical statement, twice: once as the C-rate slot, once as a
  # hand-written user constraint on the two capacity variables
  CHG <- newConstraint(
    name = "CHG", eq = "<=",
    for.each = data.frame(region = "R1", year = 2025L),
    term1 = list(variable = "vStorageInpCap", for.sum = list(stg = "STG")),
    term2 = list(variable = "vStorageStgCap", for.sum = list(stg = "STG"),
                 mult = -0.5),
    defVal = 0)
  a <- suppressMessages(suppressWarnings(interpolate_model(
    si2_build(inp2stg = data.frame(inp2stg.up = 0.5)),
    name = "si2_eq_a", overwrite = TRUE)))
  b <- suppressMessages(suppressWarnings(interpolate_model(
    si2_build(), CHG, name = "si2_eq_b", overwrite = TRUE)))
  a <- .fork_solve(a, solver_options$glpk)
  b <- .fork_solve(b, solver_options$glpk)
  expect_true(verify_solution(a)$ok)
  expect_true(verify_solution(b)$ok)
  expect_equal(.fork_objective(a), .fork_objective(b), tolerance = 1e-6)
})
