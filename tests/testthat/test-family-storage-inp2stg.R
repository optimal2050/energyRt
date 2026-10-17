# =========================================================================== #
# Feature family: storage-inp2stg (charging C-rate: charger/reservoir link).
#
# eqStorageInp2stgLo/Up: vStorageInpCap >=/<= rate * vStorageStgCap, emitted
# only where a finite bound is declared on a storage with a charger
# (mStorageInpStgCap = mStorageInpCap intersect mStorageStgCap; the reservoir
# always exists since the anchor move of 2026-10-17).
#
# Semantics, as `duration` and `inp2out`:
#   - nothing declared      -> NO equation (open default [0, Inf])
#   - one side declared     -> the other side is OPEN
#   - declared without a charger -> interpolation error
# The three ratios form a triangle (inp2stg = inp2out / duration); the
# constructor refuses a contradictory fixed triangle.
#
# Economics (discount 0, olife 30, charge/discharge 10, supply cost 1,
# inp.invcost 300 -> charger EAC 10/unit, stg.invcost 30 -> reservoir EAC
# 1/unit). `inp.cap.lo = 10` pins the charger; there is no discharger, so the
# discharge is limited by the level only. The level held between s1 and s3
# keeps StgCap >= 10.
#   base              InpCap 10, StgCap 10:                 10 + 100 + 10 = 120
#   inp2stg.up = 0.5  -> StgCap >= 2*InpCap = 20:           10 + 100 + 20 = 130
#   inp2stg.lo = 2    -> InpCap >= 2*StgCap = 20:           10 + 200 + 10 = 220
# =========================================================================== #

si2_slices <- paste0("s", 1:4)

si2_build <- function(inp2stg = NULL, charger = TRUE) {
  inv <- data.frame(stg.invcost = 30)
  cap <- data.frame()
  if (charger) {
    inv$inp.invcost <- 300
    cap <- data.frame(inp.cap.lo = 10)
  }
  newModel("si2",
    repo = newRepository("si2_repo",
      newCommodity("ELC", timeframe = "SL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(timeslice = si2_slices, cost = 1,
                                    ava.up = c(1000, 0, 0, 0))),
      newStorage("STG", commodity = "ELC",
                 olife = data.frame(olife = 30),
                 capacity = cap,
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
  # the standard EV pattern -- duration and inp2out OPENED around a bounded
  # C-rate -- is not over-determined: opened sides are not edges
  expect_no_warning(
    newStorage("S6", commodity = "ELC",
               duration = data.frame(duration.lo = 0, duration.up = 1e6),
               inp2out  = data.frame(inp2out.lo = 0, inp2out.up = 1e6),
               inp2stg  = data.frame(inp2stg.up = 0.5)))
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
  # a rate declared on a storage without a charger is an error
  expect_error(suppressMessages(suppressWarnings(interpolate_model(
    si2_build(inp2stg = data.frame(inp2stg.up = 0.5), charger = FALSE),
    name = "si2_i3", overwrite = TRUE))), "no charger")
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
  # InpCap == 10 (pinned) and InpCap <= 0.5 * StgCap
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
  # StgCap >= 10 (must hold the cycle), so InpCap >= 20
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
