# =========================================================================== #
# Feature family: tech-availability (af / afs availability bounds + ramps).
#
# Mini-model, 1 region, ANNUAL + 4 equal slices, one year (2025), discount 0:
#   ELC demand 10 per slice (annual 40), ECOA converts COA (cost 1) -> ELC,
#   invcost 1000, olife 30, cap2act 1.
# Arithmetic (verified against the GLPK template):
#   per-slice bound: act_s <= af.up * share_s * cap  -> cap = 10/(af*0.25)
#   annual bound   : sum act <= afs.up * cap         -> cap = 40/afs
#   objective = cap * invcost/olife + fuel (= total activity * 1)
#   af.up = 0.5 (or afs.up = 0.5): cap = 80, objective = 80*1000/30 + 40
#                                                      = 2706.6667
#   af.up = 1: cap = 40, objective = 40*1000/30 + 40   = 1373.3333
#
# Ramp semantics (fixed 2026-08-24; previously the feature was silently inert
# and the templates carried a squared cap2act and swapped Up/Down):
#   rampup/rampdown live as columns of technology@af and are ramping TIMES:
#   allowed rate change per consecutive-slice step (s -> next sp) is
#     share[s] * cap2act * cap / ramp
#   eqTechRampUp bounds increases (act[sp]/share - act[s]/share),
#   eqTechRampDown bounds decreases. Pairing is cyclic via mTimesliceNext.
# Analytic case (rampup = 5, demand 5,10,15,20, af.up = 1):
#   rates must climb to 80 in steps of at most 0.25*80/5 = 4, so the optimum
#   overproduces early: rates 68,72,76,80 -> activity 17,18,19,20,
#   objective = 2666.667 + 74 = 2740.667 (base 2716.667 + 24 smoothing fuel).
# With cap2act = 2 and doubled demand the allowance is 0.25*2*80/5 = 8 --
# a squared cap2act would allow 16, so the max observed jump discriminates.
# =========================================================================== #

ta_slices <- paste0("s", 1:4)

ta_build <- function(af = data.frame(af.up = 0.5),
                     afs = data.frame(),
                     dem = rep(10, 4)) {
  tt <- make_timetable(struct = list(ANNUAL = "ANNUAL", SL = ta_slices))
  newModel("ta",
    repo = newRepository("ta_repo",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SL"),
      newSupply("SUP_COA", commodity = "COA", supply = data.frame(cost = 1)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    af = af, afs = afs,
                    invcost = list(invcost = 1000), olife = list(olife = 30),
                    cap2act = 1),
      newDemand("DEM_ELC", commodity = "ELC",
                demand = data.frame(timeslice = ta_slices, demand = dem))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = ta_slices)),
      name = "ta_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

ta_expected_obj <- function(cap, total_act = 40) cap * 1000 / 30 + total_act

ta_check_params <- function(scen, case_name) {
  af <- ff_bound(scen, "pTechAf", "up")
  expect_false(is.null(af) || nrow(af) == 0,
               label = paste0("tech-availability:", case_name, " pTechAf rows"))
  expect_equal(unique(af$value), 0.5,
               label = paste0("tech-availability:", case_name, " af.up value"))
}

ta_check_solution <- function(scen, backend, case_name) {
  lbl <- function(x) paste0("tech-availability:", backend, ":", case_name, " ", x)
  expect_equal(ff_solution_sum(scen, "vTechCap"), 80, tolerance = 1e-6,
               label = lbl("capacity forced by af"))
  expect_equal(.fork_objective(scen), ta_expected_obj(80), tolerance = 1e-6,
               label = lbl("objective"))
}

# @covers pTechAf depth=X backends=glpk,julia_highs,pyomo,gams forks=fold,ondisk,sparse,default
# @covers vTechCap vTechAct eqTechAfUp depth=X backends=glpk,julia_highs,pyomo,gams
run_family("tech-availability",
           build = ta_build,
           check_params = ta_check_params,
           check_solution = ta_check_solution)

# ---- af.up = 1 sanity anchor of the analytic formula ------------------------ #
test_that("family tech-availability: af.up = 1 halves the required capacity", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ta_build(af = data.frame(af.up = 1)), name = "ta_af1", overwrite = TRUE)))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vTechCap"), 40, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), ta_expected_obj(40), tolerance = 1e-6)
})

# ---- afs: annual energy availability -------------------------------------- #
# @covers pTechAfs eqTechAfsUp depth=S backends=glpk
test_that("family tech-availability: afs.up bounds the annual energy", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ta_build(af = data.frame(),
             afs = data.frame(timeslice = "ANNUAL", afs.up = 0.5)),
    name = "ta_afs", overwrite = TRUE)))
  af <- ff_bound(scen, "pTechAfs", "up")
  expect_equal(unique(af$value), 0.5)
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vTechCap"), 80, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), ta_expected_obj(80), tolerance = 1e-6)
})

# ---- ramps: parameter and gate map reach the model -------------------------- #
ta_build_ramp <- function(rampup = 5) {
  ta_build(af = data.frame(af.up = 1, rampup = rampup),
           dem = c(5, 10, 15, 20))
}

# ta_build with a cap2act override (base helper pins cap2act = 1)
ta_build_c2a <- function(rampup = 5, cap2act = 2) {
  newModel("ta",
    repo = newRepository("ta_repo",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SL"),
      newSupply("SUP_COA", commodity = "COA", supply = data.frame(cost = 1)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    af = data.frame(af.up = 1, rampup = rampup),
                    invcost = list(invcost = 1000), olife = list(olife = 30),
                    cap2act = cap2act),
      newDemand("DEM_ELC", commodity = "ELC",
                demand = data.frame(timeslice = ta_slices,
                                    demand = 2 * c(5, 10, 15, 20)))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = ta_slices)),
      name = "ta_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

# rate profile of the solved tech, in timeslice order
.ta_rates <- function(scen, share = 0.25) {
  act <- ff_solution_sum(scen, "vTechAct", by = "timeslice")
  act[order(timeslice)]$value / share
}

# @covers pTechRampUp pTechRampDown depth=I backends=glpk
test_that("family tech-availability: ramp columns reach modInp and the gate map", {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ta_build(af = data.frame(af.up = 1, rampup = 5, rampdown = 7)),
    name = "ta_ramp_i", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pTechRampUp")$value), 5)
  expect_equal(unique(ff_param(scen, "pTechRampDown")$value), 7)
  # cyclic consecutive-slice pairing: one row per slice
  expect_equal(nrow(ff_param(scen, "mTechRampUp")), length(ta_slices))
  expect_equal(nrow(ff_param(scen, "mTechRampDown")), length(ta_slices))
})

# @covers pTechRampUp mTechRampUp eqTechRampUp depth=S backends=glpk
test_that("family tech-availability: a binding rampup forces analytic smoothing", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ta_build_ramp(rampup = 5), name = "ta_ramp5", overwrite = TRUE)))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  # allowed rate step = share * cap2act * cap / ramp = 0.25 * 80 / 5 = 4;
  # optimum overproduces early: rates 68,72,76,80
  expect_equal(.ta_rates(scen), c(68, 72, 76, 80), tolerance = 1e-6)
  expect_equal(ff_solution_sum(scen, "vTechCap"), 80, tolerance = 1e-6)
  # base 2716.667 + 24 smoothing fuel
  expect_equal(.fork_objective(scen), 80 * 1000 / 30 + 74, tolerance = 1e-6)
})

test_that("family tech-availability: ramp allowance scales with cap2act ONCE", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ta_build_c2a(rampup = 5, cap2act = 2), name = "ta_ramp_c2a", overwrite = TRUE)))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  rates <- .ta_rates(scen)
  cap <- ff_solution_sum(scen, "vTechCap")
  expect_equal(cap, 80, tolerance = 1e-6)  # peak rate 160 / cap2act 2
  # single cap2act: allowed step = 0.25 * 2 * 80 / 5 = 8. The old squared
  # template allowed 16 -- the observed max jump discriminates.
  expect_lte(max(diff(rates)), 8 + 1e-6)
  expect_equal(max(diff(rates)), 8, tolerance = 1e-6)  # binding
  expect_equal(.fork_objective(scen), 80 * 1000 / 30 + 148, tolerance = 1e-6)
})

# @covers pTechRampUp depth=X backends=julia_highs,pyomo forks=default
test_that("family tech-availability: ramp objective agrees across backends", {
  skip_if_no_solver()
  ref <- 80 * 1000 / 30 + 74
  for (backend in c("julia_highs", "pyomo_cbc")) {
    if (backend == "julia_highs") skip_if_no_julia_highs() else skip_if_no_pyomo()
    scen <- suppressMessages(suppressWarnings(interpolate_model(
      ta_build_ramp(rampup = 5), name = paste0("ta_ramp_", backend),
      overwrite = TRUE)))
    solver <- if (backend == "julia_highs") solver_options$julia_highs else
      solver_options$pyomo_cbc
    scen <- .fork_solve(scen, solver)
    expect_true(verify_solution(scen)$ok, label = paste0(backend, " invariants"))
    expect_equal(.fork_objective(scen), ref, tolerance = 1e-6,
                 label = paste0(backend, " ramp objective"))
  }
})
