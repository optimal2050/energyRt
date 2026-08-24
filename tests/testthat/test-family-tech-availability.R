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
# RAMPS ARE PINNED AS KNOWN-BUG-INERT below: technology@af carries
# rampup/rampdown columns and users can set them, but interpolation never
# populates pTechRampUp/Down or mTechRampUp/Down (the .modInp provenance
# points at nonexistent slots "rampup"/"rampdown"), so the constraint is
# silently dead on every backend. The GLPK template equations additionally
# look wrong (divide by pTechRampUp -- smaller ramp would RELAX the
# constraint -- and square pTechCap2act). When the pipeline is fixed these
# pins must be replaced with real analytic assertions.
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

# ---- KNOWN BUG: ramp limits are silently inert ----------------------------- #
# The pins below assert the CURRENT (broken) behavior so that a fix breaks
# them loudly and gets asserted properly. Do not "fix" the test without
# fixing the pipeline. See file header.
# @covers pTechRampUp pTechRampDown depth=I backends=glpk
test_that("family tech-availability: KNOWN-BUG ramp columns are accepted but never reach the model", {
  # the user-facing surface accepts ramps...
  tech <- newTechnology("T", input = list(comm = "A"), output = list(comm = "B"),
                        af = data.frame(af.up = 1, rampup = 0.1, rampdown = 0.2))
  expect_true(all(c("rampup", "rampdown") %in% colnames(tech@af)))
  expect_equal(tech@af$rampup, 0.1)
  # ...but interpolation drops them: parameter and gate map stay empty
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ta_build(af = data.frame(af.up = 0.5, rampup = 0.1, rampdown = 0.2)),
    name = "ta_ramp", overwrite = TRUE)))
  pru <- ff_param(scen, "pTechRampUp")
  expect_true(is.null(pru) || nrow(pru) == 0,
              label = "KNOWN-BUG pin: pTechRampUp empty (fix pending)")
  mru <- ff_param(scen, "mTechRampUp")
  expect_true(is.null(mru) || nrow(mru) == 0,
              label = "KNOWN-BUG pin: mTechRampUp empty (fix pending)")
})

test_that("family tech-availability: KNOWN-BUG a binding ramp does not change the solution", {
  skip_if_no_solver()
  base <- suppressMessages(suppressWarnings(interpolate_model(
    ta_build(af = data.frame(af.up = 1), dem = c(5, 10, 15, 20)),
    name = "ta_noramp", overwrite = TRUE)))
  ramp <- suppressMessages(suppressWarnings(interpolate_model(
    ta_build(af = data.frame(af.up = 1, rampup = 0.1), dem = c(5, 10, 15, 20)),
    name = "ta_ramp01", overwrite = TRUE)))
  ob <- .fork_objective(.fork_solve(base, solver_options$glpk))
  or <- .fork_objective(.fork_solve(ramp, solver_options$glpk))
  # rampup = 0.1 against inter-slice jumps of 5 SHOULD bind; today it cannot
  expect_equal(or, ob, tolerance = 1e-9,
               label = "KNOWN-BUG pin: ramp has no effect (fix pending)")
})
