# =========================================================================== #
# Feature family: constraints-costs (user constraints solved end-to-end, and
# the `costs` class adding user cost terms to the objective).
#
# Mini-model, 1 region, ANNUAL, one year (2025), discount 0:
#   ECHEAP (invcost 300, olife 30 -> EAC 10/unit/yr)
#   EEXP   (invcost 3000, olife 30 -> EAC 100/unit/yr)
#   both COA -> ELC (COA cost 1), DEM 40.
#   base: all cheap -> obj = 40 fuel + 40*10 = 440
#   constraint vTechNewCap[ECHEAP] <= 25 -> 25 cheap + 15 expensive:
#     obj = 40 + 25*10 + 15*100 = 1790
#   costs: 2 per unit vTechAct of ECHEAP -> obj = 440 + 2*40 = 520
# =========================================================================== #

cc_build <- function(extra = NULL) {
  objs <- list(
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "ANNUAL"),
    newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
    newTechnology("ECHEAP", input = list(comm = "COA"), output = list(comm = "ELC"),
                  invcost = list(invcost = 300), olife = list(olife = 30),
                  cap2act = 1),
    newTechnology("EEXP", input = list(comm = "COA"), output = list(comm = "ELC"),
                  invcost = list(invcost = 3000), olife = list(olife = 30),
                  cap2act = 1),
    newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 40)))
  if (!is.null(extra)) objs <- c(objs, list(extra))
  newModel("cc",
    repo = do.call(newRepository, c(list("cc_repo"), objs)),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
      name = "cc_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

# @covers vTechNewCap depth=S backends=glpk
test_that("family constraints-costs: baseline picks the cheap technology only", {
  skip_if_no_solver()
  scen <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    cc_build(), name = "cc_base", overwrite = TRUE))), solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(.fork_objective(scen), 440, tolerance = 1e-6)
})

test_that("family constraints-costs: an unknown summand field is refused, not silently dropped", {
  expect_error(
    newConstraint(name = "BAD", eq = "<=",
                  for.each = data.frame(year = 2025),
                  term1 = list(variable = "vTechNewCap", tech = "ECHEAP"),
                  defVal = 25),
    "Unknown summand field")
})

test_that("family constraints-costs: a user constraint on one tech forces the merit order", {
  skip_if_no_solver()
  cns <- newConstraint(
    name = "CHEAPCAP", eq = "<=",
    for.each = data.frame(year = 2025),
    term1 = list(variable = "vTechNewCap", for.sum = list(tech = "ECHEAP")),
    defVal = 25)
  scen <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    cc_build(cns), name = "cc_cns", overwrite = TRUE))), solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  ncap <- ff_solution_sum(scen, "vTechNewCap", by = "tech")
  expect_equal(ncap[ncap$tech == "ECHEAP"]$value, 25, tolerance = 1e-6)
  expect_equal(ncap[ncap$tech == "EEXP"]$value, 15, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 40 + 25 * 10 + 15 * 100, tolerance = 1e-6)
})

test_that("family constraints-costs: a set-indexed mult prices each member differently", {
  skip_if_no_solver()
  # force a 25/15 split, then fee vTechAct at 2 (cheap) / 3 (expensive):
  # fee = 25*2 + 15*3 = 95 on top of the constrained obj 1790
  cns <- newConstraint(
    name = "CHEAPCAP", eq = "<=",
    for.each = data.frame(year = 2025),
    term1 = list(variable = "vTechNewCap", for.sum = list(tech = "ECHEAP")),
    defVal = 25)
  uc <- newCosts(name = "ACTFEES", variable = "vTechAct",
                 mult = data.frame(tech = c("ECHEAP", "EEXP"), value = c(2, 3)))
  mod <- cc_build(cns)
  mod <- add(mod, uc)
  scen <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    mod, name = "cc_multdf", overwrite = TRUE))), solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vTotalUserCosts"), 95, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 1790 + 95, tolerance = 1e-6)
})

# @covers vUserCosts vTotalUserCosts depth=S backends=glpk
test_that("family constraints-costs: a costs object adds its term to the objective", {
  skip_if_no_solver()
  uc <- newCosts(name = "ACTFEE", variable = "vTechAct",
                 mult = 2, subset = list(tech = "ECHEAP"))
  scen <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    cc_build(uc), name = "cc_costs", overwrite = TRUE))), solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vUserCosts"), 80, tolerance = 1e-6)
  expect_equal(ff_solution_sum(scen, "vTotalUserCosts"), 80, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 440 + 80, tolerance = 1e-6)
})

# @covers vTechNewCap vTotalUserCosts depth=S backends=mm_highs
test_that("family constraints-costs: a user constraint binds on the multimod route", {
  skip_if_no_multimod()

  # The GLPK cases above fix the arithmetic: 440 unconstrained, 1790 capped,
  # 520 with the fee. Re-solving them on the matrix route checks that a user
  # constraint arrives with COEFFICIENTS, not just a row and an rhs: a row
  # carrying the right rhs and no coefficients constrains nothing and solves
  # to the unconstrained 440 with status Optimal.
  base <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    cc_build(), name = "cc_mm_base", overwrite = TRUE))),
    solver_options$mm_highs)
  expect_true(verify_solution(base)$ok)
  expect_equal(.fork_objective(base), 440, tolerance = 1e-6)

  cns <- newConstraint(
    name = "CHEAPCAP", eq = "<=",
    for.each = data.frame(year = 2025),
    term1 = list(variable = "vTechNewCap", for.sum = list(tech = "ECHEAP")),
    defVal = 25)
  capped <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    cc_build(cns), name = "cc_mm_cns", overwrite = TRUE))),
    solver_options$mm_highs)
  expect_true(verify_solution(capped)$ok)
  ncap <- ff_solution_sum(capped, "vTechNewCap", by = "tech")
  expect_equal(ncap[ncap$tech == "ECHEAP"]$value, 25, tolerance = 1e-6)
  expect_equal(ncap[ncap$tech == "EEXP"]$value, 15, tolerance = 1e-6)
  expect_equal(.fork_objective(capped), 1790, tolerance = 1e-6)
  expect_gt(.fork_objective(capped), .fork_objective(base))

  uc <- newCosts(name = "ACTFEE", variable = "vTechAct",
                 mult = 2, subset = list(tech = "ECHEAP"))
  fee <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    cc_build(uc), name = "cc_mm_costs", overwrite = TRUE))),
    solver_options$mm_highs)
  expect_true(verify_solution(fee)$ok)
  expect_equal(ff_solution_sum(fee, "vTotalUserCosts"), 80, tolerance = 1e-6)
  expect_equal(.fork_objective(fee), 520, tolerance = 1e-6)
})

# A term with no for.sum and no subset: vTechNewCap(tech, region, year) against
# for.each = year, so the row sums over tech AND region - a multi-index free
# sum. That is the shape that once emitted the row with its rhs and no
# coefficients at all, solving to the unconstrained 440 and reporting Optimal.
# Total new capacity >= 50 overbuilds past the 40 the demand needs:
#   40 fuel + 50 * 10 EAC = 540, against 440 unconstrained.
# @covers vTechNewCap depth=S backends=glpk,mm_highs
test_that("family constraints-costs: an implicit multi-index sum carries coefficients", {
  skip_if_no_multimod()
  cns <- newConstraint(name = "TOTCAP", eq = ">=",
                       for.each = data.frame(year = 2025),
                       term1 = list(variable = "vTechNewCap"),
                       defVal = 50)
  mm <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    cc_build(cns), name = "cc_mm_totcap", overwrite = TRUE))),
    solver_options$mm_highs)
  expect_true(verify_solution(mm)$ok)
  expect_equal(.fork_objective(mm), 540, tolerance = 1e-6)

  # the same model on GLPK, which never touches multimod
  gl <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    cc_build(cns), name = "cc_gl_totcap", overwrite = TRUE))),
    solver_options$glpk)
  expect_equal(.fork_objective(mm), .fork_objective(gl), tolerance = 1e-6)

  ncap <- ff_solution_sum(mm, "vTechNewCap")
  expect_equal(ncap, 50, tolerance = 1e-6)
})
