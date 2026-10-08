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

# --------------------------------------------------------------------------- #
# A user cost on a per-timeslice variable must be weighted to the year.
#
# `pTimesliceWeight` is 1 on every FULL calendar, which is why this went
# unnoticed: it is the SAMPLED-calendar scale-up factor (4.06 for a quarter
# standing in for a year, 91.25 for one day per season). The generated cost
# term omitted it, so a levy on `vTechAct` was under-charged by exactly that
# factor on any sampled calendar, while the native cost equations
# (`eqTechVarom`, `eqTaxCost`) carried it all along.
# --------------------------------------------------------------------------- #

cc_build_sampled <- function(extra = NULL) {
  utils::data("calendars", package = "energyRt", envir = environment())
  cal <- calendars$m12_subset_q1
  objs <- list(
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = cal@default_timeframe),
    newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
    newTechnology("ECHEAP", input = list(comm = "COA"), output = list(comm = "ELC"),
                  invcost = list(invcost = 300), olife = list(olife = 30),
                  cap2act = 1),
    newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 40)))
  if (!is.null(extra)) objs <- c(objs, list(extra))
  newModel("ccs",
    repo = do.call(newRepository, c(list("ccs_repo"), objs)),
    calendar = cal, region = "R1", horizon = newHorizon(2025), discount = 0)
}

# @covers vUserCosts vTotalUserCosts pTimesliceWeight depth=S backends=glpk
test_that("family constraints-costs: a user cost is weighted on a sampled calendar", {
  skip_if_no_solver()
  uc <- newCosts(name = "ACTFEE", variable = "vTechAct", mult = 2,
                 subset = list(tech = "ECHEAP"))
  scen <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    cc_build_sampled(uc), name = "ccs_costs", overwrite = TRUE))),
    solver_options$glpk)
  expect_true(verify_solution(scen)$ok)

  # the calendar must actually sample, or this test proves nothing
  wd <- ff_param(scen, "pTimesliceWeight")
  expect_gt(max(wd$value), 1)

  # independent arithmetic: rate x sum(activity x weight), from the solution
  act <- get_data_slot(scen@modOut@variables[["vTechAct"]], optional = TRUE)
  wv <- stats::setNames(wd$value, as.character(wd$timeslice))
  expected <- 2 * sum(act$value * wv[as.character(act$timeslice)])

  expect_equal(ff_solution_sum(scen, "vUserCosts"), expected, tolerance = 1e-6)
  # the hand number, so a change of fixture cannot quietly move the target
  expect_equal(ff_solution_sum(scen, "vUserCosts"), 973.3333333, tolerance = 1e-5)
  # the solver's own total must agree with the R-side reconstruction
  expect_equal(ff_solution_sum(scen, "vTotalUserCosts"),
               ff_solution_sum(scen, "vUserCosts"), tolerance = 1e-6)
  # and it must NOT be the unweighted figure
  expect_false(isTRUE(all.equal(ff_solution_sum(scen, "vUserCosts"),
                                2 * sum(act$value), tolerance = 1e-6)))
})

# @covers vUserCosts vTotalUserCosts depth=S backends=glpk
test_that("family constraints-costs: a per-timeslice rate compiles and solves", {
  skip_if_no_solver()
  # `mult` carrying a `timeslice` column used to error with "no applicable
  # method for `@` applied to an object of class NULL": the calendar was read
  # from the TRIMMED approximation context, where it is never present.
  uc <- newCosts(name = "TSFEE", variable = "vTechAct",
                 mult = data.frame(timeslice = c("WINTER", "SUMMER"),
                                   value = c(2, 5)))
  objs <- list(
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "SEASON"),
    newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
    newTechnology("ECHEAP", input = list(comm = "COA"), output = list(comm = "ELC"),
                  invcost = list(invcost = 300), olife = list(olife = 30),
                  cap2act = 1),
    newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 40)),
    uc)
  mod <- newModel("ccts",
    repo = do.call(newRepository, c(list("ccts_repo"), objs)),
    calendar = newCalendar(timetable = make_timetable(
      struct = list(SEASON = c("WINTER", "SUMMER"))), name = "ccts_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
  scen <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    mod, name = "ccts", overwrite = TRUE))), solver_options$glpk)
  expect_true(verify_solution(scen)$ok)

  act <- get_data_slot(scen@modOut@variables[["vTechAct"]], optional = TRUE)
  wv <- stats::setNames(ff_param(scen, "pTimesliceWeight")$value,
                        as.character(ff_param(scen, "pTimesliceWeight")$timeslice))
  rate <- c(WINTER = 2, SUMMER = 5)
  expected <- sum(act$value * wv[as.character(act$timeslice)] *
                    rate[as.character(act$timeslice)])
  expect_equal(ff_solution_sum(scen, "vUserCosts"), expected, tolerance = 1e-6)
  expect_equal(ff_solution_sum(scen, "vUserCosts"), 280, tolerance = 1e-6)
  # the two rates must differ in the result, or the per-slice split did nothing
  expect_gt(ff_solution_sum(scen, "vUserCosts"), 2 * sum(act$value))
})

test_that("family constraints-costs: a cost filter that matches nothing is an error", {
  # Both filters used to drop every row and compile to a sum over an empty
  # set -- a cost of exactly zero, with no error anywhere.
  ghost <- newCosts("GHOST", variable = "vTechAct", mult = 2,
                    subset = list(tech = "NOSUCHTECH"))
  expect_error(
    suppressMessages(suppressWarnings(interpolate_model(
      cc_build(ghost), name = "cc_ghost", overwrite = TRUE))),
    "silently zero")
  ghost2 <- newCosts("GHOST2", variable = "vTechAct",
                     mult = data.frame(tech = "NOSUCHTECH", value = 2))
  expect_error(
    suppressMessages(suppressWarnings(interpolate_model(
      cc_build(ghost2), name = "cc_ghost2", overwrite = TRUE))),
    "silently zero")
  # and a real target is unaffected
  ok <- newCosts("REAL", variable = "vTechAct", mult = 2,
                 subset = list(tech = "ECHEAP"))
  expect_s4_class(
    suppressMessages(suppressWarnings(interpolate_model(
      cc_build(ok), name = "cc_real", overwrite = TRUE))), "scenario")
})

# @covers vUserCosts vTotalUserCosts depth=S backends=glpk
test_that("family constraints-costs: the user-cost breakdown reconciles with its total", {
  skip_if_no_solver()
  # `.vs_check_cost` excludes vUserCosts because it is "already aggregated into
  # vTotalUserCosts" -- an assumption nothing verified. The two are computed by
  # different code (the generated equation vs the R reconstruction in read.R)
  # and they HAD drifted: the equation weighted per-timeslice variables and the
  # reconstruction did not. This check is what makes that fail loudly.
  expect_true("user_costs" %in% verify_checks()$check)

  uc <- newCosts(name = "ACTFEE", variable = "vTechAct", mult = 2,
                 subset = list(tech = "ECHEAP"))
  scen <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    cc_build_sampled(uc), name = "ccs_inv", overwrite = TRUE))),
    solver_options$glpk)
  v <- verify_solution(scen)
  expect_true(v$ok)
  expect_identical(v$checks$user_costs$status, "ok")
  expect_equal(ff_solution_sum(scen, "vUserCosts"),
               ff_solution_sum(scen, "vTotalUserCosts"), tolerance = 1e-6)
})

# @covers vTotalUserCosts depth=S backends=glpk
test_that("family constraints-costs: a NEGATIVE user cost solves", {
  skip_if_no_solver()
  # vTotalUserCosts was declared non-negative in all four templates while
  # vTaxCost/vSubsCost were free. Since eqTotalUserCosts is an equality, a net
  # negative user cost (i.e. a subsidy) pinned a >= 0 variable to a negative
  # value: GLPK reported "PROBLEM HAS NO PRIMAL FEASIBLE SOLUTION" and the
  # other three backends failed in their own way. Data-dependent, too -- a
  # model where taxes outweighed the subsidy in that (region, year) solved fine.
  sub <- newCosts("SUBV", variable = "vTechAct", mult = -1,
                  subset = list(tech = "ECHEAP"))
  scen <- .fork_solve(suppressMessages(suppressWarnings(interpolate_model(
    cc_build(sub), name = "cc_negcost", overwrite = TRUE))), solver_options$glpk)
  expect_true(isTRUE(scen@status$optimal))
  expect_equal(ff_solution_sum(scen, "vTotalUserCosts"), -40, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 400, tolerance = 1e-6)
  expect_true(verify_solution(scen)$ok)
})
