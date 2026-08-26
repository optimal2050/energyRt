# =========================================================================== #
# UTOPIA golden regression -- pins the shipped teaching model that the frozen
# (eval = FALSE) vignettes describe as "validated out-of-band": base layouts
# R1 / R3 on s4_h24 and the five scenario levers on R1, each against
# tests/testthat/goldens/utopia.json (captured by make_goldens.R, which
# refuses a benchmark that fails verify_solution()).
#
# Lever note: on the R1 layout only CT_CO2 (the carbon tax) shifts the base
# objective; CO2_CAP / RES_SHARE / NO_NEW_NUC / EARLY_RET are non-binding at
# the base optimum -- the goldens pin exactly that invariance.
# =========================================================================== #

# @covers vObjective vTechNewCap vTechOut depth=S backends=glpk
test_that("UTOPIA layouts and levers reproduce their goldens (GLPK)", {
  skip_if_no_solver()
  g <- skip_if_no_golden("utopia")
  entries <- ut_entries()
  objs <- c()
  for (nm in names(g)) {
    skip_if(is.null(entries[[nm]]), paste0("entry ", nm, " not in ut_entries()"))
    scen <- ut_solve_glpk(entries[[nm]], paste0("utg_", nm))
    vs <- verify_solution(scen)
    expect_true(vs$ok, label = paste0(nm, " invariants"))
    expect_true(isTRUE(scen@status$solved), label = paste0(nm, " solved"))
    expect_matches_golden(scen, "utopia", nm, kind = "same_solver")
    objs[nm] <- .fork_objective(scen)
  }
  # levers can only constrain (or tax): never cheaper than base
  for (nm in setdiff(names(objs), c("base_R1", "base_R3"))) {
    expect_gte(objs[[nm]], objs[["base_R1"]] - 1e-6)
  }
  # the carbon tax strictly increases the objective
  expect_gt(objs[["ctco2_R1"]], objs[["base_R1"]])
})
