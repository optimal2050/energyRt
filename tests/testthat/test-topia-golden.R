# =========================================================================== #
# TOPIA golden regression -- pins the shipped teaching model that the frozen
# (eval = FALSE) vignettes describe as "validated out-of-band": base layouts
# R1 / R3 on s4_h24 and the five scenario levers on R1, each against
# tests/testthat/goldens/topia.json (captured by make_goldens.R, which
# refuses a benchmark that fails verify_solution()).
#
# Lever note: with the endowments finite (0.92) three levers now bite on R1 --
# CO2_CAP, CT_CO2 and EARLY_RET all move the objective. RES_SHARE is all but
# non-binding and NO_NEW_NUC is exactly non-binding (new nuclear is never
# economic), and the goldens pin that invariance too. NO_NEW_NUC runs on R3:
# nuclear sits in C1 alone, so R1 carries no ENUC to constrain.
# =========================================================================== #

# @covers vObjective vTechNewCap vTechOut depth=S backends=glpk
# The R3 corridors carry a flow cost, so the frozen objective moves with
# `trade@varom`: this entry is what pins it to a number.
# @covers pTradeIrVarom depth=X backends=glpk
test_that("TOPIA layouts and levers reproduce their goldens (GLPK)", {
  skip_if_no_solver()
  g <- skip_if_no_golden("topia")
  entries <- ut_entries()
  objs <- c()
  for (nm in names(g)) {
    skip_if(is.null(entries[[nm]]), paste0("entry ", nm, " not in ut_entries()"))
    scen <- ut_solve_glpk(entries[[nm]], paste0("utg_", nm))
    vs <- verify_solution(scen)
    expect_true(vs$ok, label = paste0(nm, " invariants"))
    expect_true(isTRUE(scen@status$solved), label = paste0(nm, " solved"))
    expect_matches_golden(scen, "topia", nm, kind = "same_solver")
    objs[nm] <- .fork_objective(scen)
  }
  # levers can only constrain (or tax): never cheaper than the base of the SAME
  # layout, which is what the entry's suffix names
  for (nm in setdiff(names(objs), c("base_R1", "base_R3"))) {
    base <- paste0("base_", sub("^.*_", "", nm))
    skip_if(is.null(objs[[base]]), paste0("no base entry for ", nm))
    expect_gte(objs[[nm]], objs[[base]] - 1e-6)
  }
  # the carbon tax strictly increases the objective
  expect_gt(objs[["ctco2_R1"]], objs[["base_R1"]])
})
