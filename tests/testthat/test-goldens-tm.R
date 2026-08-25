# =========================================================================== #
# Golden regression on the tm_* tier models: objective and tracked values at
# annual x region and annual x total levels must reproduce the stored
# benchmark (tests/testthat/goldens/tm.json, written by
# tools/test/make_goldens.R -- which refuses to freeze a benchmark that fails
# verify_solution()). Complements the inline objective pins of
# test-model-regression.R with variable-level tracking.
# =========================================================================== #

# @covers vObjective vTechNewCap vTechOut vTechInp vSupOut vDemInp vEmsFuelTot depth=S backends=glpk
test_that("tier models reproduce their golden tracked values (GLPK)", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  g <- skip_if_no_golden("tm")
  env <- .mapping_fixture_env()
  for (tier in names(g)) {
    skip_if(!is.function(env[[tier]]), paste0(tier, "() not in fixture catalog"))
    scen <- suppressMessages(suppressWarnings(
      solve_model(env[[tier]](), name = paste0("gold_", tier),
                solver = solver_options$glpk, tmp.del = TRUE, wait = TRUE)))
    vs <- verify_solution(scen)
    expect_true(vs$ok, label = paste0(tier, " invariants"))
    expect_matches_golden(scen, "tm", tier, kind = "same_solver")
  }
})
