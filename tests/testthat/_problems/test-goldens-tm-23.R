# Extracted from test-goldens-tm.R:23

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "energyRt", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
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
