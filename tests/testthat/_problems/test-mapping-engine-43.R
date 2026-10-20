# Extracted from test-mapping-engine.R:43

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "energyRt", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_no_fixtures()
scen <- interp_tier("tm_policy")
expect_snapshot_value(mapping_counts(scen), style = "json2")
