# Standard testthat entry point: makes the suite run under R CMD check.
# Under check the tier resolves to "check" (solver-free; see
# tests/testthat/helper-solvers.R); locally devtools::test() runs tier "fast".
# Solver paths are read from ~/.energyRt/config.yml at load; see ?en_config_show.
library(testthat)
library(energyRt)

test_check("energyRt")
