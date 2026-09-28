# =========================================================================== #
# run_cross.R -- the cross-backend tier: parity suites at tier "cross"
# (GLPK + whichever of Julia/HiGHS, Python/Pyomo, GAMS are installed).
#   Rscript tools/test/run_cross.R
# Prints the toolchain availability first, then runs the cross-related files.
# Missing toolchains skip; only real mismatches fail. See tests/README.md.
#
# Two defects fixed 2026-09-28 by routing through lib/runner.R:
#   * the exit code was `sum(res$failed)` with no `sum(res$error)`, so a file
#     that ONLY errored exited 0 -- the run reported success;
#   * `aggregate()` on an empty result frame errors, so a filter matching
#     nothing crashed instead of reporting nothing to do.
# The run is also no longer silent: it prints each file as it finishes.
# =========================================================================== #
if (!file.exists("DESCRIPTION")) stop("Run from the package root.")
source(file.path("tools", "test", "lib", "runner.R"))
Sys.setenv(ENERGYRT_TEST_TIER = "cross")
suppressMessages(pkgload::load_all(".", quiet = TRUE))

en_toolchains()

filter <- "cross-solver|one-all-solvers|family-"
lst <- testthat::ListReporter$new()
rep <- en_reporters(lst)

t0 <- proc.time()[3]
devtools::test(reporter = rep, filter = filter)
res <- as.data.frame(lst$get_results())
mins <- (proc.time()[3] - t0) / 60

broken <- en_summarise(res, "cross", mins, filter)
quit(status = if (broken > 0) 1L else 0L)
