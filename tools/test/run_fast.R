# =========================================================================== #
# run_fast.R -- the pre-commit tier: full test suite at tier "fast" (GLPK).
#
#   Rscript tools/test/run_fast.R                    # the whole tier
#   Rscript tools/test/run_fast.R cluster storage    # only matching files
#
# Prints each file as it finishes and every failure the moment it happens, so a
# long run can be read while it runs, then a one-line summary and the slowest
# files. Exits non-zero on failure. See tests/README.md for tiers.
#
# The full tier is the gate before a commit, not the edit loop: name files on
# the command line while iterating -- the argument is a regex over test file
# names, without the `test-` prefix ("process" matches test-process-groups.R).
#
# The reporter pair and the end-of-run tables now live in lib/runner.R, where
# run_cross.R and run_nightly.R can reach them too; both used to be silent and
# to undercount error-only files. Behaviour here is unchanged.
# =========================================================================== #
if (!file.exists("DESCRIPTION")) stop("Run from the package root.")
source(file.path("tools", "test", "lib", "runner.R"))
Sys.setenv(ENERGYRT_TEST_TIER = "fast")
suppressMessages(pkgload::load_all(".", quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
filter <- if (length(args)) paste(args, collapse = "|") else NULL

# Progress so the run is readable as it goes, and a list so the per-test
# timings survive to the end. See en_reporters() for why each option is set.
lst <- testthat::ListReporter$new()
rep <- en_reporters(lst)

t0 <- proc.time()[3]
devtools::test(reporter = rep, filter = filter)
res <- as.data.frame(lst$get_results())
mins <- (proc.time()[3] - t0) / 60

broken <- en_summarise(res, "fast", mins, filter)
quit(status = if (broken > 0) 1L else 0L)
