# =========================================================================== #
# run_fast.R -- the pre-commit tier: full test suite at tier "fast" (GLPK).
#   Rscript tools/test/run_fast.R
# Equivalent to devtools::test() with glpsol present; prints a one-line
# summary and exits non-zero on failure. See tests/README.md for tiers.
# =========================================================================== #
if (!file.exists("DESCRIPTION")) stop("Run from the package root.")
Sys.setenv(ENERGYRT_TEST_TIER = "fast")
suppressMessages(pkgload::load_all(".", quiet = TRUE))
t0 <- proc.time()[3]
res <- as.data.frame(devtools::test(reporter = "silent"))
failed <- sum(res$failed)
agg <- aggregate(cbind(failed, skipped, passed) ~ file, res, sum)
bad <- agg[agg$failed > 0, , drop = FALSE]
if (nrow(bad)) print(bad, row.names = FALSE)
cat(sprintf("[fast] failed %d | skipped %d | passed %d | %.1f min\n",
            failed, sum(res$skipped), sum(res$passed),
            (proc.time()[3] - t0) / 60))
quit(status = if (failed > 0) 1L else 0L)
