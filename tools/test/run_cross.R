# =========================================================================== #
# run_cross.R -- the cross-backend tier: parity suites at tier "cross"
# (GLPK + whichever of Julia/HiGHS, Python/Pyomo, GAMS are installed).
#   Rscript tools/test/run_cross.R
# Prints the toolchain availability first, then runs the cross-related files.
# Missing toolchains skip; only real mismatches fail. See tests/README.md.
# =========================================================================== #
if (!file.exists("DESCRIPTION")) stop("Run from the package root.")
Sys.setenv(ENERGYRT_TEST_TIER = "cross")
suppressMessages(pkgload::load_all(".", quiet = TRUE))

cat("Toolchains:\n")
for (probe in list(c("glpsol", "glpk"), c("julia", "julia"),
                   c("python", "python"), c("gams", "gams"))) {
  path <- tryCatch(get_option(paste0(probe[2], "_path")), error = function(e) "")
  found <- (is.character(path) && nzchar(path) && file.exists(path)) ||
    nzchar(Sys.which(probe[1]))
  cat(sprintf("  %-7s %s\n", probe[1], if (found) "found" else "NOT found"))
}

t0 <- proc.time()[3]
res <- as.data.frame(devtools::test(
  filter = "cross-solver|one-all-solvers|family-", reporter = "silent"))
failed <- sum(res$failed)
agg <- aggregate(cbind(failed, skipped, passed) ~ file, res, sum)
bad <- agg[agg$failed > 0, , drop = FALSE]
if (nrow(bad)) print(bad, row.names = FALSE)
cat(sprintf("[cross] failed %d | skipped %d | passed %d | %.1f min\n",
            failed, sum(res$skipped), sum(res$passed),
            (proc.time()[3] - t0) / 60))
quit(status = if (failed > 0) 1L else 0L)
