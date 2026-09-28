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
# =========================================================================== #
if (!file.exists("DESCRIPTION")) stop("Run from the package root.")
Sys.setenv(ENERGYRT_TEST_TIER = "fast")
suppressMessages(pkgload::load_all(".", quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
filter <- if (length(args)) paste(args, collapse = "|") else NULL

# Progress so the run is readable as it goes, and a list so the per-test
# timings survive to the end. `progress_max_fails = Inf` because the default
# stops the run after ten failures, which leaves the rest of the suite
# unmeasured -- the opposite of what a baseline needs. `cli.dynamic = FALSE`
# keeps in-place line updates out of a redirected log.
options(testthat.progress_max_fails = Inf, cli.dynamic = FALSE)
lst <- testthat::ListReporter$new()
# `update_interval = Inf` when the output is not a terminal: the spinner
# redraws in place, which a redirected log records as one line per redraw --
# hundreds of them per file. Off, each file prints exactly one line, with its
# own elapsed time, the moment it finishes.
rep <- testthat::MultiReporter$new(reporters = list(
  testthat::ProgressReporter$new(
    max_failures = Inf,
    update_interval = if (isatty(stdout())) 1 else Inf),
  lst))

t0 <- proc.time()[3]
devtools::test(reporter = rep, filter = filter)
res <- as.data.frame(lst$get_results())
mins <- (proc.time()[3] - t0) / 60

failed <- sum(res$failed) + sum(res$error)
if (nrow(res)) {
  # `error` is counted separately from `failed`: a file that only errors has
  # failed == 0, so aggregating on `failed` alone drops it from this table while
  # the headline still counts it -- silence that reads as success.
  res$broken <- res$failed + res$error
  agg <- aggregate(cbind(broken, failed, error, skipped, passed) ~ file, res,
                   sum)
  bad <- agg[agg$broken > 0, , drop = FALSE]
  if (nrow(bad)) {
    cat("\nfiles with failures or errors:\n")
    print(bad[order(-bad$broken), c("file", "failed", "error", "passed")],
          row.names = FALSE)
  }
  # where the time went, so the next run can be aimed rather than repeated
  tm <- aggregate(real ~ file, res, sum)
  tm <- tm[order(-tm$real), , drop = FALSE]
  tm$min <- round(tm$real / 60, 2)
  cat("\nslowest files (", nrow(tm), " ran, ", round(sum(tm$min), 1),
      " min in tests):\n", sep = "")
  print(utils::head(tm[, c("file", "min")], 12L), row.names = FALSE)
}

cat(sprintf("[fast] failed %d | skipped %d | passed %d | %.1f min%s\n",
            failed, sum(res$skipped), sum(res$passed), mins,
            if (is.null(filter)) "" else paste0(" | filter: ", filter)))
quit(status = if (failed > 0) 1L else 0L)
