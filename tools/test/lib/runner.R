# =========================================================================== #
# lib/runner.R -- machinery shared by every runner in tools/test/.
#
# Extracted from run_fast.R, which had the only correct implementations: the
# TTY-aware reporter pair and the failed+error aggregation. run_cross.R and
# run_nightly.R each had their own, both silent and both undercounting, and the
# toolchain probe was copied verbatim into both. One home for all three.
#
# Sourced, never run:  source(file.path("tools", "test", "lib", "runner.R"))
# =========================================================================== #

# Every runner is path-relative to the package root.
en_require_root <- function() {
  if (!file.exists("DESCRIPTION")) stop("Run from the package root.")
  invisible(TRUE)
}

# Which backends this machine can reach. Prints the table the runners used to
# print, and RETURNS the findings so a run record can store them -- the probe
# result was previously computed, printed and thrown away.
#
# Deliberately shallow: `get_option()` then `Sys.which()`, no `--version` call.
# The tests' own guards (helper-solvers.R) are shallow for the same reason, and
# starting Julia to label a report is not worth seconds per run.
en_toolchains <- function(print = TRUE) {
  probes <- list(c("glpsol", "glpk"), c("julia", "julia"),
                 c("python", "python"), c("gams", "gams"))
  found <- vapply(probes, function(probe) {
    path <- tryCatch(get_option(paste0(probe[2], "_path")),
                     error = function(e) "")
    (is.character(path) && nzchar(path) && file.exists(path)) ||
      nzchar(Sys.which(probe[1]))
  }, logical(1))
  names(found) <- vapply(probes, `[`, character(1), 1L)
  if (print) {
    cat("Toolchains:\n")
    for (nm in names(found)) {
      cat(sprintf("  %-7s %s\n", nm, if (found[[nm]]) "found" else "NOT found"))
    }
  }
  invisible(found)
}

# The reporter pair, verbatim from run_fast.R.
#
# `progress_max_fails = Inf` because the default stops the run after ten
# failures, which leaves the rest of the suite unmeasured -- the opposite of
# what a baseline needs. `cli.dynamic = FALSE` keeps in-place line updates out
# of a redirected log.
#
# `update_interval = Inf` when the output is not a terminal: the spinner redraws
# in place, which a redirected log records as one line per redraw -- hundreds
# per file. Off, each file prints exactly one line, with its own elapsed time,
# the moment it finishes. This is the whole of the "lines to a log, live block
# on a TTY" behaviour, and it composes: a module worker's stdout is a file, so
# it takes the one-line branch automatically.
#
# `extra` takes additional reporters (the module heartbeat) without every
# caller rebuilding the MultiReporter.
en_reporters <- function(list_reporter, extra = NULL) {
  options(testthat.progress_max_fails = Inf, cli.dynamic = FALSE)
  reporters <- list(
    testthat::ProgressReporter$new(
      max_failures = Inf,
      update_interval = if (isatty(stdout())) 1 else Inf),
    list_reporter)
  if (!is.null(extra)) reporters <- c(reporters, list(extra))
  testthat::MultiReporter$new(reporters = reporters)
}

# The end-of-run tables and the headline, verbatim from run_fast.R. Returns the
# broken count so the caller can set its exit status.
#
# `error` is counted separately from `failed` by testthat: a file that only
# errors has failed == 0. Aggregating on `failed` alone drops it from the table
# while the headline still counts it -- silence that reads as success. That is
# exactly the bug run_cross.R shipped (its exit code ignored `error`
# altogether) and the reason run_nightly.R's report said 16 where its own
# headline said 22.
en_summarise <- function(res, label, mins, filter = NULL) {
  broken <- sum(res$failed) + sum(res$error)
  if (nrow(res)) {
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
  cat(sprintf("[%s] failed %d | skipped %d | passed %d | %.1f min%s\n",
              label, broken, sum(res$skipped), sum(res$passed), mins,
              if (is.null(filter)) "" else paste0(" | filter: ", filter)))
  invisible(broken)
}

# Where a run's artifacts go. Second resolution plus the pid, because the old
# nightly path was minute-resolution and two runs in one minute overwrote each
# other's report.
en_run_dir <- function(tier, given = NULL, create = TRUE) {
  dir <- if (!is.null(given) && nzchar(given)) {
    given
  } else {
    file.path("tmp", "test-runs",
              sprintf("%s-%s-%d", tier, format(Sys.time(), "%Y%m%d-%H%M%S"),
                      Sys.getpid()))
  }
  if (create) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}
