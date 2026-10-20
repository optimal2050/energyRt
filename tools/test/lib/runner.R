# =========================================================================== #
# lib/runner.R -- machinery shared by every runner in tools/test/.
#
# Extracted from run_fast.R, which had the only correct implementations: the
# TTY-aware reporter pair and the failed+error aggregation. run_cross.R and
# run_full.R each had their own, both silent and both undercounting, and the
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
# The package's own `en_check_*()` backend checks do the probing, so the runner
# and the tests' guards (helper-solvers.R) agree by construction. Deeper probes
# (`en_check_julia_pkgs()`, which starts a Julia session) are not worth seconds
# per run just to label a report.
en_toolchains <- function(print = TRUE, solve = NULL) {
  checks <- list(glpsol = en_check_glpk, julia = en_check_julia,
                 python = en_check_python, gams = en_check_gams)
  found <- vapply(checks, function(f) {
    isTRUE(tryCatch(f()$found, error = function(e) FALSE))
  }, logical(1))
  if (print) {
    cat("Toolchains:\n")
    for (nm in names(found)) {
      cat(sprintf("  %-7s %s\n", nm, if (found[[nm]]) "found" else "NOT found"))
    }
  }
  probe <- en_backend_probe(solve, print = print)
  invisible(structure(found, probe = probe))
}

# ---------------------------------------------------------------------------- #
# The solve probe, run ONCE per suite run and published for the shard workers.
#
# `en_check_solve()` solves the unit kit U1 (objective 8) on each configured
# backend: the only check that separates "installed" from "can produce a
# number". A backend that is not configured is not probed and is reported
# quietly; one that IS configured and fails is reported loudly, and
# helper-solvers.R then skips its cells naming this probe.
#
# The verdict is written as JSON so the parallel shard workers read it instead
# of paying Julia's ~23 s startup eight times over. Tier `check` (R CMD check /
# CRAN) probes nothing; GAMS needs ENERGYRT_TEST_GAMS=true because it is
# proprietary.
# ---------------------------------------------------------------------------- #

en_probe_file <- function() {
  Sys.getenv("ENERGYRT_TEST_PROBE",
             file.path("tools", "test", ".backend-probe.json"))
}

en_probe_backends_for_tier <- function(
    tier = Sys.getenv("ENERGYRT_TEST_TIER", "fast")) {
  # Tier `check` is what R CMD check / CRAN runs: probe nothing, start nothing.
  if (identical(tier, "check")) return(character(0))
  if (identical(tier, "fast")) {
    # `fast` touches GLPK only; the env var forces the full set anyway
    return(if (identical(Sys.getenv("ENERGYRT_TEST_GAMS"), "true"))
      c("glpk", "gams") else "glpk")
  }
  # cross / full own GAMS cells, so GAMS is probed HERE by default even though
  # `en_check_solve()` leaves it out of its own default set: without a verdict
  # the guard cannot tell a broken install from a working one, and the cells
  # would fail one by one instead of skipping with the reason.
  c("glpk", "julia", "pyomo", "mm", "gams")
}

en_backend_probe <- function(solve = NULL, print = TRUE) {
  backends <- if (is.null(solve) || isTRUE(solve)) {
    en_probe_backends_for_tier()
  } else if (isFALSE(solve)) {
    character(0)
  } else {
    as.character(solve)
  }
  if (length(backends) == 0) return(invisible(NULL))

  tab <- en_check_solve(backends = backends)
  if (print) {
    cat("Backend probe (tiny = unit kit U1 -> 8; full = R1 vs the GLPK answer):\n")
    for (i in seq_len(nrow(tab))) {
      r <- tab[i, ]
      state <- if (isTRUE(r$found)) "OK" else
        if (identical(r$note, "No backend")) "--" else "FAILED"
      cat(sprintf("  %-6s %-5s %-7s %s\n", r$backend, r$level, state, r$note))
    }
    bad <- tab[!tab$found & tab$note != "No backend", , drop = FALSE]
    if (nrow(bad) > 0) {
      cat(sprintf(paste0("  !! %d configured backend/level(s) cannot solve; ",
                         "those tests will SKIP naming this probe.\n"),
                  nrow(bad)))
    }
  }
  en_write_probe(tab)
  invisible(tab)
}

# Publish the verdict for the shard workers. One entry per backend, collapsing
# its levels: `ok` is the verdict the guards act on (every probed level passed),
# with `tiny`/`full` kept so a skip can say WHICH level failed -- "cannot solve
# at all" and "solves only small models" are different diagnoses.
en_write_probe <- function(tab, path = en_probe_file()) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(invisible(NULL))
  lv_ok <- function(d, lv) {
    r <- d[d$level == lv, , drop = FALSE]
    if (nrow(r) == 0) NA else isTRUE(r$found[1])
  }
  bb <- lapply(split(tab, tab$backend), function(d) {
    probed <- !all(d$note == "No backend", na.rm = TRUE)
    bad <- d[!d$found & d$note != "No backend", , drop = FALSE]
    list(ok = probed && nrow(bad) == 0,
         probed = probed,
         tiny = lv_ok(d, "tiny"),
         full = lv_ok(d, "full"),
         failed_level = if (nrow(bad)) bad$level[1] else NA,
         note = if (nrow(bad)) bad$note[1] else
           if (!probed) "No backend" else paste(d$note, collapse = "; "))
  })
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(written_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
         tier = Sys.getenv("ENERGYRT_TEST_TIER", NA_character_),
         backends = bb),
    path, auto_unbox = TRUE)
  # Export an ABSOLUTE path: testthat runs with the working directory at
  # tests/testthat, and a sharded run reads the file from another process.
  Sys.setenv(ENERGYRT_TEST_PROBE = normalizePath(path, winslash = "/"))
  invisible(path)
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
# altogether) and the reason run_full.R's report said 16 where its own
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
# full-tier report path was minute-resolution and two runs in one minute overwrote each
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
