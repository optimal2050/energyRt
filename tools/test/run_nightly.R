# =========================================================================== #
# run_nightly.R -- the deep tier: the FULL suite at tier "nightly" (which
# includes everything the fast and cross tiers run) plus the interpolation
# goldens check, with a markdown report left in tmp/test-reports/.
#   Rscript tools/test/run_nightly.R
# Opt-in extras picked up from the environment (see tests/README.md):
#   ENERGYRT_EXT_MODELS=<dir>   external real-world models (belgium_*.rds)
#   ENERGYRT_TEST_HEAVY=true    full-year external models (~minutes)
#   ENERGYRT_TEST_NEOS=true     NEOS remote-solver tests
# Exits non-zero when any test or the interp guard fails.
#
# Fixed 2026-09-28: the run is no longer silent for its full ~70 minutes, and
# `error` is counted alongside `failed` everywhere. The per-file table used to
# report only `failed`, so a file that ONLY errored appeared with 0 -- which is
# why this report said 16 while its own headline said 22.
# =========================================================================== #
if (!file.exists("DESCRIPTION")) stop("Run from the package root.")
source(file.path("tools", "test", "lib", "runner.R"))
Sys.setenv(ENERGYRT_TEST_TIER = "nightly")
suppressMessages(pkgload::load_all(".", quiet = TRUE))
t0 <- proc.time()[3]
started <- Sys.time()

tools <- en_toolchains()

# --- interpolation goldens -------------------------------------------------- #
source(file.path("tests", "testthat", "helper-goldens.R"))
source(file.path("tools", "test", "interp_guard.R"))   # defines ig_check()
cat("\n== interp goldens ==\n")
ig_ok <- tryCatch(
  isTRUE(withCallingHandlers(
    ig_check(),
    message = function(m) invokeRestart("muffleMessage"))),
  error = function(e) { cat("interp guard ERROR:", conditionMessage(e), "\n"); FALSE })

# --- full suite at tier nightly --------------------------------------------- #
cat("\n== full suite (tier nightly) ==\n")
lst <- testthat::ListReporter$new()
rep <- en_reporters(lst)
devtools::test(reporter = rep)
res <- as.data.frame(lst$get_results())
mins <- (proc.time()[3] - t0) / 60

broken <- en_summarise(res, "nightly", mins)

# --- markdown report -------------------------------------------------------- #
# `broken` per file, so an error-only file is visible. The old table showed
# `failed` alone and therefore undercounted the headline.
res$broken <- res$failed + res$error
agg <- aggregate(cbind(broken, failed, error, skipped, passed, real) ~ file, res,
                 sum)
agg <- agg[order(-agg$broken, -agg$real), , drop = FALSE]

dir.create(file.path("tmp", "test-reports"), recursive = TRUE, showWarnings = FALSE)
# Second resolution plus the pid: the old %H%M stamp meant two runs started in
# the same minute overwrote each other's report.
report <- file.path("tmp", "test-reports",
                    sprintf("nightly-%s-%d.md",
                            format(started, "%Y%m%d-%H%M%S"), Sys.getpid()))
md <- c(
  "# energyRt nightly test report",
  "",
  sprintf("- date: %s", format(started, "%Y-%m-%d %H:%M:%S")),
  sprintf("- version: %s", as.character(utils::packageVersion("energyRt"))),
  sprintf("- interp goldens: %s", if (ig_ok) "PASS" else "**FAIL**"),
  sprintf("- tests: **%d broken** (%d failed + %d errored) / %d passed / %d skipped / %d warnings",
          broken, sum(res$failed), sum(res$error), sum(res$passed),
          sum(res$skipped), sum(res$warning)),
  sprintf("- broken test blocks: %d",
          nrow(unique(res[res$failed > 0 | res$error, c("file", "test"),
                          drop = FALSE]))),
  sprintf("- toolchains: %s",
          paste(sprintf("%s=%s", names(tools),
                        ifelse(tools, "yes", "no")), collapse = " ")),
  sprintf("- ENERGYRT_EXT_MODELS: %s",
          if (nzchar(Sys.getenv("ENERGYRT_EXT_MODELS"))) Sys.getenv("ENERGYRT_EXT_MODELS") else "(unset)"),
  sprintf("- heavy: %s | NEOS: %s",
          Sys.getenv("ENERGYRT_TEST_HEAVY", "false"),
          Sys.getenv("ENERGYRT_TEST_NEOS", "false")),
  sprintf("- wall time: %.1f min", mins),
  "",
  "| file | broken | failed | error | skipped | passed | sec |",
  "|---|---:|---:|---:|---:|---:|---:|",
  sprintf("| %s | %d | %d | %d | %d | %d | %.1f |",
          agg$file, agg$broken, agg$failed, agg$error, agg$skipped, agg$passed,
          agg$real)
)
fails <- unique(res[res$failed > 0 | res$error, c("file", "test"), drop = FALSE])
if (nrow(fails)) {
  md <- c(md, "", sprintf("## Broken blocks (%d)", nrow(fails)), "",
          sprintf("- `%s`: %s", fails$file, fails$test))
}
writeLines(md, report)

cat(sprintf("[nightly] interp %s | report %s\n",
            if (ig_ok) "PASS" else "FAIL", report))
quit(status = if (broken > 0 || !ig_ok) 1L else 0L)
