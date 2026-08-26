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
# =========================================================================== #
if (!file.exists("DESCRIPTION")) stop("Run from the package root.")
Sys.setenv(ENERGYRT_TEST_TIER = "nightly")
suppressMessages(pkgload::load_all(".", quiet = TRUE))
t0 <- proc.time()[3]

cat("Toolchains:\n")
for (probe in list(c("glpsol", "glpk"), c("julia", "julia"),
                   c("python", "python"), c("gams", "gams"))) {
  path <- tryCatch(get_option(paste0(probe[2], "_path")), error = function(e) "")
  found <- (is.character(path) && nzchar(path) && file.exists(path)) ||
    nzchar(Sys.which(probe[1]))
  cat(sprintf("  %-7s %s\n", probe[1], if (found) "found" else "NOT found"))
}

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
res <- as.data.frame(devtools::test(reporter = "silent"))
failed <- sum(res$failed) + sum(res$error)
agg <- aggregate(cbind(failed, skipped, passed, real) ~ file, res, sum)
agg <- agg[order(-agg$failed, -agg$real), , drop = FALSE]
bad <- agg[agg$failed > 0, , drop = FALSE]
if (nrow(bad)) print(bad, row.names = FALSE)
mins <- (proc.time()[3] - t0) / 60

# --- markdown report -------------------------------------------------------- #
dir.create(file.path("tmp", "test-reports"), recursive = TRUE, showWarnings = FALSE)
report <- file.path("tmp", "test-reports",
                    format(Sys.time(), "nightly-%Y%m%d-%H%M.md"))
md <- c(
  "# energyRt nightly test report",
  "",
  sprintf("- date: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
  sprintf("- version: %s", as.character(utils::packageVersion("energyRt"))),
  sprintf("- interp goldens: %s", if (ig_ok) "PASS" else "**FAIL**"),
  sprintf("- tests: **%d failed** / %d passed / %d skipped / %d warnings",
          failed, sum(res$passed), sum(res$skipped), sum(res$warning)),
  sprintf("- ENERGYRT_EXT_MODELS: %s",
          if (nzchar(Sys.getenv("ENERGYRT_EXT_MODELS"))) Sys.getenv("ENERGYRT_EXT_MODELS") else "(unset)"),
  sprintf("- heavy: %s | NEOS: %s",
          Sys.getenv("ENERGYRT_TEST_HEAVY", "false"),
          Sys.getenv("ENERGYRT_TEST_NEOS", "false")),
  sprintf("- wall time: %.1f min", mins),
  "",
  "| file | failed | skipped | passed | sec |",
  "|---|---:|---:|---:|---:|",
  sprintf("| %s | %d | %d | %d | %.1f |",
          agg$file, agg$failed, agg$skipped, agg$passed, agg$real)
)
fails <- res[res$failed > 0 | res$error, c("file", "test"), drop = FALSE]
if (nrow(fails)) {
  md <- c(md, "", "## Failures", "",
          sprintf("- `%s`: %s", fails$file, fails$test))
}
writeLines(md, report)

cat(sprintf("\n[nightly] failed %d | skipped %d | passed %d | interp %s | %.1f min\n",
            failed, sum(res$skipped), sum(res$passed),
            if (ig_ok) "PASS" else "FAIL", mins))
cat("report:", report, "\n")
quit(status = if (failed > 0 || !ig_ok) 1L else 0L)
