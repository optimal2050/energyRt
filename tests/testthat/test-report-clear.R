# Removing rendered reports (clear_report_cache), and the scenario-level
# derived tiers in the artifacts accounting (R/report_cache.R, R/portability.R).
#
# The properties that would break silently:
#   * `reports/` sits BESIDE runs/, so a run cleaner never sees it -- the
#     `kind` column is what keeps the two apart;
#   * a report is identified by its SIDECAR, because reports/ is a flat folder
#     of files rather than one folder per report;
#   * dry-run is the default, unlike clear_levcost_cache();
#   * a sealed owner is an archive: its reports are part of what was archived.

# Covers the R API clear_report_cache(), scenario_artifacts() and
# drop_solver_outputs(). Deliberately NOT an `@covers` tag: that vocabulary is
# the model coverage matrix (sets, maps, parameters, equations, variables), and
# check_tags() resolves names only against it, so an exported function name can
# never resolve there.

rc_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "report-clear-suite", ...))
}

rc_local <- function(env = parent.frame()) {
  old_sp <- set_scenarios_path(rc_root("scenarios"))
  old_mp <- set_models_path(rc_root("models"))
  old_rf <- set_registry_file(rc_root("reg.csv"))
  old_rp <- set_reports_path(rc_root("reports-tier"))
  expr <- bquote({
    set_scenarios_path(.(old_sp)); set_models_path(.(old_mp))
    set_registry_file(.(old_rf)); set_reports_path(.(old_rp))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

rc_scen <- function(name) {
  mod <- sp_tech(c(100, 100, 100), optret = FALSE, name = paste0(name, "m"))
  sc <- interpolate_model(mod, name = name, path = rc_root("scenarios", name))
  suppressMessages(save_scenario(sc, verbose = FALSE))
}

# Rendered reports, written the way report() writes them: the files plus the
# sidecar that keys them. Rendering for real needs pandoc; the cleaner's
# contract is about the FILES, so they are placed directly.
rc_fake_report <- function(dir, stub, formats = c("html", "pdf")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  base <- file.path(dir, paste0("report_", stub))
  # ~0.2 MB each: sizes are reported in MB to 3 decimals, so a toy file
  # would round to zero and the size assertions would say nothing
  for (f in formats) writeLines(strrep("x", 2e5), paste0(base, ".", f))
  yaml::write_yaml(
    list(kind = "report", version = 1L, key = "deadbeef",
         files = stats::setNames(as.list(paste0(basename(base), ".", formats)),
                                 formats),
         created = "2026-09-09T00:00:00Z"),
    paste0(base, ".report.yml"))
  base
}

test_that("a dry run lists report groups and removes nothing", {
  rc_local()
  s <- rc_scen("rc_dry")
  rc_fake_report(file.path(s@path, "reports"), "a")
  rc_fake_report(file.path(s@path, "reports"), "b", formats = "html")

  out <- suppressMessages(clear_report_cache(s))
  expect_identical(nrow(out), 2L)
  expect_setequal(out$report, c("report_a", "report_b"))
  expect_true(all(out$action == "would_delete"))
  expect_gt(sum(out$size_mb), 0)
  # nothing gone
  expect_true(file.exists(file.path(s@path, "reports", "report_a.html")))
})

test_that("dry_run = FALSE removes the files and their sidecars", {
  rc_local()
  s <- rc_scen("rc_del")
  rc_fake_report(file.path(s@path, "reports"), "a")

  out <- suppressMessages(clear_report_cache(s, dry_run = FALSE))
  expect_true(all(out$action == "deleted"))
  expect_false(file.exists(file.path(s@path, "reports", "report_a.html")))
  expect_false(file.exists(file.path(s@path, "reports", "report_a.report.yml")))
  # the scenario itself is untouched
  expect_true(file.exists(file.path(s@path, "scen.RData")))
})

test_that("a sealed owner refuses", {
  rc_local()
  s <- rc_scen("rc_seal")
  rc_fake_report(file.path(s@path, "reports"), "a")
  suppressMessages(seal_scenario(s))
  expect_error(suppressMessages(clear_report_cache(s, dry_run = FALSE)),
               "sealed")
  expect_true(file.exists(file.path(s@path, "reports", "report_a.html")))
})

test_that("with no object it targets the project tier, still dry-run", {
  rc_local()
  rc_fake_report(get_reports_path(), "loose")
  out <- suppressMessages(clear_report_cache())
  expect_identical(nrow(out), 1L)
  expect_true(all(out$action == "would_delete"))
  expect_true(file.exists(file.path(get_reports_path(), "report_loose.html")))
})

test_that("files no sidecar claims are listed as an unnamed group", {
  rc_local()
  s <- rc_scen("rc_loose")
  d <- file.path(s@path, "reports")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  writeLines("stray", file.path(d, "leftover.html"))
  out <- suppressMessages(clear_report_cache(s))
  expect_true(any(is.na(out$report)))
})

test_that("reports are a scenario-level row, and the run cleaner leaves them", {
  rc_local()
  s <- rc_scen("rc_kind")
  rc_fake_report(file.path(s@path, "reports"), "a")

  art <- scenario_artifacts(s)
  expect_true("kind" %in% names(art))
  rep_row <- art[art$kind == "reports", ]
  expect_identical(nrow(rep_row), 1L)
  expect_gt(rep_row$scratch_mb, 0)
  expect_true(is.na(rep_row$run))
  expect_true(nzchar(rep_row$suggest))

  # drop_solver_outputs() is a RUN cleaner and must not touch them
  suppressMessages(drop_solver_outputs(s, dry_run = FALSE))
  expect_true(file.exists(file.path(s@path, "reports", "report_a.html")))
})

test_that("prepare_for_sharing drops reports by default, keeps them on request", {
  rc_local()
  s <- rc_scen("rc_share")
  rc_fake_report(file.path(s@path, "reports"), "a")

  out1 <- rc_root("shared-default")
  suppressMessages(prepare_for_sharing(s, path = out1, verbose = FALSE))
  expect_false(dir.exists(file.path(out1, "reports")))

  out2 <- rc_root("shared-kept")
  suppressMessages(prepare_for_sharing(s, path = out2, keep_reports = TRUE,
                                       verbose = FALSE))
  expect_true(file.exists(file.path(out2, "reports", "report_a.html")))
})
