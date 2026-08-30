# Report output locations + skip-if-current (R/report_cache.R): reports of
# saved objects live in the owning folder's reports/; reports of in-memory
# objects land in the temporary get_reports_path() tier. A *.report.yml
# sidecar keys the content; unchanged reports are not re-rendered.

rpc_skip <- function() {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available("2.0"), "pandoc >= 2.0")
}

rpc_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "rpc-suite", ...))
}

rpc_local <- function(env = parent.frame()) {
  old_rp <- set_reports_path(rpc_root("reports"))
  old_mp <- set_models_path(rpc_root("models"))
  old_rf <- set_registry_file(rpc_root("reg.csv"))
  expr <- bquote({
    set_reports_path(.(old_rp))
    set_models_path(.(old_mp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

rpc_tech <- function(invcost = 800) {
  newTechnology(
    "RPX", desc = "Report-cache fixture",
    input = list(comm = "GAS", unit = "GWh"),
    output = list(comm = "ELC", unit = "GWh"),
    ceff = data.frame(comm = "GAS", cinp2use = 0.55),
    invcost = data.frame(invcost = invcost), fixom = data.frame(fixom = 20),
    vintage = data.frame(olife = 25L), cap2act = 8.76)
}

test_that("in-memory tech reports to the temporary tier and skips when current", {
  rpc_skip()
  rpc_local()
  unlink(rpc_root("reports"), recursive = TRUE)

  f1 <- suppressMessages(suppressWarnings(
    report(rpc_tech(), format = "html", open = FALSE)))
  expect_true(startsWith(gsub("[\\/]+", "/", f1), rpc_root("reports")))
  expect_true(file.exists(f1))
  side <- sub("\\.html$", ".report.yml", f1)
  expect_true(file.exists(side))

  # identical rebuild is current: no re-render, mtime untouched
  mt1 <- file.mtime(f1)
  expect_message(
    f2 <- suppressWarnings(report(rpc_tech(), format = "html", open = FALSE)),
    "Report up to date")
  expect_identical(file.mtime(f1), mt1)

  # force re-renders
  suppressWarnings(expect_message(
    report(rpc_tech(), format = "html", open = FALSE, force = TRUE),
    "Report written to"))

  # changed content invalidates
  suppressWarnings(expect_message(
    report(rpc_tech(900), format = "html", open = FALSE),
    "Report written to"))
})

test_that("file= is honored verbatim and still gets skip-if-current", {
  rpc_skip()
  rpc_local()
  fb <- tempfile(pattern = "rpc_explicit_")
  f1 <- suppressMessages(suppressWarnings(
    report(rpc_tech(), format = "html", file = fb, open = FALSE)))
  expect_identical(normalizePath(f1), normalizePath(paste0(fb, ".html")))
  expect_true(file.exists(paste0(fb, ".report.yml")))
  expect_message(
    suppressWarnings(report(rpc_tech(), format = "html", file = fb,
                            open = FALSE)),
    "Report up to date")
})

test_that("a saved model's report lands in its store entry and skips", {
  rpc_skip()
  rpc_local()
  unlink(rpc_root("reports"), recursive = TRUE)
  m <- suppressMessages(save_model(sp_tech(c(100, 100, 100), optret = FALSE,
                                           name = "rpm"), verbose = FALSE))
  f1 <- suppressMessages(suppressWarnings(
    report(m, format = "html", open = FALSE)))
  expect_true(startsWith(gsub("[\\/]+", "/", f1),
                         gsub("[\\/]+", "/", file.path(m@misc$path, "reports"))))
  expect_message(
    suppressWarnings(report(m, format = "html", open = FALSE)),
    "Report up to date")
  # nothing in the temporary tier
  expect_false(dir.exists(rpc_root("reports")))
})
