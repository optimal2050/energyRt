# report() smoke tests: template rendering with per-vintage levelized costs.
# Levcost runs analytically (method = "auto", no solver needed), so the only
# external requirements are pandoc (all formats) and LaTeX (pdf only).

skip_if_no_pandoc <- function() {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available("2.0"), "pandoc >= 2.0")
}

.rep_vint_tech <- function() {
  newTechnology(
    "CHPX", desc = "Co-fired CHP unit (test fixture)",
    input = data.frame(comm = c("COA", "BIO"), unit = "GWh", group = "FUEL"),
    output = data.frame(comm = "ELC", unit = "GWh"),
    geff = data.frame(group = "FUEL", ginp2use = 0.42),
    ceff = data.frame(comm = c("COA", "BIO"),
                      share.lo = c(0.2, 0.1), share.up = c(0.9, 0.8)),
    vintage = data.frame(vintage = c("2025", "2035"),
                         start = c(2025L, 2035L), end = c(2034L, NA),
                         olife = c(25L, 30L)),
    invcost = data.frame(vintage = c("2025", "2035"), invcost = c(1200, 900)),
    fixom = data.frame(fixom = 25), varom = data.frame(varom = 1.5),
    af = data.frame(af.up = 0.85),
    units = data.frame(capacity = "GW", activity = "GWh", costs = "MUSD"),
    cap2act = 8.76)
}

.rep_plain_tech <- function() {
  newTechnology(
    "PPX", desc = "Simple gas power plant",
    input = list(comm = "GAS", unit = "GWh"),
    output = list(comm = "ELC", unit = "GWh"),
    ceff = data.frame(comm = "GAS", cinp2use = 0.55),
    invcost = data.frame(invcost = 800), fixom = data.frame(fixom = 20),
    vintage = data.frame(olife = 25L), cap2act = 8.76)
}

.rep_render <- function(tech, template, format, ...) {
  fb <- tempfile(pattern = paste0("rep_", template, "_"))
  f <- suppressMessages(suppressWarnings(
    report(tech, template = template, format = format, file = fb,
           open = FALSE, levcost = TRUE,
           fuel_costs = c(COA = 8, BIO = 20, GAS = 15),
           discount = 0.06, base_year = 2025, verbose = FALSE, ...)))
  f
}

test_that("generic html: vintaged tech gets the by-vintage levcost section", {
  skip_if_no_pandoc()
  f <- .rep_render(.rep_vint_tech(), "generic", "html")
  expect_true(file.exists(f))
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(h, "Levelized Cost per unit of ELC, by vintage")
  expect_match(h, "CHPX_VIN2025")
  expect_match(h, "CHPX_VIN2035")
  expect_match(h, "Vintages")                # the vintages table
  # per-vintage invcost survives (900 used to be silently dropped)
  expect_match(h, "900")
  # no all-NA columns leak into any table
  expect_false(grepl(">NA</td>", h, fixed = TRUE))
})

test_that("generic html: plain tech renders with no vintage section", {
  skip_if_no_pandoc()
  f <- .rep_render(.rep_plain_tech(), "generic", "html")
  expect_true(file.exists(f))
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_false(grepl("by vintage", h, fixed = TRUE))
  expect_match(h, "Levelized Cost per unit of ELC")
})

test_that("generic pdf renders for a vintaged tech", {
  skip_if_no_pandoc()
  skip_if_not(energyRt:::.report_has_latex(), "no LaTeX toolchain")
  f <- .rep_render(.rep_vint_tech(), "generic", "pdf")
  expect_true(file.exists(f))
  expect_gt(file.size(f), 10 * 1024)
})

test_that("generic docx renders as a real Word document", {
  skip_if_no_pandoc()
  f <- .rep_render(.rep_vint_tech(), "generic", "docx")
  expect_true(file.exists(f))
  expect_gt(file.size(f), 5 * 1024)
  # docx is a zip: the by-vintage section and embedded figures must be inside
  xml <- tryCatch({
    con <- unz(f, "word/document.xml")
    on.exit(close(con), add = TRUE)
    paste(readLines(con, warn = FALSE, encoding = "UTF-8"), collapse = "")
  }, error = function(e) "")
  expect_match(xml, "by vintage")
  media <- grep("^word/media/", unzip(f, list = TRUE)$Name, value = TRUE)
  expect_gte(length(media), 2L)
})

test_that("vehicle and summary templates still render (shared fixes)", {
  skip_if_no_pandoc()
  fv <- .rep_render(.rep_vint_tech(), "vehicle", "html")
  expect_true(file.exists(fv))
  h <- paste(readLines(fv, warn = FALSE), collapse = "\n")
  expect_false(grepl(">NA</td>", h, fixed = TRUE))
  expect_match(h, "900")                     # per-vintage invcost shown
  fs <- .rep_render(.rep_vint_tech(), "summary", "html")
  expect_true(file.exists(fs))
})

test_that("report params are filtered to the template's declared set", {
  skip_if_no_pandoc()
  # a minimal custom template lacking every new param must still render
  tmpl <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "---",
    "params:",
    "  name: \"x\"",
    "---",
    "`r params$name`"), tmpl)
  fb <- tempfile(pattern = "rep_custom_")
  f <- suppressMessages(suppressWarnings(
    report(.rep_vint_tech(), template = tmpl, format = "html", file = fb,
           open = FALSE, levcost = FALSE, verbose = FALSE)))
  expect_true(file.exists(f))
  expect_match(paste(readLines(f, warn = FALSE), collapse = "\n"), "CHPX")
})

test_that("levcost report params carry the vintage tables", {
  # no rendering: check the param assembly directly (fast, pandoc-free)
  tech <- .rep_vint_tech()
  lc <- levcost(tech, fuel_costs = c(COA = 8, BIO = 20), discount = 0.06,
                base_year = 2025, verbose = FALSE)
  expect_s3_class(lc, "levcost_variants")
  df <- energyRt:::.report_drop_empty_cols(levcost_by_variant(lc, "npv"))
  expect_true(all(c("variant", "vintage", "levcost_npv") %in% names(df)))
  expect_equal(nrow(df), 2L)
  inst <- energyRt:::.report_pick_instance(lc)
  expect_s3_class(inst, "levcost")
  # newest vintage picked
  expect_match(names(inst$levcost_npv), "VIN2035")
})

test_that(".report_drop_empty_cols prunes columns and rows", {
  f <- energyRt:::.report_drop_empty_cols
  d <- data.frame(a = c(1, NA), b = c(NA, NA), c = c("", ""), d = c("x", NA),
                  stringsAsFactors = FALSE)
  out <- f(d)
  expect_setequal(names(out), c("a", "d"))
  expect_null(f(data.frame(b = c(NA, NA))))
  expect_null(f(NULL))
  expect_null(f(data.frame()))
})
