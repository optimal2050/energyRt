# Rich report visuals: branding helpers, page breaks, map wrappers, and the
# comprehensive scenario template. Pandoc-free parts first; render/map tests
# are gated on their toolchains.

skip_if_no_geoscales <- function() skip_if_not_installed("geoscales")

.rv_png <- function() {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 60, height = 60)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  grDevices::dev.off()
  f
}

# --------------------------------------------------------------------------- #
# report_img_row / report_pagebreak (pandoc-free)
# --------------------------------------------------------------------------- #

test_that("report_img_row renders a row per output and drops missing", {
  p1 <- .rv_png(); p2 <- .rv_png(); p3 <- .rv_png()
  h <- paste(capture.output(
    report_img_row(c(p1, p2, p3), frac_total = 0.9, output = "html")),
    collapse = "\n")
  expect_match(h, "display:flex")
  expect_equal(lengths(regmatches(h, gregexpr("<img ", h))), 3L)

  w <- paste(capture.output(
    report_img_row(c(p1, p2), output = "word")), collapse = "\n")
  expect_equal(lengths(regmatches(w, gregexpr("!\\[\\]\\(", w))), 2L)
  expect_match(w, "\\{width=49%\\}")            # (1 - 0.02) / 2

  l <- paste(capture.output(
    report_img_row(c(p1, p2), output = "latex")), collapse = "\n")
  expect_match(l, "includegraphics")
  expect_match(l, "hfill")

  # missing files dropped; all-missing emits nothing
  m <- capture.output(report_img_row(c(p1, "no/such.png"), output = "html"))
  expect_equal(lengths(regmatches(paste(m, collapse = ""),
                                  gregexpr("<img ", paste(m, collapse = "")))),
               1L)
  expect_equal(capture.output(report_img_row("no/such.png", output = "html")),
               character(0))
  # a lone image is capped at half the row
  s <- paste(capture.output(
    report_img_row(p1, frac_total = 0.9, output = "word")), collapse = "\n")
  expect_match(s, "\\{width=45%\\}")
})

test_that("report_pagebreak emits the right break per output", {
  h <- paste(capture.output(report_pagebreak("html")), collapse = "\n")
  expect_match(h, "page-break-after: always")
  l <- paste(capture.output(report_pagebreak("latex")), collapse = "\n")
  expect_match(l, "\\\\newpage")
  w <- capture.output(report_pagebreak("word"))
  expect_true(any(w == "```{=openxml}"))       # fence at column 0, own line
  expect_true(any(grepl("w:br w:type=\"page\"", w)))
  expect_true(any(w == "```"))
})

# --------------------------------------------------------------------------- #
# branding collectors (pandoc-free)
# --------------------------------------------------------------------------- #

test_that("branding collectors: misc vectors, order, overrides", {
  l1 <- .rv_png(); l2 <- .rv_png(); l3 <- .rv_png(); fig <- .rv_png()
  mod <- newModel("BRAND", region = "R1")
  mod@misc$logos <- c(l1, l2)
  mod@misc$figure <- fig
  expect_equal(basename(energyRt:::.container_logo_files(mod)),
               basename(c(l1, l2)))
  expect_equal(basename(energyRt:::.container_figure_file(mod)),
               basename(fig))
  # scalar back-compat: misc$logo only
  mod2 <- newModel("BRAND2", region = "R1")
  mod2@misc$logo <- l3
  expect_equal(basename(energyRt:::.container_logo_files(mod2)),
               basename(l3))
  # override replaces and validates
  expect_equal(basename(energyRt:::.container_logo_files(mod,
                                                         override = l3)),
               basename(l3))
  expect_error(energyRt:::.container_logo_files(mod, override = "nope.png"),
               "not found")
  # scenario: model logos first, then its own
  scen <- new("scenario")
  scen@name <- "s"
  scen@model <- mod
  scen@misc$logos <- l3
  expect_equal(basename(energyRt:::.container_logo_files(scen)),
               basename(c(l1, l2, l3)))
  # none anywhere -> character(0) / NULL
  bare <- newModel("BARE", region = "R1")
  expect_length(energyRt:::.container_logo_files(bare), 0)
  expect_null(energyRt:::.container_figure_file(bare))
})

test_that(".report_call_params dispatches by closure arity", {
  f0 <- function() list(a = 1)
  f1 <- function(declared = NULL) list(got = declared)
  expect_equal(energyRt:::.report_call_params(list(x = 1)), list(x = 1))
  expect_equal(energyRt:::.report_call_params(f0), list(a = 1))
  expect_equal(energyRt:::.report_call_params(f1, c("p", "q")),
               list(got = c("p", "q")))
  expect_equal(energyRt:::.report_call_params(f1, NULL), list(got = NULL))
})

# --------------------------------------------------------------------------- #
# plot_map(name =) / facet and plot_geoscale (geo-toolchain gated)
# --------------------------------------------------------------------------- #

.rv_geo_solved <- local({
  sol <- NULL
  function() {
    if (is.null(sol)) {
      regs <- c("R1", "R2")
      mod <- setGeoscale(vt_model(name = "rvgeo", regions = regs),
                        utopia_geoscale(region = regs))
      sol <<- vt_solve(vt_interp(mod, "rvgeo"))
    }
    sol
  }
})

test_that("plot_map(name=) maps any region-dim variable", {
  skip_if_no_geoscales()
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  skip_if_no_solver()
  sol <- .rv_geo_solved()
  p <- plot_map(sol, name = "vTechCap")
  expect_s3_class(p, "ggplot")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSf" %in% geoms)
  expect_equal(p$labels$title, "vTechCap")
})

test_that("plot_map(facet='year') panels the milestone years", {
  skip_if_no_geoscales()
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  skip_if_no_solver()
  sol <- .rv_geo_solved()
  p <- plot_map(sol, name = "vTechNewCap", facet = "year")
  expect_s3_class(p, "ggplot")
  expect_s3_class(p$facet, "FacetWrap")
  # the plot must BUILD: faceting fails at build time when a layer lacks
  # the facet variable, so a returned object alone proves nothing. (The
  # fixture may put new capacity in a single year -- one panel is legal.)
  b <- ggplot2::ggplot_build(p)
  expect_gte(length(unique(b$layout$layout$PANEL)), 1L)
})

test_that("plot_map(name=) refuses region-less variables", {
  skip_if_no_geoscales()
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  # no solve needed: the guard fires before any data access
  expect_error(plot_map(new("scenario"), name = "vTradeStockCap"),
               "region dimension")
})

# --------------------------------------------------------------------------- #
# nested render: report() called from inside a knitr chunk
# --------------------------------------------------------------------------- #

test_that("report() renders inside a knit whose document uses 'setup'", {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available("2.0"), "pandoc >= 2.0")
  # The failure mode this pins: the OUTER document's chunk labels stay in
  # knitr::knit_code during the nested render, so a template chunk whose
  # label collides ('setup' before the en- prefixes) aborted the render
  # with "Duplicate chunk label".
  outer <- tempfile(fileext = ".Rmd")
  out_html <- tempfile(fileext = ".html")
  writeLines(c(
    "---",
    "title: nested",
    "---",
    "```{r setup, include=FALSE}",
    "library(energyRt)",
    "```",
    "```{r report-tech}",
    "tech <- newTechnology('NEST', input = list(comm = 'GAS'),",
    "  output = list(comm = 'ELC'),",
    "  ceff = data.frame(comm = 'GAS', cinp2use = 0.5),",
    "  invcost = data.frame(invcost = 800),",
    "  vintage = data.frame(olife = 20L), cap2act = 8.76)",
    "f <- report(tech, format = 'html', file = tempfile('nested_rep_'),",
    "            open = FALSE, levcost = FALSE)",
    "basename(f)",
    "```",
    "```{r after}",
    "1 + 1                       # the outer knit must continue normally",
    "```"), outer)
  expect_no_error(suppressMessages(suppressWarnings(
    rmarkdown::render(outer, output_file = out_html, quiet = TRUE,
                      envir = new.env(parent = globalenv())))))
  h <- paste(readLines(out_html, warn = FALSE), collapse = "\n")
  expect_match(h, "nested_rep_")               # inner report rendered
  expect_match(h, "\\[1\\] 2")                 # outer chunk after it ran
})

# --------------------------------------------------------------------------- #
# template renders (pandoc + toolchain gated)
# --------------------------------------------------------------------------- #

.rv_skip_if_no_pandoc <- function() {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available("2.0"), "pandoc >= 2.0")
}

test_that("the full scenario template renders its pages", {
  .rv_skip_if_no_pandoc()
  skip_if_no_solver()
  skip_if_no_geoscales()
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  regs <- c("R1", "R2")
  mod <- setGeoscale(vt_model(name = "rvfull", regions = regs),
                     utopia_geoscale(region = regs))
  mod@misc$logos <- c(.rv_png(), .rv_png())
  sol <- vt_solve(vt_interp(mod, "rvfull"))
  sol <- suppressMessages(solve_scenario(sol, solver = "glpk", run = "alt",
                                         force = TRUE))
  sol@misc$badges <- .rv_png()
  expect_gt(nrow(scenario_runs(sol)), 1)     # the runs_note precondition
  f <- suppressMessages(suppressWarnings(report(
    sol, template = "full", format = "html",
    file = tempfile("rv_full_"), open = FALSE)))
  expect_true(file.exists(f))
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(h, "page-break-after: always")
  expect_match(h, "Time structure")
  expect_match(h, "Geography")
  expect_match(h, "Capacity, retirements, and flows")
  expect_match(h, "Costs and checks")
  # pandoc reflows long lines: collapse whitespace before matching the note
  h1 <- gsub("\\s+", " ", h)
  expect_match(h1, "compare_scenarios\\(scen, runs = c\\(")  # runs_note
  # branding: the logo row and badge row rendered
  expect_match(h, "display:flex")
  expect_false(grepl(">NA</td>", h, fixed = TRUE))
})

test_that("the full template degrades without a geoscale", {
  .rv_skip_if_no_pandoc()
  skip_if_no_solver()
  regs <- "R1"
  sol <- vt_solve(vt_interp(vt_model(name = "rvdeg", regions = regs),
                            "rvdeg"))
  f <- suppressMessages(suppressWarnings(report(
    sol, template = "full", format = "html",
    file = tempfile("rv_deg_"), open = FALSE)))
  expect_true(file.exists(f))
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_false(grepl("## Geography", h, fixed = TRUE))
  expect_match(h, "Time structure")        # calendar/horizon need no geoscale
})

test_that("model report renders logos row and half-page figure", {
  .rv_skip_if_no_pandoc()
  mod <- vt_model(name = "rvbrand", regions = "R1")
  mod@misc$logos <- c(.rv_png(), .rv_png())
  mod@misc$figure <- .rv_png()
  f <- suppressMessages(suppressWarnings(report(
    mod, format = "html", file = tempfile("rv_brand_"), open = FALSE)))
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(h, "display:flex")
  expect_match(h, "width:50%")             # the half-page figure
})

test_that("docx page break lands in word/document.xml", {
  .rv_skip_if_no_pandoc()
  skip_if_no_solver()
  sol <- vt_solve(vt_interp(vt_model(name = "rvdocx", regions = "R1"),
                            "rvdocx"))
  f <- suppressMessages(suppressWarnings(report(
    sol, template = "full", format = "docx",
    file = tempfile("rv_docx_"), open = FALSE)))
  expect_true(file.exists(f))
  con <- unz(f, "word/document.xml")
  xml <- paste(readLines(con, warn = FALSE, encoding = "UTF-8"),
               collapse = "")
  close(con)
  expect_match(xml, "w:br w:type=\"page\"")
})

test_that("plot_geoscale draws map, icicle, and stack", {
  skip_if_no_geoscales()
  skip_if_not_installed("ggplot2")
  gs <- utopia_geoscale(region = c("R1", "R2", "R3"))
  pi_ <- plot_geoscale(gs, type = "icicle")
  expect_s3_class(pi_, "ggplot")
  skip_if_not_installed("sf")
  pm <- plot_geoscale(gs, type = "map")
  expect_s3_class(pm, "ggplot")
  fa <- names(formals(geoscales::geoscale_autoplot))
  if (all(c("view", "direction") %in% fa)) {
    ps <- plot_geoscale(gs, type = "stack")
    expect_s3_class(ps, "ggplot")
  } else {
    expect_message(plot_geoscale(gs, type = "stack"), "icicle")
  }
})
