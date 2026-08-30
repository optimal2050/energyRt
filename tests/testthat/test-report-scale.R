# Report readability at scale: process-topology grouping, chart lumping,
# compact legends, and capped/scrollable tables.

.rs_skip_if_no_pandoc <- function() {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available("2.0"), "pandoc >= 2.0")
}

# A "large" model in miniature: 4 process structures x n regions of
# region-prefixed technologies (the USENSYS pattern), all sharing 4 fuels.
.rs_big_model <- function(n_regions = 3) {
  regs <- paste0("R", seq_len(n_regions))
  fuels <- c(coal = "COA", gas = "GAS", bio = "BIO", oil = "OIL")
  objs <- list(newCommodity("ELC", unit = "GWh", timeframe = "ANNUAL"),
               newDemand("DEM_ELC", commodity = "ELC", unit = "GWh",
                         demand = data.frame(demand = 100)))
  for (f in fuels) {
    objs <- c(objs, list(
      newCommodity(f, unit = "GWh", timeframe = "ANNUAL"),
      # each fuel is supply-limited so the solution must USE several
      # process structures -- the lumping tests need a real mix
      newSupply(paste0("SUP_", f), commodity = f, unit = "GWh",
                supply = data.frame(cost = 5, ava.up = 120))))
  }
  for (r in regs) {
    for (i in seq_along(fuels)) {
      objs <- c(objs, list(newTechnology(
        paste0(r, "_", names(fuels)[i], "_pp"),
        desc = paste(names(fuels)[i], "power plant"),
        input = list(comm = fuels[[i]], unit = "GWh"),
        output = list(comm = "ELC", unit = "GWh"),
        ceff = data.frame(comm = fuels[[i]], cinp2use = 0.35 + 0.05 * i),
        invcost = data.frame(invcost = 800 + 100 * i),
        vintage = data.frame(olife = 30L), cap2act = 8.76)))
    }
  }
  newModel("RSBIG", desc = "scale fixture", region = regs,
           horizon = newHorizon(2025:2030, intervals = 3),
           data = do.call(newRepository, c(list("rs_repo"), objs)))
}

# --------------------------------------------------------------------------- #
# .mix_lump / .legend_compact (pure)
# --------------------------------------------------------------------------- #

test_that(".mix_lump lumps into Other and preserves mass", {
  f <- energyRt:::.mix_lump
  d <- data.frame(
    process = rep(c("a", "b", "c", "d", "dem"), each = 2),
    flow = rep(c(rep("generation", 4), "demand"), each = 2),
    region = rep(c("R1", "R2"), 5),
    year = 2030L,
    value = c(100, 90, 50, 45, 10, 9, 5, 4, 200, 190),
    stringsAsFactors = FALSE)
  out <- f(d, top_n = 2)
  expect_setequal(unique(out$process), c("a", "b", "Other", "dem"))
  # mass preserved per region (incl. the demand overlay untouched)
  for (r in c("R1", "R2")) {
    expect_equal(sum(out$value[out$region == r]),
                 sum(d$value[d$region == r]))
  }
  expect_equal(sum(out$value[out$process == "Other" & out$region == "R1"]),
               10 + 5)
  # identity cases
  expect_identical(f(d, top_n = NULL), d)
  expect_identical(f(d, top_n = Inf), d)
  expect_identical(f(d, top_n = 10), d)
  # collision guard
  d2 <- d
  d2$process[d2$process == "a"] <- "Other"
  out2 <- f(d2, top_n = 2)
  expect_true("Other processes" %in% out2$process)
})

test_that(".mix_lump keeps NA-valued grouping columns", {
  f <- energyRt:::.mix_lump
  d <- data.frame(process = c("a", "b", "c", "d"),
                  flow = "capacity", comm = NA_character_,
                  year = 2030L, value = c(10, 5, 2, 1),
                  stringsAsFactors = FALSE)
  out <- f(d, top_n = 1)
  expect_equal(sum(out$value), sum(d$value))   # NA comm rows not dropped
  expect_equal(out$value[out$process == "Other"], 8)
})

test_that(".legend_compact thresholds", {
  expect_length(energyRt:::.legend_compact(5), 0)
  cc <- energyRt:::.legend_compact(20)
  expect_gt(length(cc), 0)
})

# --------------------------------------------------------------------------- #
# report_tbl caps + scroll
# --------------------------------------------------------------------------- #

test_that("report_tbl scrolls in html and caps elsewhere", {
  d <- data.frame(i = 1:40, v = rnorm(40))
  h <- paste(capture.output(report_tbl(d, output = "html")), collapse = "\n")
  expect_match(h, "class='en-scroll'")
  expect_equal(lengths(regmatches(h, gregexpr("<tr", h))), 41L)  # 40 + head
  h2 <- paste(capture.output(report_tbl(d, output = "html", scroll = FALSE)),
              collapse = "\n")
  expect_false(grepl("en-scroll", h2))
  w <- paste(capture.output(report_tbl(d, output = "word", max_rows = 10)),
             collapse = "\n")
  expect_match(w, "\\.\\.\\. 30 more rows \\(of 40\\)")
  # the non-html safety cap
  big <- data.frame(i = 1:250, v = 1)
  l <- paste(capture.output(report_tbl(big, output = "latex")),
             collapse = "\n")
  expect_match(l, "50 more rows \\(of 250\\)")
  # no footer when nothing dropped
  s <- paste(capture.output(report_tbl(head(d, 5), output = "word")),
             collapse = "\n")
  expect_false(grepl("more rows", s))
})

# --------------------------------------------------------------------------- #
# topology grouping
# --------------------------------------------------------------------------- #

test_that(".proc_groups groups region-replicated processes by structure", {
  mod <- .rs_big_model(3)
  procs <- getObjects(mod, "technology")
  expect_length(procs, 12L)
  grps <- energyRt:::.proc_groups(procs, draw = FALSE)
  expect_length(grps, 4L)                       # 4 structures, not 12
  expect_true(all(vapply(grps, function(g) g$n, integer(1)) == 3L))
  g1 <- grps[[1]]
  expect_length(g1$members, 3L)
  expect_true(g1$representative %in% g1$members)
  expect_true(is.data.frame(g1$ranges_df))
  expect_true("invcost" %in% g1$ranges_df$parameter)
  # identical member values collapse to a single number, not a range
  inv <- g1$ranges_df$range[g1$ranges_df$parameter == "invcost"]
  expect_false(grepl(" - ", inv))
  expect_true(is.data.frame(g1$detail_df) && nrow(g1$detail_df) == 3L)
  expect_match(g1$label, "-> ELC")
})

test_that(".proc_groups renders one schematic per group", {
  mod <- .rs_big_model(2)
  procs <- getObjects(mod, "technology")
  grps <- energyRt:::.proc_groups(procs[1:4], draw = TRUE)
  drawn <- Filter(Negate(is.null),
                  lapply(grps, function(g) g$draw_file))
  expect_gt(length(drawn), 0)
  expect_true(all(file.exists(unlist(drawn))))
})

# --------------------------------------------------------------------------- #
# renders (pandoc-gated)
# --------------------------------------------------------------------------- #

test_that("default model report shows topology groups, not processes", {
  .rs_skip_if_no_pandoc()
  mod <- .rs_big_model(3)
  f <- suppressMessages(suppressWarnings(report(
    mod, format = "html", file = tempfile("rs_model_"), open = FALSE)))
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  n_h3 <- lengths(regmatches(h, gregexpr("<h3", h)))
  expect_lte(n_h3, 6L)                          # ~4 groups, never 12
  expect_match(h, "3 x")                        # member counts in headings
  expect_match(h, "Members")
  expect_match(h, "Parameter ranges across members")
  expect_lt(file.size(f), 5e6)
})

test_that("template='full' resolves the full model template", {
  .rs_skip_if_no_pandoc()
  mod <- .rs_big_model(2)
  f <- suppressMessages(suppressWarnings(report(
    mod, template = "full", format = "html", file = tempfile("rs_full_"),
    open = FALSE)))
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(h, "Per-member parameters")
})

test_that("template='summary' resolves the model summary", {
  .rs_skip_if_no_pandoc()
  mod <- .rs_big_model(2)
  f <- suppressMessages(suppressWarnings(report(
    mod, template = "summary", format = "html",
    file = tempfile("rs_sum_"), open = FALSE)))
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(h, "Model summary|Model report")
  expect_false(grepl("## Processes", h, fixed = TRUE))
  expect_false(grepl("Members", h, fixed = TRUE))
})

# --------------------------------------------------------------------------- #
# solved-scenario side (glpk-gated)
# --------------------------------------------------------------------------- #

.rs_solved <- local({
  sol <- NULL
  function() {
    if (is.null(sol)) {
      s <- interpolate_model(.rs_big_model(2), name = "rs_scen",
                             verbose = FALSE)
      sol <<- suppressMessages(solve_scenario(s, solver = "glpk"))
    }
    sol
  }
})

test_that("getMix(top_n=) lumps without losing mass", {
  skip_if_no_solver()
  sol <- .rs_solved()
  full <- getMix(sol, "generation")
  n_procs <- length(unique(full$process[full$flow != "demand"]))
  expect_gt(n_procs, 1L)                        # fixture sanity
  lumped <- getMix(sol, "generation", top_n = 1)
  expect_true("Other" %in% lumped$process)
  expect_lte(length(unique(lumped$process[lumped$flow != "demand"])), 2L)
  expect_equal(sum(lumped$value), sum(full$value))
  # per year too
  for (y in unique(full$year)) {
    expect_equal(sum(lumped$value[lumped$year == y]),
                 sum(full$value[full$year == y]))
  }
  # top_n >= process count is the identity
  expect_equal(getMix(sol, "generation", top_n = 99), full)
})

test_that("autoplot(scenario) builds with the default lumping", {
  skip_if_no_solver()
  skip_if_not_installed("ggplot2")
  sol <- .rs_solved()
  p <- ggplot2::autoplot(sol, "generation")
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  expect_true(nrow(b$data[[1]]) > 0)
})

test_that("scenario summary template renders its sections", {
  .rs_skip_if_no_pandoc()
  skip_if_no_solver()
  sol <- .rs_solved()
  f <- suppressMessages(suppressWarnings(report(
    sol, template = "summary", format = "html",
    file = tempfile("rs_ssum_"), open = FALSE)))
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(h, "Costs \\(total over the horizon\\)")
  expect_match(h, "TOTAL")
  expect_false(grepl("Solution checks", h, fixed = TRUE))
  expect_false(grepl("Results on the map", h, fixed = TRUE))
})
