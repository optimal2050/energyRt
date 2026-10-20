# Element-class report() datasheets: one method body + per-class builders +
# one shipped template per class. Solver-free throughout.

.re_skip_if_no_pandoc <- function() {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available("2.0"), "pandoc >= 2.0")
}

# One tiny fixture per element class (shared calendar; config from a model).
.re_fixtures <- local({
  fx <- NULL
  function() {
    if (is.null(fx)) {
      cal <- calendars[["s4_h24"]]
      hor <- newHorizon(2025:2040, intervals = 3)
      mod <- newModel("REFIX", region = c("R1", "R2"), horizon = hor,
                      discount = 0.05)
      fx <<- list(
        commodity = newCommodity("ELC", unit = "GWh",
          emis = data.frame(comm = "CO2", emis = 0.5, unit = "kt/GWh")),
        supply = newSupply("SUP_COA", commodity = "COA", unit = "GWh",
          supply = data.frame(region = c("R1", "R2"), cost = c(8, 9))),
        demand = newDemand("DEM_ELC", commodity = "ELC", unit = "GWh",
          demand = data.frame(demand = 100)),
        trade = newTrade("TR1", commodity = "ELC",
          routes = data.frame(src = "R1", dst = "R2"),
          invcost = data.frame(invcost = 500)),
        import = newImport("IMP_GAS", commodity = "GAS",
          import = data.frame(price = 12)),
        export = newExport("EXP_ELC", commodity = "ELC",
          export = data.frame(price = 20)),
        weather = newWeather("WSOL", unit = "1",
          weather = data.frame(region = "R1", timeslice = c("d1", "d2"),
                               wval = c(0.3, 0.7))),
        tax = newTax("CTAX", comm = "CO2",
          tax = data.frame(year = c(2030, 2050), bal = c(30, 100))),
        subsidy = newSubsidy("SREN", comm = "ELC",
          subsidy = data.frame(year = 2030, bal = 10)),
        constraint = newConstraint("CAP30", eq = "<=", defVal = 100,
          for.each = data.frame(year = 2030L),
          s1 = list(variable = "vTechNewCap")),
        calendar = cal,
        horizon = hor,
        config = mod@config,
        costs = newCosts("EXTRA", variable = "vTechNewCap"))
    }
    fx
  }
})

test_that("every element class has a report method and builds params", {
  fx <- .re_fixtures()
  for (cls in energyRt:::.REPORT_ELEMENT_CLASSES) {
    expect_true(methods::existsMethod("report", cls), label = cls)
    p <- energyRt:::.element_report_params(fx[[cls]], declared = NULL)
    expect_true(is.list(p) && length(p) > 0, label = cls)
    expect_true(is.data.frame(p$info_df) && nrow(p$info_df) >= 1,
                label = paste(cls, "info_df"))
    # declaration-aware skip: asking only for info_df must not error
    p2 <- energyRt:::.element_report_params(fx[[cls]],
                                            declared = "info_df")
    expect_true(is.list(p2), label = paste(cls, "declared"))
  }
})

test_that("every element datasheet renders to html", {
  .re_skip_if_no_pandoc()
  fx <- .re_fixtures()
  for (cls in energyRt:::.REPORT_ELEMENT_CLASSES) {
    f <- suppressMessages(suppressWarnings(report(
      fx[[cls]], format = "html", file = tempfile(paste0("re_", cls, "_")),
      open = FALSE)))
    expect_true(is.character(f) && file.exists(f), label = cls)
    expect_gt(file.size(f), 10 * 1024, label = cls)
  }
})

test_that("report_templates() lists every element class", {
  tt <- report_templates()
  for (cls in energyRt:::.REPORT_ELEMENT_CLASSES) {
    expect_true(cls %in% tt$class, label = cls)
  }
})

test_that("container name= reaches any element class", {
  .re_skip_if_no_pandoc()
  fx <- .re_fixtures()
  repo <- newRepository("re_repo", fx$commodity, fx$supply, fx$demand,
                        fx$trade, fx$weather, fx$tax)
  mod <- newModel("RECONT", region = c("R1", "R2"),
                  horizon = newHorizon(2025:2040, intervals = 3),
                  data = repo)
  # a supply element by name renders the supply template
  f <- suppressMessages(suppressWarnings(report(
    mod, name = "SUP_COA", format = "html", file = tempfile("re_cont_"),
    open = FALSE)))
  expect_true(file.exists(f))
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(h, "Supply: SUP_COA")
  # unknown name: message + NULL
  expect_message(
    r <- report(mod, name = "NOPE", format = "html", open = FALSE),
    "not found")
  expect_null(r)
})

test_that("container name= honours a class= filter", {
  # containers enforce globally unique names, so cross-class ambiguity
  # cannot be constructed through the public API (the multi-match branch in
  # .report_container is defensive); `class=` still narrows the lookup.
  .re_skip_if_no_pandoc()
  fx <- .re_fixtures()
  repo <- newRepository("re_cls", fx$commodity, fx$demand)
  mod <- newModel("RECLS", region = "R1",
                  horizon = newHorizon(2025:2030, intervals = 2),
                  data = repo)
  f <- suppressMessages(suppressWarnings(report(
    mod, name = "DEM_ELC", class = "demand", format = "html",
    file = tempfile("re_cls_"), open = FALSE)))
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(h, "Demand: DEM_ELC")
  # a class filter that excludes the name behaves as not-found
  expect_message(
    r <- report(mod, name = "DEM_ELC", class = "supply", format = "html",
                open = FALSE),
    "not found")
  expect_null(r)
})

test_that("misc$report sets the default template", {
  .re_skip_if_no_pandoc()
  fx <- .re_fixtures()
  # an element whose misc$report points at a custom template file
  tmpl <- tempfile(fileext = ".Rmd")
  writeLines(c("---", "params:", "  title: \"x\"", "---",
               "CUSTOM-TEMPLATE `r params$title`"), tmpl)
  sup <- fx$supply
  sup@misc$report <- tmpl
  f <- suppressMessages(suppressWarnings(report(
    sup, format = "html", file = tempfile("re_misc_"), open = FALSE)))
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(h, "CUSTOM-TEMPLATE")
  # explicit template beats misc$report
  f2 <- suppressMessages(suppressWarnings(report(
    sup, template = "supply", format = "html",
    file = tempfile("re_misc2_"), open = FALSE)))
  h2 <- paste(readLines(f2, warn = FALSE), collapse = "\n")
  expect_false(grepl("CUSTOM-TEMPLATE", h2, fixed = TRUE))
  expect_match(h2, "Supply: SUP_COA")
})

test_that("supply datasheet renders the region charts", {
  .re_skip_if_no_pandoc()
  sup <- newSupply("SUP_STEP", commodity = "GAS", unit = "GWh",
                   region = c("R1", "R2"),
                   supply = data.frame(region = c("R1", "R2"), year = 2025L,
                                       ava.up = c(100, NA), cost = c(3, 4)))
  f <- suppressMessages(suppressWarnings(report(
    sup, format = "html", file = tempfile("re_sup2_"), open = FALSE)))
  h <- paste(gsub("\\s+", " ", readLines(f, warn = FALSE)), collapse = " ")
  expect_match(h, "Supply by region")
  expect_match(h, "Across regions")
  # the uncapped-bar caption lives inside the PNG; assert it on the builder
  pp <- energyRt:::.el_params_supply(sup, function(p) TRUE)
  expect_match(pp$regions_plot$labels$caption, "unlimited availability")
})

test_that("element reports are skip-if-current cached", {
  .re_skip_if_no_pandoc()
  fx <- .re_fixtures()
  fb <- tempfile("re_cache_")
  f1 <- suppressMessages(suppressWarnings(report(
    fx$commodity, format = "html", file = fb, open = FALSE)))
  expect_message(
    suppressWarnings(report(fx$commodity, format = "html", file = fb,
                            open = FALSE)),
    "up to date")
})