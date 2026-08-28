# Container reports: model / repository (assumptions) and scenario (results).
# Rendering needs pandoc; the scenario tests additionally need glpsol.

skip_if_no_pandoc <- function() {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available("2.0"), "pandoc >= 2.0")
}

# One small but non-trivial model: two commodities, supply, demand, one
# technology, one tax -- enough for every inventory section to have rows.
.rc_model <- function() {
  COA <- newCommodity("COA", unit = "GWh", timeframe = "ANNUAL")
  ELC <- newCommodity("ELC", unit = "GWh", timeframe = "ANNUAL")
  sup <- newSupply("SUP_COA", commodity = "COA", unit = "GWh",
                   supply = data.frame(cost = 8))
  dem <- newDemand("DEM_ELC", commodity = "ELC", unit = "GWh",
                   demand = data.frame(demand = 100))
  pp <- newTechnology(
    "PP_COA", desc = "Coal power plant",
    input = list(comm = "COA", unit = "GWh"),
    output = list(comm = "ELC", unit = "GWh"),
    ceff = data.frame(comm = "COA", cinp2use = 0.4),
    invcost = data.frame(invcost = 1200), fixom = data.frame(fixom = 25),
    vintage = data.frame(olife = 30L), cap2act = 8.76)
  newModel("RCDEMO", desc = "Container-report fixture",
           region = "R1", horizon = newHorizon(2025:2030, intervals = 3),
           data = newRepository("rc_repo", COA, ELC, sup, dem, pp))
}

.rc_solved <- local({
  scen <- NULL
  function() {
    if (is.null(scen)) {
      s <- interpolate_model(.rc_model(), verbose = FALSE)
      s <- solve_scenario(s, solver = "glpk")
      scen <<- s
    }
    scen
  }
})

test_that("model report renders html and docx with the new sections", {
  skip_if_no_pandoc()
  f <- suppressMessages(suppressWarnings(report(
    .rc_model(), format = c("html", "docx"),
    file = tempfile("rc_model_"), open = FALSE)))
  fh <- grep("html$", f, value = TRUE)
  expect_true(file.exists(fh))
  h <- paste(readLines(fh, warn = FALSE), collapse = "\n")
  expect_match(h, "Model report: RCDEMO")
  expect_match(h, "Configuration")
  expect_match(h, "Calendar \\(timeslices per timeframe\\)")
  expect_match(h, "Commodities")
  # per-process sections are grouped by structure since the scale round:
  # the group heading carries the topology and class, the member table the
  # process name
  expect_match(h, "COA -&gt; ELC \\(technology\\)|COA -> ELC \\(technology\\)")
  expect_match(h, "PP_COA")                       # listed as a member
  expect_false(grepl(">NA</td>", h, fixed = TRUE))
  fd <- grep("docx$", f, value = TRUE)
  expect_true(file.exists(fd) && file.size(fd) > 5 * 1024)
})

test_that("repository report shares the model template", {
  skip_if_no_pandoc()
  repo <- newRepository("rc_repo2", newCommodity("ELC", unit = "GWh"))
  f <- suppressMessages(suppressWarnings(report(
    repo, format = "html", file = tempfile("rc_repo_"), open = FALSE)))
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(h, "Repository report: rc_repo2")
  expect_match(h, "Commodities")
})

test_that("scenario report: results, provenance, checks, cost breakdown", {
  skip_if_no_pandoc()
  skip_if_no_solver()
  scen <- .rc_solved()
  f <- suppressMessages(suppressWarnings(report(
    scen, format = c("html", "docx"),
    file = tempfile("rc_scen_"), open = FALSE)))
  fh <- grep("html$", f, value = TRUE)
  h <- paste(readLines(fh, warn = FALSE), collapse = "\n")
  expect_match(h, "Scenario report")
  expect_match(h, "Problem size")
  expect_match(h, "Solution checks")
  expect_match(h, "Runs")                     # scenario_runs() inventory
  expect_match(h, "variable")                 # role-driven cost breakdown
  expect_false(grepl(">NA</td>", h, fixed = TRUE))
  fd <- grep("docx$", f, value = TRUE)
  expect_true(file.exists(fd) && file.size(fd) > 5 * 1024)
})

test_that("scenario report: run= validates and renders a named run", {
  skip_if_no_pandoc()
  skip_if_no_solver()
  scen <- .rc_solved()
  expect_error(report(scen, run = "no/such_run", open = FALSE),
               "unknown run")
  rid <- scenario_runs(scen)$run[1]
  f <- suppressMessages(suppressWarnings(report(
    scen, run = rid, format = "html", file = tempfile("rc_scen_run_"),
    open = FALSE)))
  expect_true(file.exists(f))
})

test_that("containers render a minimal custom template (param filtering)", {
  skip_if_no_pandoc()
  tmpl <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "---",
    "params:",
    "  title: \"x\"",
    "---",
    "`r params$title`"), tmpl)
  fm <- suppressMessages(suppressWarnings(report(
    .rc_model(), template = tmpl, format = "html",
    file = tempfile("rc_custom_m_"), open = FALSE)))
  expect_match(paste(readLines(fm, warn = FALSE), collapse = "\n"),
               "Model report: RCDEMO")
})
