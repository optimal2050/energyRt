# Comparison layer: the value-diff kernel, compare_inputs, compare_scenarios,
# compare_models, and the comparative report. Kernel and inputs tests are
# solver-free.

# --------------------------------------------------------------------------- #
# fixtures
# --------------------------------------------------------------------------- #

# Small but non-trivial model; `sup_cost` shifts the supply cost so two
# builds differ deterministically in exactly one parameter family.
.cc_model <- function(sup_cost = 8) {
  COA <- newCommodity("COA", unit = "GWh", timeframe = "ANNUAL")
  ELC <- newCommodity("ELC", unit = "GWh", timeframe = "ANNUAL")
  sup <- newSupply("SUP_COA", commodity = "COA", unit = "GWh",
                   supply = data.frame(cost = sup_cost))
  dem <- newDemand("DEM_ELC", commodity = "ELC", unit = "GWh",
                   demand = data.frame(demand = 100))
  pp <- newTechnology(
    "PP_COA", desc = "Coal power plant",
    input = list(comm = "COA", unit = "GWh"),
    output = list(comm = "ELC", unit = "GWh"),
    ceff = data.frame(comm = "COA", cinp2use = 0.4),
    invcost = data.frame(invcost = 1200), fixom = data.frame(fixom = 25),
    vintage = data.frame(olife = 30L), cap2act = 8.76)
  newModel("CCDEMO", desc = "comparison fixture",
           region = "R1", horizon = newHorizon(2025:2030, intervals = 3),
           data = newRepository("cc_repo", COA, ELC, sup, dem, pp))
}

.cc_interp_pair <- local({
  pair <- NULL
  function() {
    if (is.null(pair)) {
      a <- interpolate_model(.cc_model(8), name = "cc_base", verbose = FALSE)
      b <- interpolate_model(.cc_model(9), name = "cc_high", verbose = FALSE)
      pair <<- list(a = a, b = b)
    }
    pair
  }
})

# --------------------------------------------------------------------------- #
# .compare_values kernel
# --------------------------------------------------------------------------- #

test_that(".compare_values: matches, diffs, tolerances", {
  f <- energyRt:::.compare_values
  a <- data.frame(region = c("R1", "R1", "R2"), year = c(2025L, 2030L, 2025L),
                  value = c(10, 20, 30))
  b <- a
  out <- f(a, b)
  expect_setequal(names(out),
                  c("region", "year", "base", "value", "diff", "rel_diff",
                    "differs"))
  expect_false(any(out$differs))

  b$value[2] <- 21
  out <- f(a, b, tol_rel = 1e-6, tol_abs = 1e-6)
  expect_equal(sum(out$differs), 1L)
  bad <- out[out$differs, ]
  expect_equal(bad$base, 20)
  expect_equal(bad$value, 21)
  expect_equal(bad$diff, 1)

  # within relative tolerance -> no diff
  b2 <- a
  b2$value <- a$value * (1 + 1e-9)
  expect_false(any(f(a, b2)$differs))
})

test_that(".compare_values: one-sided rows under both missing modes", {
  f <- energyRt:::.compare_values
  a <- data.frame(k = c("x", "y"), value = c(1, 2))
  b <- data.frame(k = c("x", "z"), value = c(1, 3))
  # fill: absent side counts as 0 and is tolerance-tested
  out <- f(a, b, missing = "fill")
  expect_equal(nrow(out), 3L)
  expect_equal(sum(out$differs), 2L)              # y (2 vs 0), z (0 vs 3)
  # flag: one-sided rows marked, excluded from the tolerance test
  out2 <- f(a, b, missing = "flag")
  expect_setequal(out2$side, c("both", "base", "other"))
  expect_false(any(out2$differs))
  expect_true(is.na(out2$value[out2$side == "base"]))
})

test_that(".compare_values: key mismatch is a structural error", {
  f <- energyRt:::.compare_values
  a <- data.frame(k = "x", value = 1)
  b <- data.frame(j = "x", value = 1)
  expect_error(f(a, b), "key columns differ")
})

# --------------------------------------------------------------------------- #
# compare_inputs
# --------------------------------------------------------------------------- #

test_that("compare_inputs flags the changed parameter and nothing else", {
  pr <- .cc_interp_pair()
  cmp <- compare_inputs(pr$a, pr$b, names = c("BASE", "HIGH"))
  expect_s3_class(cmp, "inputs_cmp")
  sdf <- cmp$summary
  expect_true(nrow(sdf) > 0)
  diff_pars <- sdf$parameter[sdf$status != "identical"]
  expect_true(length(diff_pars) >= 1)
  # the supply cost parameter family is the difference
  expect_true(any(grepl("Cost", diff_pars)),
              info = paste("differing:", paste(diff_pars, collapse = ", ")))
  # an untouched parameter stays identical
  expect_true("pDemand" %in% sdf$parameter[sdf$status == "identical"])
  # user constraints / costs / sets identical here
  expect_length(cmp$equations$cns_only_A, 0)
  expect_length(cmp$sets$only_A, 0)
})

test_that("compare_inputs: self-comparison is all-identical", {
  pr <- .cc_interp_pair()
  cmp <- compare_inputs(pr$a, pr$a)
  expect_true(all(cmp$summary$status == "identical"))
  expect_length(cmp$params$only_A, 0)
  expect_length(cmp$params$only_B, 0)
})

test_that("print.inputs_cmp reports the differing parameter", {
  pr <- .cc_interp_pair()
  cmp <- compare_inputs(pr$a, pr$b, names = c("BASE", "HIGH"))
  txt <- paste(capture.output(print(cmp, max_diffs = 2)), collapse = "\n")
  expect_match(txt, "compare_inputs")
  expect_match(txt, "Differing parameters")
  expect_match(txt, "BASE")
  expect_match(txt, "HIGH")
})

# --------------------------------------------------------------------------- #
# compare_scenarios (glpk-gated)
# --------------------------------------------------------------------------- #

.cc_solved_pair <- local({
  pair <- NULL
  function() {
    if (is.null(pair)) {
      slv <- function(m, nm) {
        s <- interpolate_model(m, name = nm, verbose = FALSE)
        suppressMessages(solve_scenario(s, solver = "glpk"))
      }
      pair <<- list(a = slv(.cc_model(8), "cs_base"),
                    b = slv(.cc_model(12), "cs_high"))
    }
    pair
  }
})

test_that("compare_scenarios: overview, values, deltas, top", {
  skip_if_no_solver()
  pr <- .cc_solved_pair()
  cmp <- compare_scenarios(list(BASE = pr$a, HIGH = pr$b))
  expect_s3_class(cmp, "scenarios_cmp")
  ov <- cmp$overview
  expect_equal(nrow(ov), 2L)
  expect_equal(ov$scenario[1], "BASE")          # base first
  expect_equal(ov$obj_diff[1], 0)
  expect_gt(ov$obj_diff[2], 0)                  # dearer fuel -> higher cost
  # at least one variable differs, with the delta schema
  expect_true(any(cmp$values$status == "value-diff"))
  expect_true(length(cmp$deltas) >= 1)
  d1 <- cmp$deltas[[1]]
  expect_true(all(c("scenario", "base", "value", "diff", "rel_diff",
                    "differs") %in% names(d1)))
  expect_true(!is.null(cmp$top) && nrow(cmp$top) >= 1)
  expect_true(all(c("variable", "scenario", "where", "diff") %in%
                    names(cmp$top)))
})

test_that("compare_scenarios: print and autoplot", {
  skip_if_no_solver()
  pr <- .cc_solved_pair()
  cmp <- compare_scenarios(list(BASE = pr$a, HIGH = pr$b))
  txt <- paste(capture.output(print(cmp)), collapse = "\n")
  expect_match(txt, "scenario comparison \\(base: BASE\\)")
  expect_match(txt, "DIFFER")
  expect_match(txt, "Largest differences")
  skip_if_not_installed("ggplot2")
  expect_s3_class(ggplot2::autoplot(cmp, "objective"), "ggplot")
  expect_s3_class(ggplot2::autoplot(cmp, "generation", position = "dodge"),
                  "ggplot")
  expect_s3_class(ggplot2::autoplot(cmp, "generation"), "ggplot")
  expect_s3_class(ggplot2::autoplot(cmp, "delta"), "ggplot")
})

test_that("compare_scenarios: self-comparison has zero deltas", {
  skip_if_no_solver()
  pr <- .cc_solved_pair()
  cmp <- compare_scenarios(list(A = pr$a, B = pr$a))
  expect_true(all(cmp$values$status %in% c("identical", "missing")))
  expect_equal(cmp$overview$obj_diff[2], 0)
  expect_length(cmp$deltas, 0)
})

test_that("compare_scenarios: input validation", {
  skip_if_no_solver()
  pr <- .cc_solved_pair()
  expect_error(compare_scenarios(pr$a), "named list|runs =")
  expect_error(compare_scenarios(list(pr$a, pr$a)), "unique")
  expect_error(compare_scenarios(pr$a, runs = "no/such"), "unknown run")
  expect_error(compare_scenarios(list(A = pr$a, B = pr$b), base = "C"),
               "must name one of")
})

test_that("compare_scenarios: runs mode on one scenario", {
  skip_if_no_solver()
  s <- interpolate_model(.cc_model(8), name = "cs_runs", verbose = FALSE)
  s <- suppressMessages(solve_scenario(s, solver = "glpk", run = "one"))
  s <- suppressMessages(solve_scenario(s, solver = "glpk", run = "two",
                                       force = TRUE))
  runs <- scenario_runs(s)$run
  expect_true(all(c("one", "two") %in% runs))
  cmp <- compare_scenarios(s, runs = c("one", "two"))
  expect_s3_class(cmp, "scenarios_cmp")
  expect_true(cmp$runs_mode)
  # same problem, same solver: identical solutions
  expect_true(all(cmp$values$status %in% c("identical", "missing")))
  expect_lt(abs(cmp$overview$obj_diff[2]), 1e-6)
})

# --------------------------------------------------------------------------- #
# compare_models (solver-free)
# --------------------------------------------------------------------------- #

test_that("compare_models: identical rebuilds are all-same", {
  cmp <- compare_models(.cc_model(8), .cc_model(8))
  expect_s3_class(cmp, "models_cmp")
  expect_true(all(cmp$inventory$status == "same"))
  expect_false(any(cmp$config$differs))
})

test_that("compare_models: changed and added objects are flagged", {
  a <- .cc_model(8)
  b <- .cc_model(9)                       # supply cost changes SUP_COA
  GAS <- newCommodity("GAS", unit = "GWh", timeframe = "ANNUAL")
  b <- add(b, GAS)                        # and one added commodity
  cmp <- compare_models(a, b, names = c("BASE", "MOD"))
  inv <- cmp$inventory
  expect_equal(inv$status[inv$name == "SUP_COA"], "changed")
  expect_equal(inv$status[inv$name == "GAS"], "added")
  expect_equal(inv$status[inv$name == "PP_COA"], "same")
  # drill-down exists for the changed object and mentions the supply slot
  expect_true(!is.null(cmp$details$SUP_COA))
  txt <- paste(capture.output(print(cmp, name = "SUP_COA")), collapse = "\n")
  expect_match(txt, "SUP_COA")
  # summary print
  out <- paste(capture.output(print(cmp)), collapse = "\n")
  expect_match(out, "BASE vs MOD")
  expect_match(out, "added: 1")
})

test_that("compare_models: config differences are flagged", {
  a <- .cc_model(8)
  b <- .cc_model(8)
  b@config@horizon <- newHorizon(2025:2040, intervals = 4)
  cmp <- compare_models(a, b)
  expect_true(cmp$config$differs[cmp$config$item == "horizon"])
})

test_that("compare_scenarios: inputs = TRUE attaches the input diff", {
  skip_if_no_solver()
  pr <- .cc_solved_pair()
  cmp <- compare_scenarios(list(BASE = pr$a, HIGH = pr$b), inputs = TRUE)
  expect_true(!is.null(cmp$inputs$HIGH))
  expect_s3_class(cmp$inputs$HIGH, "inputs_cmp")
  expect_true(any(cmp$inputs$HIGH$summary$status != "identical"))
})

# --------------------------------------------------------------------------- #
# comparative report (pandoc + glpk gated)
# --------------------------------------------------------------------------- #

.cc_skip_if_no_pandoc <- function() {
  testthat::skip_if_not_installed("rmarkdown")
  testthat::skip_if_not(rmarkdown::pandoc_available("2.0"), "pandoc >= 2.0")
}

test_that("report(list of scenarios) renders the comparative report", {
  .cc_skip_if_no_pandoc()
  skip_if_no_solver()
  pr <- .cc_solved_pair()
  f <- suppressMessages(suppressWarnings(report(
    list(BASE = pr$a, HIGH = pr$b), format = "html",
    file = tempfile("cmp_rep_"), open = FALSE, inputs = TRUE)))
  expect_true(file.exists(f))
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(h, "Scenario comparison")
  expect_match(h, "BASE")
  expect_match(h, "HIGH")
  expect_match(h, "Value differences")
  expect_match(h, "Solution checks")
  expect_match(h, "Input differences")
  expect_false(grepl(">NA</td>", h, fixed = TRUE))
})

test_that("report(scenarios_cmp) renders the same report", {
  .cc_skip_if_no_pandoc()
  skip_if_no_solver()
  pr <- .cc_solved_pair()
  cmp <- compare_scenarios(list(BASE = pr$a, HIGH = pr$b))
  f <- suppressMessages(suppressWarnings(report(
    cmp, format = "html", file = tempfile("cmp_rep2_"), open = FALSE)))
  expect_true(file.exists(f))
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(h, "Scenario comparison")
  expect_match(h, "Largest differences")
})

test_that("report(list) rejects non-scenario lists", {
  expect_error(report(list(1, 2)), "scenario")
})
