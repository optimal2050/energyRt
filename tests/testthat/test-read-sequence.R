# Rebuilding a driver's sequence from the variants on disk (R/read_sequence.R).
#
# getData.myopic(), sample_summary(), myopic_objective() and guided_gap() all
# read the live result's `$scenarios`. Nothing reconstructed it, so once a
# session ended the variants sat on disk and the sequence did not -- the
# results were present and unreachable through the methods written for them.
#
# The test of a faithful rebuild is that those very methods give the same
# answers as they did on the live object.

# Covers the R API: read_sequence(), getData(), myopic_objective(),
# sample_summary(), solved on glpk.
# NOT an `@covers` tag -- see the note in test-getdata-run.R.

rq_root <- function(...) {
  gsub("[\\/]+", "/", file.path(tempdir(), "read-sequence-suite", ...))
}

rq_local <- function(env = parent.frame()) {
  old_sp <- set_scenarios_path(rq_root("scenarios"))
  old_mp <- set_models_path(rq_root("models"))
  old_rf <- set_registry_file(rq_root("reg.csv"))
  expr <- bquote({
    set_scenarios_path(.(old_sp)); set_models_path(.(old_mp))
    set_registry_file(.(old_rf))
  })
  do.call(on.exit, list(expr, add = TRUE), envir = env)
  invisible(NULL)
}

# A sampling fixture of its own: `bs_mod()` lives in test-by-sample.R, and
# test files do not share definitions -- only helper-*.R files do.
rq_mod <- function(name = "rqs", demand = 2) {
  cal <- calendars$s4_h24
  leaf <- as.character(cal@timeframes[[length(cal@timeframes)]])
  newModel(name, region = "R1", discount = 0, calendar = cal,
           horizon = newHorizon(2020),
           repo = newRepository("rqr",
             newCommodity("ELC", timeframe = "HOUR"),
             newSupply("SUP", commodity = "ELC",
                       supply = data.frame(region = "R1", cost = 15)),
             newDemand("DEM", commodity = "ELC",
                       demand = data.frame(region = "R1", year = 2020L,
                                           timeslice = leaf,
                                           demand = demand))))
}

test_that("a myopic sequence rebuilds and answers the same as the live one", {
  skip_if_no_solver()
  rq_local()
  live <- suppressMessages(solve_myopic(my_mod(years = 2020:2022, name = "rq"),
                                        name = "rq", step = 1L, verbose = FALSE))
  obj <- myopic_objective(live)
  dl <- as.data.frame(getData(live, "vTechCap", merge = TRUE))
  p <- live$path

  back <- suppressMessages(load_scenario(p, env = NULL, verbose = FALSE))
  s <- suppressMessages(read_sequence(back))

  expect_s3_class(s, "myopic")
  expect_true(isTRUE(s$reconstructed))
  expect_identical(nrow(s$steps), nrow(live$steps))
  expect_identical(s$steps$step, live$steps$step)
  # the rebuilt object must answer the methods identically -- that is what
  # "faithful" means here, not field-by-field equality
  expect_equal(myopic_objective(s), obj)
  db <- as.data.frame(getData(s, "vTechCap", merge = TRUE))
  expect_identical(nrow(db), nrow(dl))
  expect_equal(sum(db$value), sum(dl$value))
  # `$scenarios` is indexed BY STEP by getData.myopic(), not by position
  expect_identical(length(s$scenarios), max(live$steps$step))
})

test_that("working state is declared missing, not silently NULL", {
  skip_if_no_solver()
  rq_local()
  live <- suppressMessages(solve_myopic(my_mod(years = 2020:2021, name = "rq2"),
                                        name = "rq2", step = 1L, verbose = FALSE))
  back <- suppressMessages(load_scenario(live$path, env = NULL, verbose = FALSE))
  s <- suppressMessages(read_sequence(back))
  # the carry ledger is working state and was never written; a rebuilt
  # sequence is for reading, not for resuming, and has to say so
  expect_true("ledger" %in% s$missing)
  expect_null(s$ledger)
  # what WAS recorded comes back
  expect_identical(s$store, "variants")
  expect_false(is.null(s$horizon))
})

test_that("a by_sample sequence rebuilds and sample_summary agrees", {
  skip_if_no_solver()
  rq_local()
  live <- suppressMessages(solve_by_sample(rq_mod(), name = "rq_bs",
                                           sample_size = 2,
                                           method = "sequential",
                                           verbose = FALSE))
  sl <- sample_summary(live, "vSupOut")
  back <- suppressMessages(load_scenario(live$path, env = NULL, verbose = FALSE))
  s <- suppressMessages(read_sequence(back))

  expect_s3_class(s, "by_sample")
  expect_identical(nrow(s$runs), nrow(live$runs))
  expect_identical(s$method, live$method)
  expect_true("samples" %in% s$missing)   # the timeslice spec is not on disk
  sb <- sample_summary(s, "vSupOut")
  expect_identical(nrow(sb), nrow(sl))
  expect_equal(sb$mean, sl$mean)
})

test_that("a scenario with no variants, or an unknown sequence, says so", {
  skip_if_no_solver()
  rq_local()
  mod <- sp_tech(c(100, 100, 100), optret = FALSE, name = "rq")
  sc <- interpolate_model(mod, name = "rq_plain", path = rq_root("scenarios", "rq_plain"))
  s <- suppressMessages(save_scenario(suppressMessages(solve_scenario(
    sc, solver = solver_options$glpk, echo = FALSE)), verbose = FALSE))
  expect_error(read_sequence(s), "no own-problem variants")

  live <- suppressMessages(solve_myopic(my_mod(years = 2020:2021, name = "rq3"),
                                        name = "rq3", step = 1L, verbose = FALSE))
  back <- suppressMessages(load_scenario(live$path, env = NULL, verbose = FALSE))
  expect_error(read_sequence(back, sequence = "not_a_sequence"), "No driver")
})
