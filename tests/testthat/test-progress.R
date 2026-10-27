# One live progress bar at a time (R/utils.R).
#
# progressr's with_progress() (1.1.0) listens to a SINGLE progressor: creating
# a second terminates the first, and later updates on the dead one warn with
# "no longer listening to this progressor". A sequence driver loops over units
# and every unit interpolates, which creates an inner progressor -- so the
# driver's own bar died on its first unit and each later unit warned. The
# driver claims the bar and the inner operations report nothing while it holds
# it; the outer bar is the useful one over a long sweep.

test_that("an inner progressor is silent while a driver holds the bar", {
  expect_false(isTRUE(energyRt:::.prg_state$outer))
  expect_s3_class(
    progressr::with_progress(energyRt:::.progressor(steps = 2)), "progressor")

  old <- energyRt:::.prg_claim()
  on.exit(energyRt:::.prg_release(old), add = TRUE)
  expect_true(isTRUE(energyRt:::.prg_state$outer))
  # the inner call yields nothing rather than killing the outer bar
  expect_null(progressr::with_progress(energyRt:::.progressor(steps = 2)))

  energyRt:::.prg_release(old)
  expect_false(isTRUE(energyRt:::.prg_state$outer))
})

test_that("a sampled sweep reports every unit, without warning", {
  skip_if_no_solver()
  skip_if_not_installed("progressr")
  p <- file.path(tempdir(), "prog-suite")
  unlink(p, recursive = TRUE)
  old_sp <- set_scenarios_path(p)
  on.exit(set_scenarios_path(old_sp), add = TRUE)
  withr::local_options(progressr.enable = TRUE)

  cal <- calendars$s4_h24
  leaf <- as.character(cal@timeframes[[length(cal@timeframes)]])
  mod <- newModel("pg", region = "R1", discount = 0, calendar = cal,
                  horizon = newHorizon(2020),
                  repo = newRepository("pgr",
                    newCommodity("ELC", timeframe = "HOUR"),
                    newSupply("SUP", commodity = "ELC",
                              supply = data.frame(region = "R1", cost = 15)),
                    newDemand("DEM", commodity = "ELC",
                              demand = data.frame(region = "R1", year = 2020L,
                                                  timeslice = leaf,
                                                  demand = 2))))
  seen <- character(0)
  run <- function() {
    withCallingHandlers(
      progressr::with_progress(
        suppressMessages(solve_by_sample(
          mod, name = "pg", sample_size = 2, method = "sequential",
          solver = solver_options$glpk, verbose = FALSE))),
      progression = function(c) {
        m <- conditionMessage(c)
        if (length(m) && nzchar(m)) seen <<- c(seen, m)
      })
  }
  # the defect was a WARNING on every unit after the first
  expect_no_warning(res <- run())
  skip_if(isTRUE(res$failed))
  # both units reported, so the bar spans the sweep rather than one unit
  expect_true(all(c("sample s01", "sample s02") %in% seen))
})
