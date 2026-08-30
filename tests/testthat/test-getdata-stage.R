# getData must never silently withhold a stored solution (R/get_data.R
# availability): variables are served whenever they carry rows, with
# @modOut@stage as an ADVISORY warning. Regression for the MIP-incumbent
# case — stage "The solution status is not optimal (-100)" while modOut
# held the full exported solution, and getData returned a mute 0x0 tibble.
# Fixtures from helper-stock-path.R.

gds_path <- function(name) {
  gsub("[\\/]+", "/", file.path(tempdir(), "gds-suite", name))
}

test_that("a not-proven-optimal stage serves the data WITH a warning", {
  skip_if_no_solver()
  sc <- interpolate_model(sp_tech(c(50, 50, 50), optret = FALSE,
                                  name = "gds"),
                          name = "gds", path = gds_path("gds"))
  sol <- solve_scenario(sc, transient = TRUE)
  sol@modOut@stage <- "The solution status is not optimal (-100)"

  # the reporting call shape that surfaced the bug
  expect_warning(
    d <- getData(sol, name = "vTechOut", comm = "ELC", merge = TRUE,
                 process = TRUE, timeframe = "lowest"),
    "not proven optimal")
  expect_gt(nrow(d), 0)
  expect_true("process" %in% names(d))

  expect_warning(d2 <- getData(sol, name = "vObjective", merge = TRUE),
                 "not proven optimal")
  expect_gt(nrow(d2), 0)

  # a parameters-only query touches no variable: stays quiet
  expect_no_warning(getData(sol, name = "pDemand", merge = TRUE))
})

test_that("solved scenarios stay silent; valueless failed solves say why", {
  skip_if_no_solver()
  sc <- interpolate_model(sp_tech(c(40, 40, 40), optret = FALSE,
                                  name = "gds2"),
                          name = "gds2", path = gds_path("gds2"))
  sol <- solve_scenario(sc, transient = TRUE)

  # normal solved path: data, no advisory
  expect_no_warning(d <- getData(sol, name = "vTechOut", merge = TRUE))
  expect_gt(nrow(d), 0)

  # a failed solve that exported nothing: informative message, empty result
  sol2 <- sol
  sol2@modOut@stage <- "Unexpected termination"
  for (v in names(sol2@modOut@variables)) {
    sol2@modOut@variables[[v]]@data <-
      sol2@modOut@variables[[v]]@data[0, ]
  }
  expect_message(
    getData(sol2, name = "vTechOut", merge = TRUE),
    "carries no solution values")
})
