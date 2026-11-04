# =========================================================================== #
# Recasting a model onto another calendar -- the time mirror of
# test-subset_regions.R.
#
# The ground truths, in the order the implementation depends on them:
#   * a timeslice-indexed value is a RATE, so the weighted annual total is
#     conserved exactly by the aggregation,
#   * rows declared above the source's leaf timeframe are untouched,
#   * a `@timeframe` the target does not have falls back to the finest it has,
#   * the recast model builds and solves, and
#   * the weights -- not the values -- carry the collapsed detail, so a model
#     recast and recast back is the original where the detail was flat.
# =========================================================================== #

skip_if_not_installed("timescales")

# A two-region hourly system: one dispatchable source, one demand with a shape,
# and a fuel declared ANNUAL so there is a row that must NOT move.
mk_hourly <- function(calendar = calendars$s4_h24, regions = c("R1", "R2")) {
  slices <- as.character(calendar@timetable$timeslice)
  dem <- expand.grid(region = regions, year = 2025L, timeslice = slices,
                     stringsAsFactors = FALSE)
  # a shape that is NOT flat within a season, so a mean is not a copy
  dem$demand <- 10 + 5 * sin(seq_len(nrow(dem)))
  repo <- newRepository(
    "rc",
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "HOUR"),
    newSupply("SUP_COA", commodity = "COA",
              supply = data.frame(region = regions, cost = 5)),
    newTechnology("ECOA", input = list(comm = "COA"),
                  output = list(comm = "ELC"),
                  cap2act = 1, invcost = list(invcost = 100),
                  olife = list(olife = 30)),
    newDemand("DEM", commodity = "ELC", demand = dem)
  )
  newModel("RC", data = repo, calendar = calendar, region = regions,
           horizon = newHorizon(2025L, name = "Y2025"), discount = 0.05)
}

# The weighted annual total of one value column: what a rate conserves.
wtot <- function(d, val, calendar) {
  w <- stats::setNames(calendar@timetable$share,
                       as.character(calendar@timetable$timeslice))
  d <- d[as.character(d$timeslice) %in% names(w), , drop = FALSE]
  sum(d[[val]] * w[as.character(d$timeslice)], na.rm = TRUE)
}

test_that("aggregating conserves the weighted annual total", {
  mod <- mk_hourly()
  light <- suppressMessages(recast_model_calendar(mod, calendar = calendars$s4))

  before <- getObject(mod, name = "DEM", drop = TRUE)@demand
  after <- getObject(light, name = "DEM", drop = TRUE)@demand
  expect_equal(wtot(after, "demand", calendars$s4),
               wtot(before, "demand", calendars$s4_h24),
               tolerance = 1e-10)
  # and the shape genuinely collapsed: 4 timeslices per region, not 96
  expect_equal(sort(unique(as.character(after$timeslice))),
               sort(as.character(calendars$s4@timetable$timeslice)))
  expect_lt(nrow(after), nrow(before))
})

test_that("the model carries the target calendar and loses the parent hash", {
  mod <- mk_hourly()
  mod@misc$hash <- "stale"
  light <- suppressMessages(recast_model_calendar(mod, calendar = calendars$s4))
  expect_identical(getCalendar(light)@name, calendars$s4@name)
  expect_null(light@misc$hash)
})

test_that("rows above the source's leaf timeframe are untouched", {
  mod <- mk_hourly()
  light <- suppressMessages(recast_model_calendar(mod, calendar = calendars$s4))
  expect_equal(getObject(light, name = "SUP_COA", drop = TRUE)@supply,
               getObject(mod, name = "SUP_COA", drop = TRUE)@supply)
})

test_that("a dropped @timeframe falls back to the finest one that survives", {
  mod <- mk_hourly()
  expect_identical(getObject(mod, name = "ELC", drop = TRUE)@timeframe, "HOUR")
  light <- suppressMessages(recast_model_calendar(mod, calendar = calendars$s4))
  expect_identical(getObject(light, name = "ELC", drop = TRUE)@timeframe,
                   "SEASON")
  # a timeframe the target keeps is not rewritten
  expect_identical(getObject(light, name = "COA", drop = TRUE)@timeframe,
                   "ANNUAL")
})

test_that("the recast model interpolates and solves", {
  skip_on_cran()
  mod <- mk_hourly()
  light <- suppressMessages(recast_model_calendar(mod, calendar = calendars$s4))
  scen <- suppressMessages(interpolate_model(light, name = "RCLIGHT"))
  # the hourly vocabulary is gone from the solver's set; ANNUAL stays, as the
  # root every calendar carries
  tsl <- as.character(scen@modInp@sets$timeslice)
  expect_false(any(tsl %in% setdiff(
    as.character(calendars$s4_h24@timetable$timeslice),
    as.character(calendars$s4@timetable$timeslice))))
  expect_true(all(as.character(calendars$s4@timetable$timeslice) %in% tsl))
  td <- file.path(tempdir(), "rc_light")
  unlink(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  solved <- suppressMessages(suppressWarnings(
    solve_scenario(scen, solver = solver_options$glpk, tmp.dir = td,
                   tmp.del = TRUE, force = TRUE)))
  expect_true(isTRUE(solved@status$solved))
})

test_that("a flat profile survives a round trip", {
  # with no variation inside a season there is nothing for the mean to lose,
  # so coarsening and re-expanding returns the values it started from
  slices <- as.character(calendars$s4_h24@timetable$timeslice)
  mod <- mk_hourly()
  dem <- getObject(mod, name = "DEM", drop = TRUE)@demand
  dem$demand <- 12
  repo <- newRepository(
    "flat",
    newCommodity("ELC", timeframe = "HOUR"),
    newDemand("DEM", commodity = "ELC", demand = dem)
  )
  m2 <- newModel("FLAT", data = repo, calendar = calendars$s4_h24,
                 region = c("R1", "R2"),
                 horizon = newHorizon(2025L, name = "Y2025"))
  coarse <- suppressMessages(recast_model_calendar(m2, calendar = calendars$s4))
  back <- suppressMessages(recast_model_calendar(coarse,
                                                 calendar = calendars$s4_h24))
  d_back <- getObject(back, name = "DEM", drop = TRUE)@demand
  expect_equal(sort(unique(d_back$demand)), 12)
  expect_setequal(as.character(d_back$timeslice), slices)
})

test_that("rules= overrides the rate default per column", {
  mod <- mk_hourly()
  before <- getObject(mod, name = "DEM", drop = TRUE)@demand
  summed <- suppressMessages(
    recast_model_calendar(mod, calendar = calendars$s4,
                          rules = c(demand = "sum")))
  after <- getObject(summed, name = "DEM", drop = TRUE)@demand
  # summing a rate does not conserve the weighted total -- which is the point
  # of making the override explicit
  expect_gt(wtot(after, "demand", calendars$s4),
            wtot(before, "demand", calendars$s4_h24))
})

test_that("bad input is refused by name", {
  mod <- mk_hourly()
  expect_error(recast_model_calendar(mod, calendar = "s4"), "energyRt calendar")
  expect_error(recast_model_calendar(mod, calendar = calendars$annual),
               "below ANNUAL")
  expect_error(recast_model_calendar(mod, calendar = calendars$s4,
                                     rules = c("sum")),
               "named character vector")
  expect_error(recast_model_calendar(mod, calendar = calendars$s4,
                                     rules = c(demand = "average")),
               "unknown rule")
})

test_that("recasting onto the model's own calendar is a no-op", {
  mod <- mk_hourly()
  expect_message(same <- recast_model_calendar(mod, calendar = calendars$s4_h24),
                 "already carries this calendar")
  expect_equal(getObject(same, name = "DEM", drop = TRUE)@demand,
               getObject(mod, name = "DEM", drop = TRUE)@demand)
})
