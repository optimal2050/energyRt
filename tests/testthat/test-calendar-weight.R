# =========================================================================== #
# `calendar@timetable$weight` -- the validity rule.
#
# `weight` annualises a sampled calendar: whatever fraction of the year the
# timetable covers, `sum(share * weight)` is 1, and the leaf weight is
# `1 / year_fraction`. It reaches the model as `pTimesliceWeight` and, as a
# ratio of parent to child, as `pTimesliceAgg`.
#
# The hole this pins: `.check_timetable()` dropped the column before validating
# anything, so a user- or `timescales`-supplied weight was never checked --
# while `.complete_calendar()` renormalises the weights it derives
# (`wh / sum(wh * sh)`), silently absorbing an inconsistent value, and a zero
# or NA weight propagated to `@timeslice_share` as Inf / NaN instead of an
# error.
#
# Ruling (2026-11-07): a non-annualising sum WARNS and the rescale still
# happens. Declared weights are never dropped -- interpolation rescales them
# and says so, which is all the user needs; a representative-period calendar
# supplies weights that cannot be derived from `share` at all. Only
# non-finite / non-positive weights are refused.
#
# @covers .check_timetable .complete_calendar
# =========================================================================== #

cw_tt <- function(n = 4, year_fraction = 1) {
  make_timetable(list(ANNUAL = "ANNUAL", DAY = paste0("d", seq_len(n))),
                 year_fraction = year_fraction)
}

test_that("a consistent weight passes, present or absent", {
  # the `make_timetable()` path validates BEFORE the column is filled
  expect_s4_class(newCalendar(timetable = cw_tt()), "calendar")

  tt <- cw_tt()
  expect_true("weight" %in% names(tt))
  expect_equal(sum(tt$share * tt$weight), 1, tolerance = 1e-9)
  expect_silent(energyRt:::.check_timetable(tt, year_fraction = 1))

  # no weight column at all is legal
  expect_silent(energyRt:::.check_timetable(
    dplyr::select(tt, -"weight"), year_fraction = 1))

  # a sample: shares sum to year_fraction, weight is its reciprocal
  tt5 <- cw_tt(year_fraction = 0.5)
  expect_equal(sum(tt5$share), 0.5, tolerance = 1e-9)
  expect_equal(unique(tt5$weight), 2)
  expect_silent(energyRt:::.check_timetable(tt5, year_fraction = 0.5))

  # non-uniform weights are allowed as long as they annualise
  ttn <- cw_tt()
  ttn$share <- c(0.4, 0.3, 0.2, 0.1)
  ttn$weight <- c(0.5, 1, 1.5, 2)
  expect_equal(sum(ttn$share * ttn$weight), 1, tolerance = 1e-9)
  expect_silent(energyRt:::.check_timetable(ttn, year_fraction = 1))
})

test_that("a non-finite, zero or negative weight is refused", {
  for (bad in list(NA_real_, 0, -1, Inf)) {
    tt <- cw_tt()
    tt$weight[2] <- bad
    expect_error(energyRt:::.check_timetable(tt, year_fraction = 1),
                 "finite and positive",
                 label = paste0("weight = ", bad))
  }
  # the message names the offending timeslice, not the row number
  tt <- cw_tt()
  tt$weight[3] <- 0
  expect_error(energyRt:::.check_timetable(tt, year_fraction = 1), tt$timeslice[3])

  tt <- cw_tt()
  tt$weight <- as.character(tt$weight)
  expect_error(energyRt:::.check_timetable(tt, year_fraction = 1),
               "must be numeric")
})

test_that("a weight that does not annualise the shares warns", {
  # the sampled calendar's own trap: half a year still carrying the full
  # year's weight, as slicing a timetable leaves it
  tt <- cw_tt(year_fraction = 0.5)
  tt$weight <- 1
  expect_warning(energyRt:::.check_timetable(tt, year_fraction = 0.5),
                 "share \\* weight")

  # and the reverse: a full year weighted as though it were a sample
  tt <- cw_tt()
  tt$weight <- 2
  expect_warning(energyRt:::.check_timetable(tt, year_fraction = 1),
                 "share \\* weight")

  # the message states the behaviour -- normalised, by that factor
  tt <- cw_tt(year_fraction = 0.5)
  tt$weight <- 1
  expect_warning(energyRt:::.check_timetable(tt, year_fraction = 0.5),
                 "normalised \\(divided by 0.5\\)")
  expect_warning(energyRt:::.check_timetable(tt, year_fraction = 0.5),
                 "annualise to a full year")
})

test_that("newCalendar() warns and still builds the right calendar", {
  tt <- cw_tt(year_fraction = 0.5)
  tt$weight <- 1 # stale: inherited from the full-year table
  expect_warning(cal <- newCalendar(timetable = tt, year_fraction = 0.5),
                 "share \\* weight")
  ss <- as.data.frame(cal@timeslice_share)
  # the rescale is what makes the sample annualise: leaf 1/year_fraction,
  # top slice full-year magnitude
  expect_equal(ss$weight[nrow(ss)], 2, tolerance = 1e-9)
  expect_equal(ss$share[1], 1, tolerance = 1e-9)
  expect_equal(ss$weight[1], 1, tolerance = 1e-9)

  # declared consistently, the same calendar comes out without a warning
  tt$weight <- 2
  expect_silent(cal2 <- newCalendar(timetable = tt, year_fraction = 0.5))
  expect_equal(as.data.frame(cal2@timeslice_share)$weight,
               ss$weight, tolerance = 1e-9)
})

test_that("every shipped calendar satisfies the rule", {
  for (nm in names(calendars)) {
    tt <- calendars[[nm]]@timetable
    skip_if(is.null(tt[["weight"]]))
    expect_equal(sum(tt$share * tt$weight), 1, tolerance = 1e-7,
                 label = paste0(nm, ": sum(share * weight)"))
    expect_silent(energyRt:::.check_timetable(
      tt, year_fraction = calendars[[nm]]@year_fraction))
  }
})
