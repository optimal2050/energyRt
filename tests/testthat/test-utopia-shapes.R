# =========================================================================== #
# utopia_profile(): deterministic synthetic shapes (step / sine / cosine /
# hex) on any calendar. Values are pinned EXACTLY where the geometry gives
# exact numbers (equal-share calendars, midpoint sampling).
# =========================================================================== #

# @covers pWeather depth=C forks=default
test_that("step shape is the exact staircase on the unit calendar", {
  p2 <- utopia_profile("step", levels = 2, calendar = "unit_s4")
  expect_identical(p2$timeslice, paste0("S", 1:4))   # chronological
  expect_identical(p2$step, c(0L, 0L, 1L, 1L))
  expect_identical(p2$value, c(0, 0, 1, 1))
  p4 <- utopia_profile("step", levels = 4, calendar = "unit_s4")
  expect_identical(p4$step, 0:3)
  expect_equal(p4$value, (0:3) / 3)
  # min/max scaling
  p <- utopia_profile("step", levels = 2, calendar = "unit_s4",
                      min = 2, max = 6)
  expect_identical(p$value, c(2, 2, 6, 6))
})

# A perfectly EQUAL 96-slice grid for the exactness tests below. The shipped
# `s4_h24` uses real season lengths (unequal shares), which is right for
# models but breaks midpoint-exactness -- geometry tests get their own grid.
.eq96 <- function() {
  make_timetable(list(SEASON = paste0("S", 1:4),
                      HOUR = sprintf("H%02d", 1:24)))
}

test_that("sine/cosine sample slice midpoints; means are exact on equal shares", {
  p <- utopia_profile("sine", calendar = "unit_s4")
  # midpoints x = 1/8, 3/8, 5/8, 7/8 -> (1 + sin(2 pi x)) / 2
  expect_equal(p$value, (1 + sin(2 * pi * c(1, 3, 5, 7) / 8)) / 2)
  # midpoint sums over a full equally-spaced period are exact for the mean
  expect_equal(mean(p$value), 0.5, tolerance = 1e-12)
  ph <- utopia_profile("cosine", calendar = "unit_s4h4")
  expect_equal(mean(ph$value), 0.5, tolerance = 1e-12)
  # ... and only approximate on the real-season-length s4_h24
  pr <- utopia_profile("cosine", calendar = "s4_h24")
  expect_equal(mean(pr$value), 0.5, tolerance = 1e-2)
})

test_that("hex is the equal-thirds trapezoid with mean exactly 2/3", {
  p <- utopia_profile("hex", calendar = .eq96())
  # 1/3 and 2/3 fall on slice boundaries of the equal 96-slice grid, so every
  # slice is linear inside and the midpoint mean is exact
  expect_equal(mean(p$value), 2 / 3, tolerance = 1e-12)
  expect_equal(max(p$value), 1)
  # plateau: the middle third sits at max
  expect_true(all(p$value[33:64] == 1))
})

test_that("per-region variation: phase rotates, amplitude scales", {
  p <- utopia_profile("step", levels = 4, calendar = .eq96(),
                      regions = paste0("R", 1:4), vary = "phase")
  # a rotation never changes the multiset of plateau values
  sums <- tapply(p$step, p$region, sum)
  expect_true(all(sums == sums[[1]]))
  # region 2's offset of 1/4 year delays the staircase by one season:
  # r2[k] = r1[k - 24] (cyclically)
  r1 <- p$value[p$region == "R1"]
  r2 <- p$value[p$region == "R2"]
  expect_equal(r2, c(r1[73:96], r1[1:72]))
  a <- utopia_profile("sine", calendar = "unit_s4",
                      regions = paste0("R", 1:3), vary = "amplitude")
  expect_equal(as.numeric(tapply(a$value, a$region, max) /
                            max(a$value[a$region == "R1"])),
               c(1, 0.75, 0.5))
})

test_that("period = 'frame' repeats the shape inside each parent timeframe", {
  p <- utopia_profile("sine", calendar = "s4_h24", period = "frame")
  expect_equal(p$value[1:24], p$value[25:48])
  expect_equal(p$value[1:24], p$value[73:96])
})

test_that("calendar argument accepts a name, an object, or a timetable", {
  by_name <- utopia_profile("step", levels = 2, calendar = "unit_s4")
  by_obj <- utopia_profile("step", levels = 2,
                           calendar = energyRt::calendars$unit_s4)
  expect_identical(by_name, by_obj)
  tt <- as.data.frame(energyRt::calendars$unit_s4@timetable)
  by_tt <- utopia_profile("step", levels = 2, calendar = tt)
  expect_identical(by_name$value, by_tt$value)
  expect_error(utopia_profile(calendar = "no_such_calendar"),
               "unknown calendar")
  expect_error(utopia_profile("sine", calendar = "unit_s4", vary = "phase"),
               "needs `regions`")
})

test_that("unequal shares position the shape by cumulative share midpoints", {
  # utopia_seasons: DAY/NGT/PK shares are unequal within a season --
  # positions must follow cumulative-share midpoints, not slice counts
  p <- utopia_profile("sine", calendar = "utopia_seasons")
  shr <- as.data.frame(energyRt::calendars$utopia_seasons@timeslice_share)
  sh <- shr$share[match(p$timeslice, shr$timeslice)]
  x <- (cumsum(sh) - sh / 2) / sum(sh)
  expect_equal(p$value, (1 + sin(2 * pi * x)) / 2)
})
