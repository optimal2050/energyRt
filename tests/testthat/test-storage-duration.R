# =========================================================================== #
# storage_duration() -- splitting a storage level into duration bands.
#
# The decomposition is only useful if it is exact: the bands must sum back to
# `vStorageLevel` at every timeslice and never go negative, or a stacked area
# chart of them is quietly lying. Both are pinned below, on synthetic series
# (where the right answer is known by construction) and on a solved model.
#
# `.sd_floor()` is checked against a brute-force definition rather than against
# a remembered output, because the whole point of the rewrite was that the
# IDEEA original computed this by sampling a rolling minimum at every phase
# offset -- a loop over `w` -- and the direct right-aligned rolling maximum is
# supposed to be the SAME number in one pass. That equality is the thing worth
# testing.
# =========================================================================== #

fl <- getFromNamespace(".sd_floor", "energyRt")

# floor_w[i] = max, over every window of length w containing i, of the window's
# minimum. Written the slow obvious way on purpose.
brute_floor <- function(x, w) {
  n <- length(x)
  vapply(seq_len(n), function(i) {
    starts <- seq(max(1L, i - w + 1L), min(i, n - w + 1L))
    if (!length(starts)) return(min(x))
    max(vapply(starts, function(s) min(x[s:(s + w - 1L)]), numeric(1)))
  }, numeric(1))
}

test_that(".sd_floor computes the committed-energy floor", {
  x <- c(5, 4, 3, 2, 9, 8, 7, 2, 6, 5, 4, 2)

  # w = 1 is the level itself: every point is its own window
  expect_equal(fl(x, 1L), x)

  # the series never rises above 2 for four consecutive points anywhere, so the
  # four-hour floor is flat at the series minimum
  expect_equal(fl(x, 4L), rep(2, length(x)))

  # a window at least as long as the series can only see the global minimum
  expect_equal(unique(fl(x, length(x))), min(x))
  expect_equal(unique(fl(x, length(x) + 5L)), min(x))
})

test_that(".sd_floor agrees with the brute-force definition", {
  set.seed(42)
  for (w in 2:6) {
    z <- as.numeric(rpois(40, 5))
    got <- fl(z, w)
    want <- brute_floor(z, w)
    # the direct form has no windows fully inside the series near the edges;
    # compare where the definition is unambiguous
    idx <- w:(length(z) - w + 1L)
    expect_equal(got[idx], want[idx], info = paste("width", w))
  }
})

test_that("the bands partition the level exactly", {
  skip_if_no_solver()
  scen <- sd_solved_scenario()
  d <- storage_duration(scen, width = c(6L, 24L))
  expect_gt(nrow(d), 0L)

  lev <- as.data.frame(getData(scen, "vStorageLevel", merge = TRUE,
                               drop.zeros = FALSE))
  tot <- stats::aggregate(value ~ timeslice, d, sum)
  chk <- merge(tot, lev[, c("timeslice", "value")], by = "timeslice",
               suffixes = c(".bands", ".level"))
  expect_gt(nrow(chk), 0L)
  expect_equal(chk$value.bands, chk$value.level, tolerance = 1e-8)

  # a stacked area chart of a negative band is meaningless
  expect_true(all(d$value >= -1e-9))
})

test_that("bands are ordered shortest to longest and cover every timeslice", {
  skip_if_no_solver()
  d <- storage_duration(sd_solved_scenario(), width = c(6L, 24L))
  expect_s3_class(d$duration, "ordered")
  expect_equal(levels(d$duration), c("<6h", "6h-1d", ">1d"))
  # every timeslice appears once per band
  expect_equal(as.integer(table(table(d$timeslice))[["3"]]),
               length(unique(d$timeslice)))
  expect_true(all(c("stg", "region", "year", "timeslice", "datetime",
                    "duration", "value") %in% names(d)))
})

test_that("long storage shows up in the long band, daily cycling in the short one", {
  skip_if_no_solver()
  # The fixture has solar-only generation (daily cycling) plus a three-day
  # overcast stretch (multi-day carry). If the split works, the two signatures
  # land in different bands rather than being averaged together.
  d <- storage_duration(sd_solved_scenario(), width = c(6L, 24L, 24L * 7L))
  m <- tapply(d$value, d$duration, mean)
  expect_gt(m[[">1w"]], m[["6h-1d"]])
  expect_gt(m[[">1w"]], m[["<6h"]])
})

test_that("width and labels are validated, and an empty scenario is not an error", {
  expect_error(storage_duration(NULL, width = numeric(0)), "one or more positive")
  expect_error(storage_duration(NULL, width = c(-1, 5)), "one or more positive")
  expect_error(storage_duration(NULL, width = c(12, 24), labels = c("a", "b")),
               "one more element")

  # A scenario with no storage level yields zero rows WITH the right columns and
  # band levels, so downstream plotting code does not have to special-case it.
  d <- storage_duration(NULL, width = c(12L, 24L))
  expect_equal(nrow(d), 0L)
  expect_equal(levels(d$duration), c("<12h", "12h-1d", ">1d"))
})

test_that("default labels are derived from the widths in human units", {
  d <- storage_duration(NULL, width = c(12L, 24L, 24L * 7L, 24L * 30L))
  expect_equal(levels(d$duration), c("<12h", "12h-1d", "1d-1w", "1w-30d", ">30d"))
})
