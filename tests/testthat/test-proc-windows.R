# =========================================================================== #
# Process investment / operating windows.
#
# `.proc_windows()` reads `@vintage` for the investment window and
# `@capacity$stock` for exogenous capacity. Getting only the first meant a
# process whose window closes before the horizon -- `end = year - 1`, the way a
# converted model says "not investable" -- produced build_start > build_end and
# was drawn backwards, while its stock was not drawn at all.
# =========================================================================== #

pw_horizon <- function() newHorizon(2020:2050, intervals = 5)

pw_model <- function() {
  h <- pw_horizon()
  hs <- min(h@intervals$mid)
  newModel(
    name = "pw", desc = "", region = "R1", horizon = h,
    repo = newRepository(
      "pw_repo",
      # exogenous: window shut before the horizon, capacity given as stock
      newTechnology("T_STOCK", input = list(comm = "C"),
                    output = list(comm = "E"),
                    capacity = data.frame(region = "R1", year = c(2020, 2030),
                                          stock = c(10, 8)),
                    vintage = data.frame(end = hs - 1L), cap2act = 1),
      # investable with a finite life
      newTechnology("T_30", input = list(comm = "C"), output = list(comm = "E"),
                    invcost = data.frame(invcost = 100),
                    vintage = data.frame(olife = 30), cap2act = 1),
      # investable with no life at all -> mTechOlifeInf
      newTechnology("T_INF", input = list(comm = "C"), output = list(comm = "E"),
                    invcost = data.frame(invcost = 100), cap2act = 1)))
}

pw <- function() energyRt:::.proc_windows(pw_model(), horizon = pw_horizon())

test_that("a window that closes before the horizon is empty, never inverted", {
  w <- pw()
  expect_false(any(!is.na(w$build_start) & !is.na(w$build_end) &
                     w$build_end < w$build_start))
  r <- w[w$process == "T_STOCK", ]
  expect_true(is.na(r$build_start))
  expect_true(is.na(r$build_end))
  # and with no investment window there is no new-capacity tail either
  expect_true(is.na(r$oper_end))
})

test_that("exogenous stock is reported over the years it is declared for", {
  r <- pw()[pw()$process == "T_STOCK", ]
  expect_equal(r$stock_start, 2020)
  expect_equal(r$stock_end, 2030)
})

test_that("a process without stock reports none", {
  w <- pw()
  expect_true(all(is.na(w$stock_start[w$process %in% c("T_30", "T_INF")])))
})

test_that("a finite life runs the tail past the last build year", {
  r <- pw()[pw()$process == "T_30", ]
  expect_equal(r$oper_end, r$build_end + 30)
})

test_that("no olife is an infinite life, not a zero one", {
  h <- pw_horizon()
  r <- pw()[pw()$process == "T_INF", ]
  # runs to the end of the horizon rather than collapsing onto the build year
  expect_equal(r$oper_end, max(h@intervals$end))
  expect_gt(r$oper_end, r$build_end)
})

test_that("the plot builds, and every drawn segment runs forwards", {
  skip_if_not_installed("ggplot2")
  p <- plot_process_windows(pw_model())
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  seg <- do.call(rbind, lapply(b$data, function(d) {
    if (!all(c("x", "xend") %in% names(d))) return(NULL)
    d[is.finite(d$x) & is.finite(d$xend), c("x", "xend"), drop = FALSE]
  }))
  expect_gt(nrow(seg), 0)
  expect_true(all(seg$xend >= seg$x))
  # the exogenous process contributes a bar even though it has no vintage window
  expect_true(any(seg$x == 2020 & seg$xend == 2030))
})
