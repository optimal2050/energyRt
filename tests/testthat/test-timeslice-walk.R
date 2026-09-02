# Sequential, chronological access to a solved scenario (R/timeslice_walk.R).
#
# The properties that would break silently:
#   * the walk must follow the CALENDAR's chronology, not an alphabetical
#     rotation of it -- `.shape_slices()` deliberately anchors at `sort(sl)[1]`,
#     which on s4_h24 starts the year at FAL;
#   * a folded parameter's NA timeslice is a wildcard meaning "every slice", and
#     `%in%` does not match NA, so it must be admitted explicitly;
#   * a table with no timeslice column applies to every slice;
#   * the solver writes only non-zero rows, so a slice with no row is a
#     structural zero and must not end the walk;
#   * the block size is a read-efficiency knob and must not change what comes
#     back.

# @covers vSupOut vDemInp pTimesliceShare depth=S backends=glpk

tw_cal <- function() calendars$s4_h24

tw_leaf <- function() {
  cal <- tw_cal()
  as.character(cal@timeframes[[cal@default_timeframe]])
}

tw_scen <- function(name = "tw") {
  leaf <- tw_leaf()
  mod <- newModel(name, region = "R1", discount = 0, calendar = tw_cal(),
    horizon = newHorizon(2020),
    repo = newRepository("twr",
      newCommodity("ELC", timeframe = "HOUR"),
      newSupply("SUP", commodity = "ELC", region = "R1",
                supply = data.frame(region = "R1", cost = 5)),
      newDemand("DEM", commodity = "ELC", region = "R1",
                demand = data.frame(region = "R1", year = 2020L,
                                    timeslice = leaf, demand = 2))))
  s <- suppressMessages(suppressWarnings(
    solve_scenario(interpolate_model(mod, name = name, overwrite = TRUE),
                   echo = FALSE)))
  suppressMessages(suppressWarnings(save_scenario(s, verbose = FALSE)))
}

tw_local <- function(name, env = parent.frame()) {
  p <- gsub("[\\\\/]+", "/", file.path(tempdir(), "tsl-walk", name))
  unlink(p, recursive = TRUE)
  old <- set_scenarios_path(p)
  do.call(on.exit, list(bquote(set_scenarios_path(.(old))), add = TRUE),
          envir = env)
  invisible(NULL)
}

test_that("the walk follows the calendar's chronology and covers every slice", {
  skip_if_no_solver()
  tw_local("order")
  s <- tw_scen("tw_order")
  leaf <- tw_leaf()

  w <- timeslice_walk(s, vars = "vSupOut")
  expect_s3_class(w, "timeslice_walk")

  seen <- character(0)
  repeat {
    x <- w$next_slice()
    if (is.null(x)) break
    seen <- c(seen, x$timeslice)
  }
  # identical, not setequal: the ORDER is the contract
  expect_identical(seen, leaf)
  # and it starts where the calendar starts, not at sort()[1]
  expect_identical(seen[1], leaf[1])
  expect_false(identical(seen[1], sort(leaf)[1]))
})

test_that("reset replays the walk", {
  skip_if_no_solver()
  tw_local("reset")
  s <- tw_scen("tw_reset")
  w <- timeslice_walk(s, vars = "vSupOut")
  a <- w$next_slice()$timeslice
  w$reset()
  expect_identical(w$next_slice()$timeslice, a)
})

test_that("the block size changes nothing that comes back", {
  skip_if_no_solver()
  tw_local("block")
  s <- tw_scen("tw_block")

  grab <- function(b) {
    unlist(timeslice_walk(s, vars = "vSupOut", block = b,
                          fun = function(x) sum(x$vSupOut$value)))
  }
  expect_equal(grab(1L), grab(24L))
  expect_equal(grab(24L), grab(1000L))
})

test_that("every stored row is visited exactly once", {
  skip_if_no_solver()
  tw_local("cover")
  s <- tw_scen("tw_cover")

  whole <- as.data.frame(getData(s, "vSupOut", merge = TRUE))
  walked <- timeslice_walk(s, vars = "vSupOut",
                           fun = function(x) x$vSupOut)
  walked <- do.call(rbind, walked)
  expect_equal(sum(walked$value), sum(whole$value))
  expect_identical(nrow(walked), nrow(whole))
})

test_that("a table with no timeslice column is attached to every step", {
  skip_if_no_solver()
  tw_local("static")
  s <- tw_scen("tw_static")

  # `vSupCost` is (sup, region, year) -- no timeslice at all, so it applies to
  # every slice and is read once rather than per step.
  res <- timeslice_walk(s, vars = c("vSupOut", "vSupCost"),
                        fun = function(x) x$vSupCost)
  n <- vapply(res, nrow, integer(1))
  expect_true(all(n == n[1]))
  expect_gt(n[[1]], 0)
  # identical rows at every step, not a per-slice subset
  expect_equal(res[[1]], res[[length(res)]])
  expect_false("timeslice" %in% names(res[[1]]))
})

test_that("an NA timeslice is a wildcard and appears at every step", {
  skip_if_no_solver()
  tw_local("wild")
  s <- tw_scen("tw_wild")

  # a folded parameter uses NA to mean "every slice"; `%in%` would drop it
  fake <- data.frame(region = "R1", year = 2020L,
                     timeslice = NA_character_, value = 7)
  src <- list(name = "fake", kind = "parameter", query = fake,
              cols = names(fake), has_ts = TRUE)
  blk <- utils::head(tw_leaf(), 3)
  got <- energyRt:::.tw_collect_block(src, blk)
  expect_identical(nrow(got), 1L)
  expect_true(is.na(got$timeslice))
})

test_that("a slice with no rows is a zero, not the end of the walk", {
  skip_if_no_solver()
  tw_local("sparse")
  s <- tw_scen("tw_sparse")
  leaf <- tw_leaf()

  # ask for a variable that exists but is empty for most keys by filtering to a
  # region the data does not have: every step must still be visited
  n <- 0L
  w <- timeslice_walk(s, vars = "vSupOut", filter = list(region = "NOWHERE"))
  repeat {
    x <- w$next_slice()
    if (is.null(x)) break
    n <- n + 1L
    expect_identical(nrow(x$vSupOut), 0L)
  }
  expect_identical(n, length(leaf))
})

test_that("filters are honoured and unknown slices are refused", {
  skip_if_no_solver()
  tw_local("filter")
  s <- tw_scen("tw_filter")

  keep <- utils::head(tw_leaf(), 5)
  w <- timeslice_walk(s, vars = "vSupOut", slices = keep)
  expect_identical(length(w$slices), 5L)

  expect_error(timeslice_walk(s, vars = "vSupOut", slices = "NOT_A_SLICE"),
               "Not timeslices")
  expect_error(timeslice_walk(s), "Nothing to walk")
})
