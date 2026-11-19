# =========================================================================== #
# `update(config, ...)` forwards every `...` name into the config's, the
# calendar's and the horizon's slots, so a calendar slot name is accepted by
# construction rather than by design. The timetable is the one that must not
# be: seven slots are derived from it by `newCalendar()`, and writing it alone
# left those describing the previous calendar -- and had the new timetable
# validated against that stale structure, which reported an "unknown column".
#
# Ruled 2026-11-19: refuse them. They are outputs of calendar construction.
# `setCalendar(cfg, newCalendar(timetable = ...))` is the route, and is what
# every caller in the package already uses.
# =========================================================================== #

uc_cfg <- function() setCalendar(new("config"), calendars$s4)

uc_tt <- function() make_timetable(struct = list(
  ANNUAL = "ANNUAL", SEASON = c("WIN", "SPR", "SUM", "FAL", "EXT", "XTR")))

test_that("update(config): the timetable is refused, naming the way on", {
  expect_error(update(uc_cfg(), timetable = uc_tt()),
               "derived from the calendar's timetable")
  expect_error(update(uc_cfg(), timetable = uc_tt()), "setCalendar")
})

test_that("update(config): every derived calendar slot is refused", {
  for (sl in energyRt:::.config_derived_calendar_slots) {
    args <- list(uc_cfg())
    args[[sl]] <- uc_tt()
    expect_error(do.call(update, args), "cannot be set here", info = sl)
  }
})

test_that("update(config): the settable slots still update", {
  cfg <- update(uc_cfg(), region = c("R1", "R2"), discount = 0.07)
  expect_setequal(cfg@region, c("R1", "R2"))
  expect_true(all(cfg@discount$wacc == 0.07))
  # and the calendar is untouched by a config-level update
  expect_equal(nrow(cfg@calendar@timetable), nrow(calendars$s4@timetable))
})

test_that("setCalendar is the route, and rebuilds everything", {
  cfg <- setCalendar(uc_cfg(), newCalendar(timetable = uc_tt(), name = "six"))
  ref <- newCalendar(timetable = uc_tt(), name = "six")
  for (sl in setdiff(slotNames("calendar"), c("desc", "misc")))
    expect_equal(methods::slot(cfg@calendar, sl), methods::slot(ref, sl),
                 info = sl)
})

test_that("the conflicting-row guard reads a data.table", {
  # it subset keys with `d[, keys, drop = FALSE]`, which is data.frame
  # indexing: a data.table reads `keys` as a column name in `j` and stops, so
  # a data.table handed to ANY keyed slot died on a data.table error instead of
  # being checked. Keys are read with `[[` now, which also allocates less.
  # the idiom, for the record: `j` as a SYMBOL is looked up as a column name,
  # so the variable's value is never seen (a literal string would have worked)
  keys <- "region"
  expect_error(data.table::data.table(region = "R1")[, keys, drop = FALSE],
               "not found")
  ok  <- data.table::data.table(region = c("R1", "R2"), cost = c(1, 2))
  bad <- data.table::data.table(region = c("R1", "R1"), cost = c(1, 2))
  expect_true(energyRt:::.assert_no_conflicting_rows(ok, "supply", "supply"))
  expect_error(energyRt:::.assert_no_conflicting_rows(bad, "supply", "supply"),
               "same key")
  # and through the constructor, which is how a user meets it
  expect_error(newSupply("S", commodity = "COA", supply = bad), "same key")
  expect_s4_class(newSupply("S", commodity = "COA", supply = ok), "supply")
})

# --------------------------------------------------------------------------- #
# The horizon's twin of the same shape: `intervals` is derived from `period`.
# --------------------------------------------------------------------------- #

test_that("update(horizon): `period` is refused rather than left inconsistent", {
  h <- newHorizon(period = 2025:2034, intervals = c(5, 5))
  expect_error(update(h, period = 2040:2059), "derives `intervals`")
  expect_error(update(h, period = 2040:2059), "newHorizon")
  # it used to write `period` and leave the milestones at the old years
  expect_equal(h@intervals$mid, c(2025L, 2027L, 2032L))
})

test_that("update(config): `period` is refused, naming setHorizon", {
  cfg <- uc_cfg()
  cfg@horizon <- newHorizon(period = 2025:2034, intervals = c(5, 5))
  expect_error(update(cfg, period = 2040:2059), "setHorizon")
})

test_that("update(horizon): `intervals` stays settable, labels filled", {
  h <- newHorizon(period = 2025:2034, intervals = c(5, 5))
  iv <- data.table::as.data.table(h@intervals)
  iv$label <- NULL
  u <- update(h, intervals = iv)
  expect_equal(nrow(u@intervals), nrow(h@intervals))
  expect_true(all(nzchar(u@intervals$label)))
})

test_that("an unnamed object is checked for conflicting rows, not crashed on", {
  # `@name` is `character(0)` on an unnamed object, and `TRUE &&
  # nzchar(character(0))` is NA, which stopped the guard before any row was
  # read -- so `update()` on any unnamed object died there
  expect_error(energyRt:::.assert_no_conflicting_rows(
    data.frame(region = c("R1", "R1"), cost = c(1, 2)), "supply", "supply",
    name = character(0)), "same key")
  expect_true(energyRt:::.assert_no_conflicting_rows(
    data.frame(region = c("R1", "R2"), cost = c(1, 2)), "supply", "supply",
    name = character(0)))
})

test_that(".tech_regions(): a class with no region slot returns nothing", {
  expect_equal(energyRt:::.tech_regions(newCommodity("ELC", timeframe = "ANNUAL")),
               character(0))
  tech <- newTechnology("T", region = c("R1", "R2"),
                        input = list(comm = "COA"), output = list(comm = "ELC"))
  expect_setequal(energyRt:::.tech_regions(tech), c("R1", "R2"))
})
