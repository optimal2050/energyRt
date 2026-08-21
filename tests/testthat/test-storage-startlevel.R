# =========================================================================== #
# storage@startLevel  (was @charge, briefly @inflow)
#
# Energy added to the level ONCE PER CYCLE, at the FIRST timeslice of the cycle.
# There is no `timeslice` column: the slice is derived from the calendar and
# `@fullYear`. That is the point -- the old `@charge` took a timeslice, and a row
# without one was broadcast by the NA wildcard to EVERY timeslice, which on an
# hourly calendar is 8760 unpriced injections where one was meant.
#
# It is free to the model by design (a store ending a cycle below where it
# started has consumed an endowment). Being additive, the level at the first
# timeslice is startLevel PLUS any carry-over, i.e. >= startLevel, not = it.
# =========================================================================== #

sl_cal <- function() newCalendar(
  timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", DAY = c("d1", "d2"), HOUR = paste0("h", 1:4))),
  name = "sl_cal")

sl_model <- function(fullYear = TRUE, startLevel = 7, name = "sl") newModel(
  name = name, region = "R1", horizon = newHorizon(2020), discount = 0,
  calendar = sl_cal(),
  repo = newRepository("r",
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "HOUR"),
    newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
    newTechnology("E", input = list(comm = "COA"), output = list(comm = "ELC"),
                  invcost = data.frame(invcost = 100),
                  vintage = data.frame(olife = 30L), cap2act = 1),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(
                timeslice = paste0(rep(c("d1", "d2"), each = 4), "_h", 1:4),
                demand = 10)),
    newStorage("STG", commodity = "ELC", invcost = list(invcost = 1),
               vintage = data.frame(olife = 30L), fullYear = fullYear,
               startLevel = data.frame(startLevel = startLevel))))

sl_param <- function(fullYear) {
  s <- suppressMessages(suppressWarnings(interpolate_model(
    sl_model(fullYear), name = "sl", ondisk = FALSE)))
  gds <- getFromNamespace("get_data_slot", "energyRt")
  as.data.frame(gds(s@modInp@parameters[["pStorageStartLevel"]]))
}

test_that("@startLevel has no timeslice column -- the slice is derived", {
  s <- newStorage("S", commodity = "ELC",
                  startLevel = data.frame(startLevel = 7))
  expect_false("timeslice" %in% names(s@startLevel))
  expect_equal(s@startLevel$startLevel, 7)
})

test_that("fullYear = TRUE injects once, at the first timeslice of the year", {
  d <- sl_param(TRUE)
  expect_equal(nrow(d), 1L)
  expect_equal(d$timeslice, "d1_h1")
  expect_equal(d$value, 7)
})

test_that("fullYear = FALSE injects once per cycle, SHARING the annual value", {
  d <- sl_param(FALSE)
  expect_equal(nrow(d), 2L)
  expect_setequal(d$timeslice, c("d1_h1", "d2_h1"))
  # `startLevel` is ANNUAL: two daily cycles get half each, and the year still
  # totals 7. Without the share scaling a daily-cycling store is endowed twice
  # over here -- and 365 times on a real calendar.
  expect_equal(sum(d$value), 7)
  expect_true(all(abs(d$value - 3.5) < 1e-12))
  # never on a non-first slice -- the wildcard bug the old slot had
  expect_false(any(d$timeslice %in% c("d1_h2", "d1_h4", "d2_h4")))
})

test_that("the annual total is invariant to where the cycle closes", {
  expect_equal(sum(sl_param(TRUE)$value), sum(sl_param(FALSE)$value))
})

test_that("a storage on a COARSE commodity still gets its startLevel", {
  # `@timetable` holds leaves only, so ordering by it made a storage whose
  # commodity sits above the leaf level (a reservoir on daily water) match no
  # timeslices and emit NOTHING -- silently. `@timeslice_share` covers every
  # level, and `mvStorageLevel` keeps the placement at the storage's own
  # resolution rather than once per level.
  mod <- newModel(
    name = "slc", region = "R1", horizon = newHorizon(2020), discount = 0,
    calendar = sl_cal(),
    repo = newRepository("r",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "DAY"),          # COARSE, not a leaf
      newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
      newTechnology("E", input = list(comm = "COA"), output = list(comm = "ELC"),
                    invcost = data.frame(invcost = 100),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = c("d1", "d2"), demand = 10)),
      newStorage("STG", commodity = "ELC", invcost = list(invcost = 1),
                 vintage = data.frame(olife = 30L),
                 startLevel = data.frame(startLevel = 7))))
  s <- suppressMessages(suppressWarnings(
    interpolate_model(mod, name = "slc", ondisk = FALSE)))
  gds <- getFromNamespace("get_data_slot", "energyRt")
  d <- as.data.frame(gds(s@modInp@parameters[["pStorageStartLevel"]]))
  expect_equal(nrow(d), 1L)
  expect_equal(d$timeslice, "d1")        # the DAY level, not an hour
  expect_equal(sum(d$value), 7)
})

test_that("a zero or absent @startLevel writes nothing", {
  s <- suppressMessages(suppressWarnings(interpolate_model(
    sl_model(TRUE, startLevel = 0), name = "sl0", ondisk = FALSE)))
  gds <- getFromNamespace("get_data_slot", "energyRt")
  d <- as.data.frame(gds(s@modInp@parameters[["pStorageStartLevel"]]))
  expect_true(is.null(d) || nrow(d) == 0L)
})

test_that("the deprecated `charge` and `inflow` still work, warned once", {
  reset <- getFromNamespace(".en_deprecate_reset", "energyRt")
  for (old_nm in c("charge", "inflow")) {
    reset()
    args <- list(name = "S", commodity = "ELC")
    args[[old_nm]] <- stats::setNames(data.frame(7), old_nm)
    expect_warning(s <- do.call(newStorage, args), "renamed to `@startLevel`")
    expect_equal(s@startLevel$startLevel, 7)
  }
  reset()
})

test_that("a `timeslice` column on the deprecated spelling is dropped, loudly", {
  reset <- getFromNamespace(".en_deprecate_reset", "energyRt")
  reset()
  # trip the once-per-session deprecation notice first, so the only warning left
  # to assert on is the one about the ignored `timeslice`
  suppressWarnings(newStorage("S0", commodity = "ELC",
                              charge = data.frame(charge = 1)))
  expect_warning(
    s <- newStorage("S", commodity = "ELC",
                    charge = data.frame(timeslice = "d1_h3", charge = 7)),
    "ignored")
  expect_false("timeslice" %in% names(s@startLevel))
  expect_equal(s@startLevel$startLevel, 7)
  reset()
})

test_that("supplying both spellings is an error", {
  expect_error(
    newStorage("S", commodity = "ELC",
               charge = data.frame(charge = 1),
               startLevel = data.frame(startLevel = 1)),
    "not both")
})

test_that("update() honours the same rename", {
  reset <- getFromNamespace(".en_deprecate_reset", "energyRt")
  reset()
  s <- newStorage("S", commodity = "ELC")
  expect_warning(s2 <- update(s, charge = data.frame(charge = 3)),
                 "renamed to `@startLevel`")
  expect_equal(s2@startLevel$startLevel, 3)
  reset()
})

test_that("the public constructors are still exported", {
  # Guard against a roxygen accident: inserting a helper BETWEEN a roxygen block
  # and the function it documents silently reassigns the block (and its @export)
  # to the helper. `devtools::load_all()` exposes everything, so the suite stays
  # green while an INSTALLED package loses the function. That happened here to
  # `newStorage()` while adding `.storage_deprecated_args()`.
  exported <- getNamespaceExports(asNamespace("energyRt"))
  for (f in c("newStorage", "newTechnology", "newCommodity", "newTrade",
              "newSupply", "newDemand", "newModel", "newCalendar")) {
    expect_true(f %in% exported, info = paste(f, "is not exported"))
  }
  expect_false(any(grepl("^[.]storage_", exported)))
})
