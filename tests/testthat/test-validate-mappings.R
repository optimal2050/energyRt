# =========================================================================== #
# Mapping-consistency guards in validate_scenario_parameters(): free-meal
# detection (a populated variable domain whose governing constraint map is
# missing an object) and the calendar-chronology guard (timetable row order
# drives the storage balance chain AND the timeslice family, so a scrambled
# order is self-consistent between them and only the timetable's own columns
# can convict it). The chronology case scrambles a timetable hour-major,
# which chains the storage balance across the wrong hours.
# @covers meqStorageLevel meqTradeCapFlow mTimesliceNext depth=I
# =========================================================================== #

.vm_fixture <- function() {
  tm_file <- NULL
  for (cand in c(testthat::test_path("fixtures", "testing-models.R"),
                 "data-raw/testing-models.R",
                 file.path("..", "..", "data-raw", "testing-models.R"))) {
    if (file.exists(cand)) { tm_file <- cand; break }
  }
  skip_if(is.null(tm_file), "fixtures/testing-models.R not available")
  env <- new.env(parent = environment())
  source(tm_file, local = env)
  env$tm_core()
}

.vm_calendar <- function(scramble = FALSE) {
  tt <- make_timetable(struct = list(
    ANNUAL = "ANNUAL",
    SEASON = c("WIN", "SPR", "SUM", "AUT"),
    PART = c("p1", "p2")
  ))
  if (scramble) {
    d <- as.data.frame(tt)
    tt <- data.table::as.data.table(d[order(d$PART, d$SEASON), ])
  }
  newCalendar(timetable = tt,
              name = if (scramble) "vm_scrambled" else "vm_chrono")
}

test_that("a chronological nested calendar interpolates without structural findings", {
  mod <- .vm_fixture()
  withr::local_dir(withr::local_tempdir())
  scen <- suppressWarnings(suppressMessages(
    interpolate_model(mod, .vm_calendar(), name = "vm_ok")))
  res <- validate_scenario_parameters(scen, action = "silent")
  expect_false(any(res$severity == "structural"))
  # the fixture's storage genuinely declares no charge/discharge power
  # bounds; the free-meal guard surfaces that as ADVISORY, not error
  expect_true(all(res$check %in% c("free_meal")))
})

test_that("a scrambled (hour-major style) timetable is refused structurally", {
  mod <- .vm_fixture()
  withr::local_dir(withr::local_tempdir())
  err <- tryCatch(
    suppressWarnings(suppressMessages(
      interpolate_model(mod, .vm_calendar(scramble = TRUE), name = "vm_bad"))),
    error = function(e) conditionMessage(e))
  expect_type(err, "character")
  expect_match(err, "chronology")
  expect_match(err, "structural")
})

test_that("the free-meal guard reports missing storage balance as structural", {
  mod <- .vm_fixture()
  withr::local_dir(withr::local_tempdir())
  scen <- suppressWarnings(suppressMessages(
    interpolate_model(mod, .vm_calendar(), name = "vm_fm")))
  # simulate a lost balance map: empty meqStorageLevel next to a populated
  # level domain is exactly "discharge unlinked from history"
  p <- scen@modInp@parameters[["meqStorageLevel"]]
  skip_if(is.null(p), "no storage balance map in fixture scenario")
  d <- get_data_slot(p)
  scen@modInp@parameters[["meqStorageLevel"]] <-
    set_data_slot(p, d[0, , drop = FALSE])
  expect_error(validate_scenario_parameters(scen, action = "silent"),
               "free energy|free_meal|balance chain")
})
