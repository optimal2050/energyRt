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
    .fold_write_back(p, d[0, , drop = FALSE])
  expect_error(validate_scenario_parameters(scen, action = "silent"),
               "free energy|free_meal|balance chain")
})

# ---- coefficient_scale advisory --------------------------------------------- #
# cap2act x finest timeslice share outside [1e-4, 1e4] is reported with the
# cap2act that would put the product near 1; inside the band nothing fires.
test_that("the coefficient_scale advisory fires on cap2act x share far from 1", {
  scen <- suppressMessages(suppressWarnings(
    interpolate_model(fold_model(), name = "vm_scale", ondisk = FALSE)))
  # the fold fixture: cap2act 8760 on an annual calendar -> 8760, in band
  expect_equal(nrow(energyRt:::.vsp_coefficient_scale(scen)), 0L)
  c2 <- scen@modInp@parameters$pTechCap2act
  d2 <- as.data.frame(get_data_slot(c2)); d2$value <- 1
  scen@modInp@parameters$pTechCap2act <- energyRt:::.fold_write_back(c2, d2)
  sh <- scen@modInp@parameters$pTimesliceShare
  d <- as.data.frame(get_data_slot(sh)); d$value <- 1e-5
  scen@modInp@parameters$pTimesliceShare <- energyRt:::.fold_write_back(sh, d)
  sc <- energyRt:::.vsp_coefficient_scale(scen)
  expect_equal(sc$parameter, "pTechCap2act")
  expect_match(sc$detail, "TPP: 1 x 1e-05")
  res <- validate_scenario_parameters(scen, action = "silent")
  expect_true("coefficient_scale" %in% res$check)
  expect_equal(res$severity[res$check == "coefficient_scale"], "advisory")
})

# ---- constraint_not_in_param ------------------------------------------------ #
# A data-driven `meq*` map is the domain INTERSECTED with the parameter that
# drives it, so a tuple with no value behind it is a bound the modeller never
# wrote (measured: a coarse process got `meqTechAfcInpUp` rows at the
# commodity's finer slices while `pTechAfc` sat at the process's own, and the
# writer's `default 0` shut the process down). The `structural = TRUE` entries
# are the exception and must stay exempt: they are instantiated over their
# WHOLE domain by design and their declared default binds -- `pTechAf` up = 1
# is the plain capacity limit, written for every slice left blank.
.vm_af_model <- function() {
  newModel("vmaf", repo = newRepository("r",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SEASON"),
      newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
      newTechnology("PP", input = list(comm = "COA"),
        output = list(comm = "ELC"),
        # one of the two seasons only: the other reads the declared default 1
        af = data.frame(timeslice = "SUM", af.up = 0.2, af.lo = 0.05),
        invcost = data.frame(invcost = 1), cap2act = 1,
        vintage = data.frame(olife = 30L)),
      newDemand("DE", commodity = "ELC",
                demand = data.frame(timeslice = c("WIN", "SUM"), demand = 10))),
    calendar = newCalendar(timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
      name = "vmafc"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

test_that("af declared on a subset of timeslices is not a missing-value finding", {
  scen <- suppressMessages(suppressWarnings(
    interpolate_model(.vm_af_model(), name = "vm_af_subset", ondisk = FALSE)))
  res <- validate_scenario_parameters(scen, action = "silent")
  expect_false("constraint_not_in_param" %in% res$check)
  # the map really is domain-wide, or the exemption would be untested
  mp <- as.data.frame(get_data_slot(scen@modInp@parameters$meqTechAfUp))
  pa <- as.data.frame(get_data_slot(scen@modInp@parameters$pTechAf))
  expect_gt(nrow(mp), nrow(pa[pa$type == "up", , drop = FALSE]))
})

# The other half: on a DATA-DRIVEN map the invariant holds, and a widened map
# is exactly the shape the coarse-process work produced. Inject one tuple the
# parameter has no value for and the finding must be structural.
test_that("a widened data-driven constraint map is a structural finding", {
  scen <- suppressMessages(suppressWarnings(
    interpolate_model(.vm_af_model(), name = "vm_af_widen", ondisk = FALSE)))
  mp <- scen@modInp@parameters$meqTechAfLo
  d <- as.data.frame(get_data_slot(mp))
  expect_equal(d$timeslice, "SUM")           # built from pTechAf's lo side
  d2 <- rbind(d, transform(d, timeslice = "WIN"))
  scen@modInp@parameters$meqTechAfLo <- energyRt:::.fold_write_back(mp, d2)
  # structural findings are errors whatever `action` says
  err <- tryCatch(validate_scenario_parameters(scen, action = "silent"),
                  error = function(e) conditionMessage(e))
  expect_type(err, "character")
  expect_match(err, "constraint_not_in_param")
  expect_match(err, "meqTechAfLo: 1 constraint tuple")
  expect_match(err, "pTechAf")
})
