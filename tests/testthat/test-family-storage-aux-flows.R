# =========================================================================== #
# Feature family: storage-aux-flows (storage aeff -- auxiliary flows driven by
# charge, discharge, and the stored level).
#
# Shift model (supply at s1 only, demand 10 at s3), WAT aux at cost 2.
# Verified conventions (aux lands at the STORAGE's own timeslices; storage
# aeff has NO `comm` column -- the storage's main commodity is implied):
#   cinp2ainp r: aux_s = r * charge_s        (0.1 -> 1 @ s1)
#   cout2ainp r: aux_s = r * discharge_s     (0.2 -> 2 @ s3)
#   stg2ainp  r: aux_s = r * level_s         (0.05 -> 0.5 @ s2, s3)
#
# KNOWN BUG pinned below: unlike the tech side (eqTechInpTot has
# SameTimeslice + Agg branches), eqStorageInpTot sums vStorageAInp only at
# the SAME (comm, region, year, timeslice) -- a storage aux flowing into a
# commodity with a COARSER timeframe (e.g. ANNUAL WAT vs slice-level storage)
# never reaches the balance: the aux is computed but FREE. Workaround until
# fixed: declare the aux commodity at the storage's timeframe.
# =========================================================================== #

sa_slices <- paste0("s", 1:4)

sa_build <- function(aeff, wat_timeframe = "SL") {
  newModel("sa",
    repo = newRepository("sa_repo",
      newCommodity("ELC", timeframe = "SL"),
      newCommodity("WAT", timeframe = wat_timeframe),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(timeslice = sa_slices, cost = 1,
                                    ava.up = c(1000, 0, 0, 0))),
      newSupply("SWAT", commodity = "WAT", supply = data.frame(cost = 2)),
      newStorage("STG", commodity = "ELC", olife = data.frame(olife = 30),
                 invcost = data.frame(stg.invcost = 0),
                 aux = data.frame(acomm = "WAT"), aeff = aeff),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = "s3", demand = 10))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = sa_slices)),
      name = "sa_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

sa_case <- function(aeff, tag, param, total_aux) {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    sa_build(aeff), name = tag, overwrite = TRUE)))
  p <- ff_param(scen, param)
  expect_false(is.null(p) || nrow(p) == 0, label = paste0(tag, " ", param, " rows"))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok, label = paste0(tag, " invariants"))
  expect_equal(ff_solution_sum(scen, "vStorageAInp"), total_aux, tolerance = 1e-6,
               label = paste0(tag, " total aux"))
  # base obj 10 (fuel) + aux at cost 2
  expect_equal(.fork_objective(scen), 10 + 2 * total_aux, tolerance = 1e-6,
               label = paste0(tag, " objective"))
}

# @covers pStorageCinp2AInp vStorageAInp depth=S backends=glpk
test_that("family storage-aux-flows: charge-driven aux input", {
  skip_if_no_solver()
  sa_case(data.frame(acomm = "WAT", cinp2ainp = 0.1), "sa_cinp",
          "pStorageCinp2AInp", 1)
})

# @covers pStorageCout2AInp depth=S backends=glpk
test_that("family storage-aux-flows: discharge-driven aux input", {
  skip_if_no_solver()
  sa_case(data.frame(acomm = "WAT", cout2ainp = 0.2), "sa_cout",
          "pStorageCout2AInp", 2)
})

# @covers pStorageStg2AInp depth=S backends=glpk
test_that("family storage-aux-flows: level-driven aux input", {
  skip_if_no_solver()
  sa_case(data.frame(acomm = "WAT", stg2ainp = 0.05), "sa_stg",
          "pStorageStg2AInp", 1)
})

# @covers pStorageCap2AInp pStorageNCap2AInp pStorageCinp2AOut pStorageCout2AOut pStorageStg2AOut depth=I backends=glpk
test_that("family storage-aux-flows: remaining ratios land in modInp", {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    sa_build(rbind(
      data.frame(acomm = "WAT", cap2ainp = 0.01, ncap2ainp = NA,
                 cinp2aout = NA, cout2aout = NA, stg2aout = NA),
      data.frame(acomm = "WAT", cap2ainp = NA, ncap2ainp = 0.02,
                 cinp2aout = NA, cout2aout = NA, stg2aout = NA),
      data.frame(acomm = "WAT", cap2ainp = NA, ncap2ainp = NA,
                 cinp2aout = 0.03, cout2aout = NA, stg2aout = NA),
      data.frame(acomm = "WAT", cap2ainp = NA, ncap2ainp = NA,
                 cinp2aout = NA, cout2aout = 0.04, stg2aout = NA),
      data.frame(acomm = "WAT", cap2ainp = NA, ncap2ainp = NA,
                 cinp2aout = NA, cout2aout = NA, stg2aout = 0.05))),
    name = "sa_rest", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pStorageCap2AInp")$value), 0.01)
  expect_equal(unique(ff_param(scen, "pStorageNCap2AInp")$value), 0.02)
  expect_equal(unique(ff_param(scen, "pStorageCinp2AOut")$value), 0.03)
  expect_equal(unique(ff_param(scen, "pStorageCout2AOut")$value), 0.04)
  expect_equal(unique(ff_param(scen, "pStorageStg2AOut")$value), 0.05)
})

test_that("family storage-aux-flows: KNOWN-BUG aux into a coarser-timeframe commodity is free", {
  skip_if_no_solver()
  # same stg2ainp model but WAT at ANNUAL: the aux is computed yet never
  # enters WAT's balance -- the objective misses the 2 * 1.0 aux cost
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    sa_build(data.frame(acomm = "WAT", stg2ainp = 0.05), wat_timeframe = "ANNUAL"),
    name = "sa_annual", overwrite = TRUE)))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_equal(ff_solution_sum(scen, "vStorageAInp"), 1, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 10, tolerance = 1e-6,
               label = "KNOWN-BUG pin: aux cost dropped (fix: add Agg branch to eqStorageInpTot)")
})
