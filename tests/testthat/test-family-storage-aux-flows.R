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
# A storage aux flowing into a commodity with a COARSER timeframe (ANNUAL WAT
# vs slice-level storage) aggregates through eqStorageInpTot's Agg branch --
# the same SameTimeslice/Agg map machinery the tech side uses (fixed
# 2026-08-25; previously the aux was computed but silently FREE because the
# totals equation only summed at identical timeslices).
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

# @covers eqStorageInpTot depth=X backends=julia_highs,pyomo forks=default
test_that("family storage-aux-flows: coarse-timeframe aux agrees across backends", {
  skip_if_no_solver()
  for (backend in c("julia_highs", "pyomo_cbc")) {
    if (backend == "julia_highs") skip_if_no_julia_highs() else skip_if_no_pyomo()
    scen <- suppressMessages(suppressWarnings(interpolate_model(
      sa_build(data.frame(acomm = "WAT", stg2ainp = 0.05),
               wat_timeframe = "ANNUAL"),
      name = paste0("sa_x_", backend), overwrite = TRUE)))
    solver <- if (backend == "julia_highs") solver_options$julia_highs else
      solver_options$pyomo_cbc
    scen <- .fork_solve(scen, solver)
    expect_true(verify_solution(scen)$ok, label = paste0(backend, " invariants"))
    expect_equal(.fork_objective(scen), 12, tolerance = 1e-6,
                 label = paste0(backend, " aux cost charged"))
  }
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

# @covers mStorageAInpCommAgg mStorageAInpCommAggTimeslice eqStorageInpTot depth=S backends=glpk
test_that("family storage-aux-flows: aux into a coarser-timeframe commodity is costed", {
  skip_if_no_solver()
  # same models but WAT at ANNUAL: the slice-level aux aggregates through the
  # Agg branch of eqStorageInpTot and the cost matches the slice-level case
  for (case in list(list(aeff = data.frame(acomm = "WAT", stg2ainp = 0.05),
                         tag = "sa_annual_stg", aux = 1),
                    list(aeff = data.frame(acomm = "WAT", cinp2ainp = 0.1),
                         tag = "sa_annual_cinp", aux = 1),
                    list(aeff = data.frame(acomm = "WAT", cout2ainp = 0.2),
                         tag = "sa_annual_cout", aux = 2))) {
    scen <- suppressMessages(suppressWarnings(interpolate_model(
      sa_build(case$aeff, wat_timeframe = "ANNUAL"),
      name = case$tag, overwrite = TRUE)))
    # the Agg classification maps must be populated (WAT coarser than storage)
    expect_gte(nrow(ff_param(scen, "mStorageAInpCommAgg")), 1)
    expect_gte(nrow(ff_param(scen, "mStorageAInpCommAggTimeslice")), 1)
    scen <- .fork_solve(scen, solver_options$glpk)
    expect_true(verify_solution(scen)$ok, label = paste0(case$tag, " invariants"))
    expect_equal(ff_solution_sum(scen, "vStorageAInp"), case$aux, tolerance = 1e-6,
                 label = paste0(case$tag, " aux volume"))
    expect_equal(.fork_objective(scen), 10 + 2 * case$aux, tolerance = 1e-6,
                 label = paste0(case$tag, " objective includes aux cost"))
  }
})
