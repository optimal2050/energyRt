# =========================================================================== #
# Remaining zero-coverage technology parameters: new-capacity bounds
# (pTechNewCap), combustion share (pTechEmisComm), and the grouped-input
# conversion (pTechCinp2ginp).
#
# Two-tech merit-order model (ANNUAL, one year, discount 0): ECHEAP EAC
# 10/unit, EEXP EAC 100/unit, both COA -> ELC at fuel cost 1, demand 40.
# Baseline all-cheap obj = 440.
# =========================================================================== #

to_build <- function(cheap_capacity = data.frame(), extra = NULL,
                     cheap_input = list(comm = "COA")) {
  objs <- list(
    newCommodity("COA", timeframe = "ANNUAL",
                 emis = data.frame(comm = "CO2", emis = 0.09)),
    newCommodity("ELC", timeframe = "ANNUAL"),
    newCommodity("CO2", timeframe = "ANNUAL"),
    newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
    newTechnology("ECHEAP", input = cheap_input, output = list(comm = "ELC"),
                  capacity = cheap_capacity,
                  invcost = list(invcost = 300), olife = list(olife = 30),
                  cap2act = 1),
    newTechnology("EEXP", input = list(comm = "COA"), output = list(comm = "ELC"),
                  invcost = list(invcost = 3000), olife = list(olife = 30),
                  cap2act = 1),
    newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 40)))
  if (!is.null(extra)) objs <- c(objs, list(extra))
  newModel("to",
    repo = do.call(newRepository, c(list("to_repo"), objs)),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
      name = "to_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

# @covers pTechNewCap depth=S backends=glpk
test_that("family tech-odds: an ncap.up bound forces the merit order", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    to_build(cheap_capacity = data.frame(ncap.up = 25)),
    name = "to_ncap", overwrite = TRUE)))
  expect_equal(unique(ff_bound(scen, "pTechNewCap", "up")$value), 25)
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  ncap <- as.data.frame(ff_solution_sum(scen, "vTechNewCap", by = "tech"))
  expect_equal(ncap$value[ncap$tech == "ECHEAP"], 25, tolerance = 1e-6)
  expect_equal(ncap$value[ncap$tech == "EEXP"], 15, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 40 + 25 * 10 + 15 * 100, tolerance = 1e-6)
})

# @covers pTechEmisComm depth=S backends=glpk
test_that("family tech-odds: the combustion share scales fuel emissions", {
  skip_if_no_solver()
  # combustion = 0.5 halves the CO2 from burning COA (e.g. partial capture);
  # a tax on CO2 makes it observable: 40 * 0.09 * 0.5 = 1.8, taxed at 10
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    to_build(cheap_input = list(comm = "COA", combustion = 0.5),
             extra = newTax("TCO2", comm = "CO2", tax = data.frame(out = 10))),
    name = "to_comb", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pTechEmisComm")$value), 0.5)
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vEmsFuelTot"), 1.8, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 440 + 18, tolerance = 1e-4)
})

# @covers pTechCinp2ginp depth=I backends=glpk
test_that("family tech-odds: the grouped-input fuel conversion lands in modInp", {
  tech <- newTechnology("EMIX",
    group = data.frame(group = "FUEL"),
    input = data.frame(comm = c("COA", "GAS"), group = "FUEL"),
    ceff = data.frame(comm = c("COA", "GAS"), cinp2ginp = c(0.9, 1.1)),
    geff = data.frame(group = "FUEL", ginp2use = 0.8),
    output = list(comm = "ELC"),
    invcost = list(invcost = 300), olife = list(olife = 30), cap2act = 1)
  mod <- newModel("to2",
    repo = newRepository("to2_repo",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("GAS", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "ANNUAL"),
      newSupply("SC", commodity = "COA", supply = data.frame(cost = 1)),
      newSupply("SG", commodity = "GAS", supply = data.frame(cost = 2)),
      tech,
      newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 40))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
      name = "to2_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mod, name = "to_ginp", overwrite = TRUE)))
  v <- ff_param(scen, "pTechCinp2ginp")
  expect_equal(sort(unique(v$value)), c(0.9, 1.1))
})
