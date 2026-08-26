# =========================================================================== #
# Feature family: commodity (emission factors, aggregation, limtype).
#
# Mini-model, 1 region, ANNUAL-only calendar, one year (2025), discount 0:
#   SUP COA at cost 1 -> ECOA (COA -> ELC, geff 1) -> DEM_ELC 40.
#   COA carries emis: CO2 0.09/unit  -> combustion CO2 = 40 * 0.09 = 3.6
#   GHG aggregates CO2 with factor 2 -> vAggOutTot[GHG] = 7.2
#   A tax on the emission / aggregate commodity makes both observable in the
#   objective: base obj = 40 (fuel) + 40 * 1000/30 (EAC) = 1373.333.
# =========================================================================== #

cm_build <- function(tax = NULL, agg_factor = NULL, limtype_extra = NULL) {
  coa <- newCommodity("COA", timeframe = "ANNUAL",
                      emis = data.frame(comm = "CO2", emis = 0.09))
  objs <- list(
    coa,
    newCommodity("ELC", timeframe = "ANNUAL"),
    newCommodity("CO2", timeframe = "ANNUAL"),
    newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
    newTechnology("ECOA", input = list(comm = "COA"), output = list(comm = "ELC"),
                  invcost = list(invcost = 1000), olife = list(olife = 30),
                  cap2act = 1),
    newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 40)))
  if (!is.null(agg_factor)) {
    objs <- c(objs, list(newCommodity(
      "GHG", timeframe = "ANNUAL",
      agg = data.frame(comm = "CO2", agg = agg_factor))))
  }
  if (!is.null(tax)) objs <- c(objs, list(tax))
  if (!is.null(limtype_extra)) {
    # the commodity must actually be USED or it is pruned from the balance
    objs <- c(objs, list(
      limtype_extra,
      newSupply("SXTR", commodity = "XTR", supply = data.frame(cost = 1)),
      newDemand("DXTR", commodity = "XTR", demand = data.frame(demand = 5))))
  }
  newModel("cm",
    repo = do.call(newRepository, c(list("cm_repo"), objs)),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
      name = "cm_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

cm_base_obj <- 40 + 40 * 1000 / 30

# @covers pEmissionFactor vEmsFuelTot eqEmsFuelTot mEmsFuelTot depth=S backends=glpk
test_that("family commodity: fuel emission factor produces and prices CO2", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    cm_build(tax = newTax("TCO2", comm = "CO2", tax = data.frame(out = 10))),
    name = "cm_emis", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pEmissionFactor")$value), 0.09)
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vEmsFuelTot"), 3.6, tolerance = 1e-6)
  # tax 10 per unit CO2 produced: + 36
  expect_equal(.fork_objective(scen), cm_base_obj + 36, tolerance = 1e-4)
})

# @covers pAggregateFactor vAggOutTot eqAggOutTot mAggOut depth=S backends=glpk
test_that("family commodity: aggregate commodity scales its member by the agg factor", {
  skip_if_no_solver()
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    cm_build(agg_factor = 2,
             tax = newTax("TGHG", comm = "GHG", tax = data.frame(out = 5))),
    name = "cm_agg", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pAggregateFactor")$value), 2)
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  # GHG = 2 * CO2 = 7.2; tax 5/unit: + 36
  expect_equal(ff_solution_sum(scen, "vAggOutTot"), 7.2, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), cm_base_obj + 36, tolerance = 1e-4)
})

# @covers meqBalLo meqBalUp meqBalFx depth=I backends=glpk
test_that("family commodity: limtype routes the balance to the right equation", {
  for (lt in c("LO", "UP", "FX")) {
    scen <- suppressMessages(suppressWarnings(interpolate_model(
      cm_build(limtype_extra = newCommodity("XTR", timeframe = "ANNUAL",
                                            limtype = lt)),
      name = paste0("cm_lt_", lt), overwrite = TRUE)))
    map <- paste0("meqBal", c(LO = "Lo", UP = "Up", FX = "Fx")[[lt]])
    d <- ff_param(scen, map)
    expect_true("XTR" %in% as.character(d$comm),
                label = paste0("limtype ", lt, " -> ", map))
    for (other in setdiff(c("meqBalLo", "meqBalUp", "meqBalFx"), map)) {
      od <- ff_param(scen, other)
      expect_false(!is.null(od) && "XTR" %in% as.character(od$comm),
                   label = paste0("limtype ", lt, " not in ", other))
    }
  }
})


# @covers mCommTimeslice depth=S backends=glpk
test_that("family commodity: an omitted timeframe defaults to the finest level", {
  skip_if_no_solver()
  # `newCommodity()` leaves @timeframe empty (the documented default). This
  # used to crash `map_comm_timeframe()` outright ("replacement has length
  # zero") -- every real model had dodged it by always passing `timeframe =`.
  cal <- newCalendar(
    timetable = make_timetable(struct = list(ANNUAL = "ANNUAL",
                                             SEASON = c("WIN", "SUM"))),
    name = "cmtf_cal")
  mod <- newModel("cmtf",
    repo = newRepository("cmtf_repo",
      newCommodity("ELC"),                        # <- no timeframe=
      newSupply("SUP", commodity = "ELC", supply = data.frame(cost = 1)),
      newDemand("DEM", commodity = "ELC", demand = data.frame(demand = 6))),
    calendar = cal, region = "R1", horizon = newHorizon(2025), discount = 0)
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mod, name = "cmtf", overwrite = TRUE)))
  # balanced at the calendar's finest level (SEASON), not ANNUAL
  ct <- as.data.frame(get_data_slot(scen@modInp@parameters[["mCommTimeslice"]]))
  expect_setequal(ct$timeslice[ct$comm == "ELC"], c("WIN", "SUM"))
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  d <- as.data.frame(get_data_slot(scen@modOut@variables[["vObjective"]]))
  # the timeslice-less demand row broadcasts 6 onto EACH season -- 12 total at
  # cost 1 (were ELC balanced at ANNUAL, the objective would be 6: this doubles
  # as the assertion that the finest level actually won)
  expect_equal(sum(d$value), 12, tolerance = 1e-9)
})
