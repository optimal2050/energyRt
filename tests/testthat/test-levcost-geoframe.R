# =========================================================================== #
# levcost() on a model whose commodity is balanced at a COARSER geoframe.
#
# `levcost()` prices one technology by building a tiny unit-demand model that
# inherits the parent's regions, calendar, discount rate and horizon -- but not
# its geoscale, because a screening model is one region with no hierarchy. A
# commodity carrying `@geoframe` then arrived in that model with nothing to
# resolve it against and the build died on
#
#   Commodity 'GAS' declare(s) `@geoframe`, but the model has no geoscale with
#   coarser levels. Attach one with `setGeoscale()`.
#
# which made every technology burning a zone-balanced fuel unscreenable. The
# declaration is dropped on the way in, the way `@emis` and a sub-annual
# `@timeframe` already were.
#
# @covers pTechInvcost pTechCinp2use depth=S backends=glpk forks=geoframe
# =========================================================================== #

lcg_geoscale <- function() {
  geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", region = c("R1", "R2"),
               stringsAsFactors = FALSE),
    geoframes = c("nation", "region"), key = "region", name = "lcg")
}

# GAS balanced at the nation, ELC at the region: one plant draws the national
# pool, which is the shape of a zone-level fuel.
lcg_model <- function(name, geoframe = "nation") {
  sup_reg <- if (length(geoframe) > 0) "NAT" else c("R1", "R2")
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = paste0("cal_", name))
  repo <- newRepository(paste0("repo_", name),
    newCommodity("GAS", desc = "Natural gas", timeframe = "ANNUAL",
                 geoframe = geoframe),
    newCommodity("ELC", desc = "Electricity", timeframe = "SEASON"),
    # a strict class sits where its commodity is balanced: the nation when GAS
    # is pooled there, the regions when it is not
    newSupply("SUP_GAS", desc = "Gas supply", commodity = "GAS",
              region = sup_reg,
              supply = data.frame(region = sup_reg, cost = 5)),
    newTechnology("EGAS", desc = "Gas-fired power plant",
                  input = list(comm = "GAS"), output = list(comm = "ELC"),
                  ceff = data.frame(comm = "GAS", cinp2use = 0.5),
                  cap2act = 31.536, olife = 30L,
                  invcost = list(invcost = 900), fixom = list(fixom = 20)),
    newDemand("DEM_ELC", desc = "Electricity demand", commodity = "ELC",
              demand = data.frame(region = c("R1", "R2"), demand = 10)))
  mod <- newModel(name, data = repo, calendar = cal, region = c("R1", "R2"),
                  horizon = newHorizon(2025), discount = 0.05)
  if (identical(geoframe, "nation")) mod <- setGeoscale(mod, lcg_geoscale())
  mod
}

test_that("levcost() screens a technology whose fuel is balanced coarsely", {
  skip_if_no_solver()
  testthat::skip_if_not_installed("geoscales")
  # levcost() anchors its screening model in a scratch directory of its own,
  # so no project is needed here
  coarse <- as.numeric(levcost(lcg_model("lcg_coarse"), name = "EGAS",
                               verbose = FALSE)$levcost_npv)
  expect_true(is.finite(coarse))
  expect_gt(coarse, 0)

  # the same plant in a model with no hierarchy at all must price identically:
  # where the fuel is BALANCED cannot change one technology's levelized cost
  flat <- as.numeric(levcost(lcg_model("lcg_flat", geoframe = character(0)),
                             name = "EGAS", verbose = FALSE)$levcost_npv)
  expect_equal(coarse, flat, tolerance = 1e-8)
})
