# =========================================================================== #
# Feature families: storage-efficiency (seff: inpeff/outeff/stgeff) and
# storage-varom (per-role operating costs inpcost/outcost/stgcost).
#
# Shift model, 1 region, ANNUAL + 4 slices, one year, discount 0:
#   supply of ELC only at s1 (ava.up), demand 10 at s3 -> the storage must
#   carry 10 across two slice steps (s1->s2->s3).
# Verified conventions (GLPK; vStorageLevel is the start-of-slice level):
#   inpeff scales what charging stores; outeff scales what discharge delivers;
#   stgeff is an ANNUAL retention rate applied per step as stgeff^share.
#   varom: vStorageVarom = inpcost*charge + outcost*discharge
#                          + stgcost*sum(levels)
# Analytic anchors:
#   base                          -> obj 10 (fuel only)
#   inpeff .8, outeff .5          -> charge 25, level 20, obj 25
#   stgeff .5 (share .25)         -> charge 10*2^0.5 = 14.142136, obj same;
#                                    successive levels shrink by 0.5^0.25
#   inpcost 2, outcost 3, stgcost .5 -> vStorageVarom = 20+30+10 = 60, obj 70
# =========================================================================== #

se_slices <- paste0("s", 1:4)

se_build <- function(seff = data.frame(), varom = data.frame()) {
  newModel("se",
    repo = newRepository("se_repo",
      newCommodity("ELC", timeframe = "SL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(timeslice = se_slices, cost = 1,
                                    ava.up = c(1000, 0, 0, 0))),
      newStorage("STG", commodity = "ELC",
                 olife = data.frame(olife = 30),
                 invcost = data.frame(stg.invcost = 0),
                 seff = seff, varom = varom),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = "s3", demand = 10))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = se_slices)),
      name = "se_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

se_solve <- function(mod, tag) {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mod, name = tag, overwrite = TRUE)))
  .fork_solve(scen, solver_options$glpk)
}

# @covers pStorageInpEff pStorageOutEff depth=S backends=glpk
test_that("family storage-efficiency: charge and discharge efficiencies compound", {
  skip_if_no_solver()
  scen0 <- suppressMessages(suppressWarnings(interpolate_model(
    se_build(seff = data.frame(inpeff = 0.8, outeff = 0.5)),
    name = "se_io_i", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen0, "pStorageInpEff")$value), 0.8)
  expect_equal(unique(ff_param(scen0, "pStorageOutEff")$value), 0.5)
  scen <- se_solve(se_build(seff = data.frame(inpeff = 0.8, outeff = 0.5)), "se_io")
  expect_true(verify_solution(scen)$ok)
  # deliver 10 = charge * 0.8 * 0.5 -> charge 25, stored level 20
  expect_equal(ff_solution_sum(scen, "vStorageInp"), 25, tolerance = 1e-6)
  expect_equal(ff_solution_sum(scen, "vStorageOut"), 10, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 25, tolerance = 1e-6)
})

# @covers pStorageStgEff depth=S backends=glpk
test_that("family storage-efficiency: stgeff decays the level as stgeff^share per step", {
  skip_if_no_solver()
  scen <- se_solve(se_build(seff = data.frame(stgeff = 0.5)), "se_stg")
  expect_true(verify_solution(scen)$ok)
  # two retention steps of 0.5^0.25 each between charge (s1) and delivery (s3)
  expect_equal(ff_solution_sum(scen, "vStorageInp"), 10 * 2^0.5, tolerance = 1e-6)
  lv <- ff_solution_sum(scen, "vStorageLevel", by = "timeslice")
  lv <- lv[order(timeslice)]
  expect_equal(lv$value[2] / lv$value[1], 0.5^0.25, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 10 * 2^0.5, tolerance = 1e-6)
})

# @covers pStorageCostInp pStorageCostOut pStorageCostStore vStorageVarom depth=S backends=glpk
test_that("family storage-varom: per-role operating costs add up exactly", {
  skip_if_no_solver()
  scen0 <- suppressMessages(suppressWarnings(interpolate_model(
    se_build(varom = data.frame(inpcost = 2, outcost = 3, stgcost = 0.5)),
    name = "se_varom_i", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen0, "pStorageCostInp")$value), 2)
  expect_equal(unique(ff_param(scen0, "pStorageCostOut")$value), 3)
  expect_equal(unique(ff_param(scen0, "pStorageCostStore")$value), 0.5)
  scen <- se_solve(se_build(varom = data.frame(inpcost = 2, outcost = 3,
                                               stgcost = 0.5)), "se_varom")
  expect_true(verify_solution(scen)$ok)
  # charge 10 (2*10) + discharge 10 (3*10) + levels 10+10 (0.5*20) = 60
  expect_equal(ff_solution_sum(scen, "vStorageVarom"), 60, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 70, tolerance = 1e-6)
})
