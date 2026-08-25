# =========================================================================== #
# An `eac`-financed charging or storing part must EXIST.
#
# `mStorageInpCap` / `mStorageStgCap` follow the STRUCTURE FOLLOWS DATA rule in
# map_value.R: a part acquires its own capacity variable only when the object
# carries data for it. `pStorage*Invcost` and `pStorage*Fixom` were sources;
# `pStorage*Eac` was not.
#
# That combination failed silently and expensively. `mStorage*Eac` still built
# an objective term, so the LP carried a capacity variable that was PRICED but
# appeared in no constraint -- and drove it to zero. An eac-financed charger or
# reservoir was free, and its flow fell back to the `mStorageNo*Cap` branch
# (`ratio * vStorageOutCap`) instead of being bounded by its own capacity.
#
# Found converting PyPSA-Eur, where `capital_cost` is already annuitised and so
# maps to `eac` rather than `invcost`: a fused H2 store plus electrolyser plus
# fuel cell solved 0.35% BELOW the equivalent three-object model, with an
# unbounded reservoir and a free electrolyser.
# =========================================================================== #

se_slices <- paste0("s", 1:4)

# Charge in s1 (cheap, plentiful), discharge in s3. The charger is priced, so a
# correct model pays for it; a broken one gets it for nothing.
# `invcost` is an overnight cost annualised over `olife`; `eac` IS the annuity.
# With olife 30 and discount 0 the equivalent pair is 300 and 10.
se_price <- c(invcost = 300, eac = 10)

se_build <- function(cost_slot, price = se_price[[cost_slot]]) {
  invcost <- data.frame(price, 0)
  names(invcost) <- c(paste0("inp.", cost_slot), "stg.invcost")
  newModel("se",
    repo = newRepository("se_repo",
      newCommodity("ELC", timeframe = "SL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(timeslice = se_slices, cost = 1,
                                    ava.up = c(1000, 0, 0, 0))),
      newStorage("STG", commodity = "ELC",
                 olife = data.frame(olife = 30),
                 invcost = invcost),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = "s3", demand = 10))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = se_slices)),
      name = "se_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

se_interp <- function(cost_slot, nm) {
  suppressMessages(suppressWarnings(interpolate_model(
    se_build(cost_slot), name = nm, overwrite = TRUE)))
}

# @covers mStorageInpCap depth=I backends=glpk
test_that("an eac-priced charging part gets its own capacity variable", {
  for (slot in c("invcost", "eac")) {
    scen <- se_interp(slot, paste0("se_", slot))
    expect_gte(nrow(ff_param(scen, "mStorageInpCap")), 1,
               label = paste0("mStorageInpCap rows with inp.", slot))
    # and therefore is NOT pushed onto the no-capacity fallback branch
    expect_equal(nrow(ff_param(scen, "mStorageNoInpCap")), 0,
                 label = paste0("mStorageNoInpCap rows with inp.", slot))
  }
})

# @covers mStorageInpCap depth=S backends=glpk
test_that("an equivalent eac and invcost price a charger identically", {
  skip_if_no_solver()
  obj <- vapply(c("invcost", "eac"), function(slot) {
    scen <- suppressMessages(suppressWarnings(solve_model(
      se_build(slot), name = paste0("se_solve_", slot),
      solver = solver_options$glpk, overwrite = TRUE)))
    getData(scen, "vObjective", merge = TRUE)$value
  }, numeric(1))
  # 10 MW charged at supply cost 1, plus 10 MW of charger at 10/yr = 110.
  expect_equal(unname(obj[["invcost"]]), 110, tolerance = 1e-9)
  expect_equal(unname(obj[["eac"]]), unname(obj[["invcost"]]), tolerance = 1e-9)
  # The charger is PAID for. Before the fix `inp.eac` bought nothing: the
  # capacity variable existed, carried its cost, and appeared in no constraint,
  # so the LP set it to zero and the objective fell to the bare supply cost.
  expect_gt(unname(obj[["eac"]]), 10)
})

# @covers mStorageStgCap depth=I backends=glpk
test_that("an eac-priced storing part gets its own capacity variable", {
  mk <- function(slot) {
    inv <- data.frame(10)
    names(inv) <- paste0("stg.", slot)
    newModel("ss",
      repo = newRepository("ss_repo",
        newCommodity("ELC", timeframe = "SL"),
        newSupply("SUP", commodity = "ELC",
                  supply = data.frame(timeslice = se_slices, cost = 1,
                                      ava.up = c(1000, 0, 0, 0))),
        newStorage("STG", commodity = "ELC",
                   olife = data.frame(olife = 30), invcost = inv,
                   duration = data.frame(duration.lo = 0)),
        newDemand("DEM", commodity = "ELC",
                  demand = data.frame(timeslice = "s3", demand = 10))),
      calendar = newCalendar(
        timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = se_slices)),
        name = "ss_cal"),
      region = "R1", horizon = newHorizon(2025), discount = 0)
  }
  for (slot in c("invcost", "eac")) {
    scen <- suppressMessages(suppressWarnings(interpolate_model(
      mk(slot), name = paste0("ss_", slot), overwrite = TRUE)))
    expect_gte(nrow(ff_param(scen, "mStorageStgCap")), 1,
               label = paste0("mStorageStgCap rows with stg.", slot))
    expect_equal(nrow(ff_param(scen, "mStorageNoStgCap")), 0,
                 label = paste0("mStorageNoStgCap rows with stg.", slot))
  }
})
