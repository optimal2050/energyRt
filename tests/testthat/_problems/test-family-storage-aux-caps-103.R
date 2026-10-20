# Extracted from test-family-storage-aux-caps.R:103

# prequel ----------------------------------------------------------------------
sc_slices <- paste0("s", 1:4)
sc_build <- function(aeff = data.frame(), stg_priced = TRUE) {
  invcost <- if (stg_priced) data.frame(stg.invcost = 30) else
    data.frame(out.invcost = 0)
  newModel("sc",
    repo = newRepository("sc_repo",
      newCommodity("ELC", timeframe = "SL"),
      newCommodity("WAT", timeframe = "SL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(timeslice = sc_slices, cost = 1,
                                    ava.up = c(1000, 0, 0, 0))),
      newSupply("SWAT", commodity = "WAT", supply = data.frame(cost = 2)),
      newStorage("STG", commodity = "ELC", olife = data.frame(olife = 30),
                 invcost = invcost,
                 aux = data.frame(acomm = "WAT"), aeff = aeff),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = "s3", demand = 10))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = sc_slices)),
      name = "sc_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

# test -------------------------------------------------------------------------
skip_if_no_solver()
scen <- suppressMessages(suppressWarnings(interpolate_model(
    sc_build(data.frame(acomm = "WAT", timeslice = "s2", stg.cap2ainp = 0.05)),
    name = "sc_cap", overwrite = TRUE)))
scen <- .fork_solve(scen, solver_options$glpk)
expect_true(verify_solution(scen)$ok)
expect_equal(ff_solution_sum(scen, "vStorageAInp"), 0.5, tolerance = 1e-6)
