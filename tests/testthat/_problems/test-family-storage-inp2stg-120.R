# Extracted from test-family-storage-inp2stg.R:120

# prequel ----------------------------------------------------------------------
si2_slices <- paste0("s", 1:4)
si2_build <- function(inp2stg = NULL, inp_invcost = 300, stg_invcost = 30) {
  inv <- data.frame(stg.invcost = stg_invcost)
  if (!is.null(inp_invcost)) inv$inp.invcost <- inp_invcost
  newModel("si2",
    repo = newRepository("si2_repo",
      newCommodity("ELC", timeframe = "SL"),
      newSupply("SUP", commodity = "ELC",
                supply = data.frame(timeslice = si2_slices, cost = 1,
                                    ava.up = c(1000, 0, 0, 0))),
      newStorage("STG", commodity = "ELC",
                 olife = data.frame(olife = 30),
                 duration = data.frame(duration.lo = 0, duration.up = Inf),
                 invcost = inv, inp2stg = inp2stg),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(timeslice = "s3", demand = 10))),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = si2_slices)),
      name = "si2_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

# test -------------------------------------------------------------------------
skip_if_no_solver()
base <- suppressMessages(suppressWarnings(interpolate_model(
    si2_build(), name = "si2_base", overwrite = TRUE)))
base <- .fork_solve(base, solver_options$glpk)
expect_true(verify_solution(base)$ok)
expect_equal(ff_solution_sum(base, "vStorageStgCap"), 10, tolerance = 1e-6)
expect_equal(.fork_objective(base), 120, tolerance = 1e-6)
scen <- suppressMessages(suppressWarnings(interpolate_model(
    si2_build(inp2stg = data.frame(inp2stg.up = 0.5)),
    name = "si2_up", overwrite = TRUE)))
scen <- .fork_solve(scen, solver_options$glpk)
expect_true(verify_solution(scen)$ok)
expect_equal(ff_solution_sum(scen, "vStorageInpCap"), 10, tolerance = 1e-6)
expect_equal(ff_solution_sum(scen, "vStorageStgCap"), 20, tolerance = 1e-6)
