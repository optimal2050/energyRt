# =========================================================================== #
# Timeslice-sampling contract for ANNUAL-quantity couplings.
#
# test-subset_slices.R establishes the base check: on grouped-identical data
# a weighted sample (year_fraction < 1, top-slice weight 1/yf) reproduces
# the full objective exactly. This file pins the two halves of the timeframe
# contract around that:
#   (a) per-slice variables are RAW -- operation at native resolution,
#       identical between full and sampled calendars, so hourly/seasonal
#       operation stays visible and directly constrainable;
#   (b) sums that cross into the ANNUAL timeframe carry the annualisation
#       weight (child/parent weight ratio; a no-op on a full calendar).
# Cost roll-ups and the cumulative Row bounds implement (b) already; the
# annual emission total and ANNUAL-timeframe process sizing do not yet --
# those tests assert the target contract and fail by exactly 1/yf until the
# aggregation weights land.
#
# Fixture: one year, four identical seasons; sampled twin keeps two seasons
# (year_fraction = 1/2), so any weight defect shows as an exact factor 2.
# =========================================================================== #

sv_seasons <- c("WIN", "SPR", "SUM", "AUT")

sv_cal <- function(keep = sv_seasons, name = "svfull") {
  tt <- make_timetable(struct = list(ANNUAL = "ANNUAL", SEASON = sv_seasons))
  tt1 <- tt[tt$SEASON %in% keep, ]
  newCalendar(timetable = tt1, name = name,
              year_fraction = sum(tt1$share))
}

# One year, one region. ELC is seasonal (10 per season); GAS carries a unit
# CO2 factor; HEA is ANNUAL for the annual-process test.
sv_model <- function(..., cal, name = "sv", fuel_cost = 1) {
  newModel(
    name = name, desc = "", calendar = cal, region = "R1",
    horizon = newHorizon(2020), discount = 0,
    repo = newRepository(
      paste0("repo_", name),
      newCommodity("GAS", timeframe = "ANNUAL",
                   emis = data.frame(comm = "CO2", emis = 1)),
      newCommodity("BIO", timeframe = "ANNUAL"),
      newCommodity("CO2", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SEASON"),
      newSupply("SUP_GAS", commodity = "GAS",
                supply = data.frame(region = "R1", cost = fuel_cost)),
      newSupply("SUP_BIO", commodity = "BIO",
                supply = data.frame(region = "R1", cost = 0)),
      newDemand("DEM_ELC", commodity = "ELC",
                demand = data.frame(region = "R1", timeslice = sv_seasons,
                                    demand = 10)),
      ...
    )
  )
}

sv_tech <- function(name = "E1", input_comm = "GAS", ...) {
  newTechnology(
    name, input = list(comm = input_comm), output = list(comm = "ELC"),
    ceff = data.frame(comm = input_comm, cinp2use = 1),
    cap2act = 1, vintage = data.frame(olife = 100L), ...
  )
}

# Demand rows exist for all four seasons; the sampled calendar serves only
# the kept ones, weighted back to a full year.
sv_solve <- function(mod, name) {
  scen <- suppressMessages(suppressWarnings(
    interpolate_model(mod, name = name, ondisk = FALSE)
  ))
  suppressMessages(suppressWarnings(
    solve_scenario(scen, solver = solver_options$glpk, wait = TRUE,
                   tmp.del = TRUE, force = TRUE)
  ))
}

sv_obj <- function(sol) {
  sum(suppressMessages(getData(sol, "vObjective", merge = TRUE))$value)
}

# ---------------------------------------------------------------------------
test_that("per-slice operation is raw; annual emission totals annualise", {
  skip_if_no_solver()
  # dirty tech serves 10/season at emis 1. Contract (a): seasonal fuel use
  # on the slices PRESENT in the sample is identical to the full calendar
  # (raw operation, no weight). Contract (b): the ANNUAL total vEmsFuelTot
  # reads 40 on both calendars (weights annualise the sample).
  mk <- function(cal, nm) sv_model(
    sv_tech(capacity = data.frame(cap.fx = 40)), cal = cal, name = nm)
  ems <- function(sol)
    sum(suppressMessages(getData(sol, "vEmsFuelTot", merge = TRUE))$value)
  inp_win <- function(sol) {
    d <- suppressMessages(getData(sol, "vTechInp", merge = TRUE))
    sum(d$value[d$tech == "E1" & d$timeslice == "WIN"])
  }
  s_full <- sv_solve(mk(sv_cal(), "svef"), "svef")
  s_half <- sv_solve(mk(sv_cal(c("WIN", "SUM"), "svhalf"), "sveh"), "sveh")
  expect_equal(inp_win(s_full), 10, tolerance = 1e-9)
  expect_equal(inp_win(s_half), inp_win(s_full), tolerance = 1e-9,
               info = "raw per-slice operation, sampled slice == full")
  expect_equal(ems(s_full), 40, tolerance = 1e-9)
  expect_equal(ems(s_half), ems(s_full), tolerance = 1e-9,
               info = "annualised emissions, yf = 1/2 sample vs full")
})

# ---------------------------------------------------------------------------
test_that("an annual CO2 cap binds identically on a sampled calendar", {
  skip_if_no_solver()
  # dirty (fuel 1, emis 1) vs clean (varom 5); demand 40/yr; cap 16/yr.
  # Optimum splits dirty 16 / clean 24 -> objective 16x1 + 24x5 = 136,
  # full and sampled alike. A cap row missing the annualisation weight
  # loosens to 2x on the half-year sample (objective drops toward 40).
  cap <- newConstraint(
    name = "CO2CAP", eq = "<=",
    for.each = data.frame(year = 2020, comm = "CO2"),
    term1 = list(variable = "vEmsFuelTot"),
    rhs = data.frame(year = 2020, rhs = 16),
    defVal = Inf)
  mk <- function(cal, nm) sv_model(
    sv_tech(capacity = data.frame(cap.fx = 40)),
    sv_tech(name = "ECLN", input_comm = "BIO",
            varom = data.frame(varom = 5),
            capacity = data.frame(cap.fx = 40)),
    cap, cal = cal, name = nm)
  o_full <- sv_obj(sv_solve(mk(sv_cal(), "svcf"), "svcf"))
  o_half <- sv_obj(sv_solve(mk(sv_cal(c("WIN", "SUM"), "svch"), "svch"),
                            "svch"))
  expect_equal(o_full, 136, tolerance = 1e-9)
  expect_equal(o_half, o_full, tolerance = 1e-9,
               info = "capped objective, yf = 1/2 sample vs full")
})

# ---------------------------------------------------------------------------
test_that("ANNUAL-timeframe processes are sampling-invariant", {
  skip_if_no_solver()
  # A fully ANNUAL chain (GAS -> HEA, annual demand 10, invcost 7): capacity,
  # investment and objective must not depend on the calendar sample. A
  # missing annualisation weight at the ANNUAL level inflates capacity and
  # objective by exactly 1/yf.
  mk <- function(cal, nm) newModel(
    name = nm, desc = "", calendar = cal, region = "R1",
    horizon = newHorizon(2020), discount = 0,
    repo = newRepository(
      paste0("repo_", nm),
      newCommodity("GAS", timeframe = "ANNUAL"),
      newCommodity("HEA", timeframe = "ANNUAL"),
      newSupply("SUP_GAS", commodity = "GAS",
                supply = data.frame(region = "R1", cost = 1)),
      newDemand("DEM_HEA", commodity = "HEA",
                demand = data.frame(region = "R1", demand = 10)),
      newTechnology("EANN", input = list(comm = "GAS"),
                    output = list(comm = "HEA"),
                    ceff = data.frame(comm = "GAS", cinp2use = 1),
                    cap2act = 1, vintage = data.frame(olife = 100L),
                    invcost = data.frame(invcost = 7))
    )
  )
  cap_of <- function(sol) {
    d <- suppressMessages(getData(sol, "vTechCap", merge = TRUE))
    max(d$value[d$tech == "EANN"])
  }
  s_full <- sv_solve(mk(sv_cal(), "svaf"), "svaf")
  s_half <- sv_solve(mk(sv_cal(c("WIN", "SUM"), "svah"), "svah"), "svah")
  expect_equal(cap_of(s_full), 10, tolerance = 1e-9)
  expect_equal(cap_of(s_half), cap_of(s_full), tolerance = 1e-9,
               info = "ANNUAL tech capacity, yf = 1/2 sample vs full")
  expect_equal(sv_obj(s_half), sv_obj(s_full), tolerance = 1e-9,
               info = "ANNUAL chain objective, yf = 1/2 sample vs full")
})
