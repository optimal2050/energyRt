# =========================================================================== #
# Fixtures for the time-discretization invariance suites:
#   test-multiyear-invariance.R  -- milestone granularity (pPeriodLen)
#   test-sampling-invariance.R   -- timeslice sampling (year_fraction)
#
# The unit-model idea: every parameter is a small integer and the discount
# rate is 0, so each expected objective is exact arithmetic (years x units x
# unit cost) and any granularity-dependent factor (a stray pPeriodLen, a
# missing year_fraction weight) shows up as an integer-ratio failure that
# names its own cause.
#
# The core invariant: a model whose parameters are constant in time must
# yield the SAME undiscounted totals whether the horizon is discretized in
# 1-, 5- or 10-year milestones, and whether the year is solved in full or on
# a weighted timeslice sample.
# =========================================================================== #

# ---- multi-year fixture ----------------------------------------------------

IV_YEARS  <- 2021:2030          # 10 years
IV_DEMAND <- 10                 # units of ELC per year

# Horizon of `g`-year milestones over IV_YEARS. The base-year forcing is off
# so every granularity has uniform intervals (1x10, 5x2, 10x1).
iv_horizon <- function(g) {
  newHorizon(IV_YEARS, intervals = rep(g, length(IV_YEARS) / g),
             force_BY_interval_to_1_year = FALSE)
}

# One-region annual-resolution model. Commodities are ANNUAL so every total
# is per-year; GAS carries a unit CO2 emission factor for the cap tests.
# `...` receives the technology/constraint objects under test.
iv_model <- function(..., g = 1, name = "iv", fuel_cost = 0) {
  cal <- newCalendar(
    timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
    name = paste0("cal_iv", g)
  )
  newModel(
    name = name, desc = "", calendar = cal, region = "R1",
    horizon = iv_horizon(g), discount = 0,
    repo = newRepository(
      paste0("repo_", name, g),
      newCommodity("GAS", timeframe = "ANNUAL",
                   emis = data.frame(comm = "CO2", emis = 1)),
      newCommodity("BIO", timeframe = "ANNUAL"),
      newCommodity("CO2", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "ANNUAL"),
      newSupply("SUP_GAS", commodity = "GAS",
                supply = data.frame(region = "R1", cost = fuel_cost)),
      newSupply("SUP_BIO", commodity = "BIO",
                supply = data.frame(region = "R1", cost = 0)),
      newDemand("DEM_ELC", commodity = "ELC",
                demand = data.frame(region = "R1", demand = IV_DEMAND)),
      ...
    )
  )
}

# The workhorse technology: GAS -> ELC at efficiency 1, annual, cap2act 1,
# long-lived by default. Cost/capacity slots come from `...`.
iv_tech <- function(name = "E1", input_comm = "GAS", olife = 100L, ...) {
  newTechnology(
    name, input = list(comm = input_comm), output = list(comm = "ELC"),
    ceff = data.frame(comm = input_comm, cinp2use = 1),
    cap2act = 1, vintage = data.frame(olife = olife), ...
  )
}

iv_solve <- function(mod, name) {
  scen <- suppressMessages(suppressWarnings(
    interpolate_model(mod, name = name, ondisk = FALSE)
  ))
  suppressMessages(suppressWarnings(
    solve_scenario(scen, solver = solver_options$glpk, wait = TRUE,
                   tmp.del = TRUE, force = TRUE)
  ))
}

iv_obj <- function(sol) {
  sum(suppressMessages(getData(sol, "vObjective", merge = TRUE))$value)
}

# Sum of a solution variable; `plen` (the uniform milestone length g)
# converts annual quantities to horizon totals.
iv_total <- function(sol, v, plen = 1) {
  d <- tryCatch(suppressMessages(getData(sol, v, merge = TRUE)),
                error = function(e) NULL)
  if (is.null(d) || !NROW(d)) return(0)
  sum(d$value) * plen
}

# Solve one model builder across the three granularities; returns the named
# objective vector c(g1=, g5=, g10=).
iv_sweep <- function(build, tag) {
  vapply(c(1, 5, 10), function(g) {
    iv_obj(iv_solve(build(g), paste0(tag, "_g", g)))
  }, numeric(1)) |> stats::setNames(c("g1", "g5", "g10"))
}
