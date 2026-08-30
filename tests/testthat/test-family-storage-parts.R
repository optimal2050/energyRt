# =========================================================================== #
# Feature families: storage-roles-capacities, storage-costs-eac (per-part),
# storage-weather. Shift model: supply at s1 (cost 1), demand 10 at s3,
# olife 30, one year, discount 0; base objective 10.
#
# Verified semantics:
#   - part capacities are POWER rates: flow bounds read
#     cap * cap2act * share * af, with cap2act defaulting to 8760 -- the af
#     tests pin cap2act = 1 on the role slot so the arithmetic is exact.
#   - capacity bounds (inp.cap.up / stg.cap.up) bind through the structural
#     part links; an unmeetable bound is infeasible.
#   - whole-storage af bounds the LEVEL against the storing capacity:
#     af.up = 0.8 -> StgCap (and the duration-linked OutCap) = 10/0.8 = 12.5.
#   - wacc / payback are STORAGE-WIDE and carried on the OUT columns
#     (compute_eac_parameters reuses pStorageOutWacc/Payback for all three
#     parts -- deliberate: "one storage, one lifetime and one wacc").
#     out.wacc = 0.1: EAC = 300 * 0.1/(1 - 1.1^-30) = 31.824/unit -> obj 328.24
#     out.payback = 10: EAC = 300/10 = 30/unit -> obj 310.
#   - KNOWN GAP (pinned): inp./stg. wacc and payback columns are accepted and
#     interpolated into pStorageInpWacc/StgWacc/InpPayback/StgPayback but
#     NEVER read -- the EAC keeps invcost/olife. Either honour them per part
#     or refuse the columns; until then the pin documents the silent ignore.
# =========================================================================== #

pt_slices <- paste0("s", 1:4)

pt_build <- function(..., sup_cost = c(1, 1, 1, 1),
                     sup_ava = c(1000, 0, 0, 0), weather_obj = NULL) {
  objs <- list(
    newCommodity("ELC", timeframe = "SL"),
    newSupply("SUP", commodity = "ELC",
              supply = data.frame(timeslice = pt_slices, cost = sup_cost,
                                  ava.up = sup_ava)),
    newStorage("STG", commodity = "ELC", olife = data.frame(olife = 30), ...),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(timeslice = "s3", demand = 10)))
  if (!is.null(weather_obj)) objs <- c(objs, list(weather_obj))
  newModel("pt",
    repo = do.call(newRepository, c(list("pt_repo"), objs)),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = pt_slices)),
      name = "pt_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

pt_solve <- function(mod, tag) {
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    mod, name = tag, overwrite = TRUE)))
  .fork_solve(scen, solver_options$glpk)
}

# @covers pStorageInpCap pStorageStgCap depth=S backends=glpk
test_that("family storage-parts: unmeetable part-capacity bounds are infeasible", {
  skip_if_no_solver()
  for (case in list(list(cap = data.frame(inp.cap.up = 5), tag = "pt_inpcap"),
                    list(cap = data.frame(stg.cap.up = 6), tag = "pt_stgcap"))) {
    scen <- pt_solve(pt_build(invcost = data.frame(stg.invcost = 0),
                              capacity = case$cap), case$tag)
    expect_true(is.na(.fork_objective(scen)),
                label = paste0(case$tag, " infeasible (bound < required)"))
  }
})

# @covers pStorageInpNewCap vStorageInpCap depth=S backends=glpk
test_that("family storage-parts: a new-capacity floor oversizes a priced charger", {
  skip_if_no_solver()
  scen <- pt_solve(pt_build(invcost = data.frame(inp.invcost = 300, stg.invcost = 0),
                            capacity = data.frame(inp.ncap.lo = 25)),
                   "pt_ncap_lo")
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vStorageInpCap"), 25, tolerance = 1e-6)
  # 10 fuel + 25 * 300/30 EAC
  expect_equal(.fork_objective(scen), 10 + 250, tolerance = 1e-6)
})

# @covers pStorageOutAf pStorageOutCap depth=S backends=glpk
test_that("family storage-parts: out.af with cap2act = 1 sizes the discharger as a rate", {
  skip_if_no_solver()
  scen <- pt_solve(pt_build(
    invcost = data.frame(out.invcost = 300, stg.invcost = 0),
    output = data.frame(cap2act = 1),
    af = data.frame(out.af.up = 0.5)), "pt_out_af")
  expect_true(verify_solution(scen)$ok)
  # discharge rate 10/0.25 = 40 <= OutCap * 1 * 0.5 -> OutCap = 80
  expect_equal(ff_solution_sum(scen, "vStorageOutCap"), 80, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), 10 + 80 * 10, tolerance = 1e-6)
})

# @covers pStorageAf vStorageStgCap depth=S backends=glpk
test_that("family storage-parts: whole-storage af bounds the level against the store", {
  skip_if_no_solver()
  scen <- pt_solve(pt_build(invcost = data.frame(stg.invcost = 0),
                            af = data.frame(af.up = 0.8)), "pt_af")
  expect_true(verify_solution(scen)$ok)
  # level 10 <= 0.8 * StgCap; duration [1,1] links OutCap to it
  expect_equal(ff_solution_sum(scen, "vStorageOutCap"), 12.5, tolerance = 1e-6)
})

# @covers pStorageOutWacc pStorageInpEac depth=S backends=glpk
test_that("family storage-parts: storage-wide wacc rides the out columns; eac bypasses annuitisation", {
  skip_if_no_solver()
  scen <- pt_solve(pt_build(invcost = data.frame(inp.invcost = 300, out.wacc = 0.1,
                                                 stg.invcost = 0)), "pt_wacc")
  expect_true(verify_solution(scen)$ok)
  crf <- 0.1 / (1 - 1.1^-30)
  expect_equal(.fork_objective(scen), 10 + 10 * 300 * crf, tolerance = 1e-4)
  # direct eac replaces the computed annuity
  scen2 <- pt_solve(pt_build(invcost = data.frame(inp.eac = 17, stg.invcost = 0)),
                    "pt_eac")
  expect_true(verify_solution(scen2)$ok)
  expect_equal(.fork_objective(scen2), 10 + 170, tolerance = 1e-6)
})

# @covers pStorageOutPayback depth=S backends=glpk
test_that("family storage-parts: storage-wide payback shortens the recovery life", {
  skip_if_no_solver()
  scen <- pt_solve(pt_build(invcost = data.frame(inp.invcost = 300,
                                                 out.payback = 10,
                                                 stg.invcost = 0)), "pt_payback")
  expect_true(verify_solution(scen)$ok)
  expect_equal(.fork_objective(scen), 10 + 10 * 300 / 10, tolerance = 1e-6)
})

# @covers pStorageInpWacc pStorageInpPayback depth=S backends=glpk
test_that("family storage-parts: a part-specific wacc/payback annuitises that part", {
  skip_if_no_solver()
  crf <- function(r, n) r / (1 - (1 + r)^-n)
  # inp.wacc alone: the charger is annuitised at ITS rate
  scen <- pt_solve(pt_build(invcost = data.frame(inp.invcost = 300,
                                                 inp.wacc = 0.1,
                                                 stg.invcost = 0)), "pt_iw")
  expect_true(verify_solution(scen)$ok)
  expect_equal(.fork_objective(scen), 10 + 10 * 300 * crf(0.1, 30),
               tolerance = 1e-4)
  # inp.payback alone: cost recovered over 10 years, not olife
  scen2 <- pt_solve(pt_build(invcost = data.frame(inp.invcost = 300,
                                                  inp.payback = 10,
                                                  stg.invcost = 0)), "pt_ip")
  expect_true(verify_solution(scen2)$ok)
  expect_equal(.fork_objective(scen2), 10 + 10 * 300 / 10, tolerance = 1e-6)
})

# @covers pStorageStgWacc pStorageStgPayback depth=S backends=glpk
test_that("family storage-parts: the part rate overrides the storage-wide out.* carrier", {
  skip_if_no_solver()
  crf <- function(r, n) r / (1 - (1 + r)^-n)
  # inp.wacc 0.1 beats out.wacc 0.2 for the charger
  scen <- pt_solve(pt_build(invcost = data.frame(inp.invcost = 300,
                                                 inp.wacc = 0.1, out.wacc = 0.2,
                                                 stg.invcost = 0)), "pt_prec")
  expect_true(verify_solution(scen)$ok)
  expect_equal(.fork_objective(scen), 10 + 10 * 300 * crf(0.1, 30),
               tolerance = 1e-4)
  # a priced store annuitised at its own stg.wacc (level 10 -> StgCap 10)
  scen2 <- pt_solve(pt_build(invcost = data.frame(stg.invcost = 50,
                                                  stg.wacc = 0.1)), "pt_sw")
  expect_true(verify_solution(scen2)$ok)
  expect_equal(.fork_objective(scen2), 10 + 10 * 50 * crf(0.1, 30),
               tolerance = 1e-4)
  # stg.payback: recovery over 5 years
  scen3 <- pt_solve(pt_build(invcost = data.frame(stg.invcost = 50,
                                                  stg.payback = 5)), "pt_sp")
  expect_true(verify_solution(scen3)$ok)
  expect_equal(.fork_objective(scen3), 10 + 10 * 50 / 5, tolerance = 1e-6)
})

# @covers pStorageInpStock pStorageOutStock pStorageStgStock depth=I backends=glpk
test_that("family storage-parts: per-part stock lands in modInp", {
  # per-part retcost (inp./out./stg.retcost) needs the retirement window
  # machinery (multi-year horizon + ret bounds) and is exercised with the
  # stock-retirement family instead
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    pt_build(invcost = data.frame(stg.invcost = 0),
             capacity = data.frame(inp.stock = 3, out.stock = 4, stg.stock = 5)),
    name = "pt_ret", overwrite = TRUE)))
  expect_equal(unique(ff_param(scen, "pStorageInpStock")$value), 3)
  expect_equal(unique(ff_param(scen, "pStorageOutStock")$value), 4)
  expect_equal(unique(ff_param(scen, "pStorageStgStock")$value), 5)
})

# @covers pStorageWeatherInpAf vStorageInp depth=S backends=glpk
test_that("family storage-parts: a weather window restricts when the storage may charge", {
  skip_if_no_solver()
  w <- newWeather("WCH", timeframe = "SL",
                  weather = data.frame(timeslice = pt_slices, wval = c(1, 0, 0, 0)))
  # supply cheaper at s2 -- without the weather window the storage charges
  # there; with inp.waf (weather 1,0,0,0) it must charge at the dearer s1
  base <- pt_solve(pt_build(invcost = data.frame(stg.invcost = 0),
                            sup_cost = c(2, 1, 1, 1),
                            sup_ava = c(1000, 1000, 0, 0)), "pt_w_base")
  expect_equal(.fork_objective(base), 10, tolerance = 1e-6)  # charges at s2
  scen <- pt_solve(pt_build(invcost = data.frame(stg.invcost = 0),
                            af = data.frame(inp.af.up = 1),
                            weather = data.frame(weather = "WCH", inp.waf.up = 1),
                            sup_cost = c(2, 1, 1, 1),
                            sup_ava = c(1000, 1000, 0, 0),
                            weather_obj = w), "pt_w_win")
  expect_true(verify_solution(scen)$ok)
  inp <- ff_solution_sum(scen, "vStorageInp", by = "timeslice")
  expect_equal(as.character(inp$timeslice), "s1")
  expect_equal(.fork_objective(scen), 20, tolerance = 1e-6)  # forced to s1 at cost 2
})


# @covers pStorageInpAfUp pStorageOutAfUp depth=S backends=glpk forks=sampled
test_that("KNOWN BUG: out-part af rows are infeasible on a year_fraction < 1 calendar", {
  skip_if_no_solver()
  # Found 2026-08-28 building the EV training chapter (one-day calendar,
  # year_fraction = 1/365): ANY `out.af.*` row -- timeslice-less or hourly --
  # makes the storage's discharge infeasible, while the identical model with
  # `inp.af.*` rows (or no af at all, or year_fraction = 1) solves. The two
  # pins below record BOTH sides; when the out-part af path is fixed, the
  # second expectation flips and this test names the intended objective.
  hrs <- sprintf("h%02d", 0:23)
  cal1d <- newCalendar(
    timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", HOUR = hrs),
                               year_fraction = 1 / 365),
    year_fraction = 1 / 365, name = "sp_one_day")
  price <- ifelse(0:23 < 6, 0.05, ifelse(0:23 >= 17 & 0:23 <= 21, 0.40, 0.15))
  mk <- function(af) {
    stg <- newStorage(name = "CAR", input = list(comm = "ELC"),
      storage = list(comm = "BAT"), output = list(comm = "KM"),
      seff = data.frame(inpeff = 0.90, outeff = 6), af = af,
      capacity = list(inp.cap.up = 7, stg.cap.up = 60, out.cap.up = 120),
      duration = data.frame(duration.lo = 0, duration.up = 1e6),
      vintage = data.frame(olife = 10L))
    newModel(name = "sp_af", region = "R1", horizon = newHorizon(2020),
      discount = 0, calendar = cal1d,
      repo = newRepository("r",
        newCommodity(name = "ELC", timeframe = "HOUR"),
        newCommodity(name = "BAT", timeframe = "HOUR"),
        newCommodity(name = "KM",  timeframe = "HOUR"),
        newSupply(name = "GRID", commodity = "ELC",
                  supply = data.frame(timeslice = hrs, cost = price)),
        stg,
        newDemand(name = "TRIPS", commodity = "KM",
                  demand = data.frame(timeslice = c("h08", "h18"),
                                      demand = 30))))
  }
  slv <- function(mod, tag) tryCatch(
    suppressMessages(suppressWarnings(
      interpolate_model(mod, name = tag, overwrite = TRUE) |> .fork_solve(solver_options$glpk))),
    error = function(e) NULL)

  # inp-part af per hour: works, and prices the commute exactly
  s_in <- slv(mk(data.frame(timeslice = hrs, inp.af.up = 1)), "sp_af_inp")
  expect_true(!is.null(s_in) && isTRUE(s_in@status$solved))
  d <- as.data.frame(get_data_slot(s_in@modOut@variables[["vObjective"]]))
  expect_equal(sum(d$value), 202.7778, tolerance = 1e-4)

  # out-part af: the SAME rating expressed on the discharge side -- currently
  # infeasible (the bug). 120 km/h x share never binds, so a fix must land on
  # the identical 202.7778 objective.
  s_out <- slv(mk(data.frame(timeslice = hrs, out.af.up = 1)), "sp_af_out")
  expect_false(!is.null(s_out) && isTRUE(s_out@status$solved),
               label = "KNOWN BUG pin: flip to expect_true + objective when fixed")
})
