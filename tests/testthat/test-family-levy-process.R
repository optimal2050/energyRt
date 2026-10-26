# =========================================================================== #
# Feature family: levy-process (taxes and subsidies on a PROCESS, not just a
# commodity). See R/levy.R for the routing rules.
#
# Mini-model, 1 region, ANNUAL, discount 0:
#   SUP COA at cost 1; TA (invcost 300, olife 30) and TB (invcost 3000) both
#   convert COA -> ELC 1:1; DEM 40 of ELC.
# TA is the cheap build, so the primal is pinned and closed-form:
#   vTechAct[TA] = vTechInp[TA,COA] = vTechOut[TA,ELC] = 40,
#   vTechCap[TA] = vTechNewCap[TA] = 40, base objective 440.
# A rate k on any of those bases therefore moves the objective by exactly 40k,
# which makes a wrong answer a clean ratio (2 for a double count, pPeriodLen
# for a period-scaling error) rather than an opaque number.
# =========================================================================== #

lp_build <- function(..., demand = 40) {
  objs <- list(
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "ANNUAL"),
    newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
    newTechnology("TA", input = list(comm = "COA"), output = list(comm = "ELC"),
                  invcost = list(invcost = 300), olife = list(olife = 30),
                  cap2act = 1),
    newTechnology("TB", input = list(comm = "COA"), output = list(comm = "ELC"),
                  invcost = list(invcost = 3000), olife = list(olife = 30),
                  cap2act = 1),
    newDemand("DEM", commodity = "ELC", demand = data.frame(demand = demand)),
    ...)
  newModel("lp",
    repo = do.call(newRepository, c(list("lp_repo"), objs)),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
      name = "lp_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

# two 10-year periods: the only shape that separates `cap` (charged in every
# standing year) from `ncap` (charged once on commissioning)
lp_build_mp <- function(..., demand = c(40, 40)) {
  objs <- list(
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "ANNUAL"),
    newSupply("SUP", commodity = "COA", supply = data.frame(cost = 1)),
    newTechnology("TA", input = list(comm = "COA"), output = list(comm = "ELC"),
                  invcost = list(invcost = 300), olife = list(olife = 30),
                  cap2act = 1),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(year = c(2025, 2039), demand = demand)), ...)
  newModel("lpm",
    repo = do.call(newRepository, c(list("lpm_repo"), objs)),
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
      name = "lpm_cal"),
    region = "R1", horizon = newHorizon(2025:2044, intervals = c(10, 10)),
    discount = 0)
}

lp_interp <- function(mod, tag) {
  suppressMessages(suppressWarnings(
    interpolate_model(mod, name = tag, overwrite = TRUE)))
}
lp_solve <- function(mod, tag) {
  .fork_solve(lp_interp(mod, tag), solver_options$glpk)
}
lp_rows <- function(scen, par) {
  d <- tryCatch(get_data_slot(scen@modInp@parameters[[par]]),
                error = function(e) NULL)
  if (is.null(d)) 0L else nrow(d)
}

LP_BASE <- 440

# --------------------------------------------------------------------------- #
# the five bases, each against a hand-computed quantity
# --------------------------------------------------------------------------- #

# @covers pTaxProcAct vTaxCost eqTaxCost depth=S backends=glpk
test_that("family levy-process: act tax charges the process's own activity", {
  skip_if_no_solver()
  scen <- lp_solve(lp_build(newTax("LACT", process = "TA",
                                   tax = data.frame(act = 0.5))), "lp_act")
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vTaxCost"), 20, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), LP_BASE + 20, tolerance = 1e-6)
})

# @covers pTaxProcOut depth=S backends=glpk
test_that("family levy-process: out tax charges the process's own output", {
  skip_if_no_solver()
  scen <- lp_solve(lp_build(newTax("LOUT", process = "TA", comm = "ELC",
                                   tax = data.frame(out = 0.5))), "lp_out")
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vTaxCost"), 20, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), LP_BASE + 20, tolerance = 1e-6)
})

# @covers pTaxProcInp depth=S backends=glpk
test_that("family levy-process: inp tax charges the process's own input", {
  skip_if_no_solver()
  scen <- lp_solve(lp_build(newTax("LINP", process = "TA", comm = "COA",
                                   tax = data.frame(inp = 0.25))), "lp_inp")
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vTaxCost"), 10, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), LP_BASE + 10, tolerance = 1e-6)
})

# @covers pTaxProcCap depth=S backends=glpk
test_that("family levy-process: cap tax charges installed capacity", {
  skip_if_no_solver()
  scen <- lp_solve(lp_build(newTax("LCAP", process = "TA",
                                   tax = data.frame(cap = 2))), "lp_cap")
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vTaxCost"), 80, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), LP_BASE + 80, tolerance = 1e-6)
})

# @covers pSubProcNcap vSubsCost eqSubsCost depth=S backends=glpk
test_that("family levy-process: ncap subsidy credits capacity added", {
  skip_if_no_solver()
  scen <- lp_solve(lp_build(newSubsidy("LNCAP", process = "TA",
                                   subsidy = data.frame(ncap = 1))), "lp_ncap")
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vSubsCost"), -40, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), LP_BASE - 40, tolerance = 1e-6)
})

# --------------------------------------------------------------------------- #
# period scaling: `cap` recurs, `ncap` does not. A single-year horizon has
# pPeriodLen 1 and cannot tell the two apart, nor catch an L^2 error.
# --------------------------------------------------------------------------- #

# @covers pTaxProcCap pPeriodLen depth=X backends=glpk
test_that("family levy-process: cap recurs in every standing year", {
  skip_if_no_solver()
  b <- lp_solve(lp_build_mp(), "lpm_b")
  s <- lp_solve(lp_build_mp(newTax("MCAP", process = "TA",
                                   tax = data.frame(cap = 2))), "lpm_cap")
  expect_true(verify_solution(s)$ok)
  # milestone years 2025/2030/2039 have pPeriodLen 1/9/10 and 40 units stand
  # throughout: 2 * 40 * (1 + 9 + 10) = 1600
  expect_equal(.fork_objective(s) - .fork_objective(b), 1600, tolerance = 1e-6)
})

# @covers pTaxProcNcap depth=X backends=glpk
test_that("family levy-process: ncap charges each unit added exactly once", {
  skip_if_no_solver()
  # demand grows, so capacity is added in a 10-year period too -- the case a
  # pPeriodLen error would scale by 10 while leaving the first period right
  b <- lp_solve(lp_build_mp(demand = c(40, 80)), "lpm_g")
  s <- lp_solve(lp_build_mp(newTax("MNCAP", process = "TA",
                                   tax = data.frame(ncap = 2)),
                            demand = c(40, 80)), "lpm_gn")
  expect_true(verify_solution(s)$ok)
  bi <- lp_interp(lp_build_mp(demand = c(40, 80)), "lpm_gi")
  plen <- get_data_slot(bi@modInp@parameters$pPeriodLen)
  nc <- get_data_slot(b@modOut@variables[["vTechNewCap"]])
  # vTechNewCap is a per-YEAR rate, so capacity added = sum(rate * pPeriodLen)
  added <- sum(nc$value * plen$value[match(nc$year, plen$year)])
  expect_equal(added, 80, tolerance = 1e-6)
  expect_equal(.fork_objective(s) - .fork_objective(b), 2 * added,
               tolerance = 1e-6)
})

# --------------------------------------------------------------------------- #
# routing: `inp`/`out` reach the commodity parameters or the process ones,
# never both. This is the double-count guard; the linearity identity alone
# would hold even if both were charged.
# --------------------------------------------------------------------------- #

# @covers pTaxCostOut pTaxProcOut depth=X backends=glpk
test_that("family levy-process: out routes to exactly one parameter", {
  skip_if_no_solver()
  cm <- lp_interp(lp_build(newTax("C", comm = "ELC",
                                  tax = data.frame(out = 0.5))), "lp_rc")
  pr <- lp_interp(lp_build(newTax("P", comm = "ELC", process = "TA",
                                  tax = data.frame(out = 0.5))), "lp_rp")
  # no process filter -> commodity total only
  expect_equal(lp_rows(cm, "pTaxCostOut"), 1L)
  expect_equal(lp_rows(cm, "pTaxProcOut"), 0L)
  # process filter -> the process's own output only
  expect_equal(lp_rows(pr, "pTaxCostOut"), 0L)
  expect_equal(lp_rows(pr, "pTaxProcOut"), 1L)

  # and the two instruments are additive, each worth its own 20
  bo <- lp_solve(lp_build(newTax("C", comm = "ELC", tax = data.frame(out = 0.5)),
                          newTax("P", comm = "ELC", process = "TA",
                                 tax = data.frame(out = 0.5))), "lp_rb")
  expect_true(verify_solution(bo)$ok)
  expect_equal(.fork_objective(bo), LP_BASE + 40, tolerance = 1e-6)
  expect_equal(nrow(get_data_slot(bo@modOut@variables[["vTaxCost"]])), 2L)
})

# @covers pTaxProcAct depth=S backends=glpk
test_that("family levy-process: a zero rate is a bit-for-bit no-op", {
  skip_if_no_solver()
  b <- lp_solve(lp_build(), "lp_z0")
  z <- lp_solve(lp_build(newTax("Z", process = "TA",
                                tax = data.frame(act = 0))), "lp_z1")
  expect_equal(.fork_objective(z), .fork_objective(b))
  expect_equal(lp_rows(z, "pTaxProcAct"), 0L)
})

# --------------------------------------------------------------------------- #
# targeting
# --------------------------------------------------------------------------- #

# @covers pTaxProcAct depth=S backends=glpk
test_that("family levy-process: classes and regex select the same technology", {
  skip_if_no_solver()
  byc <- lp_solve(lp_build(newTax("BC", classes = "technology",
                                  tax = data.frame(act = 0.5))), "lp_tc")
  byr <- lp_solve(lp_build(newTax("BR", process_ = "^T",
                                  tax = data.frame(act = 0.5))), "lp_tr")
  # only TA runs, so both select TA+TB but charge 40 units of activity
  expect_equal(.fork_objective(byc), LP_BASE + 20, tolerance = 1e-6)
  expect_equal(.fork_objective(byr), LP_BASE + 20, tolerance = 1e-6)
})

# @covers pTaxProcAct depth=I backends=glpk
test_that("family levy-process: a per-process rate row overrides the roster", {
  scen <- lp_interp(lp_build(newTax("PR", process_ = "^T",
    tax = data.frame(process = c("TA", "TB"), act = c(0.5, 0.25)))), "lp_pr")
  d <- get_data_slot(scen@modInp@parameters$pTaxProcAct)
  expect_equal(nrow(d), 2L)
  expect_equal(d$value[match(c("TA", "TB"), d$process)], c(0.5, 0.25))
})

# --------------------------------------------------------------------------- #
# refusals: every one of these would otherwise be a silent zero or a double
# charge, which is the failure mode this family exists to prevent
# --------------------------------------------------------------------------- #

test_that("family levy-process: meaningless rate/target combinations refuse", {
  expect_error(
    lp_interp(lp_build(newTax("E1", comm = "ELC", process = "TA",
                              tax = data.frame(bal = 1))), "lp_e1"),
    "`bal` cannot be combined with process targeting")
  expect_error(
    lp_interp(lp_build(newTax("E2", process = "NOPE",
                              tax = data.frame(act = 1))), "lp_e2"),
    "selects no process")
  expect_error(
    lp_interp(lp_build(newTax("E3", process_ = "^ZZZ",
                              tax = data.frame(act = 1))), "lp_e3"),
    "selects no process")
  expect_error(
    lp_interp(lp_build(newTax("E4", classes = "supply",
                              tax = data.frame(act = 1))), "lp_e4"),
    "is not supported yet")
  expect_error(
    lp_interp(lp_build(newTax("E5", vintage = "2099",
                              tax = data.frame(act = 1))), "lp_e5"),
    "selects no process")
  expect_error(
    lp_interp(lp_build(newTax("E6", classes = "storage",
                              tax = data.frame(act = 1))), "lp_e6"),
    "`act` is not defined for storage")
  expect_error(
    lp_interp(lp_build(newTax("E7", variable = "vTechAct",
                              tax = data.frame(rate = 1))), "lp_e7"),
    "is not implemented yet")
  expect_error(
    lp_interp(lp_build(newTax("E8", process = "TA",
                              tax = data.frame(year = 2025))), "lp_e8"),
    "sets no rate")
})

# =========================================================================== #
# Storage. Shift model: ELC supplied at s1 only (cost 1, ava.up 1000), demand
# 10 at s3, so the storage carries exactly 10 units s1 -> s3 and
#   vStorageInp[s1] = vStorageOut[s3] = vStorageOutCap = vStorageOutNewCap = 10.
# pTimesliceWeight is 1 on a full calendar, so a rate k is worth exactly 10k.
#
# SUBSIDIES on storage are mostly unbounded and that is a property of the
# model, not of the levy:
#   * `out` on a LOSSLESS storage -- it charges from its own discharge, a free
#     loop, so each cycle collects the subsidy again (the reason
#     test-family-tax-subsidy.R excludes storage altogether);
#   * `ncap`/`cap` when the capacity is free to build -- build without limit,
#     collect without limit.
# Only a capacity subsidy BELOW the capacity's own annualised cost is bounded,
# so the subsidy case prices the storage (stg.invcost 300 / olife 30,
# discount 0 -> EAC 10/unit/year, base objective 110).
# =========================================================================== #

lg_slices <- paste0("s", 1:4)

lg_stg <- function(..., inv = 0) {
  objs <- list(
    newCommodity("ELC", timeframe = "SL"),
    newSupply("SUP", commodity = "ELC",
              supply = data.frame(timeslice = lg_slices, cost = 1,
                                  ava.up = c(1000, 0, 0, 0))),
    newStorage("STG", commodity = "ELC", olife = data.frame(olife = 30),
               invcost = data.frame(stg.invcost = inv)),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(timeslice = "s3", demand = 10)), ...)
  newModel("lgs",
    repo = do.call(newRepository, c(list("lgs_repo"), objs)),
    calendar = newCalendar(timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", SL = lg_slices)), name = "lgs_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

LG_STG_BASE <- 10    # free capacity
LG_STG_PRICED <- 110 # stg.invcost 300 / olife 30 -> 10 units x EAC 10

# @covers pTaxProcInp vStorageInp depth=S backends=glpk
test_that("family levy-process: inp tax charges what a storage takes in", {
  skip_if_no_solver()
  scen <- lp_solve(lg_stg(newTax("SI", process = "STG", comm = "ELC",
                                 tax = data.frame(inp = 0.5))), "lg_si")
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vTaxCost"), 5, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), LG_STG_BASE + 5, tolerance = 1e-6)
})

# @covers pTaxProcOut vStorageOut depth=S backends=glpk
test_that("family levy-process: out tax charges what a storage gives back", {
  skip_if_no_solver()
  scen <- lp_solve(lg_stg(newTax("SO", process = "STG", comm = "ELC",
                                 tax = data.frame(out = 0.5))), "lg_so")
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vTaxCost"), 5, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), LG_STG_BASE + 5, tolerance = 1e-6)
})

# @covers pTaxProcCap vStorageStgCap depth=S backends=glpk
test_that("family levy-process: stg.cap tax on a storage reads its reservoir capacity", {
  skip_if_no_solver()
  scen <- lp_solve(lg_stg(newTax("SC", process = "STG",
                                 tax = data.frame(stg.cap = 2))), "lg_sc")
  expect_true(verify_solution(scen)$ok)
  # the reservoir: vStorageStgCap, 10 here
  expect_equal(ff_solution_sum(scen, "vTaxCost"), 20, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), LG_STG_BASE + 20, tolerance = 1e-6)
})

# @covers pTaxProcNcap vStorageStgNewCap depth=S backends=glpk
test_that("family levy-process: stg.ncap tax on a storage reads new reservoir capacity", {
  skip_if_no_solver()
  scen <- lp_solve(lg_stg(newTax("SN", process = "STG",
                                 tax = data.frame(stg.ncap = 2))), "lg_sn")
  expect_true(verify_solution(scen)$ok)
  expect_equal(ff_solution_sum(scen, "vTaxCost"), 20, tolerance = 1e-6)
  expect_equal(.fork_objective(scen), LG_STG_BASE + 20, tolerance = 1e-6)
})

# @covers pSubProcNcap vStorageStgNewCap depth=S backends=glpk
test_that("family levy-process: stg.ncap subsidy credits a priced storage's capacity", {
  skip_if_no_solver()
  b <- lp_solve(lg_stg(inv = 300), "lg_sb")
  expect_equal(.fork_objective(b), LG_STG_PRICED, tolerance = 1e-6)
  s <- lp_solve(lg_stg(newSubsidy("SS", process = "STG",
                              subsidy = data.frame(stg.ncap = 1)), inv = 300),
                "lg_ss")
  expect_true(verify_solution(s)$ok)
  expect_equal(ff_solution_sum(s, "vSubsCost"), -10, tolerance = 1e-6)
  expect_equal(.fork_objective(s), LG_STG_PRICED - 10, tolerance = 1e-6)
})

# @covers pSubProcNcap depth=S backends=glpk
test_that("family levy-process: a capacity subsidy on free capacity is unbounded", {
  skip_if_no_solver()
  # Pinned, not fixed: with stg.invcost 0 there is nothing to trade off against
  # the credit, so the LP builds capacity without limit. Process targeting
  # widens this exposure -- it reaches capacity, which commodity levies never
  # could -- and the only defence is the modeller's, so it must stay visible.
  # An UNBOUNDED LP is not an infeasible one: glpsol writes no solution at all
  # ("unable to recover undefined or non-optimal solution"), so the read fails
  # rather than returning a scenario with an NA objective.
  expect_error(
    lp_solve(lg_stg(newSubsidy("SU", process = "STG",
                           subsidy = data.frame(stg.ncap = 1)), inv = 0), "lg_su"),
    "Solution files not found")
})

# @covers pTaxProcOut depth=S backends=glpk
test_that("family levy-process: classes = storage selects the storage alone", {
  skip_if_no_solver()
  scen <- lp_solve(lg_stg(newTax("SK", classes = "storage", comm = "ELC",
                                 tax = data.frame(out = 0.5))), "lg_sk")
  expect_true(verify_solution(scen)$ok)
  expect_equal(.fork_objective(scen), LG_STG_BASE + 5, tolerance = 1e-6)
})

# --- storage parts: the charger and the discharger ------------------------------

# a storage with a charger and a discharger: a price on each part makes it exist;
# the charger's annualised cost (30 / 30 years) stays above the 0.5 subsidy
lg_stg_parts <- function(...) {
  objs <- list(
    newCommodity("ELC", timeframe = "SL"),
    newSupply("SUP", commodity = "ELC",
              supply = data.frame(timeslice = lg_slices, cost = 1,
                                  ava.up = c(1000, 0, 0, 0))),
    newStorage("STG", commodity = "ELC", olife = data.frame(olife = 30),
               invcost = data.frame(stg.invcost = 0, inp.invcost = 30,
                                    out.invcost = 1)),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(timeslice = "s3", demand = 10)), ...)
  newModel("lgp",
    repo = do.call(newRepository, c(list("lgp_repo"), objs)),
    calendar = newCalendar(timetable = make_timetable(
      struct = list(ANNUAL = "ANNUAL", SL = lg_slices)), name = "lgp_cal"),
    region = "R1", horizon = newHorizon(2025), discount = 0)
}

# @covers pTaxProcOutCap pTaxProcInpNcap vStorageOutCap vStorageInpNewCap depth=S backends=glpk
test_that("family levy-process: out.cap and inp.ncap charge the discharger and the charger", {
  skip_if_no_solver()
  scen <- lp_solve(lg_stg_parts(
    newTax("PO", process = "STG", tax = data.frame(out.cap = 2)),
    newSubsidy("PI", process = "STG", subsidy = data.frame(inp.ncap = 0.5))),
    "lg_parts")
  expect_true(verify_solution(scen)$ok)
  oc <- ff_solution_sum(scen, "vStorageOutCap")
  inc <- ff_solution_sum(scen, "vStorageInpNewCap")
  expect_gt(oc, 0)
  expect_gt(inc, 0)
  expect_equal(ff_solution_sum(scen, "vTaxCost"), 2 * oc, tolerance = 1e-6)
  expect_equal(ff_solution_sum(scen, "vSubsCost"), -0.5 * inc, tolerance = 1e-6)
})

test_that("family levy-process: part columns need a storage; cap and ncap charge technologies", {
  expect_error(lp_interp(lp_build(newTax("PT", process = "TA",
                                         tax = data.frame(out.cap = 1))), "lg_pt"),
               "applies to storages")
  # storages selected by class or by name with cap/ncap alone: an error
  expect_error(lp_interp(lp_build(newTax("PK", classes = c("technology", "storage"),
                                         tax = data.frame(ncap = 1))), "lg_pk"),
               "charge technologies and trades")
  expect_error(lp_interp(lg_stg(newTax("PN", process = "STG",
                                       tax = data.frame(cap = 1))), "lg_pn"),
               "charge technologies and trades")
})

# @covers pTaxProcCap depth=S backends=glpk
test_that("family levy-process: cap without a filter does not charge a storage", {
  skip_if_no_solver()
  scen <- lp_solve(lg_stg(newTax("PD", tax = data.frame(cap = 2))), "lg_pd")
  expect_true(verify_solution(scen)$ok)
  expect_equal(.fork_objective(scen), LG_STG_BASE, tolerance = 1e-6)
})

# --- trade: capacity only ---------------------------------------------------------

# @covers pTaxProcCap vTradeCap depth=S backends=glpk
test_that("family levy-process: cap on a trade charges its capacity in each region levied", {
  skip_if_no_solver()
  routes <- data.frame(src = "R1", dst = "R2")
  # base: capacity 10 fixed, 10 imported at 1
  one <- lp_solve(add(tr_model(routes, cap = 10), newRepository("trl1",
    newTax("TT1", classes = "trade", region = "R1", tax = data.frame(cap = 2)))), "trl1")
  expect_equal(ff_solution_sum(one, "vTaxCost"), 2 * 10, tolerance = 1e-6)
  expect_equal(.fork_objective(one), 10 + 20, tolerance = 1e-6)
  # no region: each region of the trade, as its own costs are booked
  both <- lp_solve(add(tr_model(routes, cap = 10), newRepository("trl2",
    newTax("TT2", classes = "trade", tax = data.frame(cap = 2)))), "trl2")
  expect_equal(ff_solution_sum(both, "vTaxCost"), 2 * 2 * 10, tolerance = 1e-6)
})

# @covers pSubProcNcap vTradeNewCap depth=S backends=glpk
test_that("family levy-process: ncap subsidy on a named trade credits its new capacity", {
  skip_if_no_solver()
  routes <- data.frame(src = "R1", dst = "R2")
  scen <- lp_solve(add(tr_model(routes, cap = 10), newRepository("trl3",
    newSubsidy("TS", process = "TRD", region = "R1",
               subsidy = data.frame(ncap = 0.5)))), "trl3")
  expect_equal(ff_solution_sum(scen, "vSubsCost"), -0.5 * 10, tolerance = 1e-6)
})

test_that("family levy-process: a trade is levied on its capacity only", {
  routes <- data.frame(src = "R1", dst = "R2")
  expect_error(lp_interp(add(tr_model(routes), newRepository("trl4",
    newTax("TF", classes = "trade", comm = "ELC", tax = data.frame(out = 1)))), "trl4"),
    "capacity only")
})
