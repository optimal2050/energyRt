# =========================================================================== #
# getData() aggregation-level consistency ("roll-up"): the ANNUAL level
# reported by timeframe = "lowest" must equal the plain sum over timeslices of
# timeframe = "highest" (the model's internal convention: pTimesliceAgg is the
# weight RATIO child/parent, which is 1 on both full and uniformly sampled
# calendars, so fine -> coarse aggregation is a plain sum). timeframe = "all"
# must contain both levels unchanged.
#
# This is the first direct test of get_data.R's level handling; golden
# tracked-value tables (helper-goldens.R) rely on exactly this convention.
# =========================================================================== #

.ru_levels <- function(scen, name) {
  hi <- data.table::as.data.table(
    getData(scen, name, merge = TRUE, timeframe = "highest"))
  lo <- data.table::as.data.table(
    getData(scen, name, merge = TRUE, timeframe = "lowest"))
  all_ <- data.table::as.data.table(
    getData(scen, name, merge = TRUE, timeframe = "all"))
  list(hi = hi, lo = lo, all = all_)
}

# returns FALSE (without failing) when the variable has no rows in this model
.ru_expect_rollup <- function(scen, name, label) {
  lv <- .ru_levels(scen, name)
  if (nrow(lv$hi) == 0) return(invisible(FALSE))
  grp <- setdiff(colnames(lv$hi), c("timeslice", "value"))
  agg_hi <- lv$hi[, .(value = sum(value)), by = grp]
  agg_lo <- lv$lo[, .(value = sum(value)), by = grp]
  m <- merge(agg_hi, agg_lo, by = grp, all = TRUE, suffixes = c(".hi", ".lo"))
  expect_false(any(is.na(m$value.hi) | is.na(m$value.lo)),
               label = paste0(label, ":", name, " levels cover same keys"))
  expect_equal(m$value.hi, m$value.lo, tolerance = 1e-9,
               label = paste0(label, ":", name, " lowest == sum(highest)"))
  # "all" carries both levels unchanged
  lo_slice <- unique(as.character(lv$lo$timeslice))
  in_all_lo <- lv$all[as.character(timeslice) %in% lo_slice]
  expect_equal(nrow(in_all_lo), nrow(lv$lo),
               label = paste0(label, ":", name, " 'all' contains lowest level"))
  expect_equal(nrow(lv$all), nrow(lv$lo) + nrow(lv$hi) -
                 nrow(merge(lv$hi, lv$lo, by = intersect(colnames(lv$hi), colnames(lv$lo)))),
               label = paste0(label, ":", name, " 'all' row count"))
  invisible(TRUE)
}

# @covers vTechOut vTechInp vSupOut vDemInp depth=S backends=glpk
test_that("lowest equals plain sum of highest on full-calendar tier models", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  checked <- 0L
  for (tier in c("tm_core", "tm_weather")) {
    scen <- solved_tier(tier)
    for (nm in c("vTechOut", "vTechInp", "vSupOut", "vDemInp")) {
      if (isTRUE(.ru_expect_rollup(scen, nm, tier))) checked <- checked + 1L
    }
  }
  # most tracked variables must actually have been exercised
  expect_gte(checked, 5)
})

# @covers pTimesliceShare pTimesliceWeight pYearFraction depth=S backends=glpk forks=sampled
test_that("roll-up holds on a sampled (subset) calendar", {
  skip_if_no_solver()
  K <- 12; m <- 4
  sl <- sprintf("s%02d", 1:K)
  tt <- make_timetable(struct = list(ANNUAL = "ANNUAL", TIMESLICE = sl))
  tt_sub <- tt[tt$TIMESLICE %in% sl[seq_len(m)], ]
  cal <- newCalendar(timetable = tt_sub, year_fraction = sum(tt_sub$share),
                     name = "ru_sub")
  mod <- newModel("ru_smp",
    repo = newRepository("r",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "TIMESLICE"),
      newSupply("SUP_COA", commodity = "COA",
                supply = data.frame(region = "R1", cost = 5)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    af = data.frame(af.up = 0.5),
                    invcost = data.frame(region = "R1", invcost = 1000),
                    olife = list(olife = 30), cap2act = 1),
      newDemand("DEM_ELC", commodity = "ELC",
                demand = data.frame(region = "R1", timeslice = sl[seq_len(m)],
                                    demand = 10))),
    calendar = cal, region = "R1", horizon = newHorizon(2020), discount = 0.05)
  scen <- suppressMessages(suppressWarnings(
    solve_mod(mod, name = "ru_smp", solver = solver_options$glpk,
              tmp.del = TRUE, wait = TRUE)))
  # weights annualise the sample: top slice carries weight 1/year_fraction
  w <- energyRt:::get_data_slot(scen@modInp@parameters[["pTimesliceWeight"]])
  expect_equal(unique(w$value), K / m)
  # the roll-up convention is weight-invariant: still a plain sum
  .ru_expect_rollup(scen, "vTechOut", "sampled")
  # and the solution passes the generic identities on a sampled calendar too
  expect_true(verify_solution(scen)$ok)
})
