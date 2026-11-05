# =========================================================================== #
# check_feasibility(): contradictions that need no solver.
#
# The function's value is entirely in what it does NOT report -- a check that
# cries wolf is worse than none -- so the suppression cases (a backstop
# exists, no ceiling is declared) matter as much as the detections.
# =========================================================================== #

# A one-region model whose only hard coupling is land per unit of standing
# capacity. `land` is how much the supply offers; `per_gw` how much a GW of
# the exogenous fleet consumes.
.cf_model <- function(land = 100, per_gw = 20, stock = 1, import = FALSE,
                      region = "R1", ret = NA_real_, opt_ret = FALSE) {
  objs <- list(
    newCommodity("ELC", timeframe = "ANNUAL"),
    newCommodity("LAND", timeframe = "ANNUAL"),
    newSupply("RES_LAND", commodity = "LAND", unit = "km2", region = region,
              supply = data.frame(region = region, cost = 0, ava.up = land)),
    newTechnology("EWIN",
      output = list(comm = "ELC", unit = "GWh"),
      aux = data.frame(acomm = "LAND", unit = "km2"),
      aeff = data.frame(acomm = "LAND", cap2ainp = per_gw),
      cap2act = 8760,
      vintage = data.frame(start = 2015L, olife = 50L),
      optimizeRetirement = opt_ret,
      capacity = if (is.na(ret)) {
        data.frame(region = region, year = 2025, stock = stock)
      } else {
        data.frame(region = region, year = 2025, stock = stock, ret.up = ret)
      }))
  if (import) {
    objs <- c(objs, list(newImport("IMP_LAND", commodity = "LAND",
                                   unit = "km2", region = region,
                                   import = data.frame(region = region,
                                                       price = 1))))
  }
  newModel("CF", region = region, optimizeRetirement = opt_ret,
           data = newRepository("cf", objs))
}

test_that("a forced aux draw above a hard ceiling is reported", {
  f <- check_feasibility(.cf_model(land = 10, per_gw = 20, stock = 1),
                         verbose = FALSE)
  expect_equal(nrow(f), 1L)
  expect_equal(f$check, "aux-from-stock")
  expect_equal(f$commodity, "LAND")
  expect_equal(f$required, 20)
  expect_equal(f$available, 10)
  expect_equal(f$shortfall, 10)
  expect_equal(f$forced_by, "EWIN")
})

test_that("a requirement the ceiling covers is not reported", {
  f <- check_feasibility(.cf_model(land = 100, per_gw = 20), verbose = FALSE)
  expect_equal(nrow(f), 0L)
})

test_that("an import makes the local ceiling non-binding, so nothing fires", {
  # the arithmetic still fails locally; the commodity can simply be bought
  f <- check_feasibility(.cf_model(land = 10, per_gw = 20, import = TRUE),
                         verbose = FALSE)
  expect_equal(nrow(f), 0L)
})

test_that("a commodity with no bounded supply has no ceiling to contradict", {
  m <- .cf_model(land = 10, per_gw = 20)
  # drop the bound, keep the supply
  sup <- newSupply("RES_LAND", commodity = "LAND", unit = "km2",
                   region = "R1", supply = data.frame(region = "R1", cost = 0))
  m <- add(m, sup, overwrite = TRUE)
  expect_equal(nrow(check_feasibility(m, verbose = FALSE)), 0L)
})

test_that("`years` narrows the report", {
  m <- .cf_model(land = 10, per_gw = 20)
  expect_equal(nrow(check_feasibility(m, years = 2025, verbose = FALSE)), 1L)
  expect_equal(nrow(check_feasibility(m, years = 2050, verbose = FALSE)), 0L)
})

test_that("a declared demand above a bounded supply is reported", {
  m <- newModel("CFD", region = "R1", data = newRepository("cfd", list(
    newCommodity("GAS", timeframe = "ANNUAL"),
    newSupply("SUP_GAS", commodity = "GAS", unit = "PJ", region = "R1",
              supply = data.frame(region = "R1", cost = 1, ava.up = 10)),
    newDemand("DEM_GAS", commodity = "GAS", unit = "PJ",
              demand = data.frame(region = "R1", year = 2025, demand = 40)))))
  f <- check_feasibility(m, verbose = FALSE)
  expect_equal(nrow(f), 1L)
  expect_equal(f$check, "demand-vs-supply")
  expect_equal(f$required, 40)
  expect_equal(f$available, 10)
})

test_that("a demand naming no year is checked, not dropped", {
  # `year` is NA in every row, and aggregate() drops NA groups: the
  # requirement vanished and the contradiction was reported as clean.
  m <- newModel("CFN", region = c("R1", "R2"), data = newRepository("cfn", list(
    newCommodity("ELC", timeframe = "ANNUAL"),
    newSupply("SUP_ELC", commodity = "ELC", unit = "GWh",
              supply = data.frame(region = c("R1", "R2"), ava.up = c(5, 100))),
    newDemand("DEM_BY", commodity = "ELC", unit = "GWh",
              demand = data.frame(region = c("R1", "R2"),
                                  timeslice = "ANNUAL", demand = c(10, 20))))))
  f <- check_feasibility(m, verbose = FALSE)
  expect_equal(nrow(f), 1L)
  expect_equal(f$region, "R1")
  expect_equal(f$required, 10)
  expect_equal(f$available, 5)
  expect_true(is.na(f$year))
  # a requirement for no particular year holds in every year, so naming one
  # must not filter it out
  expect_equal(nrow(check_feasibility(m, years = 2030, verbose = FALSE)), 1L)
})

test_that("a clean result is reported as no contradiction, not as feasible", {
  expect_message(check_feasibility(.cf_model()),
                 "NOT a feasibility guarantee")
})

test_that("the report names which side was checked", {
  expect_message(check_feasibility(.cf_model(land = 10, per_gw = 20)),
                 "\\[declared\\]")
})

test_that("tiers are not pooled: LO land cannot serve an HI process", {
  # both tiers together would cover the draw; the HI tier alone cannot
  objs <- list(
    newCommodity("ELC", timeframe = "ANNUAL"),
    newCommodity("LAND", timeframe = "ANNUAL",
                 cluster = data.frame(cluster = c("HI", "LO"), order = 1:2)),
    newSupply("RES_LAND", commodity = "LAND", unit = "km2", region = "R1",
              cluster = data.frame(cluster = c("HI", "LO"), order = 1:2),
              supply = data.frame(region = "R1", cluster = c("HI", "LO"),
                                  cost = 0, ava.up = c(10, 500))),
    newTechnology("EWIN",
      output = list(comm = "ELC", unit = "GWh"),
      cluster = data.frame(cluster = c("HI", "LO"), order = 1:2),
      aux = data.frame(acomm = "LAND", unit = "km2"),
      aeff = data.frame(cluster = c("HI", "LO"), acomm = "LAND",
                        cap2ainp = c(20, 5)),
      cap2act = 8760,
      vintage = data.frame(olife = 50L),
      capacity = data.frame(region = "R1", cluster = "HI", year = 2025,
                            stock = 1)))
  f <- check_feasibility(newModel("CFT", region = "R1",
                                  data = newRepository("cft", objs)),
                         verbose = FALSE)
  expect_equal(nrow(f), 1L)
  expect_equal(f$required, 20)
  expect_equal(f$available, 10)   # the HI tier, not HI + LO
})


# --------------------------------------------------------------------------- #
# Economic retirement. A requirement that follows from standing capacity is
# forced only while that capacity must stand, so the report carries both
# settings and names the one the model actually has.
#
# The gate is `optimizeRetirement`, on the model AND on the process -- not the
# `ret.*` bounds, which are inert without it.
# --------------------------------------------------------------------------- #

test_that("with retirement off there is one setting, named as the live one", {
  f <- check_feasibility(.cf_model(land = 10, per_gw = 20, stock = 1),
                         verbose = FALSE)
  expect_equal(nrow(f), 1L)
  expect_equal(f$retirement, "off")
  expect_equal(attr(f, "retirement_actual"), "off")
})

test_that("a ret.up bound WITHOUT the gate is inert, not retirement", {
  # `pTechRet` gets rows but no retirement variable is built, so the stock
  # still has to stand: reporting this as retirement would be wrong
  f <- check_feasibility(.cf_model(land = 10, per_gw = 20, stock = 1,
                                   ret = 1),
                         verbose = FALSE)
  expect_equal(attr(f, "retirement_actual"), "off")
  expect_equal(f$retirement, "off")
  expect_equal(f$required, 20)
})

test_that("the gate with no bound makes the whole draw avoidable", {
  f <- check_feasibility(.cf_model(land = 10, per_gw = 20, stock = 1,
                                   opt_ret = TRUE),
                         verbose = FALSE)
  expect_equal(attr(f, "retirement_actual"), "on")
  expect_equal(f$retirement, "off")          # the counterfactual still fires
  expect_equal(nrow(f), 1L)                  # the live setting does not
  expect_equal(f$required, 20)
})

test_that("a partly retirable stock is reported under both settings", {
  # 1 GW standing, 0.25 may go: 20 forced with retirement off, 15 with it on,
  # and the ceiling is 10 either way
  f <- check_feasibility(.cf_model(land = 10, per_gw = 20, stock = 1,
                                   ret = 0.25, opt_ret = TRUE),
                         verbose = FALSE)
  expect_setequal(f$retirement, c("off", "on"))
  expect_equal(f$required[f$retirement == "off"], 20)
  expect_equal(f$required[f$retirement == "on"], 15)
  expect_equal(unique(f$available), 10)
  expect_equal(attr(f, "retirement_actual"), "on")
})

test_that("the model gate alone is not enough without the process gate", {
  m <- .cf_model(land = 10, per_gw = 20, stock = 1)
  m@config@optimizeRetirement <- TRUE       # process slot still FALSE
  f <- check_feasibility(m, verbose = FALSE)
  expect_equal(attr(f, "retirement_actual"), "off")
})

test_that("the report names the live retirement setting", {
  expect_message(check_feasibility(.cf_model(land = 100, per_gw = 20)),
                 "retirement is OFF")
  expect_message(
    check_feasibility(.cf_model(land = 100, per_gw = 20, opt_ret = TRUE)),
    "retirement is ON")
})

# --------------------------------------------------------------------------- #
# Tables the scenario keeps on disk. `save_scenario()` empties `@data` for any
# table above `energyRt.ondisk_min_rows`, so a check that read the slot alone
# saw no demand at all and reported "no contradiction" -- silently, on exactly
# the large models where it matters.
# --------------------------------------------------------------------------- #

test_that("a demand kept on disk is still checked", {
  pr_local_model()
  withr::local_options(list(energyRt.ondisk_min_rows = 1L))
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SL = pr_slices)), name = "odc")
  m <- newModel("ODSK", region = "R1", discount = 0, calendar = cal,
                horizon = newHorizon(2025),
                repo = newRepository("odsk", list(
                  newCommodity("GAS", timeframe = "ANNUAL"),
                  newSupply("SUP_GAS", commodity = "GAS", unit = "PJ",
                            region = "R1",
                            supply = data.frame(region = "R1", cost = 1,
                                                ava.up = 10)),
                  newDemand("DEM_GAS", commodity = "GAS", unit = "PJ",
                            demand = data.frame(region = "R1", year = 2025,
                                                demand = 40)))))
  scen <- suppressMessages(interpolate_model(m, name = "ODSK",
                                             verbose = FALSE))
  scen <- suppressMessages(save_scenario(scen, verbose = FALSE))
  scen <- suppressMessages(load_scenario(scen@path, env = NULL,
                                         verbose = FALSE))
  # the slot is empty once the table is in the store; the check must still
  # find the contradiction, which is what the slot-only read missed
  expect_equal(nrow(as.data.frame(scen@modInp@parameters[["pDemand"]]@data)),
               0L)
  f <- check_feasibility(scen, verbose = FALSE)
  expect_true(any(f$check == "demand-vs-supply"))
  expect_equal(f$required[f$check == "demand-vs-supply"][1], 40)
})

test_that("interpolate_model(check=) stops only on the live setting", {
  pr_local_model()
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SL = pr_slices)), name = "rtc")
  mk <- function(ret) {
    newModel("RETC", region = "R1", discount = 0, calendar = cal,
             horizon = newHorizon(2025), optimizeRetirement = ret,
             repo = newRepository("retc", list(
               newCommodity("ELC", timeframe = "ANNUAL"),
               newCommodity("LAND", timeframe = "ANNUAL"),
               newSupply("RES_LAND", commodity = "LAND", unit = "km2",
                         region = "R1",
                         supply = data.frame(region = "R1", cost = 0,
                                             ava.up = 10)),
               newTechnology("EWIN",
                 output = list(comm = "ELC", unit = "GWh"),
                 aux = data.frame(acomm = "LAND", unit = "km2"),
                 aeff = data.frame(acomm = "LAND", cap2ainp = 20),
                 cap2act = 8760,
                 vintage = data.frame(start = 2015L, olife = 50L),
                 optimizeRetirement = ret,
                 capacity = data.frame(region = "R1", year = 2025,
                                       stock = 1)))))
  }
  # retirement off: the contradiction is live and must stop the build
  expect_error(
    suppressMessages(interpolate_model(mk(FALSE), name = "RETC_OFF",
                                       check = TRUE, verbose = FALSE)),
    "contradicts itself")
  # retirement on and able to shed the whole stock: the only finding is the
  # counterfactual, so the build must proceed
  expect_s4_class(
    suppressMessages(interpolate_model(mk(TRUE), name = "RETC_ON",
                                       check = TRUE, verbose = FALSE)),
    "scenario")
})
