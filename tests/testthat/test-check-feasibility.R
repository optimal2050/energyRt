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
                      region = "R1") {
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
      capacity = data.frame(region = region, year = 2025, stock = stock)))
  if (import) {
    objs <- c(objs, list(newImport("IMP_LAND", commodity = "LAND",
                                   unit = "km2", region = region,
                                   import = data.frame(region = region,
                                                       price = 1))))
  }
  newModel("CF", region = region, data = newRepository("cf", objs))
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
