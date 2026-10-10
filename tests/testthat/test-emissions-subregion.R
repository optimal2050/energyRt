# =========================================================================== #
# Emissions of plants below the emission commodity's balancing level.
#
# A fuel supplied at the nation, burnt by plants in its sub-regions, emits CO2
# balanced at the nation. map_mEmsFuelTot() filtered the emission rows through
# the commodity-region closure (mCommReg), where CO2 is available only at the
# nation, so every sub-region plant's emission row was dropped: vEmsFuelTot
# counted nothing, and a tax or cap on CO2 missed the whole fleet. Emissions
# belong to the plant's own region, as its fuel input does (mvTechInp is
# filtered by reachability within the balancing cell); the balance then sums
# the sub-regions into the nation.
# =========================================================================== #

es_model <- function(name, co2_frame = "nation", tax = NULL) {
  gs <- geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", region = c("R1", "R2")),
    geoframes = c("nation", "region"), key = "region", name = paste0("gs_", name))
  cal <- newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
                     name = paste0("cal_", name))
  co2 <- if (is.na(co2_frame)) newCommodity("CO2", timeframe = "ANNUAL") else
    newCommodity("CO2", timeframe = "ANNUAL", geoframe = co2_frame)
  objs <- list(
    co2,
    newCommodity("COA", timeframe = "ANNUAL", geoframe = "nation",
                 emis = data.frame(comm = "CO2", emis = 0.5)),
    newCommodity("ELC", timeframe = "ANNUAL", geoframe = "nation"),
    newSupply("SUP_COA", commodity = "COA", region = "NAT",
              supply = data.frame(region = "NAT", cost = 1)),
    newTechnology("ECOA", input = list(comm = "COA", combustion = 1),
                  output = list(comm = "ELC"), region = c("R1", "R2"),
                  ceff = data.frame(comm = "COA", cinp2use = 1),
                  capacity = data.frame(region = c("R1", "R2"), stock = 50, cap.up = 50),
                  cap2act = 1),
    newDemand("DEM_ELC", commodity = "ELC", region = "NAT",
              demand = data.frame(region = "NAT", demand = 80)))
  if (!is.null(tax)) objs <- c(objs, list(tax))
  m <- newModel(name = name, calendar = cal, region = c("R1", "R2"),
                horizon = newHorizon(2025), discount = 0,
                repo = do.call(newRepository, c(list(paste0("repo_", name)), objs)))
  setGeoscale(m, gs)
}

es_interp <- function(mod, name) {
  suppressMessages(suppressWarnings(interpolate_model(mod, name = name, overwrite = TRUE)))
}

# @covers mTechEmsFuel mEmsFuelTot depth=I
test_that("sub-region plants carry emission rows when CO2 balances at the nation", {
  skip_if_not_installed("geoscales")
  scen <- es_interp(es_model("es_map"), "es_map")
  te <- as.data.frame(.gds(scen, "mTechEmsFuel"))
  expect_setequal(unique(te$region[te$tech == "ECOA"]), c("R1", "R2"))
})

# @covers vEmsFuelTot eqEmsFuelTot depth=S backends=glpk
test_that("emissions count the fuel burnt in every sub-region", {
  skip_if_not_installed("geoscales")
  skip_if_no_solver()
  for (fr in list("nation", NA)) {
    scen <- es_interp(es_model("es_sol"), "es_sol")
    if (is.na(fr)) scen <- es_interp(es_model("es_sol_f", co2_frame = NA), "es_sol_f")
    scen <- .fork_solve(scen, solver_options$glpk)
    expect_true(verify_solution(scen)$ok)
    inp <- as.data.frame(getData(scen, "vTechInp", merge = TRUE))
    ems <- as.data.frame(getData(scen, "vEmsFuelTot", merge = TRUE))
    expect_equal(sum(inp$value[inp$comm == "COA"]), 80, tolerance = 1e-6)
    expect_equal(sum(ems$value), 0.5 * 80, tolerance = 1e-6,
                 label = paste("CO2 frame", fr))
  }
})

# @covers pTaxCostBal depth=S backends=glpk
test_that("a CO2 tax at the nation prices the sub-region plants' emissions", {
  skip_if_not_installed("geoscales")
  skip_if_no_solver()
  tx <- newTax("TAX_CO2", comm = "CO2", region = "NAT",
               tax = data.frame(region = "NAT", year = 2025, bal = 10))
  scen <- es_interp(es_model("es_tax", tax = tx), "es_tax")
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  # fuel 80 x 1 + emissions 40 t x 10
  expect_equal(.fork_objective(scen), 80 + 400, tolerance = 1e-6)
})
