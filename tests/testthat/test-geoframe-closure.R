# =========================================================================== #
# Commodity availability within its balancing cell (`.commreg_closure`).
#
# A commodity with a coarse `@geoframe` is balanced per cell of that level, so
# what is supplied anywhere in the cell is available to every region in it.
# Before this, a plant in a sub-region burning a fuel supplied at the nation
# was judged to have no input: `.drop_region_gaps()` removed it and the model
# solved without it (or failed on the dropped cell). A commodity balanced at
# the finest level must NOT spread: R2 cannot burn gas supplied in R1.
# =========================================================================== #

gc_model <- function(name = "gc") {
  gs <- geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", region = c("R1", "R2")),
    geoframes = c("nation", "region"), key = "region", name = paste0("gs_", name))
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = paste0("cal_", name))
  m <- newModel(
    name = name, desc = "", calendar = cal, region = c("R1", "R2"),
    horizon = newHorizon(2020:2030, intervals = 10), discount = 0.05,
    repo = newRepository(paste0("repo_", name),
      newCommodity("COA", timeframe = "ANNUAL", geoframe = "nation"),
      newCommodity("GAS", timeframe = "ANNUAL"),            # finest level
      newCommodity("ELC", timeframe = "SEASON"),
      newSupply("SUP_COA", commodity = "COA", region = "NAT",
                supply = data.frame(region = "NAT", cost = 1)),
      newSupply("SUP_GAS", commodity = "GAS", region = "R1",
                supply = data.frame(region = "R1", cost = 2)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"), region = c("R1", "R2"),
                    invcost = data.frame(invcost = 900),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newTechnology("EGAS", input = list(comm = "GAS"),
                    output = list(comm = "ELC"), region = c("R1", "R2"),
                    invcost = data.frame(invcost = 600),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newDemand("DEM_ELC", commodity = "ELC",
                demand = data.frame(region = rep(c("R1", "R2"), each = 2),
                                    timeslice = c("WIN", "SUM"), demand = 30))))
  setGeoscale(m, gs)
}

gc_interp <- function(mod, name) {
  suppressMessages(suppressWarnings(interpolate_model(mod, name = name, overwrite = TRUE)))
}

test_that("a nationally balanced fuel reaches plants in every sub-region", {
  skip_if_not_installed("geoscales")
  scen <- gc_interp(gc_model("gc_i"), "gc_i")
  inp <- as.data.frame(get_data_slot(scen@modInp@parameters$mvTechInp))
  # the coal plants draw COA in both regions: it is balanced nationally
  expect_setequal(unique(inp$region[inp$tech == "ECOA" & inp$comm == "COA"]), c("R1", "R2"))
  # GAS is balanced per region: only the R1 plant can draw it
  expect_setequal(unique(inp$region[inp$tech == "EGAS" & inp$comm == "GAS"]), "R1")
  # mCommReg is NOT widened: it also bounds supply / import domains, and a
  # widened one put unpriced import rows in every sub-region
  cr <- as.data.frame(get_data_slot(scen@modInp@parameters$mCommReg))
  expect_false(any(cr$comm == "COA" & cr$region %in% c("R1", "R2")))
  gaps <- scen@misc$region_gaps
  gaps <- if (is.null(gaps)) data.frame(process = character(), region = character()) else as.data.frame(gaps)
  # the coal plant is NOT dropped anywhere; the gas plant only in R2
  expect_false(any(gaps$process == "ECOA"))
  expect_true(any(gaps$process == "EGAS" & gaps$region == "R2"))
  expect_false(any(gaps$process == "EGAS" & gaps$region == "R1"))
})

test_that("the sub-regional coal plants run on the national fuel", {
  skip_if_not_installed("geoscales")
  skip_if_no_solver()
  scen <- gc_interp(gc_model("gc_s"), "gc_s")
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  act <- as.data.frame(getData(scen, "vTechAct", merge = TRUE))
  # R2 has no gas, so its demand can only be met by the coal plant there
  expect_gt(sum(act$value[act$tech == "ECOA" & act$region == "R2"]), 0)
  expect_equal(sum(act$value[act$tech == "EGAS" & act$region == "R2"]), 0)
  # the coal they burn is SUPPLIED -- drawn through the national balance and
  # paid for, not free: supply out equals the plants' input
  inp <- as.data.frame(getData(scen, "vTechInp", merge = TRUE))
  sup <- as.data.frame(getData(scen, "vSupOut", merge = TRUE))
  expect_gt(sum(sup$value[sup$comm == "COA"]), 0)
  expect_equal(sum(sup$value[sup$comm == "COA"]),
               sum(inp$value[inp$comm == "COA"]), tolerance = 1e-6)
})

# --- imports exist only at their commodity's balancing level ----------------- #
# Electricity balanced nationally, plants in the regions below, unserved load as
# a dear import declared at the nation. The plants mark ELC present in their
# regions; the import must still have rows at NAT only -- in R1/R2 it would be
# unpriced (its price is declared at NAT) and the model would buy free unserved
# load instead of generating.
gc_import_model <- function(name = "gci") {
  gs <- geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", region = c("R1", "R2")),
    geoframes = c("nation", "region"), key = "region", name = paste0("gs_", name))
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = paste0("cal_", name))
  m <- newModel(
    name = name, desc = "", calendar = cal, region = c("R1", "R2"),
    horizon = newHorizon(2020:2030, intervals = 10), discount = 0.05,
    repo = newRepository(paste0("repo_", name),
      newCommodity("COA", timeframe = "ANNUAL", geoframe = "nation"),
      newCommodity("ELC", timeframe = "SEASON", geoframe = "nation"),
      newSupply("SUP_COA", commodity = "COA", region = "NAT",
                supply = data.frame(region = "NAT", cost = 1)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"), region = c("R1", "R2"),
                    invcost = data.frame(invcost = 900),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newImport("UNSERVED", commodity = "ELC", region = "NAT",
                import = data.frame(region = "NAT", price = 1e4)),
      newDemand("DEM_ELC", commodity = "ELC", region = "NAT",
                demand = data.frame(region = "NAT", timeslice = c("WIN", "SUM"), demand = 30))))
  setGeoscale(m, gs)
}

test_that("an import declared at the nation has rows at the nation only", {
  skip_if_not_installed("geoscales")
  scen <- gc_interp(gc_import_model("gci_i"), "gci_i")
  ir <- as.data.frame(get_data_slot(scen@modInp@parameters$mImportRow))
  expect_setequal(unique(ir$region), "NAT")
})

test_that("demand is met by the sub-regional plants, not by unpriced imports", {
  skip_if_not_installed("geoscales")
  skip_if_no_solver()
  scen <- .fork_solve(gc_interp(gc_import_model("gci_s"), "gci_s"), solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  imp <- getData(scen, "vImportRow", merge = TRUE)
  expect_true(is.null(imp) || nrow(imp) == 0 || sum(imp$value) < 1e-6)
  out <- as.data.frame(getData(scen, "vTechOut", merge = TRUE))
  expect_gt(sum(out$value[out$tech == "ECOA"]), 0)
})

test_that("an import at the nation is priced: with no plant, demand costs its price", {
  skip_if_not_installed("geoscales")
  skip_if_no_solver()
  m <- gc_import_model("gci_p")
  m@data[[1]]@data$ECOA <- NULL                        # nothing generates
  scen <- .fork_solve(gc_interp(m, "gci_p"), solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  imp <- as.data.frame(getData(scen, "vImportRow", merge = TRUE))
  expect_setequal(unique(imp$region), "NAT")
  # 60 MWh a year at 10,000 EUR over the horizon: the cost reaches the objective
  expect_gt(.fork_objective(scen), 6e5)
})
