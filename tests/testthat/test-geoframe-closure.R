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

# --- a process at a coarse cell annuitises at its children's rate ----------- #
# The rates are interpolated for the model's own regions; a technology declared
# at NAT found none and was annuitised at 0 (1/olife). It must use the rate its
# regions share: 5% here.
test_that("a technology at the nation is annuitised at the model rate", {
  skip_if_not_installed("geoscales")
  m <- gc_import_model("gci_e")
  m@data[[1]]@data$ESOL <- newTechnology(
    "ESOL", output = list(comm = "ELC"), region = "NAT",
    invcost = data.frame(region = "NAT", invcost = 1000),
    vintage = data.frame(olife = 20L), cap2act = 1)
  scen <- gc_interp(m, "gci_e")
  eac <- as.data.frame(get_data_slot(scen@modInp@parameters$pTechEac))
  v <- unique(round(eac$value[eac$tech == "ESOL"], 6))
  crf <- 0.05 / (1 - 1.05^-20)
  expect_equal(v, round(1000 * crf, 6))
  expect_false(isTRUE(all.equal(v, 1000 / 20)))       # the old, silent 0% annuity
})

# --- rates declared at a parent level; children override ------------------ #
gc_rate_model <- function(name, discount, nat_tech = TRUE) {
  gs <- geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", region = c("R1", "R2")),
    geoframes = c("nation", "region"), key = "region", name = paste0("gs_", name))
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))), name = paste0("cal_", name))
  objs <- list(
    newCommodity("ELC", timeframe = "SEASON", geoframe = "nation"),
    newTechnology("EGEN", output = list(comm = "ELC"), region = c("R1", "R2"),
                  invcost = data.frame(invcost = 1000),
                  vintage = data.frame(olife = 20L), cap2act = 1),
    newDemand("DEM_ELC", commodity = "ELC", region = "NAT",
              demand = data.frame(region = "NAT", timeslice = c("WIN", "SUM"), demand = 30)))
  if (nat_tech) objs <- c(objs, list(newTechnology(
    "ENAT", output = list(comm = "ELC"), region = "NAT",
    invcost = data.frame(region = "NAT", invcost = 1000),
    vintage = data.frame(olife = 20L), cap2act = 1)))
  m <- newModel(name = name, desc = "", calendar = cal, region = c("R1", "R2"),
                horizon = newHorizon(2020:2030, intervals = 10), discount = discount,
                repo = do.call(newRepository, c(list(paste0("repo_", name)), objs)))
  setGeoscale(m, gs)
}
gc_eac <- function(scen, tech) {
  e <- as.data.frame(get_data_slot(scen@modInp@parameters$pTechEac))
  e <- e[e$tech == tech, , drop = FALSE]
  if ("region" %in% names(e) && any(!is.na(e$region))) {
    stats::setNames(round(e$value, 6), e$region)[!duplicated(e$region)]
  } else round(unique(e$value), 6)
}
crf <- function(r, n = 20) round(1000 * r / (1 - (1 + r)^-n), 6)

test_that("a rate declared at the nation is inherited by its regions", {
  skip_if_not_installed("geoscales")
  scen <- gc_interp(gc_rate_model("gcr_a", data.frame(region = c(NA, "NAT"), discount = c(0.05, 0.08))), "gcr_a")
  w <- as.data.frame(get_data_slot(scen@modInp@parameters$pWacc))
  expect_equal(unique(round(w$value[w$region %in% c("R1", "R2")], 6)), 0.08)
  e <- gc_eac(scen, "EGEN")
  expect_equal(unname(unique(e)), crf(0.08))
  expect_equal(unname(gc_eac(scen, "ENAT")), crf(0.08))
})

test_that("a region's own rate overrides the nation's; the nation's process keeps it", {
  skip_if_not_installed("geoscales")
  scen <- gc_interp(gc_rate_model("gcr_b", data.frame(region = c("NAT", "R2"), discount = c(0.08, 0.03))), "gcr_b")
  e <- gc_eac(scen, "EGEN")
  expect_equal(unname(e[["R1"]]), crf(0.08))
  expect_equal(unname(e[["R2"]]), crf(0.03))
  expect_equal(unname(gc_eac(scen, "ENAT")), crf(0.08))   # declared at NAT, not averaged
})

test_that("children that disagree are an error only where a process needs the rate", {
  skip_if_not_installed("geoscales")
  d <- data.frame(region = c("R1", "R2"), discount = c(0.08, 0.03))
  # no process at NAT: fine
  scen <- gc_interp(gc_rate_model("gcr_c", d, nat_tech = FALSE), "gcr_c")
  expect_equal(unname(gc_eac(scen, "EGEN")[["R2"]]), crf(0.03))
  # a process at NAT with no rate of its own: refused, and named
  expect_error(suppressMessages(suppressWarnings(
    interpolate_model(gc_rate_model("gcr_d", d), name = "gcr_d", overwrite = TRUE))),
    "No investment rate for 'ENAT' at 'NAT'")
})

# --- a producer with no inputs makes its output available in its region ------- #
# Run-of-river and rooftop PV have no input commodity. `map_mCommReg()` built the
# secondary availability from process_inputs only, so a no-input plant alone in
# a sub-region left ELC absent there; its output rows were cut and it ran
# producing nothing. Free plant in R2 (stock 50), coal plant in R1 (fuel 5),
# national demand 80: the free plant runs flat out, coal covers 30 -> 150.
# @covers mCommReg mvTechOut depth=S backends=glpk
test_that("a no-input plant in a sub-region keeps its output", {
  skip_if_not_installed("geoscales")
  gs <- geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", region = c("R1", "R2")),
    geoframes = c("nation", "region"), key = "region", name = "gs_ni")
  cal <- newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
                     name = "cal_ni")
  m <- newModel(name = "ni", calendar = cal, region = c("R1", "R2"),
    horizon = newHorizon(2025), discount = 0,
    repo = newRepository("repo_ni",
      newCommodity("COA", timeframe = "ANNUAL", geoframe = "nation"),
      newCommodity("ELC", timeframe = "ANNUAL", geoframe = "nation"),
      newSupply("SUP_COA", commodity = "COA", region = "NAT",
                supply = data.frame(region = "NAT", cost = 5)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"), region = "R1",
                    capacity = data.frame(stock = 100, cap.up = 100), cap2act = 1),
      newTechnology("EFREE", output = list(comm = "ELC"), region = "R2",
                    capacity = data.frame(stock = 50, cap.up = 50), cap2act = 1),
      newDemand("DEM_ELC", commodity = "ELC", region = "NAT",
                demand = data.frame(region = "NAT", demand = 80))))
  m <- setGeoscale(m, gs)
  scen <- gc_interp(m, "ni")
  out <- as.data.frame(.gds(scen, "mvTechOut"))
  expect_setequal(unique(out$region[out$tech == "EFREE"]), "R2")

  skip_if_no_solver()
  scen <- .fork_solve(scen, solver_options$glpk)
  expect_true(verify_solution(scen)$ok)
  expect_equal(.fork_objective(scen), 150, tolerance = 1e-6)
})
