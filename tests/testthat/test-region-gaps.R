# A process declared where a commodity it consumes is unavailable.
#
# `mCommReg` narrows the per-commodity flow domains but not the activity domain
# (`mvTechAct` carries no commodity). Left alone, such a process keeps its
# activity and output rows and loses only its input rows: the input constraint
# is not violated, it does not exist, and the process produces from nothing at
# zero marginal cost. The solve is feasible, optimal, and passes every
# `verify_solution()` invariant, because the balance it would violate was never
# generated.
#
# Two counterparts are pinned here: the interpolation-time drop (which records
# `scenario@misc$region_gaps`) and `model_region_gaps()`, the declaration-time
# check that needs no pipeline.

# @covers mvTechAct mvTechInp mvTechOut mCommReg depth=S backends=glpk

rg_cal <- function() {
  newCalendar(timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
              name = "rg1")
}

# Coal supplied in R1 only; ECOA unscoped by default, so declared in both
# regions and short of fuel in R2. `dem` picks which regions carry demand
# (feasibility); `tech_region` scopes the technology, which is the declaration
# that removes the gap.
rg_mod <- function(dem = c("R1", "R2"), trade = FALSE,
                   tech_region = character(), name = "rg") {
  objs <- list(
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "ANNUAL"),
    newSupply("SUP_COA", commodity = "COA", region = "R1",
              supply = data.frame(region = "R1", cost = 2)),
    newTechnology("ECOA", input = list(comm = "COA"),
                  output = list(comm = "ELC"), region = tech_region,
                  ceff = data.frame(comm = "COA", cinp2use = 1), cap2act = 1,
                  invcost = list(invcost = 10), olife = 30L),
    # scoped on the slot as well as in the data: an unscoped demand exists in
    # every region, and would trip the separate "no producer for a demand
    # commodity" warning in the cases below that narrow the technology.
    newDemand("DEM", commodity = "ELC", region = dem,
              demand = data.frame(region = dem, year = 2020L,
                                  timeslice = "ANNUAL", demand = 5)))
  if (trade) {
    objs <- c(objs, list(newTrade("TR_COA", commodity = "COA",
      routes = data.frame(src = "R1", dst = "R2"),
      trade = data.frame(src = "R1", dst = "R2", teff = 1), cap2act = 1,
      invcost = data.frame(invcost = 1), olife = list(olife = 30))))
  }
  newModel(name, region = c("R1", "R2"), discount = 0, calendar = rg_cal(),
           horizon = newHorizon(2020),
           repo = do.call(newRepository, c(list("rgr"), objs)))
}

rg_interp <- function(mod, name) {
  suppressWarnings(suppressMessages(
    interpolate_model(mod, name = name, overwrite = TRUE)))
}

rg_regions <- function(scen, map) {
  d <- as.data.frame(get_data_slot(scen@modInp@parameters[[map]]))
  sort(unique(as.character(d$region)))
}

# --- interpolation-time ------------------------------------------------------ #

test_that("a process without its input is dropped from the ACTIVITY domain", {
  scen <- rg_interp(rg_mod(), "rg_drop")
  # the input alone was already narrowed; the point is that activity and output
  # follow it, which is what stops the free production
  expect_identical(rg_regions(scen, "mvTechInp"), "R1")
  expect_identical(rg_regions(scen, "mvTechAct"), "R1")
  expect_identical(rg_regions(scen, "mvTechOut"), "R1")
})

test_that("the dropped cells are reported and recorded", {
  expect_warning(
    scen <- suppressMessages(
      interpolate_model(rg_mod(name = "rg_w"), name = "rg_warn",
                        overwrite = TRUE)),
    "produce from nothing")
  g <- scen@misc$region_gaps
  expect_s3_class(g, "data.frame")
  expect_identical(nrow(g), 1L)
  expect_identical(g$process, "ECOA")
  expect_identical(g$class, "technology")
  expect_identical(g$region, "R2")
  expect_identical(g$missing, "COA")
})

test_that("a route that carries the commodity in removes the gap", {
  scen <- rg_interp(rg_mod(trade = TRUE, name = "rg_t"), "rg_trade")
  expect_null(scen@misc$region_gaps)
  expect_identical(rg_regions(scen, "mvTechAct"), c("R1", "R2"))
})

test_that("scoping the technology removes the gap, so nothing is reported", {
  # ECOA declared only where its fuel is: there is no cell to drop. Note that
  # moving the DEMAND would not help -- the gap is a property of the
  # declaration, not of what the model is asked to serve.
  expect_no_warning(
    scen <- suppressMessages(
      interpolate_model(rg_mod(dem = "R1", tech_region = "R1", name = "rg_ok"),
                        name = "rg_clean", overwrite = TRUE)))
  expect_null(scen@misc$region_gaps)
  expect_identical(rg_regions(scen, "mvTechAct"), "R1")
})

test_that("the dropped process cannot generate from nothing", {
  skip_if_no_solver()
  old <- set_scenarios_path(
    gsub("[\\/]+", "/", file.path(tempdir(), "region-gaps")))
  on.exit(set_scenarios_path(old), add = TRUE)

  # demand in R1 only -> feasible, and R2 produces nothing at all
  s <- suppressMessages(suppressWarnings(solve_scenario(
    rg_interp(rg_mod(dem = "R1", name = "rg_s"), "rg_solve"), echo = FALSE)))
  expect_true(isTRUE(s@status$optimal))
  out <- as.data.frame(getData(s, "vTechOut", merge = TRUE))
  expect_setequal(unique(as.character(out$region)), "R1")
  # every unit produced is matched by an input
  inp <- as.data.frame(getData(s, "vTechInp", merge = TRUE))
  expect_equal(sum(out$value), sum(inp$value))
})

# --- declaration-time -------------------------------------------------------- #

test_that("model_region_gaps() finds the same cell with no interpolation", {
  g <- model_region_gaps(rg_mod())
  expect_identical(nrow(g), 1L)
  expect_identical(g$process, "ECOA")
  expect_identical(g$region, "R2")
  expect_identical(g$missing, "COA")

  # and clears once the commodity can be traded in
  expect_null(model_region_gaps(rg_mod(trade = TRUE)))
})

test_that("model_region_gaps() reads @region, not the data columns", {
  # SUP_COA scoped through its DATA only: under the declaration rule the supply
  # exists everywhere, so there is no gap to report.
  mod <- newModel("rg_decl", region = c("R1", "R2"), discount = 0,
    calendar = rg_cal(), horizon = newHorizon(2020),
    repo = newRepository("rgd",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "ANNUAL"),
      newSupply("SUP_COA", commodity = "COA",
                supply = data.frame(region = "R1", cost = 2)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    ceff = data.frame(comm = "COA", cinp2use = 1),
                    cap2act = 1, invcost = list(invcost = 10), olife = 30L)))
  expect_null(model_region_gaps(mod))
})
