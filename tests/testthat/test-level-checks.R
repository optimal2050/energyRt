# =========================================================================== #
# Declared resolution must match the commodity's own level.
#
# A commodity declares where it is BALANCED (`@timeframe`, `@geoframe`).
# Classes with no aggregation path of their own -- demand, supply, import,
# export, trade, storage -- must declare AT that level. Before this check, a
# mismatch produced a cell nothing reads: the parameter was written, the
# equation was generated at the commodity's level, the lookup missed, and the
# term silently evaluated to ZERO. The model stayed feasible and was wrong.
#
# Three variants of that one failure are pinned here, all reproduced before
# the check existed:
#   1. demand at a finer TIMESLICE  than the commodity's timeframe -> demand lost
#   2. demand at a finer REGION than the commodity's geoframe  -> demand lost
#   3. a technology declared COARSER than a commodity it uses  -> flows stranded
#
# Technology is the exception: it may run at a different resolution, but never
# coarser, because aggregation only flows fine -> coarse.
# =========================================================================== #

lc_cal <- function() {
  newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))), name = "lc")
}

lc_geoscale <- function(regions = c("R1", "R2")) {
  geoscales::geoscale_from_leaves(
    data.frame(nation = "NAT", region = regions),
    levels = c("nation", "region"), key = "region", name = "lc")
}

# STEEL's levels and the demand's declaration are the knobs under test.
lc_model <- function(steel_tf = "ANNUAL", steel_gl = character(),
                     demand = data.frame(region = "R1", timeslice = "ANNUAL", demand = 25),
                     dem_region = "R1", mill_tf = character(),
                     name = "lc", regions = c("R1", "R2")) {
  newModel(
    name = name, desc = "", calendar = lc_cal(), region = regions,
    horizon = newHorizon(2020:2030, intervals = 10), discount = 0.05,
    repo = newRepository("r",
      newCommodity("COA", timeframe = "ANNUAL"),
      newCommodity("ELC", timeframe = "SEASON"),
      newCommodity("STEEL", timeframe = steel_tf, geoframe = steel_gl),
      newSupply("SUP", commodity = "COA",
                supply = data.frame(region = regions, cost = 1)),
      newTechnology("ECOA", input = list(comm = "COA"),
                    output = list(comm = "ELC"),
                    invcost = data.frame(invcost = 900),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newTechnology("MILL", input = list(comm = "ELC"),
                    output = list(comm = "STEEL"), region = "R1",
                    timeframe = mill_tf,
                    ceff = data.frame(comm = "ELC", cinp2use = 0.5),
                    invcost = data.frame(invcost = 50),
                    vintage = data.frame(olife = 30L), cap2act = 1),
      newDemand("DE", commodity = "ELC",
                demand = data.frame(region = rep(regions, each = 2),
                                 timeslice = c("WIN", "SUM"), demand = 30)),
      newDemand("DS", commodity = "STEEL", region = dem_region, demand = demand)))
}

lc_interp <- function(mod, name = "lc") {
  suppressMessages(suppressWarnings(
    interpolate_model(mod, name = name, fold = TRUE)))
}

# --------------------------------------------------------------------------- #

test_that("demand below the commodity's timeframe is refused", {
  # STEEL balances ANNUAL; declaring demand at WIN/SUM used to yield 0 steel.
  expect_error(
    lc_interp(lc_model(
      steel_tf = "ANNUAL",
      demand = data.frame(region = "R1", timeslice = c("WIN", "SUM"), demand = c(10, 15)),
      name = "t1"), "t1"),
    "does not match the commodity"
  )
})

test_that("demand at the commodity's own timeframe is accepted", {
  expect_s4_class(lc_interp(lc_model(name = "t2"), "t2"), "scenario")
})

test_that("demand below the commodity's geoframe is refused", {
  skip_if_not_installed("geoscales")
  # STEEL balances at the nation; county demand used to yield 0 steel.
  mod <- setGeoscale(
    lc_model(steel_gl = "nation",
             demand = data.frame(region = "R2", timeslice = "ANNUAL", demand = 10),
             dem_region = "R2", name = "t3"),
    lc_geoscale())
  expect_error(lc_interp(mod, "t3"), "does not match the commodity")
})

test_that("demand at the commodity's own geoframe is accepted", {
  skip_if_not_installed("geoscales")
  mod <- setGeoscale(
    lc_model(steel_gl = "nation",
             demand = data.frame(region = "NAT", timeslice = "ANNUAL", demand = 10),
             dem_region = "NAT", name = "t4"),
    lc_geoscale())
  expect_s4_class(lc_interp(mod, "t4"), "scenario")
})

test_that("a technology coarser than a commodity it uses is refused", {
  # MILL consumes SEASON electricity; pinning it to ANNUAL strands the flow.
  # This used to warn and silently substitute the finest timeframe.
  expect_error(
    lc_interp(lc_model(mill_tf = "ANNUAL", name = "t5"), "t5"),
    "COARSER"
  )
})

test_that("a technology finer than its commodities is still allowed", {
  # The documented rule: a process runs at the FINEST of its commodities, so
  # MILL (SEASON, from ELC) producing ANNUAL STEEL is legitimate and its
  # output aggregates up.
  scen <- lc_interp(lc_model(name = "t6"), "t6")
  ptf <- get_process_timeframe(scen)
  expect_equal(ptf[["MILL"]], "SEASON")
})

# --- the geo twins: summand@geoframe and getData(geoframe=) ---------------- #

lc_geo_model <- function(extra = list(), regions = c("R1", "R2", "R3", "R4")) {
  objs <- c(list(
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "SEASON"),
    newSupply("SUP", commodity = "COA",
              supply = data.frame(region = regions, cost = 1)),
    newTechnology("ECOA", input = list(comm = "COA"),
                  output = list(comm = "ELC"),
                  invcost = data.frame(invcost = 900),
                  vintage = data.frame(olife = 30L), cap2act = 1),
    newDemand("DE", commodity = "ELC",
              demand = data.frame(region = rep(regions, each = 2),
                               timeslice = c("WIN", "SUM"), demand = 30))), extra)
  m <- newModel(name = "g", desc = "", calendar = lc_cal(), region = regions,
                horizon = newHorizon(2020:2030, intervals = 10), discount = 0.05,
                repo = do.call(newRepository, c(list("r"), objs)))
  setGeoscale(m, geoscales::geoscale_from_leaves(
    data.frame(nation = "NAT", zone = c("W", "W", "E", "E"), region = regions),
    levels = c("nation", "zone", "region"), key = "region", name = "g"))
}

test_that("getData(geoframe=) rolls results up and conserves the total", {
  skip_if_not_installed("geoscales")
  skip_if_no_solver()
  scen <- lc_interp(lc_geo_model(), "g1")
  sol <- suppressMessages(suppressWarnings(
    solve_scen(scen, solver = solver_options$glpk, wait = TRUE)))
  tot <- function(gl) {
    d <- suppressMessages(getData(sol, "vTechOut", merge = TRUE, geoframe = gl))
    d <- d[d$comm == "ELC", ]
    list(regions = sort(unique(as.character(d$region))), total = sum(d$value))
  }
  fine <- tot("finest"); zone <- tot("zone"); nat <- tot("coarsest")

  expect_setequal(fine$regions, c("R1", "R2", "R3", "R4"))
  expect_setequal(zone$regions, c("W", "E"))
  expect_setequal(nat$regions, "NAT")
  # aggregation must not create or destroy anything
  expect_equal(zone$total, fine$total, tolerance = 1e-9)
  expect_equal(nat$total, fine$total, tolerance = 1e-9)

  d <- suppressMessages(getData(sol, "vTechOut", merge = TRUE, geoframe = "all"))
  expect_true(all(c("R1", "W", "NAT") %in% d$region))
})

lc_con_regions <- function(scen, prefix = "^mCns") {
  for (n in grep(prefix, names(scen@modInp@parameters), value = TRUE)) {
    d <- get_data_slot(scen@modInp@parameters[[n]])
    if (!is.null(d) && nrow(d) && "region" %in% names(d)) {
      return(sort(unique(as.character(as.data.frame(d)$region))))
    }
  }
  character()
}

test_that("a coarse region named in for.sum expands to the variable's own", {
  skip_if_not_installed("geoscales")
  # THE motivating case: constrain a variable for ONE province, when the
  # variable actually lives at counties. Naming the province must sum its
  # counties -- not build a constraint over an index the variable lacks, which
  # would leave a free variable and a trivially satisfiable constraint.
  sm <- list(variable = "vTechOut", for.sum = list(comm = "ELC", region = "W"))
  con <- newConstraint("CAP_W", desc = "", sm, eq = "<=", defVal = Inf,
                       for.each = data.frame(year = c(2020, 2030)),
                       rhs = data.frame(rhs = 1e6))
  expect_setequal(lc_con_regions(lc_interp(lc_geo_model(list(con)), "g2")),
                  c("R1", "R2"))
})

test_that("a region the variable already has is kept, not expanded", {
  skip_if_not_installed("geoscales")
  sm <- list(variable = "vTechOut", for.sum = list(comm = "ELC", region = "R3"))
  con <- newConstraint("CAP_R3", desc = "", sm, eq = "<=", defVal = Inf,
                       for.each = data.frame(year = c(2020, 2030)),
                       rhs = data.frame(rhs = 1e6))
  expect_setequal(lc_con_regions(lc_interp(lc_geo_model(list(con)), "g3")), "R3")
})

test_that("summand@geoframe resolves to indices the variable actually has", {
  skip_if_not_installed("geoscales")
  # `vTechOut` exists only at process regions, so every level resolves to those
  # regions -- summing them is the level aggregate either way. The point of the
  # rule is that it resolves to something REAL rather than to a bare level name.
  con <- function(gl) {
    sm <- list(variable = "vTechOut", for.sum = list(comm = "ELC"),
               geoframe = gl)
    newConstraint("CAP", desc = "", sm, eq = "<=", defVal = Inf,
                  for.each = data.frame(year = c(2020, 2030)),
                  rhs = data.frame(rhs = 1e6))
  }
  for (gl in c("region", "zone", "nation")) {
    expect_setequal(lc_con_regions(lc_interp(lc_geo_model(list(con(gl))),
                                             paste0("g4", gl))),
                    c("R1", "R2", "R3", "R4"))
  }
  expect_error(lc_interp(lc_geo_model(list(con("bogus"))), "g5"),
               "unknown geoframe")
})

test_that("summand@geoframe needs a geoscale and a region dimension", {
  skip_if_not_installed("geoscales")
  # a variable with no region dimension cannot be pinned to a geoframe
  sm <- list(variable = "vObjective", geoframe = "zone")
  con <- newConstraint("NOREG", desc = "", sm, eq = "<=", defVal = Inf,
                       for.each = data.frame(year = 2020),
                       rhs = data.frame(rhs = 1))
  expect_error(lc_interp(lc_geo_model(list(con)), "g5"),
               "no region dimension")
})

test_that("the check is inert for a flat model with matched levels", {
  # No geoscale, every level matched: nothing to report.
  expect_silent(energyRt:::.check_process_levels(lc_interp(lc_model(name = "t7"),
                                                           "t7")))
})
