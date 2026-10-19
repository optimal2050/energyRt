# =========================================================================== #
# Auxiliary flows on a coarse process.
#
# A process declared at a region COARSER than an auxiliary commodity it draws
# or emits holds that flow in every finer cell beneath its own: the aux
# VARIABLES sit at the commodity's regions (`.flow_region_spread()`), while the
# equation and its coupling coefficients stay on the process's own cell
# (`.lift_flow_to_proc_region()`), and the equation sums the children through
# `m*ProcAggRegion`.
#
# The invariant each fixture checks is the equation itself:
#
#     sum over child regions of vTechAInp  ==  coefficient x (capacity | flow)
#
# which is what makes a per-region limit on the aux commodity bind without
# splitting the process per region. The region axis is the mirror of the time
# axis, which already summed a coarse aux commodity's finer process slices.
#
# @covers vTechAInp vTechAOut vStorageAInp vStorageAOut meqTechAInp meqTechAOut
# @covers meqStorageAInp meqStorageAOut depth=X backends=glpk,julia,pyomo,mm
# @covers forks=coarse
# =========================================================================== #

ca_gs <- function() {
  geoscales::geoscale_from_leaftable(
    data.frame(nation = "P", region = c("A", "B")),
    geoframes = c("nation", "region"), key = "region", name = "ca")
}

ca_cal <- function(nm) {
  newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = paste0("ca_", nm))
}

ca_model <- function(nm, objs) {
  setGeoscale(newModel(
    name = nm, calendar = ca_cal(nm), region = c("A", "B"),
    horizon = newHorizon(2025), discount = 0,
    repo = do.call(newRepository, c(list(paste0("car_", nm)), objs))), ca_gs())
}

ca_solve <- function(mod, nm, solver = "glpk") {
  pd <- file.path(tempdir(), "coarse_aux")
  dir.create(pd, showWarnings = FALSE, recursive = TRUE)
  open_project(pd, verbose = FALSE)
  suppressMessages(suppressWarnings(solve_model(
    mod, name = nm, solver = solver_options[[solver]], overwrite = TRUE)))
}

ca_var <- function(sol, nm) {
  v <- sol@modOut@variables[[nm]]
  if (is.null(v)) return(NULL)
  d <- get_data_slot(v)
  if (is.null(d) || !nrow(d)) return(NULL)
  as.data.frame(d)
}
ca_sum <- function(sol, nm) {
  d <- ca_var(sol, nm)
  if (is.null(d)) 0 else sum(d$value)
}

# -- the shared fixture: LAND balanced per child, capped cheaply in A -------- #
# A coarse wind technology at the nation draws 2 LAND per unit of capacity.
# LAND costs nothing in A but is capped at 4; in B it is unlimited at 1 each.
# Demand of 3 per season over two equal seasons needs capacity 6
# (6 x cap2act 1 x share 0.5 = 3), hence 12 LAND: 4 from A, 8 from B, cost 8.
ca_cap_objs <- function(coef = 2) {
  list(
    newCommodity("ELC", timeframe = "SEASON", geoframe = "nation"),
    newCommodity("LAND", timeframe = "ANNUAL"),
    newSupply("S_LAND", commodity = "LAND",
              supply = data.frame(region = c("A", "B"), ava.up = c(4, 100),
                                  cost = c(0, 1))),
    newTechnology("EWIN", output = list(comm = "ELC"), region = "P",
                  cap2act = 1, invcost = data.frame(invcost = 0),
                  vintage = data.frame(olife = 30L),
                  aux = data.frame(acomm = "LAND"),
                  aeff = data.frame(acomm = "LAND", cap2ainp = coef)),
    newDemand("D", commodity = "ELC", region = "P",
              demand = data.frame(region = "P", timeslice = c("WIN", "SUM"),
                                  demand = 3)))
}

# --------------------------------------------------------------------------- #

test_that("a coarse process draws a finer aux commodity in the cells beneath it", {
  skip_if_not_installed("geoscales")
  skip_if_no_solver()
  sol <- ca_solve(ca_model("ca1", ca_cap_objs()), "ca1")

  cap <- ca_sum(sol, "vTechCap")
  ai <- ca_var(sol, "vTechAInp")
  expect_equal(cap, 6, tolerance = 1e-9)
  # the equation: the children together carry coefficient x capacity
  expect_equal(sum(ai$value), 2 * cap, tolerance = 1e-9)
  # and the per-child limit binds -- A is exhausted before B is used
  expect_setequal(as.character(ai$region), c("A", "B"))
  expect_equal(ai$value[ai$region == "A"], 4, tolerance = 1e-9)
  expect_equal(ai$value[ai$region == "B"], 8, tolerance = 1e-9)
  expect_equal(ca_sum(sol, "vObjective"), 8, tolerance = 1e-9)
})

test_that("the aux variables sit at the children and the equation at the parent", {
  skip_if_not_installed("geoscales")
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ca_model("ca2", ca_cap_objs()), name = "ca2", overwrite = TRUE)))
  gd <- function(nm) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p)) return(NULL)
    d <- get_data_slot(p)
    if (is.null(d) || !nrow(d)) NULL else as.data.frame(d)
  }
  expect_setequal(as.character(gd("mvTechAInp")$region), c("A", "B"))
  expect_equal(unique(as.character(gd("meqTechAInp")$region)), "P")
  # the coupling coefficient and its map stay where the equation reads them
  expect_equal(unique(as.character(gd("mTechCap2AInp")$region)), "P")
  expect_equal(unique(as.character(gd("pTechCap2AInp")$region)), "P")
  # the child enumeration pairs the process cell with each commodity cell
  agg <- gd("mTechProcAggRegion")
  expect_setequal(paste(agg$comm, agg$region, agg$regionp),
                  c("LAND P A", "LAND P B"))
})

test_that("a flow-type coupling sums the slices and the regions together", {
  skip_if_not_installed("geoscales")
  skip_if_no_solver()
  # `cinp2ainp`: 1 LAND per unit of FUEL burnt. The aux commodity is ANNUAL
  # while the process runs per season, so this exercises the slice sum (the
  # time axis) and the region sum together. Demand 3 in each of two seasons
  # with unit efficiency burns 6 FUEL, hence 6 LAND: 4 free in A, 2 at 1 in B.
  objs <- c(
    ca_cap_objs()[c(1, 2, 3, 5)],
    list(
      newCommodity("FUEL", timeframe = "ANNUAL", geoframe = "nation"),
      newSupply("S_FUEL", commodity = "FUEL", region = "P",
                supply = data.frame(region = "P", cost = 0)),
      newTechnology("EWIN", input = list(comm = "FUEL"),
                    output = list(comm = "ELC"), region = "P", cap2act = 1,
                    ceff = data.frame(comm = "FUEL", cinp2use = 1),
                    invcost = data.frame(invcost = 0),
                    vintage = data.frame(olife = 30L),
                    aux = data.frame(acomm = "LAND"),
                    aeff = data.frame(acomm = "LAND", comm = "FUEL",
                                      cinp2ainp = 1))))
  sol <- ca_solve(ca_model("ca3", objs), "ca3")

  ai <- ca_var(sol, "vTechAInp")
  expect_equal(sum(ai$value), ca_sum(sol, "vTechInp"), tolerance = 1e-9)
  expect_equal(sum(ai$value), 6, tolerance = 1e-9)
  expect_setequal(as.character(ai$region), c("A", "B"))
  expect_equal(ca_sum(sol, "vObjective"), 2, tolerance = 1e-9)
})

test_that("a coarse process emits a finer aux commodity into the cells beneath it", {
  skip_if_not_installed("geoscales")
  skip_if_no_solver()
  # the aux OUTPUT side: 1 CRED per unit of capacity, demanded in both children
  objs <- c(
    ca_cap_objs()[c(1, 5)],
    list(
      newCommodity("CRED", timeframe = "ANNUAL"),
      newTechnology("EWIN", output = list(comm = "ELC"), region = "P",
                    cap2act = 1, invcost = data.frame(invcost = 1),
                    vintage = data.frame(olife = 30L),
                    aux = data.frame(acomm = "CRED"),
                    aeff = data.frame(acomm = "CRED", cap2aout = 1)),
      newDemand("DC", commodity = "CRED",
                demand = data.frame(region = c("A", "B"),
                                    timeslice = "ANNUAL", demand = 2))))
  sol <- ca_solve(ca_model("ca4", objs), "ca4")

  ao <- ca_var(sol, "vTechAOut")
  expect_setequal(as.character(ao$region), c("A", "B"))
  # the equation again: the children together carry coefficient x capacity
  expect_equal(sum(ao$value), ca_sum(sol, "vTechCap"), tolerance = 1e-9)
  # and each child's own demand is met from it
  expect_true(all(ao$value >= 2 - 1e-9))
})

test_that("a coarse storage draws a finer aux commodity per child", {
  skip_if_not_installed("geoscales")
  skip_if_no_solver()
  objs <- list(
    newCommodity("ELC", timeframe = "SEASON", geoframe = "nation"),
    newCommodity("LAND", timeframe = "ANNUAL"),
    newSupply("S_ELC", commodity = "ELC", region = "P",
              supply = data.frame(region = "P", cost = 1)),
    newSupply("S_LAND", commodity = "LAND",
              supply = data.frame(region = c("A", "B"), ava.up = c(1, 100),
                                  cost = c(0, 1))),
    newStorage("STG", commodity = "ELC", region = "P",
               seff = data.frame(inpeff = 1, outeff = 1),
               invcost = data.frame(stg.invcost = 0),
               vintage = data.frame(olife = 30L),
               aux = data.frame(acomm = "LAND"),
               aeff = data.frame(acomm = "LAND", stg.cap2ainp = 1)),
    newDemand("D", commodity = "ELC", region = "P",
              demand = data.frame(region = "P", timeslice = c("WIN", "SUM"),
                                  demand = c(1, 3))))
  sol <- ca_solve(ca_model("ca5", objs), "ca5")

  ai <- ca_var(sol, "vStorageAInp")
  expect_false(is.null(ai))
  expect_true(all(as.character(ai$region) %in% c("A", "B")))
  # the reservoir's capacity is what the coupling reads, at the storage's cell
  expect_equal(sum(ai$value), ca_sum(sol, "vStorageStgCap"), tolerance = 1e-9)
})

test_that("a model with no coarse process is untouched", {
  skip_if_not_installed("geoscales")
  # every process at its commodities' level: the enumeration is empty, so the
  # equation domain is exactly the variable domain and the added sum generates
  # nothing
  objs <- ca_cap_objs()
  objs[[1]] <- newCommodity("ELC", timeframe = "SEASON")
  objs[[4]] <- newTechnology(
    "EWIN", output = list(comm = "ELC"), region = c("A", "B"), cap2act = 1,
    invcost = data.frame(invcost = 0), vintage = data.frame(olife = 30L),
    aux = data.frame(acomm = "LAND"),
    aeff = data.frame(acomm = "LAND", cap2ainp = 2))
  objs[[5]] <- newDemand(
    "D", commodity = "ELC",
    demand = data.frame(region = rep(c("A", "B"), each = 2),
                        timeslice = c("WIN", "SUM"), demand = 3))
  scen <- suppressMessages(suppressWarnings(interpolate_model(
    ca_model("ca6", objs), name = "ca6", overwrite = TRUE)))
  gd <- function(nm) {
    p <- scen@modInp@parameters[[nm]]
    if (is.null(p)) return(NULL)
    d <- get_data_slot(p)
    if (is.null(d) || !nrow(d)) NULL else as.data.frame(d)
  }
  expect_null(gd("mTechProcAggRegion"))
  key <- c("tech", "comm", "region", "year", "timeslice")
  mv <- gd("mvTechAInp")[, key]
  eq <- gd("meqTechAInp")[, key]
  expect_equal(nrow(mv), nrow(eq))
  expect_setequal(do.call(paste, mv), do.call(paste, eq))
})

test_that("the coupling is declared at the process's cell, not per child", {
  skip_if_not_installed("geoscales")
  # The coefficient is ONE value per (process, commodity, year, slice), read at
  # the process's own region. Naming a child region in `@aeff` is refused by
  # the `@region`-scope guard rather than silently landing where the equation
  # never reads it -- a per-child LIMIT belongs on the aux commodity's supply
  # (as in the first fixture above), not on the coupling.
  objs <- ca_cap_objs()
  objs[[4]] <- newTechnology(
    "EWIN", output = list(comm = "ELC"), region = "P", cap2act = 1,
    invcost = data.frame(invcost = 0), vintage = data.frame(olife = 30L),
    aux = data.frame(acomm = "LAND"),
    aeff = data.frame(acomm = "LAND", region = c("A", "B"),
                      cap2ainp = c(2, 2)))
  expect_error(
    suppressMessages(suppressWarnings(interpolate_model(
      ca_model("ca8", objs), name = "ca8", overwrite = TRUE))),
    "appear in a slot but are not in its @region scope",
    fixed = TRUE)
})

# -- the same fixture on the other three backends (cross tier) -------------- #
test_that("every backend agrees on a coarse aux draw", {
  skip_if_not_installed("geoscales")
  skip_if_no_solver()
  ref <- ca_solve(ca_model("ca7", ca_cap_objs()), "ca7")
  ref_obj <- ca_sum(ref, "vObjective")

  for (b in c("julia_highs", "pyomo_highs", "mm_highs")) {
    guard <- switch(b,
      julia_highs = skip_if_no_julia_highs,
      pyomo_highs = skip_if_no_pyomo,
      mm_highs    = skip_if_no_multimod)
    ok <- tryCatch({guard(); TRUE}, skip = function(e) FALSE)
    if (!ok) next
    sol <- ca_solve(ca_model(paste0("ca7_", b), ca_cap_objs()),
                    paste0("ca7_", b), solver = b)
    expect_equal(ca_sum(sol, "vObjective"), ref_obj, tolerance = 1e-6,
                 label = paste0(b, " objective"))
    ai <- ca_var(sol, "vTechAInp")
    expect_equal(sum(ai$value), 2 * ca_sum(sol, "vTechCap"),
                 tolerance = 1e-6, label = paste0(b, " aux total"))
  }
})
