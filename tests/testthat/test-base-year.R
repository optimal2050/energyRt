# The base year: a milestone that is given rather than decided.
#
# Three groups of years, not two: history (before the mark) enters no LP at
# all, the base year is solved on its own, and the projection is what the
# solver optimises. Every year that leaves still anchors interpolation.

.by_cal <- function() {
  newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SL = sprintf("s%02d", 1:4))),
    name = "bycal")
}

# 2020 history, 2025 base year, 2030/2040 projection
.by_iv <- function() {
  data.frame(start = c(2020, 2025, 2026, 2036),
             mid   = c(2020, 2025, 2030, 2040),
             end   = c(2020, 2025, 2035, 2045))
}

.by_model <- function(horizon, olife = 30, name = "by_mod") {
  newModel(name, region = "R1", discount = 0.05, calendar = .by_cal(),
           horizon = horizon,
           repo = newRepository("byrepo",
             newCommodity("ELC", timeframe = "SL"),
             newCommodity("GAS", timeframe = "ANNUAL"),
             newSupply("SUP", commodity = "GAS",
                       supply = data.frame(region = "R1", cost = 15)),
             newTechnology("OLD", input = list(comm = "GAS"),
                           output = list(comm = "ELC"),
                           ceff = data.frame(comm = "GAS", cinp2use = 1),
                           capacity = data.frame(
                             region = "R1",
                             year = c(2020L, 2025L, 2030L, 2040L),
                             stock = c(40, 30, 20, 5), ncap.up = 0),
                           invcost = list(invcost = 1000),
                           olife = list(olife = 30), cap2act = 1,
                           optimizeRetirement = TRUE),
             newTechnology("NEW", input = list(comm = "GAS"),
                           output = list(comm = "ELC"),
                           ceff = data.frame(comm = "GAS", cinp2use = 1),
                           invcost = list(invcost = 900),
                           olife = list(olife = olife), cap2act = 1),
             newDemand("DEM", commodity = "ELC",
                       demand = data.frame(
                         region = "R1",
                         year = c(2020L, 2025L, 2030L, 2040L),
                         demand = c(35, 40, 60, 80)))))
}


test_that("the mark is a single year, or nothing", {
  iv <- .by_iv()
  h <- newHorizon(intervals = iv, base_year = 2025, name = "h25")
  expect_identical(h@base_year, 2025L)
  # the horizon's own geometry is untouched by the mark
  expect_identical(as.integer(h@intervals$mid), c(2020L, 2025L, 2030L, 2040L))

  expect_identical(newHorizon(intervals = iv)@base_year, integer(0))

  expect_error(newHorizon(intervals = iv, base_year = c(2020, 2025)),
               "single year")
  expect_error(newHorizon(intervals = iv, base_year = TRUE),
               "not a flag")
  expect_error(newHorizon(intervals = iv, base_year = 2027),
               "not a milestone")
  # the last milestone leaves nothing to project
  expect_error(newHorizon(intervals = iv, base_year = 2040),
               "nothing to project")
})


test_that("the split is three-way and keeps the original period", {
  h <- newHorizon(intervals = .by_iv(), base_year = 2025, name = "h25")
  expect_message(energyRt:::.by_split(h), "2020")
  hh <- suppressMessages(energyRt:::.by_split(h))

  expect_identical(as.integer(hh$by@intervals$mid), 2025L)
  expect_identical(as.integer(hh$main@intervals$mid), c(2030L, 2040L))
  expect_identical(hh$history, 2020L)
  # deliberately not a horizon: nothing should be able to solve it
  expect_true(is.integer(hh$history))

  # `@period` is the WHOLE horizon's on both halves: the bounds path
  # interpolates across range(period), so trimming it would clamp every
  # bound's extrapolation to the slice's first milestone
  expect_identical(range(hh$by@period), range(h@period))
  expect_identical(range(hh$main@period), range(h@period))
  expect_identical(min(hh$main@period), 2020L)

  # row slices, not rebuilt intervals: period lengths are untouched
  m <- as.data.frame(hh$main@intervals)
  expect_identical(as.integer(m$start[1]), 2026L)
  expect_identical(as.integer(m$end[1]), 2035L)

  # the halves need distinct names -- a scenario folder is named after its
  # horizon
  expect_false(identical(hh$by@name, hh$main@name))

  expect_null(energyRt:::.by_split(newHorizon(intervals = .by_iv())))
})


test_that("a horizon with no history splits silently", {
  iv <- .by_iv()[-1, , drop = FALSE]
  h <- newHorizon(intervals = iv, base_year = 2025, name = "h25b")
  expect_no_message(hh <- energyRt:::.by_split(h))
  expect_identical(hh$history, integer(0))
})


test_that("a horizon serialized before the slot still reads", {
  h <- newHorizon(intervals = .by_iv())
  attributes(h)$base_year <- NULL
  f <- withr::local_tempfile(fileext = ".rds")
  saveRDS(h, f)
  back <- readRDS(f)
  expect_null(energyRt:::.base_year(back))
  expect_null(energyRt:::.by_split(back))
})


test_that("the mark takes the base year and its history out of the LP", {
  s <- suppressMessages(interpolate_model(
    .by_model(newHorizon(intervals = .by_iv(), base_year = 2025, name = "h25")),
    name = "by_prj"))

  expect_identical(sort(as.integer(s@modInp@sets$year)), c(2030L, 2040L))
  expect_identical(
    sort(as.integer(s@modInp@parameters[["year"]]@data$year)),
    c(2030L, 2040L))
  # the stock initial condition is read at mMilestoneFirst, which must name a
  # year the LP actually contains
  expect_identical(
    as.integer(s@modInp@parameters[["mMilestoneFirst"]]@data$year), 2030L)

  expect_identical(s@misc$base_year$year, 2025L)
  expect_identical(s@misc$base_year$history, 2020L)
  # no source was given, so nothing was carried
  expect_null(s@misc$base_year$hash)
})


test_that("the years that left still anchor interpolation", {
  marked <- suppressMessages(interpolate_model(
    .by_model(newHorizon(intervals = .by_iv(), base_year = 2025, name = "h25")),
    name = "by_prj"))
  full <- suppressMessages(interpolate_model(
    .by_model(newHorizon(intervals = .by_iv(), name = "hfull")), name = "by_full"))

  pick <- function(x) {
    d <- as.data.frame(x@modInp@parameters[["pDemand"]]@data)
    d <- d[d$year %in% c(2030L, 2040L), ]
    d[order(d$year, d$slice %||% d$timeslice), "value"]
  }
  expect_equal(pick(marked), pick(full))

  # and the unmarked model is untouched
  expect_null(full@misc$base_year)
  expect_identical(sort(as.integer(full@modInp@sets$year)),
                   c(2020L, 2025L, 2030L, 2040L))
})


test_that("the base-year solve is the base year alone, with retirement off", {
  skip_if_no_solver()
  mod <- .by_model(newHorizon(intervals = .by_iv(), base_year = 2025,
                              name = "h25"))
  by <- suppressMessages(solve_base_year(
    mod, name = "by_solve", solver = solver_options$glpk, echo = FALSE))

  expect_identical(sort(as.integer(by$scenario@modInp@sets$year)), 2025L)
  expect_true(isTRUE(by$scenario@status$optimal))
  expect_identical(by$base_year, 2025L)
  expect_s3_class(by$ledger, "carry_ledger")

  # observed retirements are data, not decisions: left on, the ledger consumes
  # them FIFO against the oldest carried vintages and mis-dates the carry
  frozen <- energyRt:::.by_freeze_retirement(mod)
  ret <- vapply(frozen@data$byrepo@data, function(o)
    if (methods::.hasSlot(o, "optimizeRetirement")) isTRUE(o@optimizeRetirement)
    else NA, logical(1))
  expect_false(any(ret, na.rm = TRUE))

  expect_error(solve_base_year(.by_model(newHorizon(intervals = .by_iv()))),
               "marks no base year")
})


test_that("the carry writes only what the base year decided", {
  skip_if_no_solver()
  mk <- function() .by_model(newHorizon(intervals = .by_iv(),
                                        base_year = 2025, name = "h25"))
  by <- suppressMessages(solve_base_year(
    mk(), name = "by_solve", solver = solver_options$glpk, echo = FALSE))
  built <- as.data.frame(by$ledger$newcap)
  expect_true(nrow(built) >= 1L)
  expect_true("NEW" %in% built$proc)

  s <- suppressMessages(interpolate_model(mk(), name = "by_prjc",
                                          base_year_scenario = by))

  # OLD was not built in the base year, so its declared stock -- where
  # observed history lives, with its vintages -- is untouched
  expect_identical(mk()@data$byrepo@data$OLD@capacity,
                   s@model@data$byrepo@data$OLD@capacity)

  # NEW was built, so it crosses as stock at the projection milestones
  cap <- s@model@data$byrepo@data$NEW@capacity
  expect_setequal(as.integer(cap$year[!is.na(cap$stock)]), c(2030L, 2040L))
  expect_true(all(cap$stock[!is.na(cap$stock)] <=
                    max(built$cap[built$proc == "NEW"]) + 1e-9))

  # and it reaches the LP
  stk <- as.data.frame(s@modInp@parameters[["pTechStock"]]@data)
  expect_true("NEW" %in% stk$tech)

  expect_identical(s@misc$base_year$scenario, "by_solve")
  expect_true(nzchar(s@misc$base_year$hash))
})


test_that("the carry respects the operating life", {
  skip_if_no_solver()
  # olife 10 from a 2025 vintage: alive at 2030, gone by 2040
  mk <- function() .by_model(newHorizon(intervals = .by_iv(),
                                        base_year = 2025, name = "h25"),
                             olife = 10)
  by <- suppressMessages(solve_base_year(
    mk(), name = "by_solvel", solver = solver_options$glpk, echo = FALSE))
  skip_if(is.null(by$ledger) || !nrow(as.data.frame(by$ledger$newcap)))

  s <- suppressMessages(interpolate_model(mk(), name = "by_prjl",
                                          base_year_scenario = by))
  cap <- s@model@data$byrepo@data$NEW@capacity
  alive <- as.integer(cap$year[!is.na(cap$stock) & cap$stock > 0])
  expect_true(2030L %in% alive)
  expect_false(2040L %in% alive)
})


test_that("a marked horizon with no source proceeds from the model's stock", {
  mk <- function() .by_model(newHorizon(intervals = .by_iv(),
                                        base_year = 2025, name = "h25"))
  expect_message(
    s <- interpolate_model(mk(), name = "by_prjn"),
    "no base-year solution was given")
  # the model's own stock is what the projection starts from, unchanged
  expect_identical(mk()@data$byrepo@data$OLD@capacity,
                   s@model@data$byrepo@data$OLD@capacity)
  expect_identical(mk()@data$byrepo@data$NEW@capacity,
                   s@model@data$byrepo@data$NEW@capacity)
})


test_that("a source that holds no base-year solution carries nothing", {
  mk <- function() .by_model(newHorizon(intervals = .by_iv(),
                                        base_year = 2025, name = "h25"))
  unsolved <- suppressMessages(interpolate_model(
    .by_model(newHorizon(intervals = .by_iv(), name = "hfull")), name = "by_full"))
  expect_message(
    interpolate_model(mk(), name = "by_prju", base_year_scenario = unsolved),
    "no base-year solution was given")

  expect_error(
    suppressMessages(interpolate_model(mk(), name = "by_prjb",
                                       base_year_scenario = 42)),
    "must be a scenario")
})


test_that("the link survives a save and names a stale source", {
  skip_if_no_solver()
  mk <- function() .by_model(newHorizon(intervals = .by_iv(),
                                        base_year = 2025, name = "h25"))
  by <- suppressMessages(solve_base_year(
    mk(), name = "by_solves", solver = solver_options$glpk, echo = FALSE))
  s <- suppressMessages(interpolate_model(mk(), name = "by_prjs",
                                          base_year_scenario = by))
  s <- suppressMessages(save_scenario(s, verbose = FALSE))

  mf <- yaml::read_yaml(fp(s@path, "scenario.yml"))
  expect_identical(as.integer(mf$base_year$year), 2025L)
  expect_identical(as.integer(mf$base_year$history), 2020L)
  expect_identical(mf$base_year$scenario, "by_solves")
  expect_true(nzchar(mf$base_year$hash))

  link <- energyRt:::.by_link_of(s)
  expect_identical(link$year, 2025L)
  expect_false(energyRt:::.by_link_stale(link, s))

  # a link whose recorded answer is not the one the source now holds
  link$hash <- "not-the-hash"
  expect_true(energyRt:::.by_link_stale(link, s))
})


test_that("getData(base_year = TRUE) labels its source and refuses vObjective", {
  skip_if_no_solver()
  mk <- function() .by_model(newHorizon(intervals = .by_iv(),
                                        base_year = 2025, name = "h25"))
  by <- suppressMessages(solve_base_year(
    mk(), name = "by_solveg", solver = solver_options$glpk, echo = FALSE))
  s <- suppressMessages(interpolate_model(mk(), name = "by_prjg",
                                          base_year_scenario = by))
  s <- suppressMessages(save_scenario(s, verbose = FALSE))
  s <- suppressMessages(solve_scenario(s, solver = solver_options$glpk,
                                       echo = FALSE))

  own <- getData(s, "vTechCap", merge = TRUE)
  expect_false("source" %in% names(own))
  expect_false(any(as.integer(own$year) == 2025L))

  both <- suppressWarnings(getData(s, "vTechCap", merge = TRUE,
                                   base_year = TRUE))
  expect_true("source" %in% names(both))
  expect_setequal(unique(both$source), c("solved", "base"))
  expect_true(any(as.integer(both$year) == 2025L &
                    both$source == "base"))
  expect_true(all(both$source[as.integer(both$year) > 2025L] == "solved"))

  # the two objectives are separate optimisations and do not union
  obj <- suppressWarnings(getData(s, "vObjective", merge = TRUE,
                                  base_year = TRUE))
  expect_true(all(obj$source == "solved"))

  # a scenario recording no base year says so rather than inventing one
  plain <- suppressMessages(save_scenario(suppressMessages(interpolate_model(
    .by_model(newHorizon(intervals = .by_iv(), name = "hfull")),
    name = "by_fullg")), verbose = FALSE))
  expect_warning(getData(plain, "pDemand", merge = TRUE, base_year = TRUE),
                 "records no base year")
})
