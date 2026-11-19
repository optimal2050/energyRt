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

  # both halves keep EVERY milestone, so a saved scenario can describe the
  # whole span; which of them the LP contains is `energyRt:::.lp_years()`
  all_mids <- as.integer(h@intervals$mid)
  expect_identical(as.integer(hh$by@intervals$mid), all_mids)
  expect_identical(as.integer(hh$main@intervals$mid), all_mids)
  expect_identical(energyRt:::.lp_years(hh$by), 2025L)
  expect_identical(energyRt:::.lp_years(hh$main), c(2030L, 2040L))
  expect_identical(hh$history, 2020L)

  # the mark is kept -- energyRt:::.lp_years() needs it -- and the part marker is
  # what makes splitting idempotent
  expect_identical(energyRt:::.base_year(hh$main), 2025L)
  expect_null(energyRt:::.by_split(hh$main))
  # deliberately not a horizon: nothing should be able to solve it
  expect_true(is.integer(hh$history))

  # `@period` is the WHOLE horizon's on both halves: the bounds path
  # interpolates across range(period), so trimming it would clamp every
  # bound's extrapolation to the slice's first milestone
  expect_identical(range(hh$by@period), range(h@period))
  expect_identical(range(hh$main@period), range(h@period))
  expect_identical(min(hh$main@period), 2020L)

  # intervals are carried over untouched, not rebuilt: every milestone keeps
  # the start/end it had, so period lengths and cost weights are unchanged
  m <- as.data.frame(hh$main@intervals)
  r30 <- m[as.integer(m$mid) == 2030L, ]
  expect_identical(as.integer(r30$start), 2026L)
  expect_identical(as.integer(r30$end), 2035L)
  expect_identical(as.data.frame(hh$main@intervals),
                   as.data.frame(h@intervals))

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
  # Supplying a base-year source is what says "already optimised elsewhere",
  # and so what splits the horizon. Without one the base year is solved HERE,
  # with the projection -- covered by its own test below. The source need not
  # hold a solution: naming one is the request, and the split follows from it.
  src <- suppressMessages(interpolate_model(
    .by_model(newHorizon(intervals = .by_iv(), name = "hsrc")),
    name = "by_src"))
  s <- suppressMessages(interpolate_model(
    .by_model(newHorizon(intervals = .by_iv(), base_year = 2025, name = "h25")),
    name = "by_prj", base_year_scenario = src))

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
  # `capacity = "free"`: this test is about what the base year DECIDES and
  # what crosses from it. The default pins total capacity at stock, which is
  # right for a calibration run but leaves nothing to decide or carry.
  by <- suppressMessages(solve_base_year(
    mod, name = "by_solve", solver = solver_options$glpk, verbose = FALSE,
    capacity = "free"))

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
    mk(), name = "by_solve", solver = solver_options$glpk, verbose = FALSE,
    capacity = "free"))
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
    mk(), name = "by_solvel", solver = solver_options$glpk, verbose = FALSE,
    capacity = "free"))
  skip_if(is.null(by$ledger) || !nrow(as.data.frame(by$ledger$newcap)))

  s <- suppressMessages(interpolate_model(mk(), name = "by_prjl",
                                          base_year_scenario = by))
  cap <- s@model@data$byrepo@data$NEW@capacity
  alive <- as.integer(cap$year[!is.na(cap$stock) & cap$stock > 0])
  expect_true(2030L %in% alive)
  expect_false(2040L %in% alive)
})


test_that("a marked horizon with no source is solved WITH the projection", {
  mk <- function() .by_model(newHorizon(intervals = .by_iv(),
                                        base_year = 2025, name = "h25"))
  expect_message(
    s <- interpolate_model(mk(), name = "by_prjn"),
    "solved WITH the projection")

  # No split at all: every milestone the horizon declares is in this LP,
  # history included, each carrying its own period weight -- so observed
  # dispatch is priced rather than arbitrary, and nothing feeds the first
  # projection year from an unpriced state.
  expect_identical(sort(as.integer(s@modInp@sets$year)),
                   c(2020L, 2025L, 2030L, 2040L))
  expect_identical(energyRt:::.lp_years(getHorizon(s)),
                   c(2020L, 2025L, 2030L, 2040L))
  expect_identical(s@misc$base_year$mode, "together")
  expect_identical(s@misc$base_year$capacity, "fixed")

  # Those years are OBSERVED, so their total capacity is data, not a
  # decision: the upward bound is pinned at every year up to the mark. That
  # is what keeps a past year from building its way out of the fleet being
  # calibrated -- the price of leaving history in the LP.
  up <- s@model@data$byrepo@data$NEW@capacity
  pinned <- as.integer(up$year[!is.na(up$ncap.up) & up$ncap.up == 0])
  expect_true(all(c(2020L, 2025L) %in% pinned))
  expect_false(any(c(2030L, 2040L) %in% pinned))
})


test_that("the base year may be left free with the projection", {
  mk <- function() .by_model(newHorizon(intervals = .by_iv(),
                                        base_year = 2025, name = "h25"))
  expect_message(
    s <- interpolate_model(mk(), name = "by_prjf",
                           base_year_capacity = "free"),
    "capacity free")
  expect_identical(s@misc$base_year$capacity, "free")
  # untouched: the caller owns the consequence
  expect_identical(mk()@data$byrepo@data$NEW@capacity,
                   s@model@data$byrepo@data$NEW@capacity)
})


test_that("a named process is left free despite the pin", {
  mk <- function() .by_model(newHorizon(intervals = .by_iv(),
                                        base_year = 2025, name = "h25"))
  s <- suppressMessages(interpolate_model(mk(), name = "by_prjr",
                                          base_year_relax = "NEW"))
  expect_identical(s@misc$base_year$relax, "NEW")
  # NEW keeps its freedom, OLD does not
  expect_identical(mk()@data$byrepo@data$NEW@capacity,
                   s@model@data$byrepo@data$NEW@capacity)
  old <- s@model@data$byrepo@data$OLD@capacity
  expect_true(all(old$ncap.up[as.integer(old$year) == 2025L] == 0))
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
    mk(), name = "by_solves", solver = solver_options$glpk, verbose = FALSE,
    capacity = "free"))
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


test_that("getData(base_year = TRUE) labels its source and shows vObjective", {
  skip_if_no_solver()
  mk <- function() .by_model(newHorizon(intervals = .by_iv(),
                                        base_year = 2025, name = "h25"))
  by <- suppressMessages(solve_base_year(
    mk(), name = "by_solveg", solver = solver_options$glpk, verbose = FALSE,
    capacity = "free"))
  s <- suppressMessages(interpolate_model(mk(), name = "by_prjg",
                                          base_year_scenario = by))
  s <- suppressMessages(save_scenario(s, verbose = FALSE))
  s <- suppressMessages(solve_scenario(s, solver = solver_options$glpk,
                                       verbose = FALSE))

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

  # Asking for the base year is asking to see its cost, so both objectives
  # come back -- but they are separate optimisations, one undiscounted year
  # against a discounted multi-period total, so the warning says they must
  # not be summed and `source` is what keeps them apart.
  expect_warning(
    obj <- getData(s, "vObjective", merge = TRUE, base_year = TRUE),
    "do not sum into a system cost")
  expect_setequal(unique(obj$source), c("solved", "base"))

  # a scenario recording no base year says so rather than inventing one
  plain <- suppressMessages(save_scenario(suppressMessages(interpolate_model(
    .by_model(newHorizon(intervals = .by_iv(), name = "hfull")),
    name = "by_fullg")), verbose = FALSE))
  expect_warning(getData(plain, "pDemand", merge = TRUE, base_year = TRUE),
                 "records no base year")
})

# --------------------------------------------------------------------------- #
# A period year that belongs to no interval is modelled by nothing, which is a
# legitimate snapshot horizon and also what a mistyped interval table looks
# like. It is reported, not refused.
# --------------------------------------------------------------------------- #

test_that("newHorizon() warns about period years no interval covers", {
  expect_warning(
    h <- newHorizon(intervals = data.frame(
      start = c(2025, 2030, 2040, 2050),
      mid   = c(2025, 2030, 2040, 2050),
      end   = c(2025, 2030, 2040, 2050))),
    "belong to no interval")
  # built anyway, and the runs are named rather than every year listed
  expect_equal(nrow(h@intervals), 4L)
  w <- tryCatch(newHorizon(intervals = data.frame(
      start = c(2025, 2030, 2040, 2050),
      mid   = c(2025, 2030, 2040, 2050),
      end   = c(2025, 2030, 2040, 2050))),
    warning = function(e) conditionMessage(e))
  expect_match(w, "2026-2029")
  expect_match(w, "22 of 26")
})

test_that("a horizon whose intervals cover the period is silent", {
  expect_silent(newHorizon(2025:2050))
  expect_silent(newHorizon(2020:2030, c(1, 2, 5, 10)))
  expect_silent(newHorizon(intervals = data.frame(
    start = c(2025, 2026, 2036),
    mid   = c(2025, 2030, 2040),
    end   = c(2025, 2035, 2045))))
})

# A ledger is solved against the model it came from. Carried into a SUBSET of
# that model it still holds the regions the cut removed, and writing their
# capacity back used to surface much later as "references undeclared
# region(s)" -- the right error from the wrong place.
test_that("a carried ledger is narrowed to the model's regions", {
  testthat::skip_if_not_installed("geoscales")
  gs <- geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", grp = "G1", region = c("R1", "R2"),
               km2 = c(1, 1)),
    geoframes = c("nation", "grp", "region"), key = "region", name = "bl")
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = "bl_cal")
  mod <- setGeoscale(newModel("bl", calendar = cal, region = "R1",
    horizon = newHorizon(c(2025, 2030), intervals = 5), discount = 0,
    repo = newRepository("bl_repo",
      newCommodity("ELC", timeframe = "SEASON"),
      newTechnology("ECOA", output = list(comm = "ELC"),
        capacity = data.frame(region = "R1", stock = 10),
        invcost = data.frame(invcost = 1000),
        olife = data.frame(olife = 30), cap2act = 1))),
    geoscales::filter_geoscale(gs, "region", "R1"))
  led <- structure(list(
    newcap = tibble::tibble(family = "tech", proc = "ECOA",
      region = c("R1", "R2"), vintage = 2025L, cap = c(7, 9), olife = 30),
    retnew = tibble::tibble(family = character(), proc = character(),
      region = character(), vintage = integer(), cap = numeric(),
      ret_year = integer()),
    retstock = tibble::tibble(family = character(), proc = character(),
      region = character(), amount = numeric(), ret_year = integer()),
    sup_use = tibble::tibble()), class = "carry_ledger")
  out <- .by_carry_newcap(mod, led, newHorizon(c(2025, 2030), intervals = 5),
                          verbose = FALSE)
  cap <- getObject(out, class = "technology")[["ECOA"]]@capacity
  # only the kept region is written, and it gained the carried capacity
  expect_setequal(as.character(cap$region), "R1")
  expect_true(all(cap$stock == 17))
  # the rows left out are named when the call is verbose
  expect_message(.by_carry_newcap(mod, led,
    newHorizon(c(2025, 2030), intervals = 5), verbose = TRUE),
    "does not declare")
})

# --------------------------------------------------------------------------- #
# A bound binds only the variants its key names. A vintaged or clustered
# process whose `ncap.up` row names one vintage leaves every other vintage
# free to be built -- which is how an observed year built 980 GW of capacity
# while reporting that 72 processes had been pinned.

.by_vin_model <- function() {
  newModel("by_vin", region = "R1", discount = 0.05, calendar = .by_cal(),
    horizon = newHorizon(intervals = .by_iv(), base_year = 2025, name = "hv"),
    repo = newRepository("vrepo",
      newCommodity("ELC", timeframe = "SL"),
      newCommodity("GAS", timeframe = "ANNUAL"),
      newSupply("SUP", commodity = "GAS",
                supply = data.frame(region = "R1", cost = 15)),
      newTechnology("TV", input = list(comm = "GAS"),
        output = list(comm = "ELC"),
        ceff = data.frame(comm = "GAS", cinp2use = 1),
        # the shipped shape: one bound, scoped to a single vintage
        capacity = data.frame(vintage = "2020", year = 2025L,
                              stock = 40, ncap.up = 0),
        vintage = data.frame(vintage = c("2020", "2025"),
                             start = c(2020L, 2025L), olife = c(30L, 30L)),
        invcost = list(invcost = 900), cap2act = 1),
      newDemand("DEM", commodity = "ELC",
        demand = data.frame(region = "R1",
                            year = c(2025L, 2030L, 2040L),
                            demand = c(35, 60, 80)))))
}

# the map is the evidence: a bound exists only where the parameter is
# populated, so a missing entry is an unbounded column, not a loose one
.by_ncap_up_map <- function(scen) {
  m <- scen@modInp@parameters[["mTechNewCapUp"]]
  if (is.null(m)) return(data.frame())
  as.data.frame(energyRt:::get_lazy_data(m, slot = "data"))
}

test_that("the pin binds every variant, not the one its row names", {
  mod <- .by_vin_model()

  # untouched: the shipped row leaves the 2025 vintage free
  bare <- suppressMessages(interpolate_model(
    mod, name = "by_vin_bare", base_year_capacity = "free"))
  mb <- .by_ncap_up_map(bare)
  expect_false("TV_VIN2025" %in% mb$tech)

  # pinned: every variant in the roster is bounded at the base year
  pin <- suppressMessages(interpolate_model(mod, name = "by_vin_pin"))
  mp <- .by_ncap_up_map(pin)
  at25 <- mp[as.integer(mp$year) == 2025L, ]
  expect_true(all(c("TV_VIN2020", "TV_VIN2025") %in% at25$tech))

  # and the bound is zero for both, not merely present
  v <- as.data.frame(energyRt:::get_lazy_data(
    pin@modInp@parameters[["pTechNewCap"]], slot = "data"))
  v <- v[as.integer(v$year) == 2025L & v$type == "up", ]
  expect_setequal(v$tech, c("TV_VIN2020", "TV_VIN2025"))
  expect_true(all(v$value == 0))
})


test_that("the pin overrides a narrower bound that disagrees", {
  mod <- .by_vin_model()
  # a specific row that would otherwise allow a build
  cp <- mod@data$vrepo@data$TV@capacity
  cp$ncap.up <- 999
  mod@data$vrepo@data$TV@capacity <- cp

  pin <- suppressMessages(interpolate_model(mod, name = "by_vin_ovr"))
  v <- as.data.frame(energyRt:::get_lazy_data(
    pin@modInp@parameters[["pTechNewCap"]], slot = "data"))
  v <- v[as.integer(v$year) == 2025L & v$type == "up", ]
  expect_true(all(v$value == 0))
})


test_that("the pin does not duplicate a key it already occupies", {
  mod <- .by_vin_model()
  # an all-NA-variant row already at the base year: every trade has one, and
  # a second row on the same key stops interpolation outright
  cp <- mod@data$vrepo@data$TV@capacity
  blank <- cp[NA_integer_, , drop = FALSE][1, , drop = FALSE]
  blank$vintage <- NA_character_; blank$year <- 2025L; blank$ncap.up <- 7
  cp <- rbind(cp, blank); rownames(cp) <- NULL
  mod@data$vrepo@data$TV@capacity <- cp

  expect_no_error(
    pin <- suppressMessages(interpolate_model(mod, name = "by_vin_dup")))
  v <- as.data.frame(energyRt:::get_lazy_data(
    pin@modInp@parameters[["pTechNewCap"]], slot = "data"))
  v <- v[as.integer(v$year) == 2025L & v$type == "up", ]
  expect_true(all(v$value == 0))
})


test_that("together mode freezes retirement as well as investment", {
  # total capacity, not investment alone: with retirement endogenous the LP
  # can shrink an observed fleet instead of growing it
  mod <- .by_model(newHorizon(intervals = .by_iv(), base_year = 2025,
                              name = "h25"))
  s <- suppressMessages(interpolate_model(mod, name = "by_tog_ret"))
  expect_false(any(grepl("^vTechRetCap$", names(s@modInp@parameters))))
  ret <- s@modInp@parameters[["mTechRetireNewCap"]] %||%
    s@modInp@parameters[["mvTechRetiredNewCap"]]
  if (!is.null(ret)) {
    d <- as.data.frame(energyRt:::get_lazy_data(ret, slot = "data"))
    expect_true(!nrow(d) || !any(as.integer(d$year) == 2025L))
  }
})

# --------------------------------------------------------------------------- #
# A derived scenario re-enters interpolate_model() through
# .interpolate_on_base(), where `base` is no longer in scope. Without the
# base's recorded treatment being carried over, `base_year_scenario` defaults
# to NULL there and the derived scenario solves the base year WITH the
# projection -- splitting one way for REF and the other for everything built
# on it.

test_that("a derived scenario inherits its base's base-year mode", {
  mk <- function() .by_model(newHorizon(intervals = .by_iv(),
                                        base_year = 2025, name = "h25"))
  src <- suppressMessages(interpolate_model(
    .by_model(newHorizon(intervals = .by_iv(), name = "hsrc2")),
    name = "by_dsrc"))
  ref <- suppressMessages(interpolate_model(mk(), name = "by_dref",
                                            base_year_scenario = src))
  expect_identical(ref@misc$base_year$mode, "separate")

  # a SAMPLED calendar is what routes this through the re-entering path
  cal2 <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SL = sprintf("s%02d", 1:2))),
    name = "bycal2")
  sub <- suppressMessages(interpolate_model(
    mk(), name = "by_dsub", base = ref, calendar = cal2))

  expect_identical(sub@misc$base_year$mode, "separate")
  expect_identical(solved_years_of <- energyRt:::.lp_years(getHorizon(sub)),
                   c(2030L, 2040L))
  expect_false(any(as.integer(sub@modInp@sets$year) == 2025L))
})


test_that("the split is idempotent for a horizon that arrives split", {
  # a derived call that inherits its base's settings gets the projection half
  # already; splitting it again returns NULL and must not be treated as a
  # failure to split
  h <- newHorizon(intervals = .by_iv(), base_year = 2025, name = "hidem")
  hh <- suppressMessages(energyRt:::.by_split(h))
  expect_null(energyRt:::.by_split(hh$main))
  expect_identical(energyRt:::.lp_years(hh$main), c(2030L, 2040L))
})
