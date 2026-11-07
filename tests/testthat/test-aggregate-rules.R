# =========================================================================== #
# Aggregation rules: which columns add, which average, which are copied.
#
# Two atoms in one group, with lopsided capacity so a weighted mean differs
# from a plain one. Each test pins one rule that `.agg_rules()` gets wrong.
# =========================================================================== #

skip_if_no_geoscales <- function() {
  testthat::skip_if_not_installed("geoscales")
}

ar_geoscale <- function() {
  geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", grp = c("G1", "G1"), region = c("R1", "R2"),
               km2 = c(3, 1)),
    geoframes = c("nation", "grp", "region"), key = "region", name = "ar")
}

ar_cal <- function() {
  newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = "ar_cal")
}

# R1 carries 300 units of capacity, R2 carries 100.
ar_model <- function(objs) {
  newModel(name = "ar", desc = "", calendar = ar_cal(),
           region = c("R1", "R2"),
           horizon = newHorizon(2020:2030, intervals = 10), discount = 0.05,
           repo = do.call(newRepository, c(list("ar_repo"), objs)))
}

ar_base <- function() {
  list(
    newCommodity("ELC", timeframe = "SEASON"),
    newTechnology(
      "ETEC", input = list(comm = "ELC"), output = list(comm = "ELC"),
      capacity = data.frame(region = c("R1", "R2"), stock = c(300, 100)),
      vintage = data.frame(olife = 30L), cap2act = 1)
  )
}

ar_agg <- function(objs, ...) {
  mod <- setGeoscale(ar_model(objs), ar_geoscale())
  suppressWarnings(aggregate_model_regions(mod, level = "grp",
                                           verbose = FALSE, ...))
}

test_that("a storage start level adds up, it is not averaged", {
  skip_if_no_geoscales()
  stg <- newStorage(
    "STG", commodity = "ELC",
    capacity = data.frame(region = c("R1", "R2"), out.stock = c(300, 100)),
    startLevel = data.frame(region = c("R1", "R2"), startLevel = c(10, 20)))
  agg <- ar_agg(c(ar_base(), list(stg)))
  sl <- agg@data[[1]]@data$STG@startLevel
  # an energy level, dimensionally the same as out.stock, which is summed
  expect_equal(sum(sl$startLevel), 30)
})

ar_payback <- function(pb) {
  objs <- ar_base()
  objs[[2]]@invcost <- data.frame(region = c("R1", "R2"),
                                  invcost = c(100, 300), payback = pb)
  objs
}

test_that("one payback survives aggregation unchanged", {
  skip_if_no_geoscales()
  agg <- ar_agg(ar_payback(c(10, 10)))
  expect_equal(unique(agg@data[[1]]@data$ETEC@invcost$payback), 10)
})

test_that("a payback conflict is refused, not averaged", {
  skip_if_no_geoscales()
  # a lifetime in years: 10 and 20 have no merged value
  expect_error(ar_agg(ar_payback(c(10, 20))), "payback")
})

test_that("resolve_conflicts averages, warns and records the conflict", {
  skip_if_no_geoscales()
  mod <- setGeoscale(ar_model(ar_payback(c(10, 20))), ar_geoscale())
  expect_warning(
    agg <- aggregate_model_regions(mod, level = "grp", verbose = FALSE,
                                   resolve_conflicts = TRUE),
    "payback")
  o <- agg@data[[1]]@data$ETEC
  expect_equal(unique(o@invcost$payback), 15)
  expect_true(nrow(o@misc$aggregation$conflicts) >= 1)
  expect_true("payback" %in% o@misc$aggregation$conflicts$column)
})

test_that('resolve_conflicts = "drop" leaves the value unset', {
  skip_if_no_geoscales()
  mod <- setGeoscale(ar_model(ar_payback(c(10, 20))), ar_geoscale())
  expect_warning(
    agg <- aggregate_model_regions(mod, level = "grp", verbose = FALSE,
                                   resolve_conflicts = "drop"),
    "payback")
  o <- agg@data[[1]]@data$ETEC
  expect_true(all(is.na(o@invcost$payback)))
  # the column that did not conflict is untouched
  expect_true(all(!is.na(o@invcost$invcost)))
  expect_equal(unique(o@misc$aggregation$conflicts$resolution), "drop")
})

test_that('resolve_conflicts = "force" is TRUE, and a bad value is refused', {
  skip_if_no_geoscales()
  mod <- setGeoscale(ar_model(ar_payback(c(10, 20))), ar_geoscale())
  expect_warning(
    agg <- aggregate_model_regions(mod, level = "grp", verbose = FALSE,
                                   resolve_conflicts = "force"),
    "payback")
  expect_equal(unique(agg@data[[1]]@data$ETEC@invcost$payback), 15)
  expect_error(
    aggregate_model_regions(mod, level = "grp", resolve_conflicts = "maybe"),
    "resolve_conflicts")
})

test_that("a weather profile is weighted by the scale, not averaged flat", {
  skip_if_no_geoscales()
  objs <- ar_base()
  objs[[2]]@weather <- data.frame(weather = "WSOL", comm = "ELC", waf.up = 1)
  wth <- newWeather(
    "WSOL", timeframe = "SEASON",
    weather = data.frame(region = rep(c("R1", "R2"), each = 2),
                         timeslice = c("WIN", "SUM"),
                         wval = rep(c(0.2, 0.8), each = 2)))
  agg <- ar_agg(c(objs, list(wth)))
  w <- agg@data[[1]]@data$WSOL@weather
  # a profile has no size of its own, so the geoscale's weight decides:
  # 3 km2 at 0.2 and 1 km2 at 0.8 -> 0.35, not the plain mean 0.5
  expect_equal(unique(round(w$wval, 6)), 0.35)
})

test_that("a per-region constraint budget adds up", {
  skip_if_no_geoscales()
  cns <- newConstraint(
    "CAP", eq = "<=",
    for.each = data.frame(region = c("R1", "R2")),
    rhs = data.frame(region = c("R1", "R2"), rhs = c(40, 60)),
    defVal = Inf,
    term1 = list(variable = "vTechCap", for.sum = list(tech = "ETEC")))
  agg <- ar_agg(c(ar_base(), list(cns)))
  expect_equal(sum(agg@data[[1]]@data$CAP@rhs$rhs), 100)
})

test_that("a levy rate is copied, and a conflict is refused", {
  skip_if_no_geoscales()
  mk <- function(rate) newTax("TCO2", comm = "ELC",
                              tax = data.frame(region = c("R1", "R2"),
                                               bal = rate))
  agg <- ar_agg(c(ar_base(), list(mk(c(0.02, 0.02)))))
  expect_equal(unique(agg@data[[1]]@data$TCO2@tax$bal), 0.02)
  expect_error(ar_agg(c(ar_base(), list(mk(c(0.02, 0.08))))), "bal")
})

test_that("cluster is an identifier when recasting a calendar", {
  skip_if_no_geoscales()
  expect_true("cluster" %in% energyRt:::.recast_id_cols)
})

test_that("a ramp time is copied, and a conflict is refused", {
  skip_if_no_geoscales()
  mk <- function(r) {
    objs <- ar_base()
    objs[[2]]@af <- data.frame(region = c("R1", "R2"), rampup = r)
    objs
  }
  # a ramp time is a property of the technology, not of the region
  agg <- ar_agg(mk(c(4, 4)))
  expect_equal(unique(agg@data[[1]]@data$ETEC@af$rampup), 4)
  # averaging a divisor is wrong, so a genuine difference stops instead
  expect_error(ar_agg(mk(c(4, 8))), "rampup")
})

test_that("a storage self-discharge is copied, not averaged", {
  skip_if_no_geoscales()
  mk <- function(e) newStorage(
    "STG", commodity = "ELC",
    capacity = data.frame(region = c("R1", "R2"), out.stock = c(300, 100)),
    seff = data.frame(region = c("R1", "R2"), stgeff = e))
  agg <- ar_agg(c(ar_base(), list(mk(c(0.99, 0.99)))))
  expect_equal(unique(agg@data[[1]]@data$STG@seff$stgeff), 0.99)
  # it is raised to the slice duration, so the honest merge is geometric
  expect_error(ar_agg(c(ar_base(), list(mk(c(0.99, 0.90))))), "stgeff")
})

# --- constraint terms -------------------------------------------------------
# `@lhs` is a list, so no slot loop reaches it: before the `constraint` method
# a region-keyed coefficient survived aggregation naming regions that had been
# merged away, and matched nothing at interpolation.

ar_cns <- function(coef) {
  newConstraint(
    "CAP", eq = "<=",
    for.each = data.frame(region = c("R1", "R2")),
    rhs = data.frame(region = c("R1", "R2"), rhs = c(40, 60)), defVal = Inf,
    term1 = list(variable = "vTechCap",
                 for.sum = list(tech = "ETEC", region = c("R1", "R2")),
                 mult = data.frame(region = c("R1", "R2"), value = coef)))
}

test_that("a term's coefficients and regions move with the constraint", {
  skip_if_no_geoscales()
  agg <- ar_agg(c(ar_base(), list(ar_cns(c(2, 2)))))
  o <- agg@data[[1]]@data$CAP
  expect_equal(o@rhs$rhs, 100)                      # the budget adds
  expect_equal(o@lhs[[1]]@mult$region, "G1")        # the coefficient moves
  expect_equal(o@lhs[[1]]@mult$value, 2)            # and is kept, not averaged
  expect_equal(o@lhs[[1]]@for.sum$region, "G1")     # so does what it sums over
})

test_that("coefficients that differ across merged regions are refused", {
  skip_if_no_geoscales()
  expect_error(ar_agg(c(ar_base(), list(ar_cns(c(1, 2))))), "coefficient differs")
})

test_that("resolve_conflicts averages a differing coefficient", {
  skip_if_no_geoscales()
  mod <- setGeoscale(ar_model(c(ar_base(), list(ar_cns(c(1, 2))))), ar_geoscale())
  expect_warning(
    agg <- aggregate_model_regions(mod, level = "grp", verbose = FALSE,
                                   resolve_conflicts = "force"),
    "coefficient differs")
  expect_equal(agg@data[[1]]@data$CAP@lhs[[1]]@mult$value, 1.5)
})

# --- the relabel route ------------------------------------------------------
# `as = "clusters"` recomputes nothing: each former region survives as a
# resource tier of one object at the coarser frame. For a weather profile,
# which has no size of its own to weight a mean by, this is the honest route.

test_that("relabelling keeps every region as a cluster, inventing nothing", {
  skip_if_no_geoscales()
  wth <- newWeather(
    "WSOL", timeframe = "SEASON", region = c("R1", "R2"),
    weather = data.frame(region = rep(c("R1", "R2"), each = 2),
                         timeslice = c("WIN", "SUM"),
                         wval = rep(c(0.2, 0.8), each = 2)))
  out <- energyRt:::recast_object(wth, scale = ar_geoscale(), target = "grp",
                                  as = "clusters")
  w <- out@weather
  expect_equal(sort(unique(w$region)), "G1")
  expect_equal(sort(unique(w$cluster)), c("R1", "R2"))
  # both profiles survive unchanged -- no mean, no weight, no invention
  expect_equal(sort(unique(w$wval)), c(0.2, 0.8))
  expect_equal(sort(out@cluster$cluster), c("R1", "R2"))
  expect_equal(out@region, "G1")
})

test_that("relabelling refuses to compose with clusters already declared", {
  skip_if_no_geoscales()
  wth <- newWeather(
    "WSOL", timeframe = "SEASON",
    cluster = data.frame(cluster = c("HI", "LO")),
    weather = data.frame(region = rep(c("R1", "R2"), each = 2),
                         cluster = "HI", timeslice = c("WIN", "SUM"),
                         wval = 0.5))
  expect_error(
    energyRt:::recast_object(wth, scale = ar_geoscale(), target = "grp",
                             as = "clusters"),
    "already declares clusters")
})

test_that("the routes that are not built say so plainly", {
  skip_if_no_geoscales()
  wth <- newWeather("W", timeframe = "SEASON",
                    weather = data.frame(region = "R1", timeslice = "WIN",
                                         wval = 1))
  expect_error(
    energyRt:::recast_object(wth, scale = ar_geoscale(), target = "grp",
                             as = "objects"),
    "not built yet")
})
