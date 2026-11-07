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
