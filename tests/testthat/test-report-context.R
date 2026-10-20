# =========================================================================== #
# report(process, geoscale = / model = / scenario = )
#
# A process report documents the DECLARATION. The three context arguments widen
# it: geometry (so it can say WHERE), the model it sits in (so the levelized
# cost uses that model's calendar, horizon and discount), and the solved
# scenario (so the report can show what was actually built and priced).
#
# `scenario` implies its model implies its geoscale, and a model that is not
# the scenario's is refused rather than silently ignored -- a report that
# quietly mixed two models' assumptions would be worse than no report.
# =========================================================================== #

rc_geoscale <- function() {
  geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", region = c("R1", "R2")),
    geoframes = c("nation", "region"), key = "region", name = "rc_gs")
}

rc_tech <- function() newTechnology(
  name = "EWIN",
  output = data.frame(comm = "ELC", unit = "GWh"),
  region = c("R1", "R2"),
  cluster = data.frame(cluster = c("01", "02"), order = 1:2),
  af = data.frame(cluster = c("01", "02"), af.up = c(0.45, 0.30)),
  capacity = data.frame(region = c("R1", "R2"), cap.up = c(20, 8)),
  invcost = data.frame(cluster = c("01", "02"), invcost = c(600, 900)),
  vintage = data.frame(olife = 25L), cap2act = 8.76)

rc_model <- function(EWIN = rc_tech()) newModel(
  name = "rc_mod", region = c("R1", "R2"), discount = 0.05,
  horizon = newHorizon(c(2025, 2030)),
  calendar = newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))), name = "rc_cal"),
  geoscale = rc_geoscale(),
  repo = newRepository("rc_repo",
    newCommodity("ELC", timeframe = "SEASON", unit = "GWh"),
    newImport("IMP_ELC", commodity = "ELC", import = data.frame(price = 500)),
    EWIN,
    newDemand("DEM", commodity = "ELC",
              demand = expand.grid(region = c("R1", "R2"),
                                   timeslice = c("WIN", "SUM"), demand = 8,
                                   stringsAsFactors = FALSE))))

# @covers depth=I backends=none
test_that("the context resolves, and conflicts are refused", {
  EWIN <- rc_tech(); mod <- rc_model(EWIN)
  ctx <- getFromNamespace(".report_context", "energyRt")

  bare <- ctx(EWIN)
  expect_equal(bare$level, "declared")
  expect_null(bare$geoscale)

  with_gs <- ctx(EWIN, geoscale = rc_geoscale())
  expect_equal(with_gs$level, "declared")     # geometry alone prices nothing
  expect_true(is_geoscale(with_gs$geoscale))

  with_mod <- ctx(EWIN, model = mod)
  expect_equal(with_mod$level, "model")
  expect_true(is_geoscale(with_mod$geoscale)) # implied by the model
  expect_identical(with_mod$model@name, "rc_mod")

  expect_error(ctx(EWIN, scenario = "not a scenario"), "must be a scenario")
  expect_error(ctx(EWIN, model = "not a model"), "must be a model")
})

# @covers depth=S backends=glpk
test_that("a solved scenario gives the realized level, map data and comparison", {
  skip_if_no_solver()
  EWIN <- rc_tech(); mod <- rc_model(EWIN)
  scen <- suppressMessages(suppressWarnings(
    solve_model(mod, name = "rc_scen", solver = solver_options$glpk,
                ondisk = FALSE, overwrite = TRUE, echo = FALSE)))
  ctx <- getFromNamespace(".report_context", "energyRt")(EWIN, scenario = scen)
  expect_equal(ctx$level, "realized")
  expect_true(is_geoscale(ctx$geoscale))

  loc <- getFromNamespace(".report_locations", "energyRt")(EWIN, ctx)
  expect_equal(loc$kind, "realized")
  expect_true(all(c("region", "value") %in% names(loc$data)))

  cmp <- getFromNamespace(".report_levcost_compare", "energyRt")(EWIN, ctx)
  expect_true(is.data.frame(cmp))
  expect_true(all(c("variant", "ex_ante") %in% names(cmp)))
  # a variant the solver never built is priced ex ante and has no realized cost
  expect_true("realized" %in% names(cmp))
  expect_true(any(is.na(cmp$realized)) || nrow(cmp) == length(names(cmp)))

  # a model that is not the scenario's is refused by name
  other <- rc_model(); other@name <- "other"
  expect_error(
    getFromNamespace(".report_context", "energyRt")(EWIN, model = other,
                                                    scenario = scen),
    "not the scenario's model")
})

# @covers depth=I backends=none
test_that("the map needs GEOMETRY, not just a geoscale", {
  EWIN <- rc_tech()
  ctx <- getFromNamespace(".report_context", "energyRt")
  plot_of <- getFromNamespace(".report_location_plot", "energyRt")

  # no spatial context at all
  expect_null(plot_of(EWIN, ctx(EWIN)))

  # a leaftable geoscale knows how regions NEST but carries no shapes, so it
  # resolves as context and still yields no map
  gs <- rc_geoscale()
  cx <- ctx(EWIN, geoscale = gs)
  expect_true(is_geoscale(cx$geoscale))
  expect_null(plot_of(EWIN, cx))

  # with shapes there is a map
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  gs_map <- topia$geoscales$continent
  regs <- geoscales::geoscale_regions(gs_map, "region")[1:2]
  EWIN_map <- update(EWIN, region = regs,
                     capacity = data.frame(region = regs, cap.up = c(20, 8)))
  p <- plot_of(EWIN_map, ctx(EWIN_map, geoscale = gs_map))
  expect_s3_class(p, "ggplot")
})
