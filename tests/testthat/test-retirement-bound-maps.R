# Retirement bounds are kept only where a retirement variable exists: the model
# enables retirement and the object's own `optimizeRetirement` is TRUE.

.rb_model <- function(opt_model = TRUE) {
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SL = sprintf("s%02d", 1:2))), name = "rbcal")
  cap <- data.frame(region = "R1", year = c(2025L, 2030L), stock = c(10, 10),
                    ret.up = c(0, 0))
  tech <- function(name, opt) {
    newTechnology(name, input = list(comm = "GAS"), output = list(comm = "ELC"),
                  ceff = data.frame(comm = "GAS", cinp2use = 1),
                  capacity = cap, invcost = list(invcost = 1000),
                  olife = list(olife = 30), cap2act = 1,
                  optimizeRetirement = opt)
  }
  newModel("rb_mod", region = "R1", discount = 0.05, calendar = cal,
           horizon = newHorizon(intervals = data.frame(start = c(2025, 2026),
                                                       mid = c(2025, 2030),
                                                       end = c(2025, 2035))),
           optimizeRetirement = opt_model,
           repo = newRepository("rbrepo",
             newCommodity("ELC", timeframe = "SL"),
             newCommodity("GAS", timeframe = "ANNUAL"),
             newSupply("SUP", commodity = "GAS",
                       supply = data.frame(region = "R1", cost = 15)),
             tech("RET", TRUE),
             tech("KEEP", FALSE),
             newDemand("DEM", commodity = "ELC",
                       demand = data.frame(region = "R1", demand = 15))))
}

.rb_map <- function(scen, name) {
  p <- scen@modInp@parameters[[name]]
  d <- if (is.null(p)) NULL else energyRt:::get_data_slot(p)
  if (is.null(d)) data.frame(tech = character()) else as.data.frame(d)
}

test_that("an object that cannot retire gets no retirement-bound rows", {
  scen <- suppressMessages(interpolate_model(.rb_model(TRUE), name = "rb"))
  up <- .rb_map(scen, "mTechRetUp")
  stock <- .rb_map(scen, "mvTechRetiredStock")
  expect_setequal(unique(stock$tech), "RET")
  expect_true(nrow(up) > 0)
  expect_setequal(unique(up$tech), "RET")
})

test_that("retirement disabled model-wide leaves no retirement-bound rows", {
  scen <- suppressMessages(interpolate_model(.rb_model(FALSE), name = "rb0"))
  expect_equal(nrow(.rb_map(scen, "mTechRetUp")), 0L)
  expect_equal(nrow(.rb_map(scen, "mvTechRetiredStock")), 0L)
})
