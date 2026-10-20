# process_operability(): declared processes that cannot operate, where and
# when -- read off an interpolated scenario, without solving -- and the
# interpolation stops for undeclared names and inputs with no source.

op_cal <- function() newCalendar(
  timetable = make_timetable(struct = list(ANNUAL = "ANNUAL", SL = c("s1", "s2"))),
  name = "op_cal")

op_model <- function(..., years = 2025) newModel(
  "op", region = c("R1", "R2"), horizon = newHorizon(years), discount = 0,
  calendar = op_cal(),
  repo = newRepository("op_repo",
    newCommodity("ELC", timeframe = "SL"),
    newCommodity("GAS", timeframe = "SL"),
    newSupply("SUP", commodity = "GAS", supply = data.frame(cost = 1)),
    newImport("BACKUP", commodity = "ELC", import = data.frame(price = 100)),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(region = "R1", timeslice = c("s1", "s2"),
                                  demand = 1)),
    ...))

op_interp <- function(m, nm, ...) suppressWarnings(suppressMessages(
  interpolate_model(m, name = nm, ondisk = FALSE, ...)))

op_tech <- function(nm, ..., olife = data.frame(olife = 30)) newTechnology(
  nm, input = list(comm = "GAS"), output = list(comm = "ELC"), cap2act = 1,
  olife = olife, ...)

test_that("undeclared commodities, timeslices and timeframes stop interpolation", {
  expect_error(op_interp(op_model(newTechnology(
    "T", input = list(comm = "HYD"), output = list(comm = "ELC"), cap2act = 1)),
    "op_u1"), "undeclared name.*commodity: HYD \\(in T\\)")
  expect_error(op_interp(op_model(newDemand(
    "D2", commodity = "HEAT", demand = data.frame(demand = 1))), "op_u2"),
    "commodity: HEAT")
  expect_error(op_interp(op_model(newSupply(
    "S9", commodity = "GAS", supply = data.frame(timeslice = "s9", cost = 2))),
    "op_u3"), "timeslice: s9")
  expect_error(op_interp(op_model(newCommodity("OIL", timeframe = "HOUR"),
                                  newSupply("SO", commodity = "OIL")),
                         "op_u4"), "timeframe: HOUR")
})

test_that("an unused weather is a warning", {
  w <- newWeather("W_UNUSED", timeframe = "SL",
                  weather = data.frame(timeslice = c("s1", "s2"), wval = 1))
  expect_warning(suppressMessages(interpolate_model(op_model(w), name = "op_w",
                                                    ondisk = FALSE)),
                 "unused weather object: W_UNUSED")
})

test_that("existing capacity with no input source stops; new-only is dropped", {
  hyd <- newCommodity("HYD", timeframe = "SL")
  stock <- newTechnology("TH", input = list(comm = "HYD"),
                         output = list(comm = "ELC"), cap2act = 1,
                         capacity = data.frame(stock = 1))
  expect_error(op_interp(op_model(hyd, stock), "op_g1"),
               "existing capacity but no source")
  # the caller may ask for the drop instead
  s <- op_interp(op_model(hyd, stock), "op_g2", on_missing_input = "warn")
  expect_true("TH" %in% s@misc$region_gaps$process)
  # a new-only process is dropped, and its variable-cost rows with it
  newonly <- newTechnology("TN", input = list(comm = "HYD"),
                           output = list(comm = "ELC"), cap2act = 1,
                           invcost = data.frame(invcost = 1),
                           olife = data.frame(olife = 10),
                           varom = data.frame(varom = 1))
  s <- op_interp(op_model(hyd, newonly), "op_g3")
  vr <- ff_param(s, "mTechVarom")
  expect_false("TN" %in% vr$tech)
  op <- process_operability(s)
  r <- op[op$process == "TN", ]
  expect_equal(nrow(r), 2L)                       # R1 and R2
  expect_equal(unique(r$reason), "input has no source in the region")
  expect_equal(unique(r$kind), "structural")
  expect_match(r$detail[1], "missing: HYD")
})

test_that("no capacity in a year: stock retired and no investment window", {
  t <- op_tech("TOLD", capacity = data.frame(region = "R1", year = 2025,
                                             stock = 1, ncap.up = 0),
               olife = data.frame(olife = 5))
  s <- op_interp(op_model(t, years = c(2025, 2035)), "op_y")
  op <- process_operability(s)
  r <- op[op$process == "TOLD" & op$reason == "no capacity in the year", ]
  # the declared stock is the 2025 value; nothing can be built
  expect_equal(nrow(r), 1L)
  expect_equal(r$region, "R1")
  expect_equal(r$years, paste(2026:2035, collapse = ", "))
})

test_that("bounds fixed at zero are reported, by year", {
  s <- op_interp(op_model(
    op_tech("TCAP", capacity = data.frame(stock = 1, cap.up = 0)),
    op_tech("TAF", capacity = data.frame(stock = 1),
            af = data.frame(region = "R1", af.up = 0)),
    newSupply("SOFF", commodity = "GAS",
              supply = data.frame(region = "R2", ava.up = 0, cost = 1))),
    "op_b")
  op <- process_operability(s)
  expect_true(all(op$kind[op$process %in% c("TCAP", "TAF", "SOFF")] == "bound"))
  expect_setequal(op$region[op$process == "TCAP"], c("R1", "R2"))
  expect_equal(op$region[op$process == "TAF"], "R1")
  expect_match(op$reason[op$process == "TAF"], "af.up")
  expect_equal(op$region[op$process == "SOFF"], "R2")
})

test_that("a one-term user constraint at zero is reported", {
  k <- newConstraint(
    name = "NOBUILD", eq = "<=",
    for.each = data.frame(region = "R2", year = 2025L),
    term1 = list(variable = "vTechCap", for.sum = list(tech = "TK")),
    defVal = 0)
  s <- op_interp(op_model(op_tech("TK", invcost = data.frame(invcost = 1)), k),
                 "op_k")
  op <- process_operability(s)
  r <- op[op$process == "TK", ]
  expect_equal(nrow(r), 1L)
  expect_equal(r$region, "R2")
  expect_match(r$detail, "NOBUILD: vTechCap <= 0")
})

test_that("a model where everything can operate returns zero rows", {
  s <- op_interp(op_model(op_tech("TOK", invcost = data.frame(invcost = 1))),
                 "op_ok")
  op <- process_operability(s)
  expect_equal(nrow(op), 0L)
  expect_named(op, c("process", "class", "region", "years", "kind", "reason",
                     "detail"))
})

test_that("on a sampled calendar, slices outside the sample are a warning", {
  tt <- make_timetable(struct = list(ANNUAL = "ANNUAL", SL = paste0("s", 1:4)))
  tt1 <- tt[tt$SL %in% c("s1", "s2"), ]
  cal <- newCalendar(timetable = tt1, name = "op_s1",
                     year_fraction = sum(tt1$share))
  m <- newModel("ops", region = "R1", horizon = newHorizon(2025), discount = 0,
                calendar = cal,
                repo = newRepository("ops_repo",
                  newCommodity("ELC", timeframe = "SL"),
                  newImport("IMP", commodity = "ELC",
                            import = data.frame(price = 1)),
                  newDemand("D", commodity = "ELC",
                            demand = data.frame(timeslice = paste0("s", 1:4),
                                                demand = 1))))
  expect_warning(suppressMessages(interpolate_model(m, name = "op_samp",
                                                    ondisk = FALSE)),
                 "outside the sampled calendar")
})
