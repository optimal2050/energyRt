# =========================================================================== #
# levcost() pricing timeframe: when the ANNUAL collapse is sound, when it is
# not, and what happens in each case.
#
# `levcost()` prices on an ANNUAL timeframe by default: weather is collapsed to
# one annual capacity factor and the calendar becomes a single ANNUAL slice.
# Two things make that collapse invalid rather than approximate -- more than one
# weather factor on a process (the annual mean of a product is not the product
# of the annual means), and parameter rows pinned to a named sub-annual
# timeslice (the slice stops existing). `timeframe = "auto"` detects both and
# prices natively instead; an explicit `"ANNUAL"` declines.
#
# Added 2026-10-03 with the collapse fix: the collapse rewrote the weather data
# and the commodity timeframe but left the PROCESS declaring its sub-annual
# timeframe, so `get_process_timeframe()` kept building an hourly timeslice
# domain and the model died on `pTechAfLo[...,HOUR] out of domain`. That hit any
# sub-annual technology on the solve path, with or without variants.
# =========================================================================== #

tf_hours <- function() sprintf("d%03d_h%02d", rep(1:365, each = 24),
                               rep(0:23, times = 365))

tf_weather <- function(name, level) {
  set.seed(1)
  newWeather(name, timeframe = "HOUR", region = "R1",
             weather = data.frame(region = "R1", timeslice = tf_hours(),
                                  wval = pmin(pmax(level + 0.1 *
                                                   stats::runif(8760), 0), 1)))
}

# An hourly wind-like technology: sub-annual @timeframe, weather-driven, no fuel.
tf_tech <- function(weathers = "W_A", subannual = FALSE) {
  a <- list(name = "WND", output = data.frame(comm = "ELC", unit = "MWh"),
            cap2act = 8760, region = "R1", timeframe = "HOUR",
            invcost = list(invcost = 1000, wacc = 0.07),
            fixom = list(fixom = 12))
  a$weather <- data.frame(weather = weathers, waf.fx = 1)
  if (subannual) a$varom <- data.frame(timeslice = "d001_h00", varom = 5)
  do.call(newTechnology, a)
}

tf_run <- function(tech, ...) {
  suppressWarnings(suppressMessages(levcost(tech, comm = "ELC",
                           weather = list(tf_weather("W_A", 0.25),
                                          tf_weather("W_B", 0.35)),
                           region = "R1", horizon = newHorizon(2020:2049),
                           discount = 0.07, cache = FALSE, verbose = FALSE,
                           ...)))
}

# Covers the R API: levcost()
test_that("a sub-annual technology annualizes and prices on both engines", {
  skip_if_no_solver()
  # The regression: the collapse left @timeframe = "HOUR" while installing a
  # single-slice ANNUAL calendar, so the solver got an out-of-domain af index.
  a <- tf_run(tf_tech(), method = "analytic")
  s <- tf_run(tf_tech(), method = "solve")

  expect_true(is.finite(a$levcost_npv))
  expect_true(is.finite(s$levcost_npv))
  expect_equal(unname(a$levcost_npv), unname(s$levcost_npv), tolerance = 5e-5)
})

# Covers the R API: levcost()
test_that("two weather factors decline annualization", {
  # One af row per @weather row, all at timeslice = NA: they collide, and no
  # single annual factor represents the pair once the series covary.
  expect_error(tf_run(tf_tech(c("W_A", "W_B")), timeframe = "ANNUAL"),
               "more than one @weather factor")
})

# Covers the R API: levcost()
test_that("a sub-annual parameter row declines annualization, and is named", {
  expect_error(tf_run(tf_tech(subannual = TRUE), timeframe = "ANNUAL"),
               "sub-annual parameter rows")
  # the reason has to name the slot, or it is not actionable
  expect_error(tf_run(tf_tech(subannual = TRUE), timeframe = "ANNUAL"),
               "@varom")
})

# Covers the R API: levcost()
test_that("auto falls back to the native timeframe instead of annualizing", {
  # and native then insists on a calendar rather than guessing one
  expect_error(tf_run(tf_tech(c("W_A", "W_B"))), "needs a `calendar")
  expect_message(
    try(levcost(tf_tech(c("W_A", "W_B")), comm = "ELC",
                weather = list(tf_weather("W_A", 0.25), tf_weather("W_B", 0.35)),
                region = "R1", horizon = newHorizon(2020:2049), discount = 0.07,
                cache = FALSE, verbose = TRUE), silent = TRUE),
    "native timeframe")
})

# Covers the R API: levcost()
test_that("native never guesses a sub-annual calendar", {
  expect_error(tf_run(tf_tech(), timeframe = "native"), "needs a `calendar")
})

# Covers the R API: levcost()
test_that("an annual technology needs no calendar from the caller", {
  # The guard must fire only for a process whose timeslices an annual calendar
  # cannot hold. Routed through the solver because the analytic engine declines
  # `timeframe = "native"` unconditionally, for reasons of its own.
  skip_if_no_solver()
  flat <- newTechnology(name = "GAS", output = data.frame(comm = "ELC", unit = "PJ"),
                        cap2act = 1, region = "R1",
                        invcost = list(invcost = 1000, wacc = 0.07))
  r <- suppressWarnings(suppressMessages(
    levcost(flat, comm = "ELC", region = "R1", horizon = newHorizon(2020:2049),
            discount = 0.07, method = "solve", timeframe = "native",
            cache = FALSE, verbose = FALSE)))
  expect_s3_class(r, "levcost")
  expect_true(is.finite(r$levcost_npv))
})

test_that(".levcost_annual_blockers() reports each reason and nothing else", {
  f <- getFromNamespace(".levcost_annual_blockers", "energyRt")

  expect_length(f(tf_tech()), 0L)
  expect_match(f(tf_tech(c("W_A", "W_B"))), "@weather factor", all = FALSE)
  expect_match(f(tf_tech(subannual = TRUE)), "sub-annual", all = FALSE)
  # both at once are reported together, not the first only
  expect_length(f(tf_tech(c("W_A", "W_B"), subannual = TRUE)), 2L)
  # an ANNUAL timeslice label is not sub-annual
  ok <- tf_tech()
  ok@varom <- data.frame(timeslice = "ANNUAL", varom = 5)
  expect_length(f(ok), 0L)
})
