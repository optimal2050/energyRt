# =========================================================================== #
# Weather units: declared for reporting and a cross-check, never converted.
#
# A weather factor is DIMENSIONLESS by default and that is the common case. A
# series carrying a real measure declares it in `weather@units$wval`; a process
# says what it expects in the `unit` column of its own `@weather` row, per row
# because one process may read a temperature from one series and a wind speed
# from another. Where both are declared and differ, that is an error: nothing
# in the model converts degC to degK, so a coefficient fitted to one measure
# and applied to the other is wrong in a way no output reveals.
#
# Nothing here changes a value, which is the point: the three tests below pin
# the report, the check, and the fact that the coefficient's own unit does NOT
# follow from the series'.
# =========================================================================== #

wu_weather <- function(unit = NA_character_) {
  newWeather(name = "TEMP", unit = unit, timeframe = "SL",
             weather = data.frame(timeslice = c("s1", "s2"), wval = c(-5, 18)))
}

wu_tech <- function(expect = NA_character_, waf = 1.1) {
  newTechnology(
    name = "HP", desc = "heat pump",
    input  = data.frame(comm = "ELC", unit = "GWh"),
    output = data.frame(comm = "HET", unit = "GWh"),
    weather = data.frame(weather = "TEMP", unit = expect, waf.fx = waf))
}

wu_model <- function(unit = NA_character_, expect = NA_character_, waf = 1.1) {
  newModel("wu", data = add(
    newRepository("wu"),
    newCommodity("ELC", desc = "Electricity", unit = "GWh"),
    newCommodity("HET", desc = "Heat", unit = "GWh"),
    wu_weather(unit), wu_tech(expect, waf)))
}


# --------------------------------------------------------------------------- #
# 1. what a declaration changes, and what it does not
# --------------------------------------------------------------------------- #

test_that("a weather series reports its declared unit and nothing else does", {
  # undeclared: the placeholder, not a guess and not an error
  expect_equal(getUnit(wu_weather()), NA_character_)
  u0 <- getUnits(wu_weather())
  expect_equal(u0$unit[u0$parameter == "wval"], "{wval}")

  w <- wu_weather("degC")
  expect_equal(getUnit(w), "degC")
  expect_equal(w@units$wval, "degC")
  u1 <- getUnits(w)
  expect_equal(u1$unit[u1$parameter == "wval"], "degC")
})

test_that("the process's own units do not move when a weather unit is declared", {
  # The user's ruling: declaring a real measure on the series says what the
  # SERIES is. A process's capacity, activity and output units are given, and
  # the coefficient reading the series stays dimensionless -- the relation is
  # reciprocal for a temperature and direct for other measures, so it is not
  # derived.
  bare <- as.data.frame(getUnits(wu_tech()))
  decl <- as.data.frame(getUnits(wu_tech("degC")))
  expect_equal(bare, decl)

  cp <- energyRt:::.col_param
  waf <- cp$unit[cp$class == "technology" & cp$slot == "weather" &
                   cp$column == "waf.fx"]
  expect_equal(waf, "factor")
})

test_that("a weather factor above 1 is accepted, deliberately", {
  # Low temperature raises conductivity and puts output above nameplate, so
  # `factor` is dimensionless but NOT bounded by 1. Nothing in the three
  # classes restricts it, and nothing warns.
  expect_silent(te <- wu_tech("degC", waf = 1.4))
  expect_equal(te@weather$waf.fx, 1.4)
  expect_silent(m <- wu_model("degC", "degC", waf = 1.4))
  expect_equal(getObject(m)[["HP"]]@weather$waf.fx, 1.4)
})


# --------------------------------------------------------------------------- #
# 2. the cross-check: both sides declared, and they disagree
# --------------------------------------------------------------------------- #

test_that("a weather unit mismatch is an error, naming both sides", {
  expect_error(wu_model("degC", "degK"),
               "weather unit mismatch", fixed = FALSE)
  expect_error(wu_model("degC", "degK"), "expects degK", fixed = TRUE)
  expect_error(wu_model("degC", "degK"), "declares degC", fixed = TRUE)
})

test_that("the check is silent unless BOTH sides declare", {
  # opt-in: a model that has never heard of weather units behaves as before
  expect_s4_class(wu_model(NA_character_, "degK"), "model")
  expect_s4_class(wu_model("degC", NA_character_), "model")
  expect_s4_class(wu_model(NA_character_, NA_character_), "model")
  expect_s4_class(wu_model("degC", "degC"), "model")
})

test_that("the mismatch is caught after construction, not only at newModel", {
  # `newModel()` names the call that introduced it; the check also runs at
  # interpolation, which is what catches a mismatch introduced afterwards --
  # here by replacing the series with a declared one the process disagrees
  # with.
  m <- wu_model(NA_character_, "degK")
  m2 <- add(m, wu_weather("degC"), overwrite = TRUE)
  expect_error(energyRt:::.check_weather_units(m2), "weather unit mismatch")
})


# --------------------------------------------------------------------------- #
# 3. the column is per row
# --------------------------------------------------------------------------- #

test_that("one process can expect different units from different series", {
  # @covers pTechWeatherAf pWeather depth=I
  wspd <- newWeather(name = "WSPD", unit = "m/s", timeframe = "SL",
                     weather = data.frame(timeslice = c("s1", "s2"),
                                          wval = c(4, 9)))
  te <- newTechnology(
    name = "HP", desc = "heat pump",
    input  = data.frame(comm = "ELC", unit = "GWh"),
    output = data.frame(comm = "HET", unit = "GWh"),
    weather = data.frame(weather = c("TEMP", "WSPD"),
                         unit = c("degC", "m/s"),
                         waf.fx = c(1.1, 0.9)))
  expect_equal(te@weather$unit, c("degC", "m/s"))

  m <- newModel("wu2", data = add(
    newRepository("wu2"),
    newCommodity("ELC", desc = "Electricity", unit = "GWh"),
    newCommodity("HET", desc = "Heat", unit = "GWh"),
    wu_weather("degC"), wspd, te))
  expect_s4_class(m, "model")

  # one row wrong is enough, and only that row is named
  te@weather$unit[2] <- "km/h"
  m2 <- newModel("wu3", data = add(
    newRepository("wu3"),
    newCommodity("ELC", desc = "Electricity", unit = "GWh"),
    newCommodity("HET", desc = "Heat", unit = "GWh"),
    wu_weather("degC"), wspd, te)) |> expect_error("expects km/h")
})
