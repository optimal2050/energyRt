# =========================================================================== #
# Weather on a model whose milestones carry calendars of different fidelity.
#
# The combination had no test anywhere: `test-year-calendars.R` never mentions
# weather, and `test-fold-year.R` -- built entirely on the `tm_weather` fixture
# -- never declares a per-year calendar. Two things were suspected of it, and
# measuring says only one is real:
#
#   * `.weather_timeframe_check()` reads year-blind maps, so it was expected to
#     raise a false "no value at the timeslices of the bound". It does not, and
#     cannot on this shape: dropping the year WIDENS the covered set, so the
#     check errs toward permissive -- a missed error, never a false one.
#   * `mWeatherTimeslice` is built from the model calendar's leaves and carries
#     no year, so it names slices a sampled year does not have. That is real,
#     and deliberate: the map is year-less by construction and the equation
#     domains gate per year. Pinned here so a change surfaces as this test
#     rather than as a wrong number somewhere downstream.
# =========================================================================== #

wyc_models <- function() {
  mod <- .mapping_fixture_env()$tm_weather()
  cal <- getCalendar(mod)
  yrs <- mod@config@horizon@intervals$mid
  last <- yrs[length(yrs)]
  samp <- sample_calendar(cal, c("WIN", "SUM"), name = "wyc_half")
  list(full = mod, per_year = setCalendar(mod, samp, years = last),
       last = last, cal = cal, samp = samp)
}

wyc_interp <- function(mod, nm) suppressMessages(suppressWarnings(
  interpolate_model(mod, name = nm, overwrite = TRUE)))

wyc_rows <- function(scen, map) {
  d <- tryCatch(energyRt:::.gds(scen, map), error = function(e) NULL)
  if (is.null(d)) 0L else nrow(d)
}

# @covers pWeather mWeatherTimeslice depth=I backends=glpk
test_that("weather interpolates on a per-year calendar, and the series is trimmed", {
  skip_if_no_fixtures()
  m <- wyc_models()
  full <- wyc_interp(m$full, "wyc_full")
  per <- wyc_interp(m$per_year, "wyc_per")      # must not raise
  expect_s4_class(per, "scenario")

  pw <- as.data.frame(energyRt:::.gds(per, "pWeather"))
  by_year <- tapply(as.character(pw$timeslice), pw$year,
                    function(x) sort(unique(x)))
  # the sampled milestone keeps only its own slices; the full one keeps all
  expect_setequal(by_year[[as.character(m$last)]], c("SUM", "WIN"))
  other <- setdiff(names(by_year), as.character(m$last))
  expect_setequal(by_year[[other[1]]],
                  sort(as.character(m$cal@timetable$timeslice)))
  expect_lt(wyc_rows(per, "pWeather"), wyc_rows(full, "pWeather"))
})

# @covers mWeatherTimeslice depth=I backends=glpk
test_that("mWeatherTimeslice is year-less, so it spans the model calendar", {
  skip_if_no_fixtures()
  m <- wyc_models()
  per <- wyc_interp(m$per_year, "wyc_per2")
  d <- as.data.frame(energyRt:::.gds(per, "mWeatherTimeslice"))
  expect_setequal(sort(unique(as.character(d$timeslice))),
                  sort(as.character(m$cal@timetable$timeslice)))
  expect_false("year" %in% colnames(d))
  # oversized for the sampled year by exactly the slices it dropped
  expect_gt(length(unique(d$timeslice)), nrow(m$samp@timetable))
})
