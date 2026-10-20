# =========================================================================== #
# A weather link must resolve to a series for every region its process runs in.
#
# The equations read a series through `mWeatherRegionAt`:
#
#   prod{wth in ...} ( pTechWeatherAfUp[wth,tech]
#     * sum{rw in region : (wth, r, rw) in mWeatherRegionAt} pWeather[wth,rw,y,s] )
#
# An unresolved (weather, region) pair has no row, so the inner sum is EMPTY --
# 0 in MathProg -- and the factor is multiplicative: the availability bound
# would become `activity <= 0` and the process would be switched off for that
# region, with an OPTIMAL solve and no message.
#
# `.assert_weather_reachable()` (mapping-builders.R) refuses that at
# interpolation: it reads the (process, weather) links off the objects, crosses
# them with `process_region`, and anti-joins `mWeatherRegionAt`. Its sibling
# guard inside `map_mWeatherRegionAt()` covers the opposite case, a pair
# resolving to TWO source regions. These tests pin both halves of the rule
# classes.yml states for `weather@region`, which had no test of its own.
# =========================================================================== #

wrc_geoscale <- function(regions = c("R1", "R2")) {
  geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", region = regions),
    geoframes = c("nation", "region"), key = "region", name = "wrc")
}

wrc_slices <- paste0("s", 1:4)

# ESUN runs in both regions; `weather_region` says where the series is declared
wrc_model <- function(weather_region, name = "wrc") {
  wth <- newWeather("WSUN", timeframe = "SL",
    weather = data.frame(
      region = rep(weather_region, each = length(wrc_slices)),
      timeslice = rep(wrc_slices, times = length(weather_region)),
      wval = 0.5))
  newModel(name, region = c("R1", "R2"), horizon = newHorizon(2025),
    discount = 0,
    calendar = newCalendar(
      timetable = make_timetable(struct = list(ANNUAL = "ANNUAL",
                                               SL = wrc_slices)),
      name = "wrc_cal"),
    repo = newRepository("wrc_repo",
      newCommodity("ELC", timeframe = "SL"),
      newCommodity("SUN", timeframe = "SL"),
      newSupply("RES_SUN", commodity = "SUN",
                supply = data.frame(region = c("R1", "R2"), cost = 0)),
      wth,
      newTechnology("ESUN", input = list(comm = "SUN"),
                    output = list(comm = "ELC"),
                    ceff = data.frame(comm = "SUN", cinp2use = 1),
                    weather = list(weather = "WSUN", comm = "SUN", waf.up = 1),
                    cap2act = 1, invcost = list(invcost = 1), olife = 1L),
      newDemand("DEM", commodity = "ELC",
                demand = expand.grid(region = c("R1", "R2"),
                                     timeslice = wrc_slices, demand = 1,
                                     stringsAsFactors = FALSE))))
}

wrc_interp <- function(mod, nm) suppressMessages(suppressWarnings(
  interpolate_model(setGeoscale(mod, wrc_geoscale()), name = nm,
                    overwrite = TRUE)))

# @covers mWeatherRegionAt pWeather pTechWeatherAf depth=I backends=glpk
test_that("a process region no series covers is refused, not silently zeroed", {
  err <- tryCatch(wrc_interp(wrc_model("R1", name = "wrc_gap"), "wrc_gap"),
                  error = conditionMessage)
  expect_match(err, "Weather link", fixed = TRUE)
  expect_match(err, "cannot be resolved", fixed = TRUE)
  # the message must name the process, its region, the series, and where the
  # series IS declared -- that is what makes it actionable
  expect_match(err, "ESUN in R2 -> weather 'WSUN'")
  expect_match(err, "declared at: R1")
  expect_match(err, "COARSER region")
})

# @covers mWeatherRegionAt depth=I backends=glpk
test_that("every region declared, or a covering ancestor, resolves", {
  both <- wrc_interp(wrc_model(c("R1", "R2"), name = "wrc_both"), "wrc_both")
  at <- as.data.frame(get_data_slot(
    both@modInp@parameters[["mWeatherRegionAt"]]))
  expect_setequal(at$region[at$weather == "WSUN"], c("R1", "R2"))

  # one series at the parent level feeds both children
  up <- wrc_interp(wrc_model("NAT", name = "wrc_up"), "wrc_up")
  at2 <- as.data.frame(get_data_slot(
    up@modInp@parameters[["mWeatherRegionAt"]]))
  expect_true(all(c("R1", "R2") %in% at2$region))
  expect_true(all(at2$regionp[at2$region %in% c("R1", "R2")] == "NAT"))
})
