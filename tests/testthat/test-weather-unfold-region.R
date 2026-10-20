# =========================================================================== #
# A weather series whose `@weather` data leaves `region` NA -- the obvious way
# to write a region-agnostic profile, and the one path no fixture covered.
#
# `ob2mi(weather)` copied the data's `region` column verbatim, so the NA
# survived interpolation while the two weather maps read the `@region` slot.
# `unfold_parameters()` then resolved the wildcard through `.region_allowed()`,
# which has no `weather` key, and the full-set fallback cross-joined it against
# the whole `region` set -- every COARSE level included. A series declared at
# one adm1 was stored at all six regions of a two-level model while
# `mWeatherRegionAt` correctly pointed every reader at the one.
#
# No answer moved (all four backends index `pWeather` only through the
# redirect), which is why this went unseen; the parameter simply claimed the
# series where nothing declared it.
#
# @covers pWeather mWeatherRegion mWeatherRegionAt depth=S backends=glpk
# =========================================================================== #

wu_gs <- function() {
  geoscales::geoscale_from_leaftable(
    data.frame(adm1 = c("A1", "A1", "A2", "A2"),
               region = c("r1", "r7", "r3", "r9")),
    geoframes = c("adm1", "region"), key = "region", name = "wu")
}

# `wreg` is the `@region` slot; `in_data` says whether the SAME regions are
# also written into the data rows. `in_data = FALSE` is the path under test.
wu_model <- function(wreg, nm, in_data = FALSE, tech_regions = c("r1", "r7")) {
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = paste0("wu_", nm))
  wdat <- if (in_data) {
    data.frame(region = rep(wreg, each = 2),
               timeslice = c("WIN", "SUM"), wval = c(1, 0.5))
  } else {
    data.frame(timeslice = c("WIN", "SUM"), wval = c(1, 0.5))
  }
  objs <- list(
    newCommodity("ELC", timeframe = "SEASON"),
    newCommodity("COA", timeframe = "ANNUAL"),
    newSupply("S", commodity = "COA",
              supply = data.frame(region = tech_regions, cost = 0)),
    newWeather("W_ON", region = wreg, timeframe = "SEASON", weather = wdat),
    newTechnology("WIND", input = list(comm = "COA"),
                  output = list(comm = "ELC"), region = tech_regions,
                  weather = data.frame(weather = "W_ON", waf.up = 1),
                  invcost = data.frame(invcost = 10),
                  vintage = data.frame(olife = 30L), cap2act = 1),
    newDemand("D", commodity = "ELC",
              demand = data.frame(region = rep(tech_regions, each = 2),
                                  timeslice = c("WIN", "SUM"), demand = 5)))
  setGeoscale(newModel(
    name = nm, calendar = cal, region = c("r1", "r7", "r3", "r9"),
    horizon = newHorizon(2025), discount = 0,
    repo = do.call(newRepository, c(list(paste0("wur_", nm)), objs))), wu_gs())
}

wu_interp <- function(mod, nm) {
  suppressMessages(suppressWarnings(
    interpolate_model(mod, name = nm, overwrite = TRUE)))
}

wu_gd <- function(scen, nm) {
  p <- scen@modInp@parameters[[nm]]
  if (is.null(p)) return(NULL)
  d <- as.data.frame(get_data_slot(p))
  if (is.null(d) || nrow(d) == 0) NULL else d
}

test_that("a region-less series is stored at the object's scope, not at every level", {
  skip_if_not_installed("geoscales")
  scen <- wu_interp(wu_model("A1", "wu_na"), "wu_na")

  pw <- wu_gd(scen, "pWeather")
  expect_equal(unique(as.character(pw$region)), "A1")
  expect_equal(nrow(pw), 2L)
  expect_false(anyNA(pw$region))

  # the redirect is untouched: every reader still resolves to the parent
  at <- wu_gd(scen, "mWeatherRegionAt")
  expect_setequal(paste(at$weather, at$region, at$regionp),
                  c("W_ON A1 A1", "W_ON r1 A1", "W_ON r7 A1"))
  # and the parameter covers every region the redirect points at -- the
  # invariant that keeps a missing tuple (silently 0, multiplicative) impossible
  expect_true(all(at$regionp %in% pw$region))
})

test_that("writing the regions into the data gives the identical parameter", {
  skip_if_not_installed("geoscales")
  # the two ways of saying the same thing must not differ
  bare <- wu_gd(wu_interp(wu_model("A1", "wu_b"), "wu_b"), "pWeather")
  spelt <- wu_gd(wu_interp(wu_model("A1", "wu_s", in_data = TRUE), "wu_s"),
                 "pWeather")
  key <- c("weather", "region", "year", "timeslice", "value")
  expect_equal(bare[order(bare$timeslice), key],
               spelt[order(spelt$timeslice), key], ignore_attr = TRUE)
})

test_that("a region-less series with no @region still spans every region", {
  skip_if_not_installed("geoscales")
  # unchanged behaviour, pinned deliberately: with nothing declared the scope
  # IS every region of every level, and narrowing that is what declaring a
  # level would do -- not this change
  scen <- wu_interp(wu_model(character(0), "wu_all"), "wu_all")
  pw <- wu_gd(scen, "pWeather")
  expect_setequal(unique(as.character(pw$region)),
                  c("r1", "r7", "r3", "r9", "A1", "A2"))
  expect_false(anyNA(pw$region))
})

test_that("resolving the region moves no answer", {
  skip_if_not_installed("geoscales")
  skip_if_no_solver()
  obj <- function(mod, nm) {
    sol <- suppressMessages(suppressWarnings(solve_scenario(
      wu_interp(mod, nm), solver = solver_options$glpk, wait = TRUE)))
    sum(as.data.frame(get_data_slot(
      sol@modOut@variables[["vObjective"]]))$value)
  }
  bare <- obj(wu_model("A1", "wu_ob"), "wu_ob")
  spelt <- obj(wu_model("A1", "wu_os", in_data = TRUE), "wu_os")
  expect_gt(bare, 0)
  expect_equal(bare, spelt, tolerance = 1e-9)
})
