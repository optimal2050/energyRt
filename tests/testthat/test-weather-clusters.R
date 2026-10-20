# =========================================================================== #
# `weather@cluster` -- resource tiers in one object.
#
# A weather object declaring clusters is a BUNDLE of series, one per tier. At
# interpolation each tier becomes an ordinary weather object (`WWIN` tier `T3`
# -> `WWIN_CLT3`) and every link that named the bundle is repointed, so
# `pWeather`, `mWeatherRegion(At)` and the four templates keep seeing plain
# single-series objects. The bundle itself is DROPPED: its rows would collapse
# into duplicate (weather, region, year, timeslice) keys, which the interpolator
# already refuses ("interpolation multiplied a series").
#
# A consumer finds its tier by CLUSTER NAME, the same rule `.resolve_cluster_ref()`
# will serve for clustered commodities. The consumer is authoritative: it decides
# which tiers exist, and asking for one the weather does not declare is an error.
#
# @covers pWeather mWeatherRegionAt depth=I backends=glpk
# =========================================================================== #

wc_cal <- function() {
  newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))), name = "wccal")
}

# one tier's rows
wc_rows <- function(cluster, win, sum, region = "R1") {
  data.frame(cluster = cluster, region = region, year = 2025L,
             timeslice = rep(c("WIN", "SUM"), length(region)),
             wval = c(win, sum))
}

wc_weather <- function(regions = "R1") {
  newWeather(
    name = "WWIN", timeframe = "SEASON",
    cluster = data.frame(cluster = c("T1", "T2"), desc = c("good", "poor"),
                         region = NA_character_, order = 1:2),
    weather = rbind(
      do.call(rbind, lapply(regions, function(r) wc_rows("T1", 0.9, 0.6, r))),
      do.call(rbind, lapply(regions, function(r) wc_rows("T2", 0.45, 0.3, r)))))
}

# ONE tier list drives every slot. Clusters are HARVESTED from the data slots
# when `@cluster` is empty, so a helper that hardcoded T1/T2 in `capacity`
# would quietly make even a "no clusters" fixture clustered.
wc_tech <- function(clusters = c("T1", "T2"), regions = NA_character_,
                    weather_name = "WWIN", transform = NULL) {
  if (is.null(clusters)) {
    return(newTechnology(
      "EWIN", output = list(comm = "ELC"),
      capacity = data.frame(cap.up = 10),
      invcost = data.frame(invcost = 1),
      weather = data.frame(weather = weather_name, waf.up = 1),
      vintage = data.frame(olife = 30L), cap2act = 1))
  }
  wl <- data.frame(cluster = clusters, weather = weather_name, waf.up = 1)
  if (!is.null(transform)) wl$transform <- transform
  newTechnology(
    "EWIN", output = list(comm = "ELC"),
    cluster = data.frame(cluster = clusters, region = regions),
    capacity = data.frame(cluster = clusters, cap.up = 10),
    invcost = data.frame(cluster = clusters, invcost = 1),
    weather = wl,
    vintage = data.frame(olife = 30L), cap2act = 1)
}

# the encoding available today: one weather OBJECT per tier, named on the link
wc_tech_named <- function(names = c("WWIN_T1", "WWIN_T2"),
                          clusters = c("T1", "T2")) {
  newTechnology(
    "EWIN", output = list(comm = "ELC"),
    cluster = data.frame(cluster = clusters, region = NA_character_),
    capacity = data.frame(cluster = clusters, cap.up = 10),
    invcost = data.frame(cluster = clusters, invcost = 1),
    weather = data.frame(cluster = clusters, weather = names, waf.up = 1),
    vintage = data.frame(olife = 30L), cap2act = 1)
}

wc_model <- function(..., regions = "R1", name = "wc") {
  newModel(name, calendar = wc_cal(), region = regions,
           horizon = newHorizon(2025), discount = 0,
           repo = newRepository("r",
             newCommodity("ELC", timeframe = "SEASON"),
             newDemand("DE", commodity = "ELC",
                       demand = data.frame(timeslice = c("WIN", "SUM"),
                                           demand = 3)),
             newImport("IMP", commodity = "ELC",
                       import = data.frame(price = 100)),
             ...))
}

wc_interp <- function(mod, name = "wc_i") {
  suppressMessages(suppressWarnings(
    interpolate_model(mod, name = name, ondisk = FALSE)))
}

# --------------------------------------------------------------------------- #

test_that("a clustered weather object round-trips through the constructor", {
  w <- wc_weather()
  expect_s4_class(w, "weather")
  expect_equal(w@cluster$cluster, c("T1", "T2"))
  expect_true("cluster" %in% colnames(w@weather))
  expect_equal(sort(unique(w@weather$cluster)), c("T1", "T2"))
  # a logical `region` column is refused, as on every other class
  expect_error(newWeather(name = "W", cluster = data.frame(cluster = "T1",
                                                           region = NA)),
               "expecting character")
})

test_that("each referenced tier becomes its own weather object", {
  scen <- wc_interp(wc_model(wc_weather(), wc_tech()))
  expect_setequal(as.character(scen@modInp@sets$weather),
                  c("WWIN_CLT1", "WWIN_CLT2"))
  # the bundle is gone -- it is not a series
  expect_false("WWIN" %in% as.character(scen@modInp@sets$weather))

  pw <- as.data.frame(get_data_slot(scen@modInp@parameters$pWeather))
  t1 <- pw[pw$weather == "WWIN_CLT1", ]
  t2 <- pw[pw$weather == "WWIN_CLT2", ]
  expect_equal(sort(t1$value), c(0.6, 0.9))
  expect_equal(sort(t2$value), c(0.3, 0.45))
  # ... and each process variant reads its own tier
  link <- as.data.frame(get_data_slot(scen@modInp@parameters$pTechWeatherAf))
  expect_true(all(c("EWIN_CLT1", "EWIN_CLT2") %in% link$tech))
  expect_equal(link$weather[link$tech == "EWIN_CLT1"][1], "WWIN_CLT1")
  expect_equal(link$weather[link$tech == "EWIN_CLT2"][1], "WWIN_CLT2")
})

test_that("clustered weather gives the same LP as hand-named objects", {
  skip_if_no_solver()
  solve1 <- function(mod, nm) {
    s <- suppressMessages(suppressWarnings(solve_model(
      mod, name = nm, ondisk = FALSE, overwrite = TRUE, echo = FALSE)))
    getData(s, "vObjective", merge = TRUE)$value[1]
  }
  clustered <- wc_model(wc_weather(), wc_tech(), name = "wc_cl")
  named <- wc_model(
    newWeather(name = "WWIN_T1", timeframe = "SEASON",
               weather = wc_rows(NA_character_, 0.9, 0.6)[, -1]),
    newWeather(name = "WWIN_T2", timeframe = "SEASON",
               weather = wc_rows(NA_character_, 0.45, 0.3)[, -1]),
    wc_tech_named(), name = "wc_nm")
  expect_equal(solve1(clustered, "wc_eq_cl"), solve1(named, "wc_eq_nm"),
               tolerance = 1e-9)
})

test_that("tier counts may differ by region", {
  # T1 everywhere, T2 only in R1: three (process, region) activity cells, not four
  tech <- wc_tech(clusters = c("T1", "T2"), regions = c(NA, "R1"))
  scen <- wc_interp(wc_model(wc_weather(regions = c("R1", "R2")), tech,
                             regions = c("R1", "R2"), name = "wc_rr"))
  act <- as.data.frame(get_data_slot(scen@modInp@parameters$mvTechAct))
  cells <- unique(act[, c("tech", "region")])
  expect_equal(nrow(cells), 3L)
  expect_setequal(cells$region[cells$tech == "EWIN_CLT1"], c("R1", "R2"))
  expect_equal(cells$region[cells$tech == "EWIN_CLT2"], "R1")
})

test_that("a link must name a cluster of a clustered weather", {
  # no cluster at all: a multiplicative factor has no "every tier" reading
  bare <- wc_tech(clusters = NULL)
  expect_error(wc_interp(wc_model(wc_weather(), bare, name = "wc_na")),
               "without a cluster")

  # a cluster the weather does not declare
  expect_error(
    wc_interp(wc_model(
      wc_weather(),
      wc_tech(clusters = c("T1", "T9")),
      name = "wc_t9")),
    "does not declare")
})

test_that("a declared tier with no rows is refused", {
  w <- newWeather(
    name = "WWIN", timeframe = "SEASON",
    cluster = data.frame(cluster = c("T1", "EMPTY"), region = NA_character_,
                         order = 1:2),
    weather = wc_rows("T1", 0.9, 0.6))
  expect_error(
    wc_interp(wc_model(w, wc_tech(clusters = "T1"), name = "wc_empty")),
    "carries no rows")
})

test_that("every declared tier is materialized, referenced or not", {
  # T2 is declared and carries rows but no process asks for it: its data must
  # survive as its own object rather than vanishing with the bundle
  tech <- wc_tech(clusters = "T1")
  scen <- wc_interp(wc_model(wc_weather(), tech, name = "wc_unref"))
  expect_true("WWIN_CLT2" %in% as.character(scen@modInp@sets$weather))
})

test_that("a cluster composes with a transform", {
  w <- wc_weather()
  register_weather_transform("wc_half", function(weather, ...) weather * 0.5)
  tech <- wc_tech(transform = c("wc_half", NA))
  scen <- wc_interp(wc_model(w, tech, name = "wc_tr"), "wc_tr_i")
  # the cluster resolves FIRST, so the transform derives from T1's series and
  # the name carries both steps
  expect_setequal(as.character(scen@modInp@sets$weather),
                  c("WWIN_CLT1", "WWIN_CLT1_wc_half", "WWIN_CLT2"))
  pw <- as.data.frame(get_data_slot(scen@modInp@parameters$pWeather))
  der <- pw[pw$weather == "WWIN_CLT1_wc_half", ]
  expect_equal(sort(der$value), sort(c(0.9, 0.6) / 2))   # half of T1, not T2
  # and the T1 variant reads the composed series, T2 its own untransformed one
  lk <- unique(as.data.frame(
    get_data_slot(scen@modInp@parameters$pTechWeatherAf))[, c("tech", "weather")])
  expect_equal(lk$weather[lk$tech == "EWIN_CLT1"], "WWIN_CLT1_wc_half")
  expect_equal(lk$weather[lk$tech == "EWIN_CLT2"], "WWIN_CLT2")
})

test_that("unclustered weather is untouched (the EWIN_SITES encoding)", {
  w1 <- newWeather(name = "WWIN_GOOD", timeframe = "SEASON",
                   weather = wc_rows(NA_character_, 0.9, 0.6)[, -1])
  w2 <- newWeather(name = "WWIN_POOR", timeframe = "SEASON",
                   weather = wc_rows(NA_character_, 0.45, 0.3)[, -1])
  tech <- wc_tech_named(c("WWIN_GOOD", "WWIN_POOR"))
  scen <- wc_interp(wc_model(w1, w2, tech, name = "wc_plain"))
  # names survive verbatim: no cluster suffix is invented for an unclustered
  # weather, which is what keeps the existing kits working
  expect_setequal(as.character(scen@modInp@sets$weather),
                  c("WWIN_GOOD", "WWIN_POOR"))
})
