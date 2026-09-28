# =========================================================================== #
# Corridor grouping: `aggregate_model_regions(clusters = list(<trade> = k))`.
#
# Plain aggregation merges every corridor between a pair of coarse regions
# into one. That is right when they are alike and wrong when they are not: the
# merged corridor carries the capacity-weighted `teff`, which is the CHORD of
# a delivery curve the fine model traverses by filling its best corridor
# first. The fixture below makes that measurable -- a 100-unit link at 0.99
# beside a 20-unit link at 0.90 -- so the tests assert energy, not shapes.
# =========================================================================== #

skip_if_no_grouping <- function() {
  testthat::skip_if_not_installed("geoscales")
  testthat::skip_if_not_installed("sf")
}

tg_regions <- c("W1", "W2", "C1", "C2")

tg_geoscale <- function() {
  geoscales::filter_geoscale(topia$geoscales$honeycomb, "region", tg_regions)
}

tg_corridor <- function(nm, s, d, cap, teff, react = NA_real_) {
  rt <- data.frame(src = c(s, d), dst = c(d, s))
  trd <- rt
  trd$ava.up <- cap
  trd$teff <- teff
  if (!is.na(react)) trd$reactance <- react
  newTrade(name = nm, commodity = "ELC", routes = rt, trade = trd,
           capacity = data.frame(cap.fx = cap), cap2act = 1,
           vintage = data.frame(olife = 50L))
}

# Both corridors serve the SAME demand node, so the fine model genuinely
# chooses between them; demand sits in CENTRAL and the cheap supply in WEST,
# so everything must cross.
tg_model <- function(dem = 50, react = NA_real_) {
  cal <- topia$modules$calendars$topia_seasons
  tt <- as.data.frame(cal@timetable)
  sh <- tt$share / sum(tt$share)
  newModel(
    "TG", region = tg_regions, calendar = cal, horizon = newHorizon(2025),
    discount = 0, data = newRepository("tg", list(
      newCommodity("ELC", unit = "PJ", timeframe = "HOUR"),
      newSupply("SUP", commodity = "ELC", unit = "PJ",
                supply = data.frame(region = c("W1", "W2"), cost = 1)),
      newDemand("DEM", commodity = "ELC",
                demand = data.frame(region = "C1", timeslice = tt$timeslice,
                                    demand = dem * sh)),
      tg_corridor("TRD_ELC_W1__C1", "W1", "C1", 100, 0.99, react),
      tg_corridor("TRD_ELC_W2__C1", "W2", "C1", 20, 0.90, react)))) |>
    setGeoscale(tg_geoscale())
}

tg_objective <- function(mod, nm) {
  s <- suppressMessages(interpolate_model(mod, name = nm, overwrite = TRUE))
  s <- solve_scenario(s, solver = solver_options$glpk, wait = TRUE,
                      echo = FALSE)
  sum(getData(s, name = "vObjective", merge = TRUE)$value)
}

tg_trades <- function(m) {
  tr <- Filter(function(o) methods::is(o, "trade"), m@data[[1]]@data)
  vapply(tr, function(o) o@name, character(1))
}

# Covers the R API: get_process_groups()
test_that("corridors of one prefix and commodity form ONE family", {
  skip_if_no_grouping()
  g <- get_process_groups(tg_model())
  tg <- g[g$class == "trade", , drop = FALSE]

  expect_equal(nrow(tg), 1L)
  # Named for the trade prefix, not the common stem of the object names --
  # `TRD_ELC_W1__C1` and `TRD_ELC_W2__C1` share the stem `TRD_ELC_W`, which
  # names nothing.
  expect_equal(tg$group, "TRD_ELC")
  # `n` counts CORRIDORS here, not regions: the unit of a corridor family is
  # the whole trade object
  expect_equal(tg$n, 2L)
  expect_equal(tg$members, "TRD_ELC_W1__C1,TRD_ELC_W2__C1")

  # Every corridor spans two regions by construction, so the rule that gives a
  # multi-region process a group of its own must NOT apply here, or no family
  # could ever form.
  expect_false(grepl("@", tg$group, fixed = TRUE))
})

# Covers the R API: aggregate_model_regions(clusters=)
# @covers vObjective depth=S backends=glpk
test_that("k at its floor is plain aggregation, exactly", {
  skip_if_no_grouping()
  skip_if_no_solver()
  plain <- suppressMessages(aggregate_model_regions(tg_model(), level = "zone",
                                                    verbose = FALSE))
  k1 <- suppressMessages(aggregate_model_regions(
    tg_model(), level = "zone", clusters = list(TRD_ELC = 1), verbose = FALSE))

  expect_length(tg_trades(plain), 1L)
  expect_length(tg_trades(k1), 1L)
  expect_equal(unname(tg_trades(k1)), unname(tg_trades(plain)))
  expect_equal(tg_objective(k1, "TGK1"), tg_objective(plain, "TGPLAIN"),
               tolerance = 1e-9)
})

# @covers vObjective depth=S backends=glpk
test_that("keeping unlike corridors apart recovers what merging loses", {
  skip_if_no_grouping()
  skip_if_no_solver()
  fine <- tg_objective(tg_model(), "TGFINE")

  merged <- tg_objective(
    suppressMessages(aggregate_model_regions(
      tg_model(), level = "zone", clusters = list(TRD_ELC = 1),
      verbose = FALSE)), "TGM")
  split <- tg_objective(
    suppressMessages(aggregate_model_regions(
      tg_model(), level = "zone", clusters = list(TRD_ELC = 2),
      verbose = FALSE)), "TGS")

  # Merging replaces the fill-the-best-first delivery curve with its chord, so
  # below saturation the coarse model loses more energy in transit. Assert the
  # direction AND the size: 0.99 / 0.975 - 1.
  expect_gt(merged, fine)
  expect_equal(100 * (merged - fine) / fine, 100 * (0.99 / 0.975 - 1),
               tolerance = 1e-6)
  # Kept apart, the LP can fill the good corridor first again
  expect_equal(split, fine, tolerance = 1e-9)
})

test_that("a part is named for its place in the merit order", {
  skip_if_no_grouping()
  m <- suppressMessages(aggregate_model_regions(
    tg_model(), level = "zone", clusters = list(TRD_ELC = 2), verbose = FALSE))
  nms <- unname(tg_trades(m))
  expect_length(nms, 2L)
  expect_true(all(grepl("_G[12]$", nms)))
  expect_equal(anyDuplicated(nms), 0L)

  # `_G1` is the LOWEST-loss part, so a solved result reads without a
  # crosswalk lookup
  g1 <- Filter(function(o) grepl("_G1$", o@name),
               m@data[[1]]@data)[[1]]
  g2 <- Filter(function(o) grepl("_G2$", o@name),
               m@data[[1]]@data)[[1]]
  expect_gt(max(g1@trade$teff), max(g2@trade$teff))
})

# Covers the R API: model_clusters()
test_that("the partition is reported, without a geoscale", {
  skip_if_no_grouping()
  m <- suppressMessages(aggregate_model_regions(
    tg_model(), level = "zone", clusters = list(TRD_ELC = 2), verbose = FALSE))
  cl <- model_clusters(m)$TRD_ELC

  expect_equal(cl$k, 2L)
  expect_setequal(names(cl$crosswalk), c("trade", "part", "bucket"))
  expect_setequal(cl$crosswalk$trade,
                  c("TRD_ELC_W1__C1", "TRD_ELC_W2__C1"))
  # every part sits in exactly one coarse pair -- the bucket IS the constraint
  expect_equal(length(unique(cl$crosswalk$bucket)), 1L)
  expect_equal(length(unique(cl$crosswalk$part)), 2L)
  # a corridor has no territory to colour, unlike a region cluster
  expect_null(cl$geoscale)
})

test_that("k outside its range names both ends and the caveat", {
  skip_if_no_grouping()
  expect_error(
    suppressMessages(aggregate_model_regions(
      tg_model(), level = "zone", clusters = list(TRD_ELC = 3))),
    "out of range")
  # the floor and ceiling are stated, so the caller can choose
  expect_error(
    suppressMessages(aggregate_model_regions(
      tg_model(), level = "zone", clusters = list(TRD_ELC = 3))),
    "plain aggregation")
  # and the trap is named: even k = kmax is NOT the fine model, because
  # corridors internal to a coarse region are still dropped
  expect_error(
    suppressMessages(aggregate_model_regions(
      tg_model(), level = "zone", clusters = list(TRD_ELC = 3))),
    "internal to a coarse region are still dropped")
})

test_that("parallel AC circuits are refused above the floor", {
  skip_if_no_grouping()
  # Two circuits on one corridor do not split flow by optimisation, they split
  # it by impedance -- and .kvl_lines() refuses a line whose reactance sits on
  # more than one trade object. Keeping them apart would hand the LP a
  # controllability the network does not have.
  expect_error(
    suppressMessages(aggregate_model_regions(
      tg_model(react = 0.1), level = "zone", clusters = list(TRD_ELC = 2))),
    "impedance")
  expect_error(
    suppressMessages(aggregate_model_regions(
      tg_model(react = 0.1), level = "zone", clusters = list(TRD_ELC = 2))),
    "1/x_eq")
  # merging them is still fine: that is what the equivalent reactance is for
  expect_no_error(
    suppressMessages(aggregate_model_regions(
      tg_model(react = 0.1), level = "zone", clusters = list(TRD_ELC = 1),
      verbose = FALSE)))
})

# Covers the R API: process_cluster_sweep()
test_that("a corridor family has no region sweep, and says so", {
  skip_if_no_grouping()
  expect_error(process_cluster_sweep(tg_model(), "TRD_ELC", level = "zone"),
               "corridor family")
})
