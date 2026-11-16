# =========================================================================== #
# Aggregating a model whose declarations are not all at the atoms.
#
# A commodity balanced at a group, a supply declared on a group code and a
# corridor between two group codes are not atoms, so an atoms-only recast
# drops them. Every value declared above the model's own regions has to be
# carried when it is already at the target, rolled up when it is finer, and
# kept when it is coarser.
# =========================================================================== #

skip_if_no_geoscales <- function() {
  testthat::skip_if_not_installed("geoscales")
}

ml_geoscale <- function() {
  geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT",
               grp    = c("G1", "G1", "G2", "G2"),
               region = c("R1", "R2", "R3", "R4"),
               km2    = c(1, 1, 1, 1)),
    geoframes = c("nation", "grp", "region"), key = "region", name = "ml")
}

# `COA` balances at the group; its supply and its corridor are declared on
# group codes. `ELC` and everything on it stays at the atoms.
ml_model <- function() {
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = "ml_cal")
  objs <- list(
    newCommodity("COA", timeframe = "ANNUAL", geoframe = "grp"),
    newCommodity("ELC", timeframe = "SEASON"),
    newSupply("SUP_COA", commodity = "COA", region = "G2",
              supply = data.frame(region = "G2", ava.up = 300, cost = 1)),
    newTechnology(
      "ECOA", input = list(comm = "COA"), output = list(comm = "ELC"),
      ceff = data.frame(region = c("R1", "R2", "R3", "R4"), comm = "COA",
                        cinp2use = c(0.6, 0.2, 0.5, 0.5)),
      capacity = data.frame(region = c("R1", "R2", "R3", "R4"),
                            stock = c(300, 100, 200, 200)),
      vintage = data.frame(olife = 30L), cap2act = 1),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(
                region = rep(c("R1", "R2", "R3", "R4"), each = 2),
                timeslice = c("WIN", "SUM"), demand = 10)),
    newTrade("TRD_COA_G1__G2", commodity = "COA",
             routes = data.frame(src = "G1", dst = "G2"),
             trade = data.frame(src = "G1", dst = "G2", ava.up = 40,
                                teff = 0.95)),
    newTrade("TRD_ELC_R2__R3", commodity = "ELC",
             routes = data.frame(src = "R2", dst = "R3"),
             trade = data.frame(src = "R2", dst = "R3", ava.up = 70,
                                teff = 0.8))
  )
  m <- newModel(name = "ml", desc = "", calendar = cal,
                region = c("R1", "R2", "R3", "R4"),
                horizon = newHorizon(2020:2030, intervals = 10),
                discount = 0.05,
                repo = do.call(newRepository, c(list("ml_repo"), objs)))
  # the group codes are legal declaration sites only through the hierarchy, so
  # the source model carries it too -- as a multi-level model must
  setGeoscale(m, ml_geoscale())
}

test_that("codes are mapped from the geoframe they sit at", {
  skip_if_no_geoscales()
  gs <- ml_geoscale()
  expect_equal(.agg_map_codes(c("R1", "R3"), gs, "grp"), c("G1", "G2"))
  expect_equal(.agg_map_codes(c("G1", "G2"), gs, "nation"), c("NAT", "NAT"))
  # already at the target, and no way down from above it
  expect_equal(.agg_map_codes("G1", gs, "grp"), "G1")
  expect_true(is.na(.agg_map_codes("NAT", gs, "grp")))
  expect_true(is.na(.agg_map_codes("ZZ", gs, "grp")))
  expect_equal(.agg_frames_of(c("R1", "G1", "NAT", "ZZ"), gs),
               c("region", "grp", "nation", NA))
})

test_that("a declaration already at the target geoframe is carried", {
  skip_if_no_geoscales()
  a <- aggregate_model_regions(ml_model(), ml_geoscale(), level = "grp",
                               verbose = FALSE)
  expect_setequal(get_region(a), c("G1", "G2"))
  sup <- getObject(a, class = "supply")[["SUP_COA"]]@supply
  expect_equal(as.character(sup$region), "G2")
  expect_equal(sup$ava.up, 300)
  expect_equal(sup$cost, 1)
  # the regions ARE the groups now, so the declaration says nothing and goes
  expect_length(getObject(a, class = "commodity")[["COA"]]@geoframe, 0L)
})

test_that("a coarse declaration rolls up to the coarsest level", {
  skip_if_no_geoscales()
  a <- aggregate_model_regions(ml_model(), ml_geoscale(), level = "nation",
                               verbose = FALSE)
  expect_setequal(get_region(a), "NAT")
  sup <- getObject(a, class = "supply")[["SUP_COA"]]@supply
  # the only group that declares it: the national total is its value, not NA
  expect_equal(as.character(sup$region), "NAT")
  expect_equal(sup$ava.up, 300)
  # extensive conservation across both steps
  dem <- function(m) sum(getObject(m, class = "demand")[["DEM"]]@demand$demand)
  az <- aggregate_model_regions(ml_model(), ml_geoscale(), level = "grp",
                                verbose = FALSE)
  expect_equal(dem(ml_model()), 80)
  expect_equal(dem(az), 80)
  expect_equal(dem(a), 80)
})

test_that("corridors at a group geoframe follow the same rule", {
  skip_if_no_geoscales()
  trd <- function(m) names(getObject(m, class = "trade"))
  az <- aggregate_model_regions(ml_model(), ml_geoscale(), level = "grp",
                                verbose = FALSE)
  # the coal corridor is already at the groups and survives; the atom-level
  # electricity corridor merges onto the same pair
  tz <- trd(az)
  expect_length(tz, 2L)
  an <- aggregate_model_regions(ml_model(), ml_geoscale(), level = "nation",
                                verbose = FALSE)
  # both become internal to the single region
  expect_length(trd(an), 0L)
})

test_that("a `copy` column is judged inside one identifier cell", {
  skip_if_no_geoscales()
  gs <- ml_geoscale()
  # two clusters of one region, each with its own order: that is not a
  # conflict, and reading it as one blocked every clustered object
  df <- data.frame(region = c("R1", "R1", "R2", "R2"),
                   cluster = c("A", "B", "A", "B"),
                   order = c(1, 2, 1, 2))
  expect_null(.agg_conflicts(df, gs, "grp", "order"))
  # the same cluster disagreeing between the regions of one group IS one
  df2 <- data.frame(region = c("R1", "R2"), cluster = c("A", "A"),
                    order = c(1, 2))
  cf <- .agg_conflicts(df2, gs, "grp", "order")
  expect_s3_class(cf, "data.frame")
  expect_equal(cf$target, "G1")
})

test_that("the aggregated multi-level model interpolates", {
  skip_if_no_geoscales()
  a <- aggregate_model_regions(ml_model(), ml_geoscale(), level = "grp",
                               verbose = FALSE)
  sc <- suppressWarnings(interpolate_model(a, name = "ML_GRP"))
  d <- getData(sc, "pDemand", merge = TRUE)
  expect_setequal(unique(as.character(d$region)), c("G1", "G2"))
  # the total is a calendar-weighted rate, so it is pinned against the model
  # it was aggregated from rather than against a literal
  s0 <- suppressWarnings(interpolate_model(ml_model(), name = "ML_FINE"))
  d0 <- getData(s0, "pDemand", merge = TRUE)
  expect_equal(sum(d$value, na.rm = TRUE), sum(d0$value, na.rm = TRUE))
})
