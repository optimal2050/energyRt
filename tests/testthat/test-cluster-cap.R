# =========================================================================== #
# `max_clusters`: a ceiling per coarse region.
#
# `clusters = list(F = k)` is a model-wide count whose floor is the number of
# coarse regions; `max_clusters` says "at most n in every coarse region" and
# picks the number below it per region. A region with nothing to separate --
# one member, or no candidate parameter that varies -- comes out at 1, which
# satisfies a ceiling and is therefore reported rather than refused.
# =========================================================================== #

skip_if_no_clustering <- function() {
  testthat::skip_if_not_installed("geoscales")
  testthat::skip_if_not_installed("nestedscales")
  testthat::skip_if_not_installed("sf")
}

# Two zones, two provinces each: Z4 = R07 + R08, Z5 = R11 + R12.
mc_regions <- c("R07", "R08", "R11", "R12")

mc_geoscale <- function() {
  geoscales::filter_geoscale(topia$geoscales$honeycomb, "province", mc_regions)
}

mc_model <- function(vary = TRUE) {
  eff <- if (vary) c(0.42, 0.33, 0.41, 0.34) else rep(0.40, 4L)
  inv <- if (vary) c(1000, 1500, 1020, 1480) else rep(1200, 4L)
  objs <- list(
    newCommodity("COA", unit = "PJ"),
    newCommodity("ELC", unit = "PJ"),
    newSupply("SUP_COA", commodity = "COA", unit = "PJ",
              supply = data.frame(region = mc_regions, cost = 2)),
    newDemand("DEM_ELC", commodity = "ELC",
              demand = data.frame(region = mc_regions, demand = 10))
  )
  techs <- lapply(seq_along(mc_regions), function(i) {
    r <- mc_regions[i]
    newTechnology(paste0("ECOA_", r),
      input = list(comm = "COA", unit = "PJ"),
      output = list(comm = "ELC", unit = "PJ"),
      ceff = data.frame(comm = "COA", cinp2use = eff[i]), region = r,
      invcost = data.frame(region = r, invcost = inv[i]),
      fixom = data.frame(region = r, fixom = 0.03 * inv[i]),
      olife = data.frame(olife = 30), cap2act = 1,
      capacity = data.frame(region = r, cap.up = 100))
  })
  mod <- newModel("MC", region = mc_regions, calendar = calendars$annual,
                  horizon = newHorizon(2025), discount = 0.05,
                  data = newRepository("mc", c(objs, techs)))
  setGeoscale(mod, mc_geoscale())
}

test_that("a cap is read as a scalar, a blanket or per family", {
  groups <- c("ECOA", "EWIN")
  expect_equal(.cl_cap_spec(3, groups), list(ECOA = 3L, EWIN = 3L))
  expect_equal(.cl_cap_spec(list(all = 2), groups),
               list(ECOA = 2L, EWIN = 2L))
  # a family key wins over the blanket
  expect_equal(.cl_cap_spec(list(all = 2, EWIN = 4), groups),
               list(ECOA = 2L, EWIN = 4L))
  expect_equal(.cl_cap_spec(NULL, groups), list())
  expect_error(.cl_cap_spec(list(3), groups), "must be named")
  expect_error(.cl_cap_spec(0, groups), "integer >= 1")
  expect_error(.cl_cap_spec(list(NOPE = 2), groups), "not a process group")
})

test_that("the cap is applied inside each coarse region", {
  skip_if_no_clustering()
  a <- aggregate_model_regions(mc_model(), level = "zone", max_clusters = 2,
                               verbose = FALSE)
  expect_setequal(get_region(a), c("Z4", "Z5"))
  cl <- model_clusters(a)
  expect_named(cl, "ECOA")
  # two provinces per zone, cap 2: each zone keeps both apart
  expect_equal(cl$ECOA$k, 4L)
  per <- cl$ECOA$per_region
  expect_setequal(per$region, c("Z4", "Z5"))
  expect_true(all(per$k == 2L))
  expect_equal(cl$ECOA$max_clusters, 2L)
  expect_setequal(cl$ECOA$crosswalk$target, c("Z4", "Z5"))
})

test_that("a cap of one is plain aggregation, with the labels untouched", {
  skip_if_no_clustering()
  a <- aggregate_model_regions(mc_model(), level = "zone", max_clusters = 1,
                               verbose = FALSE)
  p <- aggregate_model_regions(mc_model(), level = "zone", verbose = FALSE)
  expect_length(model_clusters(a), 0L)
  expect_setequal(names(getObject(a, class = "technology")),
                  names(getObject(p, class = "technology")))
})

test_that("a family with nothing to separate is not clustered", {
  skip_if_no_clustering()
  # every efficiency and cost identical: the distance is degenerate, and a
  # ceiling of 3 is satisfied by 1
  a <- aggregate_model_regions(mc_model(vary = FALSE), level = "zone",
                               max_clusters = 3, verbose = FALSE)
  expect_length(model_clusters(a), 0L)
  expect_setequal(get_region(a), c("Z4", "Z5"))
  # named explicitly, it is still not an error -- a cap admits 1
  b <- aggregate_model_regions(mc_model(vary = FALSE), level = "zone",
                               max_clusters = list(ECOA = 3), verbose = FALSE)
  expect_length(model_clusters(b), 0L)
})

test_that("an exact k and a ceiling are not both given for one family", {
  skip_if_no_clustering()
  expect_error(
    aggregate_model_regions(mc_model(), level = "zone",
                            clusters = list(ECOA = 4),
                            max_clusters = list(ECOA = 2), verbose = FALSE),
    "both name")
})

test_that("a capped model interpolates and keeps its demand", {
  skip_if_no_clustering()
  a <- aggregate_model_regions(mc_model(), level = "zone", max_clusters = 2,
                               verbose = FALSE)
  sc <- suppressWarnings(interpolate_model(a, name = "MC_CAP"))
  d <- getData(sc, "pDemand", merge = TRUE)
  expect_setequal(unique(as.character(d$region)), c("Z4", "Z5"))
  expect_equal(sum(d$value, na.rm = TRUE), 40)
})
