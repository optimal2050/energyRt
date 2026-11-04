# =========================================================================== #
# Subsetting a MULTI-LEVEL model: what happens to a declaration made above the
# region level.
#
# A commodity with `@geoframe = "zone"` is balanced per zone, so its supply and
# its trade routes are declared on zone codes. Against a subset each such code
# is covered (every region under it survives), split (some do) or outside
# (none does). The ground truths:
#   * covered -> the object is kept as it stands, and the model still solves,
#   * split   -> the call stops, naming the group and the ways on,
#   * a split group no object is declared at stays silent, so the flat case
#     (every object at region level) is unaffected.
# =========================================================================== #

skip_if_not_installed("geoscales")

# two zones: Z1 = {R1, R2}, Z2 = {R3}
gs2 <- function() {
  geoscales::geoscale_from_leaftable(
    data.frame(zone = c("Z1", "Z1", "Z2"),
               region = c("R1", "R2", "R3"),
               km2 = c(1, 1, 1)),
    geoframes = c("zone", "region"), name = "gs2")
}

# COA is balanced per ZONE and supplied on the zone code; ELC is per region.
mk_zoned <- function(regions = c("R1", "R2", "R3")) {
  repo <- newRepository(
    "zoned",
    newCommodity("COA", timeframe = "ANNUAL", geoframe = "zone"),
    newCommodity("ELC", timeframe = "ANNUAL"),
    # the zone-level declaration: scope AND data on zone codes, as TOPIA's
    # SUP_COA is declared at WEST
    newSupply("SUP_COA", commodity = "COA", region = c("Z1", "Z2"),
              supply = data.frame(region = c("Z1", "Z2"), cost = 5)),
    newTechnology("ECOA", input = list(comm = "COA"),
                  output = list(comm = "ELC"),
                  cap2act = 1, invcost = list(invcost = 100),
                  olife = list(olife = 30)),
    # a region-level object outside the subset, so something is dropped and the
    # summary message fires
    newSupply("SUP_R3", commodity = "ELC", region = "R3",
              supply = data.frame(region = "R3", cost = 99)),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(region = regions, year = 2025L, demand = 10))
  )
  mod <- newModel("ZONED", data = repo, calendar = calendars$annual,
                  region = regions, horizon = newHorizon(2025L, name = "Y2025"),
                  discount = 0.05)
  setGeoscale(mod, gs2())
}

# every object at region level: the flat case
mk_flat <- function(regions = c("R1", "R2", "R3")) {
  repo <- newRepository(
    "flat",
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "ANNUAL"),
    newSupply("SUP_COA", commodity = "COA",
              supply = data.frame(region = regions, cost = 5)),
    newTechnology("ECOA", input = list(comm = "COA"),
                  output = list(comm = "ELC"),
                  cap2act = 1, invcost = list(invcost = 100),
                  olife = list(olife = 30)),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(region = regions, year = 2025L, demand = 10))
  )
  mod <- newModel("FLAT", data = repo, calendar = calendars$annual,
                  region = regions, horizon = newHorizon(2025L, name = "Y2025"),
                  discount = 0.05)
  setGeoscale(mod, gs2())
}

test_that("a fully covered group is reported covered, a halved one split", {
  cl <- energyRt:::.region_group_closure(mk_zoned(), c("R1", "R2"))
  expect_identical(cl$covered, "Z1")
  expect_false("Z1" %in% cl$split)

  cl1 <- energyRt:::.region_group_closure(mk_zoned(), "R1")
  expect_identical(cl1$split, "Z1")
  expect_length(cl1$covered, 0L)

  # a group with nothing kept is neither: it is simply dropped
  cl3 <- energyRt:::.region_group_closure(mk_zoned(), "R3")
  expect_identical(cl3$covered, "Z2")
  expect_false("Z1" %in% c(cl3$covered, cl3$split))
})

test_that("a covered group keeps its zone-level supply, and the model solves", {
  sub <- suppressMessages(subset_model_regions(mk_zoned(), c("R1", "R2")))
  expect_setequal(get_region(sub), c("R1", "R2"))
  sup <- getObject(sub, name = "SUP_COA", drop = TRUE)
  expect_false(is.null(sup))
  expect_identical(unique(as.character(sup@supply$region)), "Z1")

  skip_on_cran()
  scen <- suppressMessages(interpolate_model(sub, name = "ZCOV"))
  td <- file.path(tempdir(), "zoned_cov")
  unlink(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  solved <- suppressMessages(suppressWarnings(
    solve_scenario(scen, solver = solver_options$glpk, tmp.dir = td,
                   tmp.del = TRUE, force = TRUE)))
  expect_true(isTRUE(solved@status$solved))
})

test_that("the kept message counts regions, not the coarse codes", {
  expect_message(subset_model_regions(mk_zoned(), c("R1", "R2")),
                 "kept 2 region\\(s\\) \\(whole: Z1\\)")
})

test_that("splitting a declared group stops and names the ways on", {
  expect_error(subset_model_regions(mk_zoned(), "R1"), "cuts region group")
  err <- tryCatch(subset_model_regions(mk_zoned(), "R1"),
                  error = conditionMessage)
  expect_match(err, "Z1 \\(add R2 to keep it whole\\)")
  expect_match(err, "SUP_COA")
  expect_match(err, "aggregate_model_regions")
  expect_match(err, "disaggregate")
})

test_that("a coarse COST on a split group warns rather than stopping", {
  # the distinction the refusal rests on: a cost charged once at the cell makes
  # the subset too expensive, not infeasible, so it stays the comparability
  # warning it was. See test-trade-cost-regions.R for the warning's own tests.
  repo <- newRepository(
    "cost",
    newCommodity("ELC", timeframe = "ANNUAL"),
    newSupply("SUP_ELC", commodity = "ELC",
              supply = data.frame(region = c("R1", "R2", "R3"), cost = 5)),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(region = c("R1", "R2", "R3"), year = 2025L,
                                  demand = 10)),
    newTrade("TRD", commodity = "ELC",
             routes = data.frame(src = "R1", dst = "R2"),
             trade = data.frame(src = "R1", dst = "R2", teff = 1),
             invcost = data.frame(region = "Z1", invcost = 50))
  )
  mod <- newModel("COST", data = repo, calendar = calendars$annual,
                  region = c("R1", "R2", "R3"),
                  horizon = newHorizon(2025L, name = "Y2025"))
  mod <- setGeoscale(mod, gs2())
  expect_warning(suppressMessages(subset_model_regions(mod, c("R1", "R3"))),
                 "coarse level")
})

test_that("a split group no object is declared at stays silent", {
  # R1 alone halves Z1 here too, but every object sits at region level
  sub <- suppressMessages(subset_model_regions(mk_flat(), "R1"))
  expect_identical(get_region(sub), "R1")
  sup <- getObject(sub, name = "SUP_COA", drop = TRUE)
  expect_identical(unique(as.character(sup@supply$region)), "R1")
})
