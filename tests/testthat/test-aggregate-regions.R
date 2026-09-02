# =========================================================================== #
# Region aggregation: `aggregate_model_regions()`.
#
# Four atoms in two groups. Extensive quantities must add up, intensive ones
# must take the capacity-weighted mean and so stay inside the range of the
# values they came from, and a corridor whose endpoints land in one group must
# disappear rather than become a region trading with itself.
# =========================================================================== #

skip_if_no_geoscales <- function() {
  testthat::skip_if_not_installed("geoscales")
}

ag_geoscale <- function() {
  geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT",
               grp    = c("G1", "G1", "G2", "G2"),
               region = c("R1", "R2", "R3", "R4"),
               km2    = c(1, 1, 1, 1)),
    geoframes = c("nation", "grp", "region"), key = "region", name = "ag")
}

# Capacity is deliberately lopsided so a weighted mean differs from a plain
# one: R1 carries 300 MW at 0.6 efficiency, R2 carries 100 MW at 0.2.
ag_model <- function(eff = c(0.6, 0.2, 0.5, 0.5)) {
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = "ag_cal")
  objs <- list(
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "SEASON"),
    newSupply("SUP_COA", commodity = "COA",
              supply = data.frame(region = c("R1", "R2", "R3", "R4"),
                                  ava.up = c(100, 200, 300, 400),
                                  cost = 1)),
    newTechnology(
      "ECOA", input = list(comm = "COA"), output = list(comm = "ELC"),
      ceff = data.frame(region = c("R1", "R2", "R3", "R4"), comm = "COA",
                        cinp2use = eff),
      capacity = data.frame(region = c("R1", "R2", "R3", "R4"),
                            stock = c(300, 100, 200, 200)),
      vintage = data.frame(olife = 30L), cap2act = 1),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(
                region = rep(c("R1", "R2", "R3", "R4"), each = 2),
                timeslice = c("WIN", "SUM"), demand = 10)),
    # R1-R2 is internal to G1 and must vanish; R2-R3 crosses and must survive
    newTrade("TRD_ELC_R1__R2", commodity = "ELC",
             routes = data.frame(src = "R1", dst = "R2"),
             trade = data.frame(src = "R1", dst = "R2", ava.up = 50,
                                teff = 0.9)),
    newTrade("TRD_ELC_R2__R3", commodity = "ELC",
             routes = data.frame(src = "R2", dst = "R3"),
             trade = data.frame(src = "R2", dst = "R3", ava.up = 70,
                                teff = 0.8))
  )
  newModel(name = "ag", desc = "", calendar = cal,
           region = c("R1", "R2", "R3", "R4"),
           horizon = newHorizon(2020:2030, intervals = 10), discount = 0.05,
           repo = do.call(newRepository, c(list("ag_repo"), objs)))
}

test_that("regions collapse to the target geoframe", {
  skip_if_no_geoscales()
  a <- aggregate_model_regions(ag_model(), ag_geoscale(), level = "grp",
                               verbose = FALSE)
  expect_setequal(get_region(a), c("G1", "G2"))
  expect_equal(a@name, "ag_grp")
})

test_that("extensive quantities add up", {
  skip_if_no_geoscales()
  a <- aggregate_model_regions(ag_model(), ag_geoscale(), level = "grp",
                               verbose = FALSE)
  cap <- getObject(a, class = "technology")[["ECOA"]]@capacity
  expect_equal(cap$stock[cap$region == "G1"], 400)
  expect_equal(cap$stock[cap$region == "G2"], 400)

  sup <- getObject(a, class = "supply")[["SUP_COA"]]@supply
  expect_equal(sup$ava.up[sup$region == "G1"], 300)
  expect_equal(sup$ava.up[sup$region == "G2"], 700)

  dem <- getObject(a, class = "demand")[["DEM"]]@demand
  expect_equal(sum(dem$demand), 80)
})

test_that("intensive quantities take the capacity-weighted mean", {
  skip_if_no_geoscales()
  a <- aggregate_model_regions(ag_model(), ag_geoscale(), level = "grp",
                               verbose = FALSE)
  ce <- getObject(a, class = "technology")[["ECOA"]]@ceff
  # 300 MW at 0.6 and 100 MW at 0.2 give 0.5, not the unweighted 0.4
  expect_equal(ce$cinp2use[ce$region == "G1"], 0.5, tolerance = 1e-9)
  expect_equal(ce$cinp2use[ce$region == "G2"], 0.5, tolerance = 1e-9)
  # and it stays within the range of the values it came from
  expect_true(all(ce$cinp2use >= 0.2 & ce$cinp2use <= 0.6))
})

test_that("a corridor inside one region disappears, a crossing one survives", {
  skip_if_no_geoscales()
  a <- aggregate_model_regions(ag_model(), ag_geoscale(), level = "grp",
                               verbose = FALSE)
  tr <- getObject(a, class = "trade")
  expect_length(tr, 1L)
  rt <- tr[[1]]@routes
  expect_setequal(unique(c(rt$src, rt$dst)), c("G1", "G2"))
  expect_false(any(rt$src == rt$dst))
  expect_equal(sum(tr[[1]]@trade$ava.up, na.rm = TRUE), 70)
})

ag_objective <- function(mod, nm) {
  s <- interpolate_model(mod, name = nm)
  s <- write_script(s, solver = solver_options$glpk)
  s <- read_solution(solve_scenario(s, wait = TRUE))
  getData(s, "vObjective", merge = TRUE)$value
}

test_that("with uniform efficiency the coarse model costs no more", {
  skip_if_no_geoscales()
  skip_on_cran()
  # Aggregation does two opposing things: it removes transport constraints,
  # which can only help, and it averages intensive values, which loses the
  # ability to dispatch the best unit. With one efficiency everywhere the
  # second effect is absent and only the relaxation remains.
  m <- ag_model(eff = rep(0.5, 4))
  a <- aggregate_model_regions(m, ag_geoscale(), level = "grp",
                               verbose = FALSE)
  o_f <- ag_objective(m, "ag_uni_fine")
  o_c <- ag_objective(a, "ag_uni_coarse")
  expect_lte(o_c, o_f * (1 + 1e-9))
})

test_that("averaging heterogeneous efficiency can cost more, not less", {
  skip_if_no_geoscales()
  skip_on_cran()
  # The fine model can send R1's 0.6 plant through the corridor to serve R2;
  # the aggregate holds one 0.5 plant and cannot. Coarser is not a bound on
  # finer in general -- only the direction of the transport relaxation is.
  m <- ag_model(eff = c(0.6, 0.2, 0.5, 0.5))
  a <- aggregate_model_regions(m, ag_geoscale(), level = "grp",
                               verbose = FALSE)
  o_f <- ag_objective(m, "ag_het_fine")
  o_c <- ag_objective(a, "ag_het_coarse")
  expect_true(is.finite(o_f) && is.finite(o_c))
  expect_gt(o_c, o_f)
})

test_that("regions the geoscale does not know are refused, not dropped", {
  skip_if_no_geoscales()
  gs <- geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT", grp = c("G1", "G1"), region = c("R1", "R2")),
    geoframes = c("nation", "grp", "region"), key = "region", name = "part")
  expect_error(
    aggregate_model_regions(ag_model(), gs, level = "grp", verbose = FALSE),
    "not atoms of the geoscale")
})

test_that("the finest geoframe and an unknown one are refused", {
  skip_if_no_geoscales()
  expect_error(
    aggregate_model_regions(ag_model(), ag_geoscale(), level = "region"),
    "nothing to aggregate")
  expect_error(
    aggregate_model_regions(ag_model(), ag_geoscale(), level = "county"),
    "not a geoframe")
})

test_that("a model with no geoscale says so", {
  skip_if_no_geoscales()
  expect_error(aggregate_model_regions(ag_model(), level = "grp"),
               "no geoscale")
})

test_that("the model's own geoscale is used when none is passed", {
  skip_if_no_geoscales()
  m <- setGeoscale(ag_model(), ag_geoscale())
  a <- aggregate_model_regions(m, level = "grp", verbose = FALSE)
  expect_setequal(get_region(a), c("G1", "G2"))
})

# @covers pTechCap pSupAva pDemand pTradeAva depth=S backends=glpk forks=geoframe
