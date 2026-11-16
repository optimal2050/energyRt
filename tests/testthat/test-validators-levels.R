# =========================================================================== #
# The two D4 validators: `check_model_levels()` reports the rule interpolation
# raises, and `verify_aggregation()` says what a coarsening conserved.
# =========================================================================== #

skip_if_no_geoscales <- function() {
  testthat::skip_if_not_installed("geoscales")
}

vq_geoscale <- function() {
  geoscales::geoscale_from_leaftable(
    data.frame(nation = "NAT",
               grp    = c("G1", "G1", "G2", "G2"),
               region = c("R1", "R2", "R3", "R4"),
               km2    = c(1, 1, 1, 1)),
    geoframes = c("nation", "grp", "region"), key = "region", name = "vq")
}

# `COA` balances at the group, everything else at the atoms. One corridor is
# internal to G1 and one crosses, so aggregation drops one and keeps one.
vq_model <- function(coa_frame = "grp", elc_timeframe = character()) {
  cal <- newCalendar(timetable = make_timetable(
    struct = list(ANNUAL = "ANNUAL", SEASON = c("WIN", "SUM"))),
    name = "vq_cal")
  objs <- list(
    newCommodity("COA", timeframe = "ANNUAL", geoframe = coa_frame),
    newCommodity("ELC", timeframe = elc_timeframe),
    newSupply("SUP_COA", commodity = "COA", region = "G2",
              supply = data.frame(region = "G2", ava.up = 300, cost = 1)),
    newTechnology(
      "ECOA", input = list(comm = "COA"), output = list(comm = "ELC"),
      ceff = data.frame(region = c("R1", "R2", "R3", "R4"), comm = "COA",
                        cinp2use = c(0.6, 0.2, 0.5, 0.5)),
      capacity = data.frame(region = c("R1", "R2", "R3", "R4"),
                            stock = c(300, 100, 200, 200)),
      invcost = data.frame(region = c("R1", "R2", "R3", "R4"),
                           invcost = c(100, 200, 150, 150)),
      vintage = data.frame(olife = 30L), cap2act = 1),
    newDemand("DEM", commodity = "ELC",
              demand = data.frame(
                region = rep(c("R1", "R2", "R3", "R4"), each = 2),
                timeslice = c("WIN", "SUM"), demand = 10)),
    newTrade("TRD_R1__R2", commodity = "ELC",
             routes = data.frame(src = "R1", dst = "R2"),
             trade = data.frame(src = "R1", dst = "R2", ava.up = 50,
                                teff = 0.9)),
    newTrade("TRD_R2__R3", commodity = "ELC",
             routes = data.frame(src = "R2", dst = "R3"),
             trade = data.frame(src = "R2", dst = "R3", ava.up = 70,
                                teff = 0.8))
  )
  m <- newModel(name = "vq", desc = "", calendar = cal,
                region = c("R1", "R2", "R3", "R4"),
                horizon = newHorizon(2020:2030, intervals = 10),
                discount = 0.05,
                repo = do.call(newRepository, c(list("vq_repo"), objs)))
  setGeoscale(m, vq_geoscale())
}

test_that("a model whose declarations match reports nothing", {
  skip_if_no_geoscales()
  r <- check_model_levels(vq_model())
  expect_s3_class(r, "model_levels_check")
  expect_equal(nrow(r), 0L)
  expect_output(print(r), "every declaration matches")
})

test_that("a frame the model does not have is named", {
  skip_if_no_geoscales()
  # the geoscale has nation/grp/region, not "county"
  r <- check_model_levels(vq_model(coa_frame = "county"))
  expect_true("geoframe" %in% r$dim)
  expect_equal(r$declared[r$dim == "geoframe"], "county")
  expect_true(grepl("grp", r$expected[r$dim == "geoframe"]))
  # and the same on the time axis, which had no pre-flight at all
  r2 <- check_model_levels(vq_model(elc_timeframe = "HOUR"))
  expect_true("timeframe" %in% r2$dim)
  expect_equal(r2$declared[r2$dim == "timeframe"], "HOUR")
})

test_that("a strict class below its commodity is reported, every one of them", {
  skip_if_no_geoscales()
  # move ELC to the group but leave its demand and corridors at the atoms
  bad <- suppressWarnings(update(vq_model(), geoframe = list(ELC = "grp"),
                                 force = TRUE, verbose = FALSE))
  r <- check_model_levels(bad)
  expect_gt(nrow(r), 1L)
  expect_setequal(unique(r$comm), "ELC")
  expect_true(all(r$dim == "region"))
  expect_true("DEM" %in% r$process)
  # interpolation refuses the same model, at whichever guard fires first --
  # the point of the report is that it lists every one of them beforehand
  expect_error(suppressWarnings(interpolate_model(bad, name = "VQ_BAD")),
               "inconsistent with a declared|Declared resolution")
})

test_that("the coarse direction stays exempt for a consumed commodity", {
  skip_if_no_geoscales()
  # ECOA consumes COA, which balances at the group while ECOA sits at the
  # atoms: that is the no-disaggregation exemption, not a mismatch
  r <- check_model_levels(vq_model())
  expect_false("COA" %in% r$comm)
})

test_that("an aggregation conserves every extensive total", {
  skip_if_no_geoscales()
  m <- vq_model()
  a <- aggregate_model_regions(m, level = "grp", verbose = FALSE)
  v <- verify_aggregation(m, a)
  expect_s3_class(v, "aggregation_check")
  expect_false(any(v$verdict == "differs"))
  expect_false(any(v$verdict == "outside range"))
  # the quantities themselves
  dem <- v[v$class == "demand" & v$column == "demand", ]
  expect_equal(dem$verdict, "conserved")
  expect_equal(dem$fine, dem$coarse)
  cap <- v[v$class == "technology" & v$column == "stock", ]
  expect_equal(cap$coarse, 800)
  # a rate reports no total, only its range
  eff <- v[v$column == "cinp2use", ]
  expect_true(is.na(eff$fine))
  expect_equal(eff$verdict, "within range")
})

test_that("a corridor that became internal is not read as loss", {
  skip_if_no_geoscales()
  m <- vq_model()
  a <- aggregate_model_regions(m, level = "grp", verbose = FALSE)
  v <- verify_aggregation(m, a)
  trd <- v[v$class == "trade" & v$column == "ava.up", ]
  expect_equal(nrow(trd), 1L)
  expect_equal(trd$verdict, "by construction")
  expect_output(print(v), "by construction")
})

test_that("a broken total is caught, and so is a rate out of range", {
  skip_if_no_geoscales()
  m <- vq_model()
  a <- aggregate_model_regions(m, level = "grp", verbose = FALSE)
  tamper <- function(mod, nm, sl, cn, f) {
    for (i in seq_along(mod@data)) {
      d <- mod@data[[i]]@data
      if (nm %in% names(d)) {
        v <- methods::slot(d[[nm]], sl)
        v[[cn]] <- f(v[[cn]])
        methods::slot(d[[nm]], sl) <- v
        mod@data[[i]]@data <- d
      }
    }
    mod
  }
  half <- tamper(a, "DEM", "demand", "demand", function(x) x / 2)
  v1 <- verify_aggregation(m, half)
  dem <- v1[v1$class == "demand" & v1$column == "demand", ]
  expect_equal(dem$verdict, "differs")
  expect_equal(dem$rel, -0.5)

  dear <- tamper(a, "ECOA", "invcost", "invcost", function(x) x * 10)
  v2 <- verify_aggregation(m, dear)
  iv <- v2[v2$column == "invcost", ]
  expect_equal(iv$verdict, "outside range")
  expect_output(print(v2), "outside range")
})
