# =========================================================================== #
# SPATIAL sampling validation -- the region mirror of test-subset_slices.R.
#
# A filtered geoscale passed to interpolate_model() turns the scenario into a
# SUB-TERRITORY model: only the sampled regions are declared, out-of-sample
# parameter rows are filtered, boundary-crossing trade routes are dropped
# (optionally replaced by priced stubs). Unlike calendar sampling there is no
# reweighting -- regional quantities are extensive -- so the ground truths are:
#   * a sampled scenario equals the HAND-BUILT sub-territory model, and
#   * with no cross-boundary trade, sample objectives ADD UP to the full one.
# =========================================================================== #

skip_if_not_installed("geoscales")

obj_glpk_r <- function(scen, tag) {
  td <- file.path(tempdir(), paste0("rsmpl_", tag))
  unlink(td, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  s <- tryCatch(suppressWarnings(suppressMessages(
    solve_scenario(scen, solver = solver_options$glpk,
                   tmp.dir = td, tmp.del = TRUE, force = TRUE))),
    error = function(e) NULL)
  if (is.null(s)) return(NA_real_)
  getData(s, "vObjective", merge = TRUE)$value
}

.have_glpk <- function() {
  nzchar(Sys.which("glpsol")) ||
    (!is.null(get_glpk_path()) && nzchar(get_glpk_path()))
}

gs3 <- function() {
  geoscales::geoscale_from_leaftable(
    data.frame(zone = c("Z12", "Z12", "Z3"),
               region = c("R1", "R2", "R3"),
               km2 = c(1, 1, 1)),
    geoframes = c("zone", "region"), name = "g3")
}

# per-region self-sufficient system; regions differ only in demand level
mk3 <- function(regions = c("R1", "R2", "R3"), dem = c(10, 20, 30),
                trade = NULL) {
  bricks <- list(
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "ANNUAL"),
    newSupply("SUP_COA", commodity = "COA",
              supply = data.frame(region = regions, cost = 5)),
    newTechnology("ECOA", input = list(comm = "COA"),
                  output = list(comm = "ELC"),
                  af = data.frame(af.up = 0.9),
                  invcost = data.frame(invcost = 1000),
                  olife = list(olife = 30), cap2act = 1),
    newDemand("DEM_ELC", commodity = "ELC",
              demand = data.frame(region = regions,
                                  demand = dem[seq_along(regions)]))
  )
  if (!is.null(trade)) bricks <- c(bricks, list(trade))
  newModel("rsmpl",
           repo = do.call(newRepository, c(list("r"), bricks)),
           region = regions, horizon = newHorizon(2020), discount = 0.05)
}

# --------------------------------------------------------------------------- #
test_that("a filtered geoscale yields the hand-built sub-territory model", {
  gs <- gs3()
  gs12 <- geoscales::filter_geoscale(gs, "region", c("R1", "R2"),
                                     drop_empty_geoframes = TRUE)

  scen_s <- suppressMessages(interpolate_model(
    mk3(), "smp12", gs12, ondisk = FALSE))
  scen_h <- suppressMessages(interpolate_model(
    mk3(regions = c("R1", "R2"), dem = c(10, 20)), "hand12",
    ondisk = FALSE))

  # declared regions are the sample
  expect_setequal(as.character(scen_s@settings@region), c("R1", "R2"))
  # parameter-level equality: demand rows match the hand-built model
  ds <- get_data_slot(scen_s@modInp@parameters$pDemand)
  dh <- get_data_slot(scen_h@modInp@parameters$pDemand)
  expect_setequal(unique(as.character(ds$region)), c("R1", "R2"))
  expect_equal(sum(ds$value), sum(dh$value))

  skip_if(!.have_glpk(), "GLPK (glpsol) not available")
  os <- obj_glpk_r(scen_s, "smp12")
  oh <- obj_glpk_r(scen_h, "hand12")
  expect_false(is.na(os))
  expect_equal(os, oh, tolerance = 1e-6)
})

test_that("sample objectives add up to the full model without cross trade", {
  skip_if(!.have_glpk(), "GLPK (glpsol) not available")
  gs <- gs3()
  o_full <- obj_glpk_r(suppressMessages(interpolate_model(
    mk3(), "full3", ondisk = FALSE)), "full3")
  o_12 <- obj_glpk_r(suppressMessages(interpolate_model(
    mk3(), "s12", geoscales::filter_geoscale(gs, "region", c("R1", "R2"),
                                             drop_empty_geoframes = TRUE),
    ondisk = FALSE)), "s12")
  o_3 <- obj_glpk_r(suppressMessages(interpolate_model(
    mk3(), "s3", geoscales::filter_geoscale(gs, "region", "R3",
                                            drop_empty_geoframes = TRUE),
    ondisk = FALSE)), "s3")
  expect_false(any(is.na(c(o_full, o_12, o_3))))
  expect_equal(o_12 + o_3, o_full, tolerance = 1e-6)
})

test_that("boundary-crossing trade routes are dropped with a message", {
  gs <- gs3()
  trd <- newTrade("TR23", commodity = "ELC", cap2act = 1,
                  routes = data.frame(src = "R2", dst = "R3"),
                  trade = data.frame(src = "R2", dst = "R3", ava.up = 4),
                  invcost = data.frame(invcost = 10),
                  capacity = data.frame(year = 2020, cap.up = 100))
  expect_message(
    scen <- suppressWarnings(interpolate_model(
      mk3(trade = trd), "drop23",
      geoscales::filter_geoscale(gs, "region", c("R1", "R2"),
                                 drop_empty_geoframes = TRUE),
      ondisk = FALSE)),
    "dropping trade route")
  expect_false("TR23" %in% as.character(scen@modInp@sets$trade))
})

test_that("a priced boundary stub equals the explicit export object", {
  gs <- gs3()
  trd <- newTrade("TR23", commodity = "ELC", cap2act = 1,
                  routes = data.frame(src = "R2", dst = "R3"),
                  trade = data.frame(src = "R2", dst = "R3", ava.up = 4),
                  invcost = data.frame(invcost = 10),
                  capacity = data.frame(year = 2020, cap.up = 100))
  bp <- data.frame(src = "R2", dst = "R3", price = 50, cap.up = 4)
  scen <- suppressMessages(interpolate_model(
    mk3(trade = trd), "stub23",
    geoscales::filter_geoscale(gs, "region", c("R1", "R2"),
                               drop_empty_geoframes = TRUE),
    boundary_prices = bp, ondisk = FALSE))
  expect_true("EXP_TR23_R22R3" %in% as.character(scen@modInp@sets$expp))

  skip_if(!.have_glpk(), "GLPK (glpsol) not available")
  exp_obj <- newExport("EXP_TR23_R22R3", commodity = "ELC",
                       export = data.frame(region = "R2", price = 50,
                                           exp.up = 4))
  scen_h <- suppressMessages(interpolate_model(
    mk3(regions = c("R1", "R2"), dem = c(10, 20), trade = exp_obj),
    "hand_stub", ondisk = FALSE))
  expect_equal(obj_glpk_r(scen, "stub23"),
               obj_glpk_r(scen_h, "hand_stub"), tolerance = 1e-6)
})

test_that("region-NA wildcards survive; declarations are judged vs the model", {
  gs <- gs3()
  scen <- suppressMessages(interpolate_model(
    mk3(), "wild12",
    geoscales::filter_geoscale(gs, "region", c("R1", "R2"),
                               drop_empty_geoframes = TRUE),
    ondisk = FALSE))
  # invcost was declared without a region (wildcard): present for the sample
  inv <- get_data_slot(scen@modInp@parameters$pTechInvcost)
  expect_true(nrow(inv) > 0L)
  expect_true(all(as.character(inv$region) %in% c("R1", "R2")))
  # demand naming R3 (the model's own region) did NOT error above; a truly
  # unknown region still does
  bad <- mk3()
  bad@data$r@data$DEM_BAD <- newDemand(
    "DEM_BAD", commodity = "ELC",
    demand = data.frame(region = "R9", demand = 1))
  expect_error(suppressMessages(interpolate_model(
    bad, "bad9",
    geoscales::filter_geoscale(gs, "region", c("R1", "R2"),
                               drop_empty_geoframes = TRUE),
    ondisk = FALSE)))
})
