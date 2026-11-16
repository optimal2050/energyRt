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

  skip_if_no_solver()
  os <- obj_glpk_r(scen_s, "smp12")
  oh <- obj_glpk_r(scen_h, "hand12")
  expect_false(is.na(os))
  expect_equal(os, oh, tolerance = 1e-6)
})

test_that("sample objectives add up to the full model without cross trade", {
  skip_if_no_solver()
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
    # dropped content is reported ONCE at the end; `verbose = TRUE` names each
    # route instead (R/sample_region.R)
    "trade route\\(s\\) outside the sample")
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

  skip_if_no_solver()
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

test_that("a sub-territory solve announces itself even when quiet", {
  # `.spatial_sample_mode()` accepts a filtered geoscale silently, so a
  # geoscale covering only some of the declared regions yields a smaller solve
  # -- a different answer from the full model's. The one line saying so must
  # not sit behind `verbose`.
  gs12 <- geoscales::filter_geoscale(gs3(), "region", c("R1", "R2"),
                                     drop_empty_geoframes = TRUE)
  expect_message(
    interpolate_model(mk3(), "smp_quiet", gs12, ondisk = FALSE,
                      verbose = FALSE),
    "sub-territory solve on 2 of 3 regions")
})

# --------------------------------------------------------------------------- #
# The region-axis closure: a cut leaves objects with nothing left to do.
# --------------------------------------------------------------------------- #

# mk3 plus a hydro chain that exists in R3 alone -- its own commodity, its own
# weather series, and the only plant that uses either.
mk3_hyd <- function(regions = c("R1", "R2", "R3")) {
  m <- mk3(regions = regions)
  add(m, newRepository(
    "hyd",
    newCommodity("HYD", timeframe = "ANNUAL"),
    newWeather("WHYD", timeframe = "ANNUAL",
               weather = data.frame(region = "R3", wval = 0.5)),
    newSupply("SUP_HYD", commodity = "HYD",
              supply = data.frame(region = "R3", ava.up = 50, cost = 0)),
    newTechnology("EHYD", region = "R3", input = list(comm = "HYD"),
                  output = list(comm = "ELC"),
                  weather = data.frame(weather = "WHYD", waf.up = 1),
                  invcost = list(invcost = 900), olife = list(olife = 50),
                  cap2act = 1)))
}

test_that("a cut drops what it leaves with nothing to do, and records it", {
  m <- mk3_hyd()
  expect_true(all(c("EHYD", "HYD", "SUP_HYD", "WHYD") %in%
                    names(getObject(m))))

  s <- suppressMessages(subset_model_regions(m, region = c("R1", "R2")))
  left <- names(getObject(s))
  # EHYD goes with its region; then HYD has no consumer, SUP_HYD no customer,
  # and WHYD nothing that names it -- the closure, not the interpolator.
  expect_false(any(c("EHYD", "HYD", "SUP_HYD", "WHYD") %in% left))
  expect_true(all(c("ECOA", "SUP_COA", "ELC", "COA", "DEM_ELC") %in% left))
  expect_setequal(s@misc$subset$orphans, c("HYD", "SUP_HYD", "WHYD"))
  # EHYD left for having no region at all, which is not an orphan
  expect_false("EHYD" %in% s@misc$subset$orphans)
})

test_that("the closure is a no-op on a model that loses no region", {
  m <- mk3_hyd()
  s <- suppressMessages(subset_model_regions(m, region = c("R1", "R2", "R3")))
  expect_setequal(names(getObject(s)), names(getObject(m)))
  expect_length(s@misc$subset$orphans, 0L)
})

test_that("the cut is recorded on the model", {
  m <- mk3_hyd()
  s <- suppressMessages(subset_model_regions(m, region = c("R1", "R2")))
  rec <- s@misc$subset
  expect_setequal(rec$regions, c("R1", "R2"))
  expect_true(rec$objects_dropped >= 4L)
  expect_equal(rec$routes_dropped, 0L)
})

test_that("a code at any geoframe names its regions", {
  m <- setGeoscale(mk3_hyd(), gs3())
  by_zone <- suppressMessages(subset_model_regions(m, region = "Z12"))
  by_hand <- suppressMessages(subset_model_regions(m, region = c("R1", "R2")))
  # asking for the zone IS asking for its members: same model, not merely a
  # similar one
  expect_identical(by_zone, by_hand)
  expect_setequal(as.character(by_zone@config@region), c("R1", "R2"))
})

test_that("a code at no geoframe is refused, naming the frames", {
  m <- setGeoscale(mk3_hyd(), gs3())
  expect_error(subset_model_regions(m, region = "ZZ"),
               "not a code at any geoframe")
  expect_error(subset_model_regions(m, region = "ZZ"), "zone Z12/Z3")
})

mk_topia_mod <- function() {
  k <- topia$modules$electricity
  mod <- newModel("T", data = k$repo, calendar = calendars[["d365_h24_1dps"]],
                  region = k$regions,
                  horizon = topia$modules$horizons$BY2025_50_by10,
                  discount = 0.05)
  setGeoscale(mod, k$geoscale)
}

# @covers subset_model_regions depth=U
test_that("a dry run answers whether a cut can be made, without cutting", {
  skip_if_not_installed("geoscales")
  mod <- mk_topia_mod()
  ok <- subset_model_regions(mod, "Z3", dryrun = TRUE)
  expect_s3_class(ok, "region_subset_report")
  expect_true(ok$ready)
  expect_setequal(ok$regions, c("R04", "R05", "R06"))
  expect_null(ok$rows)
  # the model is untouched: a dry run builds nothing
  expect_length(get_region(mod), 12L)
})

# @covers subset_model_regions depth=U
test_that("a dry run names what a split cut still needs", {
  skip_if_not_installed("geoscales")
  mod <- mk_topia_mod()
  r <- suppressWarnings(subset_model_regions(mod, "R03", dryrun = TRUE))
  expect_false(r$ready)
  # the nation is split too, but nothing is declared there, so it is not named
  expect_equal(r$declared, "Z2")
  expect_equal(unname(r$missing["Z2"]), "R02")
  ask <- r$rows[!r$rows$ready, ]
  expect_true("SUP_GAS" %in% ask$name)
  expect_equal(ask$needs[ask$name == "SUP_GAS" & ask$column == "ava.up"],
               "weight")
  # a corridor endpoint at a split group is a network question, not a weight
  expect_true(all(ask$needs[ask$name == "TBD_GAS_Z2_Z3"] == "endpoint"))
  # a rate is carried, and the coarse corridor cost is a caveat, not a blocker
  carried <- r$rows[r$rows$ready, ]
  expect_true(all(c("cost", "price", "invcost") %in% carried$column))
  expect_match(r$caveats, "TBD_GAS_Z2_Z3@invcost")
  # the refusal is the same report
  expect_error(suppressWarnings(subset_model_regions(mod, "R03")),
               "cannot be cut as it stands")
})

# @covers subset_model_regions depth=U
test_that("a named weight settles a quantity in the pre-flight", {
  skip_if_not_installed("geoscales")
  mod <- mk_topia_mod()
  r <- suppressWarnings(subset_model_regions(
    mod, "R03", dryrun = TRUE,
    split_weight = list(SUP_GAS = c(ava.up = "area"))))
  row <- r$rows[r$rows$name == "SUP_GAS" & r$rows$column == "ava.up", ]
  expect_equal(row$weight, "area")
  expect_true(row$ready)
  # a weight the geoscale does not declare is refused, naming what it has
  expect_error(suppressWarnings(subset_model_regions(
    mod, "R03", dryrun = TRUE, split_weight = list(supply = "population"))),
    "not a weight column")
})

# @covers is_separable depth=U
test_that("is_separable answers the question in one logical", {
  skip_if_not_installed("geoscales")
  mod <- mk_topia_mod()
  expect_false(suppressWarnings(is_separable(mod, "R03")))
  expect_true(is_separable(mod, "Z3"))
  expect_s3_class(attr(suppressWarnings(is_separable(mod, "R03")), "report"),
                  "region_subset_report")
  # a geoframe sweep asks the question once per code at that frame
  z <- suppressWarnings(is_separable(mod, geoframe = "zone"))
  expect_setequal(names(z), c("Z1", "Z2", "Z3", "Z4", "Z5"))
  expect_true(all(z))
  # one of the two arguments, not both and not neither
  expect_error(is_separable(mod), "either `region`")
  expect_error(is_separable(mod, "Z3", geoframe = "zone"), "either `region`")
})

# @covers subset_model_regions depth=U
test_that("a decision the cut cannot yet carry out is refused, not ignored", {
  skip_if_not_installed("geoscales")
  mod <- mk_topia_mod()
  expect_error(suppressWarnings(subset_model_regions(
    mod, "R03", split_weight = list(SUP_GAS = c(ava.up = "area")))),
    "not[ \n]+yet applied")
})

# @covers subset_model_regions depth=U
test_that("one weight can be named for everything that asks", {
  skip_if_not_installed("geoscales")
  mod <- mk_topia_mod()
  ask <- function(sw) {
    r <- suppressWarnings(subset_model_regions(mod, "R03", dryrun = TRUE,
                                               split_weight = sw))
    r$rows[r$rows$column == "ava.up", ]
  }
  expect_equal(ask("area")$weight, "area")             # unnamed shorthand
  expect_equal(ask(list(all = "area"))$weight, "area") # the reserved key
  expect_true(ask("area")$ready)
  # a key for one object beats the blanket
  gs <- geoscales::geoscale_add_weight(
    topia$modules$electricity$geoscale,
    population = stats::setNames(seq_len(12) * 10, sprintf("R%02d", 1:12)))
  m2 <- setGeoscale(mod, gs)
  r <- suppressWarnings(subset_model_regions(
    m2, "R03", dryrun = TRUE,
    split_weight = list(all = "area", SUP_GAS = c(ava.up = "population"))))
  expect_equal(r$rows$weight[r$rows$column == "ava.up"], "population")
  # two weights and no keys is ambiguous, not a blanket
  expect_error(suppressWarnings(subset_model_regions(
    mod, "R03", dryrun = TRUE, split_weight = c("area", "km2"))),
    "must be named")
})
