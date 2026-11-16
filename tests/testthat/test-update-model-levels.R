# =========================================================================== #
# `update()` on a model: moving the frame an element is declared at.
#
# The operation is one slot written in several objects and changes no value.
# The strict classes (`demand`, `supply`, `import`, `export`, `trade`,
# `storage`) are pinned to their commodity's frame, so the ground truths are:
#   * a commodity moves only when every pinned object's data is already at the
#     new frame -- otherwise the call refuses, naming the object and column,
#   * `data =` is applied FIRST, so one call carrying both goes through,
#   * `force = TRUE` writes the frame and warns, leaving the data to a later
#     call,
#   * naming a strict class directly is refused with a pointer to its
#     commodity.
# =========================================================================== #

skip_if_not_installed("geoscales")

# two zones: Z1 = {R1, R2}, Z2 = {R3}
gs_lvl <- function() {
  geoscales::geoscale_from_leaftable(
    data.frame(zone = c("Z1", "Z1", "Z2"),
               region = c("R1", "R2", "R3"),
               km2 = c(1, 1, 1)),
    geoframes = c("zone", "region"), name = "gs_lvl")
}

# Everything at region level: ELC's only pinned object is the demand.
mk_lvl <- function(regions = c("R1", "R2", "R3")) {
  repo <- newRepository(
    "lvl",
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "ANNUAL"),
    newSupply("SUP_COA", commodity = "COA",
              supply = data.frame(region = regions, cost = 5)),
    newTechnology("ECOA", input = list(comm = "COA"),
                  output = list(comm = "ELC"),
                  cap2act = 1, invcost = list(invcost = 100),
                  olife = list(olife = 30)),
    newDemand("DEM_ELC", commodity = "ELC",
              demand = data.frame(region = regions, year = 2025L, demand = 10))
  )
  mod <- newModel("LVL", data = repo, calendar = calendars$annual,
                  region = regions, horizon = newHorizon(2025L, name = "Y2025"),
                  discount = 0.05)
  setGeoscale(mod, gs_lvl())
}

# the demand re-keyed on zones, totals preserved
dem_zone <- function() {
  data.frame(region = c("Z1", "Z2"), year = 2025L, demand = c(20, 10))
}

obj_of <- function(mod, name) {
  out <- NULL
  for (rp in mod@data) {
    objs <- if (methods::is(rp, "repository")) rp@data else list(rp)
    for (o in objs) {
      if (isS4(o) && methods::.hasSlot(o, "name") && identical(o@name, name)) {
        out <- o
      }
    }
  }
  out
}

test_that("a dry run reports the pinned object not at the frame", {
  rep <- update(mk_lvl(), geoframe = list(ELC = "zone"), dryrun = TRUE)
  expect_s3_class(rep, "model_levels_report")
  expect_false(rep$ready)
  expect_identical(rep$requests$from, "region")
  expect_identical(rep$requests$to, "zone")
  # the commodity itself plus the one demand pinned to it
  expect_identical(rep$requests$objects, 2L)
  expect_true("DEM_ELC" %in% rep$rows$object)
  expect_true(all(rep$rows$status == "move"))
  expect_match(rep$rows$declared[rep$rows$object == "DEM_ELC"][1], "R1")
  out <- utils::capture.output(print(rep))
  expect_match(paste(out, collapse = " "), "DEM_ELC")
  expect_match(paste(out, collapse = " "), "aggregate_model_regions")
})

test_that("the move is refused while the data sits at the finer frame", {
  expect_error(update(mk_lvl(), geoframe = list(ELC = "zone")),
               "data is not at the requested frame")
})

test_that("one call carrying the data and the frame goes through", {
  mod <- update(mk_lvl(),
                geoframe = list(ELC = "zone"),
                data = list(DEM_ELC = list(demand = dem_zone())),
                verbose = FALSE)
  expect_identical(obj_of(mod, "ELC")@geoframe, "zone")
  # the pinned object carries the frame too: its OWN slot is what scopes it
  expect_identical(obj_of(mod, "DEM_ELC")@geoframe, "zone")
  d <- obj_of(mod, "DEM_ELC")@demand
  # the data.frame slot is REPLACED, so the province keys are gone
  expect_identical(sort(unique(as.character(d$region))), c("Z1", "Z2"))
  expect_identical(sum(d$demand), 30)
  # and the model now says nothing is outstanding
  expect_true(update(mod, geoframe = list(ELC = "zone"), dryrun = TRUE)$ready)
})

test_that("the data pass alone leaves the frames where they were", {
  mod <- update(mk_lvl(), data = list(DEM_ELC = list(demand = dem_zone())))
  expect_identical(obj_of(mod, "ELC")@geoframe, character())
  rg <- as.character(obj_of(mod, "DEM_ELC")@demand$region)
  expect_identical(sort(unique(rg)), c("Z1", "Z2"))
})

test_that("force writes the frame, warns, and records what is outstanding", {
  expect_warning(mod <- update(mk_lvl(), geoframe = list(ELC = "zone"),
                               force = TRUE, verbose = FALSE),
                 "force = TRUE")
  expect_identical(obj_of(mod, "ELC")@geoframe, "zone")
  expect_identical(obj_of(mod, "DEM_ELC")@geoframe, "zone")
  expect_length(mod@misc$levels, 1L)
  expect_true(mod@misc$levels[[1]]$forced)
  expect_true("DEM_ELC" %in% mod@misc$levels[[1]]$outstanding$object)
})

test_that("an element with no region-keyed data moves on its own", {
  mod <- update(mk_lvl(), geoframe = list(ECOA = "zone"), verbose = FALSE)
  expect_identical(obj_of(mod, "ECOA")@geoframe, "zone")
  expect_false(mod@misc$levels[[1]]$forced)
})

test_that("a strict class named directly is refused, naming its commodity", {
  expect_error(update(mk_lvl(), geoframe = list(DEM_ELC = "zone")),
               "commodity (ELC)", fixed = TRUE)
  expect_error(update(mk_lvl(), geoframe = list(SUP_COA = "zone")),
               "commodity (COA)", fixed = TRUE)
})

test_that("an unknown frame, an unknown element and a pinned class refuse", {
  expect_error(update(mk_lvl(), geoframe = list(ELC = "district")),
               "is not declared")
  expect_error(update(mk_lvl(), geoframe = list(XYZ = "zone")),
               "no object named")
  expect_error(update(mk_lvl(), timeframe = list(SUP_COA = "ANNUAL")),
               "follows its commodity")
})

test_that("an unnamed request and an unknown data target are caller errors", {
  expect_error(update(mk_lvl(), geoframe = "zone"), "must be named")
  expect_error(update(mk_lvl(), data = list(NOPE = list(demand = dem_zone()))),
               "no object named")
  expect_error(update(mk_lvl(), data = list(DEM_ELC = "nope")),
               "named list of slots")
})

test_that("the time axis reports against the calendar's timeframes", {
  rep <- update(mk_lvl(), timeframe = list(ELC = "ANNUAL"), dryrun = TRUE)
  expect_true(rep$ready)
  expect_error(update(mk_lvl(), timeframe = list(ELC = "HOUR")),
               "is not declared")
})

test_that("a model whose commodity was moved still interpolates", {
  mod <- update(mk_lvl(),
                geoframe = list(ELC = "zone"),
                data = list(DEM_ELC = list(demand = dem_zone())),
                verbose = FALSE)
  scen <- interpolate_model(mod, name = "LVLZ")
  expect_s4_class(scen, "scenario")
  # the demand landed on the zone codes
  dem <- scen@modInp@parameters$pDemand@data
  expect_true(all(c("Z1", "Z2") %in% as.character(dem$region)))
})
