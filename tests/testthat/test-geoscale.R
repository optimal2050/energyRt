# Attaching a geoscales::Geoscale to a model, and using it for maps and
# region-level aggregation. `geoscales` is a Suggests dependency, so every
# test that needs the package skips without it.

skip_if_no_geoscales <- function() skip_if_not_installed("geoscales")

demo_gs <- function() {
  geoscales::geoscale_from_leaves(
    data.frame(
      zone   = c("N", "N", "S", "S"),
      region = c("R1", "R2", "R3", "R4"),
      km2    = c(10, 20, 30, 40)
    ),
    levels = c("zone", "region"),
    name = "demo"
  )
}

# Attachment -------------------------------------------------------------------

test_that("is_geoscale() needs no geoscales namespace", {
  expect_false(is_geoscale(NULL))
  expect_false(is_geoscale(42))
  expect_false(is_geoscale(data.frame()))
  # An S7 object carries a plain character class attribute, so the test is a
  # pure attribute check -- this is what lets `config` validate without a
  # hard dependency.
  fake <- structure(list(), class = c("geoscales::Geoscale", "S7_object"))
  expect_true(is_geoscale(fake))
})

test_that("a fresh config has no geoscale", {
  expect_null(getGeoscale(new("config")))
})

test_that("setGeoscale round-trips on config, model and scenario", {
  skip_if_no_geoscales()
  gs <- demo_gs()

  cfg <- setGeoscale(new("config"), gs)
  expect_true(is_geoscale(getGeoscale(cfg)))
  expect_null(getGeoscale(setGeoscale(cfg, NULL)))

  mod <- newModel("demo", region = c("R1", "R2", "R3", "R4"), geoscale = gs)
  expect_true(is_geoscale(getGeoscale(mod)))

  # settings inherits from config, so `.config_to_settings()` carries it over
  # with no extra plumbing
  stt <- .config_to_settings(mod@config)
  expect_true(is_geoscale(getGeoscale(stt)))
})

test_that("the setter and the validity method reject non-geoscales", {
  expect_error(setGeoscale(new("config"), 42), "must be a .*Geoscale")

  # `config`'s prototype leaves `region` NULL in a "character" slot, so a
  # default object fails the slot-type check before any validity method runs.
  # Give it a valid region so the geoscale contract is what is being tested.
  cfg <- new("config")
  cfg@region <- character()
  cfg@geoscale <- 42
  msg <- methods::validObject(cfg, test = TRUE)
  expect_true(any(grepl("geoscale", msg)))

  cfg@geoscale <- NULL
  expect_true(isTRUE(methods::validObject(cfg, test = TRUE)))
})

test_that("update() and unnamed dispatch both reach the slot", {
  skip_if_no_geoscales()
  gs <- demo_gs()
  expect_true(is_geoscale(getGeoscale(update(new("config"), geoscale = gs))))
  # name and desc are positional, so an unnamed geoscale must follow them
  mod <- newModel("demo", "desc", gs, region = c("R1", "R2"))
  expect_true(is_geoscale(getGeoscale(mod)))
})

test_that("a model serialised before the slot existed still converts", {
  # `.config_to_settings()` skips slots the stored object does not carry, which
  # is what keeps models saved before a slot was added loadable.
  cfg <- new("config")
  cfg@region <- c("R1", "R2")
  attr(cfg, "geoscale") <- NULL
  expect_false(methods::.hasSlot(cfg, "geoscale"))

  stt <- .config_to_settings(cfg)
  expect_s4_class(stt, "settings")
  expect_equal(stt@region, c("R1", "R2"))
  expect_null(getGeoscale(stt))
})

test_that("getCalendar has methods again", {
  mod <- newModel("demo", region = "R1")
  expect_s4_class(getCalendar(mod), "calendar")
  expect_s4_class(getCalendar(mod@config), "calendar")
})

test_that("check_geoscale_regions warns only about uncovered regions", {
  skip_if_no_geoscales()
  gs <- demo_gs()
  expect_silent(check_geoscale_regions(gs, c("R1", "R2")))
  expect_warning(check_geoscale_regions(gs, c("R1", "GHOST")), "GHOST")
  # a geoscale wider than the model is normal, not a problem
  expect_silent(check_geoscale_regions(gs, "R1"))
})

# The variable catalogue drives the aggregation rules -------------------------

test_that("interregional variables are identified from the catalogue, not a list", {
  expect_true(.is_interregional_var("vTradeIr"))
  expect_false(.is_interregional_var("vTechOut"))
  # If a second inter-regional variable is ever declared, the netting path
  # must be revisited -- this pins the assumption.
  spec <- .variables
  flows <- names(spec)[vapply(spec, function(z) identical(z$role, "interregional"),
                              logical(1))]
  expect_equal(flows, "vTradeIr")
  # role and dimensions agree independently
  two_region <- names(spec)[vapply(
    spec, function(z) sum(z$dimSets == "region") > 1, logical(1))]
  expect_equal(two_region, "vTradeIr")
})

test_that("region dimensions are read off the catalogue", {
  expect_true(.has_region_dim("vTechOut"))
  expect_false(.has_region_dim("vTradeCap"))
})

test_that("the temporal stock rule does NOT carry over to space", {
  skip_if_no_geoscales()
  # `role: stock` means "never summed over timeslices". Summing a storage level
  # across regions is meaningful, so it must aggregate normally here.
  expect_true(.is_state_var("vStorageLevel"))
  expect_false(.is_interregional_var("vStorageLevel"))

  gs <- demo_gs()
  x <- data.frame(region = c("R1", "R2", "R3", "R4"), value = c(1, 2, 3, 4))
  out <- .geo_aggregate(x, gs, from = "region", to = "zone",
                        name = "vStorageLevel")
  expect_equal(sum(out$value), 10)
  expect_equal(out$value[out$zone == "N"], 3)
})

# Aggregation ------------------------------------------------------------------

test_that("extensive results roll up and conserve the total", {
  skip_if_no_geoscales()
  gs <- demo_gs()
  x <- data.frame(region = c("R1", "R2", "R3", "R4"), value = c(1, 2, 3, 4))
  out <- .geo_aggregate(x, gs, from = "region", to = "zone", name = "vTechOut")
  expect_equal(sum(out$value), sum(x$value))
  expect_equal(out$value[out$zone == "N"], 3)
  expect_equal(out$value[out$zone == "S"], 7)
})

test_that("aggregating to the same level is a no-op", {
  skip_if_no_geoscales()
  gs <- demo_gs()
  x <- data.frame(region = "R1", value = 1)
  expect_identical(.geo_aggregate(x, gs, "region", "region"), x)
})

test_that("inter-regional flows net instead of summing", {
  skip_if_no_geoscales()
  gs <- demo_gs()   # N = R1,R2 ; S = R3,R4
  trd <- data.frame(
    src   = c("R1", "R3", "R2"),
    dst   = c("R2", "R4", "R3"),   # first two are internal to a zone
    value = c(10, 20, 5)
  )
  out <- .geo_aggregate(trd, gs, from = "region", to = "zone",
                        name = "vTradeIr")
  # only R2 -> R3 crosses the boundary
  expect_equal(nrow(out), 2L)
  expect_equal(out$value[out$zone == "N"], -5)
  expect_equal(out$value[out$zone == "S"], 5)
  expect_equal(sum(out$value), 0)
})

test_that("wholly internal trade disappears at the coarser level", {
  skip_if_no_geoscales()
  gs <- demo_gs()
  trd <- data.frame(src = "R1", dst = "R2", value = 9)
  out <- .geo_aggregate(trd, gs, "region", "zone", name = "vTradeIr")
  expect_equal(nrow(out), 0L)
})

test_that("the src/dst shape is netted even without a variable name", {
  skip_if_no_geoscales()
  gs <- demo_gs()
  trd <- data.frame(src = "R2", dst = "R3", value = 4)
  out <- .geo_aggregate(trd, gs, "region", "zone")
  expect_equal(sum(out$value), 0)
  expect_setequal(out$zone, c("N", "S"))
})

# Maps -------------------------------------------------------------------------

test_that("utopia_geoscale builds, with and without geometry", {
  skip_if_no_geoscales()
  g0 <- utopia_geoscale(layout = NULL)
  expect_equal(geoscales::geo_levels(g0), c("nation", "zone", "region"))
  expect_setequal(geoscales::geo_regions(g0, "region"), paste0("R", 1:11))
  expect_equal(geoscales::geo_children(g0, "zone", "WEST"),
               c("R1", "R2", "R3"))

  skip_if_not_installed("sf")
  gs <- utopia_geoscale()
  expect_equal(geoscales::geo_weights(gs), "area")
  expect_equal(nrow(geoscales::geo_geometry(gs, "zone")), 3L)

  # every layout shares the region names, so the hierarchy fits all of them
  for (nm in names(utopia$map)) {
    expect_setequal(utopia$map[[nm]]$region, utopia$geo$region)
  }
})

test_that("utopia_geoscale reaches its data the way an INSTALL does", {
  skip_if_no_geoscales()
  # `utopia` is LazyData: under load_all() it sits in the namespace, but in an
  # installed package it is in the lazy-load database instead. Reading it with
  # get(..., asNamespace()) therefore worked in dev and failed only once
  # installed -- which is how it escaped into a broken vignette build. Assert
  # the accessor the installed path actually uses.
  expect_false(is.null(getExportedValue("energyRt", "utopia")))
  e <- new.env(parent = emptyenv())
  utils::data("utopia", package = "energyRt", envir = e)
  expect_setequal(names(get("utopia", envir = e)), c("map", "geo"))

  # and the function must work with the namespace copy absent
  local({
    ns_has <- exists("utopia", envir = asNamespace("energyRt"), inherits = FALSE)
    skip_if(!ns_has, "namespace copy already absent; the data() path is covered above")
    expect_s3_class(utopia_geoscale(layout = NULL), "geoscales::Geoscale")
  })
})

test_that("utopia_geoscale can be subset to a model's regions", {
  skip_if_no_geoscales()
  gs <- utopia_geoscale(layout = NULL, region = c("R1", "R2", "R3"))
  expect_setequal(geoscales::geo_regions(gs, "region"), c("R1", "R2", "R3"))
  expect_equal(geoscales::geo_regions(gs, "zone"), "WEST")
  expect_error(utopia_geoscale(region = "R99"), "Unknown UTOPIA region")
})

test_that("plot_map delegates drawing to geoscales::geo_plot", {
  skip_if_no_geoscales()
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  skip_if_no_solver()
  # energyRt decides WHAT to draw (variable, netting, level); geoscales draws
  # it. This pins the handover: the plot must carry the viridis scale, the
  # titles energyRt composes, and no legend key -- the same output as when
  # plot_map built the ggplot itself.
  skip_if(!("palette" %in% names(formals(geoscales::geo_plot))),
          "installed geoscales predates the geo_plot() enrichment")

  regs <- c("R1", "R2")
  mod <- setGeoscale(vt_model(name = "gm", regions = regs),
                     utopia_geoscale(region = regs))
  sol <- vt_solve(vt_interp(mod, "gm"))

  p <- plot_map(sol, "capacity")
  expect_s3_class(p, "ggplot")
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomSf" %in% geoms)
  expect_equal(p$labels$title, "capacity")
  expect_equal(p$labels$subtitle, "by region")
  expect_null(p$labels$fill)
  # viridis, not ggplot's default blue gradient
  fills <- ggplot2::ggplot_build(p)$data[[1]]$fill
  expect_true(any(grepl("^#", fills)))
  expect_false(any(fills == "#132B43"))

  # a coarser level aggregates first and yields one polygon per zone
  pz <- plot_map(sol, "capacity", level = "zone")
  expect_equal(pz$labels$subtitle, "by zone")
  expect_equal(nrow(ggplot2::ggplot_build(pz)$data[[1]]), 1L)
})

test_that("plot_trade_map keeps working on a CRS-less map", {
  skip_if_no_geoscales()
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  trd <- newTrade("TRD", commodity = "ELC",
                  routes = data.frame(src = c("R1", "R2"),
                                      dst = c("R2", "R7")))
  p <- plot_trade_map(trd, map = utopia$map$honeycomb)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  # no CRS -> the plain cartesian layers are correct and are kept
  expect_true("GeomSegment" %in% geoms)
})

test_that("plot_trade_map draws sf layers when the map has a CRS", {
  skip_if_no_geoscales()
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  trd <- newTrade("TRD", commodity = "ELC",
                  routes = data.frame(src = c("R1", "R2"),
                                      dst = c("R2", "R7")))
  m <- utopia$map$honeycomb
  sf::st_crs(m) <- 4326
  p <- plot_trade_map(trd, map = m)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  # `geom_sf()` installs a coord_sf that would leave geom_segment behind
  expect_false("GeomSegment" %in% geoms)
  expect_false("GeomPoint" %in% geoms)
})

test_that("plot_trade_map accepts a Geoscale and a coarser level", {
  skip_if_no_geoscales()
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  gs <- utopia_geoscale()
  trd <- newTrade("TRD", commodity = "ELC",
                  routes = data.frame(src = c("R1", "R4"),
                                      dst = c("R2", "R8")))
  expect_s3_class(plot_trade_map(trd, map = gs), "ggplot")

  # R1->R2 is inside WEST and drops out; R4->R8 crosses CENTRAL -> EAST
  p <- plot_trade_map(trd, map = gs, level = "zone")
  expect_s3_class(p, "ggplot")

  only_internal <- newTrade("TRD2", commodity = "ELC",
                            routes = data.frame(src = "R1", dst = "R2"))
  expect_message(plot_trade_map(only_internal, map = gs, level = "zone"),
                 "cross a boundary")
})

test_that("plot_trade_map still demands geometry when there is none", {
  trd <- newTrade("TRD", commodity = "ELC",
                  routes = data.frame(src = "R1", dst = "R2"))
  expect_error(plot_trade_map(trd), "pass a `map`")
})
