# Attaching a geoscales::Geoscale to a model, and using it for maps and
# region-level aggregation. `geoscales` is a Suggests dependency, so every
# test that needs the package skips without it.

skip_if_no_geoscales <- function() skip_if_not_installed("geoscales")

demo_gs <- function() {
  geoscales::geoscale_from_leaftable(
    data.frame(
      zone   = c("N", "N", "S", "S"),
      region = c("R1", "R2", "R3", "R4"),
      km2    = c(10, 20, 30, 40)
    ),
    geoframes = c("zone", "region"),
    name = "demo"
  )
}

# Attachment -------------------------------------------------------------------

test_that("energyRt:::is_geoscale() needs no geoscales namespace", {
  expect_false(energyRt:::is_geoscale(NULL))
  expect_false(energyRt:::is_geoscale(42))
  expect_false(energyRt:::is_geoscale(data.frame()))
  # An S7 object carries a plain character class attribute, so the test is a
  # pure attribute check -- this is what lets `config` validate without a
  # hard dependency.
  fake <- structure(list(), class = c("geoscales::Geoscale", "S7_object"))
  expect_true(energyRt:::is_geoscale(fake))
})

test_that("a fresh config has no geoscale", {
  expect_null(getGeoscale(new("config")))
})

test_that("setGeoscale round-trips on config, model and scenario", {
  skip_if_no_geoscales()
  gs <- demo_gs()

  cfg <- setGeoscale(new("config"), gs)
  expect_true(energyRt:::is_geoscale(getGeoscale(cfg)))
  expect_null(getGeoscale(setGeoscale(cfg, NULL)))

  mod <- newModel("demo", region = c("R1", "R2", "R3", "R4"), geoscale = gs)
  expect_true(energyRt:::is_geoscale(getGeoscale(mod)))

  # settings inherits from config, so `.config_to_settings()` carries it over
  # with no extra plumbing
  stt <- .config_to_settings(mod@config)
  expect_true(energyRt:::is_geoscale(getGeoscale(stt)))
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
  expect_true(energyRt:::is_geoscale(getGeoscale(update(new("config"), geoscale = gs))))
  # name and desc are positional, so an unnamed geoscale must follow them
  mod <- newModel("demo", "desc", gs, region = c("R1", "R2"))
  expect_true(energyRt:::is_geoscale(getGeoscale(mod)))
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

test_that("topia ships one Geoscale per layout, sharing one hierarchy", {
  skip_if_no_geoscales()
  skip_if_not_installed("sf")
  expect_setequal(names(topia$geoscales),
                  c("squares", "honeycomb", "island", "continent"))

  g0 <- topia$geoscales$honeycomb
  expect_equal(geoscales::geoscale_geoframes(g0), c("nation", "zone", "region"))
  expect_setequal(geoscales::geoscale_regions(g0, "region"),
                  c("W1", "W2", paste0("C", 1:6), paste0("E", 1:3)))
  # zone-prefixed codes: the zone is readable from the name alone
  expect_equal(geoscales::geoscale_children(g0, "zone", "WEST"),
               c("W1", "W2"))
  expect_equal(geoscales::geoscale_children(g0, "zone", "EAST"),
               c("E1", "E2", "E3"))
  expect_equal(geoscales::geoscale_weights(g0), "area")
  expect_equal(nrow(geoscales::geoscale_geometry(g0, "zone")), 3L)

  # The four layouts differ ONLY in the polygons they carry. `area` is measured
  # from those, so compare the hierarchy columns and leave it out.
  cols <- c("nation", "zone", "region", "name")
  ref <- geoscales::geoscale_leaftable(g0)[, cols]
  for (nm in names(topia$geoscales)) {
    gs <- topia$geoscales[[nm]]
    expect_s3_class(gs, "geoscales::Geoscale")
    expect_equal(geoscales::geoscale_leaftable(gs)[, cols], ref,
                 label = paste0("hierarchy of ", nm))
    expect_equal(length(geoscales::geoscale_geometry(gs, "region")$region), 11L,
                 label = paste0("features in ", nm))
  }
})

test_that("every zone is one contiguous block on every layout", {
  skip_if_no_geoscales()
  skip_if_not_installed("sf")
  # The reason the codes are assigned per layout (data-raw/topia_geoscale.R):
  # the four layouts draw the same eleven regions in different places, and each
  # one gets the assignment that keeps W/C/E together on the picture. Before
  # 0.91 the zones were slices of R1..R11 and 7 of these 12 cells were split,
  # which is what made zone shapes and zone-level labels fragile.
  #
  # `nrow(geoscale_geometry(gs, "zone")) == 3` above passes even when a zone
  # dissolves to several disjoint parts, so assert connectedness directly.
  geo <- geoscales::geoscale_leaftable(topia$geoscales[[1]])
  for (nm in names(topia$geoscales)) {
    x <- geoscales::geoscale_geometry(topia$geoscales[[nm]], "region")
    reg <- as.character(x$region)
    nbl <- setNames(lapply(sf::st_touches(x), function(i) reg[i]), reg)
    # an offshore region touches nothing and joins its nearest neighbour
    # across the sea -- one explicit link, not a distance tolerance (a
    # tolerance wide enough to bridge the gap on `squares` also links every
    # pair one cell apart)
    d <- sf::st_distance(x)
    units(d) <- NULL
    for (i in which(lengths(nbl) == 0)) {
      v <- d[i, ]; v[i] <- Inf; j <- which.min(v)
      nbl[[reg[i]]] <- c(nbl[[reg[i]]], reg[j])
      nbl[[reg[j]]] <- c(nbl[[reg[j]]], reg[i])
    }
    for (z in unique(geo$zone)) {
      m <- geo$region[geo$zone == z]
      seen <- m[1]; fr <- m[1]
      while (length(fr)) {
        nx <- setdiff(intersect(unlist(nbl[fr]), m), seen)
        seen <- c(seen, nx); fr <- nx
      }
      expect_setequal(seen, m)
    }
  }
})

test_that("topia reaches its data the way an INSTALL does", {
  skip_if_no_geoscales()
  # `topia` is LazyData: under load_all() it sits in the namespace, but in an
  # installed package it is in the lazy-load database instead. Reading it with
  # get(..., asNamespace()) therefore worked in dev and failed only once
  # installed -- which is how it escaped into a broken vignette build. Assert
  # the accessor the installed path actually uses.
  expect_false(is.null(getExportedValue("energyRt", "topia")))
  e <- new.env(parent = emptyenv())
  utils::data("topia", package = "energyRt", envir = e)
  shipped <- get("topia", envir = e)
  expect_setequal(names(shipped),
                  c("geoscales", "weather", "demand", "stock", "modules"))
  # the 0.91 break: no second copy of the geometry anywhere in the dataset
  expect_false(any(c("map", "geo") %in% names(shipped)))
  expect_null(shipped$modules$maps)
  expect_s3_class(shipped$geoscales$honeycomb, "geoscales::Geoscale")
})

test_that("a shipped geoscale subsets to a model's regions", {
  skip_if_no_geoscales()
  skip_if_not_installed("sf")
  # `topia_geoscale(region =)` was removed in 0.91; subsetting is now the
  # geoscales verb, which keeps the geometry and the `area` weight
  full <- topia$geoscales$honeycomb
  # the kits are PREFIXES of the west-to-east order, so a 3-region model
  # spans WEST and the first CENTRAL region
  gs <- geoscales::filter_geoscale(full, "region", c("W1", "W2", "C1"))
  expect_setequal(geoscales::geoscale_regions(gs, "region"), c("W1", "W2", "C1"))
  expect_setequal(geoscales::geoscale_regions(gs, "zone"), c("WEST", "CENTRAL"))
  expect_equal(geoscales::geoscale_weights(gs), "area")
  expect_equal(length(geoscales::geoscale_geometry(gs, "region")$region), 3L)

  gsw <- geoscales::filter_geoscale(full, "region", c("W1", "W2"))
  expect_equal(geoscales::geoscale_regions(gsw, "zone"), "WEST")
  expect_error(geoscales::filter_geoscale(full, "region", "R99"))
})

test_that("plot_map delegates drawing to geoscales::geoscale_plot", {
  skip_if_no_geoscales()
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  skip_if_no_solver()
  # energyRt decides WHAT to draw (variable, netting, level); geoscales draws
  # it. This pins the handover: the plot must carry the viridis scale, the
  # titles energyRt composes, and no legend key -- the same output as when
  # plot_map built the ggplot itself.
  skip_if(!("palette" %in% names(formals(geoscales::geoscale_plot))),
          "installed geoscales predates the geoscale_plot() enrichment")

  regs <- c("W1", "W2")
  mod <- setGeoscale(vt_model(name = "gm", regions = regs),
                     geoscales::filter_geoscale(topia$geoscales$honeycomb,
                                                "region", regs))
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
                  routes = data.frame(src = c("W1", "W2"),
                                      dst = c("W2", "C5")))
  # a plain `sf` straight out of the geoscale: no `x`/`y` columns, so this
  # also pins that plot_trade_map() derives the label anchors itself
  m <- geoscales::geoscale_geometry(topia$geoscales$honeycomb, "region")
  expect_false(any(c("x", "y") %in% names(m)))
  p <- plot_trade_map(trd, map = m)
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  # no CRS -> the plain cartesian layers are correct and are kept
  expect_true("GeomSegment" %in% geoms)
})

test_that("plot_trade_map draws sf layers when the map has a CRS", {
  skip_if_no_geoscales()
  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")
  trd <- newTrade("TRD", commodity = "ELC",
                  routes = data.frame(src = c("W1", "W2"),
                                      dst = c("W2", "C5")))
  m <- geoscales::geoscale_geometry(topia$geoscales$honeycomb, "region")
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
  gs <- topia$geoscales$honeycomb
  trd <- newTrade("TRD", commodity = "ELC",
                  routes = data.frame(src = c("W1", "C4"),
                                      dst = c("W2", "E2")))
  expect_s3_class(plot_trade_map(trd, map = gs), "ggplot")

  # W1->W2 is inside WEST and drops out; C4->E2 crosses CENTRAL -> EAST
  p <- plot_trade_map(trd, map = gs, level = "zone")
  expect_s3_class(p, "ggplot")

  only_internal <- newTrade("TRD2", commodity = "ELC",
                            routes = data.frame(src = "W1", dst = "W2"))
  expect_message(plot_trade_map(only_internal, map = gs, level = "zone"),
                 "cross a boundary")
})

test_that("plot_trade_map still demands geometry when there is none", {
  trd <- newTrade("TRD", commodity = "ELC",
                  routes = data.frame(src = "R1", dst = "R2"))
  expect_error(plot_trade_map(trd), "pass a `map`")
})
