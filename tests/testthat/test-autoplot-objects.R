# Object autoplots: points + interpolated lines (interpolate = TRUE default),
# optional defaults (show_defaults), and the NULL-filter fix that made every
# process autoplot claim "No year-indexed data".

skip_if_not_installed("ggplot2")

.ap_geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1],
                                character(1))

test_that("a year-less supply plots without the 'no data' message", {
  sup <- newSupply("SUP_COA", commodity = "COA",
                   supply = data.frame(region = "R1", cost = 2.5))
  expect_no_message(p <- ggplot2::autoplot(sup))
  expect_s3_class(p, "ggplot")
  # constant series: dashed hline + the given point
  expect_true("GeomHline" %in% .ap_geoms(p))
  expect_true("GeomPoint" %in% .ap_geoms(p))
})

test_that("year-keyed data gives points + interpolated lines, extrapolable", {
  sup <- newSupply("SUP_GAS", commodity = "GAS",
                   supply = data.frame(year = c(2020L, 2040L),
                                       cost = c(10, 20)))
  p <- ggplot2::autoplot(sup, year = 2020:2050)
  g <- .ap_geoms(p)
  expect_true("GeomLine" %in% g)
  expect_true("GeomPoint" %in% g)
  ld <- ggplot2::layer_data(p, which(g == "GeomLine")[1])
  expect_equal(range(ld$x), c(2020, 2050))          # grid, not observed range
  expect_equal(ld$y[ld$x == 2050], 20)              # constant extrapolation
  expect_equal(ld$y[ld$x == 2030], 15)              # linear inside
})

test_that("interpolate = FALSE draws the given data only", {
  sup <- newSupply("SUP_GAS", commodity = "GAS",
                   supply = data.frame(year = c(2020L, 2040L),
                                       cost = c(10, 20)))
  p <- ggplot2::autoplot(sup, interpolate = FALSE)
  expect_false("GeomLine" %in% .ap_geoms(p))
  expect_true("GeomPoint" %in% .ap_geoms(p))
})

test_that("show_defaults draws finite defaults, notes non-finite ones", {
  sup <- newSupply("SUP_COA", commodity = "COA",
                   supply = data.frame(region = "R1", cost = 2.5))
  p <- ggplot2::autoplot(sup, show_defaults = TRUE)
  g <- .ap_geoms(p)
  expect_gte(sum(g == "GeomHline"), 2L)   # constant cost + ava.lo default
  hl <- do.call(rbind, lapply(which(g == "GeomHline"), function(i)
    ggplot2::layer_data(p, i)))
  expect_true(0 %in% hl$yintercept)       # ava.lo default = 0 drawn
  expect_match(p$labels$caption, "ava.up = Inf")   # Inf not drawn, noted
})

test_that("the other process classes plot without messages", {
  tech <- newTechnology("TT", output = list(comm = "ELC"),
    invcost = data.frame(year = c(2020L, 2030L), invcost = c(1000, 800)),
    fixom = data.frame(fixom = 20),
    vintage = data.frame(olife = 20L), cap2act = 1)
  expect_no_message(pt <- ggplot2::autoplot(tech))
  expect_s3_class(pt, "ggplot")
  imp <- newImport("IMP_GAS", commodity = "GAS",
                   imp = data.frame(year = 2025L, imp.up = 100))
  expect_no_message(pi <- ggplot2::autoplot(imp))
  expect_s3_class(pi, "ggplot")
})

test_that("the parameter registry is populated and aliased", {
  mp <- energyRt:::.param_interp_map()
  expect_gt(length(mp), 50L)
  cost <- mp[[paste("supply", "supply", "cost", sep = "\r")]]
  expect_identical(cost$param, "pSupCost")
  expect_identical(cost$rule, "back.inter.forth")
  expect_identical(cost$defVal, 0)
  ava <- mp[[paste("supply", "supply", "ava.up", sep = "\r")]]
  expect_identical(ava$param, "pSupAva")
  expect_identical(ava$defVal, Inf)
  # technology slots are mapped under their real names
  expect_false(is.null(mp[[paste("technology", "invcost", "invcost",
                                 sep = "\r")]]))
  cols <- energyRt:::.slot_param_cols("supply", "supply")
  expect_true(all(c("cost", "ava.lo", "ava.up") %in% names(cols)))
})

test_that("interpolation expands only GIVEN columns, never defaults", {
  sup <- newSupply("SUP_GAS", commodity = "GAS",
                   supply = data.frame(year = c(2020L, 2040L),
                                       cost = c(10, 20)))
  gd <- getData(sup, interpolate = TRUE, years = 2020:2040)
  d <- as.data.frame(gd[[1]])
  expect_true("cost" %in% names(d))
  # ava.* were not given: they must not materialise at their defaults
  for (cc in intersect(c("ava.lo", "ava.up", "ava.fx"), names(d)))
    expect_true(all(is.na(d[[cc]])))
})

test_that("NULL filters are ignored; real filters still work", {
  sup <- newSupply("SUP_GAS", commodity = "GAS",
                   supply = data.frame(year = c(2020L, 2040L),
                                       cost = c(10, 20)))
  d0 <- as.data.frame(getData(sup, year = NULL)[[1]])
  expect_equal(nrow(d0), 2L)                        # NULL = no filter
  d1 <- as.data.frame(getData(sup, year = 2020)[[1]])
  expect_equal(nrow(d1), 1L)                        # explicit filter kept
})

test_that("lever and demand plots honour interpolate", {
  tax <- newTax("CTAX", comm = "CO2",
                tax = data.frame(year = c(2025L, 2040L), bal = c(10, 50)))
  p1 <- ggplot2::autoplot(tax)
  expect_true("GeomLine" %in% .ap_geoms(p1))
  p2 <- ggplot2::autoplot(tax, interpolate = FALSE)
  expect_false("GeomLine" %in% .ap_geoms(p2))
  dem <- newDemand("DEM", commodity = "ELC",
                   demand = data.frame(year = c(2020L, 2030L),
                                       demand = c(1, 2)))
  expect_s3_class(ggplot2::autoplot(dem), "ggplot")
  expect_s3_class(ggplot2::autoplot(dem, interpolate = FALSE), "ggplot")
})

# ── unit labels on the process autoplots ──────────────────────────────────────
# These are the package's first assertions on a plot label. The facet strip
# carries the unit because each facet holds one parameter family
# (ava.lo/up/fx share a dimension) while the y scale is free.

.facet_labs <- function(p) {
  keys <- unique(as.character(ggplot2::ggplot_build(p)$layout$layout$base))
  as.character(unlist(p$facet$params$labeller(data.frame(base = keys))))
}

.units_supply <- function() {
  newSupply("SUP_COA", commodity = "COA", unit = "GWh",
            reserve = data.frame(region = "R1", res.up = 1e5),
            supply = data.frame(region = "R1", year = c(2020L, 2060L),
                                ava.up = c(1000, 2000), cost = c(50, 60)))
}

test_that("supply autoplot labels facets with the commodity unit", {
  skip_if_not_installed("ggplot2")
  labs <- .facet_labs(ggplot2::autoplot(.units_supply()))
  expect_true(any(grepl("ava [GWh/year]", labs, fixed = TRUE)))
  # the currency is unknown on a supply, so it stays a placeholder token
  expect_true(any(grepl("cost [{costs}/GWh]", labs, fixed = TRUE)))
})

test_that("`units =` resolves the tokens the object cannot know", {
  skip_if_not_installed("ggplot2")
  labs <- .facet_labs(
    ggplot2::autoplot(.units_supply(), units = c(costs = "cr.INR")))
  expect_true(any(grepl("cost [cr.INR/GWh]", labs, fixed = TRUE)))
  expect_true(any(grepl("ava [GWh/year]", labs, fixed = TRUE)))
})

test_that("technology autoplot resolves units from @units", {
  skip_if_not_installed("ggplot2")
  tec <- newTechnology(
    "EGAS", input = data.frame(comm = "GAS", unit = "GWh"),
    output = data.frame(comm = "ELC", unit = "GWh"),
    units = data.frame(capacity = "GW", activity = "GWh", costs = "MUSD"),
    invcost = data.frame(year = c(2020L, 2050L), invcost = c(800, 700)),
    fixom = data.frame(fixom = 20), varom = data.frame(varom = 2),
    capacity = data.frame(year = 2020L, stock = 1))
  labs <- .facet_labs(ggplot2::autoplot(tec))
  expect_true(any(grepl("invcost [MUSD/GW]", labs, fixed = TRUE)))
  expect_true(any(grepl("varom [MUSD/GWh]", labs, fixed = TRUE)))
  expect_true(any(grepl("stock [GW]", labs, fixed = TRUE)))
})

test_that("getUnits() covers the flow classes", {
  u <- getUnits(.units_supply())
  expect_s3_class(u, "energyRtUnits")
  expect_setequal(u$parameter, c("res.up", "ava.up", "cost"))
  expect_equal(u$unit[u$parameter == "ava.up"], "GWh/year")
  expect_equal(u$unit[u$parameter == "cost"], "{costs}/GWh")
  u2 <- getUnits(.units_supply(), units = c(costs = "cr.INR"))
  expect_equal(u2$unit[u2$parameter == "cost"], "cr.INR/GWh")
})
