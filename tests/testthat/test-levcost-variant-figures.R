# Variant figures: recovering the vintage x cluster x region grid from a
# levcost fan-out and putting one dimension on each axis.
#
# `autoplot.levcost_list()` only knows `tech`, so the fan-out could only ever be
# drawn as raw cell names (`PWR_VIN2030_CLT3`) along one axis -- which hides
# which dimension drives the spread. These guard the grid recovery, the choice
# of axis vs facet, and the one thing that is easy to get wrong: a levelized
# cost is a RATE, so a collapsed dimension must be averaged, never summed.

skip_if_not_installed("ggplot2")

.vf_long <- function(...) getFromNamespace(".levcost_variants_long", "energyRt")(...)
.vf_dims <- function(...) getFromNamespace(".levcost_variant_dims", "energyRt")(...)
.vf_plot <- function(...) getFromNamespace(".levcost_variant_plot", "energyRt")(...)
.vf_figs <- function(...) getFromNamespace(".levcost_report_dim_plots", "energyRt")(...)

# A synthetic long frame, so the plot builder can be tested without a solve.
.vf_df <- function(vintage = c("2020", "2030"), cluster = c("A", "B"),
                   region = "R1", value = NULL) {
  d <- expand.grid(vintage = vintage, cluster = cluster, region = region,
                   stringsAsFactors = FALSE)
  d$variant <- paste0("T_VIN", d$vintage, "_CL", d$cluster)
  d$component <- "invcost"
  d$value <- if (is.null(value)) seq_len(nrow(d)) * 10 else value
  d[, c("variant", "vintage", "cluster", "region", "component", "value")]
}

test_that("a collapsed dimension is averaged, not summed", {
  # two regions, one cell each: on a vintage axis the region dimension is
  # collapsed. Summing would report 30 for a technology that costs 10 or 20.
  df <- .vf_df(vintage = "2020", cluster = "A", region = c("R1", "R2"),
               value = c(10, 20))
  p  <- .vf_plot(df, x = "vintage")
  ld <- ggplot2::layer_data(p)

  expect_equal(sum(ld$y), 15)
  expect_match(p$labels$caption, "averaged over")
  expect_match(p$labels$caption, "region")
})

test_that("nothing is averaged when x and facet span the grid", {
  df <- .vf_df()                        # 2 vintages x 2 clusters, one region
  p  <- .vf_plot(df, x = "cluster", facet = "vintage")
  ld <- ggplot2::layer_data(p)

  expect_equal(sort(ld$y), sort(df$value))     # every cell kept as-is
  expect_null(p$labels$caption)
})

test_that("x and facet place the dimensions as asked", {
  df <- .vf_df(vintage = c("2020", "2030", "2040"), cluster = c("A", "B"))

  p1 <- .vf_plot(df, x = "cluster", facet = "vintage")
  expect_equal(attr(p1, "n_x"), 2L)
  expect_equal(attr(p1, "n_panels"), 3L)
  expect_equal(p1$labels$x, "Cluster")

  p2 <- .vf_plot(df, x = "vintage", facet = "cluster")
  expect_equal(attr(p2, "n_x"), 3L)
  expect_equal(attr(p2, "n_panels"), 2L)
  expect_equal(p2$labels$x, "Vintage")

  p3 <- .vf_plot(df, x = "variant")            # all cells on one chart
  expect_equal(attr(p3, "n_x"), 6L)
  expect_s3_class(p3$facet, "FacetNull")
})

test_that("panels are capped and the caption says so", {
  df <- .vf_df(vintage = as.character(2001:2020), cluster = "A")
  p  <- .vf_plot(df, x = "cluster", facet = "vintage")

  expect_equal(attr(p, "n_panels"), 16L)       # .facet_cap()'s default
  expect_match(p$labels$caption, "16 of 20 panels")
})

test_that("x labels rotate once they would collide", {
  flat <- .vf_plot(.vf_df(vintage = "2020", cluster = c("A", "B")), x = "cluster")
  many <- .vf_plot(.vf_df(vintage = as.character(2001:2010), cluster = "A"),
                   x = "vintage")
  # a two-category axis stays horizontal; ten do not
  expect_equal(flat$scales$get_scales("x")$guide$params$angle, 0)
  expect_equal(many$scales$get_scales("x")$guide$params$angle, 45)
})

# ── Against a real fan-out ───────────────────────────────────────────────────

vf_tech <- function(regions = "R1") {
  g <- expand.grid(vintage = c("2020", "2030"), cluster = c("A", "B"),
                   region = regions, stringsAsFactors = FALSE)
  g$invcost <- rep(c(300, 200, 330, 220), length(regions)) +
    rep(seq_along(regions) * 10, each = 4)
  newTechnology(
    name = "PWR",
    input = data.frame(comm = "COA", unit = "PJ"),
    output = data.frame(comm = "ELC", unit = "PJ"),
    ceff = data.frame(comm = c("COA", "ELC"), cinp2use = c(1, NA),
                      use2cact = c(NA, 0.4)),
    cap2act = 1, region = regions,
    vintage = data.frame(vintage = c("2020", "2030"), olife = 20L),
    cluster = data.frame(cluster = c("A", "B")),
    invcost = g, fixom = list(fixom = 10))
}

vf_repo <- function(regions = "R1")
  newRepository("vf", newCommodity("COA", timeframe = "ANNUAL"),
                newCommodity("ELC", timeframe = "ANNUAL"),
                newSupply("SUP_COA", commodity = "COA",
                          supply = data.frame(region = regions, cost = 1)))

vf_run <- function(regions = "R1", ...)
  suppressMessages(suppressWarnings(levcost(
    vf_tech(regions), repo = vf_repo(regions), horizon = newHorizon(2020:2060),
    calendar = newCalendar(), method = "analytic", verbose = FALSE,
    cache = FALSE, ...)))

test_that("the grid is recovered from the fan-out's own provenance", {
  lc <- vf_run(region = "R1")
  df <- .vf_long(lc)

  expect_true(is.data.frame(df))
  expect_true(all(c("variant", "vintage", "cluster", "region",
                    "component", "value") %in% names(df)))
  expect_setequal(unique(df$vintage), c("2020", "2030"))
  expect_setequal(unique(df$cluster), c("A", "B"))
  # the REAL region, never the artificial `R1_VIN2020_CLA` the cells are priced in
  expect_equal(unique(df$region), "R1")
  expect_false(any(grepl("_VIN", df$region)))
})

test_that("only varying dimensions are reported", {
  df <- .vf_long(vf_run(region = "R1"))
  # one region, so region distinguishes nothing and must not become a figure
  expect_setequal(.vf_dims(df), c("vintage", "cluster"))
})

# Covers the R API: autoplot.levcost_variants()
test_that("autoplot() on a fan-out takes x and facet", {
  lc <- vf_run(region = "R1")

  p <- ggplot2::autoplot(lc, x = "cluster", facet = "vintage")
  expect_s3_class(p, "ggplot")
  expect_equal(attr(p, "n_x"), 2L)
  expect_equal(attr(p, "n_panels"), 2L)

  # the default puts every cell on one axis, as the list method used to
  expect_equal(attr(ggplot2::autoplot(lc), "n_x"), 4L)
  # a non-npv type still falls through to the levcost_list method
  expect_silent(q <- ggplot2::autoplot(lc, type = "npv", x = "vintage"))
  expect_s3_class(q, "ggplot")
})

test_that("a by_region fan-out is flattened, and region becomes a dimension", {
  lc <- vf_run(regions = c("R1", "R2"), by_region = TRUE)
  expect_s3_class(lc, "levcost_list")
  expect_false(inherits(lc, "levcost_variants"))   # nested: region -> fan-out

  df <- .vf_long(lc)
  expect_setequal(unique(df$region), c("R1", "R2"))
  expect_setequal(.vf_dims(df), c("vintage", "cluster", "region"))
  expect_equal(nrow(unique(df[, c("vintage", "cluster", "region")])), 8L)
})

test_that("report() asks for one figure per varying dimension", {
  # a single region: vintage and cluster vary, region does not
  f1 <- .vf_figs(vf_run(region = "R1"))
  expect_setequal(names(f1), c("Clusters, by vintage", "Vintages, by cluster"))

  # two regions: region joins the facets and gets an axis of its own
  f2 <- .vf_figs(vf_run(regions = c("R1", "R2"), by_region = TRUE))
  expect_true("Regions" %in% names(f2))
  expect_length(f2, 3L)
  expect_true(all(vapply(f2, inherits, logical(1), "ggplot")))
})
