# =========================================================================== #
# draw() for CLUSTERED technologies -- and for technologies carrying both axes.
#
# Vintages and clusters are drawn differently on purpose. A vintage is the same
# plant at a different build year: mutually exclusive selves, which is what the
# fan of faded past/future ghosts says. A cluster is a parallel sub-process that
# coexists with its siblings in the same year. Fanning those out would say the
# wrong thing, and would swamp the figure -- the real IDEEA wind technology is
# 11 clusters x 4 vintages = 44 cells. So the cluster axis is drawn as an INDEX
# (a tick rail, or a deck edge) and only the vintage marginal is fanned.
#
# The regression these tests exist for: `draw()` used to take the expanded cross
# product as a flat "vintage sequence", so a 4 x 11 technology drew 44 boxes,
# stamped "VIN2020" eleven times over, and made 10 of every 11 cells unreachable
# by name.
# =========================================================================== #

dc_tech <- function(nv = 3L, nc = 4L) {
  cl <- sprintf("%02d", seq_len(nc))
  v <- c("2020", "2030", "2040", "2050")[seq_len(nv)]
  newTechnology(
    "EWX", desc = "Clustered plant",
    input = list(comm = "REN"), output = list(comm = "ELC"),
    cap2act = 1,
    ceff = data.frame(comm = "ELC", cact2cout = 1),
    cluster = data.frame(cluster = cl, desc = paste("cluster", cl),
                         order = seq_len(nc)),
    vintage = data.frame(vintage = v,
                         start = as.integer(v),
                         end = as.integer(v) + 9L,
                         olife = 30L),
    invcost = data.frame(vintage = v, invcost = seq(1000, by = -50,
                                                    length.out = nv))
  )
}

# Draw onto a null device and report the grob tree (mirrors test-draw-vintage.R).
dc_grobs <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  force(expr)
  grid::grid.ls(print = FALSE)$name
}

# --------------------------------------------------------------------------- #

test_that("both axes survive expansion, in declared order", {
  v <- energyRt:::.tech_variants(dc_tech(3L, 4L))
  expect_equal(length(v$objects), 12L)
  expect_equal(v$vintages, c("2020", "2030", "2040"))
  expect_equal(v$clusters, c("01", "02", "03", "04"))
  # cells ordered vintage-major, so the ghost fan reads oldest to newest
  expect_equal(v$prov$name[1:2], c("EWX_VIN2020_CL01", "EWX_VIN2020_CL02"))
})

test_that("a declared cluster ORDER is respected, not alphabetical", {
  t <- dc_tech(2L, 3L)
  # rank them worst-to-best so `order` and the label disagree
  t@cluster$order <- c(3L, 2L, 1L)
  v <- energyRt:::.tech_variants(t)
  expect_equal(v$clusters, c("03", "02", "01"))
  # ... and the default selection follows the ranking, not the name
  cell <- energyRt:::.resolve_cell(v)
  expect_equal(cell$cluster, "03")
})

test_that("REGRESSION: the ghost fan is the vintage marginal, not the product", {
  # The defect: 3 x 4 = 12 cells were fanned as a 12-long vintage sequence.
  # A clustered technology must draw exactly as many boxes as a plain vintaged
  # one with the same number of vintages -- the clusters cost a rail, not boxes.
  plain <- dc_grobs(draw(dc_tech(3L, 1L), cluster_style = "none"))
  clust <- dc_grobs(draw(dc_tech(3L, 4L), cluster_style = "none"))
  expect_equal(length(clust), length(plain))

  # and adding clusters must not add boxes, whereas adding vintages must
  more_cl <- dc_grobs(draw(dc_tech(3L, 9L), cluster_style = "none"))
  expect_equal(length(more_cl), length(clust))
  more_vin <- dc_grobs(draw(dc_tech(4L, 4L), cluster_style = "none"))
  expect_gt(length(more_vin), length(clust))
})

test_that("every cell is reachable by name", {
  v <- energyRt:::.tech_variants(dc_tech(3L, 4L))
  # selecting a vintage used to return the FIRST row of the cross product
  # carrying it, leaving the other three clusters of that vintage unreachable
  for (vin in v$vintages) {
    for (cl in v$clusters) {
      cell <- energyRt:::.resolve_cell(v, vin, cl)
      expect_equal(v$prov$name[cell$index],
                   paste0("EWX_VIN", vin, "_CL", cl))
    }
  }
})

test_that("the defaults are the newest vintage and the first cluster", {
  v <- energyRt:::.tech_variants(dc_tech(3L, 4L))
  cell <- energyRt:::.resolve_cell(v)
  expect_equal(cell$vintage, "2040")   # newest: the one being built now
  expect_equal(cell$cluster, "01")     # declarations rank clusters best-first
  expect_equal(v$prov$name[cell$index], "EWX_VIN2040_CL01")
})

test_that("an unknown cluster is refused, listing the real ones", {
  err <- expect_error(dc_grobs(draw(dc_tech(3L, 4L), cluster = "99")),
                      "Unknown cluster")
  expect_match(conditionMessage(err), "01, 02, 03, 04")
  expect_error(dc_grobs(draw(dc_tech(3L, 4L), cluster = 9L)), "out of range")
})

test_that("a single-cluster technology takes the plain path", {
  # one cluster is not a choice; it must not cost a rail
  expect_null(energyRt:::.tech_variants(dc_tech(1L, 1L)))
  one <- dc_grobs(draw(dc_tech(3L, 1L)))
  none <- dc_grobs(draw(dc_tech(3L, 1L), cluster_style = "none"))
  expect_equal(length(one), length(none))
})

test_that("the rail draws one tick per cluster and marks exactly one", {
  # counted on the rail alone rather than on the whole figure, so this does not
  # move when the surrounding drawing changes
  rail <- function(n, pick) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    grid::grid.newpage()
    energyRt:::.draw_cluster_rail(sprintf("%02d", seq_len(n)), pick, y = 0.1)
    grid::grid.ls(print = FALSE)$name
  }
  expect_equal(sum(grepl("rect", rail(4L, 1L))), 4L)
  expect_equal(sum(grepl("rect", rail(11L, 6L))), 11L)
  # the caption names the selection and its position
  expect_equal(sum(grepl("text", rail(11L, 6L))), 1L)
})

test_that("the rail stays inside the viewport however many clusters", {
  # the band is capped, so 50 clusters pack tighter rather than running off-page
  for (n in c(2L, 4L, 11L, 50L)) {
    span <- min(0.34, 0.03 * n)
    x <- seq(0.5 - span / 2, 0.5 + span / 2, length.out = n)
    expect_true(all(x > 0 & x < 1), info = paste("n =", n))
  }
})

test_that("cluster stamps use the CL set-name convention without doubling it", {
  expect_equal(energyRt:::.cluster_stamp_label("03"), "CL03")
  # a level someone already wrote with the prefix is not stamped CLCL03 --
  # which is exactly what a literal "CL03" level produced in the set names
  expect_equal(energyRt:::.cluster_stamp_label("CL03"), "CL03")
  expect_equal(energyRt:::.cluster_stamp_label(c("01", "02")), c("CL01", "CL02"))
})

test_that("the cluster axis clears the vintage stamps", {
  # anchored below the lowest stamp, and floored so a wide box cannot push it
  # off the bottom of the page
  expect_lt(energyRt:::.cluster_axis_y(below = 0.17), 0.17)
  expect_gte(energyRt:::.cluster_axis_y(below = 0.0), 0.04)
  expect_gte(energyRt:::.cluster_axis_y(below = -5), 0.04)
  # no stamps to clear (single vintage): still on the page
  y <- energyRt:::.cluster_axis_y(NULL)
  expect_true(y > 0 && y < 0.5)
})

test_that("every style and selection draws, and the stack stays balanced", {
  for (st in c("rail", "deck", "none")) {
    for (cl in list(NULL, "02", 3L)) {
      grDevices::pdf(NULL)
      grid::grid.newpage()
      depth0 <- length(grid::current.vpPath())
      expect_silent(draw(dc_tech(3L, 4L), cluster = cl, cluster_style = st))
      expect_equal(length(grid::current.vpPath()), depth0,
                   info = paste(st, if (is.null(cl)) "NULL" else cl))
      grDevices::dev.off()
    }
  }
})

test_that("the deck is cheaper than the rail and both beat drawing boxes", {
  none <- length(dc_grobs(draw(dc_tech(3L, 11L), cluster_style = "none")))
  deck <- length(dc_grobs(draw(dc_tech(3L, 11L), cluster_style = "deck")))
  rail <- length(dc_grobs(draw(dc_tech(3L, 11L), cluster_style = "rail")))
  expect_gt(deck, none)
  expect_gt(rail, deck)          # 11 ticks vs 4 capped slivers
  # the deck is capped: 11 clusters must not mean 11 slivers
  expect_lt(deck - none, 11L)
})

test_that("facets pin the axis that was not asked for", {
  # `cluster = "all"` alone means every cluster of ONE vintage, not the whole
  # cross product -- otherwise the two arguments could not be combined
  t <- dc_tech(3L, 4L)
  expect_silent(dc_grobs(draw(t, cluster = "all")))
  expect_silent(dc_grobs(draw(t, vintage = "all")))
  expect_silent(dc_grobs(draw(t, vintage = "all", cluster = "all")))
})

test_that("an unreadable facet grid is refused rather than drawn", {
  # 44 panels drawn silently reads as "here is your figure"; it is not one
  expect_error(
    dc_grobs(draw(dc_tech(4L, 11L), vintage = "all", cluster = "all")),
    "exceeds `max_facets`"
  )
  expect_silent(
    dc_grobs(draw(dc_tech(4L, 11L), vintage = "all", cluster = "all",
                  max_facets = 100L))
  )
})

# --------------------------------------------------------------------------- #
# ghost_options()

test_that("ghost_options() carries the documented defaults", {
  g <- ghost_options()
  expect_s3_class(g, "ghost_options")
  expect_equal(names(g), c("alpha", "alpha_min", "scale", "drift", "rise",
                           "stamp"))
  expect_equal(g$drift, 0.17)
  expect_equal(g$rise, 0)
})

test_that("a plain list of overrides is merged onto the defaults", {
  g <- energyRt:::.as_ghost_options(list(drift = 0.25))
  expect_equal(g$drift, 0.25)
  expect_equal(g$scale, ghost_options()$scale)   # untouched
  expect_s3_class(g, "ghost_options")
  expect_equal(energyRt:::.as_ghost_options(NULL), ghost_options())
  expect_equal(energyRt:::.as_ghost_options(list()), ghost_options())
})

test_that("a misspelled ghost option is an error, not a silent no-op", {
  # the whole reason for the constructor: as flat arguments, `ghost_drfit` fell
  # into `...` and was forwarded downstream, changing nothing and saying nothing
  expect_error(energyRt:::.as_ghost_options(list(dirft = 0.2)),
               "Unknown ghost option")
  expect_error(energyRt:::.as_ghost_options(list(0.2)), "must be NAMED")
  expect_error(energyRt:::.as_ghost_options("wide"), "named list")
  expect_error(draw(dc_tech(2L, 2L), ghost = list(drfit = 1)),
               "Unknown ghost option")
})

test_that("ghost geometry actually reaches the figure", {
  flat <- dc_grobs(draw(dc_tech(3L, 2L), ghost = ghost_options(stamp = FALSE)))
  with_stamps <- dc_grobs(draw(dc_tech(3L, 2L), ghost = ghost_options()))
  expect_gt(length(with_stamps), length(flat))
})
