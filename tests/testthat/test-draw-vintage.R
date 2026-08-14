# =========================================================================== #
# draw() for vintaged technologies.
#
# A vintaged technology is N technologies after expansion. The figure draws ONE
# in full and the others as faded, slightly smaller ghosts behind -- earlier
# vintages drifting left, later ones right -- so a glance shows how many
# vintages exist and whether the selected one is first, middle or last.
#
# The enabling change was making `draw_process()` composable: it used to call
# `grid.newpage()` unconditionally and end with `popViewport(0)`, which unwinds
# the WHOLE viewport stack including the caller's. Test 1 pins that, because
# every other feature here depends on it.
#
# These are structural checks on the grob tree, not image comparisons: they
# catch "nothing was drawn" and "the stack was corrupted", which is what
# actually regresses.
# =========================================================================== #

dv_plain <- function() {
  newTechnology("EPX", desc = "Plain plant",
                input = list(comm = "COA"), output = list(comm = "ELC"),
                cap2act = 1, ceff = data.frame(comm = "COA", cinp2use = 0.4),
                invcost = data.frame(invcost = 1000))
}

dv_vintaged <- function(n = 3) {
  v <- c("v2020", "v2030", "v2040")[seq_len(n)]
  newTechnology("EPP", desc = "Coal power plant",
                input = list(comm = "COA"), output = list(comm = "ELC"),
                cap2act = 1, ceff = data.frame(comm = "COA", cinp2use = 0.42),
                invcost = data.frame(invcost = 1000),
                vintage = data.frame(vintage = v,
                                     start = c(2020L, 2030L, 2040L)[seq_len(n)],
                                     end = c(2029L, 2039L, 2050L)[seq_len(n)],
                                     olife = 30L))
}

# Draw onto a null device and report the grob tree.
dv_grobs <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  force(expr)
  grid::grid.ls(print = FALSE)$name
}

# --------------------------------------------------------------------------- #

test_that("draw_process(newpage = FALSE) leaves the caller's viewport intact", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(name = "caller"))
  before <- grid::current.viewport()$name
  res <- draw_process(process_name = "P", newpage = FALSE)
  expect_true(res)
  # the old popViewport(0) unwound this too, making composition impossible
  expect_equal(grid::current.viewport()$name, before)
})

test_that("a technology with no vintages draws the plain figure", {
  # A fixed count, so an UNINTENDED change to the plain path shows up. Update
  # it deliberately when the figure really changes -- it last moved from 21 to
  # 23 when arrow labels gained their semi-transparent pad (one rect each).
  g <- dv_grobs(draw(dv_plain()))
  kinds <- table(sub("[.][0-9]+$", "", g))
  expect_equal(as.integer(kinds[["GRID.rect"]]), 4L)   # background, box, 2 pads
  expect_equal(as.integer(kinds[["GRID.lines"]]), 8L)  # arrows
  expect_equal(length(g), 23L)
})

test_that("a vintaged technology draws more as vintages are added", {
  one <- length(dv_grobs(draw(dv_plain())))
  n2  <- length(dv_grobs(draw(dv_vintaged(2))))
  n3  <- length(dv_grobs(draw(dv_vintaged(3))))

  # Monotonic only. Earlier versions of this test asserted an exact arithmetic
  # relationship between the counts and broke twice on cosmetic changes -- once
  # when ghost text was suppressed, once when stamps were added. What actually
  # matters is that every vintage contributes something and nothing is dropped.
  expect_gt(n2, one)
  expect_gt(n3, n2)
})

test_that("every selection mode draws, and the stack stays balanced", {
  for (v in list(NULL, "v2020", "v2030", "v2040", 1L, 3L, "all")) {
    grDevices::pdf(NULL)
    grid::grid.newpage()
    depth0 <- length(grid::current.vpPath())
    expect_silent(draw(dv_vintaged(3), vintage = v))
    expect_equal(length(grid::current.vpPath()), depth0,
                 info = paste("vintage =", if (is.null(v)) "NULL" else v))
    grDevices::dev.off()
  }
})

test_that("an unknown or out-of-range vintage is refused by name", {
  expect_error(dv_grobs(draw(dv_vintaged(3), vintage = "nope")),
               "Unknown vintage")
  expect_error(dv_grobs(draw(dv_vintaged(3), vintage = 9L)),
               "out of range")
})

test_that("the selected vintage is the last by default", {
  lv <- energyRt:::.variant_levels(dv_vintaged(3), "vintage")
  expect_equal(energyRt:::.resolve_vintage_pick(NULL, lv), 3L)
  expect_equal(energyRt:::.resolve_vintage_pick("v2020", lv), 1L)
  expect_equal(energyRt:::.resolve_vintage_pick(2L, lv), 2L)
})

test_that("variants are ordered oldest to newest, whatever the slot order", {
  # ordering drives which side a ghost drifts to, so it must not depend on the
  # order rows happen to appear in `@vintage`.
  t <- dv_vintaged(3)
  t@vintage <- t@vintage[c(3, 1, 2), , drop = FALSE]
  v <- energyRt:::.tech_variants(t)
  expect_equal(v$prov$vintage, c("v2020", "v2030", "v2040"))
})

test_that("the ghost stack stays inside the viewport however many vintages", {
  # The geometry parameters describe the WHOLE stack, not a per-step offset.
  # With a per-step offset, 30 annual vintages put the furthest ghost at
  # 0.5 - 0.075*29 = -1.68 npc, far off-canvas.
  drift <- 0.17
  for (n in c(2L, 3L, 10L, 30L, 60L)) {
    for (pick in unique(c(1L, (n + 1L) %/% 2L, n))) {
      far <- max(abs(seq_len(n) - pick))
      f <- abs(seq_len(n) - pick) / far
      x <- 0.5 + sign(seq_len(n) - pick) * drift * f
      expect_true(all(x >= 0 & x <= 1),
                  info = paste("n =", n, "pick =", pick))
      expect_equal(max(abs(x - 0.5)), drift, tolerance = 1e-9)
    }
  }
})

dv_xyzs <- function(n, pick, drift = 0.17, rise = 0, scale = 0.72) {
  s <- sign(seq_len(n) - pick)
  f <- abs(seq_len(n) - pick) / max(abs(seq_len(n) - pick))
  rbind(0.5 + s * drift * f, 0.5 + s * rise * f, 1 - (1 - scale) * f, s)
}

test_that("stamps thin out instead of overlapping, and always mark the ends", {
  n <- 30L; pick <- 15L
  xyzs <- dv_xyzs(n, pick)
  lv <- as.character(2021:2050)

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  keep <- energyRt:::.draw_vintage_stamps(lv, pick, xyzs)$keep

  # first, last and selected are never dropped
  expect_true(all(c(1L, pick, n) %in% keep))
  # 30 vintages must not produce 30 labels
  expect_lt(length(keep), n)
  # thinning is judged on box CENTRES, so it stays symmetric about the
  # selection -- a corner metric let the immediate neighbour through
  gaps <- diff(sort(xyzs[1, keep]))
  expect_true(all(gaps >= 0.075 - 1e-9))
  expect_false((pick + 1L) %in% keep)
})

test_that("stamps use the VIN set-name convention without doubling it", {
  # the level is the bare year; energyRt's variant prefix supplies "VIN"
  expect_equal(energyRt:::.vintage_stamp_label("2030"), "VIN2030")
  # a level that already carries the prefix is not stamped twice
  expect_equal(energyRt:::.vintage_stamp_label("VIN2030"), "VIN2030")
  expect_equal(energyRt:::.vintage_stamp_label(c("2020", "2030")),
               c("VIN2020", "VIN2030"))
})

test_that("ghost rise defaults to flat", {
  # with ghost arrows suppressed there is nothing for a vertical offset to
  # clear, so the stack is a flat fan unless asked otherwise
  expect_equal(ghost_options()$rise, 0)
})

test_that("ghosts draw no arrows", {
  # ghost arrows extend past the box on both sides and cross the selected
  # figure's own flows; only the selection should show arrows
  one <- length(dv_grobs(draw(dv_plain())))
  ghosted <- length(dv_grobs(draw(dv_vintaged(3))))
  ghost_cost <- (ghosted - (one - 1L) - 3L) / 2L   # minus selection, minus stamps
  expect_lt(ghost_cost, one - 1L)
})

test_that("a single-vintage technology takes the plain path", {
  # one vintage is not a sequence: no ghosts, no expansion
  expect_null(energyRt:::.tech_variants(dv_vintaged(1)))
  expect_null(energyRt:::.tech_variants(dv_plain()))
})
