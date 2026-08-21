# =========================================================================== #
# draw(): the open box edge, and arrow labels that clear their arrow.
#
# 1. A process with flows on only one side is drawn with NO BORDER on the
#    arrow-free edge, so the box reads as a flow entering or leaving the system
#    rather than a sealed container. The rule is derived from
#    `show_inputs`/`show_outputs`, not from the class name, so a new one-sided
#    process gets it for free.
#
# 2. The white pad behind an arrow label must not touch the arrow. It used to
#    be positioned by a device-INDEPENDENT constant (`font_in_npc()` returns
#    `font_spacing` whatever you pass it) against a device-DEPENDENT grob
#    height, so the clearance shrank as the figure grew and eventually went
#    negative. `.draw_arrow_label()` now measures the pad itself.
# =========================================================================== #

do_repo <- function() utopia_modules$electricity$R3$repo
do_obj  <- function(cl) {
  o <- getObject(do_repo(), class = cl, drop = FALSE)
  if (length(o) == 0L) NULL else o[[1]]
}

# Grob-type counts for one figure.
do_kinds <- function(expr) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  force(expr)
  table(sub("[.][0-9]+$", "", grid::grid.ls(print = FALSE)$name))
}
do_n <- function(k, what) if (what %in% names(k)) as.integer(k[[what]]) else 0L

# --------------------------------------------------------------------------- #
# 1. the open edge

test_that("one-sided processes lose the border on the arrow-free edge", {
  # The open edge is drawn as fill-with-no-border plus three segments, so its
  # signature is: a GRID.segments grob appears.
  for (cl in c("supply", "demand", "import", "export")) {
    o <- do_obj(cl)
    skip_if(is.null(o), paste("no", cl, "in the fixture repo"))
    k <- do_kinds(draw(o))
    expect_equal(do_n(k, "GRID.segments"), 1L, info = cl)
  }
})

test_that("two-sided processes keep the closed box", {
  # technology/storage/trade flow both ways, so nothing opens and the box stays
  # a single bordered rect -- no segments grob at all.
  for (cl in c("technology", "trade")) {
    o <- do_obj(cl)
    skip_if(is.null(o), paste("no", cl, "in the fixture repo"))
    k <- do_kinds(draw(o))
    expect_equal(do_n(k, "GRID.segments"), 0L, info = cl)
  }
})

test_that("box_open = 'none' restores the closed rectangle", {
  # the escape hatch. This also pins that the four one-sided methods FORWARD
  # `...` to draw_process() -- they used to accept it and silently drop it, so
  # no draw_process option could reach them at all.
  for (cl in c("supply", "demand", "import", "export")) {
    o <- do_obj(cl)
    skip_if(is.null(o), paste("no", cl, "in the fixture repo"))
    expect_equal(do_n(do_kinds(draw(o, box_open = "none")), "GRID.segments"),
                 0L, info = cl)
  }
})

test_that("the open side follows the flags, not the class name", {
  # supply and import are open on the LEFT (no inputs); demand and export on
  # the RIGHT (no outputs). Read off the segments' x coordinates: the three
  # drawn edges share one vertical, and it is the side that HAS the arrows.
  side_of <- function(o) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    grid::grid.newpage()
    draw(o)
    nm <- grep("^GRID.segments", grid::grid.ls(print = FALSE)$name, value = TRUE)
    g <- grid::grid.get(nm[1])
    x0 <- grid::convertX(g$x0, "npc", valueOnly = TRUE)
    x1 <- grid::convertX(g$x1, "npc", valueOnly = TRUE)
    vert <- which(abs(x0 - x1) < 1e-9)          # the one vertical edge drawn
    if (x0[vert] > 0.5) "right" else "left"
  }
  for (cl in c("supply", "import")) {
    o <- do_obj(cl); skip_if(is.null(o), cl)
    expect_equal(side_of(o), "right", info = paste(cl, "keeps its RIGHT edge"))
  }
  for (cl in c("demand", "export")) {
    o <- do_obj(cl); skip_if(is.null(o), cl)
    expect_equal(side_of(o), "left", info = paste(cl, "keeps its LEFT edge"))
  }
})

# --------------------------------------------------------------------------- #
# 2. arrow label clearance

test_that("the label pad clears the arrow by the same distance on any device", {
  o <- do_obj("supply")
  skip_if(is.null(o), "no supply in the fixture repo")

  gap_mm <- function(h_in) {
    grDevices::pdf(NULL, width = 7, height = h_in)
    on.exit(grDevices::dev.off(), add = TRUE)
    grid::grid.newpage()
    draw(o)
    nm <- grid::grid.ls(print = FALSE)$name
    y_line <- grid::convertY(
      grid::grid.get(grep("^GRID.lines", nm, value = TRUE)[1])$y[1],
      "mm", valueOnly = TRUE)
    pad <- NULL
    for (r in grep("^GRID.rect", nm, value = TRUE)) {
      gr <- grid::grid.get(r); f <- gr$gp$fill
      # the pad is the only semi-transparent white fill (#FFFFFFxx)
      if (!is.null(f) && length(f) == 1L && nchar(f) > 7L &&
          grepl("^#FFFFFF", toupper(f))) pad <- gr
    }
    if (is.null(pad)) return(NA_real_)
    grid::convertY(pad$y, "mm", valueOnly = TRUE) -
      grid::convertHeight(pad$height, "mm", valueOnly = TRUE) / 2 - y_line
  }

  g_small <- gap_mm(4.2)
  g_large <- gap_mm(12)
  expect_false(is.na(g_small))
  expect_gt(g_small, 0.5)                       # a real gap, not a hairline
  # the whole point: constant in PHYSICAL units, not shrinking with the figure
  expect_equal(g_small, g_large, tolerance = 1e-6)
})

# --------------------------------------------------------------------------- #
# 3. storage centre label

test_that("draw(storage) shows one duration value, not one per key column", {
  # `@duration` is a data.frame (vintage, cluster, region, year, duration) while
  # technology's `@cap2act` is a scalar, so `paste0("duration: ", object@duration)`
  # vectorised over COLUMNS and drew "duration: NA" four times before the value.
  o <- do_obj("storage")
  skip_if(is.null(o), "no storage in the fixture repo")

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  draw(o)
  nm <- grid::grid.ls(print = FALSE)$name
  labs <- vapply(grep("^GRID.text", nm, value = TRUE),
                 function(n) paste(grid::grid.get(n)$label, collapse = ""),
                 character(1))

  cap <- grep("duration", labs, value = TRUE)
  expect_length(cap, 1L)
  expect_false(grepl("NA", cap))
  # exactly one "duration:" inside that label, and no blank leading line from an
  # empty `stg_par$lab_par`
  expect_equal(lengths(regmatches(cap, gregexpr("duration:", cap)))[[1]], 1L)
  expect_false(startsWith(cap, "\n"))
})

test_that(".draw_arrow_label positions itself from the arrow's own y", {
  # the signature carries `y_line` (the arrow) rather than a pre-offset text
  # centre, which is what let the caller's constant drift out of step
  fo <- names(formals(energyRt:::.draw_arrow_label))
  expect_true("y_line" %in% fo)
  expect_false("y" %in% fo)
  expect_true("gap" %in% fo)
})
