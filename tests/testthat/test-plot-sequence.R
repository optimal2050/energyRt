# The coverage plots. What is worth asserting is not that a ggplot came back
# but that the drawing is faithful to the frame: an element no unit covered is
# an NA fill rather than an absent row, a shared element is marked rather than
# resolved to its first claimant, and the regions land on a geoframe that
# contains them.

skip_if_not_installed("ggplot2")

# A by_sample shell carrying just what the plots read: the spec and the
# calendar. Blocks of whole days, as `sequential` sampling records them.
cov_bs <- function(cal_name = "d365_h24", n = 3L, keep = NULL) {
  cal <- energyRt::calendars[[cal_name]]
  tt <- as.data.frame(cal@timetable)
  # block on whole days where the calendar has them -- that is what a
  # `sequential` sample is -- and on the leaves themselves where it has not,
  # so the annual and season-level calendars can be built here too
  dayc <- intersect(c("DAY", "YDAY", "MDAY"), names(tt))
  grp <- if (length(dayc)) as.character(tt[[dayc[1]]]) else
    as.character(tt$timeslice)
  keys <- unique(grp)
  idx <- if (length(keys) >= n) as.integer(cut(seq_along(keys), n,
                                               labels = FALSE))
         else rep_len(seq_len(n), length(keys))
  blocks <- split(keys, idx)
  spec <- do.call(rbind, lapply(seq_along(blocks), function(i) {
    data.frame(sample = sprintf("s%02d", i),
               timeslice = as.character(tt$timeslice)[grp %in% blocks[[i]]],
               count = 1L, stringsAsFactors = FALSE)
  }))
  if (!is.null(keep)) spec <- spec[spec$sample %in% keep, , drop = FALSE]
  structure(list(name = "cov", model = NULL, scenarios = list(),
                 samples = spec, method = "sequential", calendar = cal),
            class = "by_sample")
}

cov_br <- function(region, regions) {
  structure(list(name = "cov", model = NULL,
                 runs = tibble::tibble(region = region, regions = regions)),
            class = "by_region")
}

fill_of <- function(p) ggplot2::ggplot_build(p)$data[[1]]$fill

test_that("a calendar's uncovered timeslices are drawn, not dropped", {
  full <- cov_bs()
  part <- cov_bs(keep = c("s01", "s03"))
  # the frame shrinks with coverage; the plot does not -- it is the calendar
  expect_lt(nrow(sequence_coverage(part)), nrow(sequence_coverage(full)))
  d_full <- ggplot2::ggplot_build(ggplot2::autoplot(full))$data[[1]]
  d_part <- ggplot2::ggplot_build(ggplot2::autoplot(part))$data[[1]]
  expect_identical(nrow(d_part), nrow(d_full))
  # and the gap is an NA fill
  expect_true(any(is.na(d_part$fill) | d_part$fill == "grey90"))
  expect_false(any(is.na(d_full$fill) | d_full$fill == "grey90"))
})

test_that("the subtitle counts coverage, overlap and multiplicity", {
  bs <- cov_bs()
  # the leaves, not @timeslice_share, which counts every level of the nesting
  expect_match(ggplot2::autoplot(bs)$labels$subtitle, "3 units")
  expect_match(ggplot2::autoplot(bs)$labels$subtitle,
               "8,760 of 8,760 timeslices covered")

  shared <- bs
  extra <- bs$samples[1:40, ]
  extra$sample <- "s04"
  shared$samples <- rbind(bs$samples, extra)
  expect_match(ggplot2::autoplot(shared)$labels$subtitle,
               "40 claimed by more than one unit")

  boot <- bs
  boot$samples$count[1:20] <- 2L
  expect_match(ggplot2::autoplot(boot)$labels$subtitle, "20 drawn more than once")
})

test_that("a timeslice two samples drew is marked, not given to the first", {
  bs <- cov_bs()
  extra <- bs$samples[1:40, ]
  extra$sample <- "s04"
  bs$samples <- rbind(bs$samples, extra)
  cv <- sequence_coverage(bs)
  byk <- energyRt:::.cov_by_key(cv)
  expect_identical(sum(byk$n_units > 1L), 40L)
  expect_true(all(byk$unit[byk$n_units > 1L] == "(shared)"))
  # "(shared)" is a level of the fill, so it reaches the legend
  expect_true("(shared)" %in% energyRt:::.cov_unit_levels(cv))
})

test_that("the grid axes are the calendar's two finest timeframes", {
  # m12_d365_h24 nests MONTH > YDAY > HOUR. The finest frame carries the FEWEST
  # labels here (24 hours under 365 days), so the axes have to come from the
  # nesting order and not from how many labels each frame has.
  p <- ggplot2::autoplot(cov_bs("m12_d365_h24"))
  expect_identical(p$labels$x, "HOUR")
  expect_identical(p$labels$y, "YDAY")
  # one cell per leaf timeslice, whatever was covered
  expect_identical(nrow(ggplot2::ggplot_build(p)$data[[1]]), 8760L)
})

test_that("a one-timeframe calendar still draws", {
  expect_s3_class(ggplot2::autoplot(cov_bs("d365")), "ggplot")
})

test_that("the wall needs a day timeframe and says so when it has none", {
  expect_s3_class(ggplot2::autoplot(cov_bs("d365_h24"), "wall"), "ggplot")
  expect_error(ggplot2::autoplot(cov_bs("s4_h24"), "wall"),
               "day-resolution timeframe")
})

test_that("the wall rolls each unit up to whole days", {
  p <- ggplot2::autoplot(cov_bs("d365_h24"), "wall")
  # 365 day cells, not 8,760 slices
  expect_match(p$labels$subtitle, "365 of 365 days covered")
  expect_identical(nrow(ggplot2::ggplot_build(p)$data[[1]]), 365L)
})

test_that("an annual calendar is refused for what it is", {
  expect_error(ggplot2::autoplot(cov_bs("annual")), "annual")
})

test_that("a by_region sequence without geometry lists its regions", {
  p <- ggplot2::autoplot(cov_br(c("u1", "u2"), list(c("R1", "R2"), "R3")))
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "no geometry")
  expect_match(p$labels$subtitle, "3 of 3 regions")
})

test_that("regions are mapped on a geoframe that contains them", {
  skip_if_not_installed("sf")
  skip_if_not_installed("geoscales")
  sq <- function(x, y) sf::st_polygon(list(cbind(
    c(x, x + 1, x + 1, x, x), c(y, y, y + 1, y + 1, y))))
  gs <- geoscales::geoscale_from_leaftable(
    data.frame(country = c("N", "N", "N", "N", "S", "S"),
               state = c("N1", "N1", "N2", "N2", "S1", "S1"),
               atom = paste0("A", 1:6), km2 = 1),
    geoframes = c("country", "state", "atom"), name = "sq")
  gs <- geoscales::attach_geometry_geoscale(
    gs, sf::st_sfc(sq(0, 1), sq(0, 0), sq(1, 1), sq(1, 0), sq(2, 1), sq(2, 0),
                   crs = 4326),
    geoframe = "atom")

  # the model's regions need not be the finest frame: picking the finest would
  # leave every key unmatched and the map uniformly grey
  expect_identical(energyRt:::.cov_geoframe(gs, c("N1", "N2", "S1")), "state")
  expect_identical(energyRt:::.cov_geoframe(gs, c("A1", "A2")), "atom")
  expect_null(energyRt:::.cov_geoframe(gs, c("A1", "ZZ")))

  p <- ggplot2::autoplot(cov_br(c("u1", "u2"), list(c("N1", "N2"), "S1")),
                geoscale = gs)
  expect_true(all(!is.na(fill_of(p))))

  # an uncovered region keeps the NA fill
  p2 <- ggplot2::autoplot(cov_br(c("u1", "u2"), list(c("A1", "A2"), c("A3", "A4"))),
                 geoscale = gs)
  expect_identical(sum(fill_of(p2) == "grey90"), 2L)
  expect_match(p2$labels$subtitle, "4 of 6 regions")

  # a level the regions are not members of is a blank map, so it warns
  expect_warning(ggplot2::autoplot(cov_br("u1", list(c("A1", "A2"))), geoscale = gs,
                          level = "country"),
                 "No region of the sequence")
})

test_that("a recursive step's look-ahead is drawn apart from its decisions", {
  rc <- structure(list(steps = tibble::tibble(
    step = 1:3, decided = list(2020L, 2025L, 2030L),
    lookahead = list(2025L, 2030L, integer(0)))), class = "recursive")
  p <- ggplot2::autoplot(rc)
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "2 look-ahead year\\(s\\)")
  # two fills, and the decided one is not the look-ahead one
  d <- ggplot2::ggplot_build(p)$data[[1]]
  expect_identical(length(unique(d$fill)), 2L)
  # 3 decided + 2 look-ahead cells
  expect_identical(nrow(d), 5L)
})

test_that("a myopic walk, with no look-ahead, says nothing about one", {
  rc <- structure(list(steps = tibble::tibble(
    step = 1:3, decided = list(2020L, 2025L, 2030L))), class = "recursive")
  p <- ggplot2::autoplot(rc)
  expect_false(grepl("look-ahead", p$labels$subtitle))
  expect_identical(nrow(ggplot2::ggplot_build(p)$data[[1]]), 3L)
})

test_that("a guided ladder draws each stage's years", {
  gd <- structure(list(stages = tibble::tibble(
    stage = c("a", "b"),
    years = list(c(2020L, 2025L), c(2020L, 2025L, 2030L)))), class = "guided")
  p <- ggplot2::autoplot(gd)
  expect_match(p$labels$subtitle, "2 stages")
  expect_identical(nrow(ggplot2::ggplot_build(p)$data[[1]]), 5L)
})

test_that("an empty sequence is refused rather than drawn blank", {
  bs <- cov_bs()
  bs$samples <- bs$samples[0, ]
  expect_error(ggplot2::autoplot(bs), "no coverage to draw")
  expect_error(
    ggplot2::autoplot(structure(list(runs = NULL), class = "by_region")),
    "no coverage to draw")
})

# --- histograms of a sampled result ----------------------------------------- #
#
# The weighting is the claim worth pinning: `sample_summary()` weights its mean
# by coverage, and a histogram drawn beside it that did not would make the two
# disagree about the same ensemble.

hist_bs <- function(obj = c(100, 200), yf = c(0.5, 0.5), pass = NULL) {
  n <- length(obj)
  s <- sprintf("s%02d", seq_len(n))
  structure(list(
    name = "h", scenarios = list(),
    ncap_floor = if (is.null(pass)) 0 else 0.9,
    runs = tibble::tibble(sample = s, year_fraction = yf, status = "solved",
                          objective = obj, pass = pass %||% rep("free", n))),
    class = "by_sample")
}

vline_at <- function(p) {
  b <- ggplot2::ggplot_build(p)
  d <- b$data[[length(b$data)]]
  sort(unique(d$xintercept))
}

test_that("the histogram's mean is weighted by each sample's coverage", {
  # one sample covers four fifths of the year, the other one fifth
  bs <- hist_bs(obj = c(100, 200), yf = c(0.8, 0.2))
  v <- energyRt:::.by_sample_values(bs, "objective")
  expect_identical(v$w, c(0.8, 0.2))
  expect_equal(vline_at(ggplot2::autoplot(bs, "histogram")), 120)
  # the unweighted mean would be 150: the weighting is doing work
  expect_false(isTRUE(all.equal(vline_at(ggplot2::autoplot(bs, "histogram")),
                                150)))
})

test_that("equal coverage leaves the mean unweighted", {
  expect_equal(vline_at(ggplot2::autoplot(hist_bs(), "histogram")), 150)
})

test_that("a floored ensemble draws the free pass against the floored one", {
  bs <- hist_bs(obj = c(100, 200, 110, 200), yf = rep(0.5, 4),
                pass = c("free", "free", "floor", "floor"))
  v <- energyRt:::.by_sample_values(bs, "objective")
  expect_setequal(unique(v$pass), c("free", "floor"))
  p <- ggplot2::autoplot(bs, "histogram")
  expect_match(p$labels$subtitle, "free pass against the floored one")
  expect_identical(length(vline_at(p)), 2L)
})

test_that("without a floor there is one pass and no overlay", {
  p <- ggplot2::autoplot(hist_bs(), "histogram")
  expect_false(grepl("free pass against", p$labels$subtitle))
  expect_identical(length(vline_at(p)), 1L)
})

test_that("an unsolved ensemble has no objective to draw", {
  bs <- hist_bs()
  bs$runs$objective <- NA_real_
  expect_error(ggplot2::autoplot(bs, "histogram"), "carries an objective")
})

test_that("only by_sample has a histogram", {
  expect_error(
    ggplot2::autoplot(cov_br("u1", list("R1")), type = "histogram"),
    "should be")
  rc <- structure(list(steps = tibble::tibble(
    step = 1L, decided = list(2020L))), class = "recursive")
  expect_error(ggplot2::autoplot(rc, type = "histogram"), "should be")
})
