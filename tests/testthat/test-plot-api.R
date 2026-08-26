# =========================================================================== #
# The plotting API's CONTRACT -- not its pixels.
#
# energyRt has three visual verbs and they mean different things:
#
#   draw()      schematic process diagrams, grid-based, needs no ggplot2
#   autoplot()  ggplot charts of an object's data -- the single implementation
#   plot()      delegates to autoplot(), so whichever verb a user reaches for
#               works
#
# These tests are written by REFLECTION over the package's own method tables and
# export list, so a class or function added later cannot silently drift out of
# the convention. A hardcoded list would pass forever while the package grew
# around it -- that is exactly the failure this file exists to prevent.
# =========================================================================== #

# Classes carrying an `autoplot` method, read from the namespace rather than
# listed here. `levcost`/`levcost_list` are S3 and live in R/levcost.R.
pa_autoplot_classes <- function(s4_only = TRUE) {
  nm <- grep("^autoplot\\.", ls(asNamespace("energyRt"), all.names = TRUE),
             value = TRUE)
  cl <- sub("^autoplot\\.", "", nm)
  if (s4_only) {
    cl <- cl[vapply(cl, methods::isClass, logical(1))]
    # setOldClass() bridges (e.g. scenarios_cmp) register VIRTUAL classes:
    # they are S3 underneath, live in their feature file rather than a
    # class-*.R, and their plot() symmetry is asserted separately.
    cl <- cl[!vapply(cl, methods::isVirtualClass, logical(1))]
  }
  sort(cl)
}

# --------------------------------------------------------------------------- #
# plot() / autoplot() symmetry

test_that("every class with an autoplot() method also has a plot() method", {
  cls <- pa_autoplot_classes()
  expect_gt(length(cls), 10L)          # the reflection actually found something
  missing <- cls[!vapply(cls, function(k)
    methods::existsMethod("plot", c(k, "ANY")), logical(1))]
  expect_equal(missing, character(0))
})

test_that("the plot generic is EXPORTED, not merely defined", {
  # Registering 17 methods is not enough: without `exportMethods(plot)` a user
  # calling `plot(x)` reaches base::plot, which is an S3 generic and never
  # dispatches them -- the methods exist and are unreachable. This is invisible
  # under devtools::load_all() (export_all = TRUE puts the generic in scope), so
  # it must be checked against NAMESPACE rather than the loaded environment.
  ns_file <- file.path(find.package("energyRt"), "NAMESPACE")
  skip_if_not(file.exists(ns_file), "NAMESPACE not found")
  ns <- readLines(ns_file, warn = FALSE)
  expect_true(any(grepl("^exportMethods\\(plot\\)", ns)))
  expect_true(any(grepl("^exportMethods\\(draw\\)", ns)))
})

test_that("classes with plot methods are collated BEFORE R/plot.R", {
  # `setMethod("plot", c(<class>, "ANY"), ...)` needs the class defined when the
  # file is sourced. Collate order comes from @include tags in R/plot.R; a
  # missing one makes R CMD INSTALL warn "no definition for class ..." and binds
  # the method to nothing. devtools::load_all() does not reproduce this.
  desc <- file.path(find.package("energyRt"), "DESCRIPTION")
  coll <- read.dcf(desc)[1, "Collate"]
  skip_if(is.na(coll), "no Collate field (source package not installed)")
  files <- gsub("'", "", strsplit(trimws(coll), "\\s+")[[1]])
  ip <- match("plot.R", files)
  expect_false(is.na(ip))

  # class -> file, since not every file is named after its class
  odd <- c(sub = "class-subsidy.R")
  cls <- pa_autoplot_classes()
  hit <- 0L
  for (k in cls) {
    f <- if (k %in% names(odd)) odd[[k]] else
      grep(sprintf("^class-.*%s\\.R$", k), files, value = TRUE)[1]
    expect_true(f %in% files, label = paste("collate entry for class", k))
    expect_lt(match(f, files), ip, label = paste("collate position of", f))
    hit <- hit + 1L
  }
  # every class is actually checked -- a silent `next` would make this vacuous
  expect_equal(hit, length(cls))
  expect_gt(hit, 15L)
})

test_that("plot() is a thin delegation, not a second implementation", {
  # Same object -> same class of result. If plot() ever grows its own rendering
  # this stops being true, which is the point.
  h <- newHorizon(2020:2030, intervals = 5L)
  expect_equal(class(plot(h)), class(ggplot2::autoplot(h)))
  cal <- calendars[["s4_h24"]]
  expect_equal(class(plot(cal)), class(ggplot2::autoplot(cal)))
})

test_that("draw() covers every process class", {
  procs <- c("technology", "storage", "supply", "demand", "export", "import",
             "trade")
  missing <- procs[!vapply(procs, function(k)
    methods::existsMethod("draw", k), logical(1))]
  expect_equal(missing, character(0))
})

# --------------------------------------------------------------------------- #
# one vocabulary

test_that("every exported plot_* and draw method takes `object` first", {
  # `plot()` is excluded on purpose: base R fixes its formals as (x, y, ...).
  # No exemptions: R/levcost.R was harmonized once its parallel rewrite landed.
  exported <- getNamespaceExports("energyRt")
  fns <- sort(grep("^plot_", exported, value = TRUE))
  expect_gt(length(fns), 2L)
  first <- vapply(fns, function(f) {
    fo <- names(formals(getExportedValue("energyRt", f)))
    if (length(fo)) fo[1] else NA_character_
  }, character(1))
  expect_equal(unname(first), rep("object", length(fns)),
               info = paste(fns, first, sep = "=", collapse = "; "))

  expect_equal(names(formals(energyRt::draw))[1], "object")
})

test_that("the S3 levcost classes reach plot() too", {
  # `levcost`/`levcost_list` are S3, so `setMethod("plot", ...)` cannot serve
  # them; they need S3 methods to satisfy the same contract as the 17 S4
  # classes. Checked by reflection over the S3 method table.
  s3 <- pa_autoplot_classes(s4_only = FALSE)
  expect_true(all(c("levcost", "levcost_list") %in% s3))
  for (k in c("levcost", "levcost_list")) {
    expect_false(is.null(getS3method("plot", k, optional = TRUE)), info = k)
  }
})

test_that("no plotting entry point still says `years`", {
  # `year` matches getData()/getMix() and the `year` set; `years` was the odd
  # one out in three functions.
  ns <- asNamespace("energyRt")
  fns <- c(grep("^plot_", getNamespaceExports("energyRt"), value = TRUE),
           grep("^(plot_|autoplot\\.)", ls(ns, all.names = TRUE), value = TRUE))
  bad <- Filter(function(f) {
    fo <- tryCatch(names(formals(get(f, envir = ns))), error = function(e) NULL)
    "years" %in% fo
  }, unique(fns))
  expect_equal(bad, character(0))
})

test_that("`type` means the quantity and `style` means the chart shape", {
  ns <- asNamespace("energyRt")
  # chart shape
  expect_true("style" %in% names(formals(get("plot_demand", envir = ns))))
  expect_true("style" %in% names(formals(get("plot_weather", envir = ns))))
  expect_false("type" %in% names(formals(get("plot_demand", envir = ns))))
  expect_false("type" %in% names(formals(get("plot_weather", envir = ns))))
  # quantity
  expect_true("type" %in% names(formals(getMix)))
  expect_true("type" %in% names(formals(plot_map)))
  expect_equal(eval(formals(getMix)$type)[1], "generation")
  expect_equal(eval(formals(plot_map)$type)[1], "generation")
})

# --------------------------------------------------------------------------- #
# export surface

test_that("a plot_* is exported only when no generic reaches it", {
  exported <- grep("^plot_", getNamespaceExports("energyRt"), value = TRUE)
  # these take raw data, or span classes no single autoplot method covers
  expect_true(all(c("plot_heatmap", "plot_map", "plot_trade_map") %in% exported))
  # these are fully reachable through autoplot() and must NOT be public --
  # two public names for one chart is what this pass removed
  expect_false(any(c("plot_demand", "plot_weather", "plot_process_windows",
                     "plot_calendar", "plot_commodity", "plot_horizon")
                   %in% exported))
})

test_that("geo_map is gone; geo_* is geoscales' prefix", {
  expect_false("geo_map" %in% getNamespaceExports("energyRt"))
  expect_true("plot_map" %in% getNamespaceExports("energyRt"))
})

# --------------------------------------------------------------------------- #
# ggplot2 is a Suggests

test_that("ggplot2 is declared in Suggests, not Imports", {
  # the premise of the guard test below; if ggplot2 ever moves to Imports the
  # guards become unnecessary and this should be revisited deliberately
  d <- read.dcf(system.file("DESCRIPTION", package = "energyRt"))
  expect_false(grepl("ggplot2", d[1, "Imports"]))
  expect_true(grepl("ggplot2", d[1, "Suggests"]))
})

test_that("scenario-level plots guard ggplot2 like the rest of R/plot.R", {
  # These four used to call `ggplot2::` with no guard, so a user without
  # ggplot2 got R's bare "there is no package called 'ggplot2'" and R CMD check
  # flagged a Suggests package used unconditionally.
  ns <- asNamespace("energyRt")
  for (f in c("autoplot.scenario", "plot_process_windows")) {
    src <- paste(deparse(get(f, envir = ns)), collapse = " ")
    expect_match(src, "check_package", info = f)
  }
})

# --------------------------------------------------------------------------- #
# smoke: every verb returns something of the right kind

test_that("draw/autoplot/plot all render without error", {
  tech <- newTechnology("T", input = list(comm = "COA"),
                        output = list(comm = "ELC"), cap2act = 1,
                        ceff = data.frame(comm = "COA", cinp2use = 0.4),
                        invcost = data.frame(invcost = 1000))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)

  grid::grid.newpage()
  expect_true(draw(tech))
  expect_gt(length(grid::grid.ls(print = FALSE)$name), 0L)

  cal <- calendars[["s4_h24"]]
  expect_s3_class(ggplot2::autoplot(cal), "ggplot")
  expect_s3_class(plot(cal), "ggplot")
})

test_that("plot_share_frontier builds panels for both branches", {
  # This had NO coverage at all, which is how a 13-line argument rename inside
  # it could pass a fully green suite. The column shape is read off the
  # plotting loop: `direction` picks the colour, `n_in_group == 2` takes the
  # single-diagonal branch and anything else the faceted one.
  mk <- function(n_in_group, direction = "input", group = "g1") {
    data.frame(direction = direction, group = group,
               comm = c("COA", "GAS"), others = "rest",
               share_lo = c(0.1, 0.3), share_hi = c(0.7, 0.9),
               n_in_group = n_in_group, stringsAsFactors = FALSE)
  }
  expect_null(plot_share_frontier(NULL))
  expect_null(plot_share_frontier(data.frame()))

  for (n in c(2L, 3L)) {
    p <- plot_share_frontier(mk(n))
    expect_s3_class(p, "share_frontier_plots")
    expect_length(p, 1L)
    expect_true(all(vapply(p, inherits, logical(1), "ggplot")))
  }
  # one panel per (direction, group)
  both <- plot_share_frontier(rbind(mk(2L, "input"), mk(2L, "output", "g2")))
  expect_length(both, 2L)

  # not `expect_silent`: without patchwork the print method falls back to
  # drawing the panels one by one and the list's attributes reach stdout
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_no_error(suppressWarnings(print(both)))
})

test_that("theme_energyRt is the one place the look is set", {
  expect_s3_class(theme_energyRt(), "theme")
  expect_equal(theme_energyRt(base_size = 14)$text$size, 14)
})
