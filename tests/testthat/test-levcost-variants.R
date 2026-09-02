# =========================================================================== #
# levcost() for technologies declaring vintages / clusters.
#
# The regression being guarded: the LCOE mini-model prices ONE process against a
# unit demand, but `interpolate_model()` expands a vintaged technology into one
# process per cell. Left alone the cells compete for that single unit of demand
# and the extraction sums across them -- no error, just a wrong number. Each cell
# now gets its own artificial region, and is extracted by filtering to it.
# =========================================================================== #

# A coal plant whose capex falls with each vintage, so the per-vintage LCOE has
# a known ordering. Every year is a period, so each vintage year is investable.
lv_hor <- function() newHorizon(2020:2060)

lv_tech <- function(vintages = NULL, invcost = 300, clusters = NULL,
                    name = "PWR") {
  args <- list(
    name    = name,
    input   = data.frame(comm = "COA", unit = "PJ"),
    output  = data.frame(comm = "ELC", unit = "PJ"),
    ceff    = data.frame(comm = c("COA", "ELC"), cinp2use = c(1, NA),
                         use2cact = c(NA, 0.4)),
    cap2act = 1,
    fixom   = list(fixom = 10)
  )
  if (is.null(vintages) && is.null(clusters)) {
    args$vintage <- data.frame(olife = 20L)
    args$invcost <- list(invcost = invcost)
  } else if (is.null(clusters)) {
    args$vintage <- data.frame(vintage = as.character(vintages), olife = 20L)
    args$invcost <- data.frame(vintage = as.character(vintages), invcost = invcost)
  } else {
    grid <- expand.grid(vintage = as.character(vintages),
                        cluster = as.character(clusters),
                        stringsAsFactors = FALSE)
    args$vintage <- data.frame(vintage = as.character(vintages), olife = 20L)
    args$cluster <- data.frame(cluster = as.character(clusters))
    grid$invcost <- invcost
    args$invcost <- grid
  }
  do.call(newTechnology, args)
}

lv_repo <- function() {
  newRepository(
    "lv",
    newCommodity("COA", timeframe = "ANNUAL"),
    newCommodity("ELC", timeframe = "ANNUAL"),
    newSupply("SUP_COA", commodity = "COA",
              supply = data.frame(region = "R1", cost = 1))
  )
}

lv_run <- function(tech, ...) {
  suppressMessages(suppressWarnings(
    levcost(tech, repo = lv_repo(), region = "R1", horizon = lv_hor(),
            calendar = newCalendar(), verbose = FALSE, ...)
  ))
}

# --------------------------------------------------------------------------- #

test_that("an un-vintaged technology is unaffected", {
  skip_if_no_solver()
  lc <- lv_run(lv_tech())

  expect_s3_class(lc, "levcost")
  expect_false(inherits(lc, "levcost_variants"))
  expect_length(lc$levcost_npv, 1L)
  expect_equal(names(lc$levcost_npv), "PWR")
  expect_true(is.finite(lc$levcost_npv))
})

test_that("a vintaged technology returns one levcost per vintage", {
  skip_if_no_solver()
  lc <- lv_run(lv_tech(c(2020, 2030, 2040), invcost = c(300, 200, 100)))

  expect_s3_class(lc, "levcost_variants")
  expect_s3_class(lc, "levcost_list")
  expect_length(lc, 3L)
  expect_equal(names(lc), c("PWR_VIN2020", "PWR_VIN2030", "PWR_VIN2040"))
  # each element is a fully-formed levcost result
  expect_s3_class(lc[[1]], "levcost")
  expect_length(lc[[1]]$levcost_npv, 1L)
  # exactly one _VIN token per name: a second expansion would double-suffix
  expect_true(all(lengths(regmatches(names(lc), gregexpr("_VIN", names(lc)))) == 1L))
})

test_that("cheaper vintages price lower, and the base vintage matches the plain tech", {
  skip_if_no_solver()
  lc  <- lv_run(lv_tech(c(2020, 2030, 2040), invcost = c(300, 200, 100)))
  npv <- levcost(lc, by_variant = "npv")$levcost_npv

  expect_true(all(is.finite(npv)))
  expect_true(all(diff(npv) < 0))          # 300 > 200 > 100 invcost

  # The 2020 vintage carries the same capex as the un-vintaged technology, so it
  # must reproduce it exactly. This is the assertion that would have caught the
  # old silent sum-across-vintages behaviour.
  plain <- lv_run(lv_tech(invcost = 300))
  expect_equal(unname(npv[1]), unname(plain$levcost_npv), tolerance = 1e-8)
})

test_that("vintage x cluster expands to the full grid", {
  skip_if_no_solver()
  lc <- lv_run(lv_tech(c(2020, 2030), clusters = c("a", "b"),
                       invcost = c(300, 200, 280, 180)))

  expect_s3_class(lc, "levcost_variants")
  expect_length(lc, 4L)
  expect_setequal(names(lc), c("PWR_VIN2020_CLa", "PWR_VIN2030_CLa",
                               "PWR_VIN2020_CLb", "PWR_VIN2030_CLb"))
  key <- levcost(lc, by_variant = "npv")
  expect_setequal(unique(key$vintage), c("2020", "2030"))
  expect_setequal(unique(key$cluster), c("a", "b"))
})

test_that("levcost(, by_variant = TRUE) stacks the tables with variant keys", {
  skip_if_no_solver()
  lc <- lv_run(lv_tech(c(2020, 2030, 2040), invcost = c(300, 200, 100)))

  npv <- levcost(lc, by_variant = "npv")
  expect_equal(nrow(npv), 3L)
  expect_true(all(c("tech", "variant", "vintage", "cluster", "levcost_npv")
                  %in% names(npv)))
  expect_equal(unique(npv$tech), "PWR")

  lcv <- levcost(lc, by_variant = "levcost")
  expect_true(all(c("tech", "variant", "vintage", "year", "levcost") %in% names(lcv)))
  expect_setequal(unique(lcv$variant), names(lc))

  cmp <- levcost(lc, by_variant = "components")
  expect_true(all(c("variant", "component", "value") %in% names(cmp)))
  expect_true("eac" %in% cmp$component)

  # a result with no variants names the reason, rather than failing dispatch
  expect_error(levcost(lv_run(lv_tech()), by_variant = TRUE),
               "vintages or clusters")
})

test_that("sequential and single runs agree", {
  skip_if_no_solver()
  tech <- lv_tech(c(2020, 2030, 2040), invcost = c(300, 200, 100))
  a <- levcost(lv_run(tech, run = "single"), by_variant = "npv")
  b <- levcost(lv_run(tech, run = "sequential"), by_variant = "npv")

  expect_equal(a$variant, b$variant)
  expect_equal(a$levcost_npv, b$levcost_npv, tolerance = 1e-8)
})

test_that("artificial variant regions never reach the results", {
  skip_if_no_solver()
  lc <- lv_run(lv_tech(c(2020, 2030, 2040), invcost = c(300, 200, 100)))

  regs <- unique(unlist(lapply(lc, function(x)
    c(x$levcost$region, x$cost_breakdown$region, x$cost_yearly$region))))
  regs <- regs[!is.na(regs)]
  expect_equal(unique(regs), "R1")
  expect_false(any(grepl("_VIN", regs)))
})

test_that(".levcost_devintage() collapses a cell to a single un-vintaged row", {
  tech <- lv_tech(c(2020, 2030, 2040), invcost = c(300, 200, 100))
  ex   <- getFromNamespace(".expand_one_process", "energyRt")(tech)
  dv   <- getFromNamespace(".levcost_devintage", "energyRt")(ex$objects[[2]], "R1_VIN2030")

  expect_equal(nrow(dv@vintage), 1L)
  expect_true(is.na(dv@vintage$vintage))
  expect_true(is.na(dv@vintage$cluster))
  # the cell's window survives the collapse AS DECLARED: no start/end
  # given here, so both stay NA (unbounded -- nothing defaults to the
  # vintage year)
  expect_true(is.na(dv@vintage$start))
  expect_true(is.na(dv@vintage$end))
  expect_equal(dv@vintage$olife, 20L)
  # its own capex is kept, untagged
  expect_equal(dv@invcost$invcost, 200)
  expect_true(all(is.na(dv@invcost$vintage)))
  expect_equal(dv@region, "R1_VIN2030")
  # and it will not be expanded a second time
  expect_null(getFromNamespace(".tech_variants", "energyRt")(dv))
})

test_that("variant region names are built from the base region and validated", {
  f <- getFromNamespace(".levcost_variant_regions", "energyRt")
  prov <- data.frame(vintage = c("2020", "2030"), cluster = c(NA, NA),
                     stringsAsFactors = FALSE)
  expect_equal(f("IND", prov), c("IND_VIN2020", "IND_VIN2030"))

  prov2 <- data.frame(vintage = c("2020", "2020"), cluster = c("a", "a"),
                      stringsAsFactors = FALSE)
  expect_error(f("IND", prov2), "collide")

  # a base region that is not a legal set name cannot yield a legal variant name
  expect_error(f("1BAD", prov), "must match")
})

test_that("autoplot.levcost_list survives variants without a breakdown", {
  skip_if_not_installed("ggplot2")
  # a solved variant carries the 5-column breakdown; a variant whose window
  # yields no solution carries none -- the plot must not rbind them raw
  full <- list(
    levcost_npv = c(ELC = 0.1),
    cost_breakdown_npv = data.frame(
      tech = "T_VIN2025", group = NA_character_, comm = "ELC",
      component = c("eac", "fixom"), value = c(0.06, 0.04),
      stringsAsFactors = FALSE))
  empty <- list(levcost_npv = 0, cost_breakdown_npv = NULL)
  x <- structure(list(T_VIN2025 = full, T_VIN2050 = empty),
                 class = c("levcost_variants", "levcost_list", "list"))
  p <- ggplot2::autoplot(x)
  expect_s3_class(p, "ggplot")
})

test_that("autoplot.levcost_list sorts, caps and facets for comparisons", {
  skip_if_not_installed("ggplot2")
  mk <- function(nm, v) structure(list(
    levcost_npv = stats::setNames(v, nm),
    cost_breakdown_npv = data.frame(component = c("eac", "varom"),
                                    value = c(v * 0.6, v * 0.4)),
    units = list(costs = "MUSD", activity = "GWh")),
    class = c("levcost", "list"))
  lcl <- structure(list(A = mk("A", 5), B = mk("B", 9), C = mk("C", 2)),
                   class = c("levcost_list", "list"))
  # default keeps the list's own order (the variants contract)
  p0 <- ggplot2::autoplot(lcl)
  expect_equal(levels(p0$data$tech), c("A", "B", "C"))
  # sort = most expensive first
  p1 <- ggplot2::autoplot(lcl, sort = TRUE)
  expect_equal(levels(p1$data$tech), c("B", "A", "C"))
  # top_n caps and captions
  p2 <- ggplot2::autoplot(lcl, top_n = 2)
  expect_setequal(unique(as.character(p2$data$tech)), c("A", "B"))
  expect_match(p2$labels$caption, "2 most expensive of 3")
  # groups facet with the short index
  grp <- list(list(index = "G1", label = "x", members = c("A", "B"), n = 2L),
              list(index = "G2", label = "y", members = "C", n = 1L))
  p3 <- ggplot2::autoplot(lcl, groups = grp)
  expect_equal(sort(unique(p3$data$group)), c("G1", "G2"))
  expect_s3_class(p3$facet, "FacetWrap")
})

test_that("fuel_costs prices auxiliary input commodities", {
  tech <- newTechnology(
    name    = "PWRA",
    input   = data.frame(comm = "COA", unit = "PJ"),
    output  = data.frame(comm = "ELC", unit = "PJ"),
    aux     = data.frame(acomm = "LUB", unit = "kt"),
    ceff    = data.frame(comm = c("COA", "ELC"), cinp2use = c(1, NA),
                         use2cact = c(NA, 0.4)),
    aeff    = data.frame(acomm = "LUB", act2ainp = 0.1),
    cap2act = 1,
    vintage = data.frame(olife = 20L),
    invcost = list(invcost = 300),
    fixom   = list(fixom = 10)
  )
  free   <- lv_run(tech, frontier = FALSE)
  priced <- lv_run(tech, frontier = FALSE,
                   fuel_costs = c(COA = 1, LUB = 5))
  npv <- function(x) as.numeric(x$levcost_npv[1])
  expect_gt(npv(priced), npv(free))
  # 0.1 kt LUB per unit activity at 5 -> +0.5 per unit output-activity
  expect_equal(npv(priced) - npv(free), 0.5 / 0.4 * 0.4, tolerance = 0.1)
})

test_that("the auto-horizon covers late vintage windows", {
  # horizon omitted: it used to be sized to the FIRST cell's window, which
  # priced every later vintage to zero via the backstop
  tech <- lv_tech(vintages = c(2030, 2060), invcost = c(300, 300))
  res <- suppressMessages(suppressWarnings(
    levcost(tech, repo = lv_repo(), region = "R1",
            calendar = newCalendar(), frontier = FALSE, verbose = FALSE)))
  npv <- vapply(res, function(z) as.numeric(z$levcost_npv[1]), numeric(1))
  expect_true(all(is.finite(npv)))
  expect_true(all(npv > 0))
})
# levcost(by_variant = ) -- the merged form of the former
# levcost(, by_variant = TRUE). Both paths must give the same tables: computing with
# by_variant set, and passing an already-computed result back (no re-solve).

test_that("levcost(by_variant=) matches the result's own tables", {
  skip_if_no_solver()
  tech <- newTechnology("TV",
    input = list(comm = "COA"), output = list(comm = "ELC"),
    vintage = data.frame(vintage = c("v1", "v2"), start = c(2020L, 2030L),
                         end = c(2029L, 2050L), olife = c(20L, 20L)),
    ceff = data.frame(comm = "COA", cinp2use = 0.4),
    invcost = data.frame(vintage = c("v1", "v2"), invcost = c(1000, 800)),
    cap2act = 1)
  lc <- suppressMessages(suppressWarnings(
    levcost(tech, comm = "ELC", fuel_costs = c(COA = 2), verbose = FALSE)))
  skip_if_not(inherits(lc, "levcost_variants"),
              "technology did not fan out into variants")

  # result method: extraction without recomputing
  expect_identical(levcost(lc, by_variant = TRUE), attr(lc, "by_variant"))
  expect_identical(levcost(lc, by_variant = "levcost"), attr(lc, "by_variant"))
  expect_identical(levcost(lc, by_variant = "npv"), attr(lc, "by_variant_npv"))
  expect_identical(levcost(lc, by_variant = "components"),
                   attr(lc, "by_variant_components"))

  # the deprecated extractor still agrees, and warns
  expect_warning(old <- levcost_by_variant(lc, "npv"), "v0.90")
  expect_identical(old, levcost(lc, by_variant = "npv"))

  # computing with by_variant returns the table, not the object
  direct <- suppressMessages(suppressWarnings(
    levcost(tech, comm = "ELC", fuel_costs = c(COA = 2), verbose = FALSE,
            by_variant = "npv")))
  expect_s3_class(direct, "data.frame")
  expect_identical(nrow(direct), nrow(attr(lc, "by_variant_npv")))

  # a technology with no variants says so rather than returning NULL
  flat <- newTechnology("TF", input = list(comm = "COA"),
                        output = list(comm = "ELC"),
                        ceff = data.frame(comm = "COA", cinp2use = 0.4),
                        invcost = data.frame(invcost = 1000), cap2act = 1)
  expect_error(
    suppressMessages(suppressWarnings(
      levcost(flat, comm = "ELC", fuel_costs = c(COA = 2), verbose = FALSE,
              by_variant = TRUE))),
    "vintages or clusters")
})

# --------------------------------------------------------------------------- #
# report(levcost = TRUE, by_variant = ) on a vintaged technology.
#
# The regression: `by_variant` used to fall through report()'s dots straight to
# rmarkdown::render(). Forwarding it to levcost() instead is equally wrong --
# levcost(by_variant = TRUE) returns an EXTRACTED data.frame, which fails
# report()'s `inherits(levcost, "levcost_variants")` test and silently drops the
# whole levelised-cost section. report() consumes the flag itself: TRUE (the
# default for a variant process) keeps the per-variant table and comparison
# chart, FALSE reports the display instance alone.
# --------------------------------------------------------------------------- #

test_that("report() consumes by_variant instead of forwarding it to levcost()", {
  skip_if_not_installed("rmarkdown")
  tech <- lv_tech(vintages = c(2020, 2030, 2040), invcost = c(300, 200, 100))

  f_on <- tempfile(fileext = ".html")
  expect_no_error(suppressWarnings(suppressMessages(
    report(tech, levcost = TRUE, by_variant = TRUE, horizon = lv_hor(),
           file = f_on, format = "html", open = FALSE))))
  expect_true(file.exists(f_on))

  f_off <- tempfile(fileext = ".html")
  expect_no_error(suppressWarnings(suppressMessages(
    report(tech, levcost = TRUE, by_variant = FALSE, horizon = lv_hor(),
           file = f_off, format = "html", open = FALSE))))
  expect_true(file.exists(f_off))

  # by_variant = TRUE must not degrade the report: dropping the per-variant
  # section was the silent failure, and it makes the file materially smaller
  expect_gt(file.size(f_on), file.size(f_off))

  # and it must match the default, which is already per-variant
  f_def <- tempfile(fileext = ".html")
  suppressWarnings(suppressMessages(
    report(tech, levcost = TRUE, horizon = lv_hor(),
           file = f_def, format = "html", open = FALSE)))
  expect_equal(file.size(f_on), file.size(f_def), tolerance = 0.01)
})
