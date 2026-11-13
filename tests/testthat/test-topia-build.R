# =========================================================================== #
# build_topia(): TOPIA instances cut from the shipped data files.
#
# The load-bearing test is the FIRST one. `build_topia()` authors nothing, so
# with every argument at its default it must hand back the shipped kit object
# for object -- not an equal one, the same one. That is what keeps the article
# the only place a TOPIA object is declared: if this ever needs a tolerance,
# the function has started building rather than selecting.
# =========================================================================== #

tb_kit <- function() topia$modules$electricity

# Covers the R API: build_topia()
test_that("every default is an identity, not a rebuild", {
  expect_identical(build_topia(quiet = TRUE), tb_kit())
})

test_that("the kit shape is the dataset's own, so the two interchange", {
  k <- build_topia(quiet = TRUE)
  expect_identical(names(k), names(tb_kit()))
  expect_identical(names(k)[1:4], c("regions", "calendar", "repo", "geoscale"))
  expect_s4_class(k$repo, "repository")
})

# Covers the R API: build_topia(regions=)
test_that("a zone code expands to the provinces beneath it", {
  skip_if_not_installed("geoscales")
  z <- build_topia("Z3", quiet = TRUE)
  expect_equal(z$regions, c("R04", "R05", "R06"))

  # the nation is a code too, and reaches everything
  expect_equal(build_topia("TOPIA", quiet = TRUE)$regions, tb_kit()$regions)

  # a count takes the first n in CANONICAL order, and so does an out-of-order
  # request -- the model's order is a property of the model, not of the asking
  expect_equal(build_topia(3, quiet = TRUE)$regions, c("R01", "R02", "R03"))
  expect_equal(build_topia(c("R03", "R02"), quiet = TRUE)$regions,
               c("R02", "R03"))
})

# Covers the R API: build_topia(regions=), subset_model_regions()
test_that("a request that splits a zone is refused, and names the province", {
  skip_if_not_installed("geoscales")
  # `COA` and `GAS` balance at the ZONE, so two of Z3's three provinces is not
  # a smaller model, it is a model whose fuel pool has no defined size. The
  # refusal comes from subset_model_regions() and names what to add -- which
  # is why asking by zone code is the shorter road.
  expect_error(build_topia(c("R04", "R05"), quiet = TRUE),
               "cuts region group")
  expect_error(build_topia(c("R04", "R05"), quiet = TRUE), "R06")
  expect_no_error(build_topia("Z3", quiet = TRUE))
})

test_that("a code at no geoframe is refused, and the message lists them", {
  skip_if_not_installed("geoscales")
  expect_error(build_topia("R99", quiet = TRUE), "not a code at any geoframe")
  expect_error(build_topia("R99", quiet = TRUE), "Z1/Z2/Z3")
  expect_error(build_topia(99, quiet = TRUE), "between 1 and 12")
})

# Covers the R API: build_topia(regions=), subset_model_regions()
test_that("a subset drops the objects that left with its provinces", {
  skip_if_not_installed("geoscales")
  full <- length(getObject(tb_kit()$repo))
  one <- build_topia("R01", quiet = TRUE)
  three <- build_topia("Z3", quiet = TRUE)

  expect_lt(length(getObject(one$repo)), length(getObject(three$repo)))
  expect_lt(length(getObject(three$repo)), full)

  # `R01` carries no transmission on any layout, so the island keeps no
  # corridor at all while the three-province zone keeps its internal ones
  trades <- function(k) Filter(function(o) methods::is(o, "trade"),
                               getObject(k$repo))
  expect_length(trades(one), 0L)
  expect_gt(length(trades(three)), 0L)
})

# Covers the R API: build_topia(levers=)
test_that("the levers that carry a shape are narrowed with the model", {
  skip_if_not_installed("geoscales")
  k <- tb_kit()
  n_all <- length(k$regions)
  z <- build_topia("Z3", quiet = TRUE)

  # GREEN_H2 names every (region, year, timeslice): the rows of a dropped
  # province would reference a region the model no longer declares
  expect_setequal(unique(as.character(z$GREEN_H2@for.each$region)), z$regions)
  expect_equal(nrow(z$GREEN_H2@for.each),
               nrow(k$GREEN_H2@for.each) * length(z$regions) / n_all)

  # CO2_CAP is a per-province allowance times the province count, so it
  # rescales by the count kept -- left alone it would be four times the
  # emissions it is meant to bind, and would simply never bind
  expect_equal(z$CO2_CAP@rhs$rhs,
               k$CO2_CAP@rhs$rhs * length(z$regions) / n_all)

  # these two have no province dimension and must come back untouched
  expect_identical(z$CT_CO2, k$CT_CO2)
  expect_identical(z$EARLY_RET, k$EARLY_RET)
})

test_that("levers are chosen by name, or dropped", {
  k <- build_topia(levers = c("CT_CO2", "CO2_CAP"), quiet = TRUE)
  # named in the dataset's order, whatever order they were asked in
  expect_identical(names(k), c("regions", "calendar", "repo", "geoscale",
                               "CO2_CAP", "CT_CO2"))
  expect_identical(names(build_topia(levers = FALSE, quiet = TRUE)),
                   c("regions", "calendar", "repo", "geoscale"))
  expect_error(build_topia(levers = "NO_SUCH", quiet = TRUE), "unknown lever")
})

# Covers the R API: build_topia(as=)
test_that("`as` returns the three shapes, and the model carries its geoscale", {
  skip_if_not_installed("geoscales")
  expect_s4_class(build_topia(as = "repository", quiet = TRUE), "repository")
  m <- build_topia("Z3", as = "model", quiet = TRUE)
  expect_s4_class(m, "model")
  expect_equal(as.character(m@config@region), c("R04", "R05", "R06"))
  expect_true(is_geoscale(m@config@geoscale))
  expect_equal(geoscales::geoscale_geoframes(m@config@geoscale, finest = TRUE),
               "province")
})

# Covers the R API: build_topia(layout=)
test_that("a layout changes the picture, not the model", {
  skip_if_not_installed("geoscales")
  # the corridor network is frozen in data-raw/topia_links.R and is the same
  # on all four layouts, so the repository must not move with the drawing
  a <- build_topia(layout = "squares", quiet = TRUE)
  b <- build_topia(layout = "continent", quiet = TRUE)
  expect_identical(names(getObject(a$repo)), names(getObject(b$repo)))
  expect_false(identical(a$geoscale, b$geoscale))
  expect_error(build_topia(layout = "nope", quiet = TRUE), "unknown layout")
})

# Covers the R API: build_topia(preset=)
test_that("a preset expands to arguments and cannot contradict them", {
  skip_if_not_installed("geoscales")
  expect_equal(build_topia(preset = "island", quiet = TRUE)$regions, "R01")
  expect_equal(build_topia(preset = "zone", quiet = TRUE)$regions,
               c("R04", "R05", "R06"))
  expect_identical(build_topia(preset = "full", quiet = TRUE), tb_kit())
  expect_error(build_topia("R01", preset = "zone", quiet = TRUE),
               "not both")
  expect_error(build_topia(preset = "nope", quiet = TRUE), "unknown preset")
})

test_that("an argument that is not built yet is refused, not ignored", {
  # The whole point: a case asking for one of these must fail rather than come
  # back shaped like the default and silently wrong.
  for (call in list(
    function() build_topia(techs = "ECOA", quiet = TRUE),
    function() build_topia(modules = "-hydrogen", quiet = TRUE),
    function() build_topia(clusters = FALSE, quiet = TRUE),
    function() build_topia(demand = 2, quiet = TRUE),
    function() build_topia(weather = "simulated", quiet = TRUE),
    function() build_topia(level = "zone", quiet = TRUE),
    function() build_topia(calendar = "s4_h24", quiet = TRUE),
    function() build_topia(horizon = "BY2025", quiet = TRUE))) {
    expect_error(call(), "is not built yet")
  }
})

test_that("quiet is quiet, and the default reports what it built", {
  skip_if_not_installed("geoscales")
  expect_silent(build_topia("Z3", quiet = TRUE))
  expect_message(build_topia(quiet = FALSE), "build_topia: 12 regions")
})
