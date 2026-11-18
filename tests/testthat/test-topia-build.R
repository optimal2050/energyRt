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
  expect_identical(names(k)[1:5],
                   c("regions", "groups", "calendar", "repo", "geoscale"))
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
               "splits region group")
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
  expect_identical(names(k), c("regions", "groups", "calendar", "repo",
                               "geoscale", "CO2_CAP", "CT_CO2"))
  expect_identical(names(build_topia(levers = FALSE, quiet = TRUE)),
                   c("regions", "groups", "calendar", "repo", "geoscale"))
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
    function() build_topia(clusters = FALSE, quiet = TRUE),
    function() build_topia(demand = 2, quiet = TRUE),
    function() build_topia(level = "zone", quiet = TRUE))) {
    expect_error(call(), "is not built yet")
  }
})

# Covers the R API: build_topia(weather=), build_topia(demand=)
test_that("the series can be replaced by name or by frame", {
  skip_if_not_installed("geoscales")
  kit <- tb_kit()
  # The shipped measured set is what the model already holds, so naming it
  # round-trips: the same series come back and the annual energy is unchanged.
  k <- build_topia(weather = "sample", demand = "sample", quiet = TRUE)
  o1 <- getObject(k$repo)
  o0 <- getObject(kit$repo)
  expect_equal(o1$WSOL@weather$wval, o0$WSOL@weather$wval)
  ann <- function(o) {
    d <- o$DEM_ELC@demand
    as.numeric(tapply(d$demand, as.character(d$region), sum))
  }
  expect_equal(ann(o1), ann(o0))

  # A set built for another calendar is refused naming that calendar, rather
  # than silently leaving the shipped series in place.
  expect_error(build_topia(weather = "simulated", quiet = TRUE), "s4_h24")
  expect_error(build_topia(weather = "nope", quiet = TRUE), "simulated")

  # A frame whose timeslices are not the model's is refused naming the model's.
  bad <- topia_profiles(kit$regions, calendar = "d365_h24")$demand
  bad$timeslice <- "WIN_h01"
  expect_error(build_topia(demand = bad, quiet = TRUE), "d365_h24")

  # A `load` column is a relative shape and is scaled to the annual energy the
  # shipped object already holds.
  shape <- topia_profiles(kit$regions, calendar = "d365_h24")$demand
  expect_equal(ann(getObject(build_topia(demand = shape, quiet = TRUE)$repo)),
               ann(o0))

  # A weather frame that covers none of a technology's declared grades is
  # refused: a clustered technology left with no weather row runs unbounded.
  flat <- topia_profiles(kit$regions, calendar = "d365_h24")$weather
  flat$cluster <- NA_character_
  expect_error(build_topia(weather = flat, quiet = TRUE), "clusters = FALSE")
})

test_that("quiet is quiet, and the default reports what it built", {
  skip_if_not_installed("geoscales")
  expect_silent(build_topia("Z3", quiet = TRUE))
  expect_message(build_topia(quiet = FALSE), "build_topia: 12 regions")
})

# --------------------------------------------------------------------------- #
# Process selection and the dependency closure.
# --------------------------------------------------------------------------- #

tb_names <- function(k) names(getObject(k$repo))

# Covers the R API: build_topia(techs=)
test_that("dropping a burner takes its whole fuel chain with it", {
  k <- build_topia(techs = "-ECOA", quiet = TRUE)
  n <- tb_names(k)
  # nothing burns coal any more, so the fuel, both sources and both corridors
  # go -- this is the closure, and doing it by hand is where a kit ends up
  # with a supply feeding nothing
  expect_false(any(c("ECOA", "COA", "SUP_COA", "IMP_COA",
                     "TBD_COA_Z4_Z3", "TBD_COA_Z4_Z5") %in% n))
  # gas is untouched, and so is its corridor
  expect_true(all(c("EGAS", "GAS", "SUP_GAS", "IMP_GAS",
                    "TBD_GAS_Z2_Z3") %in% n))
})

test_that("an aux commodity is a consumer, so land follows its plants", {
  # `LANDS` is named in `@aux`/`@aeff` as `acomm`, not as an input `comm`.
  # Read only `comm` and solar looks like it consumes nothing, the land pool
  # looks unused, and it is dropped out from under the plant that needs it.
  with_sol <- build_topia(techs = "ESOL", quiet = TRUE)
  expect_true(all(c("ESOL", "LANDS", "RES_LANDS") %in% tb_names(with_sol)))

  no_sol <- build_topia(techs = "-ESOL", quiet = TRUE)
  expect_false(any(c("ESOL", "LANDS", "RES_LANDS") %in% tb_names(no_sol)))
  # wind keeps its own pool
  expect_true(all(c("EWIN", "LANDW", "RES_LANDW") %in% tb_names(no_sol)))
})

test_that("a store and a corridor do not keep their own commodity alive", {
  # Both sit on BOTH ends of one commodity -- `STG_H2` takes H2 in and puts
  # H2 out, a coal corridor carries COA either way -- so counting them as
  # consumers or producers lets a commodity hold itself up after its last
  # real user is gone.
  k <- build_topia(techs = "-H2ELC", quiet = TRUE)
  expect_false(any(c("H2ELC", "H2", "STG_H2", "EXP_H2") %in% tb_names(k)))
  # the electricity store survives: ELC has real producers and a real consumer
  expect_true("STG_ELC" %in% tb_names(k))
})

test_that("CO2 is reachable only as an emission, and must survive", {
  # Nothing takes CO2 in or puts it out: it is declared on `COA` and `GAS` as
  # `@emis`. Judged on flows alone it looks orphaned, and dropping it takes
  # the carbon cap and the tax with it.
  k <- build_topia(techs = "-ECOA", quiet = TRUE)
  expect_true("CO2" %in% tb_names(k))
  expect_true(all(c("CO2_CAP", "CT_CO2") %in% names(k)))

  # with nothing burning at all there is no CO2, and the levers go too
  v <- build_topia(techs = "vre", quiet = TRUE)
  expect_false("CO2" %in% tb_names(v))
  expect_false(any(c("CO2_CAP", "CT_CO2") %in% names(v)))
})

# Covers the R API: build_topia(modules=)
test_that("modules use the article's own nine groups", {
  k <- build_topia(quiet = TRUE)
  expect_setequal(names(k$groups),
                  c("commodities", "supply", "weather", "generators",
                    "storage", "hydrogen", "transmission", "fuel_trade",
                    "demand"))
  # the groups partition the repository: every object in exactly one
  g <- unlist(k$groups, use.names = FALSE)
  expect_setequal(g, tb_names(k))
  expect_equal(anyDuplicated(g), 0L)

  h <- build_topia(modules = "-hydrogen", quiet = TRUE)
  expect_false(any(c("H2ELC", "STG_H2", "EXP_H2", "H2") %in% tb_names(h)))
  expect_true("STG_ELC" %in% tb_names(h))
})

test_that("a keyword is derived from the objects, not from a list", {
  vre <- build_topia(techs = "vre", quiet = TRUE)
  th <- build_topia(techs = "thermal", quiet = TRUE)
  tech_of <- function(k) names(Filter(function(o) methods::is(o, "technology"),
                                      getObject(k$repo)))
  # a VRE plant is one that declares a weather series
  expect_setequal(tech_of(vre), c("ESOL", "EWIN", "EHYD"))
  # a thermal one is one that burns its input
  expect_setequal(tech_of(th), c("ECOA", "EGAS", "EBIO"))
})

test_that("a lever naming a process that is gone goes with it", {
  # `EARLY_RET` caps ECOA and EGAS by name; `GREEN_H2` sums over H2ELC, ESOL,
  # EWIN and EHYD. Left in place they reference a process the model does not
  # declare, and the model is refused before it is ever interpolated.
  expect_false("EARLY_RET" %in% names(build_topia(techs = "-ECOA",
                                                  quiet = TRUE)))
  expect_false("GREEN_H2" %in% names(build_topia(techs = "-H2ELC",
                                                 quiet = TRUE)))
  expect_true(all(c("EARLY_RET", "GREEN_H2") %in%
                    names(build_topia(quiet = TRUE))))
})

test_that("no kept process is left naming an object that is gone", {
  for (sel in list(list(techs = "-ECOA"), list(techs = c("EGAS", "ESOL")),
                   list(techs = "vre"), list(modules = "-hydrogen"))) {
    k <- do.call(build_topia, c(sel, list(quiet = TRUE)))
    ob <- getObject(k$repo)
    n <- names(ob)
    # every weather series a kept process names is still there
    wref <- unique(unlist(lapply(ob, energyRt:::.tb_wrefs), use.names = FALSE))
    expect_true(all(wref %in% n),
                label = paste(names(sel), sel, collapse = " "))
    # and every commodity it touches
    cm <- unique(unlist(lapply(
      Filter(function(o) !methods::is(o, "commodity") &&
                         !methods::is(o, "weather"), ob),
      energyRt:::.tb_touches), use.names = FALSE))
    expect_true(all(cm %in% n), label = paste(names(sel), sel, collapse = " "))
  }
})

test_that("a selection is all keeps or all drops, and names are checked", {
  expect_error(build_topia(techs = c("EGAS", "-ECOA"), quiet = TRUE),
               "not both")
  expect_error(build_topia(techs = "NOPE", quiet = TRUE), "unknown keyword")
  expect_error(build_topia(modules = "-nope", quiet = TRUE), "unknown modules")
})

test_that("process selection composes with a region subset", {
  skip_if_not_installed("geoscales")
  k <- build_topia("R01", techs = c("EGAS", "ESOL"), quiet = TRUE)
  expect_equal(k$regions, "R01")
  n <- tb_names(k)
  expect_setequal(
    names(Filter(function(o) methods::is(o, "technology"), getObject(k$repo))),
    c("EGAS", "ESOL"))
  # the island has no wire on any layout, and no coal is burned here
  expect_false(any(grepl("^TBD_", n)))
  expect_false("COA" %in% n)
})

# --------------------------------------------------------------------------- #
# Time: re-keying the series and the demand curve onto a cheaper calendar.
# --------------------------------------------------------------------------- #

tb_annual <- function(k) {
  d <- getObject(k$repo)$DEM_ELC@demand
  cal <- topia$modules$calendars[[k$calendar]] %||% calendars[[k$calendar]]
  w <- as.data.frame(cal@timeslice_share)
  sum(d$demand * stats::setNames(w$weight,
                                 as.character(w$timeslice))[
                                   as.character(d$timeslice)])
}

# Covers the R API: build_topia(calendar=)
test_that("a sample of the hourly year keeps the annual energy", {
  full <- build_topia(quiet = TRUE)
  for (nm in c("d365_h24_1dpm", "d365_h24_1dps")) {
    k <- build_topia(calendar = nm, quiet = TRUE)
    expect_equal(k$calendar, nm)
    # fewer rows...
    expect_lt(nrow(getObject(k$repo)$DEM_ELC@demand),
              nrow(getObject(full$repo)$DEM_ELC@demand))
    # ...but the same year. A sample is a cheaper VIEW of the year, not a
    # smaller year: two cases are only comparable if the demand they must
    # meet is the same.
    expect_equal(tb_annual(k), tb_annual(full), tolerance = 1e-8)
  }
})

test_that("the measured grades survive a sample", {
  k <- build_topia(calendar = "d365_h24_1dps", quiet = TRUE)
  ob <- getObject(k$repo)
  expect_setequal(unique(ob$WSOL@weather$cluster), c("FL", "TH"))
  expect_setequal(unique(ob$WWIN@weather$cluster), c("HI", "MD", "LO"))
  expect_gt(nrow(ob$WHYD@weather), 0L)
})

test_that("a calendar that is not a sample is refused, and says why", {
  # `s4_h24` carries its OWN timeslice names and a simulated stand-in with no
  # grades, so every clustered technology would lose its weather rows and run
  # unbounded. That is the whole reason this is a refusal and not a warning.
  expect_error(build_topia(calendar = "s4_h24", quiet = TRUE),
               "not a sample")
  expect_error(build_topia(calendar = "m12_h24", quiet = TRUE),
               "not a sample")
  expect_error(build_topia(calendar = "nope", quiet = TRUE),
               "unknown calendar")
  # the per-milestone form points at the verb that does it
  expect_error(build_topia(calendar = list(default = "d365_h24"),
                           quiet = TRUE), "setCalendar")
})

test_that("a horizon is taken by name or object", {
  k <- build_topia(horizon = "BY2025", as = "model", quiet = TRUE)
  expect_s4_class(k, "model")
  expect_error(build_topia(horizon = "nope", quiet = TRUE), "unknown horizon")
})

# Covers the R API: build_topia(preset=)
test_that("preset tiny is small enough to be worth having", {
  skip_if_not_installed("geoscales")
  k <- build_topia(preset = "tiny", quiet = TRUE)
  expect_equal(k$regions, "R01")
  expect_equal(k$calendar, "d365_h24_1dps")
  expect_lt(length(getObject(k$repo)), 15L)
  # a preset cannot contradict an argument it sets
  expect_error(build_topia(preset = "tiny", techs = "ECOA", quiet = TRUE),
               "not both")
})

test_that("a frame bound to another calendar than the model's is refused", {
  # the shipped tables say which calendar their timeslices are on
  ln <- timescales::calendar_link(topia$demand$s4_h24)
  expect_equal(ln$scale[ln$column == "timeslice"], "s4_h24")
  expect_error(build_topia(demand = topia$demand$s4_h24, quiet = TRUE),
               "bound to calendar")
  # the right calendar's table goes through as an ordinary frame
  d <- topia$demand$d365_h24
  d <- d[as.character(d$dataset) == "sample", c("region", "timeslice", "load")]
  expect_no_error(build_topia(demand = d, quiet = TRUE))
})
