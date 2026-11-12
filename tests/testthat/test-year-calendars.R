# =========================================================================== #
# A calendar per milestone year: the assignment and the guard.
#
# A milestone may be solved on a SAMPLE of the model calendar -- hourly where
# the investment decisions land, cheaper in the years that only carry the
# trajectory forward. The two halves are `horizon@intervals$calendar` (a name
# per milestone) and `config@misc$calendars` (the objects).
#
# What must hold is the universe invariant: a per-year calendar keeps every
# timeframe level and draws its slices from the model calendar's own universe,
# with the same `share` per slice. Only membership and `weight` vary. Drop a
# level instead and `.complete_calendar()` re-derives the leaf names
# (`WIN_h00` -> `h00`), after which the model's data matches nothing and the
# year solves to zero WITHOUT WARNING -- which is why the guard exists and why
# these tests pin its refusals, not just its acceptances.
# =========================================================================== #

.yc_model <- function(cal = calendars$d365_h24) {
  newModel("yc", calendar = cal,
           horizon = newHorizon(period = 2030:2044, intervals = c(5, 5, 5)))
}

.yc_mid <- function(mod) as.integer(mod@config@horizon@intervals$mid)


# --------------------------------------------------------------------------- #
# 1. nothing declared
# --------------------------------------------------------------------------- #

test_that("a model with no per-year calendar reads as one calendar for all", {
  mod <- .yc_model()
  expect_null(getYearCalendars(mod))
  expect_null(energyRt:::.year_calendar_names(mod))
  expect_silent(energyRt:::.check_year_calendars(mod))
  # and the column is simply absent, not NA-filled
  expect_false("calendar" %in% names(mod@config@horizon@intervals))
})

test_that("getCalendar keeps answering the model calendar", {
  mod <- .yc_model()
  expect_equal(getCalendar(mod)@name, "d365_h24")
  mod <- setCalendar(mod, calendars$d365_h24_1dpm, years = .yc_mid(mod)[2])
  expect_equal(getCalendar(mod)@name, "d365_h24")
})


# --------------------------------------------------------------------------- #
# 2. the assignment
# --------------------------------------------------------------------------- #

test_that("setCalendar(years =) writes the name and registers the object", {
  mod <- .yc_model()
  y <- .yc_mid(mod)[2]
  mod <- setCalendar(mod, calendars$d365_h24_1dpm, years = y)
  iv <- mod@config@horizon@intervals
  expect_true("calendar" %in% names(iv))
  expect_equal(iv$calendar[iv$mid == y], "d365_h24_1dpm")
  expect_true(all(is.na(iv$calendar[iv$mid != y])))
  expect_equal(names(mod@config@misc$calendars), "d365_h24_1dpm")
  expect_s4_class(mod@config@misc$calendars[["d365_h24_1dpm"]], "calendar")
})

test_that("every milestone resolves to a calendar, named or inherited", {
  mod <- .yc_model()
  mid <- .yc_mid(mod)
  mod <- setCalendar(mod, calendars$d365_h24_1dpm, years = mid[2])
  yc <- getYearCalendars(mod)
  expect_named(yc, as.character(mid))
  expect_equal(yc[[as.character(mid[2])]]@name, "d365_h24_1dpm")
  for (k in as.character(mid[-2])) expect_equal(yc[[k]]@name, "d365_h24")
  # the sampled year really is smaller
  expect_lt(nrow(yc[[as.character(mid[2])]]@timeslice_share),
            nrow(yc[[as.character(mid[1])]]@timeslice_share))
})

test_that("two years can share one calendar, and a year can be reassigned", {
  mod <- .yc_model()
  mid <- .yc_mid(mod)
  mod <- setCalendar(mod, calendars$d365_h24_1dpm, years = mid[c(2, 3)])
  iv <- mod@config@horizon@intervals
  expect_equal(sum(!is.na(iv$calendar)), 2L)
  expect_length(mod@config@misc$calendars, 1L)
  # reassigning the same year to the model calendar is clearing the cell
  mod@config@horizon@intervals$calendar[iv$mid == mid[3]] <- NA_character_
  expect_equal(getYearCalendars(mod)[[as.character(mid[3])]]@name, "d365_h24")
})

test_that("setCalendar without years sets the model calendar, as before", {
  expect_equal(getCalendar(setCalendar(.yc_model(), calendars$m12_h24))@name,
               "m12_h24")
  # and the pre-0.93 form -- newCalendar()'s own arguments -- still builds one
  mod <- setCalendar(.yc_model(), name = "built",
                     timetable = calendars$m12@timetable)
  expect_equal(getCalendar(mod)@name, "built")
  # a non-calendar first argument is refused rather than passed to newCalendar
  expect_error(setCalendar(.yc_model(), 42), "must be a calendar object")
})

test_that("the config and scenario methods agree with the model one", {
  cfg <- .yc_model()@config
  y <- as.integer(cfg@horizon@intervals$mid[2])
  cfg <- setCalendar(cfg, calendars$d365_h24_1dpm, years = y)
  expect_equal(getYearCalendars(cfg)[[as.character(y)]]@name,
               "d365_h24_1dpm")
})


# --------------------------------------------------------------------------- #
# 3. the guard
# --------------------------------------------------------------------------- #

test_that("a dropped timeframe level is refused, with the reason", {
  # `d365` is daily where the model is hourly: a level fewer, not a sample.
  # This is the silent one -- the leaf names would change.
  mod <- .yc_model()
  expect_error(setCalendar(mod, calendars$d365, years = .yc_mid(mod)[2]),
               "must keep EVERY level")
  expect_error(setCalendar(mod, calendars$d365, years = .yc_mid(mod)[2]),
               "ANNUAL > YDAY")
})

test_that("a calendar built on another universe is refused", {
  mod <- .yc_model(calendars$m12_h24)
  expect_error(
    setCalendar(mod, calendars$d365_h24_1dpm, years = .yc_mid(mod)[2]),
    "timeframe levels")
})

test_that("a slice outside the model calendar's universe is refused", {
  # Same levels, but a member the model calendar does not have. Reached by
  # hand because no shipped pair differs this way.
  mod <- .yc_model(calendars$d365_h24)
  cal <- calendars$d365_h24_1dpm
  ts <- cal@timeslice_share
  ts$timeslice[nrow(ts)] <- "d999_h99"
  cal@timeslice_share <- ts
  mod@config@misc$calendars <- list(bad = cal)
  iv <- mod@config@horizon@intervals
  iv$calendar <- c(NA, "bad", NA, NA)[seq_len(nrow(iv))]
  mod@config@horizon@intervals <- iv
  expect_error(energyRt:::.check_year_calendars(mod),
               "the model calendar does not")
})

test_that("a different share for a shared slice is refused", {
  # `share` is the slice's own duration and pTimesliceShare carries no year,
  # so a divergence here is silently wrong for one of the two years.
  mod <- .yc_model(calendars$d365_h24)
  cal <- calendars$d365_h24_1dpm
  ts <- cal@timeslice_share
  ts$share[nrow(ts)] <- ts$share[nrow(ts)] * 2
  cal@timeslice_share <- ts
  mod@config@misc$calendars <- list(bad = cal)
  iv <- mod@config@horizon@intervals
  iv$calendar <- c(NA, "bad", NA, NA)[seq_len(nrow(iv))]
  mod@config@horizon@intervals <- iv
  expect_error(energyRt:::.check_year_calendars(mod), "different `share`")
})

test_that("a name the model does not carry is refused", {
  mod <- .yc_model()
  iv <- mod@config@horizon@intervals
  iv$calendar <- c(NA, "nowhere", NA, NA)[seq_len(nrow(iv))]
  mod@config@horizon@intervals <- iv
  expect_error(energyRt:::.check_year_calendars(mod), "does not carry")
  expect_error(energyRt:::.check_year_calendars(mod), "nowhere")
})

test_that("a year that is not a milestone, and an unnamed calendar, refuse", {
  mod <- .yc_model()
  expect_error(setCalendar(mod, calendars$d365_h24_1dpm, years = 2033),
               "not a milestone year")
  anon <- calendars$d365_h24_1dpm
  anon@name <- ""
  expect_error(setCalendar(mod, anon, years = .yc_mid(mod)[2]),
               "needs a name")
})


# --------------------------------------------------------------------------- #
# 4. the horizon carries the column
# --------------------------------------------------------------------------- #

test_that("the calendar column survives a forced base-year interval", {
  # The force-base-year path rebuilds `intervals` from a fixed set of columns;
  # anything else on the table was dropped there and nowhere else.
  iv <- data.frame(start = c(2030, 2031, 2036), mid = c(2030, 2035, 2040),
                   end = c(2030, 2035, 2044),
                   calendar = c(NA, "d365_h24_1dpm", NA))
  h <- newHorizon(intervals = iv, base_year = 2030)
  expect_true("calendar" %in% names(h@intervals))
  expect_equal(h@intervals$calendar[h@intervals$mid == 2035],
               "d365_h24_1dpm")
  expect_true(is.na(h@intervals$calendar[1]))
})

test_that("the column is validated for shape, not for the name", {
  iv <- data.frame(start = 2030, mid = 2030, end = 2030, calendar = 1)
  expect_error(newHorizon(intervals = iv), "must be a character column")
  iv$calendar <- "  "
  expect_error(newHorizon(intervals = iv), "must not contain empty strings")
  # a name no model carries is not the horizon's business
  iv$calendar <- "whatever"
  expect_s4_class(newHorizon(intervals = iv), "horizon")
})


# --------------------------------------------------------------------------- #
# 5. interpolation
# --------------------------------------------------------------------------- #

.yc_kit <- function(sample_years = NULL, nm = "yck") {
  kit <- topia$modules$unit$U1
  cal <- topia$modules$unit$calendars[["unit_s4"]]
  mod <- newModel(nm, data = kit$repo, calendar = cal, region = kit$regions,
                  horizon = newHorizon(period = 2025:2035,
                                       intervals = c(1, 5, 5)),
                  discount = 0)
  if (!is.null(sample_years)) {
    half <- sample_calendar(cal, c("S1", "S2"),
                                        name = "unit_s4_half")
    mod <- setCalendar(mod, half, years = sample_years)
  }
  mod
}

.yc_interp <- function(mod, nm) {
  scen <- NULL
  invisible(utils::capture.output(
    scen <- suppressWarnings(suppressMessages(
      interpolate_model(mod, name = nm, ondisk = FALSE, overwrite = TRUE)))))
  scen
}

test_that("a sampled year's variable domains hold only its own slices", {
  skip_if_no_fixtures()
  a <- .yc_interp(.yc_kit(nm = "yc_f"), "yc_f")
  b <- .yc_interp(.yc_kit(2028L, nm = "yc_s"), "yc_s")
  ga <- function(s, m) as.data.frame(get_data_slot(s@modInp@parameters[[m]]))
  # the full model: every year has every slice
  expect_equal(sort(unique(ga(a, "mvTechAct")$timeslice[
    ga(a, "mvTechAct")$year == 2028])), c("S1", "S2", "S3", "S4"))
  # the sampled one: 2028 keeps two, the other years keep four
  expect_equal(sort(unique(ga(b, "mvTechAct")$timeslice[
    ga(b, "mvTechAct")$year == 2028])), c("S1", "S2"))
  expect_equal(sort(unique(ga(b, "mvTechAct")$timeslice[
    ga(b, "mvTechAct")$year == 2025])), c("S1", "S2", "S3", "S4"))
  expect_lt(nrow(ga(b, "mvTechAct")), nrow(ga(a, "mvTechAct")))
})

test_that("the membership maps stay a union over years", {
  # The year gate lives in the variable domains; a membership map answers
  # "can this ever happen", which is why no solver template changed.
  skip_if_no_fixtures()
  b <- .yc_interp(.yc_kit(2028L, nm = "yc_u"), "yc_u")
  for (m in c("mCommTimeslice", "mTechTimeslice")) {
    d <- as.data.frame(get_data_slot(b@modInp@parameters[[m]]))
    expect_false("year" %in% names(d), info = m)
    expect_true(all(c("S1", "S2", "S3", "S4") %in% d$timeslice), info = m)
  }
})

test_that("no year-keyed entry holds a slice its year does not have", {
  # The guarantee, swept once after every map is built: several builders form
  # their domain as a cross product of a year-less timeslice table with the
  # milestone list, which cannot be gated at the source.
  skip_if_no_fixtures()
  b <- .yc_interp(.yc_kit(2028L, nm = "yc_g"), "yc_g")
  ok <- with(energyRt:::.year_slice_df(b), paste(year, timeslice))
  bad <- character(0)
  for (pn in names(b@modInp@parameters)) {
    d <- tryCatch(get_data_slot(b@modInp@parameters[[pn]]),
                  error = function(e) NULL)
    if (is.null(d) || !nrow(d)) next
    if (!all(c("year", "timeslice") %in% colnames(d))) next
    d <- as.data.frame(d)
    y <- suppressWarnings(as.integer(d$year))
    s <- as.character(d$timeslice)
    off <- !is.na(y) & !is.na(s) & !(paste(y, s) %in% ok)
    if (any(off)) bad <- c(bad, pn)
  }
  expect_equal(bad, character(0))
})

test_that("a sampled year's weights come from its own calendar", {
  skip_if_no_fixtures()
  b <- .yc_interp(.yc_kit(2028L, nm = "yc_w"), "yc_w")
  w <- as.data.frame(get_data_slot(b@modInp@parameters[["pTimesliceWeight"]]))
  # a season standing for two weighs 2, so the year still annualises
  expect_equal(sort(w$value[w$year == 2028 & w$timeslice %in% c("S1", "S2")]),
               c(2, 2))
  expect_equal(sort(w$value[w$year == 2025 & w$timeslice %in% c("S1", "S2")]),
               c(1, 1))
  # and the absent slices carry no weight row at all
  expect_equal(nrow(w[w$year == 2028 & w$timeslice %in% c("S3", "S4"), ]), 0L)
})

# @covers vObjective vTechAct depth=S backends=glpk
test_that("sampling a symmetric calendar does not change the answer", {
  # The kit is symmetric across seasons and every input is 1, so two seasons
  # at weight 2 must deliver exactly what four at weight 1 do. If the
  # membership gating and the per-year weights disagree -- gate without
  # reweighting, say -- the sampled year silently counts as a FRACTION of a
  # year and this is what catches it.
  skip_if_no_fixtures()
  skip_if_no_solver()
  sv <- function(mod, nm) suppressMessages(suppressWarnings(
    solve_model(mod, name = nm, solver = solver_options$glpk,
                tmp.del = TRUE, wait = TRUE)))
  a <- sv(.yc_kit(nm = "yc_sf"), "yc_sf")
  b <- sv(.yc_kit(2028L, nm = "yc_ss"), "yc_ss")
  expect_equal(capture_tracked_values(a)$objective,
               capture_tracked_values(b)$objective, tolerance = 1e-9)
  # the mechanism: half the slices, each doing twice the work
  act <- function(s) {
    v <- as.data.frame(getData(s, "vTechAct", merge = TRUE, digits = 8))
    stats::aggregate(value ~ year, v, sum)
  }
  expect_equal(act(a)$value, c(4, 4, 4))
  expect_equal(act(b)$value, c(4, 2, 4))
})

.yc_stg <- function(sample_years = NULL, nm = "ycs") {
  mod <- add(.yc_kit(nm = nm), topia$modules$unit$U1$SOLAR, overwrite = TRUE)
  if (!is.null(sample_years)) {
    cal <- topia$modules$unit$calendars[["unit_s4"]]
    half <- sample_calendar(cal, c("S1", "S2"),
                                        name = "unit_s4_half")
    mod <- setCalendar(mod, half, years = sample_years)
  }
  mod
}

test_that("a sampled year's storage cycle closes within the sample", {
  # THE correctness landmine. The model calendar says S1 follows S4; a year
  # holding only S1 and S2 must instead chain S1 -> S2 -> S1. Take the
  # relation from the model calendar and the level at S1 is paired with a
  # predecessor the year does not have -- an NA that the validator rejects, or
  # a dropped row, which leaves the level unconstrained: free energy, reported
  # OPTIMAL.
  skip_if_no_fixtures()
  a <- .yc_interp(.yc_stg(nm = "ycs_f"), "ycs_f")
  b <- .yc_interp(.yc_stg(2028L, nm = "ycs_s"), "ycs_s")
  ga <- function(s) {
    d <- as.data.frame(get_data_slot(s@modInp@parameters[["meqStorageLevel"]]))
    d[d$year == 2028, c("timeslice", "timeslicep")]
  }
  expect_equal(sum(is.na(ga(a)$timeslicep)), 0L)
  expect_equal(sum(is.na(ga(b)$timeslicep)), 0L)
  # the full year: the four-season cycle
  fa <- unique(ga(a))
  expect_equal(fa$timeslicep[fa$timeslice == "S1"], "S4")
  # the sampled year: the two-season cycle, closed
  fb <- unique(ga(b))
  expect_setequal(fb$timeslice, c("S1", "S2"))
  expect_equal(fb$timeslicep[fb$timeslice == "S1"], "S2")
  expect_equal(fb$timeslicep[fb$timeslice == "S2"], "S1")
  # every slice the year has is bounded by a balance equation
  expect_setequal(fb$timeslice, c("S1", "S2"))
})

# @covers vStorageLevel depth=S backends=glpk
test_that("a sampled year gains no free energy from storage", {
  skip_if_no_fixtures()
  skip_if_no_solver()
  sv <- function(mod, nm) suppressMessages(suppressWarnings(
    solve_model(mod, name = nm, solver = solver_options$glpk,
                tmp.del = TRUE, wait = TRUE)))
  oa <- capture_tracked_values(sv(.yc_stg(nm = "ycs_sf"), "ycs_sf"))$objective
  ob <- capture_tracked_values(sv(.yc_stg(2028L, nm = "ycs_ss"),
                                  "ycs_ss"))$objective
  # a broken chain makes the sampled year CHEAPER than it should be
  expect_gte(ob, oa - 1e-9)
  expect_equal(ob, oa, tolerance = 1e-9)
})

test_that("pYearFraction is each year's own coverage of the year", {
  # It used to come from `settings@yearFraction`, a slot deprecated as
  # "experimental, unwired", whose one-row prototype was RECYCLED across the
  # year rows instead of joined on year -- so it read 1 for every year however
  # the calendar was sampled. Nothing reads the parameter today (every use in
  # the GAMS template is commented out), but it is declared in all four
  # backends and would scale a sampled year wrongly if revived.
  skip_if_no_fixtures()
  a <- .yc_interp(.yc_kit(nm = "yc_yf1"), "yc_yf1")
  b <- .yc_interp(.yc_kit(2028L, nm = "yc_yf2"), "yc_yf2")
  gf <- function(s) {
    d <- as.data.frame(get_data_slot(s@modInp@parameters[["pYearFraction"]]))
    stats::setNames(d$value, as.character(d$year))
  }
  # a full calendar everywhere: every year covers the whole year
  expect_equal(unname(gf(a)), c(1, 1, 1))
  # two of four seasons cover half of it
  expect_equal(gf(b)[["2028"]], 0.5)
  expect_equal(gf(b)[["2025"]], 1)
  expect_equal(gf(b)[["2033"]], 1)
  # and it is joined on year, not recycled: a per-year value survives
  expect_equal(length(unique(gf(b))), 2L)
})

test_that("folding the timeslice axis is refused, folding the others is not", {
  # A whole-column fold asserts the value is uniform across EVERY timeslice,
  # which silently assumes the years share a timeslice set.
  skip_if_no_fixtures()
  expect_error(
    interpolate_model(.yc_kit(2028L, nm = "yc_fd"), "yc_fd", ondisk = FALSE,
                      overwrite = TRUE, fold = TRUE),
    "cannot include the timeslice axis")
  expect_error(
    interpolate_model(.yc_kit(2028L, nm = "yc_fd2"), "yc_fd2", ondisk = FALSE,
                      overwrite = TRUE, fold = "timeslice"),
    "cannot include the timeslice axis")
  # the other axes are unaffected
  expect_s4_class(
    suppressMessages(suppressWarnings(interpolate_model(
      .yc_kit(2028L, nm = "yc_fd3"), "yc_fd3", ondisk = FALSE,
      overwrite = TRUE, fold = c("region", "year")))),
    "scenario")
  # and a model with one calendar folds as it always did
  expect_s4_class(
    suppressMessages(suppressWarnings(interpolate_model(
      .yc_kit(nm = "yc_fd4"), "yc_fd4", ondisk = FALSE, overwrite = TRUE,
      fold = TRUE))),
    "scenario")
})

test_that("a sampled year rolls up to the annual level with its own weights", {
  # The roll-up ratio is child-weight over target-weight. Taking the model
  # calendar's weights for every year would understate a sampled year's
  # annual total by exactly its year_fraction.
  skip_if_no_fixtures()
  skip_if_no_solver()
  sv <- function(mod, nm) suppressMessages(suppressWarnings(
    solve_model(mod, name = nm, solver = solver_options$glpk,
                tmp.del = TRUE, wait = TRUE)))
  a <- sv(.yc_kit(nm = "yc_rf"), "yc_rf")
  b <- sv(.yc_kit(2028L, nm = "yc_rs"), "yc_rs")
  # `timeframe` takes a level NAME (or "lowest"/"all"); "highest" is not one.
  ann <- function(s) {
    v <- as.data.frame(getData(s, "vTechAct", timeframe = "ANNUAL",
                               merge = TRUE, digits = 8))
    stats::aggregate(value ~ year, v, sum)$value
  }
  # Every year delivers the same annual activity, sampled or not: the sampled
  # year's two seasons carry weight 2 and roll up to 4, exactly as the full
  # year's four seasons at weight 1 do. Read with the MODEL calendar's weights
  # instead, the sampled year would roll up to 2.
  expect_equal(ann(a), c(4, 4, 4), tolerance = 1e-8)
  expect_equal(ann(b), c(4, 4, 4), tolerance = 1e-8)
})

test_that("interpolation refuses a per-year calendar it cannot resolve", {
  skip_if_no_fixtures()
  kit <- topia$modules$unit$U1
  mod <- newModel("yc_bad", data = kit$repo,
                  calendar = topia$modules$unit$calendars[[kit$calendar]],
                  region = kit$regions,
                  horizon = topia$modules$horizons$BY2025, discount = 0)
  iv <- mod@config@horizon@intervals
  iv$calendar <- rep("nowhere", nrow(iv))
  mod@config@horizon@intervals <- iv
  expect_error(suppressMessages(interpolate_model(mod, "yc_bad",
                                                  ondisk = FALSE,
                                                  overwrite = TRUE)),
               "does not carry")
})
