# =========================================================================== #
# Milestone labels: `horizon@intervals$label` and `calendar@year_start`.
# The model key for a period stays the INTEGER `mid`; the label is display
# only. A default (January-start) model must render exactly as before.
# =========================================================================== #

test_that("an unlabelled horizon labels every milestone with its own year", {
  h <- newHorizon(2020:2050, c(1, 2, 5, 10))
  expect_true("label" %in% names(h@intervals))
  expect_identical(h@intervals$label, as.character(h@intervals$mid))
  expect_identical(unname(year_label(h)), as.character(h@intervals$mid))
  expect_identical(names(year_label(h)), as.character(h@intervals$mid))
})

test_that("adding `label` leaves start/mid/end untouched on every path", {
  # The roxygen examples of newHorizon(), which must not move.
  cases <- list(
    quote(newHorizon(2020:2050)),
    quote(newHorizon(2020:2030, c(1, 2, 5, 10))),
    quote(newHorizon(2020:2035, c(1, 2, 5, 5, 5))),
    quote(newHorizon(2020:2050, c(1, 2, 5, 7, 1))),
    quote(newHorizon(2020:2050, c(3, 2, 5, 10))),
    quote(newHorizon(intervals = data.frame(
      start = c(2030, 2031, 2034), mid = c(2030, 2032, 2037),
      end = c(2030, 2033, 2040)))),
    quote(newHorizon(period = 2020:2050, intervals = data.frame(
      start = c(2030, 2031, 2034), mid = c(2030, 2032, 2037),
      end = c(2030, 2033, 2040)))),
    quote(newHorizon(period = 2020:2040, intervals = data.frame(
      start = c(2030, 2032, 2035), mid = c(2031, 2033, 2037),
      end = c(2032, 2034, 2040))))
  )
  for (cl in cases) {
    h <- eval(cl)
    iv <- h@intervals
    expect_true(is.numeric(iv$start) && is.numeric(iv$mid) &&
                  is.numeric(iv$end), label = deparse(cl))
    expect_false(anyNA(iv$start) || anyNA(iv$mid) || anyNA(iv$end),
                 label = deparse(cl))
    expect_equal(iv$mid, trunc(iv$mid), label = deparse(cl))
    expect_identical(iv$label, as.character(iv$mid), label = deparse(cl))
    expect_identical(names(iv)[1:4], c("start", "mid", "end", "label"),
                     label = deparse(cl))
  }
})

test_that("an explicit label survives construction and wins over the default", {
  h <- newHorizon(intervals = data.frame(
    start = c(2025, 2026), mid = c(2025, 2030), end = c(2025, 2034),
    label = c("FY2025-26", "FY2030-31")))
  expect_identical(h@intervals$label, c("FY2025-26", "FY2030-31"))
  expect_equal(h@intervals$mid, c(2025, 2030))
  expect_identical(unname(year_label(h)), c("FY2025-26", "FY2030-31"))
})

test_that("the base-year split keeps the label on the row that kept `mid`", {
  # A multi-year first interval is split into a 1-year base year plus the
  # remainder; the explicit label belongs to the row still carrying `mid`.
  h <- newHorizon(intervals = data.frame(
    start = c(2025, 2030), mid = c(2025, 2030), end = c(2029, 2034),
    label = c("FY2025-26", "FY2030-31")))
  expect_equal(nrow(h@intervals), 3L)
  expect_equal(h@intervals$mid, c(2025, 2026, 2030))
  expect_identical(h@intervals$label, c("FY2025-26", "2026", "FY2030-31"))
})

test_that("a character label does not poison the interval range", {
  # `int_range <- as.list(intervals) |> unlist() |> range()` coerced the whole
  # table to character when a `label` column was present, so `period` came back
  # as a character range and the merge silently produced the wrong window.
  h <- newHorizon(period = 2020:2050, intervals = data.frame(
    start = c(2030, 2031, 2034), mid = c(2030, 2032, 2037),
    end = c(2030, 2033, 2040), label = c("a", "b", "c")))
  expect_type(h@period, "integer")
  expect_identical(range(h@period), c(2030L, 2040L))
  expect_identical(h@intervals$label, c("a", "b", "c"))
})

test_that("mid_is_end / mid_is_start keep the default label on the final mid", {
  he <- newHorizon(2020:2035, c(1, 5, 5, 5), mid_is_end = TRUE)
  expect_identical(he@intervals$label, as.character(he@intervals$mid))
  expect_identical(he@intervals$mid, he@intervals$end)
  hs <- newHorizon(2020:2035, c(1, 5, 5, 5), mid_is_start = TRUE)
  expect_identical(hs@intervals$label, as.character(hs@intervals$mid))
  expect_identical(hs@intervals$mid, hs@intervals$start)
})

test_that("bad labels are refused", {
  expect_error(newHorizon(intervals = data.frame(
    start = c(2025, 2030), mid = c(2025, 2030), end = c(2029, 2034),
    label = c("same", "same"))), "unique")
  expect_error(newHorizon(intervals = data.frame(
    start = c(2025, 2030), mid = c(2025, 2030), end = c(2029, 2034),
    label = c("ok", "  "))), "empty")
})

# --- calendar anchor ------------------------------------------------------- #

test_that("a default calendar is January-start and labels stay plain", {
  cal <- newCalendar(timetable = make_timetable())
  expect_identical(cal@year_start, list(month = 1L, day = 1L))
  expect_identical(cal@utc_offset_minutes, 0L)
  expect_false(energyRt:::.is_fiscal_year_start(cal@year_start))
})

test_that("a non-January anchor renders fiscal labels", {
  cal <- newCalendar(timetable = make_timetable(),
                     year_start = list(month = 4L, day = 1L))
  expect_identical(cal@year_start, list(month = 4L, day = 1L))
  expect_identical(
    energyRt:::.default_year_labels(c(2025L, 2030L, 2099L), cal@year_start),
    c("FY2025-26", "FY2030-31", "FY2099-00"))
  # The anchored year is the STARTING Gregorian year, as timescales documents.
  expect_identical(
    energyRt:::.default_year_labels(2021L, cal@year_start), "FY2021-22")
})

test_that("a bad anchor is refused", {
  expect_error(newCalendar(timetable = make_timetable(),
                           year_start = list(month = 13L, day = 1L)), "1:12")
  expect_error(newCalendar(timetable = make_timetable(),
                           year_start = list(month = 4L, day = 44L)), "1:31")
  expect_error(newCalendar(timetable = make_timetable(),
                           year_start = "April"), "list")
})

test_that("a model carrying a fiscal calendar labels its own milestones", {
  mdl <- newModel(name = "fy", region = "R1",
                  calendar = newCalendar(timetable = make_timetable(),
                                         year_start = list(month = 4L, day = 1L)),
                  horizon = newHorizon(2025:2034, c(1, 4, 5)))
  lb <- year_label(mdl)
  expect_true(all(grepl("^FY[0-9]{4}-[0-9]{2}$", lb)))
  # `mid` is untouched: the label never becomes the key.
  expect_true(is.numeric(mdl@config@horizon@intervals$mid))
  expect_identical(names(lb),
                   as.character(mdl@config@horizon@intervals$mid))
})

# --- ordering and shims ----------------------------------------------------- #

test_that("labels order by the horizon, not lexically", {
  # Lexical order would put "FY25" after "FY5" and "P10" before "P9".
  h <- newHorizon(intervals = data.frame(
    start = c(2025, 2026, 2031, 2036), mid = c(2025, 2030, 2035, 2040),
    end = c(2025, 2030, 2035, 2044),
    label = c("FY5", "FY25", "P9", "P10")))
  df <- data.frame(year = c(2040L, 2025L, 2035L, 2030L), value = 1:4)
  out <- energyRt:::.relabel_years(df, h)
  expect_s3_class(out$year, "ordered")
  expect_identical(levels(out$year), c("FY5", "FY25", "P9", "P10"))
  expect_identical(as.character(sort(out$year)),
                   c("FY5", "FY25", "P9", "P10"))
})

test_that("a year outside the horizon keeps its own number", {
  h <- newHorizon(2020:2030, c(1, 5, 5))
  df <- data.frame(year = c(2020L, 1999L), value = 1:2)
  out <- energyRt:::.relabel_years(df, h)
  expect_true("1999" %in% as.character(out$year))
})

test_that("objects without the new slots fall back to defaults", {
  # `save_scenario()`/`load_scenario()` do not run updateObject(), so a
  # calendar serialised before these slots must not error on read.
  expect_identical(energyRt:::.year_start(NULL), list(month = 1L, day = 1L))
  expect_identical(energyRt:::.utc_offset(NULL), 0L)
  stub <- methods::new("horizon")
  expect_identical(energyRt:::.year_start(stub), list(month = 1L, day = 1L))
  expect_length(year_label(methods::new("horizon")), 0L)
  expect_length(energyRt:::.year_label_safe("not an object"), 0L)
})
