# timeslices.R ###############################################################
#
# The timeslice <-> datetime decomposition helpers. The time dimension is
# `timescales`' domain, so from v0.90 `tsl2dtm()`, `tsl2year()`, `tsl2yday()`,
# `tsl2hour()`, `tsl2month()` and `tsl_guess_format()` are NOT exported: they
# stay as internals because `plot.R` and `storage_duration.R` still decompose
# timeslice labels, and timescales has no equivalent yet (it exports the other
# direction, `datetime_to_timeslice()`). When it grows them, these go and the
# call sites move over.

#' Common formats of time-timeslices.
#' @name tsl_formats
#' @rdname timeslices
#'
#' @format A character vector with formats:
#' \describe{
#'   \item{d365}{daily time-timeslices, 365 a year (leap year's 366th day is disregarded)}
#'   \item{d365_h24}{time timeslices with year-day numbers and hours, 8760 in total}
#'   \item{...}{etc.}
#' }
"tsl_formats"

# tsl_formats <- c(
#   "d365", "d366",
#   "d365_h24", "d366_h24",
#
#   "y_d365", "y_d366",
#   "y_d365_h24", "y_d366_h24",
#
#   "m12_h24",
#   "y_m12_h24"
#
# )
# # save(tsl_formats, file = "data/tsl_formats.RData")

#' Sets of the common formats with structure
#'
#' @name tsl_sets
#' @rdname timeslices
#'
"tsl_sets"

#' Example calendars
#'
#' A named list of ready-to-use [calendar][class-calendar] objects covering
#' common sub-annual time resolutions. Pass any element to [newModel()] /
#' `setCalendar()`, inspect it with [plot()] / `autoplot()`, or use it as a
#' template for [newCalendar()].
#'
#' @format A named list of `calendar` objects:
#' \describe{
#'   \item{annual}{Annual resolution (1 timeslice).}
#'   \item{season_dn}{Four seasons x day/night (8 timeslices).}
#'   \item{utopia_seasons}{UTOPIA: 4 seasons x 3 dayparts (DAY/NIGHT/PEAK)
#'     with representative shares (12 timeslices).}
#'   \item{unit_s4, unit_s4h4}{Perfectly symmetric unit calendars for the
#'     hand-computable `utopia$modules$unit` kits.}
#'   \item{m12, m12a}{Monthly resolution, day-proportional shares --
#'     `m01..m12` and `JAN..DEC` labels respectively (12 timeslices).}
#'   \item{q4}{Calendar quarters `Q1..Q4`, day-proportional (4 timeslices).}
#'   \item{s4}{Meteorological seasons `WIN/SPR/SUM/FAL` in calendar order,
#'     day-proportional 90/92/92/91 shares (4 timeslices).}
#'   \item{s4_h24}{Seasons x 24 hours (96 timeslices) -- the UTOPIA base
#'     calendar.}
#'   \item{m12_h24}{Months x 24 hours (288 timeslices) -- the
#'     higher-resolution UTOPIA option.}
#'   \item{wd7_h24}{Weekday (`MON..SUN`) x 24 hours (168 timeslices).}
#'   \item{w52_h24}{Week (`w01..w52`) x 24 hours (1248 timeslices).}
#'   \item{d365}{Daily resolution, 365 days.}
#'   \item{d365_h24}{Full hourly year: 365 days x 24 hours (8760
#'     timeslices).}
#'   \item{s4_h24_subset_2seasons}{SAMPLED: `s4_h24` filtered to WIN+SUM;
#'     `year_fraction` ~ 0.499.}
#'   \item{m12_h24_subset_4months}{SAMPLED: `m12_h24` filtered to
#'     m01/m04/m07/m10; `year_fraction` ~ 0.337.}
#'   \item{m12_subset_q1}{SAMPLED: `m12` filtered to Jan-Mar;
#'     `year_fraction` = 90/365.}
#'   \item{d365_h24_1dpm}{SAMPLED: one day per month at hourly
#'     resolution (288 timeslices, `year_fraction` ~ 12/365).}
#'   \item{d365_h24_1dps}{SAMPLED: one day per season at hourly
#'     resolution (d015/d105/d196/d288, 96 timeslices,
#'     `year_fraction` ~ 4/365).}
#' }
#' The mainstream designs (`m12` .. `w52_h24` and the first three sampled
#' entries) are generated from the `timescales` catalog at DATA-BUILD time
#' only -- timescales is not a runtime dependency. Sampled calendars carry
#' `year_fraction < 1` and solve partial years natively: their timetables
#' are row subsets of the parent's (never rebuilt with
#' [make_timetable()], which would renormalise the shares) with the
#' surviving `sum(share)` passed as `year_fraction`. The hourly
#' `d365_h24*` entries come from IDEEA. See `data-raw/calendars.R` for the
#' generating script.
#'
#' @seealso [newCalendar()], [make_timetable()], [horizons]
#' @examples
#' names(calendars)
#' plot(calendars$season_dn)
"calendars"

#' Example planning horizons
#'
#' A named list of ready-to-use [horizon][class-horizon] objects with common
#' milestone-year structures. Pass any element to [newModel()] / `setHorizon()`,
#' or visualize it with [plot()] / `autoplot()`.
#'
#' @format A named list of `horizon` objects, including:
#' \describe{
#'   \item{Y2020_2060_by_5}{2020-2060 in 5-year steps (base year 2020).}
#'   \item{Y2020_2060_by_10}{2020-2060 in 10-year steps.}
#'   \item{Y2020, Y2030, Y2040, Y2050, Y2060, Y2070}{single-year horizons.}
#' }
#' Imported from the IDEEA package; see `data-raw/calendars.R` for the
#' generating script.
#'
#' @seealso [newHorizon()], [calendars]
#' @examples
#' names(horizons)
#' plot(horizons$Y2020_2060_by_5)
"horizons"



#' @title Convert date-time objects to time-timeslice
#' @name dtm2tsl
#'
#' @param dtm vector of timepoints in Date format
#' @param format character, format of the timeslices
#' @param d366.as.na logical, if
#'
#' @rdname timeslices
#'
#' @return
#' Character vector with time-timeslices names
#' @export
#'
#' @examples
#' dtm2tsl(lubridate::now())
#' dtm2tsl(lubridate::ymd("2020-12-31"))
#' dtm2tsl(lubridate::ymd("2020-12-31"), d366.as.na = FALSE)
#' dtm2tsl(lubridate::now(tzone = "UTC"), format = "d365")
#' dtm2tsl(lubridate::ymd("2020-12-31"), format = "d365")
#' dtm2tsl(lubridate::ymd("2020-12-31"), format = "d365", d366.as.na = FALSE)
#' dtm2tsl(lubridate::ymd("2020-12-31"), format = "d366")
dtm2tsl <- function(dtm, format = "d365_h24", d366.as.na = grepl("d365", format)) {
  stopifnot(is.timepoint(dtm))
  if (format == "d365_h24" | format == "d366_h24") {
    x <- paste0(
      "d", formatC(yday(dtm), width = 3, flag = "0"), "_",
      "h", formatC(hour(dtm), width = 2, flag = "0")
    )
  } else if (format == "d365" | format == "d366") {
    x <- paste0("d", formatC(yday(dtm), width = 3, flag = "0"))
  } else if (format == "y_d365_h24" | format == "y_d366_h24") {
    x <- paste0(
      "y", formatC(year(dtm), width = 4, flag = "0"), "_",
      "d", formatC(yday(dtm), width = 3, flag = "0"), "_",
      "h", formatC(hour(dtm), width = 2, flag = "0")
    )
  } else if (format == "m12_h24") {
    x <- paste0(
      "m", formatC(month(dtm), width = 2, flag = "0"), "_",
      "h", formatC(hour(dtm), width = 2, flag = "0")
    )
  }
  if (d366.as.na) {
    x[grepl("d366", x)] <- NA
  }
  return(x)
}


# check
if (F) {

}

#' Mapping function between time-timeslices and date-time
#'
#' This set of functions converts date-time objects to model's
#' time-timeslices in a given format, and vice versa, maps
#' time-timeslices to date-time, and extracts year, month,
#' day of the year, hour.
#'
#' @name tsl2dtm
#'
#' @param tsl character vector with time-timeslices
#' @param format character, format of the timeslices
#' @param tmz time-zone
#' @param year year, used when time-timeslices don't store year
#' @param mday day of month, for time timeslices without the information
#'
#' @rdname timeslices
#'
#' @return
#' Vector in Date-Time format
#' @keywords internal
#'
#' @examples
#' tsl <- c("y2007_d365_h15", NA, "d151_h22", "d001", "m10_h12")
#' tsl2dtm(tsl[1])
#' tsl2dtm(tsl[1:2])
#' tsl2dtm(tsl[2])
#' tsl2dtm(tsl[3])
#' tsl2dtm(tsl[4])
#' tsl2dtm(tsl[3], year = 2010)
#' tsl2dtm(tsl[4], year = 1900)
#' tsl2dtm(tsl[3:4], year = 1900)
tsl2dtm <- function(tsl, format = tsl_guess_format(tsl), tmz = "UTC",
                    year = NULL, mday = NULL) {
  if (is.null(format)) {
    return(NULL)
  }
  y <- NULL
  m <- NULL
  # w <- NULL
  d <- NULL
  h <- NULL
  # A format this function has no branch for (e.g. "h24", a season x hour
  # calendar) used to fall through to `return(dtm)` with `dtm` never assigned,
  # so the caller got "object 'dtm' not found" instead of an answer. NULL is the
  # same "cannot date this" signal the two early returns below already use.
  dtm <- NULL
  if (grepl("y", format)) y <- tsl2year(tsl)
  if (grepl("m", format)) m <- tsl2month(tsl)
  if (grepl("d", format)) d <- tsl2yday(tsl)
  if (grepl("h", format)) h <- tsl2hour(tsl)

  # year
  if (is.null(y) || all(is.na(y))) {
    if (is.null(year)) {
      return(NULL)
    } # not enough info to create Date object
    if (length(year) == 1) {
      y <- rep(year, length(tsl))
    } else if (length(tsl) == length(year)) {
      y <- as.integer(year)
    } else {
      stop("length of 'year' should be equal to 1 or to the length of 'tsl'")
    }
  }

  if (format %in% c("d365_h24", "d366_h24", "y_d365_h24", "y_d366_h24")) {
    # yday-based
    dtm <- lubridate::ymd_h(paste0(y, "-01-01 0"), tz = tmz) + days(d - 1) + hours(h)
  } else if (format %in% c("d365", "d366")) {
    # yday, no-hours
    dtm <- lubridate::ymd_h(paste0(y, "-01-01 0"), tz = tmz) + days(d - 1)
  } else if (format %in% c("m12_h24", "y_m12_h24")) {
    # month-based
    if (is.null(mday)) {
      return(NULL)
    } # not enough info to create Date object
    dtm <- lubridate::ymd_h(paste0(y, "-", m, "-", mday, " ", h), tz = tmz)
  }
  return(dtm)
}


# @name tsl2year
# @rdname timeslices
#' @describeIn tsl2dtm Extract year from time-timeslices
#'
#' @param return.null logical, valid for the cased then all values are NA, then NULL will be returned if return.null = TRUE,
#'
#' @return
#' Integer vector of years, the same length as the input vector
#'
#' @keywords internal
#'
#' @examples
#' tsl <- c("y2007_d365_h15", NA, "d151_h22", "d001", "m10_h12")
#' tsl2year(tsl)
tsl2year <- function(tsl, return.null = TRUE) {
  # library(stringr)
  y <- NULL
  y <- str_extract(tsl, "y[0-9]++")
  if (return.null) {
    if (all(is.na(y))) {
      return(NULL)
    }
  }
  y <- str_sub(y, 2, 5)
  y <- as.integer(y)
  return(y)
}

# @name tsl2yday
#' Mapping function between time-timeslices and day of the year
#' @describeIn tsl2dtm Extract the day of the year from time-timeslices
#'
#' @param return.null logical, valid for the cased then all values are NA, then NULL will be returned if return.null = TRUE,
#'
#' @return
#' Integer vector of days of the year, the same length as the input vector
#' @keywords internal
#'
#' @examples
#' tsl
#' tsl2yday(tsl)
tsl2yday <- function(tsl, return.null = TRUE) {
  d <- str_extract(tsl, "d[0-9]++")
  if (return.null) {
    if (all(is.na(d))) {
      return(NULL)
    }
  }
  d <- str_sub(d, 2, 4)
  d <- as.integer(d)
  return(d)
}

#' Mapping function between time-timeslices and hour
#' @describeIn tsl2dtm Extract hour from time-timeslices
#'
#' @param return.null logical, valid for the cased then all values are NA, then NULL will be returned if return.null = TRUE,
#'
#' @return
#' Integer vector of hours, the same length as the input vector
#' @keywords internal
#'
#' @examples
#' tsl
#' tsl2hour(tsl)
tsl2hour <- function(tsl, return.null = TRUE, pattern = "h[0-9]++") {
  h <- str_extract(tsl, pattern)
  if (return.null) {
    if (all(is.na(h))) {
      return(NULL)
    }
  }
  # replace non-numeric characters
  h <- str_replace_all(h, "[^0-9.]", "")
  h <- as.integer(h)
  return(h)
}

#' Mapping function between time-timeslices and month
#' @describeIn tsl2dtm Extract month from time-timeslices
#'
#' @param return.null logical, valid for the cased then all values are NA, then NULL will be returned if return.null = TRUE,
#' @param tsl character vector with time timeslices
#' @param format character, the time timeslices format
#'
#' @return
#' Integer vector of months, the same length as the input vector
#'
#' @keywords internal
#'
#' @examples
#' tsl2month(c("d001_h00", "d151_h22", "d365_h23"))
#' tsl2month(c("m01_h12", "m05_h02", "m10_h01"))
tsl2month <- function(tsl, format = tsl_guess_format(tsl), return.null = TRUE) {
  if (grepl("m[0-9]+", format)) { # has month
    m <- str_extract(tsl, "m[0-9]+")
    if (return.null) {
      if (all(is.na(m))) {
        return(NULL)
      }
    }
    m <- str_sub(m, 2, 3)
  } else if (format == "d365_h24") {
    # yday2month <- function(x) {
    dy_int <- cumsum(
      days_in_month(ymd("2001-01-15") + days(seq(0, 349, by = 30)))
    )
    yd <- tsl2yday(tsl)
    m <- cut(yd, c(0, dy_int), labels = 1:12)
    # }
  } else {
    return(NULL)
  }
  m <- as.integer(m)
  return(m)
}

#' Guess format of time-timeslices
#' @name tsl_guess_format
#'
#' @param tsl character vector of time-timeslice names.
#'
#' @return
#' Character vector with the guessed format of the time-timeslices
#' @keywords internal
#'
#' @examples
#' tsl <- c("y2007_d365_h15", NA, "d151_h22", "d001", "m10_h12")
#' tsl_guess_format(tsl)
#' tsl_guess_format(tsl[1])
#' tsl_guess_format(tsl[2])
#' tsl_guess_format(tsl[3])
#' tsl_guess_format(tsl[4])
#' tsl_guess_format(tsl[5])
tsl_guess_format <- function(tsl) {
  y <- grepl("y[0-9]+", tsl)
  ny <- sum(y, na.rm = TRUE)
  m <- grepl("m[0-9]+", tsl)
  nm <- sum(m, na.rm = TRUE)
  d <- grepl("d[0-9]+", tsl)
  nd <- sum(d, na.rm = TRUE)
  h <- grepl("h[0-9]+", tsl)
  nh <- sum(h, na.rm = TRUE)

  ii <- !is.na(tsl)
  if (!any(ii)) {
    return(NULL)
  }
  jj <- y | m | d | h # check

  format <- NULL
  if (ny > 0) {
    if (!all(y == jj)) {
      return(NULL)
    }
    format <- "y"
  }
  if (nd > 0) {
    if (!all(d == jj)) {
      return(NULL)
    }
    dd <- ifelse(any(grepl("366", tsl[ii])), 366, 365)
    format <- paste0(format, ifelse(!is.null(format), "_", ""), "d", dd)
  }
  if (nm > 0) {
    if (!all(m == jj)) {
      return(NULL)
    }
    # mm <- tsl2month(tsl[ii])
    mm <- str_extract(tsl, "m[0-9]+")
    mm <- as.integer(gsub("m", "", mm))
    if (min(mm) < 1 | max(mm) > 12) {
      return(NULL)
    }
    format <- paste0(format, ifelse(!is.null(format), "_", ""), "m", 12)
  }
  if (nh > 0) {
    if (!all(h == jj)) {
      return(NULL)
    }
    hh <- tsl2hour(tsl[ii])
    if (min(hh, na.rm = TRUE) < 0 | max(hh, na.rm = TRUE) > 23) {
      return(NULL)
    }
    format <- paste0(format, ifelse(!is.null(format), "_", ""), "h", 24)
  }
  return(format)
}

#' Convert hours (integer) values to HOUR set 'hNN'
#'
#' @param x integer vector, hours (for example, 0-23 for daily data, 0-167 for weekly data,
#' etc.)
#' @param width integer, width of the output string
#' @param prefix character, prefix to add to the name, default is 'h'
#' @param flag character, flag to add to the name, default is '0'
#'
#' @return character vector of the same length as `x` with formatted hours to
#' be used in the HOUR set.
#' @export
#'
#' @examples
#' hour2HOUR(0:23)
hour2HOUR <- function(x, width = 2, prefix = "h", flag = "0") {
  paste0(prefix, formatC(x, width = width, flag = flag))
}

#' Convert year-days to YDAY set 'dNNN'
#'
#' @param x integer vector, year-days (for example, 1-365 for annual data)
#' @param width integer, width of the output string, default is 3
#' @param prefix character, prefix to add to the name, default is 'd'
#' @param flag character, flag to add to the name, default is '0'
#'
#' @return character vector of the same length as `x` with formatted year-days to
#' be used in the YDAY set.
#' @export
#'
#' @examples
#' yday2YDAY(1:365)
yday2YDAY <- function(x, width = 3, prefix = "d", flag = "0") {
  paste0(prefix, formatC(x, width = width, flag = flag))
}


# ---------------------------------------------------------------------------
# (was R/timeslice_walk.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =========================================================================== #
# timeslice_walk.R -- sequential, chronological access to a solved scenario.
#
# Walks the calendar's finest timeslices in order and hands back the rows of a
# caller-chosen set of variables and parameters for each one. Intended for
# per-slice work that has to see the year as a sequence: storage trajectories,
# dispatch profiles, and tests that assert slice-by-slice behaviour.
#
# Why not `getData()` in a loop: `getData()` collects the whole table and then
# filters in R (`get_data.R:424-432`), so a per-slice loop over it re-reads the
# entire dataset once per slice. The storage datasets are also unpartitioned --
# one directory per table, `part-0.<ext>` (`data2disk()`, arrow.R:405) -- so a
# per-slice predicate prunes no files. Both facts point the same way: open each
# table once, read in BLOCKS, and split in memory.
#
# Three properties of the stored data shape the contract:
#
#   * the solver writes only NON-ZERO rows (`glpk/energyRt.mod:1165`), so a
#     slice with no row is a structural zero, not the end of the series;
#   * a folded parameter carries NA as a WILDCARD in `timeslice` / `region`,
#     meaning "all of them". `%in%` does not match NA, so the block predicate
#     has to admit NA explicitly or every folded parameter silently vanishes;
#   * a table with no `timeslice` column at all (capacity, for instance) is
#     year-indexed and applies to every slice; it is read once and attached
#     unchanged to each step rather than re-read.
# =========================================================================== #

#' @include arrow.R
NULL

# Chronological finest-level slices of the scenario's calendar.
#
# `@timeframes[[leaf]]` is already in the timetable's row order, and the
# timetable's row order IS the calendar's chronology (class-calendar.R:586-592);
# `@next_in_year` is derived from it by wrapping the last slice onto the first,
# so the two agree by construction.
#
# NOT `.shape_slices()`: that anchors the walk at `sort(sl)[1]` because the
# cycle it generates shapes for has no canonical start. The rotation is
# harmless there and wrong here -- on `s4_h24` it starts the year at FAL rather
# than WIN, which a plotted profile or a storage trajectory would show.
.tw_slice_order <- function(scen) {
  cal <- tryCatch(scen@settings@calendar, error = function(e) NULL)
  if (is.null(cal) || length(cal@timeframes) == 0) {
    stop("The scenario carries no calendar to walk.", call. = FALSE)
  }
  lvl <- cal@default_timeframe
  sl <- as.character(cal@timeframes[[lvl]])
  if (!length(sl)) {
    stop("The calendar's finest timeframe ('", lvl, "') has no timeslices.",
         call. = FALSE)
  }
  sl
}

# The lazy query, columns and timeslice-awareness of one requested table.
.tw_source <- function(obj, nm, kind, filter) {
  if (is.null(obj)) return(NULL)
  cols <- tryCatch(obj@colNames, error = function(e) NULL)
  qu <- get_lazy_data(obj, slot = "data", collect_data = FALSE,
                      filter = filter, optional = TRUE)
  if (is.null(qu)) return(NULL)
  if (is.null(cols) || !length(cols)) cols <- .en_filter_cols(qu)
  list(name = nm, kind = kind, query = qu, cols = cols,
       has_ts = "timeslice" %in% (cols %||% character()))
}

# Collect a table that has no timeslice dimension. Read once, reused for every
# slice.
.tw_collect_static <- function(src) {
  d <- tryCatch(dplyr::collect(src$query), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  force_cols_classes(as.data.frame(d))
}

# Rows of one timeslice-bearing table for a block of slices, keeping the NA
# wildcards that apply to every slice in it.
.tw_collect_block <- function(src, block) {
  q <- src$query
  d <- if (inherits(q, "data.frame")) {
    ts <- as.character(q$timeslice)
    q[ts %in% block | is.na(ts), , drop = FALSE]
  } else {
    # `%in%` alone drops NA, which is the wildcard a folded parameter uses to
    # mean "every timeslice" -- admit it explicitly.
    dplyr::filter(q, .data$timeslice %in% !!block | is.na(.data$timeslice)) |>
      dplyr::collect()
  }
  if (is.null(d)) return(NULL)
  force_cols_classes(as.data.frame(d))
}

#' Walk a solved scenario one timeslice at a time
#'
#' @description
#' Steps through the calendar's finest timeslices in chronological order,
#' returning the rows of the requested variables and parameters for each one.
#' Built for work that has to see the year as a sequence — storage
#' trajectories, dispatch profiles, and slice-by-slice tests.
#'
#' @details
#' Each table is opened once and read in blocks of `block` slices, with the
#' block pushed into the Arrow scanner; the block is then split in memory. That
#' is the opposite of calling [getData()] per slice, which collects the whole
#' table every time.
#'
#' Three conventions of the stored data are handled for you:
#'
#' * **Absent rows are zeros.** The solver writes only non-zero rows, so a
#'   slice with no row for a process is not the end of the series.
#' * **`NA` is a wildcard.** A folded parameter uses `NA` in `timeslice` to
#'   mean "every slice"; such rows are returned at every step.
#' * **Tables with no `timeslice` column** (capacity, lifetimes) are read once
#'   and attached unchanged to every step.
#'
#' @param scen a solved scenario.
#' @param vars character, solved variables to pull (e.g. `"vTechOut"`).
#' @param pars character, interpolated parameters to pull (e.g. `"pTechAf"`).
#' @param ... unused, for future extension.
#' @param slices character, the slices to walk and their order; defaults to the
#'   calendar's chronological order.
#' @param block integer, slices read from disk at a time. Larger is fewer,
#'   bigger reads.
#' @param filter named list of column = allowed values, pushed into the scanner
#'   (e.g. `list(region = "R1", year = 2030L)`).
#' @param fun optional function applied to each step; when given, the walk runs
#'   to completion and its results are returned as a list.
#'
#' @return With `fun`, a named list of its results, one per slice. Without, a
#'   `timeslice_walk` object: call `$next_slice()` for the next step (`NULL`
#'   when exhausted) or `$reset()` to start over.
#' @noRd
timeslice_walk <- function(scen, vars = NULL, pars = NULL, ...,
                           slices = NULL, block = 24L, filter = list(),
                           fun = NULL) {
  stopifnot(is(scen, "scenario"))
  vars <- as.character(vars %||% character())
  pars <- as.character(pars %||% character())
  if (!length(vars) && !length(pars)) {
    stop("Nothing to walk: give `vars` and/or `pars`.", call. = FALSE)
  }
  block <- max(1L, as.integer(block))

  order_all <- .tw_slice_order(scen)
  slices <- if (is.null(slices)) order_all else as.character(slices)
  unknown <- setdiff(slices, order_all)
  if (length(unknown)) {
    stop("Not timeslices of this scenario's calendar: ",
         paste(utils::head(unknown, 5), collapse = ", "),
         if (length(unknown) > 5) ", ..." else "", call. = FALSE)
  }

  srcs <- list()
  for (v in vars) {
    s <- .tw_source(scen@modOut@variables[[v]], v, "variable", filter)
    if (is.null(s)) {
      # an empty table is never written; that is normal, not an error
      warning("No stored data for variable '", v, "'; it will be empty at ",
              "every step.", call. = FALSE)
    } else {
      srcs[[v]] <- s
    }
  }
  for (p in pars) {
    s <- .tw_source(scen@modInp@parameters[[p]], p, "parameter", filter)
    if (is.null(s)) {
      warning("No stored data for parameter '", p, "'; it will be empty at ",
              "every step.", call. = FALSE)
    } else {
      srcs[[p]] <- s
    }
  }

  # year-indexed tables: read once, reused at every step
  static <- list()
  for (nm in names(srcs)) {
    if (!srcs[[nm]]$has_ts) static[[nm]] <- .tw_collect_static(srcs[[nm]])
  }
  dynamic <- names(srcs)[vapply(srcs, function(s) s$has_ts, logical(1))]

  share <- tryCatch(as.data.frame(scen@settings@calendar@timeslice_share),
                    error = function(e) NULL)
  share_of <- function(s) {
    if (is.null(share) || !"share" %in% names(share)) return(NA_real_)
    v <- share$share[as.character(share$timeslice) == s]
    if (length(v)) as.numeric(v[1]) else NA_real_
  }

  pos <- 0L
  cache <- list(block = character(), data = list())

  load_block <- function(i) {
    blk <- slices[seq(i, min(i + block - 1L, length(slices)))]
    d <- list()
    for (nm in dynamic) d[[nm]] <- .tw_collect_block(srcs[[nm]], blk)
    cache <<- list(block = blk, data = d)
  }

  step <- function() {
    if (pos >= length(slices)) return(NULL)
    pos <<- pos + 1L
    s <- slices[pos]
    if (!s %in% cache$block) load_block(pos)
    out <- list(timeslice = s, index = pos, share = share_of(s))
    for (nm in dynamic) {
      d <- cache$data[[nm]]
      out[[nm]] <- if (is.null(d) || !nrow(d)) d else {
        ts <- as.character(d$timeslice)
        d[ts == s | is.na(ts), , drop = FALSE]
      }
    }
    for (nm in names(static)) out[[nm]] <- static[[nm]]
    out
  }

  if (!is.null(fun)) {
    res <- vector("list", length(slices))
    names(res) <- slices
    for (i in seq_along(slices)) res[[i]] <- fun(step())
    return(res)
  }

  structure(
    list(next_slice = step,
         reset = function() { pos <<- 0L; invisible(NULL) },
         slices = slices, tables = names(srcs), block = block,
         scenario = scen@name),
    class = "timeslice_walk"
  )
}

# `print` is an S4 generic here whose default body is `UseMethod("print")`
# (print.R:11), so an S3 method has to be registered explicitly or dispatch
# falls through and dumps the raw list.
#' @exportS3Method print timeslice_walk
print.timeslice_walk <- function(x, ...) {
  cat("<timeslice_walk> scenario '", x$scenario, "'\n", sep = "")
  cat("  ", length(x$slices), " slices, blocks of ", x$block, "\n", sep = "")
  cat("  tables: ", paste(x$tables, collapse = ", "), "\n", sep = "")
  cat("  $next_slice() for the next step, $reset() to start over\n")
  invisible(x)
}
