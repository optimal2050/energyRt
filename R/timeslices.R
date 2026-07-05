# timeslices.R ##############################################################################################################

#' Common formats of time-slices.
#' @name tsl_formats
#' @rdname timeslices
#'
#' @format A character vector with formats:
#' \describe{
#'   \item{d365}{daily time-slices, 365 a year (leap year's 366th day is disregarded)}
#'   \item{d365_h24}{time slices with year-day numbers and hours, 8760 in total}
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
#' @format A named list of `calendar` objects, including:
#' \describe{
#'   \item{season_dn}{Four seasons x day/night (8 slices).}
#'   \item{d365}{Daily resolution, 365 days.}
#'   \item{utopia_annual}{UTOPIA: annual resolution (1 slice).}
#'   \item{utopia_seasons}{UTOPIA: 4 seasons x 3 dayparts (DAY/NIGHT/PEAK) with
#'     representative shares (12 slices) -- the default UTOPIA resolution.}
#'   \item{utopia_m12h24}{UTOPIA: 12 months x 24 hours (288 slices).}
#'   \item{d365_h24}{Full hourly year: 365 days x 24 hours (8760 slices).}
#'   \item{d365_h24_subset_1day_per_month}{Representative subset: one day per
#'     month at hourly resolution (288 slices, `year_fraction` ~ 12/365).}
#' }
#' The `utopia_*` calendars are built from energyRt's own constructors; the
#' hourly `d365_h24*` calendars are imported from the IDEEA package. See
#' `data-raw/calendars.R` for the generating script.
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

# tsl_sets <- list(
#   d365 = list(
#     YDAY = paste0("d", formatC(1:365, width = 3, flag = "0"))),
#   d366 = list(
#     YDAY = paste0("d", formatC(1:366, width = 3, flag = "0"))),
#   d365_h24 = list(
#     YDAY = paste0("d", formatC(1:365, width = 3, flag = "0")),
#     HOUR = paste0("h", formatC(0:23, width = 2, flag = "0"))),
#   d366_h24 = list(
#     YDAY = paste0("d", formatC(1:366, width = 3, flag = "0")),
#     HOUR = paste0("h", formatC(0:23, width = 2, flag = "0"))),
#   m12_h24 = list(
#     MONTH = paste0("d", formatC(1:12, width = 3, flag = "0")),
#     HOUR = paste0("h", formatC(0:23, width = 2, flag = "0")))
# )
# save(tsl, file = "data/tsl_sets.RData")


#' @title Convert date-time objects to time-slice
#' @name dtm2tsl
#'
#' @param dtm vector of timepoints in Date format
#' @param format character, format of the slices
#' @param d366.as.na logical, if
#'
#' @rdname timeslices
#'
#' @return
#' Character vector with time-slices names
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

#' Mapping function between time-slices and date-time
#'
#' This set of functions converts date-time objects to model's
#' time-slices in a given format, and vice versa, maps
#' time-slices to date-time, and extracts year, month,
#' day of the year, hour.
#'
#' @name tsl2dtm
#'
#' @param tsl character vector with time-slices
#' @param format character, format of the slices
#' @param tmz time-zone
#' @param year year, used when time-slices don't store year
#' @param mday day of month, for time slices without the information
#'
#' @rdname timeslices
#'
#' @return
#' Vector in Date-Time format
#' @export
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
  # browser()
  if (is.null(format)) {
    return(NULL)
  }
  y <- NULL
  m <- NULL
  # w <- NULL
  d <- NULL
  h <- NULL
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
#' @describeIn tsl2dtm Extract year from time-slices
#'
#' @param return.null logical, valid for the cased then all values are NA, then NULL will be returned if return.null = TRUE,
#'
#' @return
#' Integer vector of years, the same length as the input vector
#'
#' @export
#'
#' @examples
#' tsl <- c("y2007_d365_h15", NA, "d151_h22", "d001", "m10_h12")
#' tsl2year(tsl)
tsl2year <- function(tsl, return.null = TRUE) {
  # browser()
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
#' Mapping function between time-slices and day of the year
#' @describeIn tsl2dtm Extract the day of the year from time-slices
#'
#' @param return.null logical, valid for the cased then all values are NA, then NULL will be returned if return.null = TRUE,
#'
#' @return
#' Integer vector of days of the year, the same length as the input vector
#' @export
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

#' Mapping function between time-slices and hour
#' @describeIn tsl2dtm Extract hour from time-slices
#'
#' @param return.null logical, valid for the cased then all values are NA, then NULL will be returned if return.null = TRUE,
#'
#' @return
#' Integer vector of hours, the same length as the input vector
#' @export
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

#' Mapping function between time-slices and month
#' @describeIn tsl2dtm Extract month from time-slices
#'
#' @param return.null logical, valid for the cased then all values are NA, then NULL will be returned if return.null = TRUE,
#' @param tsl character vector with time slices
#' @param format character, the time slices format
#'
#' @return
#' Integer vector of months, the same length as the input vector
#'
#' @export
#'
#' @examples
#' tsl2month(c("d001_h00", "d151_h22", "d365_h23"))
#' tsl2month(c("m01_h12", "m05_h02", "m10_h01"))
tsl2month <- function(tsl, format = tsl_guess_format(tsl), return.null = TRUE) {
  # browser()
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

#' Guess format of time-slices
#' @name tsl_guess_format
#'
#' @param tsl character vector of time-slice names.
#'
#' @return
#' Character vector with the guessed format of the time-slices
#' @export
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
  # browser()
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
