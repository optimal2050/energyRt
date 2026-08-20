# =========================================================================== #
# storage_duration() -- split a storage level into duration bands
#
# A storage level time-series answers "how full is the store", but not the
# question people actually ask of it: how much of that energy is doing SHORT
# cycling (an evening peak) and how much is sitting there for weeks (a seasonal
# reserve)? Those have completely different economics and are served by
# completely different technologies, yet both show up as one `vStorageLevel`
# number.
#
# The decomposition below answers it by nested FLOORS. For a window of `w`
# hours,
#
#     floor_w[i] = max over the w windows of length w containing i,
#                  of the minimum level inside that window
#
# is the energy that never leaves the store across some w-hour stretch spanning
# `i` -- energy committed for at least `w` hours. Those floors are nested by
# construction (a longer window can only have a lower floor), so successive
# differences partition the level into bands with no overlap and no remainder:
#
#     >= w_k            floor_wk
#     w_j .. w_{j+1}    floor_wj - floor_w{j+1}
#     < w_1             level    - floor_w1
#
# Ported from the IDEEA figure script (`R/fMA.R` + `R/storage_duration_figure.R`
# in IDEEA-Internal-Repo), which computed the same quantity by sampling a
# left-aligned rolling minimum at every phase offset, filling down and taking a
# row-wise maximum. That is exactly a right-aligned rolling MAXIMUM of the
# rolling minimum -- the two are the same number, and the direct form is one
# pass instead of `w` of them. Fixed along the way: the hard-coded
# "Asia/Kolkata", a `browser()`, the pre-v0.80 `slice` vocabulary, and
# `vStorageStore`, which is now `vStorageLevel`.
#
# NOTE despite the original name, `fMA`, none of this is a moving AVERAGE. An
# average would blur the bands into each other and would not sum back to the
# level.
# =========================================================================== #

#' Format a number of hours as a compact human label
#' @keywords internal
#' @noRd
.sd_fmt_hours <- function(h) {
  vapply(h, function(x) {
    if (x < 24) return(paste0(x, "h"))
    if (x %% (24 * 7) == 0) return(paste0(x %/% (24 * 7), "w"))
    if (x %% 24 == 0) return(paste0(x %/% 24, "d"))
    paste0(x, "h")
  }, character(1))
}

#' The committed-energy floor for one window width
#'
#' `floor_w[i]` is the largest "never dips below" level over any window of
#' `w` consecutive points containing `i`.
#'
#' @param x numeric level series, in calendar order
#' @param w window width in points
#' @return numeric of the same length as `x`
#' @keywords internal
#' @noRd
.sd_floor <- function(x, w) {
  n <- length(x)
  if (w <= 1L) return(x)
  if (w >= n) return(rep(min(x, na.rm = TRUE), n))
  # rmin[s] = min(x[s .. s+w-1]) -- the floor of the window STARTING at s
  rmin <- zoo::rollapply(x, width = w, FUN = min, align = "left", fill = NA)
  # the windows containing i start at i-w+1 .. i, so take the best of those
  out <- zoo::rollapply(rmin, width = w, FUN = function(z) max(z, na.rm = TRUE),
                        align = "right", fill = NA)
  # `fill = NA` leaves the first w-1 positions empty; with cyclic padding these
  # are inside the pad and get trimmed, so this only matters when the caller
  # padded by edge replication.
  out[is.na(out)] <- suppressWarnings(min(x, na.rm = TRUE))
  out
}

#' Split a storage level into duration bands
#'
#' Decomposes `vStorageLevel` into how long the energy actually sits in the
#' store: the part cycling within a few hours, within a day, across a week, and
#' the part held for a month or more. The bands partition the level exactly --
#' they sum back to it at every timeslice -- so they can be stacked in an area
#' chart directly.
#'
#' @param scen a solved scenario.
#' @param width integer vector of window widths in HOURS, strictly increasing.
#'   The default splits at half a day, a day, a week and 30 days, producing five
#'   bands. Widths at or above the length of the series collapse to the series
#'   minimum, which is the honest answer rather than an error.
#' @param labels optional character vector naming the bands, from shortest to
#'   longest. Must be `length(width) + 1`. Defaults to labels derived from
#'   `width`, e.g. `"<12h"`, `"12h-1d"`, `"1d-1w"`, `"1w-30d"`, `">30d"`.
#' @param cyclic how to pad the ends of the series before the rolling windows.
#'   `NA` (default) takes it from each storage's `@fullYear`: a store whose cycle
#'   closes over the year genuinely continues into itself, so the head and tail
#'   are wrapped. `FALSE` repeats the edge values instead. `TRUE`/`FALSE` force
#'   one choice for every storage.
#' @param tmz time zone used to build the datetime column. `"UTC"` by default;
#'   the original IDEEA script hard-coded `"Asia/Kolkata"`, which silently moved
#'   every model's clock.
#' @param stg optional character vector restricting the storages considered.
#'
#' @return A data frame with one row per storage, commodity, region, year,
#'   timeslice and band: columns `stg`, `comm`, `region`, `year`, `timeslice`,
#'   `datetime`, `duration` (an ordered factor, shortest first) and `value`.
#'   Zero rows if the scenario has no storage level to decompose.
#'
#' @section Reading the result:
#' A store cycling within its parent timeframe (`@fullYear = FALSE`) cannot hold
#' energy for longer than that cycle, so its long-duration bands come out at or
#' near zero. That is a property of the model, not a failure of the split.
#'
#' @examples
#' \dontrun{
#' d <- storage_duration(scen)
#' library(ggplot2)
#' ggplot(d, aes(datetime, value, fill = duration)) +
#'   geom_area() +
#'   facet_wrap(~region)
#' }
#' @export
storage_duration <- function(scen,
                             width = c(12L, 24L, 24L * 7L, 24L * 30L),
                             labels = NULL,
                             cyclic = NA,
                             tmz = "UTC",
                             stg = NULL) {
  width <- sort(unique(as.integer(width)))
  if (length(width) == 0L || any(width < 1L)) {
    stop("`width` must be one or more positive numbers of hours.", call. = FALSE)
  }
  if (is.null(labels)) {
    f <- .sd_fmt_hours(width)
    labels <- c(paste0("<", f[1]),
                if (length(f) > 1) paste0(f[-length(f)], "-", f[-1]),
                paste0(">", f[length(f)]))
  }
  if (length(labels) != length(width) + 1L) {
    stop("`labels` must have one more element than `width` (", length(width) + 1L,
         "), one per band.", call. = FALSE)
  }

  lev <- try(as.data.frame(getData(scen, "vStorageLevel", merge = TRUE,
                                   drop.zeros = FALSE)), silent = TRUE)
  empty <- data.frame(stg = character(), comm = character(), region = character(),
                      year = integer(), timeslice = character(),
                      datetime = as.POSIXct(character(), tz = tmz),
                      duration = factor(character(), levels = labels, ordered = TRUE),
                      value = numeric(), stringsAsFactors = FALSE)
  if (inherits(lev, "try-error") || is.null(lev) || !NROW(lev)) return(empty)
  if (!is.null(stg)) lev <- lev[lev$stg %in% stg, , drop = FALSE]
  if (!NROW(lev)) return(empty)

  # Which storages wrap. Read from the objects rather than from a map so this
  # works on a scenario whose maps were dropped after solving.
  fy <- tryCatch(
    apply_to_scenario_data(scen = scen, classes = "storage", as_list = TRUE,
      func = function(x) {
        o <- list()
        o[[x@name]] <- data.frame(stg = x@name, fullYear = isTRUE(x@fullYear),
                                  stringsAsFactors = FALSE)
        o
      }),
    error = function(e) NULL)
  fy <- if (is.null(fy) || !length(fy)) NULL else dplyr::bind_rows(fy)

  fmt <- tsl_guess_format(unique(lev$timeslice))
  if (is.null(fmt)) {
    stop("Cannot read a calendar format from the timeslice names, so the level ",
         "cannot be put in time order. `storage_duration()` needs a dated ",
         "calendar (day/hour timeslices).", call. = FALSE)
  }

  keys <- c("stg", "comm", "region", "year")
  keys <- intersect(keys, names(lev))
  out <- lapply(split(lev, lev[keys], drop = TRUE), function(g) {
    if (!NROW(g)) return(NULL)
    g$datetime <- tsl2dtm(g$timeslice, format = fmt, year = g$year, tmz = tmz)
    if (is.null(g$datetime) || all(is.na(g$datetime))) return(NULL)
    g <- g[order(g$datetime), , drop = FALSE]
    x <- g$value
    x[is.na(x)] <- 0

    wrap <- if (!is.na(cyclic)) isTRUE(cyclic) else {
      if (is.null(fy)) TRUE else {
        i <- match(g$stg[1], fy$stg)
        if (is.na(i)) TRUE else isTRUE(fy$fullYear[i])
      }
    }
    n <- length(x)
    pad <- min(max(width), n)
    xp <- if (wrap) {
      c(utils::tail(x, pad), x, utils::head(x, pad))
    } else {
      c(rep(x[1], pad), x, rep(x[n], pad))
    }
    keep <- (pad + 1L):(pad + n)

    floors <- lapply(width, function(w) .sd_floor(xp, min(w, length(xp)))[keep])
    names(floors) <- paste0("w", width)

    k <- length(width)
    bands <- vector("list", k + 1L)
    bands[[1]] <- x - floors[[1]]                       # shorter than width[1]
    if (k > 1L) {
      for (j in seq_len(k - 1L)) bands[[j + 1L]] <- floors[[j]] - floors[[j + 1L]]
    }
    bands[[k + 1L]] <- floors[[k]]                      # longer than width[k]

    res <- do.call(rbind, lapply(seq_along(bands), function(j) {
      d <- g[, c(keys, "timeslice"), drop = FALSE]
      d$datetime <- g$datetime
      d$duration <- labels[j]
      # tiny negatives are floating-point noise on differences of equal floors
      v <- bands[[j]]
      v[abs(v) < 1e-10] <- 0
      d$value <- v
      d
    }))
    res
  })
  out <- do.call(rbind, out[!vapply(out, is.null, logical(1))])
  if (is.null(out) || !NROW(out)) return(empty)
  out$duration <- factor(out$duration, levels = labels, ordered = TRUE)
  rownames(out) <- NULL
  out
}
