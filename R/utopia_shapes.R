# utopia_shapes.R -- deterministic SYNTHETIC input shapes for teaching and
# testing, the geometric counterpart of the realistic curves in
# utopia_profiles.R. One generator, four shapes:
#
#   step    -- ascending staircase 0, 1/(n-1), ..., 1 (n = `levels`): the
#              piecewise-constant family (levels = 2 is on/off -- the canonical
#              storage case: the resource is free in half the slices and absent
#              in the other half);
#   sine    -- smooth wave, sampled at each timeslice's midpoint;
#   cosine  -- the same wave shifted a quarter period;
#   hex     -- trapezoid ramp / plateau / ramp in equal thirds (the honeycomb
#              silhouette): piecewise-LINEAR between the constant and smooth
#              families.
#
# Unlike utopia_profiles() (locked to the three shipped realistic calendars),
# these work on ANY calendar: slice positions come from the calendar's own
# chronological chain (`@next_in_year` / `@next_in_timeframe`) and widths from
# `@timeslice_share`, so unequal slices land where they belong.

# Resolve a calendar argument: a `calendar` object, a shipped calendar's name,
# or a timetable data.frame (built into a calendar on the fly).
.shape_calendar <- function(calendar) {
  if (methods::is(calendar, "calendar")) return(calendar)
  if (is.character(calendar) && length(calendar) == 1) {
    cal <- energyRt::calendars[[calendar]]
    if (is.null(cal)) {
      stop("unknown calendar '", calendar, "'; shipped names: ",
           paste(names(energyRt::calendars), collapse = ", "),
           ". Pass a calendar object or a timetable data.frame instead.",
           call. = FALSE)
    }
    return(cal)
  }
  if (is.data.frame(calendar)) {
    return(newCalendar(timetable = data.table::as.data.table(calendar)))
  }
  stop("`calendar` must be a calendar object, a shipped calendar name, ",
       "or a timetable data.frame.", call. = FALSE)
}

# Chronologically ordered finest-level timeslices of a calendar. The cycle has
# no canonical start (storage wraps around), so the walk deterministically
# starts at the alphabetically first slice; `phase` rotates the shape when a
# different anchor is wanted.
.shape_slices <- function(cal) {
  lvl <- cal@default_timeframe
  sl <- as.character(cal@timeframes[[lvl]])
  if (length(sl) <= 1L) return(sl)
  nxt <- as.data.frame(cal@next_in_year)
  chain <- stats::setNames(as.character(nxt$timeslicep),
                           as.character(nxt$timeslice))
  chain <- chain[names(chain) %in% sl]
  out <- character(length(sl))
  out[1] <- sort(sl)[1]
  for (i in seq_len(length(sl) - 1L)) {
    out[i + 1L] <- chain[[out[i]]]
    if (is.na(out[i + 1L])) {
      stop("calendar's next_in_year chain is broken at '", out[i], "'",
           call. = FALSE)
    }
  }
  out
}

# Midpoint positions in [0, 1) for ordered slices with (possibly unequal)
# shares: x_i = (share before i + share_i / 2) / total.
.shape_positions <- function(shares) {
  tot <- sum(shares)
  (cumsum(shares) - shares / 2) / tot
}

# Evaluate one shape at positions x in [0, 1). Returns list(value in [0, 1],
# step = integer index or NA).
.shape_eval <- function(shape, x, levels) {
  x <- x %% 1
  switch(shape,
    step = {
      k <- pmin(levels - 1L, as.integer(floor(x * levels)))
      list(value = if (levels > 1L) k / (levels - 1L) else rep(1, length(x)),
           step = k)
    },
    sine   = list(value = (sin(2 * pi * x) + 1) / 2, step = rep(NA_integer_, length(x))),
    cosine = list(value = (cos(2 * pi * x) + 1) / 2, step = rep(NA_integer_, length(x))),
    hex = {
      v <- ifelse(x < 1 / 3, 3 * x, ifelse(x < 2 / 3, 1, 3 * (1 - x)))
      list(value = v, step = rep(NA_integer_, length(x)))
    },
    stop("unknown shape '", shape, "'", call. = FALSE)
  )
}

#' Synthetic input shapes on a calendar (deterministic)
#'
#' Generate a simple geometric profile -- a step-wise staircase, a sine/cosine
#' wave, or a hexagonal trapezoid -- over the timeslices of a calendar, ready
#' to use as weather (`wval`), availability multipliers, or demand shapes.
#' The synthetic counterpart of the realistic [utopia_profiles()]: where those
#' answer "what does a year look like", these answer "what is the simplest
#' input that isolates one mechanism" (storage bridging an on/off resource,
#' techs following a ramp, trade smoothing opposite phases).
#'
#' Despite the shared prefix, this function reads no UTOPIA data and is not
#' limited to UTOPIA: it works on any calendar and returns a single data.frame.
#' [utopia_profiles()] (plural) is the other one -- it expands UTOPIA's saved
#' weather / demand / stock to regions and returns a list of three.
#'
#' The shape spans `period` once: `"year"` stretches it over the whole
#' calendar; `"frame"` repeats it inside each parent timeframe (e.g. each
#' season's hours get the same diurnal shape). Slice positions are the
#' midpoints of each timeslice's `share` along the calendar's own
#' chronological chain, so unequal slices land where they belong; the cycle's
#' anchor is the alphabetically first timeslice (rotate with `phase`).
#'
#' @param shape `"step"` (ascending staircase with `levels` plateaus:
#'   0, 1/(levels-1), ..., 1), `"sine"`, `"cosine"`, or `"hex"` (trapezoid
#'   ramp/plateau/ramp in equal thirds).
#' @param levels integer >= 2, number of plateaus for `shape = "step"`
#'   (ignored otherwise). `levels = 2` is the on/off case.
#' @param calendar a [calendar] object, the name of a shipped calendar (see
#'   [calendars]), or a timetable `data.frame`.
#' @param regions optional character vector. `NULL` returns the bare shape
#'   (`timeslice`, `step`, `value`); otherwise the shape is replicated per
#'   region with a leading `region` column.
#' @param vary per-region variation (needs `regions`): `"none"` replicates
#'   identically; `"phase"` shifts region i by `(i-1)/n` of the period (a
#'   gradient across the map -- neighbouring regions peak in sequence);
#'   `"amplitude"` scales region i's amplitude by `1 - (i-1)/(2(n-1))`
#'   (from 1 down to 1/2 -- good sites to poor sites).
#' @param min,max numeric range the unit shape is scaled to
#'   (`value = min + shape * (max - min)`).
#' @param period `"year"` (one cycle over the calendar) or `"frame"` (one
#'   cycle inside each parent timeframe of the finest level).
#' @param phase numeric, fraction of the period the shape is shifted by
#'   (applied to all regions, on top of `vary = "phase"` offsets).
#'
#' @return A tidy `data.frame`, chronologically ordered: `region` (only when
#'   `regions` is given), `timeslice` (the calendar's finest level), `step`
#'   (integer plateau index `0..levels-1` for `"step"`, `NA` otherwise -- the
#'   pretty-number handle for hand-checkable inputs), and `value` in
#'   `[min, max]`.
#'
#' @examples
#' # on/off resource on the 4-slice test calendar: free half the year
#' utopia_profile("step", levels = 2, calendar = "utopia_seasons")
#'
#' # a diurnal sine for every season, phased across three regions
#' head(utopia_profile("sine", calendar = "s4_h24",
#'                     regions = paste0("R", 1:3), vary = "phase",
#'                     period = "frame"))
#' @seealso [utopia_profiles()] for the realistic UTOPIA curves, [utopia] for
#'   the reference dataset and the teaching kits.
#' @family utopia
#' @export
utopia_profile <- function(shape = c("step", "sine", "cosine", "hex"),
                           levels = 4L,
                           calendar = "s4_h24",
                           regions = NULL,
                           vary = c("none", "phase", "amplitude"),
                           min = 0, max = 1,
                           period = c("year", "frame"),
                           phase = 0) {
  shape <- match.arg(shape)
  vary <- match.arg(vary)
  period <- match.arg(period)
  levels <- as.integer(levels)
  stopifnot(length(levels) == 1L, levels >= 2L || shape != "step",
            is.numeric(min), is.numeric(max), max >= min,
            is.numeric(phase), length(phase) == 1L)
  if (!is.null(regions)) stopifnot(is.character(regions), length(regions) > 0)
  if (vary != "none" && is.null(regions)) {
    stop("`vary = \"", vary, "\"` needs `regions`.", call. = FALSE)
  }

  cal <- .shape_calendar(calendar)
  sl <- .shape_slices(cal)
  shr <- as.data.frame(cal@timeslice_share)
  shares <- shr$share[match(sl, as.character(shr$timeslice))]

  if (period == "year" || length(cal@timeframes) < 3L) {
    x <- .shape_positions(shares)
  } else {
    # one cycle per parent frame: positions restart inside each parent,
    # in chain order (sl is already chronological, so parents are contiguous)
    anc <- as.data.frame(cal@timeslice_ancestry)
    parent <- as.character(anc$parent)[match(sl, as.character(anc$child))]
    x <- numeric(length(sl))
    for (p in unique(parent)) {
      i <- which(parent == p)
      x[i] <- .shape_positions(shares[i])
    }
  }

  one <- function(region, extra_phase, amp) {
    ev <- .shape_eval(shape, x - phase - extra_phase, levels)
    df <- data.frame(timeslice = sl, step = ev$step,
                     value = min + ev$value * amp * (max - min),
                     stringsAsFactors = FALSE)
    if (!is.null(region)) df <- cbind(region = region, df, stringsAsFactors = FALSE)
    df
  }

  if (is.null(regions)) return(one(NULL, 0, 1))
  n <- length(regions)
  out <- lapply(seq_len(n), function(i) {
    one(regions[i],
        extra_phase = if (vary == "phase") (i - 1) / n else 0,
        amp = if (vary == "amplitude" && n > 1) 1 - (i - 1) / (2 * (n - 1)) else 1)
  })
  do.call(rbind, out)
}
