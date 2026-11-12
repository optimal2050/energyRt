## A calendar per milestone year.
##
## A scenario has one calendar for every year, which is the right shape when
## every year is solved at the same resolution. It is the wrong shape when the
## years that carry investment decisions deserve an hourly calendar and the
## years that only carry the trajectory forward do not: an hourly year costs
## 8760 timeslices whether or not anything is decided in it.
##
## So a milestone may name a calendar of its own, and that calendar must be a
## SAMPLE of the model's: the same timeframe levels, slices drawn from the same
## universe, the same `share` per slice. What differs is which slices a year
## has (membership) and how many times each stands for (`weight`).
##
## That invariant is not politeness, it is what keeps the change small. The
## things that describe the slice UNIVERSE -- `@timeslice_ancestry`,
## `@timeslice_family`, `@timeframe_rank` and the five hierarchy maps -- stay
## year-blind, so the ~15 helpers typed `f(calendar, ...)` need no year
## argument. Drop a level instead of a member and `.complete_calendar()`
## re-derives the leaf names (`WIN_h00` -> `h00`), after which a model keyed on
## the original labels matches nothing and solves to zero in silence. Hence
## `.check_year_calendars()`, which refuses that before anything is built.
##
## Where the two halves live:
##   * `horizon@intervals$calendar` -- a calendar NAME per milestone row, or NA
##     for the model calendar. An absent column is every horizon written before
##     this existed, and means one calendar for every year.
##   * `config@misc$calendars` -- the named calendar objects themselves.
##
## `config@calendar` does not change meaning: it stays the model's declaration
## calendar, read in 82 places, and the per-year calendars are samples of it.

#' @include class-horizon.R class-config.R class-calendar.R
NULL

#' Set or get a model's calendar, for every year or for particular years
#'
#' @description
#' `setCalendar()` puts a [calendar][class-calendar] on a `config`, `model` or
#' `scenario`; `getCalendar()` reads it back. With `years`, the calendar is
#' given to those milestone years alone, and the model keeps its own for the
#' rest.
#'
#' @details
#' Pass a calendar object — an element of [calendars], or one from
#' [newCalendar()] or [calendar_samples()]. Leaving `calendar` out and giving
#' `newCalendar()`'s arguments instead builds one on the spot, which is all
#' this accepted up to version 0.92.
#'
#' With `years`, the calendar is registered in `config@misc$calendars` and
#' named on those rows of `horizon@intervals`, so a milestone can be solved on
#' a **sample** of the model calendar: hourly where the investment decisions
#' land, cheaper in the years that only carry the trajectory forward.
#'
#' Such a calendar must be a sample rather than a different calendar — the same
#' timeframe levels, slices drawn from the model calendar's own universe, the
#' same `share` for a slice the two share. What may differ is which slices a
#' year has and how many times each stands for (its `weight`). The check runs
#' here, where the call that broke it can be named: dropping a level instead of
#' sampling its members re-derives the leaf timeslice names and the year would
#' then solve to zero in silence.
#'
#' @param obj a `config`, `settings`, `model` or `scenario`.
#' @param calendar a `calendar` object, or `NULL` to build one from `...`.
#' @param years milestone years this calendar applies to, or `NULL` (default)
#'   to make it the model's calendar for every year.
#' @param ... passed to [newCalendar()] when `calendar` is not given.
#'
#' @return `setCalendar()` returns the modified object; `getCalendar()` returns
#'   a `calendar`.
#' @seealso [getYearCalendars()], [calendar_samples()], [calendars]
#' @name setCalendar
#' @aliases setCalendar getCalendar
#' @examples
#' mod <- newModel("demo", calendar = calendars$d365_h24,
#'                 horizon = newHorizon(c(2030, 2035, 2040),
#'                                      intervals = c(5, 5, 5)))
#' getCalendar(mod)@name
#'
#' # 2035 on one day per month, the other two years hourly
#' mod <- setCalendar(mod, calendars$d365_h24_1dpm, years = 2035)
#' names(getYearCalendars(mod))
#' mod@config@horizon@intervals$calendar
NULL

# The object that holds both halves. `settings` CONTAINS `config`, so a
# scenario's solve-time settings answer the same way a model's config does.
#' @noRd
.calendar_host <- function(x) {
  if (methods::is(x, "config")) return(x)          # covers `settings`
  if (methods::is(x, "model")) return(tryCatch(x@config, error = function(e) NULL))
  if (methods::is(x, "scenario")) {
    return(tryCatch(x@settings, error = function(e) NULL))
  }
  if (methods::is(x, "horizon")) return(NULL)
  NULL
}

# The `calendar` column of the horizon, or NULL when there is none to read.
# NULL is the "one calendar for every year" answer and the fast path: every
# caller below returns early on it, so a model that declares nothing pays
# nothing and behaves exactly as it did.
#' @noRd
.year_calendar_names <- function(x) {
  h <- .calendar_host(x)
  if (is.null(h)) return(NULL)
  iv <- tryCatch(h@horizon@intervals, error = function(e) NULL)
  if (!is.data.frame(iv) || !nrow(iv)) return(NULL)
  nm <- iv[["calendar"]]
  if (is.null(nm)) return(NULL)
  nm <- as.character(nm)
  nm[!nzchar(trimws(nm))] <- NA_character_
  if (all(is.na(nm))) return(NULL)
  stats::setNames(nm, as.character(as.integer(iv$mid)))
}

# The registered calendar objects, named.
#' @noRd
.calendar_registry <- function(x) {
  h <- .calendar_host(x)
  if (is.null(h)) return(list())
  reg <- tryCatch(h@misc$calendars, error = function(e) NULL)
  if (!is.list(reg)) return(list())
  reg
}

#' Calendars by milestone year
#'
#' The calendar each milestone is solved on. Years that name one of their own
#' get it; the rest get the model calendar.
#'
#' @param x a `model`, `scenario` or `config`.
#' @return A named list of `calendar` objects, named by milestone year, or
#'   `NULL` when no milestone names a calendar of its own -- which is the
#'   one-calendar-for-every-year case.
#' @seealso [setCalendar()], [calendar_samples()]
#' @export
#' @examples
#' mod <- newModel("m", calendar = calendars$d365_h24,
#'                 horizon = newHorizon(2030, intervals = 1))
#' getYearCalendars(mod)   # NULL: one calendar for every year
getYearCalendars <- function(x) {
  nm <- .year_calendar_names(x)
  if (is.null(nm)) return(NULL)
  h <- .calendar_host(x)
  reg <- .calendar_registry(x)
  out <- list()
  for (i in seq_along(nm)) {
    y <- names(nm)[i]
    out[[y]] <- if (is.na(nm[[i]])) h@calendar else reg[[nm[[i]]]]
  }
  out
}

# ---------------------------------------------------------------------------
# What each year has
# ---------------------------------------------------------------------------

# `(year, timeslice)`: the slices each milestone's calendar carries. This is
# the ONE place the per-year membership is expressed; everything downstream
# joins against it.
#
# With no milestone naming a calendar of its own it is the full cross product
# -- every year, every slice -- which is exactly what the builders produced
# before per-year calendars existed. That is why a model that declares nothing
# is bit-identical rather than merely similar.
#
# `year` is INTEGER, matching `mTechSpan` and the other map tables; the rule
# path coerces to character itself inside `.map_rule()`.
#' @noRd
.year_slice_df <- function(scen) {
  h <- .calendar_host(scen)
  if (is.null(h)) return(NULL)
  years <- as.integer(h@horizon@intervals$mid)
  if (!length(years)) return(NULL)
  yc <- getYearCalendars(scen)
  if (is.null(yc)) {
    sl <- as.character(h@calendar@timeslice_share$timeslice)
    return(data.frame(
      year = rep(years, each = length(sl)),
      timeslice = rep(sl, times = length(years)),
      stringsAsFactors = FALSE))
  }
  rows <- lapply(names(yc), function(y) {
    data.frame(year = as.integer(y),
               timeslice = as.character(yc[[y]]@timeslice_share$timeslice),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

# Join a year-less `(…, timeslice)` table onto the years it applies to. Used
# by the builders that must GATE; the ones that must stay a union over years
# project the column away again, which is the whole difference between
# `mTechTimeslice` and `mvTechAct`.
#' @noRd
.by_year_slices <- function(scen, df) {
  ys <- .year_slice_df(scen)
  if (is.null(ys) || is.null(df) || !nrow(df)) return(df)
  out <- merge(df, ys, by = "timeslice", allow.cartesian = TRUE)
  # keep `timeslice` where the callers expect it rather than first, which is
  # where merge() puts the `by` column
  cols <- c(setdiff(names(df), "timeslice"), "year", "timeslice")
  out[, cols, drop = FALSE]
}

# `(year, timeslice, weight)` for `calendar@misc$pTimesliceWeight`, which
# `obj2modInp.R` already reads in preference to broadcasting the year-less
# weight vector across every year. A sampled year's weights are its OWN -- a
# season standing for two seasons weighs 2 -- and without this the year would
# be interpolated with the full calendar's weight of 1 and silently count as a
# fraction of a year.
#' @noRd
.year_timeslice_weights <- function(scen) {
  yc <- getYearCalendars(scen)
  if (is.null(yc)) return(NULL)
  rows <- lapply(names(yc), function(y) {
    ts <- yc[[y]]@timeslice_share
    data.frame(year = as.integer(y),
               timeslice = as.character(ts$timeslice),
               weight = as.numeric(ts$weight),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

# `(year, fraction)`: how much of the year each milestone's timeslices cover.
# 1 on a full calendar, and less where a year is solved on a sample of it --
# 0.5 for two of four seasons, 0.0329 for one day per month out of an hourly
# year.
#
# Always a value per year, per-year calendars or not, because the model
# calendar can itself be a sampled one (`solve_by_sample()` does exactly
# that) and the old filling never looked at a calendar at all.
#' @noRd
.year_fraction_df <- function(scen) {
  h <- .calendar_host(scen)
  if (is.null(h)) return(NULL)
  years <- as.integer(h@horizon@intervals$mid)
  if (!length(years)) return(NULL)
  frac <- function(cal) {
    v <- suppressWarnings(as.numeric(tryCatch(cal@year_fraction,
                                              error = function(e) NA)))
    if (!length(v) || !is.finite(v[1]) || v[1] <= 0) 1 else v[1]
  }
  yc <- getYearCalendars(scen)
  if (is.null(yc)) {
    return(data.frame(year = years, fraction = frac(h@calendar),
                      stringsAsFactors = FALSE))
  }
  out <- data.frame(
    year = as.integer(names(yc)),
    fraction = vapply(yc, frac, numeric(1)),
    stringsAsFactors = FALSE)
  row.names(out) <- NULL
  out
}

# `(year, timeslice, timeslicep)`: each year's own successor relation, from
# that year's calendar. NULL when no milestone names a calendar of its own, so
# the callers keep reading the year-less maps and nothing changes.
#
# A sampled year's cycle closes within the sample -- two seasons chain
# S1 -> S2 -> S1 -- and taking the relation from the MODEL calendar instead
# would say S1's predecessor is S4. In the storage balance that leaves the
# level at S1 with no equation (an NA predecessor the validator rejects, or
# worse, no row at all and an unconstrained level: OPTIMAL with free energy).
# In the ramp maps it links a slice to one the year does not have.
#' @noRd
.year_successor_df <- function(scen, slot_nm = c("next_in_timeframe",
                                                 "next_in_year")) {
  slot_nm <- match.arg(slot_nm)
  yc <- getYearCalendars(scen)
  if (is.null(yc)) return(NULL)
  rows <- lapply(names(yc), function(y) {
    nx <- tryCatch(as.data.frame(methods::slot(yc[[y]], slot_nm)),
                   error = function(e) NULL)
    if (is.null(nx) || !nrow(nx)) return(NULL)
    data.frame(year = as.integer(y),
               timeslice = as.character(nx$timeslice),
               timeslicep = as.character(nx$timeslicep),
               stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

# Drop every `(year, timeslice)` cell a year's calendar does not have, from
# maps as well as value parameters.
#
# The builders gate what they can -- `.process_timeslice_df()` and
# `.timeframe_timeslice_df()` carry the year, so the variable domains built
# from them admit only a year's own slices. But several builders form their
# domain as a CROSS PRODUCT of a year-less timeslice table with the milestone
# list (`merge0(di, data.frame(year = milestones))`, mapping-filter.R:204, 329,
# 959 and mapping-engine.R:729), and a cross product cannot be gated at the
# source. Gating each of them separately would leave the next one written
# ungated and silent.
#
# So this is the guarantee, applied once after every map is built: the
# builders' own gating is then an optimisation (build less) rather than the
# correctness argument. `.filter_params_by_declared_timeslices()` is the same
# idea on the timeslice axis alone, and says in its own comment that "maps are
# built later and are already calendar-consistent" -- true until a year could
# have its own calendar.
#
# A no-op, and not even a scan, when no milestone names a calendar: the cells
# are then the full cross product by construction.
#' @noRd
.filter_year_slices <- function(scen, verbose = FALSE) {
  if (is.null(.year_calendar_names(scen))) return(scen)
  ys <- .year_slice_df(scen)
  if (is.null(ys) || !nrow(ys)) return(scen)
  ok <- paste(as.integer(ys$year), as.character(ys$timeslice))
  for (pn in names(scen@modInp@parameters)) {
    param <- scen@modInp@parameters[[pn]]
    if (is.null(param)) next
    d <- tryCatch(get_data_slot(param), error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0L) next
    cn <- colnames(d)
    if (!("year" %in% cn)) next
    scols <- intersect(c("timeslice", "timeslicep", "timeslice.1"), cn)
    if (!length(scols)) next
    d <- as.data.frame(d)
    y <- suppressWarnings(as.integer(d$year))
    # NA is a wildcard in the folded representation ("every year", "every
    # timeslice"), so a row is dropped only when it names a CONCRETE pair the
    # year does not have.
    keep <- Reduce(`&`, lapply(scols, function(cc) {
      s <- as.character(d[[cc]])
      is.na(y) | is.na(s) | paste(y, s) %in% ok
    }))
    if (all(keep)) next
    if (isTRUE(verbose)) {
      .say2("year-cal filter '", pn, "': ", nrow(d), " -> ", sum(keep),
            " rows")
    }
    scen <- .interp_write_param(scen, pn, d[keep, , drop = FALSE])
  }
  scen
}

# ---------------------------------------------------------------------------
# The guard
# ---------------------------------------------------------------------------

# Every per-year calendar must be a SAMPLE of the model calendar. Checked
# before any map is built, because each of these failures is silent downstream:
# an unregistered name builds a year with no slices at all, a dropped level
# renames the leaves, and a changed `share` makes `pTimesliceShare` -- which is
# year-less by design -- wrong for one of the years that share a slice.
#' @noRd
.check_year_calendars <- function(x) {
  nm <- .year_calendar_names(x)
  if (is.null(nm)) return(invisible(NULL))
  h <- .calendar_host(x)
  main <- h@calendar
  reg <- .calendar_registry(x)

  want <- unique(nm[!is.na(nm)])
  miss <- setdiff(want, names(reg))
  if (length(miss)) {
    stop("the horizon names ", length(miss), " calendar(s) the model does ",
         "not carry: ", paste(sQuote(miss), collapse = ", "),
         "\nRegister them with setCalendar(mod, cal, years = ...), or clear ",
         "the `calendar` column of horizon@intervals to use one calendar for ",
         "every year.", call. = FALSE)
  }

  lv_main <- names(main@timeframes)
  universe <- as.character(main@timeslice_share$timeslice)
  sh_main <- stats::setNames(as.numeric(main@timeslice_share$share), universe)

  for (n in want) {
    cal <- reg[[n]]
    if (!methods::is(cal, "calendar")) {
      stop("config@misc$calendars[['", n, "']] is a ", class(cal)[1],
           ", not a calendar.", call. = FALSE)
    }
    lv <- names(cal@timeframes)
    if (!identical(lv, lv_main)) {
      stop("calendar '", n, "' has the timeframe levels ",
           paste(lv, collapse = " > "), ", but the model calendar '",
           main@name, "' has ", paste(lv_main, collapse = " > "), ".",
           "\nA per-year calendar must keep EVERY level and sample its ",
           "members: dropping a level re-derives the leaf timeslice names ",
           "(WIN_h00 -> h00), after which the model's data matches nothing ",
           "and the year solves to zero without warning.",
           "\nBuild one with calendar_samples() / a subset of the model ",
           "calendar's timetable.", call. = FALSE)
    }
    sl <- as.character(cal@timeslice_share$timeslice)
    extra <- setdiff(sl, universe)
    if (length(extra)) {
      stop("calendar '", n, "' has ", length(extra), " timeslice(s) the ",
           "model calendar does not: ",
           paste(utils::head(extra, 5), collapse = ", "),
           if (length(extra) > 5) ", ..." else "",
           "\nA per-year calendar draws its slices from the model calendar's ",
           "own universe, so that one hierarchy serves every year.",
           call. = FALSE)
    }
    sh <- as.numeric(cal@timeslice_share$share)
    d <- abs(sh - sh_main[sl])
    bad <- which(d > 1e-10)
    if (length(bad)) {
      stop("calendar '", n, "' gives ", length(bad), " timeslice(s) a ",
           "different `share` than the model calendar, e.g. ", sl[bad[1]],
           ": ", format(sh[bad[1]]), " against ",
           format(sh_main[[sl[bad[1]]]]),
           "\n`share` is the fraction of the year a slice occupies and is the ",
           "same in every year that has it -- the per-year repetition belongs ",
           "in `weight`. pTimesliceShare carries no year, so a divergence ",
           "here is wrong for one of the two years.", call. = FALSE)
    }
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# The verb, for the classes that are defined later than `config`
# ---------------------------------------------------------------------------
#
# `class-config.R` is collated before `class-model.R` and `class-scenario.R`,
# so a `setMethod()` for either there is defined against a class that does not
# exist yet -- S4 keeps it and warns. `setGeoscale()`'s model/scenario methods
# live in `R/geoscale.R` for the same reason; these live here.

#' @rdname setCalendar
#' @include class-model.R class-scenario.R
#' @export
setMethod("setCalendar", signature(obj = "model"),
          function(obj, calendar = NULL, years = NULL, ...) {
  obj@config <- .set_calendar_on(obj@config, calendar, years, ...)
  obj
})

#' @rdname setCalendar
#' @export
setMethod("setCalendar", signature(obj = "scenario"),
          function(obj, calendar = NULL, years = NULL, ...) {
  # A scenario's settings are what the solve reads; its `@model` keeps the
  # model as given, so both are set or the two disagree about the calendar.
  obj@settings <- .set_calendar_on(obj@settings, calendar, years, ...)
  if (methods::is(tryCatch(obj@model, error = function(e) NULL), "model")) {
    obj@model <- setCalendar(obj@model, calendar, years, ...)
  }
  obj
})
