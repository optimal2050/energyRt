# timeslices.R ###############################################################
#
# Calendar datasets and the chronological walk over a solved scenario.
#
# The timeslice <-> datetime conversions moved to timescales on 2026-09-26
# (see drafts/deprecated-timeslice-conversions-2026-09-26.R). They read the
# timeframe off the CALENDAR now rather than parsing the label text, so they
# take the calendar: timescales::tsl2hour(tsl, calendar). `.ts_calendar()`
# below turns an energyRt calendar into the one they expect.


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
#' Only GENERIC calendars are shipped. Calendars belonging to a particular
#' model travel with it: the TOPIA teaching calendar is
#' `topia$modules$calendars$topia_seasons`, and the unit kits' symmetric
#' calendars are `topia$modules$unit$calendars`.
#'
#' @format A named list of `calendar` objects:
#' \describe{
#'   \item{annual}{Annual resolution (1 timeslice).}
#'   \item{s4_hp3}{Four seasons x three hour types `DAY/NIGHT/PEAK`,
#'     day-proportional seasons and a uniform 12/8/4 split (12 timeslices).}
#'   \item{m12, m12a}{Monthly resolution, day-proportional shares --
#'     `m01..m12` and `JAN..DEC` labels respectively (12 timeslices).}
#'   \item{q4}{Calendar quarters `Q1..Q4`, day-proportional (4 timeslices).}
#'   \item{s4}{Meteorological seasons `WIN/SPR/SUM/FAL` in calendar order,
#'     day-proportional 90/92/92/91 shares (4 timeslices).}
#'   \item{s4_h24}{Seasons x 24 hours (96 timeslices) -- the TOPIA base
#'     calendar.}
#'   \item{m12_h24}{Months x 24 hours (288 timeslices) -- the
#'     higher-resolution TOPIA option.}
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
#' plot(calendars$s4_hp3)
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

# --------------------------------------------------------------------------
# The bridge to timescales
# --------------------------------------------------------------------------

#' An energyRt calendar as a timescales Calendar
#'
#' The timeslice conversions live in timescales and take a `Calendar`.
#' `@timetable` already IS a timescales leaftable -- one row per finest
#' timeslice, one column per timeframe, plus `share`/`weight` -- so the
#' conversion is a relabelling, not a rebuild.
#'
#' Building from the table rather than looking the name up in timescales'
#' catalog matters: the sampled and subset calendars (`d365_h24_1dps`,
#' `m12_h24_subset_4months`, ...) are constructed here and have no entry there.
#' `ANNUAL` is dropped because it is a single-member column, not a timeframe
#' the conversions can read anything off.
#'
#' @param calendar An energyRt [calendar][class-calendar], or `NULL`.
#' @return A `timescales::Calendar`, or `NULL` when the calendar carries no
#'   timeframe below `ANNUAL`.
#' @keywords internal
#' @noRd
.ts_calendar <- function(calendar) {
  if (is.null(calendar)) return(NULL)
  if (inherits(calendar, "timescales::Calendar")) return(calendar)
  if (is.character(calendar) && length(calendar) == 1L) {
    return(tryCatch(timescales::calendar(calendar), error = function(e) NULL))
  }
  if (!methods::is(calendar, "calendar")) return(NULL)
  tt <- as.data.frame(calendar@timetable)
  tfs <- setdiff(names(tt), c("timeslice", "share", "weight", "ANNUAL"))
  if (length(tfs) == 0L) return(NULL)
  keep <- tt[, c(tfs, "timeslice", "share", "weight"), drop = FALSE]
  # energyRt names timeframes freely -- `DAY` is common here and is timescales'
  # `YDAY`. Rename by what the labels ARE rather than by a synonym table, so a
  # column named anything at all still lands on the right timeframe.
  core <- vapply(tfs, function(f) .core_timeframe(unique(keep[[f]])),
                 character(1))
  named <- !is.na(core) & core != tfs & !core %in% tfs
  if (any(named)) {
    names(keep)[match(tfs[named], names(keep))] <- core[named]
    tfs[named] <- core[named]
  }
  tryCatch(
    timescales::calendar_from_leaftable(
      keep, timeframes = tfs,
      name = calendar@name,
      year_fraction = calendar@year_fraction),
    error = function(e) NULL)
}

#' The core timeframe a set of timeslice labels belongs to
#'
#' Asks the token registry which vocabulary contains them, preferring an exact
#' match over the smallest superset. `NA` when none does.
#' @noRd
.core_timeframe <- function(labels) {
  labels <- unique(labels[!is.na(labels)])
  if (!length(labels)) return(NA_character_)
  best <- NA_character_
  best_n <- Inf
  for (tok in timescales::list_calendar_tokens()) {
    e <- timescales::get_calendar_token(tok)
    vocab <- e$expand()$label
    if (!all(labels %in% vocab)) next
    if (setequal(vocab, labels)) return(e$timeframe)
    if (length(vocab) < best_n) { best <- e$timeframe; best_n <- length(vocab) }
  }
  best
}
