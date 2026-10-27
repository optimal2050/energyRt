# =============================================================================#
# plot_sequence.R — reading and drawing what a solve sequence covered.
#
# The coverage extractor and the autoplot methods over it, together, as
# plot_scenario.R keeps getMix() beside autoplot.scenario.
#
# Every driver records the units it solved, but each records them differently:
# `by_sample` keeps a spec of timeslices, `by_region` a list of regions per
# unit, `recursive` decided years plus look-ahead. Reading a sequence's design
# therefore meant knowing which driver wrote it.
#
# `sequence_coverage()` puts all four on one shape — one row per (unit,
# element) — so the questions that matter about an ensemble can be asked once:
# does it tile the scale, does it overlap, and what did it miss. The plots are
# thin layers over this frame, and the frame is useful on its own.
# =============================================================================#

.coverage_cols <- function() {
  tibble(unit = character(0), dim = character(0), key = character(0),
         role = character(0), count = integer(0))
}

#' What a solve sequence covered
#'
#' @description
#' One row per unit of work and element covered, for any sequence from
#' [solve_by_sample()], [solve_by_region()], [solve_recursive()] or
#' [solve_guided()].
#'
#' @details
#' The `dim` column names the scale the units are drawn on — `"timeslice"` for
#' a calendar sample, `"region"` for a spatial one, `"year"` for a horizon
#' walk — so a caller can join the result onto the matching
#' `timescales::Calendar` or `geoscales::GeoScale`.
#'
#' `role` separates what a unit **decided** from what it merely **saw**: a
#' recursive step's look-ahead milestones are solved for guidance and their
#' decisions discarded, so they are coverage of a different kind and are
#' labelled `"lookahead"`. Everything else is `"covered"`.
#'
#' `count` is the multiplicity of an element within its unit. It is 1 except
#' for a bootstrap calendar sample, where a timeslice drawn twice carries a
#' doubled share — overlap *within* a unit rather than between units.
#'
#' @section Reading the result:
#' Two questions are worth asking of it directly. Whether the units partition
#' the scale: `any(duplicated(res$key[res$role == "covered"]))` is TRUE when
#' two units claim the same element. And what was missed: compare `res$key`
#' against the scale's own members — the elements absent from the frame were
#' never solved, which is what a gap in an ensemble looks like.
#'
#' @param x a `by_sample`, `by_region`, `recursive` or `guided` sequence.
#' @param ... unused.
#'
#' @return a tibble with `unit`, `dim`, `key`, `role` and `count`.
#' @seealso [solve_by_sample()], [solve_by_region()], [solve_recursive()]
#' @export
#' @examples
#' \dontrun{
#' cv <- sequence_coverage(bs)
#' # did the samples tile the calendar, or overlap?
#' any(duplicated(cv$key[cv$role == "covered"]))
#' }
sequence_coverage <- function(x, ...) UseMethod("sequence_coverage")

#' @rdname sequence_coverage
#' @export
sequence_coverage.default <- function(x, ...) {
  stop("sequence_coverage() needs a solve sequence (by_sample, by_region, ",
       "recursive or guided); got ", class(x)[1], ".", call. = FALSE)
}

#' @rdname sequence_coverage
#' @export
sequence_coverage.by_sample <- function(x, ...) {
  sp <- x$samples
  # a rebuilt sequence has no spec: the timeslice membership of a sampling
  # draw is working state and is never written to disk (see read_sequence())
  if (is.null(sp) || !nrow(sp)) {
    if (isTRUE(x$reconstructed)) {
      stop("This by_sample sequence was rebuilt from disk, and a sampling ",
           "spec's timeslice membership is not stored, so its coverage ",
           "cannot be recovered. Re-run the driver to inspect coverage.",
           call. = FALSE)
    }
    return(.coverage_cols())
  }
  tibble(unit = as.character(sp$sample), dim = "timeslice",
         key = as.character(sp$timeslice), role = "covered",
         count = as.integer(sp$count %||% 1L))
}

#' @rdname sequence_coverage
#' @export
sequence_coverage.by_region <- function(x, ...) {
  rr <- x$runs
  if (is.null(rr) || !nrow(rr)) return(.coverage_cols())
  out <- lapply(seq_len(nrow(rr)), function(i) {
    rg <- as.character(rr$regions[[i]])
    if (!length(rg)) return(NULL)
    tibble(unit = as.character(rr$region[i]), dim = "region", key = rg,
           role = "covered", count = 1L)
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) .coverage_cols() else bind_rows(out)
}

#' @rdname sequence_coverage
#' @export
sequence_coverage.recursive <- function(x, ...) {
  st <- x$steps
  if (is.null(st) || !nrow(st)) return(.coverage_cols())
  out <- lapply(seq_len(nrow(st)), function(i) {
    lab <- paste0("s", formatC(st$step[i], width = 2, flag = "0"))
    dec <- as.integer(st$decided[[i]] %||% integer(0))
    la <- if (!is.null(st[["lookahead"]])) {
      as.integer(st$lookahead[[i]] %||% integer(0))
    } else {
      integer(0)
    }
    rows <- list()
    if (length(dec)) {
      rows[[1]] <- tibble(unit = lab, dim = "year", key = as.character(dec),
                          role = "decided", count = 1L)
    }
    if (length(la)) {
      rows[[2]] <- tibble(unit = lab, dim = "year", key = as.character(la),
                          role = "lookahead", count = 1L)
    }
    if (!length(rows)) NULL else bind_rows(rows)
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) .coverage_cols() else bind_rows(out)
}

#' @rdname sequence_coverage
#' @export
sequence_coverage.guided <- function(x, ...) {
  st <- x$stages
  if (is.null(st) || !nrow(st)) return(.coverage_cols())
  out <- lapply(seq_len(nrow(st)), function(i) {
    yrs <- as.integer(st$years[[i]] %||% integer(0))
    if (!length(yrs)) return(NULL)
    tibble(unit = as.character(st$stage[i]), dim = "year",
           key = as.character(yrs), role = "covered", count = 1L)
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) .coverage_cols() else bind_rows(out)
}

# ---------------------------------------------------------------------------#
# The plots.
#
# Each one left-joins the coverage frame onto the scale's own elements: the
# frame says what was solved, the scale says what there was to solve, so an
# element no unit claimed is a grey cell rather than an absent row. That is the
# point of drawing these at all -- a gap in an ensemble is invisible in a table
# of what ran.
#
# Fill is the unit, discretely. The scale packages' value plots
# (`timescales::calendar_wall_plot()`, `geoscales::geoscale_plot()`) map a
# numeric `z` continuously, which cannot label a sample or tell a shared cell
# from an interpolated one, so the layout comes from them and the drawing is
# here.
# ---------------------------------------------------------------------------#

.cov_shared <- function() "(shared)"

# Unit levels in solve order, with the synthetic fill last so it reads as an
# exception in the legend rather than as another unit.
.cov_unit_levels <- function(cv) {
  c(unique(as.character(cv$unit)), .cov_shared())
}

# One row per element, with the units that claimed it collapsed. `n_units` is
# kept so the plot can report overlap instead of quietly drawing the first.
.cov_by_key <- function(cv, role = NULL) {
  if (!is.null(role)) cv <- cv[cv$role %in% role, , drop = FALSE]
  if (!nrow(cv)) {
    return(tibble(key = character(0), unit = character(0),
                  n_units = integer(0), count = integer(0)))
  }
  sp <- split(seq_len(nrow(cv)), as.character(cv$key))
  tibble(
    key = names(sp),
    unit = vapply(sp, function(i) {
      u <- unique(as.character(cv$unit[i]))
      if (length(u) == 1L) u else .cov_shared()
    }, character(1)),
    n_units = vapply(sp, function(i) {
      length(unique(as.character(cv$unit[i])))
    }, integer(1)),
    count = vapply(sp, function(i) max(as.integer(cv$count[i])), integer(1)))
}

# The subtitle every coverage plot carries: how much of the scale was solved,
# and the two kinds of overlap, named. Read off the frame that was drawn, so it
# is true of the picture above it.
.cov_subtitle <- function(byk, n_units, n_total, what) {
  txt <- sprintf("%d %s | %s of %s %s covered", n_units,
                 if (n_units == 1L) "unit" else "units",
                 format(nrow(byk), big.mark = ","),
                 format(n_total, big.mark = ","), what)
  shared <- sum(byk$n_units > 1L)
  if (shared > 0L) {
    txt <- paste0(txt, sprintf(" | %d claimed by more than one unit", shared))
  }
  reps <- sum(byk$count > 1L)
  if (reps > 0L) {
    txt <- paste0(txt, sprintf(" | %d drawn more than once", reps))
  }
  txt
}

.cov_fill_scale <- function() {
  ggplot2::scale_fill_viridis_d(na.value = "grey90", drop = FALSE,
                                option = "D", end = 0.92)
}

#' Draw what a solve sequence covered
#'
#' @description
#' `autoplot()` over [sequence_coverage()]: the units of a sequence drawn on
#' the scale they were drawn from, so tiling, overlap and gaps are shapes to be
#' seen rather than properties to be inferred from a table of runs.
#'
#' @details
#' One method per driver, because each samples a different scale.
#'
#' * `by_sample` draws the calendar. `type = "coverage"` is a tile grid over
#'   the calendar's two finest timeframes and works for any calendar;
#'   `type = "wall"` is the month-faceted wall calendar, which needs a
#'   day-resolution timeframe (`YDAY` or `MDAY`) and rolls each unit up to
#'   whole days. `type = "histogram"` leaves coverage behind and draws the
#'   spread of a *result* across the samples: bars weighted by each sample's
#'   `year_fraction`, a dashed rule at the weighted mean
#'   [sample_summary()] reports, and the free pass overlaid on the floored one
#'   when `ncap_floor > 0`. There is no histogram for `by_region`, whose units
#'   are shards of one system rather than replicates of it.
#' * `by_region` draws the geoscale as a choropleth, falling back to a tile
#'   grid when the geoscale carries no geometry or `sf` is not installed.
#' * `recursive` and `guided` draw the horizon as a Gantt chart, one row per
#'   step. For `recursive` the look-ahead milestones are filled apart from the
#'   decided ones: the overlap between steps *is* the look-ahead, and seeing it
#'   as such is the point.
#'
#' Elements no unit covered keep the grey `NA` fill, and an element two units
#' claimed is filled as `"(shared)"` rather than resolved silently. The
#' subtitle counts both, plus the within-unit multiplicity a bootstrap draw
#' carries.
#'
#' @param object a `by_sample`, `by_region`, `recursive` or `guided` sequence.
#' @param type which drawing to make; see Details.
#' @param ... for `type = "histogram"`, passed to [getData()] to narrow what is
#'   drawn (`year = `, `region = `, ...); otherwise unused.
#'
#' @return A `ggplot` object.
#' @seealso [sequence_coverage()], [solve_by_sample()], [solve_by_region()],
#'   [solve_recursive()]
#' @name autoplot.sequence
#' @examples
#' \dontrun{
#' bs <- solve_by_sample(mod, samples = 12)
#' autoplot(bs)                        # which timeslices each sample drew
#' autoplot(bs, "wall", year = 2020)   # the same, as a wall calendar
#' autoplot(bs, "histogram")           # the objective's spread
#' autoplot(bs, "histogram", var = "vTechCap", by = "tech", year = 2050)
#' autoplot(solve_by_region(mod, group = list(c("R1", "R2"), "R3")))
#' autoplot(solve_recursive(mod, overlap = 1))
#' }
NULL

#' @rdname autoplot.sequence
#' @param year the model year whose weekdays `type = "wall"` is arranged by.
#'   Without one the days are laid out in plain sequence.
#' @param var for `type = "histogram"`, what to draw the spread of:
#'   `"objective"`, or the name of a solved variable.
#' @param by for `type = "histogram"`, the columns to group a variable on, one
#'   histogram each. Defaults to every key column, since any column left out
#'   would have to be aggregated away.
#' @param bins for `type = "histogram"`, the number of bins. Defaults to the
#'   square root of the number of samples.
#' @exportS3Method ggplot2::autoplot
autoplot.by_sample <- function(object, type = c("coverage", "wall",
                                                "histogram"),
                               year = NULL, var = "objective", by = NULL,
                               bins = NULL, ...) {
  check_package("ggplot2")
  type <- match.arg(type)
  if (type == "histogram") {
    return(.cov_plot_hist(.by_sample_values(object, var, by = by, ...),
                          var, bins = bins))
  }
  cv <- sequence_coverage(object)
  if (!nrow(cv)) {
    stop("This by_sample sequence recorded no coverage to draw.",
         call. = FALSE)
  }
  cal <- .ts_calendar(object$calendar)
  if (is.null(cal)) {
    # .ts_calendar() drops ANNUAL -- a single-member column, not a timeframe
    # anything can be drawn along. Separate that from the other way it returns
    # NULL, a calendar timescales declined to build, because only one of the
    # two is the caller's mistake.
    tfs <- setdiff(names(as.data.frame(object$calendar@timetable)),
                   c("timeslice", "share", "weight"))
    stop(if (identical(tfs, "ANNUAL")) {
           paste0("This sequence's calendar is annual: it carries no ",
                  "timeframe below ANNUAL to draw coverage along.")
         } else {
           paste0("The sequence's calendar could not be read as a timescales ",
                  "calendar, so its coverage cannot be drawn on one.")
         }, call. = FALSE)
  }
  if (type == "wall") .cov_plot_wall(cv, cal, year = year)
  else .cov_plot_grid(cv, cal)
}

# Tile grid: the calendar's finest timeframe across, the next-coarsest down.
#
# The frames come in nesting order from timescales, which is the only thing
# that identifies the finest one. Counting labels does not: `m12_d365_h24`
# carries 365 days above 24 hours, so the finest frame is the smaller of the
# two.
.cov_plot_grid <- function(cv, cal) {
  lt <- as.data.frame(timescales::calendar_leaftable(cal))
  tfs <- intersect(timescales::calendar_timeframes(cal), names(lt))
  if (!length(tfs)) {
    stop("The calendar carries no timeframe to lay a grid out on.",
         call. = FALSE)
  }
  x_tf <- tfs[length(tfs)]
  y_tf <- if (length(tfs) > 1L) tfs[length(tfs) - 1L] else NULL

  byk <- .cov_by_key(cv)
  d <- tibble(
    timeslice = as.character(lt$timeslice),
    cov_x = factor(as.character(lt[[x_tf]]),
                   levels = unique(as.character(lt[[x_tf]]))),
    cov_y = if (is.null(y_tf)) factor("") else
      factor(as.character(lt[[y_tf]]),
             levels = unique(as.character(lt[[y_tf]]))),
    unit = factor(byk$unit[match(as.character(lt$timeslice), byk$key)],
                  levels = .cov_unit_levels(cv)))

  ggplot2::ggplot(d, ggplot2::aes(x = cov_x, y = cov_y, fill = unit)) +
    ggplot2::geom_raster() +
    ggplot2::scale_y_discrete(limits = rev) +
    .cov_fill_scale() +
    ggplot2::labs(x = x_tf, y = y_tf, fill = "sample",
                  title = "Calendar coverage",
                  subtitle = .cov_subtitle(byk, length(unique(cv$unit)),
                                           nrow(lt), "timeslices")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = if (nlevels(d$cov_x) > 40) ggplot2::element_blank()
                    else ggplot2::element_text(size = 6, angle = 90,
                                               vjust = 0.5),
      axis.text.y = if (nlevels(d$cov_y) > 40) ggplot2::element_blank()
                    else ggplot2::element_text(size = 6))
}

# Wall calendar. The layout -- which weekday a date falls on, which row of the
# month -- comes from timescales; the fill is applied here because the unit is
# a name, not a number.
#
# A unit is rolled up to whole days: the wall paints day cells, so a day whose
# slices were drawn by two units is shared at day resolution even where each
# slice was claimed once.
.cov_plot_wall <- function(cv, cal, year = NULL) {
  tfs <- timescales::calendar_timeframes(cal)
  day_tf <- if ("YDAY" %in% tfs) "YDAY" else if ("MDAY" %in% tfs) "MDAY" else {
    stop("A wall calendar needs a day-resolution timeframe (YDAY or MDAY); ",
         "this calendar has: ", paste(tfs, collapse = ", "),
         ". Use type = 'coverage' instead.", call. = FALSE)
  }
  lt <- as.data.frame(timescales::calendar_leaftable(cal))
  day_of <- as.character(lt[[day_tf]])[match(as.character(cv$key),
                                             as.character(lt$timeslice))]
  cv_day <- cv
  cv_day$key <- day_of
  cv_day <- cv_day[!is.na(cv_day$key), , drop = FALSE]
  if (!nrow(cv_day)) {
    stop("None of the covered timeslices resolve to a day of the calendar.",
         call. = FALSE)
  }
  byk <- .cov_by_key(cv_day)

  # without a year there are no weekdays to arrange by, and asking for them
  # anyway only earns timescales' fallback message for a choice made here
  lay <- timescales::calendar_wall_layout(
    cal, year = year,
    arrange = if (is.null(year)) "sequence" else "weekday")
  lay$unit <- factor(byk$unit[match(as.character(lay$timeslice), byk$key)],
                     levels = .cov_unit_levels(cv))
  ndays <- length(unique(as.character(lt[[day_tf]])))

  ggplot2::ggplot(lay, ggplot2::aes(x = col, y = row, fill = unit)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::scale_y_reverse(breaks = NULL) +
    ggplot2::scale_x_continuous(breaks = NULL) +
    .cov_fill_scale() +
    ggplot2::facet_wrap(~MONTH) +
    ggplot2::labs(x = NULL, y = NULL, fill = "sample",
                  title = "Calendar coverage",
                  subtitle = .cov_subtitle(byk, length(unique(cv$unit)),
                                           ndays, "days")) +
    timescales::theme_calendar()
}

#' @rdname autoplot.sequence
#' @param geoscale a `geoscales::GeoScale` to draw on, or `NULL` for the
#'   sequence model's own.
#' @param level the geoframe to draw. Defaults to the finest frame that
#'   contains every covered region.
#' @exportS3Method ggplot2::autoplot
autoplot.by_region <- function(object, type = c("coverage"), geoscale = NULL,
                               level = NULL, ...) {
  type <- match.arg(type)
  check_package("ggplot2")
  cv <- sequence_coverage(object)
  if (!nrow(cv)) {
    stop("This by_region sequence recorded no coverage to draw.",
         call. = FALSE)
  }
  byk <- .cov_by_key(cv)
  gs <- .resolve_geoscale(object$model, geoscale, required = FALSE)
  lvl <- if (is.null(gs)) NULL else level %||% .cov_geoframe(gs, byk$key)
  mappable <- !is.null(gs) && !is.null(lvl) &&
    requireNamespace("sf", quietly = TRUE) &&
    tryCatch(!is.null(geoscales::geoscale_geometry(gs, geoframe = lvl)),
             error = function(e) FALSE)
  if (!mappable) return(.cov_plot_region_grid(cv, byk))

  members <- as.character(geoscales::geoscale_regions(gs, lvl))
  if (!any(byk$key %in% members)) {
    # only reachable through an explicit `level=`, since .cov_geoframe() picks
    # a frame that contains the keys. Say so: the map would otherwise come
    # back correctly drawn and entirely grey.
    warning("No region of the sequence is a member of geoframe '", lvl,
            "', so nothing can be filled. Covered: ",
            .preview_chr(byk$key), call. = FALSE)
  }
  d <- data.frame(.member = members, stringsAsFactors = FALSE)
  names(d) <- lvl
  d$unit <- factor(byk$unit[match(members, byk$key)],
                   levels = .cov_unit_levels(cv))
  geoscales::geoscale_plot(
    gs, data = d, geoframe = lvl, fill = "unit", fill_label = "unit",
    title = "Region coverage",
    subtitle = .cov_subtitle(byk, length(unique(cv$unit)),
                             length(members), "regions")) +
    .cov_fill_scale()
}

# The finest geoframe whose members cover every region the sequence solved. A
# model's regions need not sit at the finest frame of its geoscale, and drawing
# them on the wrong one silently loses every one of them to an NA fill.
.cov_geoframe <- function(gs, keys) {
  frames <- tryCatch(geoscales::geoscale_geoframes(gs),
                     error = function(e) NULL)
  if (!length(frames)) return(NULL)
  sizes <- vapply(frames, function(f) {
    length(tryCatch(geoscales::geoscale_regions(gs, f),
                    error = function(e) character(0)))
  }, integer(1))
  for (f in frames[order(-sizes)]) {
    m <- tryCatch(as.character(geoscales::geoscale_regions(gs, f)),
                  error = function(e) character(0))
    if (all(keys %in% m)) return(f)
  }
  NULL
}

# Without geometry the useful picture is still which unit took which region,
# so draw the frame itself: regions down, one column, filled by unit.
.cov_plot_region_grid <- function(cv, byk) {
  d <- tibble(region = factor(byk$key, levels = sort(byk$key)),
              unit = factor(byk$unit, levels = .cov_unit_levels(cv)),
              cov_x = "")
  ggplot2::ggplot(d, ggplot2::aes(x = cov_x, y = region, fill = unit)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::scale_y_discrete(limits = rev) +
    .cov_fill_scale() +
    ggplot2::labs(
      x = NULL, y = NULL, fill = "unit", title = "Region coverage",
      subtitle = paste0(.cov_subtitle(byk, length(unique(cv$unit)),
                                      nrow(byk), "regions"),
                        " | no geometry: regions listed, not mapped")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank())
}

#' @rdname autoplot.sequence
#' @exportS3Method ggplot2::autoplot
autoplot.recursive <- function(object, type = c("coverage"), ...) {
  check_package("ggplot2")
  type <- match.arg(type)
  .cov_plot_gantt(sequence_coverage(object), "step")
}

#' @rdname autoplot.sequence
#' @exportS3Method ggplot2::autoplot
autoplot.guided <- function(object, type = c("coverage"), ...) {
  check_package("ggplot2")
  type <- match.arg(type)
  .cov_plot_gantt(sequence_coverage(object), "stage")
}

# Horizon Gantt: milestone years across, units down in solve order. `role`
# carries the fill, so a recursive step's look-ahead is visibly a different
# kind of coverage from the years it decided -- which is the whole difference
# between a recursive walk and a myopic one.
.cov_plot_gantt <- function(cv, unit_label) {
  if (!nrow(cv)) {
    stop("This sequence recorded no coverage to draw.", call. = FALSE)
  }
  roles <- c("decided", "covered", "lookahead")
  yrs <- sort(unique(as.integer(cv$key)))
  d <- tibble(
    unit = factor(as.character(cv$unit),
                  levels = rev(unique(as.character(cv$unit)))),
    cov_x = factor(as.character(as.integer(cv$key)),
                   levels = as.character(yrs)),
    role = factor(as.character(cv$role), levels = roles))
  decided <- unique(cv$key[cv$role %in% c("decided", "covered")])
  sub <- sprintf("%d %ss | %d of %d milestone years decided",
                 length(unique(cv$unit)), unit_label, length(decided),
                 length(yrs))
  if (any(cv$role == "lookahead")) {
    sub <- paste0(sub, sprintf(" | %d look-ahead year(s) solved and discarded",
                               sum(cv$role == "lookahead")))
  }
  ggplot2::ggplot(d, ggplot2::aes(x = cov_x, y = unit, fill = role)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.6) +
    ggplot2::scale_fill_manual(
      values = c(decided = "#2c7fb8", covered = "#2c7fb8",
                 lookahead = "#a6bddb"),
      drop = TRUE, na.value = "grey90") +
    ggplot2::labs(x = NULL, y = NULL, fill = NULL,
                  title = "Horizon coverage", subtitle = sub) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}


# ---------------------------------------------------------------------------#
# Histograms of a sampled result.
#
# A `by_sample` ensemble is a set of replicates, each annualised, so its
# results are a distribution and not a partition: the question asked of it is
# how wide the spread is, which is a histogram and not a stack.
#
# The weight is the sample's `year_fraction`, the same weight
# `sample_summary()`'s mean uses. One rule, not two -- a picture the summary
# table disagrees with would be worse than no picture.
#
# `by_region` gets no histogram: its units are shards of one system, so their
# values are addends and their spread says only that regions differ in size.
# ---------------------------------------------------------------------------#

# One value per sample (and pass) for the histogram. `objective` comes from
# `$runs`; any other name comes from the stacked results and must reduce to one
# value per sample within each group, or the aggregation would be invented
# here.
.by_sample_values <- function(object, var, by = NULL, ...) {
  runs <- as.data.frame(object$runs %||% data.frame())
  yf <- if (!is.null(runs[["year_fraction"]])) {
    stats::setNames(as.numeric(runs$year_fraction), as.character(runs$sample))
  } else {
    NULL
  }
  wt <- function(s) {
    if (is.null(yf)) return(rep(1, length(s)))
    w <- yf[as.character(s)]
    if (anyNA(w) || !any(w > 0, na.rm = TRUE)) rep(1, length(s)) else as.numeric(w)
  }

  if (identical(var, "objective")) {
    r <- runs[!is.na(runs$objective), , drop = FALSE]
    if (!nrow(r)) {
      stop("No sample of this sequence carries an objective to draw.",
           call. = FALSE)
    }
    return(tibble(sample = as.character(r$sample),
                  pass = as.character(r[["pass"]] %||% "free"),
                  grp = "objective", value = as.numeric(r$objective),
                  w = wt(r$sample)))
  }

  one <- function(scens, pass) {
    if (!length(scens)) return(NULL)
    shell <- object
    shell$scenarios <- scens
    d <- suppressMessages(getData(shell, var, ..., merge = TRUE))
    if (is.null(d) || !nrow(d)) return(NULL)
    d <- as.data.frame(d)
    keys <- by %||% setdiff(names(d), c("sample", "value", "scenario", "name"))
    keys <- intersect(keys, names(d))
    grp <- if (length(keys)) {
      do.call(paste, c(lapply(d[, keys, drop = FALSE], as.character),
                       sep = " / "))
    } else {
      var
    }
    tab <- table(paste(as.character(d$sample), grp, sep = "\r"))
    if (any(tab > 1L)) {
      # `by` left a dimension inside each bar. Summing it would answer a
      # different question -- totalling capacity over years, say -- so name
      # what is still varying instead of choosing for the caller.
      left <- setdiff(setdiff(names(d), c("sample", "value", "scenario",
                                          "name")), keys)
      stop("'", var, "' has more than one row per sample within a group, so ",
           "there is nothing to make a histogram of yet. Add ",
           .preview_chr(left), " to `by = `, or narrow the data with the ",
           "getData() arguments (year = , region = , ...).", call. = FALSE)
    }
    tibble(sample = as.character(d$sample), pass = pass, grp = grp,
           value = as.numeric(d$value), w = wt(d$sample))
  }

  out <- bind_rows(one(object$scenarios, "floor_or_free"),
                   one(object$scenarios_free, "free"))
  if (is.null(out) || !nrow(out)) {
    stop("No sample of this sequence holds results for '", var, "'.",
         call. = FALSE)
  }
  # the pass label is only meaningful when there are two of them: without a
  # floor `$scenarios` IS the free pass
  floored <- isTRUE((object$ncap_floor %||% 0) > 0)
  out$pass[out$pass == "floor_or_free"] <- if (floored) "floor" else "free"
  out
}

# The drawing. Bars are weighted by coverage and the dashed rule is the
# weighted mean -- the same number `sample_summary()` reports as `mean`, so the
# plot and the table can be read together.
.cov_plot_hist <- function(v, var, bins = NULL) {
  n <- length(unique(v$sample))
  bins <- bins %||% max(5L, min(30L, ceiling(sqrt(n))))
  two <- length(unique(v$pass)) > 1L
  ngrp <- length(unique(v$grp))
  if (ngrp > 30L) {
    stop(ngrp, " groups is too many to read as histograms. Narrow `by = ` or ",
         "the getData() arguments.", call. = FALSE)
  }
  wmean <- function(d) sum(d$value * d$w) / sum(d$w)
  mu <- do.call(rbind, lapply(split(v, list(v$grp, v$pass), drop = TRUE),
                              function(d) {
    tibble(grp = d$grp[1], pass = d$pass[1], value = wmean(d))
  }))

  p <- ggplot2::ggplot(v, ggplot2::aes(x = value, weight = w))
  p <- if (two) {
    p + ggplot2::geom_histogram(ggplot2::aes(fill = pass), bins = bins,
                                position = "identity", alpha = 0.55,
                                colour = NA) +
      ggplot2::scale_fill_manual(values = c(free = "#9ecae1",
                                            floor = "#2c7fb8")) +
      ggplot2::geom_vline(data = mu,
                          ggplot2::aes(xintercept = value, colour = pass),
                          linetype = 2, linewidth = 0.5,
                          inherit.aes = FALSE) +
      ggplot2::scale_colour_manual(values = c(free = "#9ecae1",
                                              floor = "#2c7fb8"),
                                   guide = "none")
  } else {
    p + ggplot2::geom_histogram(bins = bins, fill = "#2c7fb8", colour = NA) +
      ggplot2::geom_vline(data = mu, ggplot2::aes(xintercept = value),
                          linetype = 2, linewidth = 0.5, colour = "grey20",
                          inherit.aes = FALSE)
  }
  if (ngrp > 1L) p <- p + ggplot2::facet_wrap(~grp, scales = "free")
  sub <- sprintf("%d sample%s, weighted by coverage; dashed = weighted mean",
                 n, if (n == 1L) "" else "s")
  if (two) {
    sub <- paste0(sub, " | free pass against the floored one")
  }
  p + ggplot2::labs(x = var, y = "samples", fill = NULL,
                    title = paste0("Spread of ", var, " across samples"),
                    subtitle = sub) +
    ggplot2::theme_minimal()
}

# aes() refers to columns by bare name, the package's convention (see draw.R)
utils::globalVariables(c("cov_x", "cov_y", "role", "col", "row", "MONTH",
                         "grp", "pass", "w"))
