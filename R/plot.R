#' @include print.R class-horizon.R class-calendar.R


plot_horizon <- function(x, ...) {

  check_package("ggplot2")

  args <- list(...)
  if (!is.null(args$hjust)) {
    hjust <- args$hjust
    stopifnot(hjust >= 0 && hjust <= 1)
  } else {
    hjust <- 1
  }

  y <- x@intervals |>
    dplyr::mutate(w = end - start + 1)

  p <- ggplot2::ggplot(y) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = start - hjust, xmax = end + (1 - hjust),
        ymin = 0, ymax = 1,
        fill = mid),
      color = "black") +
    ggplot2::geom_vline(xintercept = y$mid, color = "white", alpha = 0.75) +
    ggplot2::geom_vline(xintercept = y$mid, linetype = "dashed") +
    ggplot2::scale_fill_viridis_c(option = "C", name = "") +
    ggplot2::labs(y = NULL, x = "milestone year") +
    ggplot2::scale_x_continuous(
      breaks = unique(c(y$start[1], y$mid)), expand = c(0, 0),
      # minor_breaks = unique(c(y$start[1], y$mid, y$mid))) +
      minor_breaks = seq(min(y$start), max(y$end), by = 1),
      guide = ggplot2::guide_axis(minor.ticks = TRUE)) +
    ggplot2::scale_y_continuous(expand = c(0, 0), breaks = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = .05, hjust = .5),
      # axis.minor.ticks = element_line(size = 0.5),
      panel.border = ggplot2::element_rect(color = NA, fill = NA),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, face = "italic")
    )

  if (!is_empty(x@name)) {p <- p + ggplot2::labs(title = x@name)}
  if (!is_empty(x@desc)) {p <- p + ggplot2::labs(subtitle = x@desc)}
  p
}

#' Visualize a Horizon object
#'
#' @param x An object of class `horizon`
#' @param ... Additional optional arguments:
#' `hjust` (numeric) to adjust the horizontal position of the intervals,
#' accepts values between 0 and 1.
#'
#' @return A `ggplot` object.
#' @rdname plot_horizon_method
#' @export
#' @examples
#' NULL
setMethod("plot", c("horizon", "ANY"), plot_horizon)
# setMethod("plot", "horizon", plot_horizon)

#' @description
#' `autoplot()` is the ggplot2-idiomatic entry point and returns the same
#' `ggplot` object as [plot()], so the result can be further customised with
#' `+ ...` layers.
#'
#' @param object An object of class `horizon`.
#' @rdname plot_horizon_method
#' @exportS3Method ggplot2::autoplot
autoplot.horizon <- function(object, ...) {
  plot_horizon(object, ...)
}


# Calendar structure plot ------------------------------------------------------

plot_calendar <- function(x, ...,
                          fill = c("order", "share", "weight"),
                          color_pattern = c("within", "global"),
                          palette = "D",
                          labels = TRUE,
                          label_by = c("name", "slice", "none"),
                          label_color = "auto",
                          max_labels = 60L,
                          border = NA,
                          show_leafs = NULL,
                          reference = NULL) {
  check_package("ggplot2")
  fill <- match.arg(fill)
  color_pattern <- match.arg(color_pattern)
  label_by <- match.arg(label_by)

  # A `reference` full calendar turns this into a subset view: the layout comes
  # from `reference` (the full year), but only the slices present in `x` are
  # filled -- unselected slices are left empty.
  if (!is.null(reference)) {
    if (!methods::is(reference, "calendar")) {
      stop("`reference` must be a `calendar` object.", call. = FALSE)
    }
    grid_cal   <- reference
    sub_slices <- as.character(as.data.frame(x@timetable)$slice)
  } else {
    grid_cal   <- x
    sub_slices <- NULL
  }

  tt_full <- as.data.frame(grid_cal@timetable)
  frame_cols <- setdiff(names(tt_full), c("slice", "share", "weight"))
  if (length(frame_cols) == 0 || nrow(tt_full) == 0) {
    stop("The calendar has no timeframes/timeslices to plot.")
  }
  if (is.null(tt_full$weight)) tt_full$weight <- 1 / sum(tt_full$share)
  nlev <- length(frame_cols)

  # Selection is judged over each slice's FULL extent in the reference, not just
  # the shown window: for every level, the set of prefix paths that contain at
  # least one selected leaf. So ANNUAL stays selected whenever the subset is
  # non-empty, while a YDAY is selected only if that whole day is in the subset.
  sel_full <- if (is.null(sub_slices)) rep(TRUE, nrow(tt_full)) else tt_full$slice %in% sub_slices
  if (!is.null(sub_slices) && !any(sel_full)) {
    warning("None of the subset's slices match the reference calendar; ",
            "nothing will be highlighted.", call. = FALSE)
  }
  selected_prefix <- lapply(seq_len(nlev), function(i) {
    keyf <- do.call(paste, c(tt_full[frame_cols[seq_len(i)]], sep = "\r"))
    unique(keyf[sel_full])
  })

  # Per-level unique slices (used for within-level coloring and for numeric
  # `show_leafs` positions).
  uniq_lev <- lapply(seq_len(nlev), function(i) unique(as.character(tt_full[[frame_cols[i]]])))
  klev     <- vapply(uniq_lev, length, integer(1))

  # `show_leafs` selects which slices are drawn. A named list filters each named
  # timeframe level (character = slice names at that level; numeric = 1-based
  # positions among that level's slices). An unnamed vector filters the finest
  # (leaf) level (character = leaf slice names; numeric = leaf indices). Kept
  # slices are packed left-to-right and the x-axis spans their total year-share.
  keep <- rep(TRUE, nrow(tt_full))
  if (!is.null(show_leafs)) {
    if (is.list(show_leafs)) {
      if (is.null(names(show_leafs)) || !all(names(show_leafs) %in% frame_cols)) {
        stop("`show_leafs` list names must be timeframe levels: ",
             paste(frame_cols, collapse = ", "), call. = FALSE)
      }
      for (lv in names(show_leafs)) {
        i   <- match(lv, frame_cols)
        sel <- show_leafs[[lv]]
        if (is.numeric(sel)) sel <- uniq_lev[[i]][sel[sel >= 1 & sel <= klev[i]]]
        keep <- keep & (as.character(tt_full[[lv]]) %in% as.character(sel))
      }
    } else if (is.numeric(show_leafs)) {
      idx  <- as.integer(show_leafs); idx <- idx[idx >= 1 & idx <= nrow(tt_full)]
      keep <- logical(nrow(tt_full)); keep[idx] <- TRUE
    } else {
      keep <- as.character(tt_full$slice) %in% as.character(show_leafs)
    }
    if (!any(keep)) stop("`show_leafs` selected no slices.", call. = FALSE)
  }

  tt       <- tt_full[keep, , drop = FALSE]
  true_idx <- which(keep)   # position of each kept leaf in the full calendar
  n_full   <- nrow(tt_full)
  n  <- nrow(tt)
  w  <- tt$share
  x1 <- cumsum(w)          # cumulative year-share -> x position of leaf slices
  x0 <- x1 - w

  # Aggregate leaf slices into contiguous segments for a given timeframe level.
  # Grouping on the prefix of level columns keeps repeated child slices (e.g.
  # DAY under both WINTER and SUMMER) as distinct segments.
  seg_for_level <- function(i) {
    cols <- frame_cols[seq_len(i)]
    key  <- do.call(paste, c(tt[cols], sep = "\r"))
    gid  <- cumsum(c(TRUE, key[-1] != key[-n]))
    idx  <- split(seq_len(n), gid)
    first <- vapply(idx, function(ii) ii[1], integer(1))
    nm_i  <- as.character(tt[[frame_cols[i]]])[first]   # level value (e.g. h04)
    data.frame(
      timeframe = frame_cols[i],
      level     = i,
      # individual level name (e.g. HOUR -> h00); `slice` is the full path (d001_h00)
      name      = nm_i,
      slice     = vapply(idx, function(ii) as.character(tt$slice[ii[1]]), character(1)),
      xmin      = vapply(idx, function(ii) x0[ii[1]], numeric(1)),
      xmax      = vapply(idx, function(ii) x1[ii[length(ii)]], numeric(1)),
      # true chronology index (global) and within-level position (h04 -> 5);
      # both stable under `show_leafs` filtering.
      order     = true_idx[first],
      within    = match(nm_i, uniq_lev[[i]]),
      share     = vapply(idx, function(ii) sum(w[ii]), numeric(1)),
      weight    = vapply(idx, function(ii) mean(tt$weight[ii]), numeric(1)),
      # selected over the slice's full extent (via its prefix path)
      selected  = vapply(idx, function(ii) key[ii[1]] %in% selected_prefix[[i]], logical(1)),
      stringsAsFactors = FALSE
    )
  }

  # Every cell of the (possibly truncated) grid is drawn; a subset view leaves
  # the cells not present in `x` empty.
  df <- do.call(rbind, lapply(seq_len(nlev), seg_for_level))
  rownames(df) <- NULL

  # Stack timeframes top-to-bottom: ANNUAL (level 1) on top.
  df$y    <- nlev - df$level + 1
  df$ymin <- df$y - 0.45
  df$ymax <- df$y + 0.45
  # Fill metric. For fill = "order" the `color_pattern` controls whether cells
  # are colored by global chronology (leaf order 1..n) or "within" each level
  # (per-level full gradient: h00..h23 recycled per day, d001..d365 per year).
  within_mode <- fill == "order" && color_pattern == "within"
  if (within_mode) {
    df$fill <- (df$within - 1) / pmax(1, klev[df$level] - 1)  # -> [0, 1] per level
  } else {
    df$fill <- df[[fill]]
  }
  # Subset view: leave unselected slices empty (rendered via the scale's na.value).
  if (!is.null(sub_slices)) df$fill[!df$selected] <- NA

  fill_name <- if (within_mode) "timeslice" else
    c(order = "chronology", share = "year share", weight = "weight")[[fill]]
  empty_col <- "grey90"  # color of unselected/empty slices in a subset view

  if (within_mode) {
    # relative position within each level: 0 = first slice, 1 = last
    fill_scale <- ggplot2::scale_fill_viridis_c(
      option = palette, name = fill_name, na.value = empty_col,
      limits = c(0, 1), breaks = c(0, 1), labels = c("first", "last"))
  } else if (fill == "order" && n_full > 1) {
    # global chronology: label the legend endpoints (0 .. n_leaf of full calendar)
    fill_scale <- ggplot2::scale_fill_viridis_c(
      option = palette, name = fill_name, na.value = empty_col,
      limits = c(1, n_full), breaks = c(1, n_full), labels = c("0", as.character(n_full)))
  } else {
    fill_scale <- ggplot2::scale_fill_viridis_c(
      option = palette, name = fill_name, na.value = empty_col)
  }

  p <- ggplot2::ggplot(df) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                   fill = fill),
      color = border, linewidth = 0.15) +
    fill_scale +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(
      breaks = seq_len(nlev),
      labels = rev(frame_cols),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))) +
    ggplot2::labs(x = "share of the year", y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid    = ggplot2::element_blank(),
      axis.ticks.y  = ggplot2::element_blank(),
      plot.title    = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, face = "italic")
    )

  # Labels centered on each rectangle: individual level name (`"name"`, the
  # default, e.g. HOUR -> h00) or the full slice path (`"slice"`, d001_h00).
  # Over-crowded levels (more than `max_labels` cells) are skipped.
  if (isTRUE(labels) && label_by != "none") {
    counts      <- table(df$level)
    keep_levels <- as.integer(names(counts)[counts <= max_labels])
    lab_df      <- df[df$level %in% keep_levels, , drop = FALSE]
    lab_df$txt  <- lab_df[[label_by]]
    lab_df      <- lab_df[!is.na(lab_df$txt), , drop = FALSE]
    if (nrow(lab_df) > 0) {
      lab_df$x <- (lab_df$xmin + lab_df$xmax) / 2
      if (identical(label_color, "auto")) {
        # Contrast the text with the fill: white on the darker part of the
        # gradient, dark on the lighter part (and on empty/`grey90` cells).
        lim <- if (within_mode) {
          c(0, 1)
        } else if (fill == "order") {
          c(1, n_full)
        } else {
          range(df$fill, na.rm = TRUE)
        }
        norm <- (lab_df$fill - lim[1]) / (diff(lim) + 1e-9)
        norm[!is.finite(norm)] <- 1  # empty cells are light -> dark text
        tcol <- ifelse(norm < 0.5, "white", "grey15")
      } else {
        tcol <- label_color
      }
      p <- p + ggplot2::geom_text(
        data = lab_df,
        ggplot2::aes(x = x, y = y, label = txt),
        colour = tcol, size = 2.6, check_overlap = TRUE)
    }
  }

  if (!is_empty(x@name)) p <- p + ggplot2::labs(title = x@name)
  if (!is_empty(x@desc)) p <- p + ggplot2::labs(subtitle = x@desc)
  p
}

#' Visualize a Calendar object
#'
#' @description
#' Draws the calendar's time-structure as stacked rows (one per timeframe,
#' `ANNUAL` on top), where each rectangle is a time-slice sized by its share of
#' the year. `autoplot()` is the ggplot2-idiomatic entry point and returns the
#' same `ggplot` object as [plot()].
#'
#' @param x,object An object of class `calendar`.
#' @param ... Passed to `plot_calendar()`.
#' @param fill One of `"order"` (chronology, default), `"share"` (year-share of
#'   the slice), or `"weight"` — the metric mapped to rectangle fill color.
#' @param color_pattern For `fill = "order"`, how the color gradient is applied:
#'   `"within"` (default) colors each level over its own slices — a full
#'   `h00`→`h23` gradient recycled every day, `d001`→`d365` over the year — so
#'   each row shows its cyclical structure; `"global"` colors by absolute
#'   chronology (leaf order `1…n`, e.g. `0…8760`). Ignored for `"share"`/`"weight"`.
#' @param palette Viridis color option (e.g. `"D"`, `"C"`, `"magma"`) for the
#'   fill scale.
#' @param labels Logical; draw labels inside the rectangles (master on/off).
#' @param label_by What to label each rectangle with: `"name"` (default) the
#'   individual level name (e.g. `HOUR` cells become `h00`…`h23`, `YDAY` cells
#'   `d001`…`d365`), `"slice"` the full slice path (e.g. `d001_h00`), or
#'   `"none"`.
#' @param label_color Text color for the labels. `"auto"` (default) contrasts
#'   each label with its cell — white on the darker part of the gradient, dark
#'   on the lighter part — so labels stay readable on dark fills. Pass any single
#'   color (e.g. `"black"`, `"white"`) to use it for all labels.
#' @param max_labels Integer; timeframes with more slices than this are left
#'   unlabeled to avoid clutter.
#' @param border Rectangle outline color. `NA` (default) draws no outline, so a
#'   high-resolution row (e.g. 8760 hourly slices) reads as a smooth gradient
#'   instead of a solid block of borders. Pass e.g. `"grey30"` to outline slices
#'   on coarse calendars.
#' @param show_leafs Select which slices to draw (`NULL`, default, shows all).
#'   Two forms:
#'   * an unnamed vector filtering the finest (leaf) level — leaf slice names
#'     (e.g. `"d001_h05"`) or integer leaf indices (e.g. `1:100` for the first
#'     100 leaves);
#'   * a named list filtering per timeframe level, combined with AND, e.g.
#'     `list(YDAY = "d100", HOUR = 5:10)` — for each level a character vector of
#'     that level's slice names or integer positions among its slices
#'     (`HOUR = 5:10` selects the 5th–10th hours). The kept slices are packed
#'     left-to-right and the x-axis spans their total year-share. Colors stay
#'     stable (e.g. `h05` keeps its color whether or not other hours are shown).
#' @param reference Optional full `calendar`. When supplied, `x` is treated as a
#'   *subset* of `reference`: the plot lays out `reference`'s full structure but
#'   fills only the slices present in `x` (matched by slice name), leaving the
#'   unselected slices empty. Use it to see which part of a full calendar a
#'   sampled/subset calendar covers.
#'
#' @return A `ggplot` object.
#' @rdname plot_calendar_method
#' @export
#' @examples
#' \dontrun{
#' cal <- newCalendar(make_timetable(timeslices3), name = "m12h24")
#' plot(cal)
#' autoplot(cal, fill = "share")
#'
#' # Subset view: show which slices a reduced calendar covers within the full one
#' autoplot(calendars$d365_h24_subset_1day_per_month,
#'          reference = calendars$d365_h24)
#'
#' # Zoom into specific slices: day 100, hours 5-10
#' autoplot(calendars$d365_h24, show_leafs = list(YDAY = "d100", HOUR = 5:10))
#' }
setMethod("plot", c("calendar", "ANY"), plot_calendar)

#' @rdname plot_calendar_method
#' @exportS3Method ggplot2::autoplot
autoplot.calendar <- function(object, ...,
                              fill = c("order", "share", "weight"),
                              color_pattern = c("within", "global"),
                              palette = "D",
                              labels = TRUE,
                              label_by = c("name", "slice", "none"),
                              label_color = "auto",
                              max_labels = 60L,
                              border = NA,
                              show_leafs = NULL,
                              reference = NULL) {
  # Arguments are listed explicitly (kept in sync with plot_calendar()) so that
  # editors and `args(autoplot.calendar)` expose them through the generic.
  plot_calendar(object, ...,
                fill = fill, color_pattern = color_pattern, palette = palette,
                labels = labels, label_by = label_by, label_color = label_color,
                max_labels = max_labels, border = border, show_leafs = show_leafs,
                reference = reference)
}


# Commodity emission-intensity plot --------------------------------------------

plot_commodity <- function(x, ..., palette = "D") {
  check_package("ggplot2")

  comms <- c(list(x), list(...))
  is_comm <- vapply(comms, function(o) methods::is(o, "commodity"), logical(1))
  comms <- comms[is_comm]
  if (length(comms) == 0) stop("No 'commodity' objects to plot.")

  # Assemble emission factors across all supplied commodities.
  rows <- lapply(comms, function(cm) {
    e <- cm@emis
    if (is.null(e) || nrow(e) == 0) return(NULL)
    data.frame(
      commodity = if (nzchar(cm@name)) cm@name else "(unnamed)",
      species   = as.character(e$comm),
      emis      = as.numeric(e$emis),
      unit      = if (!is.null(e$unit)) as.character(e$unit) else NA_character_,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)

  if (is.null(df) || nrow(df) == 0) {
    message("None of the supplied commodit",
            if (length(comms) > 1) "ies have" else "y has",
            " emission factors (`@emis`) to plot.")
    return(invisible(NULL))
  }

  # Preserve the order commodities were supplied in.
  df$commodity <- factor(df$commodity, levels = unique(df$commodity))

  units <- unique(stats::na.omit(df$unit))
  y_lab <- if (length(units) == 1 && nzchar(units)) {
    paste0("emission factor [", units, "]")
  } else {
    "emission factor"
  }

  p <- ggplot2::ggplot(df,
      ggplot2::aes(x = commodity, y = emis, fill = species)) +
    # Dodge, not stack: factors for different species are not additive.
    ggplot2::geom_col(position = ggplot2::position_dodge2(preserve = "single"),
                      width = 0.8) +
    ggplot2::scale_fill_viridis_d(option = palette, end = 0.85) +
    ggplot2::labs(x = NULL, y = y_lab, fill = "emission") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x   = ggplot2::element_text(angle = 30, hjust = 1),
      plot.title    = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, face = "italic")
    )

  # Emission factors may be expressed in different units; keep them comparable.
  if (length(units) > 1) {
    p <- p + ggplot2::facet_wrap(~ unit, scales = "free_y")
  }

  if (length(comms) == 1) {
    cm <- comms[[1]]
    if (nzchar(cm@name)) p <- p + ggplot2::labs(title = cm@name)
    if (length(cm@desc) && nzchar(cm@desc)) {
      p <- p + ggplot2::labs(subtitle = cm@desc)
    }
  } else {
    p <- p + ggplot2::labs(title = "Emission intensity by commodity")
  }
  p
}

#' Visualize a Commodity object
#'
#' @description
#' Plots the commodity's emission factors (`@emis`) as bars — one bar per
#' emission species, with the y-axis in the emission unit (e.g. `kt/GWh`).
#' Additional `commodity` objects can be passed via `...` to compare emission
#' intensities side by side (e.g. `autoplot(COA, OIL, GAS)`). Commodities with
#' no emission factors produce a message and return `NULL` invisibly.
#'
#' @param object A `commodity` object.
#' @param ... Optional further `commodity` objects to include in the comparison.
#' @param palette Viridis color option for the emission-species fill scale.
#'
#' @return A `ggplot` object, or `NULL` (invisibly) if there is nothing to plot.
#' @rdname autoplot.commodity
#' @exportS3Method ggplot2::autoplot
#' @examples
#' \dontrun{
#' coa <- newCommodity("COA", emis = data.frame(comm = "CO2", unit = "kt/GWh", emis = 0.33))
#' autoplot(coa)
#' }
autoplot.commodity <- function(object, ...) {
  plot_commodity(object, ...)
}


# Value-vs-year plot for supply / demand / import / export ----------------------
# `getData()` is called twice: once for the raw given data (drawn as points),
# once with `interpolate = TRUE` for the interpolated series (drawn as lines).
# Level parameters (e.g. supply `ava.lo/up/fx`, demand `dem`) are shown over the
# range of given years; a constant parameter (single or unset year) gets a flat
# dashed reference line showing the interpolation direction.

# Year-indexed slot(s) to plot per class. Processes map to their level slot;
# technology/storage plot their economics + capacity (stock and the filled
# cap/ncap/ret bounds, invcost, fixom, varom) -- efficiency coefficients are
# structural and belong to draw(), so they are omitted here.
.process_year_slot <- list(
  supply = "availability", demand = "dem", import = "imp", export = "exp",
  technology = c("capacity", "invcost", "fixom", "varom"),
  storage    = c("capacity", "invcost", "fixom", "varom")
)

# Reshape one slot data.frame to long (id..., param, value). Value columns are
# the numeric columns except `year` -- this keeps a value that happens to share a
# set-dimension's name (e.g. demand `dem`), which getData(merge = TRUE) drops.
.obj_long <- function(d) {
  d <- as.data.frame(d)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  vcols <- setdiff(names(d)[vapply(d, is.numeric, logical(1))], "year")
  if (length(vcols) == 0) return(NULL)
  idcols <- setdiff(names(d), vcols)
  dplyr::bind_rows(lapply(vcols, function(v) {
    x <- d[, c(idcols, v), drop = FALSE]
    names(x)[names(x) == v] <- "value"
    x$param <- v
    x[!is.na(x$value), , drop = FALSE]
  }))
}

# getData() (merge = FALSE) + reshape, for raw (interpolate = FALSE) or the
# interpolated series (interpolate = TRUE).
.obj_long_get <- function(obj, nm_arg, interpolate, years, ...) {
  slots <- if (is.null(nm_arg)) list(NULL) else as.list(nm_arg)  # one or many slots
  out <- dplyr::bind_rows(lapply(slots, function(s) {
    ll <- tryCatch(getData(obj, name = s, merge = FALSE,
                           interpolate = interpolate, years = years, ...),
                   error = function(e) NULL)
    if (is.null(ll) || length(ll) == 0) return(NULL)
    dplyr::bind_rows(lapply(ll, .obj_long))
  }))
  if (is.null(out) || nrow(out) == 0) NULL else as.data.frame(out)
}

plot_process_year <- function(obj, years = NULL, ...) {
  check_package("ggplot2")
  cls      <- class(obj)[1]
  obj_name <- tryCatch(obj@name, error = function(e) "")
  yslot    <- .process_year_slot[[cls]]
  nm_arg   <- if (is.null(yslot)) NULL else yslot
  base_of  <- function(p) sub("\\.(lo|up|fx)$", "", p)

  # getData() call #1: raw given data -> points.
  raw <- .obj_long_get(obj, nm_arg, FALSE, NULL, ...)
  if (is.null(raw) || nrow(raw) == 0 || !("year" %in% names(raw))) {
    message("No year-indexed data to plot for '", obj_name, "'.")
    return(invisible(NULL))
  }
  raw <- raw[!is.na(raw$value), , drop = FALSE]

  # Target years: explicit `years`, else the observed range of given years.
  years_target <- years
  if (is.null(years_target)) {
    obsy <- sort(unique(stats::na.omit(as.integer(raw$year))))
    if (length(obsy) >= 2) years_target <- seq(min(obsy), max(obsy)) else
      if (length(obsy) == 1) years_target <- obsy
  }
  # getData() call #2: interpolated series -> lines.
  itp <- .obj_long_get(obj, nm_arg, TRUE, years_target, ...)
  if (!is.null(itp) && nrow(itp) > 0) {
    itp <- dplyr::distinct(itp[!is.na(itp$value) & !is.na(itp$year), , drop = FALSE])
  }

  # Series = param (split by region/slice when present) for correct grouping.
  grp_cols <- intersect(c("param", "region", "slice"), names(raw))
  mkg <- function(d) if (nrow(d) == 0) character(0) else
    do.call(paste, c(lapply(grp_cols, function(k) as.character(d[[k]])), sep = " | "))
  raw$base <- base_of(raw$param); raw$series <- mkg(raw)
  if (!is.null(itp) && nrow(itp) > 0) { itp$base <- base_of(itp$param); itp$series <- mkg(itp) }

  # Constant series (<= 1 distinct given year) -> flat reference line.
  ny      <- tapply(raw$year, raw$series, function(z) length(unique(stats::na.omit(z))))
  const_s <- names(ny)[ny <= 1]
  cst     <- raw[raw$series %in% const_s, , drop = FALSE]

  # Anchor unset-year (NA) points at the middle of the shown range.
  allyr    <- suppressWarnings(as.numeric(c(itp$year, raw$year)))
  no_years <- !any(is.finite(allyr))     # no year information at all
  xmid     <- if (no_years) 0 else stats::median(range(allyr, na.rm = TRUE))
  raw_pt <- raw; raw_pt$year[is.na(raw_pt$year)] <- xmid

  p <- ggplot2::ggplot()
  if (!is.null(itp) && nrow(itp) > 0) {
    p <- p + ggplot2::geom_line(
      data = itp, ggplot2::aes(year, value, colour = param, group = series), na.rm = TRUE)
  }
  if (nrow(cst) > 0) {
    p <- p + ggplot2::geom_hline(
      data = cst, ggplot2::aes(yintercept = value, colour = param),
      linetype = "dashed", linewidth = 0.4)
  }
  p <- p +
    ggplot2::geom_point(
      data = raw_pt, ggplot2::aes(year, value, colour = param), size = 2, na.rm = TRUE) +
    ggplot2::facet_wrap(~ base, scales = "free_y") +
    ggplot2::labs(x = if (no_years) "year (not set)" else "year",
                  y = NULL, colour = NULL,
                  title = if (nzchar(obj_name)) obj_name else NULL,
                  subtitle = "points: given data · lines: interpolated") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
  if (no_years) {
    # No year axis to speak of: a single "NA" tick, constant value across it.
    p <- p + ggplot2::scale_x_continuous(breaks = 0, labels = "NA",
                                         limits = c(-1, 1))
  }
  p
}

#' Visualize a process object over years
#'
#' @description
#' Plots each year-indexed level parameter of the object against year, using
#' [getData()] both for the given data (points) and its interpolation (lines):
#' supply `ava.lo/up/fx` (+`cost`), demand `dem`, import `imp.lo/up/fx` (+`price`),
#' export `exp.lo/up/fx` (+`price`), and for `technology`/`storage` their
#' economics and capacity — base-year `stock`, the filled `cap`/`ncap`/`ret`
#' bounds, `invcost`, `fixom` and `varom` (efficiency coefficients are structural
#' and shown by [draw()] instead). Only populated parameters appear; each is
#' faceted by its base name so bounds and costs keep separate y-scales. A constant
#' parameter (a single or unset year) is drawn as a flat dashed line showing the
#' interpolation direction.
#'
#' @param object A `supply`, `demand`, `import`, `export`, `technology`, or
#'   `storage` object.
#' @param years Optional integer vector of target years to interpolate over.
#'   Defaults to the range of years present in the object's data.
#' @param ... Passed to [getData()] (e.g. `region=`, `slice=` filters).
#'
#' @return A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).
#' @name autoplot.process
#' @rdname autoplot.process
#' @exportS3Method ggplot2::autoplot
autoplot.supply <- function(object, years = NULL, ...) plot_process_year(object, years = years, ...)

# Demand gets its own plot: a slice-resolved demand has too many series for the
# generic per-parameter chart (one line per slice).

#' Visualize a demand object
#'
#' @description
#' Two views of a `demand` object:
#' \describe{
#'   \item{`type = "area"` (default)}{**aggregated** demand -- slice values are
#'     summed to annual totals and drawn as an area over the years (stacked by
#'     region), with the given data years marked as points.}
#'   \item{`type = "line"`}{**profiles** -- the within-year demand shape by
#'     region and year. Slices with an hour tag (`"..._h07"`) are drawn against
#'     the hour of day (faceted season x region when a season prefix is
#'     present); other calendars fall back to the slice sequence.}
#' }
#'
#' @param object A `demand` object.
#' @param type `"area"` (annual totals) or `"line"` (slice profiles).
#' @param years Optional integer vector of years. For `"area"` these are the
#'   interpolation targets (default: range of the given years); for `"line"`
#'   they filter which given years are shown.
#' @param ... Passed to [getData()] (e.g. `region =` filter).
#'
#' @return A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).
#' @export
plot_demand <- function(object, type = c("area", "line"), years = NULL, ...) {
  check_package("ggplot2")
  type <- match.arg(type)
  obj_name <- tryCatch(object@name, error = function(e) "")
  raw <- as.data.frame(object@dem)
  raw <- raw[!is.na(raw$dem), , drop = FALSE]
  if (nrow(raw) == 0) {
    message("No demand data to plot for '", obj_name, "'.")
    return(invisible(NULL))
  }
  if (!"region" %in% names(raw)) raw$region <- "(all)"

  if (type == "area") {
    # annual totals: interpolate per slice over the target years, sum slices
    years_target <- years
    if (is.null(years_target)) {
      obsy <- sort(unique(stats::na.omit(as.integer(raw$year))))
      years_target <- if (length(obsy) >= 2) seq(min(obsy), max(obsy)) else obsy
    }
    itp <- .obj_long_get(object, "dem", TRUE, years_target, ...)
    tot <- if (!is.null(itp) && nrow(itp) > 0) {
      if (!"region" %in% names(itp)) itp$region <- "(all)"
      stats::aggregate(value ~ region + year, itp, sum)
    } else {
      stats::aggregate(dem ~ region + year, raw, sum) |>
        stats::setNames(c("region", "year", "value"))
    }
    pts <- stats::aggregate(dem ~ region + year, raw, sum)
    p <- ggplot2::ggplot(tot,
        ggplot2::aes(.data$year, .data$value, fill = .data$region)) +
      ggplot2::geom_area(alpha = 0.75) +
      ggplot2::geom_point(data = pts,
        ggplot2::aes(.data$year, .data$dem), inherit.aes = FALSE,
        size = 2, na.rm = TRUE) +
      ggplot2::labs(x = "year", y = "demand (annual total)", fill = NULL,
                    title = if (nzchar(obj_name)) obj_name else NULL,
                    subtitle = "areas: interpolated totals by region · points: given data (per region)") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    return(p)
  }

  # type = "line": within-year profiles by region and year
  if (!"slice" %in% names(raw) || all(is.na(raw$slice))) {
    message("No slice-level demand in '", obj_name,
            "'; use type = \"area\" for annual data.")
    return(invisible(NULL))
  }
  d <- raw[!is.na(raw$slice), , drop = FALSE]
  if (!is.null(years) && "year" %in% names(d)) {
    d <- d[d$year %in% years, , drop = FALSE]
  }
  d$year <- factor(d$year)
  hour <- suppressWarnings(as.integer(sub(".*_h(\\d+)$", "\\1", d$slice)))
  if (any(is.finite(hour))) {
    d$hour   <- hour
    d$season <- sub("_.*$", "", d$slice)
    p <- ggplot2::ggplot(d,
        ggplot2::aes(.data$hour, .data$dem, colour = .data$year,
                     group = .data$year)) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::facet_grid(season ~ region) +
      ggplot2::labs(x = "hour", y = "demand per slice", colour = "year",
                    title = if (nzchar(obj_name)) obj_name else NULL) +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    return(p)
  }
  # no hour tag: slice sequence on x
  d$slice <- factor(d$slice, levels = unique(d$slice))
  ggplot2::ggplot(d,
      ggplot2::aes(.data$slice, .data$dem, colour = .data$year,
                   group = .data$year)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::facet_wrap(~region) +
    ggplot2::labs(x = "slice", y = "demand per slice", colour = "year",
                  title = if (nzchar(obj_name)) obj_name else NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
}

#' @rdname plot_demand
#' @exportS3Method ggplot2::autoplot
autoplot.demand <- function(object, type = c("area", "line"), years = NULL, ...) {
  plot_demand(object, type = type, years = years, ...)
}

#' @rdname autoplot.process
#' @exportS3Method ggplot2::autoplot
autoplot.import <- function(object, years = NULL, ...) plot_process_year(object, years = years, ...)

#' @rdname autoplot.process
#' @exportS3Method ggplot2::autoplot
autoplot.export <- function(object, years = NULL, ...) plot_process_year(object, years = years, ...)

#' @rdname autoplot.process
#' @exportS3Method ggplot2::autoplot
autoplot.technology <- function(object, years = NULL, ...) plot_process_year(object, years = years, ...)

#' @rdname autoplot.process
#' @exportS3Method ggplot2::autoplot
autoplot.storage <- function(object, years = NULL, ...) plot_process_year(object, years = years, ...)


# Tax / subsidy / user-constraint plots ----------------------------------------
# These are NOT processes: a tax, subsidy or user constraint imposes a value (a
# rate or a bound) that varies by year. getData() is not defined for them, so we
# read the control path straight from the object's slot: points at the given
# years, connected by the linear interpolation the model uses between them.

.lever_spec <- list(
  tax        = list(slot = "tax", val = "bal", ylab = "tax"),
  sub        = list(slot = "sub", val = "bal", ylab = "subsidy"),
  constraint = list(slot = "rhs", val = "rhs", ylab = "rhs")
)

plot_lever_year <- function(obj, years = NULL, ...) {
  check_package("ggplot2")
  cls  <- class(obj)[1]
  spec <- .lever_spec[[cls]]
  if (is.null(spec)) stop("plot_lever_year: unsupported class '", cls, "'.", call. = FALSE)
  nm <- tryCatch(obj@name, error = function(e) "")
  d  <- tryCatch(as.data.frame(methods::slot(obj, spec$slot)), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0 || !all(c("year", spec$val) %in% names(d))) {
    message("No year-indexed data to plot for '", nm, "'.")
    return(invisible(NULL))
  }
  d <- d[!is.na(d$year) & !is.na(d[[spec$val]]), , drop = FALSE]
  if (nrow(d) == 0) {
    message("No year-indexed data to plot for '", nm, "'.")
    return(invisible(NULL))
  }
  d$year <- as.integer(d$year)
  d$.val <- as.numeric(d[[spec$val]])

  has_reg <- "region" %in% names(d) && length(unique(d$region[!is.na(d$region)])) > 1
  grp <- if (has_reg) "region" else NULL

  # linear interpolation between the given control years (what the model uses)
  mkline <- function(s) {
    xs <- if (!is.null(years)) sort(unique(as.integer(years)))
          else seq(min(s$year), max(s$year))
    if (length(unique(s$year)) < 2) data.frame(year = xs, .val = s$.val[1])
    else data.frame(year = xs, .val = stats::approx(s$year, s$.val, xout = xs, rule = 2)$y)
  }
  line_df <- if (is.null(grp)) mkline(d) else
    do.call(rbind, lapply(split(d, d$region),
                          function(s) { L <- mkline(s); L$region <- s$region[1]; L }))

  if (cls == "constraint") {
    sub_txt <- c("<=" = "upper bound (≤)", ">=" = "lower bound (≥)",
                 "==" = "fixed (=)")[as.character(obj@eq)]
    if (is.na(sub_txt)) sub_txt <- NULL
  } else {
    cc <- tryCatch(obj@comm, error = function(e) character())
    sub_txt <- if (length(cc) && nzchar(cc[1])) paste0("on ", paste(cc, collapse = ", ")) else NULL
  }

  aes_xy <- if (is.null(grp))
      ggplot2::aes(x = .data[["year"]], y = .data[[".val"]])
    else
      ggplot2::aes(x = .data[["year"]], y = .data[[".val"]], colour = .data[[grp]])

  ggplot2::ggplot() +
    ggplot2::geom_line(data = line_df, aes_xy, na.rm = TRUE) +
    ggplot2::geom_point(data = d, aes_xy, size = 2, na.rm = TRUE) +
    ggplot2::labs(x = "year", y = spec$ylab, colour = grp,
                  title = if (nzchar(nm)) nm else NULL, subtitle = sub_txt) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

#' Plot a tax, subsidy or user constraint over years
#'
#' @description
#' A `tax`, `sub`(sidy) or `constraint` imposes a value that varies by year — a
#' tax/subsidy rate (`bal`) or a constraint bound (`rhs`). `autoplot()` draws that
#' control path: the **points** are the given years and the **line** is the linear
#' interpolation used between them. These are policy *levers*, not processes.
#'
#' @param object A `tax`, `sub` or `constraint` object.
#' @param years Optional integer vector of years to draw the interpolated line
#'   over (defaults to the range of the given years).
#' @param ... Unused.
#'
#' @return A `ggplot` object (or `NULL`, invisibly, when there is nothing
#'   year-indexed to plot).
#' @name autoplot.lever
#' @rdname autoplot.lever
#' @exportS3Method ggplot2::autoplot
autoplot.tax <- function(object, years = NULL, ...) plot_lever_year(object, years = years, ...)

#' @rdname autoplot.lever
#' @exportS3Method ggplot2::autoplot
autoplot.sub <- function(object, years = NULL, ...) plot_lever_year(object, years = years, ...)

#' @rdname autoplot.lever
#' @exportS3Method ggplot2::autoplot
autoplot.constraint <- function(object, years = NULL, ...) plot_lever_year(object, years = years, ...)


# Calendar heatmap --------------------------------------------------------------
# Lay timeslice-indexed values on a 2-D grid whose axes follow the calendar: the
# finest timeframe -> y, the next -> x, any coarser level(s) -> facets. The layout
# is read from a `calendar` object's timetable (works for any calendar) or, when
# only slice names of a numeric format are available (d365_h24, m12_h24, ...), by
# decomposing them with tsl2yday()/tsl2hour()/tsl2month()/tsl2year().

.days_before_month <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)

.heatmap_prep <- function(x, calendar, value, facet) {
  # --- 1. Normalise input to a data.frame with `slice` + `.val` -----------------
  if (methods::is(x, "calendar")) {
    tt  <- as.data.frame(x@timetable)
    val <- if (!is.null(value) && value %in% names(tt)) tt[[value]] else tt$share
    df  <- data.frame(slice = as.character(tt$slice), .val = as.numeric(val),
                      stringsAsFactors = FALSE)
    if (is.null(calendar)) calendar <- x
  } else if (is.numeric(x) && !is.null(names(x))) {
    df <- data.frame(slice = names(x), .val = as.numeric(x), stringsAsFactors = FALSE)
  } else if (is.data.frame(x)) {
    d <- as.data.frame(x)
    if (!("slice" %in% names(d))) stop("`x` needs a `slice` column.", call. = FALSE)
    vcol <- value
    if (is.null(vcol)) {
      num  <- setdiff(names(d)[vapply(d, is.numeric, logical(1))], "year")
      if (length(num) == 0) stop("No numeric value column found in `x`.", call. = FALSE)
      vcol <- num[1]
    }
    df <- data.frame(slice = as.character(d$slice), .val = d[[vcol]],
                     stringsAsFactors = FALSE)
    if (is.null(value)) value <- vcol
  } else {
    stop("`x` must be a data.frame, a named numeric vector, or a `calendar`.",
         call. = FALSE)
  }

  # --- 2. Level columns (coarse -> fine) ---------------------------------------
  if (methods::is(calendar, "calendar")) {
    tt  <- as.data.frame(calendar@timetable)
    lev <- setdiff(names(tt), c("slice", "share", "weight", "ANNUAL"))
    tt  <- tt[!duplicated(tt$slice), c("slice", lev), drop = FALSE]
    df  <- merge(df, tt, by = "slice", all.x = TRUE)
    levels_ord <- lev                              # timetable columns are coarse->fine
  } else {
    fmt <- if (is.character(calendar)) calendar else tsl_guess_format(df$slice)
    if (is.null(fmt)) {
      stop("Could not determine the calendar layout. Pass a `calendar` object, ",
           "or a format string such as \"d365_h24\".", call. = FALSE)
    }
    if (grepl("y", fmt)) df$year  <- tsl2year(df$slice, return.null = FALSE)
    if (grepl("m", fmt)) df$month <- tsl2month(df$slice, format = fmt)
    if (grepl("d", fmt)) df$yday  <- tsl2yday(df$slice)
    if (grepl("h", fmt)) df$hour  <- tsl2hour(df$slice)
    # Facet by month over a day-of-year format -> split yday into month + mday.
    if (identical(facet, "month") && !("month" %in% names(df)) && "yday" %in% names(df)) {
      df$month <- tsl2month(df$slice, format = fmt)
    }
    if ("yday" %in% names(df) && "month" %in% names(df) &&
        (identical(facet, "month") || !("hour" %in% names(df)))) {
      df$mday <- df$yday - .days_before_month[df$month]   # day within month
    }
    levels_ord <- intersect(c("year", "month", "mday", "yday", "hour"), names(df))
    if (identical(facet, "month")) levels_ord <- setdiff(levels_ord, "yday")
  }

  # --- 3. Resolve x / y / facet from the hierarchy (finest -> y) ----------------
  levels_ord <- levels_ord[vapply(levels_ord, function(l)
    length(unique(df[[l]][!is.na(df[[l]])])) > 1, logical(1))]   # drop constant levels
  n <- length(levels_ord)
  if (n == 0) stop("Nothing to lay out: the calendar has no varying sub-annual levels.",
                   call. = FALSE)
  y_lvl <- levels_ord[n]
  x_lvl <- if (n >= 2) levels_ord[n - 1] else levels_ord[n]
  facet_lvls <- if (!is.null(facet)) intersect(facet, names(df)) else
    if (n >= 3) levels_ord[seq_len(n - 2)] else character(0)
  x_lvl <- setdiff(x_lvl, facet_lvls); y_lvl <- setdiff(y_lvl, facet_lvls)

  # Axis columns may be strings from a calendar's timetable (e.g. "d001", "h00",
  # "WINTER"). Extract a number for a continuous axis, else keep an ordered
  # factor (chronological, by first appearance).
  .level_to_axis <- function(v) {
    if (is.numeric(v)) return(v)
    ch  <- as.character(v)
    num <- suppressWarnings(as.integer(gsub("[^0-9]", "", ch)))
    if (!any(is.na(num[!is.na(ch)])) &&
        length(unique(num)) == length(unique(ch))) num
    else factor(ch, levels = unique(ch))
  }
  for (col in c(x_lvl[1], y_lvl[1])) df[[col]] <- .level_to_axis(df[[col]])

  list(df = df, x = x_lvl[1], y = y_lvl[1], facet = facet_lvls,
       value_name = if (is.null(value)) "value" else value)
}

#' Heatmap of timeslice-indexed values over a calendar
#'
#' @description
#' Lays timeslice-indexed values on a 2-D grid whose axes follow the calendar
#' structure: the finest timeframe becomes the y-axis, the next-finest the
#' x-axis, and any coarser level(s) become facets — e.g. for a `d365_h24`
#' calendar, `x = day-of-year`, `y = hour-of-day`. Useful for load curves,
#' renewable profiles, prices, and other sub-annual series.
#'
#' @param x A `data.frame` with a `slice` column and a numeric value column, a
#'   named numeric vector (names are slices), or a `calendar` object (then the
#'   slice `share` — or `value` column — is shown).
#' @param calendar A `calendar` object giving the layout (matched to `x` by
#'   slice), or a format string (e.g. `"d365_h24"`). If `NULL`, the format is
#'   guessed from the slice names with [tsl_guess_format()].
#' @param value Name of the value column in `x` (defaults to the single numeric
#'   column, or `share` for a calendar).
#' @param facet Optional timeframe level(s) to facet by. `"month"` over a
#'   day-of-year format splits the year into monthly panels (x = day-of-month).
#' @param palette Viridis color option for the fill scale.
#' @param name Legend title (defaults to the value name).
#'
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' data("calendars", package = "energyRt")
#' cal <- calendars$d365_h24
#' prof <- data.frame(slice = cal@timetable$slice,
#'                    load  = runif(nrow(cal@timetable)))
#' plot_heatmap(prof, calendar = cal, value = "load")
#' plot_heatmap(prof, calendar = cal, value = "load", facet = "month")
#' }
plot_heatmap <- function(x, calendar = NULL, value = NULL, facet = NULL,
                         palette = "D", name = NULL) {
  check_package("ggplot2")
  pr <- .heatmap_prep(x, calendar, value, facet)
  df <- pr$df[!is.na(pr$df$.val), , drop = FALSE]
  if (is.null(name)) name <- pr$value_name

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[pr$x]], y = .data[[pr$y]],
                                        fill = .val)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(option = palette, name = name) +
    (if (is.numeric(df[[pr$x]])) ggplot2::scale_x_continuous(expand = c(0, 0))
     else ggplot2::scale_x_discrete(expand = c(0, 0))) +
    (if (is.numeric(df[[pr$y]])) ggplot2::scale_y_continuous(expand = c(0, 0))
     else ggplot2::scale_y_discrete(expand = c(0, 0))) +
    ggplot2::labs(x = pr$x, y = pr$y) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())

  if (length(pr$facet) > 0) {
    p <- p + ggplot2::facet_wrap(pr$facet, scales = "free_x")
  }
  p
}


# Weather plots -----------------------------------------------------------------
# A weather object holds a sub-annual factor `wval` (typically a capacity /
# availability factor) per region and slice. Three views are offered, all with
# the value's unit on the value axis: a calendar heatmap (default), and diurnal
# line / area charts. The slice layout (e.g. season x hour) depends on the
# model's calendar, so pass `calendar =` for the best axes; without it the layout
# is guessed from the slice names and falls back to an ordered slice axis.

#' Visualize a weather object
#'
#' @description
#' Plots the sub-annual weather factor `wval` (a capacity / availability factor)
#' of a `weather` object, in one of three styles:
#' * `"heatmap"` (default) — a calendar heatmap (finest timeframe on `y`, next on
#'   `x`), faceted by region; the value's unit is on the fill legend;
#' * `"line"` — the factor against the finest time level (e.g. hour), one line per
#'   coarser level (e.g. season), faceted by region;
#' * `"area"` — the same as `"line"` with filled areas.
#'
#' The value axis (fill for the heatmap, `y` for line/area) is labelled with the
#' object's `@unit` (or `"capacity factor"` if unset).
#'
#' @param object A `weather` object.
#' @param type One of `"heatmap"` (default), `"line"`, `"area"`.
#' @param calendar A `calendar` object (or format string) giving the slice
#'   layout. Recommended for a fully structured view. If `NULL`, the layout is
#'   guessed; when that fails, `"<prefix>_h##"`-style slices (e.g. season+hour)
#'   are split into a coarse label + hour, otherwise slices are shown on a single
#'   ordered axis. In every case region and year (when present) are drawn as
#'   facets.
#' @param palette Viridis color option for the heatmap fill.
#' @param datetime Logical (line/area only). If `TRUE`, place the profile on a
#'   real datetime axis via [tsl2dtm()]; if the slice type is not yet supported
#'   the categorical axis is kept (with a warning).
#' @param angle Rotation (degrees) for the x-axis tick labels; overlapping labels
#'   are dropped so dense sub-annual axes stay legible. Default `45`; `0` = flat.
#' @param ... Reserved for future use.
#'
#' @return A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).
#' @export
#' @examples
#' \dontrun{
#' data("calendars", package = "energyRt")
#' W <- getObject(utopia_modules$electricity$reg3$repo, name = "WSOL", drop = TRUE)
#' plot_weather(W, calendar = calendars$utopia_s4h24)                 # heatmap
#' plot_weather(W, type = "line", calendar = calendars$utopia_s4h24)
#' }
plot_weather <- function(object, type = c("heatmap", "line", "area"),
                         calendar = NULL, palette = "D",
                         datetime = FALSE, angle = 45, ...) {
  check_package("ggplot2")
  type <- match.arg(type)
  nm   <- tryCatch(object@name, error = function(e) "")
  d    <- as.data.frame(object@weather)
  if (is.null(d) || nrow(d) == 0 || !("wval" %in% names(d))) {
    message("No weather data to plot for '", nm, "'.")
    return(invisible(NULL))
  }
  d <- d[!is.na(d$wval), , drop = FALSE]

  u        <- object@unit
  unit_lab <- if (length(u) == 1 && !is.na(u) && nzchar(u)) u else "capacity factor"

  reg_multi <- length(unique(d$region)) > 1
  yr_multi  <- length(unique(d$year[!is.na(d$year)])) > 1

  # --- Layout from the slice structure (reuse the heatmap layout engine) --------
  pr <- tryCatch(
    .heatmap_prep(data.frame(slice = unique(d$slice), .v = 1),
                  calendar = calendar, value = ".v", facet = NULL),
    error = function(e) NULL)
  degenerate <- !is.null(pr) && identical(pr$x, pr$y) &&
    length(unique(d$slice)) > length(unique(pr$df[[pr$x]]))

  # `region_axis` = TRUE means region is used as the heatmap y-axis (the
  # unstructured single-axis fallback), so it must NOT also become a facet.
  region_axis <- FALSE
  cfac <- character(0)
  if (is.null(pr) || degenerate) {
    # No calendar layout resolved. Try to split "<prefix>_h##"-style slices into
    # a coarse label (e.g. season) + a fine part (hour): this recovers a 2-D grid
    # so region/year stay as facets and the heatmap keeps a real y-axis. Fall
    # back to a single ordered `slice` axis only for unstructured slice names.
    sl  <- as.character(d$slice)
    hr  <- sub("^.*?[_-]?([hH][0-9]+)$", "\\1", sl)   # trailing hour token
    pre <- sub("[_-]?([hH][0-9]+)$", "", sl)          # label before the hour
    can_split <- all(grepl("^[hH][0-9]+$", hr)) &&
      length(unique(pre)) > 1 && all(nzchar(pre))
    if (can_split) {
      d$.coarse <- factor(pre, levels = unique(pre))
      d$.fine   <- factor(hr,  levels = unique(hr))
      x_col <- ".coarse"; y_col <- ".fine"
      fine  <- ".fine";   coarse <- ".coarse"
    } else {
      if (is.null(calendar))
        message("Could not resolve a sub-annual layout from slice names; ",
                "pass `calendar =` for a structured view. Showing slices in order.")
      d$slice <- factor(sl, levels = unique(sl))
      x_col <- "slice"; fine <- "slice"; coarse <- NULL
      y_col <- "region"; region_axis <- reg_multi
    }
  } else {
    d      <- merge(d, pr$df[, unique(c("slice", pr$x, pr$y, pr$facet)), drop = FALSE],
                    by = "slice")
    x_col  <- pr$x; y_col <- pr$y
    fine   <- pr$y
    coarse <- if (!identical(pr$x, pr$y)) pr$x else NULL
    cfac   <- pr$facet
  }

  # Optional real datetime axis for line/area via tsl2dtm(); keep the categorical
  # axis (with a warning) when the slice type is not yet supported.
  if (isTRUE(datetime) && type != "heatmap") {
    dt <- tryCatch(tsl2dtm(as.character(d$slice)), error = function(e) NULL)
    if (is.null(dt) || all(is.na(dt))) {
      warning("tsl2dtm() could not convert these slices to a datetime axis; ",
              "keeping the categorical slice axis.", call. = FALSE)
    } else {
      d$.dtm <- dt
      fine <- ".dtm"; coarse <- NULL      # one continuous series per panel
    }
  }

  facets <- c(if (reg_multi && !region_axis) "region", if (yr_multi) "year", cfac)

  ttl <- if (nzchar(nm)) nm else NULL
  dsc <- tryCatch(object@desc, error = function(e) "")
  sub <- if (length(dsc) == 1 && !is.na(dsc) && nzchar(dsc)) dsc else NULL

  # friendly axis / legend titles (hide the internal .coarse/.fine/.dtm helpers)
  nice <- function(v) if (is.null(v)) NULL else
    switch(v, ".fine" = "hour", ".coarse" = "group", ".dtm" = "time", v)

  if (type == "heatmap") {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]],
                                         fill = .data[["wval"]])) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c(option = palette, name = unit_lab) +
      (if (is.numeric(d[[y_col]])) ggplot2::scale_y_continuous(expand = c(0, 0))
       else ggplot2::scale_y_discrete(expand = c(0, 0))) +
      ggplot2::labs(x = nice(x_col), y = nice(y_col), title = ttl, subtitle = sub) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid = ggplot2::element_blank())
    xc <- x_col
  } else {
    if (is.null(coarse)) {
      # group = 1 so geom_line connects points across a discrete (factor) x;
      # without it a factor x makes each point its own group and nothing draws.
      p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[fine]], y = .data[["wval"]],
                                           group = 1))
    } else {
      p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[fine]], y = .data[["wval"]],
                                           colour = .data[[coarse]],
                                           fill = .data[[coarse]],
                                           group = .data[[coarse]]))
    }
    p <- p +
      (if (type == "line") ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE)
       else ggplot2::geom_area(position = "identity", alpha = 0.35, na.rm = TRUE)) +
      ggplot2::labs(x = nice(fine), y = unit_lab,
                    colour = nice(coarse), fill = nice(coarse),
                    title = ttl, subtitle = sub) +
      ggplot2::theme_bw()
    if (type == "line") p <- p + ggplot2::guides(fill = "none")
    xc <- fine
  }

  # Rotate x labels and drop overlapping ones so dense sub-annual axes stay
  # legible (heatmap keeps zero expansion for gap-free tiles).
  xexp   <- if (type == "heatmap") c(0, 0) else ggplot2::waiver()
  xguide <- ggplot2::guide_axis(angle = angle, check.overlap = TRUE)
  p <- p + if (inherits(d[[xc]], "POSIXct"))
      ggplot2::scale_x_datetime(guide = xguide, expand = xexp)
    else if (is.numeric(d[[xc]]))
      ggplot2::scale_x_continuous(guide = xguide, expand = xexp)
    else
      ggplot2::scale_x_discrete(guide = xguide, expand = xexp)

  if (length(facets) > 0)
    p <- p + ggplot2::facet_wrap(facets, scales = "free_x")
  p
}

#' @rdname plot_weather
#' @exportS3Method ggplot2::autoplot
autoplot.weather <- function(object, type = c("heatmap", "line", "area"),
                             calendar = NULL, ...) {
  plot_weather(object, type = type, calendar = calendar, ...)
}


# Trade route map --------------------------------------------------------------
# A trade object stores inter-regional routes (src -> dst) but no geometry, so
# the caller supplies a `map` (an sf object carrying `region` + `x`/`y` centroid
# columns + polygon geometry, e.g. one of `utopia$map`). Routes are drawn as
# arrows between region centroids over the region polygons.

# Normalise `object` to a list of trade objects (single trade, list, or a
# repository/model/scenario container).
.as_trade_list <- function(x) {
  if (methods::is(x, "trade")) return(list(x))
  if (methods::is(x, "repository") || methods::is(x, "model") ||
      methods::is(x, "scenario")) return(getObject(x, class = "trade"))
  if (is.list(x)) return(Filter(function(o) methods::is(o, "trade"), x))
  stop("plot_trade_map: `object` must be a `trade`, a list of trades, or a ",
       "repository/model/scenario.", call. = FALSE)
}

#' Map inter-regional trade routes
#'
#' @description
#' Draws inter-regional trade routes (`src` → `dst`) as arrows between region
#' centroids, laid over a region map. Accepts a single `trade`, a list of them,
#' or a `repository`/`model`/`scenario` (all of whose trade objects are drawn as
#' one network). Bidirectional links (both `src`→`dst` and `dst`→`src`, e.g. the
#' `TBD_*` lines) are collapsed to a single double-headed arrow. A `trade` stores
#' no geometry, so the **map is supplied by the caller** — an `sf` object with
#' `region`, `x`, `y` (centroid) columns and polygon `geometry`, such as one of
#' the `utopia$map` layouts (`squares`, `honeycomb`, `island`, `continent`).
#'
#' @param object A `trade`, a list of `trade` objects, or a `repository`,
#'   `model` or `scenario` (whose trade objects are all drawn).
#' @param map An `sf`/data.frame with `region`, `x`, `y` and polygon `geometry`
#'   (e.g. `utopia$map$honeycomb`). Required. Region polygons need `sf`; without
#'   it, only centroids and routes are drawn.
#' @param labels Logical; label region centroids with their names (default `TRUE`).
#' @param route_color Colour of the route arrows.
#' @param ... Unused.
#'
#' @return A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).
#' @export
#' @examples
#' \dontrun{
#' TRD <- newTrade("TRD_ELC", commodity = "ELC",
#'   routes = data.frame(src = c("R1", "R2", "R3"), dst = c("R2", "R7", "R7")))
#' autoplot(TRD, map = utopia$map$honeycomb)
#' }
plot_trade_map <- function(object, map = NULL, labels = TRUE,
                           route_color = "steelblue", ...) {
  check_package("ggplot2")
  if (is.null(map)) {
    stop("plot_trade_map: pass a `map` (an sf object with `region`/`x`/`y` and ",
         "polygon geometry), e.g. `utopia$map$honeycomb`.", call. = FALSE)
  }
  ce <- as.data.frame(map)
  if (!all(c("region", "x", "y") %in% names(ce))) {
    stop("`map` must have `region`, `x` and `y` (centroid) columns.", call. = FALSE)
  }
  centers <- ce[, c("region", "x", "y")]
  centers$region <- as.character(centers$region)

  trades <- .as_trade_list(object)
  if (length(trades) == 0) {
    message("No trade objects to plot."); return(invisible(NULL))
  }
  routes <- do.call(rbind, lapply(trades, function(t) {
    r <- as.data.frame(t@routes)
    if (is.null(r) || nrow(r) == 0 || !all(c("src", "dst") %in% names(r))) return(NULL)
    data.frame(src = as.character(r$src), dst = as.character(r$dst),
               stringsAsFactors = FALSE)
  }))
  if (is.null(routes) || nrow(routes) == 0) {
    message("No routes to plot."); return(invisible(NULL))
  }

  # Collapse each unordered {src,dst} pair; a pair is bidirectional when both
  # orderings appear (as in the TBD_* links) -> a double-headed arrow.
  key <- vapply(seq_len(nrow(routes)), function(i)
    paste(sort(c(routes$src[i], routes$dst[i])), collapse = ""), character(1))
  bidir <- tapply(paste(routes$src, routes$dst), key,
                  function(v) length(unique(v)) > 1L)
  seg <- routes[!duplicated(key), , drop = FALSE]
  seg$bidir <- as.logical(bidir[key[!duplicated(key)]])

  seg <- dplyr::rename(dplyr::left_join(seg, centers, by = c("src" = "region")),
                       xsrc = "x", ysrc = "y")
  seg <- dplyr::rename(dplyr::left_join(seg, centers, by = c("dst" = "region")),
                       xdst = "x", ydst = "y")
  miss <- is.na(seg$xsrc) | is.na(seg$xdst)
  if (any(miss)) {
    warning("Dropping ", sum(miss), " route(s) whose regions are not in `map`.",
            call. = FALSE)
    seg <- seg[!miss, , drop = FALSE]
  }
  if (nrow(seg) == 0) {
    message("No routes overlap the map."); return(invisible(NULL))
  }

  have_sf <- requireNamespace("sf", quietly = TRUE) && methods::is(map, "sf")
  comm <- unique(unlist(lapply(trades, function(t)
    tryCatch(t@commodity, error = function(e) character()))))
  comm <- comm[nzchar(comm)]
  ttl  <- if (length(trades) == 1L) {
    n1 <- tryCatch(trades[[1]]@name, error = function(e) "")
    if (nzchar(n1)) n1 else "Trade routes"
  } else "Inter-regional trade"
  sub  <- paste0(if (length(comm)) paste0(paste(comm, collapse = ", "), " · "),
                 nrow(seg), " link(s)")

  arr <- function(ends) grid::arrow(type = "closed", angle = 16, ends = ends,
                                    length = grid::unit(0.1, "inches"))
  seg_aes <- ggplot2::aes(x = .data[["xsrc"]], y = .data[["ysrc"]],
                          xend = .data[["xdst"]], yend = .data[["ydst"]])
  bi <- seg[seg$bidir %in% TRUE, , drop = FALSE]
  un <- seg[!(seg$bidir %in% TRUE), , drop = FALSE]

  p <- ggplot2::ggplot()
  if (have_sf) {
    p <- p + ggplot2::geom_sf(data = map, fill = "grey92", colour = "white",
                              linewidth = 0.4)
  } else {
    message("Package 'sf' not available: drawing centroids and routes only, ",
            "without region shapes.")
  }
  if (nrow(un) > 0) {
    p <- p + ggplot2::geom_segment(data = un, seg_aes, inherit.aes = FALSE,
      colour = route_color, linewidth = 1.1, arrow = arr("last"),
      lineend = "round", linejoin = "mitre")
  }
  if (nrow(bi) > 0) {
    p <- p + ggplot2::geom_segment(data = bi, seg_aes, inherit.aes = FALSE,
      colour = route_color, linewidth = 1.1, arrow = arr("both"),
      lineend = "round", linejoin = "mitre")
  }
  p <- p + ggplot2::geom_point(data = centers,
    ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
    inherit.aes = FALSE, colour = "grey30", size = 1.6)
  if (isTRUE(labels)) {
    p <- p + ggplot2::geom_text(data = centers,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["region"]]),
      inherit.aes = FALSE, vjust = -0.8, size = 3, colour = "grey20")
  }
  p + ggplot2::labs(title = ttl, subtitle = sub) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

#' @rdname plot_trade_map
#' @exportS3Method ggplot2::autoplot
autoplot.trade <- function(object, map = NULL, ...) {
  plot_trade_map(object, map = map, ...)
}


energy_palettes <- list(
  "default" = c(
    "Coal" = "#4B4B4B",
    "Gas" = "#1F78B4",
    "Oil" = "#FF7F00",
    "Nuclear" = "#A6CEE3",
    "Renewables" = "#33A02C",
    "Hydro" = "#6A3D9A",
    "Solar" = "#FDBF6F",
    "Wind" = "#CAB2D6"
  ),
  "renewables_focus" = c(
    "Solar" = "#FDBF6F",
    "Wind" = "#CAB2D6",
    "Hydro" = "#6A3D9A",
    "Other" = "#B2DF8A"
  ),
  "high_contrast" = c(
    "Coal" = "#000000",
    "Gas" = "#E31A1C",
    "Oil" = "#FF7F00",
    "Nuclear" = "#6A3D9A",
    "Renewables" = "#33A02C"
  )
)
