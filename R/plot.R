# `setMethod("plot", c(<class>, "ANY"), ...)` at the bottom of this file needs
# every one of those classes to already be DEFINED when the file is sourced, or
# R CMD INSTALL warns "no definition for class ..." at byte-compile and the
# method is registered against an undefined class. Collate order is derived from
# these @include tags, so each class that gains a `plot` method must be listed.
#' @include print.R class-horizon.R class-calendar.R
#' @include class-commodity.R class-constraint.R class-demand.R
#' @include class-export.R class-import.R class-scenario.R
#' @include class-storage.R class-subsidy.R class-supply.R
#' @include class-tax.R class-technology.R class-trade.R
#' @include class-weather.R class-model.R class-repository.R


plot_horizon <- function(object, ...) {

  check_package("ggplot2")

  args <- list(...)
  if (!is.null(args$hjust)) {
    hjust <- args$hjust
    stopifnot(hjust >= 0 && hjust <= 1)
  } else {
    hjust <- 1
  }

  y <- object@intervals |>
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
    theme_energyRt() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = .05, hjust = .5),
      # axis.minor.ticks = element_line(size = 0.5),
      panel.border = ggplot2::element_rect(color = NA, fill = NA),
      plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, face = "italic")
    )

  if (!is_empty(object@name)) {p <- p + ggplot2::labs(title = object@name)}
  if (!is_empty(object@desc)) {p <- p + ggplot2::labs(subtitle = object@desc)}
  p
}

#' Visualize a Horizon object
#'
#' @param object An object of class `horizon`
#' @param ... Additional optional arguments:
#' `hjust` (numeric) to adjust the horizontal position of the intervals,
#' accepts values between 0 and 1.
#'
#' @return A `ggplot` object.
#'
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

plot_calendar <- function(object, ...,
                          fill = c("order", "share", "weight"),
                          color_pattern = c("within", "global"),
                          palette = "D",
                          labels = TRUE,
                          label_by = c("name", "timeslice", "none"),
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
  # from `reference` (the full year), but only the timeslices present in `object` are
  # filled -- unselected timeslices are left empty.
  if (!is.null(reference)) {
    if (!methods::is(reference, "calendar")) {
      stop("`reference` must be a `calendar` object.", call. = FALSE)
    }
    grid_cal   <- reference
    sub_timeslices <- as.character(as.data.frame(object@timetable)$timeslice)
  } else {
    grid_cal   <- object
    sub_timeslices <- NULL
  }

  tt_full <- as.data.frame(grid_cal@timetable)
  frame_cols <- setdiff(names(tt_full), c("timeslice", "share", "weight"))
  if (length(frame_cols) == 0 || nrow(tt_full) == 0) {
    stop("The calendar has no timeframes/timeslices to plot.")
  }
  if (is.null(tt_full$weight)) tt_full$weight <- 1 / sum(tt_full$share)
  nlev <- length(frame_cols)

  # Selection is judged over each timeslice's FULL extent in the reference, not just
  # the shown window: for every level, the set of prefix paths that contain at
  # least one selected leaf. So ANNUAL stays selected whenever the subset is
  # non-empty, while a YDAY is selected only if that whole day is in the subset.
  sel_full <- if (is.null(sub_timeslices)) rep(TRUE, nrow(tt_full)) else tt_full$timeslice %in% sub_timeslices
  if (!is.null(sub_timeslices) && !any(sel_full)) {
    warning("None of the subset's timeslices match the reference calendar; ",
            "nothing will be highlighted.", call. = FALSE)
  }
  selected_prefix <- lapply(seq_len(nlev), function(i) {
    keyf <- do.call(paste, c(tt_full[frame_cols[seq_len(i)]], sep = "\r"))
    unique(keyf[sel_full])
  })

  # Per-level unique timeslices (used for within-level coloring and for numeric
  # `show_leafs` positions).
  uniq_lev <- lapply(seq_len(nlev), function(i) unique(as.character(tt_full[[frame_cols[i]]])))
  klev     <- vapply(uniq_lev, length, integer(1))

  # `show_leafs` selects which timeslices are drawn. A named list filters each named
  # timeframe level (character = timeslice names at that level; numeric = 1-based
  # positions among that level's timeslices). An unnamed vector filters the finest
  # (leaf) level (character = leaf timeslice names; numeric = leaf indices). Kept
  # timeslices are packed left-to-right and the x-axis spans their total year-share.
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
      keep <- as.character(tt_full$timeslice) %in% as.character(show_leafs)
    }
    if (!any(keep)) stop("`show_leafs` selected no timeslices.", call. = FALSE)
  }

  tt       <- tt_full[keep, , drop = FALSE]
  true_idx <- which(keep)   # position of each kept leaf in the full calendar
  n_full   <- nrow(tt_full)
  n  <- nrow(tt)
  w  <- tt$share
  x1 <- cumsum(w)          # cumulative year-share -> x position of leaf timeslices
  x0 <- x1 - w

  # Aggregate leaf timeslices into contiguous segments for a given timeframe level.
  # Grouping on the prefix of level columns keeps repeated child timeslices (e.g.
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
      # individual level name (e.g. HOUR -> h00); `timeslice` is the full path (d001_h00)
      name      = nm_i,
      timeslice     = vapply(idx, function(ii) as.character(tt$timeslice[ii[1]]), character(1)),
      xmin      = vapply(idx, function(ii) x0[ii[1]], numeric(1)),
      xmax      = vapply(idx, function(ii) x1[ii[length(ii)]], numeric(1)),
      # true chronology index (global) and within-level position (h04 -> 5);
      # both stable under `show_leafs` filtering.
      order     = true_idx[first],
      within    = match(nm_i, uniq_lev[[i]]),
      share     = vapply(idx, function(ii) sum(w[ii]), numeric(1)),
      weight    = vapply(idx, function(ii) mean(tt$weight[ii]), numeric(1)),
      # selected over the timeslice's full extent (via its prefix path)
      selected  = vapply(idx, function(ii) key[ii[1]] %in% selected_prefix[[i]], logical(1)),
      stringsAsFactors = FALSE
    )
  }

  # Every cell of the (possibly truncated) grid is drawn; a subset view leaves
  # the cells not present in `object` empty.
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
  # Subset view: leave unselected timeslices empty (rendered via the scale's na.value).
  if (!is.null(sub_timeslices)) df$fill[!df$selected] <- NA

  fill_name <- if (within_mode) "timeslice" else
    c(order = "chronology", share = "year share", weight = "weight")[[fill]]
  empty_col <- "grey90"  # color of unselected/empty timeslices in a subset view

  if (within_mode) {
    # relative position within each level: 0 = first timeslice, 1 = last
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
    theme_energyRt() +
    ggplot2::theme(
      panel.grid    = ggplot2::element_blank(),
      axis.ticks.y  = ggplot2::element_blank(),
      plot.title    = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12, face = "italic")
    )

  # Labels centered on each rectangle: individual level name (`"name"`, the
  # default, e.g. HOUR -> h00) or the full timeslice path (`"timeslice"`, d001_h00).
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

  if (!is_empty(object@name)) p <- p + ggplot2::labs(title = object@name)
  if (!is_empty(object@desc)) p <- p + ggplot2::labs(subtitle = object@desc)
  p
}

#' Visualize a Calendar object
#'
#' @description
#' Draws the calendar's time-structure as stacked rows (one per timeframe,
#' `ANNUAL` on top), where each rectangle is a time-timeslice sized by its share of
#' the year. `autoplot()` is the ggplot2-idiomatic entry point and returns the
#' same `ggplot` object as [plot()].
#'
#' @param object An object of class `calendar`.
#' @param ... Passed to `plot_calendar()`.
#' @param fill One of `"order"` (chronology, default), `"share"` (year-share of
#'   the timeslice), or `"weight"` — the metric mapped to rectangle fill color.
#' @param color_pattern For `fill = "order"`, how the color gradient is applied:
#'   `"within"` (default) colors each level over its own timeslices — a full
#'   `h00`→`h23` gradient recycled every day, `d001`→`d365` over the year — so
#'   each row shows its cyclical structure; `"global"` colors by absolute
#'   chronology (leaf order `1…n`, e.g. `0…8760`). Ignored for `"share"`/`"weight"`.
#' @param palette Viridis color option (e.g. `"D"`, `"C"`, `"magma"`) for the
#'   fill scale.
#' @param labels Logical; draw labels inside the rectangles (master on/off).
#' @param label_by What to label each rectangle with: `"name"` (default) the
#'   individual level name (e.g. `HOUR` cells become `h00`…`h23`, `YDAY` cells
#'   `d001`…`d365`), `"timeslice"` the full timeslice path (e.g. `d001_h00`), or
#'   `"none"`.
#' @param label_color Text color for the labels. `"auto"` (default) contrasts
#'   each label with its cell — white on the darker part of the gradient, dark
#'   on the lighter part — so labels stay readable on dark fills. Pass any single
#'   color (e.g. `"black"`, `"white"`) to use it for all labels.
#' @param max_labels Integer; timeframes with more timeslices than this are left
#'   unlabeled to avoid clutter.
#' @param border Rectangle outline color. `NA` (default) draws no outline, so a
#'   high-resolution row (e.g. 8760 hourly timeslices) reads as a smooth gradient
#'   instead of a solid block of borders. Pass e.g. `"grey30"` to outline timeslices
#'   on coarse calendars.
#' @param show_leafs Select which timeslices to draw (`NULL`, default, shows all).
#'   Two forms:
#'   * an unnamed vector filtering the finest (leaf) level — leaf timeslice names
#'     (e.g. `"d001_h05"`) or integer leaf indices (e.g. `1:100` for the first
#'     100 leaves);
#'   * a named list filtering per timeframe level, combined with AND, e.g.
#'     `list(YDAY = "d100", HOUR = 5:10)` — for each level a character vector of
#'     that level's timeslice names or integer positions among its timeslices
#'     (`HOUR = 5:10` selects the 5th–10th hours). The kept timeslices are packed
#'     left-to-right and the x-axis spans their total year-share. Colors stay
#'     stable (e.g. `h05` keeps its color whether or not other hours are shown).
#' @param reference Optional full `calendar`. When supplied, `x` is treated as a
#'   *subset* of `reference`: the plot lays out `reference`'s full structure but
#'   fills only the timeslices present in `x` (matched by timeslice name), leaving the
#'   unselected timeslices empty. Use it to see which part of a full calendar a
#'   sampled/subset calendar covers.
#'
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' cal <- newCalendar(make_timetable(timeslices3), name = "m12h24")
#' plot(cal)
#' autoplot(cal, fill = "share")
#'
#' # Subset view: show which timeslices a reduced calendar covers within the full one
#' autoplot(calendars$d365_h24_subset_1day_per_month,
#'          reference = calendars$d365_h24)
#'
#' # Zoom into specific timeslices: day 100, hours 5-10
#' autoplot(calendars$d365_h24, show_leafs = list(YDAY = "d100", HOUR = 5:10))
#' }

#' @rdname plot_calendar_method
#' @exportS3Method ggplot2::autoplot
autoplot.calendar <- function(object, ...,
                              fill = c("order", "share", "weight"),
                              color_pattern = c("within", "global"),
                              palette = "D",
                              labels = TRUE,
                              label_by = c("name", "timeslice", "none"),
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

plot_commodity <- function(object, ..., palette = "D") {
  check_package("ggplot2")

  comms <- c(list(object), list(...))
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
    theme_energyRt() +
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
# Level parameters (e.g. supply `ava.lo/up/fx`, demand `demand`) are shown over the
# range of given years; a constant parameter (single or unset year) gets a flat
# dashed reference line showing the interpolation direction.

# Year-indexed slot(s) to plot per class. Processes map to their level slot;
# technology/storage plot their economics + capacity (stock and the filled
# cap/ncap/ret bounds, invcost, fixom, varom) -- efficiency coefficients are
# structural and belong to draw(), so they are omitted here.
.process_year_slot <- list(
  supply = "supply", demand = "demand", import = "import", export = "export",
  technology = c("capacity", "invcost", "fixom", "varom"),
  storage    = c("capacity", "invcost", "fixom", "varom")
)

# Reshape one slot data.frame to long (id..., param, value). Value columns are
# the numeric columns except `year` -- this keeps a value that happens to share a
# set-dimension's name (e.g. demand `demand`), which getData(merge = TRUE) drops.
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
# interpolated series (interpolate = TRUE). `years` is the interpolation GRID
# (getData's `years=`), never a row filter -- passing it as `year=` made
# getData filter rows against the grid (and against NULL, dropping
# everything), which is why every process autoplot said "No year-indexed
# data".
.obj_long_get <- function(object, nm_arg, interpolate, years, ...) {
  slots <- if (is.null(nm_arg)) list(NULL) else as.list(nm_arg)  # one or many slots
  out <- dplyr::bind_rows(lapply(slots, function(s) {
    ll <- tryCatch(getData(object, name = s, merge = FALSE,
                           interpolate = interpolate, years = years, ...),
                   error = function(e) NULL)
    if (is.null(ll) || length(ll) == 0) return(NULL)
    dplyr::bind_rows(lapply(ll, .obj_long))
  }))
  if (is.null(out) || nrow(out) == 0) NULL else as.data.frame(out)
}

plot_process_year <- function(object, year = NULL, interpolate = TRUE,
                              show_defaults = FALSE, units = NULL, ...) {
  check_package("ggplot2")
  cls      <- class(object)[1]
  obj_name <- tryCatch(object@name, error = function(e) "")
  yslot    <- .process_year_slot[[cls]]
  nm_arg   <- if (is.null(yslot)) NULL else yslot
  base_of  <- function(p) sub("\\.(lo|up|fx)$", "", p)

  # getData() call #1: raw given data -> points.
  raw <- .obj_long_get(object, nm_arg, FALSE, NULL, ...)
  if (is.null(raw) || nrow(raw) == 0 || !("year" %in% names(raw))) {
    message("No year-indexed data to plot for '", obj_name, "'.")
    return(invisible(NULL))
  }
  raw <- raw[!is.na(raw$value), , drop = FALSE]

  # Target years: explicit `year`, else the observed range of given years.
  year_target <- year
  if (is.null(year_target)) {
    obsy <- sort(unique(stats::na.omit(as.integer(raw$year))))
    if (length(obsy) >= 2) year_target <- seq(min(obsy), max(obsy)) else
      if (length(obsy) == 1) year_target <- obsy
  }
  # getData() call #2: interpolated series -> lines.
  itp <- NULL
  if (isTRUE(interpolate)) {
    itp <- .obj_long_get(object, nm_arg, TRUE, year_target, ...)
    if (!is.null(itp) && nrow(itp) > 0) {
      itp <- dplyr::distinct(itp[!is.na(itp$value) & !is.na(itp$year), , drop = FALSE])
    }
  }

  # Default values of mapped-but-unset parameters of the plotted slot(s)
  # (dotted lines); non-finite defaults (Inf bounds) are noted, not drawn.
  def_df   <- NULL
  def_skip <- character(0)
  if (isTRUE(show_defaults)) {
    for (s in (if (is.null(nm_arg)) character(0) else nm_arg)) {
      pm <- .slot_param_cols(cls, s)
      for (cn in names(pm)) {
        if (cn %in% unique(raw$param)) next
        dv <- suppressWarnings(as.numeric(pm[[cn]]$defVal[1]))
        if (is.na(dv)) next                       # no default declared (fx)
        if (!is.finite(dv)) {
          def_skip <- c(def_skip, paste0(cn, " = ", dv))
          next
        }
        def_df <- rbind(def_df, data.frame(
          param = paste0(cn, " (default)"), base = base_of(cn), value = dv,
          stringsAsFactors = FALSE))
      }
    }
    if (!is.null(def_df)) def_df <- unique(def_df)
  }

  # Series = param (split by region/timeslice/cluster when present) for
  # correct grouping -- without `cluster` a supply curve's steps overplot.
  grp_cols <- intersect(c("param", "region", "timeslice", "cluster"),
                        names(raw))
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

  sub_parts <- c("points: given data",
                 if (isTRUE(interpolate)) "lines: interpolated",
                 if (!is.null(def_df)) "dotted: defaults")
  cap_txt <- if (length(def_skip) > 0)
    paste("defaults not shown:", paste(unique(def_skip), collapse = ", ")) else
    NULL

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
  if (!is.null(def_df)) {
    p <- p + ggplot2::geom_hline(
      data = def_df, ggplot2::aes(yintercept = value, colour = param),
      linetype = "dotted", linewidth = 0.4)
  }
  p <- p +
    ggplot2::geom_point(
      data = raw_pt, ggplot2::aes(year, value, colour = param), size = 2, na.rm = TRUE) +
    ggplot2::facet_wrap(~ base, scales = "free_y",
                        labeller = .unit_labeller(object, raw, units)) +
    ggplot2::labs(x = if (no_years) "year (not set)" else "year",
                  y = NULL, colour = NULL,
                  title = if (nzchar(obj_name)) obj_name else NULL,
                  subtitle = paste(sub_parts, collapse = " · "),
                  caption = cap_txt) +
    theme_energyRt() +
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
#' supply `ava.lo/up/fx` (+`cost`), demand `demand`, import `imp.lo/up/fx` (+`price`),
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
#' @param year Optional integer vector of target years to interpolate over.
#'   Defaults to the range of years present in the object's data; a wider
#'   vector (e.g. `2020:2050`) extends the lines beyond the given years
#'   (constant extrapolation, as the model interpolates).
#' @param interpolate Logical, default `TRUE`: draw the interpolated series
#'   (lines) alongside the given data (points), using each parameter's own
#'   interpolation rule via [getData()]. `FALSE` shows the given data only.
#' @param show_defaults Logical, default `FALSE`. When `TRUE`, parameters of
#'   the plotted slot(s) that are mapped to the model but NOT set in the
#'   object are drawn as dotted lines at their default values (e.g. a supply
#'   without `ava.lo` shows its default of 0). Non-finite defaults (e.g.
#'   `ava.up = Inf`) are listed in the caption instead of drawn.
#' @param ... Passed to [getData()] (e.g. `region=`, `timeslice=` filters).
#'
#' @return A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).
#' @name autoplot.process
#' @rdname autoplot.process
#' @param style For `supply` only: `"profile"` (default) draws the
#'   per-parameter year profile shared by all process classes; `"bar"` draws
#'   quantity or price by region over the years (stacked availability bars,
#'   per-region cost lines; `type =` picks the quantity); `"regions"` draws
#'   the across-region comparison — availability and cost bars per region,
#'   with an unlimited (`Inf`) availability shown as a translucent
#'   full-height bar labelled "uncapped".
#' @param type For `style = "bar"`: `"availability"` (default) or `"cost"`.
#' @exportS3Method ggplot2::autoplot
autoplot.supply <- function(object, year = NULL, interpolate = TRUE,
                            show_defaults = FALSE, units = NULL,
                            style = c("profile", "bar", "regions"),
                            type = c("availability", "cost"), ...) {
  style <- match.arg(style)
  switch(style,
    profile = plot_process_year(object, year = year,
                                interpolate = interpolate,
                                show_defaults = show_defaults,
                                units = units, ...),
    bar = plot_supply_bar(object, type = match.arg(type), year = year,
                          interpolate = interpolate, ...),
    regions = plot_supply_regions(object, year = year,
                                  interpolate = interpolate, ...))
}

# Long supply data for the bar/regions charts: the given (milestone-year)
# data, or an explicit `year` grid interpolated. A stepped supply (several
# clusters) always keeps its raw values -- interpolation collapses the
# duplicate-year step rows into one series.
.supply_long <- function(object, year = NULL, interpolate = TRUE, ...) {
  raw <- .obj_long_get(object, "supply", FALSE, NULL, ...)
  if (is.null(raw) || nrow(raw) == 0) return(NULL)
  multi_step <- "cluster" %in% names(raw) &&
    length(unique(stats::na.omit(raw$cluster))) > 1
  d <- raw
  if (!is.null(year) && isTRUE(interpolate) && !multi_step) {
    di <- .obj_long_get(object, "supply", TRUE, year, ...)
    if (!is.null(di) && nrow(di) > 0) d <- di
  }
  d <- d[is.finite(d$value), , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  if (!"region" %in% names(d)) d$region <- NA_character_
  d$region[is.na(d$region)] <- "(all)"
  d
}

# Cluster factor in @cluster$order (fill/stack order of curve steps).
.supply_cluster_levels <- function(object, d) {
  if (!"cluster" %in% names(d)) return(d)
  cl <- tryCatch(object@cluster, error = function(e) NULL)
  if (is.data.frame(cl) && nrow(cl) > 0 && "order" %in% names(cl) &&
      !all(is.na(cl$order))) {
    d$cluster <- factor(d$cluster, levels = cl$cluster[order(cl$order)])
  }
  d
}

# Effective annual availability per (region, cluster, year): timeslice rows
# sum to annual; ava.fx wins over ava.up on the same key. A key with neither
# is unbounded and carries no row here.
.supply_ava_annual <- function(d) {
  a <- d[d$param %in% c("ava.up", "ava.fx") & is.finite(d$value), ,
         drop = FALSE]
  if (nrow(a) == 0) return(NULL)
  kc <- intersect(c("region", "cluster", "year"), names(a))
  a <- a |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(kc, "param")))) |>
    dplyr::summarise(value = sum(.data$value), .groups = "drop") |>
    as.data.frame()
  fx <- a[a$param == "ava.fx", , drop = FALSE]
  up <- a[a$param == "ava.up", , drop = FALSE]
  if (nrow(fx) > 0 && nrow(up) > 0) {
    up <- up[!(interaction(up[kc], drop = TRUE) %in%
                 interaction(fx[kc], drop = TRUE)), , drop = FALSE]
  }
  rbind(fx, up)
}

# Quantity and price by region over the years. Not exported: autoplot(sup,
# style = "bar") reaches it.
plot_supply_bar <- function(object, type = c("availability", "cost"),
                            year = NULL, interpolate = TRUE, ...) {
  check_package("ggplot2")
  type <- match.arg(type)
  obj_name <- tryCatch(object@name, error = function(e) "")
  d <- .supply_long(object, year = year, interpolate = interpolate, ...)
  if (is.null(d)) {
    message("No supply data to plot for '", obj_name, "'.")
    return(invisible(NULL))
  }
  d <- .supply_cluster_levels(object, d)
  has_cluster <- "cluster" %in% names(d) &&
    length(unique(stats::na.omit(d$cluster))) > 1
  unit <- tryCatch(object@unit, error = function(e) "")
  unit <- if (length(unit) == 1 && !is.na(unit) && nzchar(unit)) unit else NULL

  if (type == "availability") {
    a <- .supply_ava_annual(d)
    if (is.null(a) || nrow(a) == 0) {
      message("No finite availability (ava.up/ava.fx) for '", obj_name,
              "': supply is uncapped.")
      return(invisible(NULL))
    }
    a$year_f <- factor(ifelse(is.na(a$year), "all", as.character(a$year)))
    fill_col <- if (has_cluster && "cluster" %in% names(a)) "cluster" else
      "region"
    p <- ggplot2::ggplot(a, ggplot2::aes(x = .data$year_f, y = .data$value,
                                         fill = .data[[fill_col]])) +
      ggplot2::geom_col(position = "stack")
    if (fill_col == "cluster" && length(unique(a$region)) > 1)
      p <- p + ggplot2::facet_wrap(~region)
    p + ggplot2::labs(
      x = "year",
      y = if (is.null(unit)) "availability" else
        paste0("availability [", unit, "/year]"),
      fill = NULL,
      title = if (nzchar(obj_name)) obj_name else NULL,
      subtitle = "annual supply availability (ava.fx, else ava.up)") +
      theme_energyRt() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                     axis.text.x = ggplot2::element_text(angle = 45,
                                                         hjust = 1)) +
      .legend_compact(length(unique(a[[fill_col]])))
  } else {
    co <- d[d$param == "cost" & !is.na(d$value), , drop = FALSE]
    if (nrow(co) == 0) {
      message("No supply cost to plot for '", obj_name, "'.")
      return(invisible(NULL))
    }
    kc <- intersect(c("region", "cluster", "year"), names(co))
    co <- co |>
      dplyr::group_by(dplyr::across(dplyr::all_of(kc))) |>
      dplyr::summarise(value = mean(.data$value), .groups = "drop") |>
      as.data.frame()
    co$year_x <- ifelse(is.na(co$year), 0, co$year)
    aes_ln <- if (has_cluster && "cluster" %in% names(co))
      ggplot2::aes(x = .data$year_x, y = .data$value,
                   colour = .data$region, linetype = .data$cluster,
                   group = interaction(.data$region, .data$cluster)) else
      ggplot2::aes(x = .data$year_x, y = .data$value,
                   colour = .data$region, group = .data$region)
    ggplot2::ggplot(co, aes_ln) +
      ggplot2::geom_step(direction = "hv", linewidth = 0.6, na.rm = TRUE) +
      ggplot2::geom_point(size = 1.6, na.rm = TRUE) +
      ggplot2::labs(
        x = "year",
        y = if (is.null(unit)) "cost" else paste0("cost [per ", unit, "]"),
        colour = NULL, linetype = NULL,
        title = if (nzchar(obj_name)) obj_name else NULL,
        subtitle = "supply cost by region (mean over timeslices)") +
      theme_energyRt() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
      .legend_compact(length(unique(co$region)))
  }
}

# Across-region availability and cost bars for one display year. Not
# exported: autoplot(sup, style = "regions") reaches it.
plot_supply_regions <- function(object, year = NULL, interpolate = TRUE,
                                ...) {
  check_package("ggplot2")
  obj_name <- tryCatch(object@name, error = function(e) "")
  d <- .supply_long(object, year = NULL, interpolate = interpolate, ...)
  if (is.null(d)) {
    message("No supply data to plot for '", obj_name, "'.")
    return(invisible(NULL))
  }
  # display year: the requested one, else the latest with data
  yrs <- sort(unique(stats::na.omit(as.integer(d$year))))
  yr <- if (!is.null(year)) as.integer(year)[1] else
    if (length(yrs) > 0) max(yrs) else NA_integer_
  dd <- if (is.na(yr)) d else {
    keep <- is.na(d$year) | d$year == yr
    d[keep, , drop = FALSE]
  }

  # declared + data regions; a region with no finite bound is uncapped
  regions <- unique(c(
    tryCatch(stats::na.omit(object@region), error = function(e) NULL),
    setdiff(unique(dd$region), "(all)")))
  if (length(regions) == 0) regions <- "(all)"

  a <- .supply_ava_annual(dd)
  ava <- stats::setNames(rep(NA_real_, length(regions)), regions)
  if (!is.null(a) && nrow(a) > 0) {
    per_reg <- tapply(a$value, a$region, sum, na.rm = TRUE)
    ava[names(per_reg)[names(per_reg) %in% regions]] <-
      per_reg[names(per_reg) %in% regions]
    if ("(all)" %in% names(per_reg) && !"(all)" %in% regions)
      ava[] <- ifelse(is.na(ava), per_reg[["(all)"]], ava)
  }
  co <- dd[dd$param == "cost" & !is.na(dd$value), , drop = FALSE]
  cost <- stats::setNames(rep(NA_real_, length(regions)), regions)
  if (nrow(co) > 0) {
    per_reg <- tapply(co$value, co$region, mean, na.rm = TRUE)
    cost[names(per_reg)[names(per_reg) %in% regions]] <-
      per_reg[names(per_reg) %in% regions]
    if ("(all)" %in% names(per_reg) && !"(all)" %in% regions)
      cost[] <- ifelse(is.na(cost), per_reg[["(all)"]], cost)
  }
  if (all(is.na(ava)) && all(is.na(cost))) {
    message("No availability or cost data to plot for '", obj_name, "'.")
    return(invisible(NULL))
  }

  # an uncapped region draws a full-height translucent bar in the
  # availability panel, labelled instead of valued
  uncapped <- is.na(ava)
  ymax <- if (any(!uncapped)) max(ava[!uncapped]) else 1
  pdf_ <- rbind(
    data.frame(region = regions, measure = "availability",
               value = ifelse(uncapped, ymax, ava),
               uncapped = uncapped, stringsAsFactors = FALSE),
    data.frame(region = regions, measure = "cost",
               value = unname(cost), uncapped = FALSE,
               stringsAsFactors = FALSE))
  pdf_ <- pdf_[!is.na(pdf_$value), , drop = FALSE]

  unit <- tryCatch(object@unit, error = function(e) "")
  unit <- if (length(unit) == 1 && !is.na(unit) && nzchar(unit)) unit else NULL
  lab_map <- c(
    availability = if (is.null(unit)) "availability" else
      paste0("availability [", unit, "/year]"),
    cost = if (is.null(unit)) "cost" else paste0("cost [per ", unit, "]"))
  pdf_$measure <- factor(lab_map[pdf_$measure], levels = unname(lab_map))

  lab_df <- pdf_[pdf_$uncapped, , drop = FALSE]
  p <- ggplot2::ggplot(pdf_, ggplot2::aes(x = .data$region, y = .data$value)) +
    ggplot2::geom_col(ggplot2::aes(alpha = .data$uncapped),
                      fill = "#4E79A7", show.legend = FALSE) +
    ggplot2::scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.35)) +
    ggplot2::facet_wrap(~measure, scales = "free_y") +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = if (nzchar(obj_name)) obj_name else NULL,
      subtitle = if (is.na(yr)) "supply by region" else
        paste0("supply by region, ", yr),
      caption = if (any(pdf_$uncapped))
        "translucent full-height bars: unlimited availability" else NULL) +
    theme_energyRt() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1))
  if (nrow(lab_df) > 0) {
    p <- p + ggplot2::geom_text(data = lab_df,
                                ggplot2::aes(label = "uncapped"),
                                vjust = -0.4, size = 3, colour = "grey30")
  }
  p
}

# Demand gets its own plot: a timeslice-resolved demand has too many series for the
# generic per-parameter chart (one line per timeslice).

#' Visualize a demand object
#'
#' @description
#' Two views of a `demand` object:
#' \describe{
#'   \item{`style = "area"` (default)}{**aggregated** demand -- timeslice values are
#'     summed to annual totals and drawn as an area over the years (stacked by
#'     region), with the given data years marked as points.}
#'   \item{`style = "line"`}{**profiles** -- the within-year demand shape,
#'     drawn by the same engine as [plot_weather()]: the finest time level on
#'     `x`, one line per coarser level, faceted by region and year.}
#'   \item{`style = "heatmap"`}{a calendar heatmap of the demand shape --
#'     the same layout as the [plot_weather()] heatmap (finest timeframe on
#'     `y`, next on `x`), faceted by region and year.}
#' }
#'
#' @param object A `demand` object.
#' @param style `"area"` (annual totals), `"line"` (timeslice profiles) or
#'   `"heatmap"` (calendar heatmap by region).
#' @param calendar Optional `calendar` object ordering the heatmap's
#'   timeslice axis.
#' @param year Optional integer vector of years. For `"area"` these are the
#'   interpolation targets (default: range of the given years); for `"line"`
#'   and `"heatmap"` they filter which given years are shown.
#' @param interpolate Logical, default `TRUE`: for `"area"`, interpolate the
#'   annual totals over the target years; `FALSE` aggregates only the given
#'   data years.
#' @param palette viridis palette option, as in [ggplot2::scale_fill_viridis_d()].
#' @param ... Passed to [getData()] (e.g. `region =` filter).
#'
#' @return A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).
#' @keywords internal
plot_demand <- function(object, style = c("area", "line", "heatmap"),
                        year = NULL, interpolate = TRUE, palette = "D",
                        calendar = NULL, ...) {
  check_package("ggplot2")
  style <- match.arg(style)
  obj_name <- tryCatch(object@name, error = function(e) "")
  raw <- as.data.frame(object@demand)
  raw <- raw[!is.na(raw$demand), , drop = FALSE]
  if (nrow(raw) == 0) {
    message("No demand data to plot for '", obj_name, "'.")
    return(invisible(NULL))
  }
  if (!"region" %in% names(raw)) raw$region <- "(all)"
  raw$region[is.na(raw$region)] <- "(all)"   # aggregate() drops NA groups

  # "line" and "heatmap" share the profile engine with plot_weather(), so a
  # demand shape and a weather factor read identically.
  if (style %in% c("line", "heatmap")) {
    d <- raw[!is.na(raw$timeslice), , drop = FALSE]
    if (!is.null(year) && "year" %in% names(d)) {
      d <- d[d$year %in% year, , drop = FALSE]
    }
    if (nrow(d) == 0) {
      message("No timeslice-resolved demand to draw for '", obj_name, "'.")
      return(invisible(NULL))
    }
    d$wval <- d$demand
    u <- tryCatch(object@unit, error = function(e) NA_character_)
    comm <- paste(tryCatch(object@commodity, error = function(e) ""),
                  collapse = ", ")
    return(.plot_profile(
      d, style = style, calendar = calendar, palette = palette,
      unit_lab = if (length(u) == 1 && !is.na(u) && nzchar(u)) u else "demand",
      title = if (nzchar(obj_name)) obj_name else NULL,
      subtitle = if (nzchar(comm)) paste0("commodity: ", comm) else NULL))
  }

  if (style == "area") {
    # annual totals: interpolate per timeslice over the target years, sum timeslices
    year_target <- year
    if (is.null(year_target)) {
      obsy <- sort(unique(stats::na.omit(as.integer(raw$year))))
      year_target <- if (length(obsy) >= 2) seq(min(obsy), max(obsy)) else obsy
    }
    itp <- if (isTRUE(interpolate))
      .obj_long_get(object, "demand", TRUE, year_target, ...) else NULL
    tot <- if (!is.null(itp) && nrow(itp) > 0) {
      if (!"region" %in% names(itp)) itp$region <- "(all)"
      itp$region[is.na(itp$region)] <- "(all)"
      stats::aggregate(value ~ region + year, itp, sum)
    } else {
      stats::aggregate(demand ~ region + year, raw, sum) |>
        stats::setNames(c("region", "year", "value"))
    }
    pts <- stats::aggregate(demand ~ region + year, raw, sum)
    p <- ggplot2::ggplot(tot,
        ggplot2::aes(.data$year, .data$value, fill = .data$region)) +
      ggplot2::geom_area(alpha = 0.75) +
      ggplot2::geom_point(data = pts,
        ggplot2::aes(.data$year, .data$demand), inherit.aes = FALSE,
        size = 2, na.rm = TRUE) +
      ggplot2::scale_fill_viridis_d(option = palette, end = 0.9) +
      ggplot2::labs(x = "year", y = "demand (annual total)", fill = NULL,
                    title = if (nzchar(obj_name)) obj_name else NULL,
                    subtitle = "areas: interpolated totals by region · points: given data (per region)") +
      theme_energyRt() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    return(p)
  }

  invisible(NULL)
}

#' @rdname plot_demand
#' @exportS3Method ggplot2::autoplot
autoplot.demand <- function(object, style = c("area", "line", "heatmap"),
                            year = NULL, interpolate = TRUE, palette = "D",
                            calendar = NULL, ...) {
  plot_demand(object, style = style, year = year, interpolate = interpolate,
              palette = palette, calendar = calendar, ...)
}

#' @rdname autoplot.process
#' @exportS3Method ggplot2::autoplot
autoplot.import <- function(object, year = NULL, interpolate = TRUE,
                            show_defaults = FALSE, units = NULL, ...) {
  plot_process_year(object, year = year, interpolate = interpolate,
                    show_defaults = show_defaults, units = units, ...)
}

#' @rdname autoplot.process
#' @exportS3Method ggplot2::autoplot
autoplot.export <- function(object, year = NULL, interpolate = TRUE,
                            show_defaults = FALSE, units = NULL, ...) {
  plot_process_year(object, year = year, interpolate = interpolate,
                    show_defaults = show_defaults, units = units, ...)
}

#' @rdname autoplot.process
#' @exportS3Method ggplot2::autoplot
autoplot.technology <- function(object, year = NULL, interpolate = TRUE,
                                show_defaults = FALSE, units = NULL, ...) {
  plot_process_year(object, year = year, interpolate = interpolate,
                    show_defaults = show_defaults, units = units, ...)
}

#' @rdname autoplot.process
#' @exportS3Method ggplot2::autoplot
autoplot.storage <- function(object, year = NULL, interpolate = TRUE,
                             show_defaults = FALSE, units = NULL, ...) {
  plot_process_year(object, year = year, interpolate = interpolate,
                    show_defaults = show_defaults, units = units, ...)
}


# Tax / subsidy / user-constraint plots ----------------------------------------
# These are NOT processes: a tax, subsidy or user constraint imposes a value (a
# rate or a bound) that varies by year. getData() is not defined for them, so we
# read the control path straight from the object's slot: points at the given
# years, connected by the linear interpolation the model uses between them.

.lever_spec <- list(
  tax        = list(slot = "tax", val = "bal", ylab = "tax"),
  subsidy    = list(slot = "subsidy", val = "bal", ylab = "subsidy"),
  constraint = list(slot = "rhs", val = "rhs", ylab = "rhs")
)

plot_lever_year <- function(object, year = NULL, interpolate = TRUE, ...) {
  check_package("ggplot2")
  cls  <- class(object)[1]
  spec <- .lever_spec[[cls]]
  if (is.null(spec)) stop("plot_lever_year: unsupported class '", cls, "'.", call. = FALSE)
  nm <- tryCatch(object@name, error = function(e) "")
  d  <- tryCatch(as.data.frame(methods::slot(object, spec$slot)), error = function(e) NULL)
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
    xs <- if (!is.null(year)) sort(unique(as.integer(year)))
          else seq(min(s$year), max(s$year))
    if (length(unique(s$year)) < 2) data.frame(year = xs, .val = s$.val[1])
    else data.frame(year = xs, .val = stats::approx(s$year, s$.val, xout = xs, rule = 2)$y)
  }
  line_df <- if (!isTRUE(interpolate)) NULL else if (is.null(grp)) mkline(d) else
    do.call(rbind, lapply(split(d, d$region),
                          function(s) { L <- mkline(s); L$region <- s$region[1]; L }))

  if (cls == "constraint") {
    sub_txt <- c("<=" = "upper bound (≤)", ">=" = "lower bound (≥)",
                 "==" = "fixed (=)")[as.character(object@eq)]
    if (is.na(sub_txt)) sub_txt <- NULL
  } else {
    cc <- tryCatch(object@comm, error = function(e) character())
    sub_txt <- if (length(cc) && nzchar(cc[1])) paste0("on ", paste(cc, collapse = ", ")) else NULL
  }

  aes_xy <- if (is.null(grp))
      ggplot2::aes(x = .data[["year"]], y = .data[[".val"]])
    else
      ggplot2::aes(x = .data[["year"]], y = .data[[".val"]], colour = .data[[grp]])

  p <- ggplot2::ggplot()
  if (!is.null(line_df)) p <- p + ggplot2::geom_line(data = line_df, aes_xy, na.rm = TRUE)
  p +
    ggplot2::geom_point(data = d, aes_xy, size = 2, na.rm = TRUE) +
    ggplot2::labs(x = "year", y = spec$ylab, colour = grp,
                  title = if (nzchar(nm)) nm else NULL, subtitle = sub_txt) +
    theme_energyRt() +
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
#' @param year Optional integer vector of years to draw the interpolated line
#'   over (defaults to the range of the given years).
#' @param interpolate Logical, default `TRUE`: draw the linearly interpolated
#'   control path between the given years. `FALSE` shows the points only.
#' @param ... Unused.
#'
#' @return A `ggplot` object (or `NULL`, invisibly, when there is nothing
#'   year-indexed to plot).
#' @name autoplot.lever
#' @rdname autoplot.lever
#' @exportS3Method ggplot2::autoplot
autoplot.tax <- function(object, year = NULL, interpolate = TRUE, ...) {
  plot_lever_year(object, year = year, interpolate = interpolate, ...)
}

#' @rdname autoplot.lever
#' @exportS3Method ggplot2::autoplot
autoplot.subsidy <- function(object, year = NULL, interpolate = TRUE, ...) {
  plot_lever_year(object, year = year, interpolate = interpolate, ...)
}

#' @rdname autoplot.lever
#' @exportS3Method ggplot2::autoplot
autoplot.constraint <- function(object, year = NULL, interpolate = TRUE, ...) {
  plot_lever_year(object, year = year, interpolate = interpolate, ...)
}


# Calendar heatmap --------------------------------------------------------------
# Lay timeslice-indexed values on a 2-D grid whose axes follow the calendar: the
# finest timeframe -> y, the next -> x, any coarser level(s) -> facets. The layout
# is read from a `calendar` object's timetable (works for any calendar) or, when
# only timeslice names of a numeric format are available (d365_h24, m12_h24, ...), by
# decomposing them with tsl2yday()/tsl2hour()/tsl2month()/tsl2year().

.days_before_month <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)

.heatmap_prep <- function(x, calendar, value, facet) {
  # --- 1. Normalise input to a data.frame with `timeslice` + `.val` -----------------
  if (methods::is(x, "calendar")) {
    tt  <- as.data.frame(x@timetable)
    val <- if (!is.null(value) && value %in% names(tt)) tt[[value]] else tt$share
    df  <- data.frame(timeslice = as.character(tt$timeslice), .val = as.numeric(val),
                      stringsAsFactors = FALSE)
    if (is.null(calendar)) calendar <- x
  } else if (is.numeric(x) && !is.null(names(x))) {
    df <- data.frame(timeslice = names(x), .val = as.numeric(x), stringsAsFactors = FALSE)
  } else if (is.data.frame(x)) {
    d <- as.data.frame(x)
    if (!("timeslice" %in% names(d))) stop("`x` needs a `timeslice` column.", call. = FALSE)
    vcol <- value
    if (is.null(vcol)) {
      num  <- setdiff(names(d)[vapply(d, is.numeric, logical(1))], "year")
      if (length(num) == 0) stop("No numeric value column found in `x`.", call. = FALSE)
      vcol <- num[1]
    }
    df <- data.frame(timeslice = as.character(d$timeslice), .val = d[[vcol]],
                     stringsAsFactors = FALSE)
    if (is.null(value)) value <- vcol
  } else {
    stop("`x` must be a data.frame, a named numeric vector, or a `calendar`.",
         call. = FALSE)
  }

  # --- 2. Level columns (coarse -> fine) ---------------------------------------
  if (methods::is(calendar, "calendar")) {
    tt  <- as.data.frame(calendar@timetable)
    lev <- setdiff(names(tt), c("timeslice", "share", "weight", "ANNUAL"))
    tt  <- tt[!duplicated(tt$timeslice), c("timeslice", lev), drop = FALSE]
    df  <- merge(df, tt, by = "timeslice", all.x = TRUE)
    levels_ord <- lev                              # timetable columns are coarse->fine
  } else {
    fmt <- if (is.character(calendar)) calendar else tsl_guess_format(df$timeslice)
    if (is.null(fmt)) {
      stop("Could not determine the calendar layout. Pass a `calendar` object, ",
           "or a format string such as \"d365_h24\".", call. = FALSE)
    }
    if (grepl("y", fmt)) df$year  <- tsl2year(df$timeslice, return.null = FALSE)
    if (grepl("m", fmt)) df$month <- tsl2month(df$timeslice, format = fmt)
    if (grepl("d", fmt)) df$yday  <- tsl2yday(df$timeslice)
    if (grepl("h", fmt)) df$hour  <- tsl2hour(df$timeslice)
    # Facet by month over a day-of-year format -> split yday into month + mday.
    if (identical(facet, "month") && !("month" %in% names(df)) && "yday" %in% names(df)) {
      df$month <- tsl2month(df$timeslice, format = fmt)
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
#' @param object A `data.frame` with a `timeslice` column and a numeric value column, a
#'   named numeric vector (names are timeslices), or a `calendar` object (then the
#'   timeslice `share` — or `value` column — is shown).
#' @param calendar A `calendar` object giving the layout (matched to `x` by
#'   timeslice), or a format string (e.g. `"d365_h24"`). If `NULL`, the format is
#'   guessed from the timeslice names with [tsl_guess_format()].
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
#' prof <- data.frame(timeslice = cal@timetable$timeslice,
#'                    load  = runif(nrow(cal@timetable)))
#' plot_heatmap(prof, calendar = cal, value = "load")
#' plot_heatmap(prof, calendar = cal, value = "load", facet = "month")
#' }
plot_heatmap <- function(object, calendar = NULL, value = NULL, facet = NULL,
                         palette = "D", name = NULL) {
  check_package("ggplot2")
  pr <- .heatmap_prep(object, calendar, value, facet)
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
# availability factor) per region and timeslice. Three views are offered, all with
# the value's unit on the value axis: a calendar heatmap (default), and diurnal
# line / area charts. The timeslice layout (e.g. season x hour) depends on the
# model's calendar, so pass `calendar =` for the best axes; without it the layout
# is guessed from the timeslice names and falls back to an ordered timeslice axis.

#' Visualize a weather object
#'
#' @description
#' Plots the sub-annual weather factor `wval` (a capacity / availability factor)
#' of a `weather` object, in one of three styles:
#' * `"heatmap"` (default) — a calendar heatmap (finest timeframe on `y`, next on
#'   `x`), faceted by region; the value's unit is on the fill legend;
#' * `"line"` — the factor against the finest time level (e.g. hour), one line per
#'   coarser level (e.g. season), faceted by region; the coarse level is coloured
#'   with a viridis `"H"` gradient (continuous when numeric, e.g. day of year);
#' * `"area"` — the same as `"line"` with filled areas.
#'
#' The value axis (fill for the heatmap, `y` for line/area) is labelled with the
#' object's `@unit` (or `"capacity factor"` if unset).
#'
#' @param object A `weather` object.
#' @param style One of `"heatmap"` (default), `"line"`, `"area"`.
#' @param calendar A `calendar` object (or format string) giving the timeslice
#'   layout. Recommended for a fully structured view. If `NULL`, the layout is
#'   guessed; when that fails, `"<prefix>_h##"`-style timeslices (e.g. season+hour)
#'   are split into a coarse label + hour, otherwise timeslices are shown on a single
#'   ordered axis. In every case region and year (when present) are drawn as
#'   facets.
#' @param palette Viridis color option for the heatmap fill.
#' @param datetime Logical (line/area only). If `TRUE`, place the profile on a
#'   real datetime axis via [tsl2dtm()]; if the timeslice type is not yet supported
#'   the categorical axis is kept (with a warning).
#' @param region Which regions to draw. `NULL` (default) draws them all. A
#'   character vector names them; a single number takes that many, in the order
#'   they appear. Useful on converted continental models, where a weather factor
#'   can carry dozens of regions and faceting on all of them leaves the panels
#'   unreadable.
#' @param angle Rotation (degrees) for the x-axis tick labels; overlapping labels
#'   are dropped so dense sub-annual axes stay legible. Default `45`; `0` = flat.
#' @param ... Reserved for future use.
#'
#' @return A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).
#' @keywords internal
#' @examples
#' \dontrun{
#' data("calendars", package = "energyRt")
#' W <- getObject(utopia$modules$electricity$R3$repo, name = "WSOL", drop = TRUE)
#' autoplot(W, calendar = calendars$s4_h24)                     # heatmap
#' autoplot(W, style = "line", calendar = calendars$s4_h24)
#' }
plot_weather <- function(object, style = c("heatmap", "line", "area"),
                         calendar = NULL, palette = "D",
                         datetime = FALSE, angle = 45, region = NULL, ...) {
  check_package("ggplot2")
  style <- match.arg(style)
  nm   <- tryCatch(object@name, error = function(e) "")
  d    <- as.data.frame(object@weather)
  if (is.null(d) || nrow(d) == 0 || !("wval" %in% names(d))) {
    message("No weather data to plot for '", nm, "'.")
    return(invisible(NULL))
  }
  d <- d[!is.na(d$wval), , drop = FALSE]

  # A weather factor from a continental model carries every region, and
  # faceting on all of them leaves panels too small to read. `region` selects
  # a subset; `region = 1` (or any number) takes
  # that many, in the order they appear.
  if (!is.null(region) && "region" %in% names(d)) {
    have <- unique(d$region)
    keep <- if (is.numeric(region)) utils::head(have, max(1L, as.integer(region[1])))
            else as.character(region)
    miss <- setdiff(keep, have)
    if (length(miss)) {
      warning("plot_weather: region(s) not in '", nm, "': ",
              paste(miss, collapse = ", "), call. = FALSE)
    }
    keep <- intersect(keep, have)
    if (!length(keep)) {
      message("No weather data left after the `region` filter for '", nm, "'.")
      return(invisible(NULL))
    }
    d <- d[d$region %in% keep, , drop = FALSE]
  }

  u        <- object@unit
  unit_lab <- if (length(u) == 1 && !is.na(u) && nzchar(u)) u else "capacity factor"

  ttl <- if (nzchar(nm)) nm else NULL
  dsc <- tryCatch(object@desc, error = function(e) "")
  sub <- if (length(dsc) == 1 && !is.na(dsc) && nzchar(dsc)) dsc else NULL
  return(.plot_profile(d, style = style, calendar = calendar,
                       palette = palette, datetime = datetime, angle = angle,
                       unit_lab = unit_lab, title = ttl, subtitle = sub))
}

# Shared sub-annual profile engine behind plot_weather() and plot_demand()
# (styles "heatmap" / "line" / "area"): resolves the timeslice layout from the
# calendar (or timeslice names), facets by region / year / coarser levels, and
# draws the value in `wval`. The heatmap fill uses the `palette` option; the
# line/area colour maps the coarse level with viridis "H", continuous when the
# level is numeric (e.g. day of year), so long calendars read as a gradient.
.plot_profile <- function(d, style, calendar = NULL, palette = "D",
                          datetime = FALSE, angle = 45, unit_lab = "value",
                          title = NULL, subtitle = NULL) {
  reg_multi <- "region" %in% names(d) && length(unique(d$region)) > 1
  yr_multi  <- "year" %in% names(d) &&
    length(unique(d$year[!is.na(d$year)])) > 1

  # --- Layout from the timeslice structure (reuse the heatmap layout engine) --------
  pr <- tryCatch(
    .heatmap_prep(data.frame(timeslice = unique(d$timeslice), .v = 1),
                  calendar = calendar, value = ".v", facet = NULL),
    error = function(e) NULL)
  degenerate <- !is.null(pr) && identical(pr$x, pr$y) &&
    length(unique(d$timeslice)) > length(unique(pr$df[[pr$x]]))

  # `region_axis` = TRUE means region is used as the heatmap y-axis (the
  # unstructured single-axis fallback), so it must NOT also become a facet.
  region_axis <- FALSE
  cfac <- character(0)
  if (is.null(pr) || degenerate) {
    # No calendar layout resolved. Try to split "<prefix>_h##"-style timeslices into
    # a coarse label (e.g. season) + a fine part (hour): this recovers a 2-D grid
    # so region/year stay as facets and the heatmap keeps a real y-axis. Fall
    # back to a single ordered `timeslice` axis only for unstructured timeslice names.
    sl  <- as.character(d$timeslice)
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
        message("Could not resolve a sub-annual layout from timeslice names; ",
                "pass `calendar =` for a structured view. Showing timeslices in order.")
      d$timeslice <- factor(sl, levels = unique(sl))
      x_col <- "timeslice"; fine <- "timeslice"; coarse <- NULL
      y_col <- "region"; region_axis <- reg_multi
    }
  } else {
    d      <- merge(d, pr$df[, unique(c("timeslice", pr$x, pr$y, pr$facet)), drop = FALSE],
                    by = "timeslice")
    x_col  <- pr$x; y_col <- pr$y
    fine   <- pr$y
    coarse <- if (!identical(pr$x, pr$y)) pr$x else NULL
    cfac   <- pr$facet
  }

  # Optional real datetime axis for line/area via tsl2dtm(); keep the categorical
  # axis (with a warning) when the timeslice type is not yet supported.
  if (isTRUE(datetime) && style != "heatmap") {
    dt <- tryCatch(tsl2dtm(as.character(d$timeslice)), error = function(e) NULL)
    if (is.null(dt) || all(is.na(dt))) {
      warning("tsl2dtm() could not convert these timeslices to a datetime axis; ",
              "keeping the categorical timeslice axis.", call. = FALSE)
    } else {
      d$.dtm <- dt
      fine <- ".dtm"; coarse <- NULL      # one continuous series per panel
    }
  }

  facets <- c(if (reg_multi && !region_axis) "region", if (yr_multi) "year", cfac)

  ttl <- title
  sub <- subtitle

  # friendly axis / legend titles (hide the internal .coarse/.fine/.dtm helpers)
  nice <- function(v) if (is.null(v)) NULL else
    switch(v, ".fine" = "hour", ".coarse" = "group", ".dtm" = "time", v)

  if (style == "heatmap") {
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
      (if (style == "line") ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE)
       else ggplot2::geom_area(position = "identity", alpha = 0.35, na.rm = TRUE)) +
      ggplot2::labs(x = nice(fine), y = unit_lab,
                    colour = nice(coarse), fill = nice(coarse),
                    title = ttl, subtitle = sub) +
      theme_energyRt()
    if (style == "line") p <- p + ggplot2::guides(fill = "none")
    if (!is.null(coarse)) {
      num <- is.numeric(d[[coarse]])
      p <- p + (if (num) ggplot2::scale_colour_viridis_c(option = "H")
                else ggplot2::scale_colour_viridis_d(option = "H", end = 0.9))
      if (style == "area") {
        p <- p + (if (num) ggplot2::scale_fill_viridis_c(option = "H")
                  else ggplot2::scale_fill_viridis_d(option = "H", end = 0.9))
      }
    }
    xc <- fine
  }

  # Rotate x labels and drop overlapping ones so dense sub-annual axes stay
  # legible (heatmap keeps zero expansion for gap-free tiles).
  xexp   <- if (style == "heatmap") c(0, 0) else ggplot2::waiver()
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
autoplot.weather <- function(object, style = c("heatmap", "line", "area"),
                             calendar = NULL, region = NULL, ...) {
  plot_weather(object, style = style, calendar = calendar, region = region, ...)
}

# Chronological timeslice levels of a calendar (timetable row order), or NULL.
.calendar_slice_levels <- function(calendar) {
  tt <- tryCatch(calendar@timetable, error = function(e) NULL)
  if (is.data.frame(tt) && "timeslice" %in% names(tt))
    unique(as.character(tt$timeslice)) else NULL
}

# Row-per-series calendar heatmap: x = timeslices (calendar order when
# given), y = `y_col`, fill = `wval`; optional facet column.
.rows_heatmap <- function(d, y_col, unit_lab, title = NULL, subtitle = NULL,
                          calendar = NULL, palette = "D", facet = NULL) {
  sl <- as.character(d$timeslice)
  lev <- .calendar_slice_levels(calendar)
  lev <- if (!is.null(lev) && all(sl %in% lev)) intersect(lev, unique(sl))
    else unique(sl)
  d$timeslice <- factor(sl, levels = lev)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$timeslice,
                                       y = .data[[y_col]],
                                       fill = .data$wval)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_viridis_c(option = palette, name = unit_lab) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::scale_x_discrete(
      expand = c(0, 0),
      guide = ggplot2::guide_axis(angle = 90, check.overlap = TRUE)) +
    ggplot2::labs(x = NULL, y = NULL, title = title, subtitle = subtitle) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold"),
                   axis.text.x = ggplot2::element_text(size = 6))
  if (!is.null(facet) && facet %in% names(d) &&
      length(unique(d[[facet]])) > 1)
    p <- p + ggplot2::facet_wrap(facet)
  p
}

# Family key of a weather factor: the name with a trailing cluster suffix
# ("_c3", "_cl12", "_7") stripped.
.weather_type_key <- function(nm) {
  sub("[_-](c|cl)?[0-9]+$", "", nm, ignore.case = TRUE)
}

# One heatmap for a family of weather factors: rows = member factors,
# columns = timeslices, faceted by region; values are meaned over years.
# More than `max_rows` members keep the highest- and lowest-mean halves.
.weather_group_heatmap <- function(members, title = NULL, calendar = NULL,
                                   max_rows = 12L) {
  dd <- dplyr::bind_rows(lapply(members, function(w) {
    d <- tryCatch(as.data.frame(w@weather), error = function(e) NULL)
    if (!is.data.frame(d) || !all(c("wval", "timeslice") %in% names(d)))
      return(NULL)
    d <- d[is.finite(suppressWarnings(as.numeric(d$wval))) &
             !is.na(d$timeslice), , drop = FALSE]
    if (nrow(d) == 0) return(NULL)
    if (!"region" %in% names(d)) d$region <- NA_character_
    d$region[is.na(d$region)] <- "(all)"
    data.frame(fct = w@name, region = d$region,
               timeslice = as.character(d$timeslice),
               wval = as.numeric(d$wval), stringsAsFactors = FALSE)
  }))
  if (is.null(dd) || nrow(dd) == 0) return(NULL)
  dd <- dd |>
    dplyr::group_by(.data$fct, .data$region, .data$timeslice) |>
    dplyr::summarise(wval = mean(.data$wval), .groups = "drop") |>
    as.data.frame()
  m <- sort(tapply(dd$wval, dd$fct, mean), decreasing = TRUE)
  sub <- NULL
  if (length(m) > max_rows) {
    k <- max_rows %/% 2L
    keep <- c(utils::head(names(m), k), utils::tail(names(m), max_rows - k))
    dd <- dd[dd$fct %in% keep, , drop = FALSE]
    sub <- paste0(length(m), " factors; showing the ", k,
                  " highest- and ", max_rows - k, " lowest-mean")
  }
  # highest mean on top
  dd$fct <- factor(dd$fct, levels = rev(names(m)[names(m) %in% dd$fct]))
  u <- tryCatch(members[[1]]@unit, error = function(e) NA_character_)
  unit_lab <- if (length(u) == 1 && !is.na(u) && nzchar(u)) u else
    "capacity factor"
  .rows_heatmap(dd, "fct", unit_lab, title = title, subtitle = sub,
                calendar = calendar, facet = "region")
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
#' no geometry, so the geometry comes either from the model's geoscale (see
#' [setGeoscale()]) or from a `map` supplied by the caller — an `sf` object with
#' `region`, `x`, `y` (centroid) columns and polygon `geometry`, such as one of
#' the `utopia$map` layouts (`squares`, `honeycomb`, `island`, `continent`).
#'
#' @param object A `trade`, a list of `trade` objects, or a `repository`,
#'   `model` or `scenario` (whose trade objects are all drawn).
#' @param map An `sf`/data.frame with `region`, `x`, `y` and polygon `geometry`
#'   (e.g. `utopia$map$honeycomb`), or a `geoscales::Geoscale`. When `NULL`, the
#'   geoscale attached to `object` is used. Region polygons need `sf`; without
#'   it, only centroids and routes are drawn.
#' @param labels Logical; label region centroids with their names (default `TRUE`).
#' @param route_color Colour of the route arrows.
#' @param level When `map` is a `Geoscale`, which level to draw. Defaults to the
#'   finest, i.e. the model's own regions.
#' @param ... Unused.
#'
#' @return A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).
#'
#' @details
#' When the map carries a coordinate reference system, routes, centroids and
#' labels are drawn as `sf` layers rather than raw `x`/`y` ones. `geom_sf()`
#' installs a `coord_sf()` that reprojects the polygons but would leave a
#' `geom_segment()` untransformed, detaching every route from its regions. Maps
#' with no CRS — including the reference `utopia$map` layouts — take the plain
#' cartesian path, which is correct for them.
#'
#' @export
#' @examples
#' \dontrun{
#' TRD <- newTrade("TRD_ELC", commodity = "ELC",
#'   routes = data.frame(src = c("R1", "R2", "R3"), dst = c("R2", "R7", "R7")))
#' autoplot(TRD, map = utopia$map$honeycomb)
#'
#' # or from a geoscale, at any level
#' plot_trade_map(TRD, map = utopia_geoscale())
#' plot_trade_map(TRD, map = utopia_geoscale(), level = "zone")
#' }
plot_trade_map <- function(object, map = NULL, labels = TRUE,
                           route_color = "steelblue", level = NULL, ...) {
  check_package("ggplot2")
  # A geoscale attached to the scenario/model is used when no map is given, so
  # `autoplot(scen)` and `plot_trade_map(repo)` work without one being passed.
  if (is.null(map)) map <- tryCatch(getGeoscale(object), error = function(e) NULL)
  if (is.null(map)) {
    stop("plot_trade_map: pass a `map` (an sf object with `region`/`x`/`y` and ",
         "polygon geometry, or a `geoscales::Geoscale`), e.g. ",
         "`utopia$map$honeycomb`.", call. = FALSE)
  }
  # Keep the geoscale: when a coarser level is drawn, the route endpoints have
  # to be lifted to it too, or nothing will match the map.
  route_gs <- if (is_geoscale(map)) map else NULL
  map <- .trade_map_normalise(map, level = level)
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
  routes <- .routes_to_level(routes, route_gs, level)
  if (nrow(routes) == 0) {
    message("No routes cross a boundary at this level."); return(invisible(NULL))
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

  # With a real CRS, `geom_sf()` installs a `coord_sf()` that reprojects the
  # polygons but NOT raw x/y layers, so routes drawn with `geom_segment()`
  # would detach from their regions. Draw everything as sf in that case, and
  # keep the plain cartesian layers when there is no CRS (the reference UTOPIA
  # layouts have none) or no sf.
  crs <- if (have_sf) sf::st_crs(map) else NA
  as_sf_layers <- have_sf && !is.na(crs)

  p <- ggplot2::ggplot()
  if (have_sf) {
    p <- p + ggplot2::geom_sf(data = map, fill = "grey92", colour = "white",
                              linewidth = 0.4)
  } else {
    message("Package 'sf' not available: drawing centroids and routes only, ",
            "without region shapes.")
  }
  if (as_sf_layers) {
    if (nrow(un) > 0) {
      p <- p + ggplot2::geom_sf(data = .routes_as_sf(un, crs),
        colour = route_color, linewidth = 1.1, arrow = arr("last"))
    }
    if (nrow(bi) > 0) {
      p <- p + ggplot2::geom_sf(data = .routes_as_sf(bi, crs),
        colour = route_color, linewidth = 1.1, arrow = arr("both"))
    }
    pts <- sf::st_as_sf(centers, coords = c("x", "y"), crs = crs,
                        remove = FALSE)
    p <- p + ggplot2::geom_sf(data = pts, colour = "grey30", size = 1.6)
    if (isTRUE(labels)) {
      p <- p + ggplot2::geom_sf_text(data = pts,
        ggplot2::aes(label = .data[["region"]]),
        vjust = -0.8, size = 3, colour = "grey20")
    }
  } else {
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
        ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                     label = .data[["region"]]),
        inherit.aes = FALSE, vjust = -0.8, size = 3, colour = "grey20")
    }
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

# Normalise the `map` argument of plot_trade_map() to an sf/data.frame carrying
# `region`, `x`, `y`. Accepts a `geoscales::Geoscale` (dissolved to `level` and
# given centroids), or anything already in that shape.
#' @noRd
.trade_map_normalise <- function(map, level = NULL) {
  if (!is_geoscale(map)) return(map)
  check_package("geoscales")
  check_package("sf")
  level <- level %||% .geo_default_level(map)
  # energyRt's own vocabulary still says `level`; geoscales' is `geoframe`
  shp <- geoscales::geoscale_geometry(map, geoframe = level)
  names(shp)[names(shp) == level] <- "region"
  # `st_point_on_surface()` rather than `st_centroid()`: a centroid can fall
  # outside a concave or multipart region, which would hang its routes and
  # label in the sea.
  xy <- sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(
    sf::st_geometry(shp))))
  shp$x <- xy[, 1]
  shp$y <- xy[, 2]
  shp
}

# Lift route endpoints to a coarser level of the geoscale. Routes that end up
# inside a single aggregate region are internal to it and are dropped -- the
# topological counterpart of the flow netting in `.geo_net_trade()`.
#' @noRd
.routes_to_level <- function(routes, gs, level) {
  if (is.null(gs) || is.null(level)) return(routes)
  finest <- .geo_default_level(gs)
  if (identical(level, finest)) return(routes)

  map <- .geo_level_map(gs, from = finest, to = level)
  routes$src <- unname(map[routes$src])
  routes$dst <- unname(map[routes$dst])
  routes <- routes[!is.na(routes$src) & !is.na(routes$dst), , drop = FALSE]
  routes[routes$src != routes$dst, , drop = FALSE]
}

# Route endpoints -> an sf LINESTRING layer in the map's CRS, so `coord_sf()`
# transforms routes and polygons together.
#' @noRd
.routes_as_sf <- function(seg, crs) {
  geom <- lapply(seq_len(nrow(seg)), function(i) {
    sf::st_linestring(rbind(c(seg$xsrc[i], seg$ysrc[i]),
                            c(seg$xdst[i], seg$ydst[i])))
  })
  keep <- setdiff(names(seg), c("xsrc", "ysrc", "xdst", "ydst"))
  sf::st_sf(seg[, keep, drop = FALSE],
            geometry = sf::st_sfc(geom, crs = crs))
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

# -- plot() -------------------------------------------------------------------
# `plot()` delegates to `autoplot()` for every class that has one, so whichever
# verb a user reaches for works and `autoplot()` stays the single implementation.
#
# The value is RETURNED, not printed: an S4 generic auto-prints a visible result
# at top level, so printing here as well would draw the figure twice. This also
# preserves `p <- plot(x)`.
#
# The schematic-diagram role that `plot()` conventionally plays for base
# graphics is filled by `draw()`, which needs no ggplot2 -- so the classical
# plot/autoplot split survives here under the name `draw`.
#
# `autoplot` is ggplot2's generic and is NOT imported -- energyRt only
# registers S3 methods on it -- so it must be called qualified. ggplot2 is in
# Suggests, hence the guard: without it this fails with R's bare
# "could not find function" instead of energyRt's message.

#' @rdname draw
#' @name plot
#' @exportMethod plot
NULL

setMethod("plot", c("calendar", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("commodity", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("constraint", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("demand", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("export", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("horizon", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("import", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("model", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("repository", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("scenario", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("storage", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("subsidy", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("supply", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("tax", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("technology", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("trade", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})
setMethod("plot", c("weather", "ANY"), function(x, y, ...) {
  check_package("ggplot2"); ggplot2::autoplot(x, ...)
})

# -- shared theme -------------------------------------------------------------

#' energyRt's default ggplot theme
#'
#' A thin wrapper around [theme_energyRt()] giving every energyRt figure one
#' base size and look. Package plots call it instead of `theme_bw()` directly,
#' so restyling them is a one-line change here rather than an edit to every
#' plotting function.
#'
#' @param base_size base font size in points.
#' @param ... passed on to [theme_energyRt()].
#'
#' @return A ggplot2 theme object.
#' @family draw
#' @export
#' @examples
#' \dontrun{
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() + theme_energyRt()
#' }
theme_energyRt <- function(base_size = 11, ...) {
  check_package("ggplot2")
  # NB `ggplot2::theme_bw()`, not `theme_energyRt()` -- this is the one place in
  # the package that must call ggplot2 directly, or it recurses into itself.
  ggplot2::theme_bw(base_size = base_size, ...)
}

# Legend management for many-key fill scales: a list of ggplot components to
# `+` onto a plot; empty for small legends. Deliberately NOT part of
# theme_energyRt() -- the theme is data-blind and serves 2-5-key charts where
# a bottom multi-column legend would waste height; legend policy needs the
# key count, which only the plot builder knows.
#' @noRd
.legend_compact <- function(n_keys, base_size = 11) {
  if (!is.finite(n_keys) || n_keys <= 8) return(list())
  list(
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(
        size = if (n_keys > 15) base_size - 4 else base_size - 3),
      legend.key.size = ggplot2::unit(if (n_keys > 15) 3.5 else 4.5, "mm"),
      legend.margin = ggplot2::margin(0, 0, 0, 0)),
    ggplot2::guides(fill = ggplot2::guide_legend(
      ncol = if (n_keys > 15) 4L else 3L, byrow = TRUE)))
}

# Reorder a discrete fill column so the lump bucket stacks and lists last.
#' @noRd
.mix_other_last <- function(x, other = c("Other", "Other processes")) {
  u <- unique(as.character(x))
  tail_ <- intersect(other, u)
  factor(as.character(x), levels = c(setdiff(sort(u), tail_), tail_))
}
