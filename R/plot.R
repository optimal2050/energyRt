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


plot_horizon <- function(object, ..., palette = "D") {

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

  # Milestone ticks carry the display label; the leading tick is the first
  # interval's start, a boundary rather than a milestone, so it stays numeric.
  .lab <- y[["label"]]
  if (is.null(.lab)) .lab <- as.character(y$mid)
  .brk <- unique(c(y$start[1], y$mid))
  .brk_lab <- as.character(.brk)
  .mi <- match(.brk, y$mid)
  .hit <- !is.na(.mi)
  if (any(.hit)) .brk_lab[.hit] <- as.character(.lab)[.mi[.hit]]

  p <- ggplot2::ggplot(y) +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = start - hjust, xmax = end + (1 - hjust),
        ymin = 0, ymax = 1,
        fill = mid),
      color = "black") +
    ggplot2::geom_vline(xintercept = y$mid, color = "white", alpha = 0.75) +
    ggplot2::geom_vline(xintercept = y$mid, linetype = "dashed") +
    ggplot2::scale_fill_viridis_c(option = palette, end = 0.9,
                                  name = "year") +
    ggplot2::labs(y = NULL, x = "milestone year") +
    ggplot2::scale_x_continuous(
      breaks = .brk, labels = .brk_lab, expand = c(0, 0),
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
# The figure is timescales' icicle: the S4 calendar is bridged to a
# timescales Calendar and drawn by gg_calendar(); only the model-side
# `reference` resolution and the title stay here.

#' Visualize a Calendar object
#'
#' @description
#' Draws the calendar's time structure as an icicle: one row per timeframe,
#' `ANNUAL` on top, each rectangle a timeslice sized by its share of the year.
#' The figure is [timescales::gg_calendar()] on the calendar's timetable;
#' `autoplot()` returns it, `plot()` draws it.
#'
#' @param object An object of class `calendar`.
#' @param ... Passed to [timescales::gg_calendar()] (`type = "icicle"`):
#'   `fill`, `color_pattern`, `palette`, `labels`, `max_labels`,
#'   `max_segments`, `border`.
#' @param reference Optional full `calendar`. When supplied, `object` is drawn
#'   as a subset of it: the layout is the reference's, and the timeslices
#'   absent from `object` are left empty. Use it to see which part of a full
#'   calendar a sampled calendar covers.
#'
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' cal <- newCalendar(make_timetable(timeslices3), name = "m12h24")
#' plot(cal)
#' autoplot(cal, fill = "share")
#'
#' # which timeslices a reduced calendar covers within the full one
#' autoplot(calendars$d365_h24_1dpm, reference = calendars$d365_h24)
#' }
#' @rdname plot_calendar_method
#' @exportS3Method ggplot2::autoplot
autoplot.calendar <- function(object, ..., reference = NULL) {
  check_package("ggplot2")
  cal <- .ts_calendar(object)
  if (is.null(cal)) {
    stop("The calendar has no timeframes below ANNUAL to draw.", call. = FALSE)
  }
  ref <- NULL
  if (!is.null(reference)) {
    if (!methods::is(reference, "calendar")) {
      stop("`reference` must be a `calendar` object.", call. = FALSE)
    }
    ref <- .ts_calendar(reference)
  }
  p <- timescales::gg_calendar(cal, type = "icicle", ..., reference = ref)
  if (!is_empty(object@name)) p <- p + ggplot2::labs(title = object@name)
  if (!is_empty(object@desc)) p <- p + ggplot2::labs(subtitle = object@desc)
  p
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
    .say("None of the supplied commodit",
            if (length(comms) > 1) "ies have" else "y has",
            " emission factors (`@emis`) to plot.")
    return(invisible(NULL))
  }

  # Preserve the order commodities were supplied in.
  df$commodity <- factor(df$commodity, levels = unique(df$commodity))

  units <- unique(stats::na.omit(df$unit))
  y_lab <- if (length(units) == 1 && nzchar(units)) units else "NA"

  p <- ggplot2::ggplot(df,
      ggplot2::aes(x = commodity, y = emis, fill = species)) +
    # Dodge, not stack: factors for different species are not additive.
    ggplot2::geom_col(position = ggplot2::position_dodge2(preserve = "single"),
                      width = 0.8) +
    .key_scale(levels(df$species) %||% sort(unique(as.character(df$species))),
               palette, "emission") +
    ggplot2::labs(x = NULL, y = y_lab) +
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
#' @param ... Optional further `commodity` objects to include in the
#'   comparison, and arguments passed on to `plot_commodity()` -- notably
#'   `palette`, the viridis colour option for the emission-species fill scale.
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
    .say("No year-indexed data to plot for '", obj_name, "'.")
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

  cap_txt <- paste(c(
    paste(c("points: given data",
            if (isTRUE(interpolate)) "lines: interpolated",
            if (!is.null(def_df)) "dotted: defaults"), collapse = " · "),
    if (length(def_skip) > 0)
      paste("defaults not shown:", paste(unique(def_skip), collapse = ", "))),
    collapse = " · ")

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
                  y = NULL, colour = "param",
                  title = if (nzchar(obj_name)) obj_name else NULL,
                  subtitle = .desc_of(object),
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
#' @param units named list of base units passed to the unit resolver for the
#'   axis label; `NULL` (default) resolves them from the object.
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
#' @param style For `supply` only, WHICH CHART: `"profile"` (default) draws
#'   the per-parameter year profile shared by all process classes; `"bar"`
#'   draws the quantity by region over the years; `"regions"` draws the
#'   across-region comparison — availability and cost bars per region, with
#'   an unlimited (`Inf`) availability shown as a translucent full-height
#'   bar; `"map"` draws it on a `map =` geoscale, at whatever level the
#'   object's own regions are declared at.
#' @param param For `supply` only, WHICH QUANTITY: `"availability"` (default,
#'   the bound that binds — `ava.fx` over `ava.up`), one of `"ava.up"`,
#'   `"ava.lo"`, `"ava.fx"` by name, or `"cost"`. The names are the columns
#'   of `@supply`, so the argument, the legend title and the slot agree.
#' @param map For `style = "map"`: a `geoscales::GeoScale`.
#' @param type deprecated, use `param`.
#' @exportS3Method ggplot2::autoplot
autoplot.supply <- function(object, year = NULL, interpolate = TRUE,
                            show_defaults = FALSE, units = NULL,
                            style = c("profile", "bar", "regions", "map"),
                            param = NULL, map = NULL,
                            type = NULL, ...) {
  # `type` named the quantity before 0.92; `param` says the same thing and
  # cannot be confused with `style`, which names the chart. `NULL` means
  # every parameter the object declares, which the chart resolves.
  if (!is.null(type)) param <- type
  style <- match.arg(style)
  .check_units_arg(units)
  if (!is.null(param)) {
    param <- match.arg(param, .supply_params, several.ok = TRUE)
  }
  if (identical(style, "map")) {
    .units_unused(units, "map")
    return(plot_supply_map(object, param = param, year = year,
                           interpolate = interpolate, map = map, ...))
  }
  if (style %in% c("bar", "regions")) .units_unused(units, style)
  switch(style,
    profile = plot_process_year(object, year = year,
                                interpolate = interpolate,
                                show_defaults = show_defaults,
                                units = units, ...),
    bar = plot_supply_bar(object, param = param %||% "availability",
                          year = year,
                          interpolate = interpolate, ...),
    regions = plot_supply_regions(object, param = param, year = year,
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

# The binding availability parameter per (region, cluster, year): `ava.fx`
# wins over `ava.up` on the same key, and a key with neither is unbounded
# (`ava.up` defaults to `Inf`) so it carries no row here. `ava.lo` is a floor,
# a different statement, and is read only when asked for by name.
#
# NOT an annual quantity, which is why this is no longer called
# `.supply_ava_annual()`. Rows that differ only by timeslice are added
# together, with no `pTimesliceShare`: `eqSupAvaUp` bounds `vSupOut` in EACH
# timeslice, so a per-slice bound summed over slices is neither the slice
# bound nor the year's. Where every row is `timeslice = NA` -- the usual case
# for an ANNUAL commodity -- the sum is a no-op and the value is the bound as
# declared.
.supply_ava_bound <- function(d, which = NULL) {
  keep <- if (is.null(which)) c("ava.up", "ava.fx") else which
  a <- d[d$param %in% keep & is.finite(d$value), , drop = FALSE]
  if (nrow(a) == 0) return(NULL)
  kc <- intersect(c("region", "cluster", "year"), names(a))
  a <- a |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(kc, "param")))) |>
    dplyr::summarise(value = sum(.data$value), .groups = "drop") |>
    as.data.frame()
  # the fx-over-up precedence is what "the bound that binds" means; asked for
  # one parameter by name, there is nothing to arbitrate
  if (!is.null(which)) return(a)
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
plot_supply_bar <- function(object, param = .supply_params,
                            year = NULL, interpolate = TRUE, palette = "D",
                            type = NULL, ...) {
  check_package("ggplot2")
  if (!is.null(type)) param <- type   # `type` named this before 0.92
  param <- match.arg(param, .supply_params)
  type <- if (identical(param, "cost")) "cost" else "availability"
  obj_name <- tryCatch(object@name, error = function(e) "")
  d <- .supply_long(object, year = year, interpolate = interpolate, ...)
  if (is.null(d)) {
    .say("No supply data to plot for '", obj_name, "'.")
    return(invisible(NULL))
  }
  d <- .supply_cluster_levels(object, d)
  has_cluster <- "cluster" %in% names(d) &&
    length(unique(stats::na.omit(d$cluster))) > 1
  unit <- tryCatch(getUnit(object), error = function(e) "")
  unit <- if (length(unit) == 1 && !is.na(unit) && nzchar(unit)) unit else NULL

  if (type == "availability") {
    a <- .supply_ava_bound(d, which = if (identical(param, "availability"))
      NULL else param)
    if (is.null(a) || nrow(a) == 0) {
      .say("No finite availability (ava.up/ava.fx) for '", obj_name,
              "': supply is uncapped.")
      return(invisible(NULL))
    }
    a$year_f <- factor(ifelse(is.na(a$year), "all", as.character(a$year)))
    fill_col <- if (has_cluster && "cluster" %in% names(a)) "cluster" else
      "region"
    if (fill_col == "cluster") a <- .facet_cap(a, "region")
    lev <- sort(unique(as.character(a[[fill_col]])))
    a[[fill_col]] <- .key_index(a[[fill_col]], lev)
    key_name <- if (identical(fill_col, "region")) .geoframe_name(object) else
      fill_col
    p <- ggplot2::ggplot(a, ggplot2::aes(x = .data$year_f, y = .data$value,
                                         fill = .data[[fill_col]])) +
      ggplot2::geom_col(position = "stack") +
      .key_scale(lev, palette, key_name)
    if (fill_col == "cluster" && length(unique(a$region)) > 1)
      p <- p + ggplot2::facet_wrap(~region)
    p <- p + ggplot2::labs(
      x = "year",
      y = .supply_labels(object)[[param]],
      title = if (nzchar(obj_name)) obj_name else NULL,
      subtitle = .desc_of(object),
      caption = "availability: ava.fx, else ava.up") +
      theme_energyRt() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                     axis.text.x = ggplot2::element_text(angle = 45,
                                                         hjust = 1)) +
      .legend_compact(length(lev))
    .facet_caption(p, a)
  } else {
    co <- d[d$param == "cost" & !is.na(d$value), , drop = FALSE]
    if (nrow(co) == 0) {
      .say("No supply cost to plot for '", obj_name, "'.")
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
    lev <- sort(unique(as.character(co$region)))
    ggplot2::ggplot(co, aes_ln) +
      ggplot2::geom_step(direction = "hv", linewidth = 0.6, na.rm = TRUE) +
      ggplot2::geom_point(size = 1.6, na.rm = TRUE) +
      .key_scale(lev, palette, .geoframe_name(object), aes = "colour") +
      ggplot2::labs(
        x = "year",
        y = .supply_labels(object)[["cost"]],
        linetype = "cluster",
        title = if (nzchar(obj_name)) obj_name else NULL,
        subtitle = .desc_of(object),
        caption = "cost: mean over timeslices") +
      theme_energyRt() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
      .legend_compact(length(lev))
  }
}

# What a supply plot can draw. `availability` is whichever bound binds
# (`ava.fx` over `ava.up`); the rest name a column of `@supply` directly, so
# the argument, the legend title and the slot all say the same word.
#' @noRd
.supply_params <- c("availability", "ava.up", "ava.lo", "ava.fx", "cost")

# The named parameters this object actually declares. `availability` is left
# out: it is whichever of `ava.fx`/`ava.up` binds, so including it would draw
# the same numbers twice under two titles.
#' @noRd
.supply_filled <- function(v) {
  nm <- setdiff(.supply_params, "availability")
  nm <- nm[vapply(nm, function(k) k %in% names(v) && any(!is.na(v[[k]])),
                  logical(1))]
  # a supply with no bound at all still has its default `ava.up = Inf`
  if (!length(nm)) "ava.up" else nm
}

# Axis and legend labels for the supply quantities: the PARAMETER as it is
# declared, and its unit, with nothing added.
#
# `ava.up` is NOT a yearly total. `eqSupAvaUp` bounds `vSupOut` in each
# timeslice and carries no `pTimesliceShare`, so what the number amounts to
# over a year depends on the commodity's timeframe -- one slice for an ANNUAL
# commodity, 96 or 8760 for an hourly one. The object does not know that, so
# the plot states the parameter and lets the reader keep the timeframe in
# mind, instead of printing a "/year" it cannot justify.
#
# The cost unit IS derivable: `costs` over `activity`, both declared.
#' @noRd
.supply_labels <- function(object) {
  act <- .object_units(object, "activity")
  if (is.null(act)) {
    u <- tryCatch(getUnit(object), error = function(e) NULL)
    if (length(u) == 1 && !is.na(u) && nzchar(u)) act <- u
  }
  cst <- .object_units(object, "currency")
  d <- tryCatch(as.data.frame(object@supply), error = function(e) NULL)
  has <- function(k) !is.null(d) && k %in% names(d) && any(!is.na(d[[k]]))
  ava <- if (has("ava.fx") && has("ava.up")) "ava" else
    if (has("ava.fx")) "ava.fx" else "ava.up"
  # `ava.up` and `cost` are short enough to sit on one line with the unit
  two <- function(nm, u) if (is.null(u)) nm else paste0(nm, ", ", u)
  cost_u <- if (!is.null(cst) && !is.null(act)) paste0(cst, "/", act) else NULL
  c(availability = two(ava, act),
    ava.up = two("ava.up", act),
    ava.lo = two("ava.lo", act),
    ava.fx = two("ava.fx", act),
    cost = two("cost", cost_u))
}

# Availability and cost per region for one display year -- the numbers behind
# both the across-region bars and the map, derived once. Returns the display
# year as an attribute, and NA where the supply declares no bound (uncapped)
# or no cost.
#' @noRd
.supply_region_values <- function(object, year = NULL, interpolate = TRUE,
                                  ...) {
  obj_name <- tryCatch(object@name, error = function(e) "")
  d <- .supply_long(object, year = NULL, interpolate = interpolate, ...)
  if (is.null(d)) {
    .say("No supply data to plot for '", obj_name, "'.")
    return(NULL)
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

  # `availability` is whichever bound binds; the three names are also
  # addressable on their own, `ava.lo` among them
  one <- function(which) {
    a <- .supply_ava_bound(dd, which = which)
    v <- stats::setNames(rep(NA_real_, length(regions)), regions)
    if (!is.null(a) && nrow(a) > 0) {
      per_reg <- tapply(a$value, a$region, sum, na.rm = TRUE)
      v[names(per_reg)[names(per_reg) %in% regions]] <-
        per_reg[names(per_reg) %in% regions]
      if ("(all)" %in% names(per_reg) && !"(all)" %in% regions)
        v[] <- ifelse(is.na(v), per_reg[["(all)"]], v)
    }
    unname(v)
  }
  ava <- stats::setNames(one(NULL), regions)
  co <- dd[dd$param == "cost" & !is.na(dd$value), , drop = FALSE]
  cost <- stats::setNames(rep(NA_real_, length(regions)), regions)
  if (nrow(co) > 0) {
    per_reg <- tapply(co$value, co$region, mean, na.rm = TRUE)
    cost[names(per_reg)[names(per_reg) %in% regions]] <-
      per_reg[names(per_reg) %in% regions]
    if ("(all)" %in% names(per_reg) && !"(all)" %in% regions)
      cost[] <- ifelse(is.na(cost), per_reg[["(all)"]], cost)
  }
  if (all(is.na(ava)) && all(is.na(cost)) && all(is.na(one("ava.lo")))) {
    .say("No availability or cost data to plot for '", obj_name, "'.")
    return(NULL)
  }
  out <- data.frame(region = regions, availability = unname(ava),
                    ava.up = one("ava.up"), ava.lo = one("ava.lo"),
                    ava.fx = one("ava.fx"), cost = unname(cost),
                    check.names = FALSE, stringsAsFactors = FALSE)
  attr(out, "year") <- yr
  out
}

# Across-region availability and cost bars for one display year. Not
# exported: autoplot(sup, style = "regions") reaches it.
plot_supply_regions <- function(object, year = NULL, interpolate = TRUE,
                                param = NULL, ...) {
  check_package("ggplot2")
  obj_name <- tryCatch(object@name, error = function(e) "")
  v <- .supply_region_values(object, year = year, interpolate = interpolate, ...)
  if (is.null(v)) return(invisible(NULL))
  regions <- v$region
  yr <- attr(v, "year")
  if (is.null(param)) param <- .supply_filled(v)
  param <- intersect(match.arg(param, .supply_params, several.ok = TRUE),
                     names(v))

  # One panel per parameter. A ceiling that is not declared is `Inf`, which
  # has no height: it draws at the panel maximum, translucent and labelled,
  # rather than being dropped as if the supply were absent.
  lab_map <- .supply_labels(object)
  pdf_ <- do.call(rbind, lapply(param, function(k) {
    val <- stats::setNames(v[[k]], regions)
    unc <- is.na(val) & k %in% c("availability", "ava.up", "ava.fx")
    ymax <- if (any(!is.na(val))) max(val, na.rm = TRUE) else 1
    data.frame(region = regions, measure = k,
               value = ifelse(unc, ymax, val), uncapped = unc,
               stringsAsFactors = FALSE)
  }))
  pdf_ <- pdf_[!is.na(pdf_$value), , drop = FALSE]
  pdf_$measure <- factor(lab_map[pdf_$measure],
                         levels = unname(lab_map[param]))

  lab_df <- pdf_[pdf_$uncapped, , drop = FALSE]
  p <- ggplot2::ggplot(pdf_, ggplot2::aes(x = .data$region, y = .data$value)) +
    ggplot2::geom_col(ggplot2::aes(alpha = .data$uncapped),
                      fill = "#4E79A7", show.legend = FALSE) +
    ggplot2::scale_alpha_manual(values = c(`FALSE` = 1, `TRUE` = 0.35)) +
    ggplot2::facet_wrap(~measure, scales = "free_y") +
    ggplot2::labs(
      x = NULL, y = NULL,
      title = if (nzchar(obj_name)) obj_name else NULL,
      subtitle = .desc_of(object),
      caption = paste(c(
        if (!is.na(yr)) paste("year:", yr),
        if (any(pdf_$uncapped))
          "translucent full-height bars: unlimited availability"),
        collapse = " · ")) +
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

# Availability or cost drawn on the map, for one display year. A supply names
# the regions it exists in, and those may be at any level -- `SUP_COA` is
# declared at the zone -- so the level is read off the names rather than
# assumed, the same way a trade route's is. Not exported:
# autoplot(sup, style = "map") reaches it.
#' @noRd
plot_supply_map <- function(object, param = NULL,
                            year = NULL, interpolate = TRUE, map = NULL,
                            palette = "D", type = NULL, ...) {
  check_package("ggplot2")
  check_package("geoscales")
  if (!is.null(type)) param <- type   # `type` named this before 0.92
  if (!is.null(param)) {
    param <- match.arg(param, .supply_params, several.ok = TRUE)
  }
  # Several parameters are several maps, not one faceted map: ggplot2 has a
  # single fill scale per plot, and `ava.up` in PJ cannot share a colour bar
  # with `cost` in MUSD/PJ. Each gets its own scale and they are composed.
  if (is.null(param) || length(param) > 1L) {
    vv <- .supply_region_values(object, year = year,
                                interpolate = interpolate, ...)
    if (is.null(vv)) return(invisible(NULL))
    if (is.null(param)) param <- .supply_filled(vv)
    if (length(param) > 1L) {
      check_package("patchwork")
      ps <- lapply(param, function(k)
        plot_supply_map(object, param = k, year = year,
                        interpolate = interpolate, map = map,
                        palette = palette, ...))
      ps <- Filter(Negate(is.null), ps)
      if (!length(ps)) return(invisible(NULL))
      if (length(ps) == 1L) return(ps[[1]])
      # the object's name and description belong to the set, not to each panel
      ps <- lapply(ps, function(p)
        p + ggplot2::labs(title = NULL, subtitle = NULL))
      return(patchwork::wrap_plots(ps) +
               patchwork::plot_annotation(
                 title = tryCatch(object@name, error = function(e) NULL),
                 subtitle = .desc_of(object)))
    }
  }
  type <- param
  obj_name <- tryCatch(object@name, error = function(e) "")
  if (is.null(map)) {
    stop("plot_supply_map: pass a `map` (a `geoscales::GeoScale`), e.g. ",
         "`topia$geoscales$honeycomb`.", call. = FALSE)
  }
  if (!is_geoscale(map)) {
    stop("plot_supply_map: `map` must be a `geoscales::GeoScale`.",
         call. = FALSE)
  }
  v <- .supply_region_values(object, year = year, interpolate = interpolate, ...)
  if (is.null(v)) return(invisible(NULL))
  yr_v <- attr(v, "year")
  v$value <- v[[type]]
  # A supply that declares no `ava.up` is not missing a value: the default IS
  # `Inf` (the catalogue default), so the bound exists and is unlimited. There is
  # nothing to shade, but where the supply exists is worth drawing, so the
  # fill becomes the bound itself -- stated as `Inf`, which is what the model
  # will use -- rather than a word the plot made up.
  uncapped <- type %in% c("availability", "ava.up") && all(is.na(v$value)) &&
    nrow(v) > 0
  if (uncapped) {
    v$value <- "Inf"
  } else {
    v <- v[!is.na(v$value), , drop = FALSE]
  }
  v <- v[, c("region", "value"), drop = FALSE]
  attr(v, "year") <- yr_v
  if (nrow(v) == 0) {
    .say("'", obj_name, "' declares no ", type,
            if (type %in% c("availability", "ava.up", "ava.fx"))
              "; it is unlimited wherever it exists" else "", ".")
    return(invisible(NULL))
  }
  # the level the supply's own regions live at
  frames <- tryCatch(as.character(geoscales::geoscale_geoframes(map)),
                     error = function(e) character())
  lvl <- NULL
  for (f in frames) {
    mem <- tryCatch(as.character(geoscales::geoscale_regions(map, geoframe = f)),
                    error = function(e) character())
    if (all(v$region %in% mem)) { lvl <- f; break }
  }
  if (is.null(lvl)) {
    stop("plot_supply_map: the regions of '", obj_name, "' (",
         paste(v$region, collapse = ", "), ") are not all members of any ",
         "level of `map` (", paste(frames, collapse = ", "), ").",
         call. = FALSE)
  }
  # the map keys the data by the geoframe's own name, not by "region": a
  # zone-level supply joins on `zone`.
  names(v)[names(v) == "region"] <- lvl
  yr <- attr(v, "year")
  lab <- .supply_labels(object)[[type]]
  # the unit belongs under the quantity, not trailing after it: a legend
  # title is a narrow column
  p <- geoscales::gg_geoscale(
    map, type = "map", data = v, geoframe = lvl, z = "value",
    palette = palette) +
    ggplot2::labs(title = if (nzchar(obj_name)) obj_name else NULL,
                  subtitle = .desc_of(object), fill = lab)
  if (uncapped) {
    # the map scales a fill continuously; an unbounded one is not
    p <- suppressMessages(
      # `breaks` keeps NA out of the LEGEND; `na.translate = FALSE` would
      # also drop those rows and take the background regions with them
      p + ggplot2::scale_fill_manual(
        values = stats::setNames("#4E79A7", "Inf"), na.value = "grey85",
        breaks = "Inf"))
  }
  lo <- tryCatch(as.data.frame(object@supply)$ava.lo, error = function(e) NULL)
  has_lo <- !is.null(lo) && any(!is.na(lo) & lo != 0)
  p + ggplot2::labs(caption = paste(c(
    if (!is.na(yr)) paste("year:", yr),
    # `.supply_ava_bound()` reads `ava.up` and `ava.fx` only
    if (has_lo && identical(type, "availability"))
      "ava.lo declared, not shown"),
    collapse = " · "))
}

# Per-region values of an object's year-indexed parameters, for one display
# year -- the generic behind the object maps, keyed on `.process_year_slot`
# so every class that has a year profile can also have a map.
#
# Rows are collapsed to one value per (region, param). A quantity is SUMMED
# over the dimensions the map does not show (a demand's timeslices add up to
# the year's total); a price is AVERAGED, because adding a cost over
# timeslices would be arithmetic on a rate. `.price_params` names which is
# which: anything not listed is treated as a quantity.
#' @noRd
.price_params <- c("cost", "price", "invcost", "fixom", "varom", "cvarom",
                   "avarom", "eac", "wacc", "payback", "retcost")

# Which unit a parameter is measured in. `stock` and the capacity bounds are
# a CAPACITY (GW); flows and bounds on them are an ACTIVITY (GWh, PJ); the
# list above is a price. Getting this from the name is what lets one map
# engine serve six classes.
#' @noRd
.capacity_params <- c("stock", "cap.lo", "cap.up", "cap.fx",
                      "ncap.lo", "ncap.up", "ncap.fx",
                      "ret.lo", "ret.up", "ret.fx")

#' @noRd
.object_region_values <- function(object, year = NULL, interpolate = TRUE,
                                  ...) {
  cls <- class(object)[1]
  nm <- tryCatch(object@name, error = function(e) "")
  slots <- .process_year_slot[[cls]]
  if (is.null(slots)) {
    stop("no map for class '", cls, "': it declares no year-indexed slots.",
         call. = FALSE)
  }
  d <- .obj_long_get(object, slots, interpolate, NULL, ...)
  if (is.null(d) || !nrow(d) || !("region" %in% names(d))) {
    .say("No region-indexed data to map for '", nm, "'.")
    return(NULL)
  }
  d <- d[!is.na(d$region) & d$region != "(all)", , drop = FALSE]
  if (!nrow(d)) {
    .say("'", nm, "' declares nothing per region.")
    return(NULL)
  }
  yrs <- sort(unique(stats::na.omit(as.integer(d$year))))
  yr <- if (!is.null(year)) as.integer(year)[1] else
    if (length(yrs)) max(yrs) else NA_integer_
  if (!is.na(yr) && "year" %in% names(d)) {
    d <- d[is.na(d$year) | d$year == yr, , drop = FALSE]
  }
  agg <- function(p, v)
    if (sub("^(inp|out|stg)[.]", "", p) %in% .price_params)
      mean(v, na.rm = TRUE) else sum(v, na.rm = TRUE)
  parts <- lapply(split(d, d$param), function(x) {
    data.frame(region = names(tapply(x$value, x$region, function(v) 1)),
               param = x$param[1],
               value = as.numeric(tapply(x$value, x$region,
                                         function(v) agg(x$param[1], v))),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, parts)
  out <- out[is.finite(out$value), , drop = FALSE]
  if (!nrow(out)) {
    .say("Nothing finite to map for '", nm, "'.")
    return(NULL)
  }
  attr(out, "year") <- yr
  out
}

# The level a set of region names lives at, within a geoscale. A trade route,
# a zone-declared supply and a region-declared technology each name their own
# level, so it is read off the names rather than asked of the caller.
#' @noRd
.region_level <- function(map, regions, what = "object") {
  frames <- tryCatch(as.character(geoscales::geoscale_geoframes(map)),
                     error = function(e) character())
  for (f in frames) {
    mem <- tryCatch(as.character(geoscales::geoscale_regions(map, geoframe = f)),
                    error = function(e) character())
    if (all(regions %in% mem)) return(f)
  }
  stop("the regions of ", what, " (", paste(regions, collapse = ", "),
       ") are not all members of any level of `map` (",
       paste(frames, collapse = ", "), ").", call. = FALSE)
}

# One parameter of any year-indexed object, drawn on the map; several are
# composed, each keeping its own scale, because one plot has one fill scale
# and `invcost` in MUSD/GW cannot share a colour bar with `stock` in GW.
#' @noRd
plot_object_map <- function(object, param = NULL, year = NULL,
                            interpolate = TRUE, map = NULL, palette = "D",
                            all_params = FALSE, ...) {
  check_package("ggplot2")
  check_package("geoscales")
  nm <- tryCatch(object@name, error = function(e) "")
  if (is.null(map) || !is_geoscale(map)) {
    stop("pass a `map` (a `geoscales::GeoScale`), e.g. ",
         "`topia$geoscales$honeycomb`.", call. = FALSE)
  }
  v <- .object_region_values(object, year = year, interpolate = interpolate,
                             ...)
  if (is.null(v)) return(invisible(NULL))
  have <- unique(v$param)
  if (is.null(param)) {
    # A process declares many parameters -- capacity bounds, three costs --
    # and mapping all of them at once is a wall of panels, so these default
    # to the first. The flow classes have few and default to all of them.
    param <- if (isTRUE(all_params)) have else have[1]
  }
  param <- intersect(param, have)
  if (!length(param)) {
    .say("'", nm, "' declares none of the requested parameter(s); it has: ",
            paste(have, collapse = ", "), ".")
    return(invisible(NULL))
  }
  if (length(param) > 1L) {
    check_package("patchwork")
    ps <- Filter(Negate(is.null), lapply(param, function(k)
      plot_object_map(object, param = k, year = year,
                      interpolate = interpolate, map = map,
                      palette = palette, ...)))
    if (!length(ps)) return(invisible(NULL))
    if (length(ps) == 1L) return(ps[[1]])
    ps <- lapply(ps, function(p) p + ggplot2::labs(title = NULL,
                                                   subtitle = NULL))
    return(patchwork::wrap_plots(ps) + patchwork::plot_annotation(
      title = if (nzchar(nm)) nm else NULL, subtitle = .desc_of(object)))
  }
  d <- v[v$param == param, c("region", "value"), drop = FALSE]
  lvl <- .region_level(map, d$region, paste0("'", nm, "'"))
  names(d)[names(d) == "region"] <- lvl
  # `import`, `export` and `demand` carry no `@units` table, but their scalar
  # the own unit IS the commodity's, which is what a quantity parameter is
  # measured in. A price needs a currency and there is none to fall back on,
  # so it stays unlabelled rather than guessed.
  base <- sub("^(inp|out|stg)[.]", "", param)   # storage names its three parts
  u <- if (base %in% .price_params) .object_units(object, "currency") else
    if (base %in% .capacity_params) .object_units(object, "capacity") else {
      .object_units(object, "activity") %||% {
        cu <- tryCatch(getUnit(object), error = function(e) NULL)
        if (length(cu) == 1 && !is.na(cu) && nzchar(cu)) cu else NULL
      }
    }
  geoscales::gg_geoscale(
    map, type = "map", data = d, geoframe = lvl, z = "value",
    palette = palette) +
    ggplot2::labs(
      title = if (nzchar(nm)) nm else NULL, subtitle = .desc_of(object),
      fill = if (is.null(u)) param else paste0(param, ", ", u),
      caption = if (!is.na(attr(v, "year")))
        paste("year:", attr(v, "year")) else NULL)
}

# Demand gets its own plot: a timeslice-resolved demand has too many series for the
# generic per-parameter chart (one line per timeslice).

# Above this many discrete keys a fill legend stops being readable and the
# scale is drawn as a colour bar with a handful of labels instead.
.plot_max_keys <- 12L

# The object's own description: the only thing a subtitle carries.
#' @noRd
.desc_of <- function(object) {
  d <- tryCatch(object@desc, error = function(e) NULL)
  if (is_empty(d)) NULL else as.character(d)[1]
}

# The unit an axis is in, `"NA"` when the object declares none. `per` appends
# a denominator ("/year"), `inv` inverts it ("per PJ").
#' @noRd
.unit_of <- function(object, per = NULL, inv = FALSE) {
  u <- tryCatch(getUnit(object), error = function(e) NA_character_)
  if (!(length(u) == 1 && !is.na(u) && nzchar(u))) return("NA")
  if (isTRUE(inv)) paste0("per ", u) else paste0(u, per)
}

# A viridis scale that stays readable as the categories grow: a key per level
# up to `.plot_max_keys`, a colour bar with evenly spaced labels beyond it.
# Past the threshold the caller must recode its column with `.key_index()`,
# since the continuous scale maps a number.
#' @noRd
.key_scale <- function(lev, palette, name, aes = "fill") {
  disc <- if (identical(aes, "colour")) ggplot2::scale_colour_viridis_d else
    ggplot2::scale_fill_viridis_d
  cont <- if (identical(aes, "colour")) ggplot2::scale_colour_viridis_c else
    ggplot2::scale_fill_viridis_c
  if (length(lev) <= .plot_max_keys) {
    disc(option = palette, end = 0.9, name = name)
  } else {
    brk <- unique(round(seq(1, length(lev), length.out = 6)))
    cont(option = palette, end = 0.9, name = name, breaks = brk,
         labels = lev[brk])
  }
}

#' @noRd
.key_index <- function(x, lev) {
  if (length(lev) <= .plot_max_keys) x else
    as.integer(factor(as.character(x), levels = lev))
}

# `region` selects which of an object's regions to draw: a vector of codes,
# or a number to take that many in the order they appear. An object from a
# continental model carries every region, and faceting on all of them leaves
# the panels too small to read. Returns zero rows when the filter selects
# nothing, having said so; the caller decides what to return.
#' @noRd
.filter_region <- function(d, region, nm, what = "data") {
  if (is.null(region) || !("region" %in% names(d))) return(d)
  have <- unique(as.character(d$region))
  keep <- if (is.numeric(region))
    utils::head(have, max(1L, as.integer(region[1]))) else as.character(region)
  miss <- setdiff(keep, have)
  if (length(miss)) {
    warning("region(s) not in '", nm, "': ", paste(miss, collapse = ", "),
            call. = FALSE)
  }
  keep <- intersect(keep, have)
  if (!length(keep)) {
    .say("No ", what, " left after the `region` filter for '", nm, "'.")
    return(d[0, , drop = FALSE])
  }
  d[as.character(d$region) %in% keep, , drop = FALSE]
}

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
#'   and `"heatmap"` they filter which given years are shown. An object that
#'   names no year applies to every year and so has no default window of its
#'   own: `"area"` then needs `year =`, while `"line"` and `"heatmap"` read
#'   its shape without one.
#' @param interpolate Logical, default `TRUE`: for `"area"`, interpolate the
#'   annual totals over the target years; `FALSE` aggregates only the given
#'   data years.
#' @param palette viridis palette option, as in [ggplot2::scale_fill_viridis_d()].
#' @param region which regions to draw: a vector of region codes, or a number
#'   to take that many in the order they appear. `NULL` (default) draws every
#'   region the object carries.
#' @param ... Passed to [getData()] by `style = "area"`, which interpolates;
#'   the `"line"` and `"heatmap"` styles read `@demand` directly and ignore it.
#'
#' @return A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).
#' @keywords internal
plot_demand <- function(object, style = c("area", "line", "heatmap"),
                        year = NULL, interpolate = TRUE, palette = "D",
                        calendar = NULL, region = NULL, ...) {
  check_package("ggplot2")
  style <- match.arg(style)

  obj_name <- tryCatch(object@name, error = function(e) "")
  obj_desc <- tryCatch(object@desc, error = function(e) NULL)
  if (is_empty(obj_desc)) obj_desc <- NULL
  raw <- as.data.frame(object@demand)
  raw <- raw[!is.na(raw$demand), , drop = FALSE]
  if (nrow(raw) == 0) {
    .say("No demand data to plot for '", obj_name, "'.")
    return(invisible(NULL))
  }
  if (!"region" %in% names(raw)) raw$region <- "(all)"
  raw$region[is.na(raw$region)] <- "(all)"   # aggregate() drops NA groups
  # Applied here, so every style honours it -- the area branch reaches
  # `getData()` through `...`, the profile branches read `@demand` directly.
  raw <- .filter_region(raw, region, obj_name, "demand")
  if (nrow(raw) == 0) return(invisible(NULL))

  # "line" and "heatmap" share the profile engine with plot_weather(), so a
  # demand shape and a weather factor read identically.
  if (style %in% c("line", "heatmap")) {
    d <- raw[!is.na(raw$timeslice), , drop = FALSE]
    if (!is.null(year) && "year" %in% names(d)) {
      d <- d[d$year %in% year, , drop = FALSE]
    }
    if (nrow(d) == 0) {
      .say("No timeslice-resolved demand to draw for '", obj_name, "'.")
      return(invisible(NULL))
    }
    d$wval <- d$demand
    u <- tryCatch(getUnit(object), error = function(e) NA_character_)
    return(.plot_profile(
      d, style = style, calendar = calendar, palette = palette,
      unit_lab = .unit_of(object),
      title = if (nzchar(obj_name)) obj_name else NULL,
      subtitle = obj_desc))
  }

  if (style == "area") {
    # annual totals: interpolate per timeslice over the target years, sum timeslices
    year_target <- year
    if (is.null(year_target)) {
      obsy <- sort(unique(stats::na.omit(as.integer(raw$year))))
      year_target <- if (length(obsy) >= 2) seq(min(obsy), max(obsy)) else obsy
    }
    # A value given for no year applies to every year, so an object that names
    # none carries no window of its own to lay on an annual axis.
    if (!length(year_target)) {
      .say("'", obj_name, "' names no year: pass `year =` for an annual ",
              "axis, or style = \"line\"/\"heatmap\" to read its shape.")
      return(invisible(NULL))
    }
    itp <- if (isTRUE(interpolate))
      .obj_long_get(object, "demand", TRUE, year_target, ...) else NULL
    tot <- if (!is.null(itp) && nrow(itp) > 0) {
      if (!"region" %in% names(itp)) itp$region <- "(all)"
      itp$region[is.na(itp$region)] <- "(all)"
      # `itp` comes back from getData(), not from the filtered `raw`
      itp <- .filter_region(itp, region, obj_name, "demand")
      stats::aggregate(value ~ region + year, itp, sum)
    } else {
      stats::aggregate(demand ~ region + year, raw, sum) |>
        stats::setNames(c("region", "year", "value"))
    }
    # The points are the GIVEN values, and a row naming no year has no year to
    # sit at -- aggregate() would drop it, taking every other row with it.
    given <- raw[!is.na(raw$year), , drop = FALSE]
    pts <- if (nrow(given)) {
      stats::aggregate(demand ~ region + year, given, sum)
    } else {
      data.frame(region = character(), year = integer(), demand = numeric())
    }
    # `geom_area()` puts the LAST fill level at the bottom, so a band's upper
    # edge is the cumulative total from the last level up to and including its
    # own. Carry the given values onto that edge: at their own level they sit in
    # a cloud at the foot of an eleven-region stack, unmatchable to a band.
    lev <- sort(unique(as.character(tot$region)))
    pts$region <- factor(as.character(pts$region), levels = lev)
    pts <- pts[!is.na(pts$region), , drop = FALSE]
    pts <- pts[order(pts$year, -as.integer(pts$region)), , drop = FALSE]
    pts$stacked <- stats::ave(pts$demand, pts$year, FUN = cumsum)
    # The unit goes on the y axis and the legend names the spatial level the
    # bands are, so a reader is told what is measured and over what.
    u <- tryCatch(getUnit(object), error = function(e) NA_character_)
    y_lab <- if (length(u) == 1 && !is.na(u) && nzchar(u)) u else "NA"
    gf_lab <- .geoframe_name(object)
    # A key per region while they fit, a colour bar carrying a few region
    # labels once they do not -- forty keys is a list, not a legend.
    if (length(lev) <= .plot_max_keys) {
      fill_scale <- ggplot2::scale_fill_viridis_d(option = palette, end = 0.9,
                                                  name = gf_lab)
    } else {
      tot$region <- as.integer(factor(as.character(tot$region), levels = lev))
      pts$region <- as.integer(pts$region)
      brk <- unique(round(seq(1, length(lev), length.out = 6)))
      fill_scale <- ggplot2::scale_fill_viridis_c(option = palette, end = 0.9,
                                                  name = gf_lab,
                                                  breaks = brk,
                                                  labels = lev[brk])
    }
    # The points share the fill scale (`shape = 21` fills), so each given value
    # carries its own band's colour and earns no legend of its own.
    p <- ggplot2::ggplot(tot,
        ggplot2::aes(.data$year, .data$value, fill = .data$region,
                     group = .data$region)) +
      ggplot2::geom_area() +
      ggplot2::geom_point(data = pts,
        ggplot2::aes(.data$year, .data$stacked, fill = .data$region),
        inherit.aes = FALSE, shape = 21, colour = "grey25", size = 2,
        na.rm = TRUE) +
      fill_scale +
      ggplot2::labs(x = "year", y = y_lab,
                    title = if (nzchar(obj_name)) obj_name else NULL,
                    subtitle = obj_desc) +
      theme_energyRt() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    return(p)
  }

  invisible(NULL)
}

#' @rdname plot_demand
#' @exportS3Method ggplot2::autoplot
autoplot.demand <- function(object,
                            style = c("area", "line", "heatmap", "map"),
                            year = NULL, interpolate = TRUE, palette = "D",
                            calendar = NULL, region = NULL, param = NULL,
                            map = NULL, ...) {
  style <- match.arg(style)
  if (identical(style, "map")) {
    return(plot_object_map(object, param = param, year = year,
                           interpolate = interpolate, map = map,
                           palette = palette, all_params = TRUE, ...))
  }
  plot_demand(object, style = style, year = year, interpolate = interpolate,
              palette = palette, calendar = calendar, region = region, ...)
}

#' @rdname autoplot.process
#' @exportS3Method ggplot2::autoplot
autoplot.import <- function(object, year = NULL, interpolate = TRUE,
                            show_defaults = FALSE, units = NULL,
                            style = c("profile", "map"), param = NULL,
                            map = NULL, ...) {
  style <- match.arg(style)
  .check_units_arg(units)
  if (identical(style, "map")) {
    .units_unused(units, "map")
    return(plot_object_map(object, param = param, year = year,
                           interpolate = interpolate, map = map,
                           all_params = TRUE, ...))
  }
  plot_process_year(object, year = year, interpolate = interpolate,
                    show_defaults = show_defaults, units = units, ...)
}

#' @rdname autoplot.process
#' @exportS3Method ggplot2::autoplot
autoplot.export <- function(object, year = NULL, interpolate = TRUE,
                            show_defaults = FALSE, units = NULL,
                            style = c("profile", "map"), param = NULL,
                            map = NULL, ...) {
  style <- match.arg(style)
  .check_units_arg(units)
  if (identical(style, "map")) {
    .units_unused(units, "map")
    return(plot_object_map(object, param = param, year = year,
                           interpolate = interpolate, map = map,
                           all_params = TRUE, ...))
  }
  plot_process_year(object, year = year, interpolate = interpolate,
                    show_defaults = show_defaults, units = units, ...)
}

#' @rdname autoplot.process
#' @exportS3Method ggplot2::autoplot
autoplot.technology <- function(object, year = NULL, interpolate = TRUE,
                                show_defaults = FALSE, units = NULL,
                                style = c("profile", "map"), param = NULL,
                                map = NULL, ...) {
  style <- match.arg(style)
  .check_units_arg(units)
  if (identical(style, "map")) {
    .units_unused(units, "map")
    return(plot_object_map(object, param = param, year = year,
                           interpolate = interpolate, map = map,
                           all_params = FALSE, ...))
  }
  plot_process_year(object, year = year, interpolate = interpolate,
                    show_defaults = show_defaults, units = units, ...)
}

#' @rdname autoplot.process
#' @exportS3Method ggplot2::autoplot
autoplot.storage <- function(object, year = NULL, interpolate = TRUE,
                             show_defaults = FALSE, units = NULL,
                             style = c("profile", "map"), param = NULL,
                             map = NULL, ...) {
  style <- match.arg(style)
  .check_units_arg(units)
  if (identical(style, "map")) {
    .units_unused(units, "map")
    return(plot_object_map(object, param = param, year = year,
                           interpolate = interpolate, map = map,
                           all_params = FALSE, ...))
  }
  plot_process_year(object, year = year, interpolate = interpolate,
                    show_defaults = show_defaults, units = units, ...)
}


# Tax / subsidy / user-constraint plots ----------------------------------------
# These are NOT processes: a tax, subsidy or user constraint imposes a value (a
# rate or a bound) that varies by year. getData() is not defined for them, so we
# read the control path straight from the object's slot: points at the given
# years, connected by the linear interpolation the model uses between them.

.lever_spec <- list(
  # `val` lists the candidate value columns; the ones the object actually sets
  # are what gets drawn. A levy can be written on any of its bases, so pinning
  # this to `bal` drew nothing at all for an act-, cap- or ncap-only
  # instrument.
  tax        = list(slot = "tax", ylab = "tax",
                    val = c("bal", "inp", "out", "act", "cap", "ncap", "rate")),
  subsidy    = list(slot = "subsidy", ylab = "subsidy",
                    val = c("bal", "inp", "out", "act", "cap", "ncap", "rate")),
  constraint = list(slot = "rhs", val = "rhs", ylab = "rhs")
)

plot_lever_year <- function(object, year = NULL, interpolate = TRUE, ...) {
  check_package("ggplot2")
  cls  <- class(object)[1]
  spec <- .lever_spec[[cls]]
  if (is.null(spec)) stop("plot_lever_year: unsupported class '", cls, "'.", call. = FALSE)
  nm <- tryCatch(object@name, error = function(e) "")
  d  <- tryCatch(as.data.frame(methods::slot(object, spec$slot)), error = function(e) NULL)
  .none <- function() {
    .say("No year-indexed data to plot for '", nm, "'.")
    invisible(NULL)
  }
  if (is.null(d) || nrow(d) == 0 || !("year" %in% names(d))) return(.none())
  vals <- intersect(spec$val, names(d))
  vals <- vals[vapply(vals, function(v) any(!is.na(d[[v]])), logical(1))]
  if (length(vals) == 0) return(.none())

  # one row per (source row x set base), so an instrument carrying two bases
  # draws both instead of only the first
  keep <- setdiff(names(d), spec$val)
  d <- do.call(rbind, lapply(vals, function(v) {
    z <- d[!is.na(d$year) & !is.na(d[[v]]), keep, drop = FALSE]
    if (nrow(z) == 0) return(NULL)
    z$base <- v
    z$.val <- as.numeric(d[[v]][!is.na(d$year) & !is.na(d[[v]])])
    z
  }))
  if (is.null(d) || nrow(d) == 0) return(.none())
  rownames(d) <- NULL
  d$year <- as.integer(d$year)

  has_reg <- "region" %in% names(d) &&
    length(unique(d$region[!is.na(d$region)])) > 1
  has_base <- length(unique(d$base)) > 1
  grp <- if (has_reg && has_base) {
    d$.series <- paste(d$base, d$region, sep = " / "); ".series"
  } else if (has_base) "base" else if (has_reg) "region" else NULL

  # linear interpolation between the given control years (what the model uses)
  mkline <- function(s) {
    xs <- if (!is.null(year)) sort(unique(as.integer(year)))
          else seq(min(s$year), max(s$year))
    if (length(unique(s$year)) < 2) data.frame(year = xs, .val = s$.val[1])
    else data.frame(year = xs, .val = stats::approx(s$year, s$.val, xout = xs, rule = 2)$y)
  }
  line_df <- if (!isTRUE(interpolate)) NULL else if (is.null(grp)) mkline(d) else
    do.call(rbind, lapply(split(d, d[[grp]]),
                          function(s) { L <- mkline(s); L[[grp]] <- s[[grp]][1]; L }))

  if (cls == "constraint") {
    sub_txt <- c("<=" = "upper bound (≤)", ">=" = "lower bound (≥)",
                 "==" = "fixed (=)")[as.character(object@eq)]
    if (is.na(sub_txt)) sub_txt <- NULL
  } else {
    cc <- tryCatch(object@comm, error = function(e) character())
    cc <- cc[nzchar(cc)]
    pp <- tryCatch(.levy_slot(object, "process"), error = function(e) character())
    pp <- pp[nzchar(pp)]
    sub_txt <- paste(c(
      if (length(cc)) paste0("on ", paste(cc, collapse = ", ")),
      if (length(pp)) paste0("process ", paste(pp, collapse = ", "))),
      collapse = "; ")
    if (!nzchar(sub_txt)) sub_txt <- NULL
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
                  title = if (nzchar(nm)) nm else NULL,
                  subtitle = .desc_of(object), caption = sub_txt) +
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
# is read from a `calendar` object's timetable (works for any calendar), or from
# the calendar a name such as "d365_h24" resolves to, via timescales::tsl2yday()
# and friends. The timeslice names themselves are never parsed.

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
    # Faceting by month over a calendar that has no MONTH column: the calendar
    # derives it. Without this the request was silently ignored and every
    # timeslice landed in one panel.
    if (identical(facet, "month") && !("month" %in% names(df))) {
      cal <- .ts_calendar(calendar)
      mon <- if (is.null(cal)) NULL else tryCatch(
        timescales::tsl2month(df$timeslice, cal), error = function(e) NULL)
      if (!is.null(mon) && !all(is.na(mon))) {
        df$month <- mon
        levels_ord <- c("month", setdiff(levels_ord, "month"))
      }
    }
  } else {
    cal <- .ts_calendar(calendar)
    if (is.null(cal)) {
      stop("`calendar` is required: the timeslice layout is read from the ",
           "calendar, not guessed from the timeslice names. Pass a `calendar` ",
           "object, or the name of one such as \"d365_h24\".", call. = FALSE)
    }
    tfs <- timescales::calendar_timeframes(cal)
    if ("YDAY"  %in% tfs) df$yday  <- timescales::tsl2yday(df$timeslice, cal)
    if ("HOUR"  %in% tfs) df$hour  <- timescales::tsl2hour(df$timeslice, cal)
    if ("MONTH" %in% tfs) df$month <- timescales::tsl2month(df$timeslice, cal)
    # Facet by month over a day-of-year format -> split yday into month + mday.
    # MONTH is not a timeframe of such a calendar; the calendar derives it.
    if (identical(facet, "month") && !("month" %in% names(df)) && "yday" %in% names(df)) {
      df$month <- timescales::tsl2month(df$timeslice, cal)
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
#'   timeslice), or the name of one (e.g. `"d365_h24"`). Required: the layout is
#'   read from the calendar, never guessed from the timeslice names.
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
#' gg_heatmap(prof, calendar = cal, value = "load")
#' gg_heatmap(prof, calendar = cal, value = "load", facet = "month")
#' }
gg_heatmap <- function(object, calendar = NULL, value = NULL, facet = NULL,
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
#' object's own unit (or `"capacity factor"` if unset).
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
#'   real datetime axis via [timescales::tsl2dtm()], which needs both a
#'   `calendar` and a `year` column; without them the categorical axis is kept
#'   (with a warning).
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
#' W <- getObject(topia$modules$electricity$repo, name = "WSOL", drop = TRUE)
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
    .say("No weather data to plot for '", nm, "'.")
    return(invisible(NULL))
  }
  d <- d[!is.na(d$wval), , drop = FALSE]

  d <- .filter_region(d, region, nm, "weather data")
  if (nrow(d) == 0) return(invisible(NULL))

  u        <- getUnit(object)
  unit_lab <- if (length(u) == 1 && !is.na(u) && nzchar(u)) u else "NA"

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
# draws the value in `wval`. Both the heatmap fill and the line/area colour
# use the `palette` option; the coarse level is mapped continuously when it is
# numeric (e.g. day of year), so long calendars read as a gradient.
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
        .say("Could not resolve a sub-annual layout from timeslice names; ",
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

  # Optional real datetime axis for line/area. Timeslice labels carry no year,
  # so this needs the data's `year` column as well as the calendar.
  if (isTRUE(datetime) && style != "heatmap") {
    cal <- .ts_calendar(calendar)
    yr  <- if ("year" %in% names(d)) suppressWarnings(as.integer(d$year)) else NULL
    dt <- if (is.null(cal) || is.null(yr) || anyNA(yr)) NULL else tryCatch(
      timescales::tsl2dtm(as.character(d$timeslice), cal, year = yr),
      error = function(e) NULL)
    if (is.null(dt) || all(is.na(dt))) {
      warning("could not place these timeslices on a datetime axis (it needs ",
              "a calendar and a year); keeping the categorical timeslice axis.",
              call. = FALSE)
    } else {
      d$.dtm <- dt
      fine <- ".dtm"; coarse <- NULL      # one continuous series per panel
    }
  }

  facets <- c(if (reg_multi && !region_axis) "region", if (yr_multi) "year", cfac)
  if (length(facets) > 0) d <- .facet_cap(d, facets)

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
      # `position = "identity"` overlays the groups, so the areas genuinely
      # cover one another and the transparency is load-bearing.
      (if (style == "line") ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE)
       else ggplot2::geom_area(position = "identity", alpha = 0.35,
                               na.rm = TRUE)) +
      ggplot2::labs(x = nice(fine), y = unit_lab,
                    colour = nice(coarse), fill = nice(coarse),
                    title = ttl, subtitle = sub) +
      theme_energyRt()
    if (style == "line") p <- p + ggplot2::guides(fill = "none")
    if (!is.null(coarse)) {
      num <- is.numeric(d[[coarse]])
      nm_c <- nice(coarse)
      if (num) {
        p <- p + ggplot2::scale_colour_viridis_c(option = palette, end = 0.9,
                                                 name = nm_c)
        if (style == "area")
          p <- p + ggplot2::scale_fill_viridis_c(option = palette, end = 0.9,
                                                 name = nm_c)
      } else {
        lv <- levels(d[[coarse]]) %||% sort(unique(as.character(d[[coarse]])))
        p <- p + .key_scale(lv, palette, nm_c, aes = "colour")
        if (style == "area") p <- p + .key_scale(lv, palette, nm_c)
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
  .facet_caption(p, d)
}

#' @rdname plot_weather
#' @exportS3Method ggplot2::autoplot
autoplot.weather <- function(object, style = c("heatmap", "line", "area"),
                             calendar = NULL, region = NULL, ...) {
  plot_weather(object, style = style, calendar = calendar, region = region,
               ...)
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
  if (!is.null(facet) && facet %in% names(d)) d <- .facet_cap(d, facet)
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
  .facet_caption(p, d)
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
  u <- tryCatch(getUnit(members[[1]]), error = function(e) NA_character_)
  unit_lab <- if (length(u) == 1 && !is.na(u) && nzchar(u)) u else
    "capacity factor"
  .rows_heatmap(dd, "fct", unit_lab, title = title, subtitle = sub,
                calendar = calendar, facet = "region")
}


# Trade route map --------------------------------------------------------------
# A trade object stores inter-regional routes (src -> dst) but no geometry, so
# the caller supplies a `map` (an sf object carrying `region` + polygon
# geometry, or a Geoscale, e.g. one of `topia$geoscales`). Routes are drawn as
# arrows between region centroids over the region polygons.

# Normalise `object` to a list of trade objects (single trade, list, or a
# repository/model/scenario container).
.as_trade_list <- function(x) {
  if (methods::is(x, "trade")) return(list(x))
  if (methods::is(x, "repository") || methods::is(x, "model") ||
      methods::is(x, "scenario")) return(getObject(x, class = "trade"))
  if (is.list(x)) return(Filter(function(o) methods::is(o, "trade"), x))
  stop("gg_trade_map: `object` must be a `trade`, a list of trades, or a ",
       "repository/model/scenario.", call. = FALSE)
}

# Who the plot is OF: the container's own name and description, which is the
# only thing a plot can honestly claim. A repository says `topia_transmission`,
# a single object says its own name, and a bare list of objects says nothing --
# there is no container to name, so inventing a title would be inventing a
# subject. What the picture measures belongs on the axes and legends, not here.
#' @noRd
.trade_plot_title <- function(object) {
  if (!isS4(object)) return(list(title = NULL, subtitle = NULL))
  nm <- tryCatch(as.character(object@name)[1], error = function(e) NA_character_)
  ds <- tryCatch(as.character(object@desc)[1], error = function(e) NA_character_)
  ok <- function(v) !is.null(v) && length(v) && !is.na(v) && nzchar(v)
  list(title = if (ok(nm)) nm else NULL,
       subtitle = if (ok(ds)) ds else NULL)
}

# Keep only the trades carrying `comm`. A repository mixes commodities --
# `ELC` region to region, `COA`/`GAS` zone to zone -- and they share neither a
# map level nor a capacity unit, so drawing one at a time is the usual case.
#' @noRd
.filter_trade_comm <- function(trades, comm) {
  if (is.null(comm) || !length(trades)) return(trades)
  comm <- as.character(comm)
  have <- vapply(trades, function(t)
    paste(as.character(t@commodity), collapse = ","), character(1))
  keep <- vapply(strsplit(have, ","), function(v) any(v %in% comm), logical(1))
  if (!any(keep)) {
    warning("no trade carries commodity ", paste(comm, collapse = ", "),
            "; the object has: ", paste(sort(unique(have)), collapse = ", "),
            call. = FALSE)
  }
  trades[keep]
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
#' the `topia$geoscales` layouts (`squares`, `honeycomb`, `island`,
#' `continent`).
#'
#' @param object A `trade`, a list of `trade` objects, or a `repository`,
#'   `model` or `scenario` (whose trade objects are all drawn).
#' @param map An `sf`/data.frame with `region`, `x`, `y` and polygon `geometry`
#'   or a `geoscales::GeoScale` (e.g. `topia$geoscales$honeycomb`). When `NULL`, the
#'   geoscale attached to `object` is used. Region polygons need `sf`; without
#'   it, only centroids and routes are drawn.
#' @param labels Logical; label region centroids with their names (default `TRUE`).
#' @param route_color Colour of the route arrows.
#' @param comm draw only the trades carrying this commodity (or these). `NULL`
#'   (default) draws every one. A repository usually mixes commodities that
#'   share neither a geoscale level nor a capacity unit, so one at a time is
#'   the readable case.
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
#' with no CRS — including the reference TOPIA layouts — take the plain
#' cartesian path, which is correct for them.
#'
#' @export
#' @examples
#' \dontrun{
#' TRD <- newTrade("TRD_ELC", commodity = "ELC",
#'   routes = data.frame(src = c("W1", "W2", "C1"), dst = c("W2", "C5", "C5")))
#' autoplot(TRD, map = topia$geoscales$honeycomb)
#'
#' # a geoscale can be drawn at any level
#' gg_trade_map(TRD, map = topia$geoscales$honeycomb, level = "zone")
#' }
gg_trade_map <- function(object, map = NULL, labels = TRUE,
                           route_color = "steelblue", level = NULL,
                           comm = NULL, ...) {
  check_package("ggplot2")
  # A geoscale attached to the scenario/model is used when no map is given, so
  # `autoplot(scen)` and `gg_trade_map(repo)` work without one being passed.
  if (is.null(map)) map <- tryCatch(getGeoscale(object), error = function(e) NULL)
  if (is.null(map)) {
    stop("gg_trade_map: pass a `map` (an sf object with `region`/`x`/`y` and ",
         "polygon geometry, or a `geoscales::GeoScale`), e.g. ",
         "`topia$geoscales$honeycomb`.", call. = FALSE)
  }
  # Keep the geoscale: when a coarser level is drawn, the route endpoints have
  # to be lifted to it too, or nothing will match the map.
  route_gs <- if (is_geoscale(map)) map else NULL
  # `level = NULL` draws EVERY level: the polygons come from the finest one,
  # the route endpoints from whichever level each names. Forcing one level
  # silently discarded the routes declared at another -- the fuel corridors
  # were simply invisible.
  all_levels <- is.null(level) && !is.null(route_gs)
  map <- .trade_map_normalise(map, level = level)
  ce <- as.data.frame(map)
  if (!all(c("region", "x", "y") %in% names(ce))) {
    stop("`map` must have `region`, `x` and `y` (centroid) columns.", call. = FALSE)
  }
  centers <- ce[, c("region", "x", "y")]
  centers$region <- as.character(centers$region)
  centers$level <- if (is.null(route_gs)) NA_character_ else
    .geo_default_level(route_gs) %||% NA_character_
  if (all_levels) {
    ca <- .trade_centers_all(route_gs)
    if (!is.null(ca)) centers <- ca
  }

  trades <- .filter_trade_comm(.as_trade_list(object), comm)
  if (length(trades) == 0) {
    .say("No trade objects to plot."); return(invisible(NULL))
  }
  routes <- do.call(rbind, lapply(trades, function(t) {
    r <- as.data.frame(t@routes)
    if (is.null(r) || nrow(r) == 0 || !all(c("src", "dst") %in% names(r))) return(NULL)
    data.frame(src = as.character(r$src), dst = as.character(r$dst),
               stringsAsFactors = FALSE)
  }))
  if (is.null(routes) || nrow(routes) == 0) {
    .say("No routes to plot."); return(invisible(NULL))
  }
  routes <- .routes_to_level(routes, route_gs, level)
  if (nrow(routes) == 0) {
    .say("No routes cross a boundary at this level."); return(invisible(NULL))
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
                       xsrc = "x", ysrc = "y", level = "level")
  seg <- dplyr::rename(dplyr::left_join(seg, centers[, c("region", "x", "y")],
                                        by = c("dst" = "region")),
                       xdst = "x", ydst = "y")
  miss <- is.na(seg$xsrc) | is.na(seg$xdst)
  if (any(miss)) {
    warning("Dropping ", sum(miss), " route(s) whose regions are not in `map`.",
            call. = FALSE)
    seg <- seg[!miss, , drop = FALSE]
  }
  if (nrow(seg) == 0) {
    .say("No routes overlap the map."); return(invisible(NULL))
  }
  # Nodes belong to the levels that were actually drawn. The centre table
  # holds every level so a route can be placed wherever its endpoints live,
  # but labelling all of them puts `WEST` and `TOPIA` on a map of electricity
  # corridors that has no zone or nation route on it, and stacks a zone
  # centroid on top of the region it covers.
  if ("level" %in% names(centers)) {
    drawn <- unique(stats::na.omit(seg$level))
    if (length(drawn)) {
      centers <- centers[centers$level %in% drawn, , drop = FALSE]
    }
  }

  have_sf <- requireNamespace("sf", quietly = TRUE) && methods::is(map, "sf")
  comm <- unique(unlist(lapply(trades, function(t)
    tryCatch(t@commodity, error = function(e) character()))))
  comm <- comm[nzchar(comm)]
  .t <- .trade_plot_title(object)
  ttl <- .t$title
  sub <- .t$subtitle
  cap <- paste0(if (length(comm)) paste0(paste(comm, collapse = ", "), " · "),
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
  # keep the plain cartesian layers when there is no CRS (the reference TOPIA
  # layouts have none) or no sf.
  crs <- if (have_sf) sf::st_crs(map) else NA
  as_sf_layers <- have_sf && !is.na(crs)
  # Routes from more than one level are different things -- a region
  # interconnector and a zone pipeline -- so they are coloured apart rather
  # than drawn as one network. A single level keeps `route_color`.
  lvls <- unique(stats::na.omit(seg$level))
  by_level <- length(lvls) > 1L
  col_aes <- if (by_level) ggplot2::aes(colour = .data[["level"]]) else NULL
  col_arg <- if (by_level) list() else list(colour = route_color)
  seg_layer <- function(d, head) {
    if (as_sf_layers) {
      do.call(ggplot2::geom_sf, c(list(data = .routes_as_sf(d, crs),
        mapping = col_aes, linewidth = 1.1, arrow = arr(head)), col_arg))
    } else {
      m <- if (is.null(col_aes)) seg_aes else utils::modifyList(seg_aes, col_aes)
      do.call(ggplot2::geom_segment, c(list(data = d, mapping = m,
        inherit.aes = FALSE, linewidth = 1.1, arrow = arr(head),
        lineend = "round", linejoin = "mitre"), col_arg))
    }
  }

  p <- ggplot2::ggplot()
  if (have_sf) {
    p <- p + ggplot2::geom_sf(data = map, fill = "grey92", colour = "white",
                              linewidth = 0.4)
  } else {
    .say("Package 'sf' not available: drawing centroids and routes only, ",
            "without region shapes.")
  }
  if (nrow(un) > 0) p <- p + seg_layer(un, "last")
  if (nrow(bi) > 0) p <- p + seg_layer(bi, "both")
  if (as_sf_layers) {
    pts <- sf::st_as_sf(centers, coords = c("x", "y"), crs = crs,
                        remove = FALSE)
    p <- p + ggplot2::geom_sf(data = pts, colour = "grey30", size = 1.6)
    if (isTRUE(labels)) {
      p <- p + ggplot2::geom_sf_text(data = pts,
        ggplot2::aes(label = .data[["region"]]),
        vjust = -0.8, size = 3, colour = "grey20")
    }
  } else {
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
  if (by_level) {
    p <- p + ggplot2::scale_colour_viridis_d(option = "D", end = 0.8,
                                             name = "geoframe")
  }
  p + ggplot2::labs(title = ttl, subtitle = sub, caption = cap) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   legend.position = "right")
}

#' @rdname gg_trade_map
#' @exportS3Method ggplot2::autoplot
autoplot.trade <- function(object, map = NULL, ...) {
  gg_trade_map(object, map = map, ...)
}

# Normalise the `map` argument of gg_trade_map() to an sf/data.frame carrying
# `region`, `x`, `y`. Accepts a `geoscales::GeoScale` (dissolved to `level` and
# given centroids), or anything already in that shape.
#' @noRd
.trade_map_normalise <- function(map, level = NULL) {
  if (!is_geoscale(map)) {
    # A plain `sf` needs `x`/`y` label anchors. TOPIA used to ship them as
    # columns; since 0.91 the layouts live inside `topia$geoscales`, so derive
    # them from the polygons when they are absent -- same rule as the geoscale
    # path below.
    if (inherits(map, "sf") && !all(c("x", "y") %in% names(map))) {
      check_package("sf")
      xy <- sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(
        sf::st_geometry(map))))
      map$x <- xy[, 1]
      map$y <- xy[, 2]
    }
    return(map)
  }
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
# Centroids for EVERY level of a geoscale, labelled with the level they came
# from. A trade's level is a property of its endpoints -- `ELC` names regions,
# `COA` names zones -- so one table lets each route be drawn where it actually
# is, instead of forcing them all onto one level and dropping whatever does
# not fit.
#' @noRd
.trade_centers_all <- function(gs) {
  frames <- tryCatch(as.character(geoscales::geoscale_geoframes(gs)),
                     error = function(e) character())
  out <- lapply(frames, function(f) {
    m <- tryCatch(.trade_map_normalise(gs, level = f), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    d <- as.data.frame(m)
    if (!all(c("region", "x", "y") %in% names(d))) return(NULL)
    data.frame(region = as.character(d$region), x = d$x, y = d$y,
               level = f, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, Filter(Negate(is.null), out))
  if (is.null(out)) return(NULL)
  out[!duplicated(out$region), , drop = FALSE]
}

.routes_to_level <- function(routes, gs, level) {
  if (is.null(gs) || is.null(level)) return(routes)
  finest <- .geo_default_level(gs)
  if (identical(level, finest)) return(routes)

  map <- .geo_level_map(gs, from = finest, to = level)
  # An endpoint may already BE at the target level -- `COA` and `GAS` move
  # zone to zone, because that is where they are balanced. The lookup is keyed
  # by finest-level names, so such an endpoint is not in it; lifting it would
  # yield NA and the route would be dropped as if it did not exist. Keep it.
  at_level <- tryCatch(
    as.character(geoscales::geoscale_regions(gs, geoframe = level)),
    error = function(e) character())
  lift <- function(v) {
    out <- unname(map[v])
    keep <- v %in% at_level
    out[keep] <- v[keep]
    out
  }
  routes$src <- lift(routes$src)
  routes$dst <- lift(routes$dst)
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
# The figure is PRINTED and returned invisibly, the base-graphics contract:
# `plot()` draws, `autoplot()` returns the object to modify.
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

# One implementation for every plot() method: draw, hand the figure back
# invisibly.
#' @noRd
.plot_via_autoplot <- function(x, ...) {
  check_package("ggplot2")
  p <- ggplot2::autoplot(x, ...)
  print(p)
  invisible(p)
}

# One implementation for every plot() method: draw, hand the figure back
# invisibly.
#' @noRd
.plot_via_autoplot <- function(x, ...) {
  check_package("ggplot2")
  p <- ggplot2::autoplot(x, ...)
  print(p)
  invisible(p)
}

setMethod("plot", c("calendar", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("commodity", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("constraint", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("demand", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("export", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("horizon", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("import", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("model", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("repository", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("scenario", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("storage", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("subsidy", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("supply", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("tax", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("technology", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("trade", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
})
setMethod("plot", c("weather", "ANY"), function(x, y, ...) {
  .plot_via_autoplot(x, ...)
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
# Cap the facet panels of a data frame: keep the first `max` distinct
# combinations of `vars` (order of first appearance) and record the counts
# as attributes for .facet_caption().
.facet_cap <- function(d, vars, max = 16L) {
  vars <- intersect(vars, names(d))
  if (length(vars) == 0 || nrow(d) == 0) return(d)
  key <- do.call(paste, c(lapply(vars, function(v) as.character(d[[v]])),
                          sep = "\r"))
  keys <- unique(key)
  n_total <- length(keys)
  if (n_total > max) d <- d[key %in% keys[seq_len(max)], , drop = FALSE]
  attr(d, "n_panels") <- min(n_total, max)
  attr(d, "n_total") <- n_total
  d
}

# Append the "showing k of N panels" note to a plot's caption when
# .facet_cap() dropped panels.
.facet_caption <- function(p, d) {
  n <- attr(d, "n_panels"); N <- attr(d, "n_total")
  if (is.null(n) || is.null(N) || N <= n) return(p)
  note <- paste0("showing ", n, " of ", N, " panels")
  old <- tryCatch(p$labels$caption, error = function(e) NULL)
  p + ggplot2::labs(caption = if (is.null(old) || !nzchar(old)) note else
    paste(old, note, sep = " · "))
}

.mix_other_last <- function(x, other = c("Other", "Other processes")) {
  u <- unique(as.character(x))
  tail_ <- intersect(other, u)
  factor(as.character(x), levels = c(setdiff(sort(u), tail_), tail_))
}
