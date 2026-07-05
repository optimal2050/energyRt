# plot_scenario.R -- mix extraction from solved scenarios + scenario/model
# autoplot methods: generation / capacity / fuel mixes and process
# investment-window charts.

# ── getMix ─────────────────────────────────────────────────────────────────────

#' Extract a tidy mix (generation, capacity, fuel) from a solved scenario
#'
#' @description
#' Collects the building blocks of a "mix" chart from a solved scenario into one
#' tidy data.frame: technology output, storage charge/discharge, inter-regional
#' and rest-of-world trade, and demand. The same extractor feeds
#' `autoplot(scenario, ...)`, the scenario report, and any custom analysis
#' (including sonification -- see the useR! training course).
#'
#' @param scen a solved `scenario` object, or a **named list** of scenarios
#'   (results are row-bound with the list names in the `scenario` column).
#' @param type character: `"generation"` (default -- output of `comm` by process,
#'   with storage-in and exports negative), `"capacity"` (`vTechCap` +
#'   `vStorageCap`), `"new_capacity"` (`vTechNewCap` + `vStorageNewCap`), or
#'   `"fuel"` (technology fuel consumption `vTechInp` by input commodity).
#' @param comm character, the balanced commodity for `"generation"`
#'   (default `"ELC"`). Ignored for the other types.
#' @param region character vector or `NULL` (all regions). Inter-regional trade
#'   flows are only meaningful for a region subset and are skipped when
#'   `region = NULL` (they cancel out in an all-region sum).
#' @param year integer vector or `NULL` (all milestone years).
#' @param slice `NULL` for annual sums, or a regular expression selecting a
#'   slice sample (e.g. `"^SUM_"` for the summer day on the `utopia_s4h24`
#'   calendar). When the matched slices carry an hour tag (`"_h00"..."_h23"`),
#'   an integer `hour` column is added.
#' @param drop_small numeric in `[0, 1)`: drop processes whose total absolute
#'   value is below this share of the largest process (default `0`, keep all).
#'
#' @return A tidy data.frame with columns `scenario`, `type`, `process`, `flow`
#'   (`generation`, `storage-in/out`, `import/export`, `demand`, `fuel`,
#'   `capacity`, `new_capacity`), `comm`, `region`, `year`, `value`, and -- when
#'   `slice` is given -- `slice` (+ `hour` when parsable). Missing variables are
#'   skipped silently (e.g. a model without storage or trade).
#'
#' @examples
#' \dontrun{
#' gen <- getMix(scen, "generation")                      # annual, all regions
#' day <- getMix(scen, "generation", slice = "^SUM_")     # summer-day dispatch
#' cmp <- getMix(list(BASE = s1, CO2CAP = s2), "capacity")
#' }
#' @export
getMix <- function(scen,
                   type = c("generation", "capacity", "new_capacity", "fuel"),
                   comm = "ELC", region = NULL, year = NULL, slice = NULL,
                   drop_small = 0) {
  type <- match.arg(type)
  # named list of scenarios -> row-bind
  if (is.list(scen) && !isS4(scen)) {
    out <- lapply(seq_along(scen), function(i) {
      d <- getMix(scen[[i]], type = type, comm = comm, region = region,
                  year = year, slice = slice, drop_small = drop_small)
      if (!is.null(d) && nrow(d) > 0) {
        nm <- names(scen)[i]
        if (!is.null(nm) && nzchar(nm)) d$scenario <- nm
      }
      d
    })
    out <- do.call(rbind, Filter(Negate(is.null), out))
    rownames(out) <- NULL
    return(out)
  }
  stopifnot(inherits(scen, "scenario"))

  pieces <- switch(type,
    generation = list(
      list(var = "vTechOut",     flow = "generation",   sign = +1, comm = comm),
      list(var = "vStorageOut",  flow = "storage-out",  sign = +1, comm = comm),
      list(var = "vStorageInp",  flow = "storage-in",   sign = -1, comm = comm),
      list(var = "vImportRow",   flow = "import",       sign = +1, comm = comm),
      list(var = "vExportRow",   flow = "export",       sign = -1, comm = comm),
      list(var = "pDemand",      flow = "demand",       sign = +1, comm = comm)
    ),
    capacity = list(
      list(var = "vTechCap",       flow = "capacity", sign = +1, comm = NULL),
      list(var = "vStorageCap",    flow = "capacity", sign = +1, comm = NULL)
    ),
    new_capacity = list(
      list(var = "vTechNewCap",    flow = "new_capacity", sign = +1, comm = NULL),
      list(var = "vStorageNewCap", flow = "new_capacity", sign = +1, comm = NULL)
    ),
    fuel = list(
      list(var = "vTechInp", flow = "fuel", sign = +1, comm = NULL)
    )
  )

  rows <- list()
  for (p in pieces) {
    d <- .mix_fetch(scen, p$var, native = !is.null(slice))
    if (is.null(d) || nrow(d) == 0) next
    # commodity filter (balanced commodity of the mix)
    if (!is.null(p$comm) && "comm" %in% names(d))
      d <- d[d$comm %in% p$comm, , drop = FALSE]
    if (!is.null(region) && "region" %in% names(d))
      d <- d[d$region %in% region, , drop = FALSE]
    if (!is.null(year) && "year" %in% names(d))
      d <- d[d$year %in% year, , drop = FALSE]
    if (nrow(d) == 0) next

    # process id: first matching id column; demand & row-trade have none
    pcol <- intersect(c("tech", "stg", "storage", "sup", "trade", "imp", "exp"),
                      names(d))
    d$process <- if (length(pcol) > 0) as.character(d[[pcol[1]]]) else p$flow

    # slice selection / annual aggregation
    if (!is.null(slice) && "slice" %in% names(d)) {
      d <- d[grepl(slice, d$slice), , drop = FALSE]
      if (nrow(d) == 0) next
      by <- intersect(c("process", "comm", "region", "year", "slice"), names(d))
    } else {
      by <- intersect(c("process", "comm", "region", "year"), names(d))
    }
    agg <- stats::aggregate(d[["value"]], by = d[by], FUN = sum, na.rm = TRUE)
    names(agg)[ncol(agg)] <- "value"
    agg$value <- p$sign * agg$value
    agg$flow  <- p$flow
    if (!"comm" %in% names(agg)) agg$comm <- NA_character_
    rows[[length(rows) + 1L]] <- agg
  }

  # inter-regional trade: only when a region subset is requested
  if (type == "generation" && !is.null(region)) {
    tr <- .mix_fetch(scen, "vTradeIr", native = !is.null(slice))
    if (!is.null(tr) && nrow(tr) > 0 && all(c("src", "dst") %in% names(tr))) {
      if (!is.null(year)) tr <- tr[tr$year %in% year, , drop = FALSE]
      if (!is.null(slice) && "slice" %in% names(tr))
        tr <- tr[grepl(slice, tr$slice), , drop = FALSE]
      if ("comm" %in% names(tr)) tr <- tr[tr$comm %in% comm, , drop = FALSE]
      imp <- tr[tr$dst %in% region, , drop = FALSE]
      exp <- tr[tr$src %in% region, , drop = FALSE]
      for (side in list(list(d = imp, reg = "dst", flow = "import", sign = +1),
                        list(d = exp, reg = "src", flow = "export", sign = -1))) {
        d <- side$d
        if (nrow(d) == 0) next
        d$region  <- d[[side$reg]]
        d$process <- if ("trade" %in% names(d)) as.character(d$trade) else side$flow
        by  <- intersect(c("process", "comm", "region", "year",
                           if (!is.null(slice)) "slice"), names(d))
        agg <- stats::aggregate(d[["value"]], by = d[by], FUN = sum, na.rm = TRUE)
        names(agg)[ncol(agg)] <- "value"
        agg$value <- side$sign * agg$value
        agg$flow  <- side$flow
        if (!"comm" %in% names(agg)) agg$comm <- NA_character_
        rows[[length(rows) + 1L]] <- agg
      }
    }
  }

  if (length(rows) == 0) {
    return(data.frame(scenario = character(), type = character(),
                      process = character(), flow = character(),
                      comm = character(), region = character(),
                      year = integer(), value = numeric(),
                      stringsAsFactors = FALSE))
  }
  # align columns before binding
  cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(d) {
    for (cc in setdiff(cols, names(d))) d[[cc]] <- NA
    d[, cols, drop = FALSE]
  })
  out <- do.call(rbind, rows)
  out$scenario <- scen@name
  out$type     <- type

  # hour column for sliced output
  if (!is.null(slice) && "slice" %in% names(out)) {
    hr <- suppressWarnings(as.integer(sub(".*_h(\\d+)$", "\\1", out$slice)))
    if (any(is.finite(hr))) out$hour <- hr
  }

  # drop marginal processes (never the demand overlay)
  if (drop_small > 0) {
    tot <- tapply(abs(out$value), out$process, sum, na.rm = TRUE)
    keep <- names(tot)[tot >= drop_small * max(tot)]
    out <- out[out$process %in% keep | out$flow == "demand", , drop = FALSE]
  }

  front <- intersect(c("scenario", "type", "process", "flow", "comm", "region",
                       "year", "slice", "hour", "value"), names(out))
  out <- out[, c(front, setdiff(names(out), front)), drop = FALSE]
  rownames(out) <- NULL
  out
}

# Fetch one solved variable / parameter as a plain data.frame, or NULL.
# `native = TRUE` requests the finest (native) timeframe so slice-level values
# survive (getData's default aggregates to ANNUAL).
.mix_fetch <- function(scen, vname, native = FALSE) {
  d <- tryCatch(
    if (native) {
      getData(scen, name = vname, merge = TRUE, drop.zeros = FALSE,
              timeframe = "highest")
    } else {
      getData(scen, name = vname, merge = TRUE, drop.zeros = FALSE)
    },
    error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  as.data.frame(d)
}

# ── autoplot.scenario ──────────────────────────────────────────────────────────

#' Plot mixes from a solved scenario
#'
#' @description
#' `autoplot()` on a solved `scenario` draws the mixes extracted by [getMix()]:
#' annual stacked bars by milestone year, or -- when `slice` selects a sample
#' (e.g. one representative day) -- an hourly dispatch profile. Storage charging
#' and exports plot below zero; demand is overlaid as a line.
#'
#' @param object a solved `scenario` object.
#' @param type `"generation"` (default), `"capacity"`, `"new_capacity"`,
#'   `"fuel"`, or `"storage"` (the storage-in/out flows only).
#' @param comm,region,year,slice,drop_small passed to [getMix()]. For a dispatch
#'   profile (`slice` given) with `year = NULL`, the last milestone year is used.
#' @param ... ignored.
#'
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' autoplot(scen)                                    # annual generation mix
#' autoplot(scen, "generation", slice = "^SUM_")     # summer-day dispatch
#' autoplot(scen, "capacity")
#' }
#' @rdname autoplot.scenario
#' @exportS3Method ggplot2::autoplot
autoplot.scenario <- function(object,
    type = c("generation", "capacity", "new_capacity", "fuel", "storage"),
    comm = "ELC", region = NULL, year = NULL, slice = NULL,
    drop_small = 0, ...) {
  type <- match.arg(type)
  gtype <- if (type == "storage") "generation" else type

  if (!is.null(slice) && is.null(year)) {
    # dispatch profile: default to the last milestone year
    yrs <- tryCatch(sort(unique(.mix_fetch(object, "vTechOut")$year)),
                    error = function(e) NULL)
    if (length(yrs) > 0) year <- max(yrs)
  }
  mx <- getMix(object, type = gtype, comm = comm, region = region,
               year = year, slice = slice, drop_small = drop_small)
  if (type == "storage")
    mx <- mx[grepl("^storage", mx$flow), , drop = FALSE]
  if (nrow(mx) == 0) stop("No '", type, "' data found in scenario '",
                          object@name, "'.")

  dem  <- mx[mx$flow == "demand", , drop = FALSE]
  bars <- mx[mx$flow != "demand", , drop = FALSE]
  cap1 <- function(s) sub("^(.)", "\\U\\1", s, perl = TRUE)
  ttl  <- paste0(cap1(gsub("_", " ", type)), " mix",
                 if (gtype == "generation") paste0(" (", comm, ")"),
                 " -- ", object@name)
  n_reg <- length(unique(bars$region))

  if (!is.null(slice) && "hour" %in% names(bars)) {
    # hourly dispatch over the slice sample
    agg <- stats::aggregate(value ~ process + hour,
                            rbind(bars[c("process", "hour", "value")]), sum)
    p <- ggplot2::ggplot(agg, ggplot2::aes(hour, value, fill = process)) +
      ggplot2::geom_col(width = 1, alpha = 0.9) +
      ggplot2::labs(x = "hour", y = "value", fill = NULL,
                    title = ttl, subtitle = paste0("slices: ", slice,
                                                   "  year: ", year)) +
      ggplot2::theme_bw()
    if (nrow(dem) > 0 && "hour" %in% names(dem)) {
      dl <- stats::aggregate(value ~ hour, dem[c("hour", "value")], sum)
      p <- p + ggplot2::geom_line(data = dl,
        ggplot2::aes(hour, value), inherit.aes = FALSE, linewidth = 0.8)
    }
    return(p)
  }

  # annual stacked bars by milestone year
  by <- c("process", "year", if (n_reg > 1) "region")
  agg <- stats::aggregate(bars[["value"]], by = bars[by], FUN = sum)
  names(agg)[ncol(agg)] <- "value"
  p <- ggplot2::ggplot(agg,
      ggplot2::aes(factor(.data$year), .data$value, fill = .data$process)) +
    ggplot2::geom_col(alpha = 0.9) +
    ggplot2::labs(x = "year", y = "value", fill = NULL, title = ttl) +
    ggplot2::theme_bw()
  if (nrow(dem) > 0 && gtype == "generation") {
    dby <- c("year", if (n_reg > 1) "region")
    dl <- stats::aggregate(dem[["value"]], by = dem[dby], FUN = sum)
    names(dl)[ncol(dl)] <- "value"
    p <- p + ggplot2::geom_point(data = dl,
      ggplot2::aes(factor(.data$year), .data$value),
      inherit.aes = FALSE, shape = 95, size = 8)
  }
  if (n_reg > 1) p <- p + ggplot2::facet_wrap(~region)
  p
}

# ── investment / availability windows ─────────────────────────────────────────

# Collect per-process availability windows from a repository/model:
# build window  = [start, end]   (defaults: horizon range when slots are empty)
# operation til = end + olife
.proc_windows <- function(x, horizon = NULL) {
  hspan <- if (!is.null(horizon) && nrow(horizon@intervals) > 0)
    range(horizon@intervals$mid) else c(NA_integer_, NA_integer_)
  procs <- c(tryCatch(getObjects(x, "technology"), error = function(e) list()),
             tryCatch(getObjects(x, "storage"),    error = function(e) list()))
  rows <- list()
  for (p in procs) {
    gslot <- function(sl, col) {
      d <- tryCatch(methods::slot(p, sl), error = function(e) NULL)
      if (is.data.frame(d) && col %in% names(d) && nrow(d) > 0)
        d[, intersect(c("region", col), names(d)), drop = FALSE] else NULL
    }
    st <- gslot("start", "start"); en <- gslot("end", "end")
    ol <- gslot("olife", "olife")
    regs <- unique(c(if (!is.null(st)) as.character(st$region),
                     if (!is.null(en)) as.character(en$region),
                     if (!is.null(ol)) as.character(ol$region)))
    regs <- regs[!is.na(regs)]
    if (length(regs) == 0) regs <- "(all)"
    for (r in regs) {
      pick <- function(d, col) {
        if (is.null(d)) return(NA_real_)
        v <- if ("region" %in% names(d))
          d[[col]][is.na(d$region) | d$region == r] else d[[col]]
        v <- suppressWarnings(as.numeric(v))
        v <- v[is.finite(v)]
        if (length(v) > 0) v[1] else NA_real_
      }
      b0 <- pick(st, "start"); b1 <- pick(en, "end"); ll <- pick(ol, "olife")
      if (!is.finite(b0)) b0 <- hspan[1]
      if (!is.finite(b1)) b1 <- hspan[2]
      rows[[length(rows) + 1L]] <- data.frame(
        process = p@name, region = r,
        build_start = b0, build_end = b1,
        oper_end = if (is.finite(ll) && is.finite(b1)) b1 + ll else b1,
        stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows)
  if (!is.null(out)) rownames(out) <- NULL
  out
}

#' Plot process investment / availability windows
#'
#' @description
#' A Gantt-style chart of when each technology (and storage) can be **built**
#' (solid bar, from `@start` to `@end`; defaults to the horizon when the slots
#' are empty) and how long the last-built vintage can **operate** (translucent
#' tail, `end + olife`). Faceted by region when the windows differ regionally.
#'
#' `autoplot(model, type = "windows")` and
#' `autoplot(repository, type = "windows")` dispatch here.
#'
#' @param x a `model` or `repository` object.
#' @param region character vector to filter regions, or `NULL` (all).
#' @param horizon a `horizon` object used for defaults; taken from the model's
#'   config automatically (optional for a repository).
#'
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' autoplot(mod, type = "windows")
#' plot_process_windows(repo, horizon = newHorizon(2020:2050))
#' }
#' @export
plot_process_windows <- function(x, region = NULL, horizon = NULL) {
  if (inherits(x, "model") && is.null(horizon)) {
    horizon <- tryCatch(x@config@horizon, error = function(e) NULL)
  }
  w <- .proc_windows(x, horizon = horizon)
  if (is.null(w) || nrow(w) == 0)
    stop("No technologies/storages with availability data found.")
  if (!is.null(region))
    w <- w[w$region %in% c(region, "(all)"), , drop = FALSE]
  # facet only when windows actually differ across regions
  vary <- length(unique(w$region)) > 1 &&
    nrow(unique(w[, c("process", "build_start", "build_end", "oper_end")])) <
    nrow(unique(w[, c("process", "region", "build_start", "build_end",
                      "oper_end")]))
  w$process <- stats::reorder(w$process, w$build_start)

  p <- ggplot2::ggplot(w) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$build_end, xend = .data$oper_end,
                   y = .data$process, yend = .data$process),
      linewidth = 4, alpha = 0.3, colour = "steelblue", na.rm = TRUE) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$build_start, xend = .data$build_end,
                   y = .data$process, yend = .data$process),
      linewidth = 4, colour = "steelblue", na.rm = TRUE) +
    ggplot2::labs(x = "year", y = NULL,
                  title = "Process availability windows",
                  subtitle = "solid: new capacity can be built · translucent: last vintage operates") +
    ggplot2::theme_bw()
  if (vary) p <- p + ggplot2::facet_wrap(~region)
  p
}

#' @param object a `model` / `repository` object (autoplot methods).
#' @param type only `"windows"` currently.
#' @param ... passed on to [plot_process_windows()].
#' @rdname plot_process_windows
#' @exportS3Method ggplot2::autoplot
autoplot.model <- function(object, type = c("windows"), ...) {
  type <- match.arg(type)
  plot_process_windows(object, ...)
}

#' @rdname plot_process_windows
#' @exportS3Method ggplot2::autoplot
autoplot.repository <- function(object, type = c("windows"), ...) {
  type <- match.arg(type)
  plot_process_windows(object, ...)
}
