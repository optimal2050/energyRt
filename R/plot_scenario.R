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
#'   `vStorageOutCap`), `"new_capacity"` (`vTechNewCap` + `vStorageOutNewCap`), or
#'   `"fuel"` (technology fuel consumption `vTechInp` by input commodity).
#' @param comm character, the balanced commodity for `"generation"`
#'   (default `"ELC"`). Ignored for the other types.
#' @param region character vector or `NULL` (all regions). Inter-regional trade
#'   flows are only meaningful for a region subset and are skipped when
#'   `region = NULL` (they cancel out in an all-region sum).
#' @param year integer vector or `NULL` (all milestone year).
#' @param timeslice `NULL` for annual sums, or a regular expression selecting a
#'   timeslice sample (e.g. `"^SUM_"` for the summer day on the `s4_h24`
#'   calendar). When the matched timeslices carry an hour tag (`"_h00"..."_h23"`),
#'   an integer `hour` column is added.
#' @param drop_small numeric in `[0, 1)`: drop processes whose total absolute
#'   value is below this share of the largest process (default `0`, keep all).
#'   Unlike `top_n` this DELETES rows -- stacked charts lose that mass.
#' @param top_n integer: keep the `top_n` largest processes (by summed
#'   absolute value) and lump the rest into one `"Other"` series.
#'   Mass-preserving -- lumped rows are re-aggregated, not dropped -- and the
#'   demand overlay is never lumped. `NULL` (default) or `Inf` keeps every
#'   process. Applied after `drop_small`. On a list of scenarios the lumping
#'   runs per scenario, so each keeps its own top set.
#' @param by character vector, any of `"vintage"` and `"cluster"`. Technology
#'   variants are always rolled back up to their base technology in `process`;
#'   by default (`NULL`) they are simply summed, so charts do not fragment into
#'   one series per variant. Naming a dimension here keeps it as an extra
#'   grouping column (with `"(none)"` for processes that have no variants), ready
#'   to drive a fill or facet.
#'
#' @return A tidy data.frame with columns `scenario`, `type`, `process`, `flow`
#'   (`generation`, `storage-in/out`, `import/export`, `demand`, `fuel`,
#'   `capacity`, `new_capacity`), `comm`, `region`, `year`, `value`, and -- when
#'   `timeslice` is given -- `timeslice` (+ `hour` when parsable), plus any column named
#'   in `by`. Missing variables are skipped silently (e.g. a model without
#'   storage or trade).
#'
#' @examples
#' \dontrun{
#' gen <- getMix(scen, "generation")                      # annual, all regions
#' day <- getMix(scen, "generation", timeslice = "^SUM_")     # summer-day dispatch
#' cmp <- getMix(list(BASE = s1, CO2CAP = s2), "capacity")
#' cl  <- getMix(scen, "capacity", by = "cluster")        # split by cluster
#' }
#' @export
getMix <- function(scen,
                   type = c("generation", "capacity", "new_capacity", "fuel"),
                   comm = "ELC", region = NULL, year = NULL, timeslice = NULL,
                   drop_small = 0, by = NULL, top_n = NULL) {
  type <- match.arg(type)
  by <- if (is.null(by)) character() else intersect(by, c("vintage", "cluster"))
  # named list of scenarios -> row-bind (lumping applied per scenario, so
  # each keeps its own top set)
  if (is.list(scen) && !isS4(scen)) {
    out <- lapply(seq_along(scen), function(i) {
      d <- getMix(scen[[i]], type = type, comm = comm, region = region,
                  year = year, timeslice = timeslice, drop_small = drop_small,
                  by = by, top_n = top_n)
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

  # `by` is shadowed by a local further down, so keep a copy under another name
  by_dims <- by
  # Variant provenance for every process class. By DEFAULT variants are rolled
  # back up to their base object, so existing charts do not fragment into one
  # series per vintage/cluster; pass `by = c("vintage", "cluster")` to split.
  tv <- tryCatch(as.data.frame(scen@modInp@sets$variant),
                 error = function(e) NULL)
  if (!is.null(tv) && NROW(tv) == 0) tv <- NULL
  # `.mix_fetch()` resolves the id column to `process`, so match on `name`
  if (!is.null(tv)) names(tv)[names(tv) == "name"] <- "tech"

  # Which series a chart is made of. The SIGN is not listed here: it is a
  # property of the (variable, chart) pair, not of the variable, so it is
  # derived from the variable's role by `.mix_sign()` below -- `vTechInp` is a
  # sink in a commodity balance but is plotted positive in a fuel-use chart.
  pieces <- switch(type,
    generation = list(
      list(var = "vTechOut",     flow = "generation",  comm = comm),
      list(var = "vStorageOut",  flow = "storage-out", comm = comm),
      list(var = "vStorageInp",  flow = "storage-in",  comm = comm),
      list(var = "vImportRow",   flow = "import",      comm = comm),
      list(var = "vExportRow",   flow = "export",      comm = comm),
      list(var = "pDemand",      flow = "demand",      comm = comm)
    ),
    capacity = list(
      list(var = "vTechCap",       flow = "capacity", comm = NULL),
      list(var = "vStorageOutCap",    flow = "capacity", comm = NULL)
    ),
    new_capacity = list(
      list(var = "vTechNewCap",    flow = "new_capacity", comm = NULL),
      list(var = "vStorageOutNewCap", flow = "new_capacity", comm = NULL)
    ),
    fuel = list(
      list(var = "vTechInp", flow = "fuel", comm = NULL)
    )
  )

  rows <- list()
  for (p in pieces) {
    d <- .mix_fetch(scen, p$var, native = !is.null(timeslice))
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

    # map variant technologies back to their base process, optionally keeping
    # the vintage / cluster as extra grouping columns
    if (!is.null(tv)) {
      m <- match(d$process, tv$tech)
      hit <- !is.na(m)
      if (length(by_dims) > 0) {
        for (dm in by_dims) {
          # "(none)" rather than NA: `stats::aggregate()` drops NA groups, which
          # would silently delete every non-variant process from the mix
          d[[dm]] <- "(none)"
          d[[dm]][hit] <- as.character(tv[[dm]][m[hit]])
          d[[dm]][is.na(d[[dm]])] <- "(none)"
        }
      }
      d$process[hit] <- as.character(tv$base[m[hit]])
    }

    # timeslice selection / annual aggregation
    if (!is.null(timeslice) && "timeslice" %in% names(d)) {
      d <- d[grepl(timeslice, d$timeslice), , drop = FALSE]
      if (nrow(d) == 0) next
      by <- intersect(c("process", "comm", "region", "year", "timeslice", by_dims),
                      names(d))
    } else {
      by <- intersect(c("process", "comm", "region", "year", by_dims), names(d))
    }
    agg <- stats::aggregate(d[["value"]], by = d[by], FUN = sum, na.rm = TRUE)
    names(agg)[ncol(agg)] <- "value"
    agg$value <- .mix_sign(p$var, type) * agg$value
    agg$flow  <- p$flow
    if (!"comm" %in% names(agg)) agg$comm <- NA_character_
    rows[[length(rows) + 1L]] <- agg
  }

  # inter-regional trade: only when a region subset is requested
  if (type == "generation" && !is.null(region)) {
    tr <- .mix_fetch(scen, "vTradeIr", native = !is.null(timeslice))
    if (!is.null(tr) && nrow(tr) > 0 && all(c("src", "dst") %in% names(tr))) {
      if (!is.null(year)) tr <- tr[tr$year %in% year, , drop = FALSE]
      if (!is.null(timeslice) && "timeslice" %in% names(tr))
        tr <- tr[grepl(timeslice, tr$timeslice), , drop = FALSE]
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
                           if (!is.null(timeslice)) "timeslice"), names(d))
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
  # pieces that carry no technology (trade, demand overlay) got NA on the
  # variant columns during column alignment
  for (dm in by_dims) {
    if (dm %in% names(out)) out[[dm]][is.na(out[[dm]])] <- "(none)"
  }

  # hour column for timesliced output
  if (!is.null(timeslice) && "timeslice" %in% names(out)) {
    hr <- suppressWarnings(as.integer(sub(".*_h(\\d+)$", "\\1", out$timeslice)))
    if (any(is.finite(hr))) out$hour <- hr
  }

  # drop marginal processes (never the demand overlay)
  if (drop_small > 0) {
    tot <- tapply(abs(out$value), out$process, sum, na.rm = TRUE)
    keep <- names(tot)[tot >= drop_small * max(tot)]
    out <- out[out$process %in% keep | out$flow == "demand", , drop = FALSE]
  }
  # lump everything below the top_n largest processes into "Other"
  # (mass-preserving, unlike drop_small; applied AFTER drop_small)
  out <- .mix_lump(out, top_n)

  front <- intersect(c("scenario", "type", "process", "flow", "comm", "region",
                       "year", "timeslice", "hour", "value"), names(out))
  out <- out[, c(front, setdiff(names(out), front)), drop = FALSE]
  rownames(out) <- NULL
  out
}

# Lump all but the top_n largest processes (by summed |value| over
# non-demand rows) into `other`. Mass-preserving: lumped rows are
# re-aggregated over every non-value column, not dropped -- stacked charts
# still sum to the balance. The demand overlay is never lumped. Identity
# when top_n is NULL / non-finite / >= the process count.
.mix_lump <- function(out, top_n = NULL, other = "Other") {
  if (is.null(out) || nrow(out) == 0 || is.null(top_n) ||
      !is.finite(top_n) || !"process" %in% names(out)) {
    return(out)
  }
  pool <- out$flow != "demand"
  procs <- unique(out$process[pool])
  procs <- procs[!is.na(procs)]
  if (length(procs) <= top_n) return(out)
  tot <- tapply(abs(out$value[pool]), out$process[pool], sum, na.rm = TRUE)
  keep <- names(sort(tot, decreasing = TRUE))[seq_len(top_n)]
  if (other %in% keep) other <- paste(other, "processes")
  lump <- pool & !out$process %in% keep
  if (!any(lump)) return(out)
  out$process[lump] <- other
  # collapse the lumped rows over all non-value columns. NOT via
  # stats::aggregate: it silently drops rows whose by-columns hold NA
  # (comm/timeslice are NA for some mix types). A composite key keeps NA
  # groups and the original column types.
  lump_rows <- out[lump, , drop = FALSE]
  grp_cols <- setdiff(names(lump_rows), "value")
  key <- do.call(paste, c(lapply(lump_rows[grp_cols], function(z)
    ifelse(is.na(z), "\r<NA>", as.character(z))), list(sep = "\r")))
  sums <- tapply(lump_rows$value, key, sum, na.rm = TRUE)
  first <- !duplicated(key)
  agg <- lump_rows[first, , drop = FALSE]
  agg$value <- unname(sums[key[first]])
  out <- rbind(out[!lump, , drop = FALSE], agg)
  rownames(out) <- NULL
  out
}

# Sign of a series in a chart. Only a commodity BALANCE has two directions, so
# only there does a `sink` plot negative; a fuel-use or capacity chart plots
# every series positive even though `vTechInp` is a sink. A parameter
# (`pDemand`) has no role and plots positive.
.mix_sign <- function(var, type) {
  if (!identical(type, "generation")) return(1)
  role <- tryCatch(.variables[[var]]$role, error = function(e) NULL)
  if (identical(role, "sink")) -1 else 1
}

# Fetch one solved variable / parameter as a plain data.frame, or NULL.
# `native = TRUE` keeps timeslice-level values; `FALSE` rolls them up to ANNUAL,
# which is what the year-axis plots want.
#
# BOTH branches now pass `timeframe` explicitly. Neither may inherit the default:
# it was "lowest" up to v0.80 and is "highest" from v0.80 on, so an inherited
# value would silently flip these plots from annual totals to whichever timeslice
# happened to sort first.
.mix_fetch <- function(scen, vname, native = FALSE) {
  d <- tryCatch(
    getData(scen, name = vname, merge = TRUE, drop.zeros = FALSE,
            timeframe = if (native) "highest" else "lowest"),
    error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  as.data.frame(d)
}

# ── autoplot.scenario ──────────────────────────────────────────────────────────

#' Plot mixes from a solved scenario
#'
#' @description
#' `autoplot()` on a solved `scenario` draws the mixes extracted by [getMix()]:
#' annual stacked bars by milestone year, or -- when `timeslice` selects a sample
#' (e.g. one representative day) -- an hourly dispatch profile. Storage charging
#' and exports plot below zero; demand is overlaid as a line.
#'
#' @param object a solved `scenario` object.
#' @param type `"generation"` (default), `"capacity"`, `"new_capacity"`,
#'   `"fuel"`, or `"storage"` (the storage-in/out flows only).
#' @param comm,region,year,timeslice,drop_small passed to [getMix()]. For a dispatch
#'   profile (`timeslice` given) with `year = NULL`, the last milestone year is used.
#' @param by character vector passed to [getMix()], any of `"vintage"` and
#'   `"cluster"`, to keep those technology-variant dimensions as grouping
#'   columns. Implied automatically by `fill`/`facet`.
#' @param fill name of the column to colour the bars by (default `"process"`);
#'   `"vintage"` or `"cluster"` shows the composition of a variant group.
#' @param top_n integer, passed to [getMix()]: keep the `top_n` largest
#'   processes and lump the rest into `"Other"` (mass-preserving). Default
#'   `12` keeps legends readable on large models; `NULL`/`Inf` shows every
#'   process. Legends with more than 8 keys move to a compact bottom
#'   multi-column layout automatically.
#' @param facet name of the column to facet by. Defaults to `"region"` when the
#'   scenario has more than one region (the historical behaviour).
#' @param ... ignored.
#'
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' autoplot(scen)                                    # annual generation mix
#' autoplot(scen, "generation", timeslice = "^SUM_")     # summer-day dispatch
#' autoplot(scen, "capacity")
#' autoplot(scen, "capacity", fill = "cluster")      # capacity by cluster
#' autoplot(scen, "capacity", fill = "vintage", facet = "process")
#' }
#' @rdname autoplot.scenario
#' @exportS3Method ggplot2::autoplot
autoplot.scenario <- function(object,
    type = c("generation", "capacity", "new_capacity", "fuel", "storage"),
    comm = "ELC", region = NULL, year = NULL, timeslice = NULL,
    drop_small = 0, by = NULL, fill = NULL, facet = NULL, top_n = 12, ...) {
  # ggplot2 is in Suggests. Without this the failure is R's bare "there is no
  # package called 'ggplot2'" from the first `ggplot2::` call, and R CMD check
  # flags a Suggests package used unconditionally. `R/plot.R` guards this way.
  check_package("ggplot2")
  type <- match.arg(type)
  gtype <- if (type == "storage") "generation" else type
  by <- if (is.null(by)) character() else intersect(by, c("vintage", "cluster"))
  # `fill`/`facet` default to the historical behaviour: colour by process, facet
  # by region when there is more than one
  if (!is.null(fill))  by <- union(by, intersect(fill,  c("vintage", "cluster")))
  if (!is.null(facet)) by <- union(by, intersect(facet, c("vintage", "cluster")))
  fill_var  <- if (is.null(fill))  "process" else fill[1]
  facet_var <- facet

  if (!is.null(timeslice) && is.null(year)) {
    # dispatch profile: default to the last milestone year
    yrs <- tryCatch(sort(unique(.mix_fetch(object, "vTechOut")$year)),
                    error = function(e) NULL)
    if (length(yrs) > 0) year <- max(yrs)
  }
  mx <- getMix(object, type = gtype, comm = comm, region = region,
               year = year, timeslice = timeslice, drop_small = drop_small,
               by = by, top_n = top_n)
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

  if (!is.null(timeslice) && "hour" %in% names(bars)) {
    # hourly dispatch over the timeslice sample
    agg <- stats::aggregate(value ~ process + hour,
                            rbind(bars[c("process", "hour", "value")]), sum)
    agg$process <- .mix_other_last(agg$process)
    p <- ggplot2::ggplot(agg, ggplot2::aes(hour, value, fill = process)) +
      ggplot2::geom_col(width = 1, alpha = 0.9) +
      ggplot2::labs(x = "hour", y = "value", fill = NULL,
                    title = ttl, subtitle = paste0("timeslices: ", timeslice,
                                                   "  year: ", year)) +
      theme_energyRt()
    for (comp in .legend_compact(length(unique(agg$process)))) p <- p + comp
    if (nrow(dem) > 0 && "hour" %in% names(dem)) {
      dl <- stats::aggregate(value ~ hour, dem[c("hour", "value")], sum)
      p <- p + ggplot2::geom_line(data = dl,
        ggplot2::aes(hour, value), inherit.aes = FALSE, linewidth = 0.8)
    }
    return(p)
  }

  # annual stacked bars by milestone year
  auto_facet <- is.null(facet_var) && n_reg > 1
  if (is.null(facet_var) && auto_facet) facet_var <- "region"
  bycols <- unique(c("process", "year", if (n_reg > 1) "region",
                     fill_var, facet_var))
  bycols <- intersect(bycols, names(bars))
  agg <- stats::aggregate(bars[["value"]], by = bars[bycols], FUN = sum)
  names(agg)[ncol(agg)] <- "value"
  if (identical(fill_var, "process")) {
    agg$process <- .mix_other_last(agg$process)
  }
  p <- ggplot2::ggplot(agg,
      ggplot2::aes(factor(.data$year), .data$value,
                   fill = .data[[fill_var]])) +
    ggplot2::geom_col(alpha = 0.9) +
    ggplot2::labs(x = "year", y = "value", fill = NULL, title = ttl) +
    theme_energyRt()
  for (comp in .legend_compact(length(unique(agg[[fill_var]])))) p <- p + comp
  if (nrow(dem) > 0 && gtype == "generation") {
    dby <- c("year", if (n_reg > 1) "region")
    dl <- stats::aggregate(dem[["value"]], by = dem[dby], FUN = sum)
    names(dl)[ncol(dl)] <- "value"
    p <- p + ggplot2::geom_point(data = dl,
      ggplot2::aes(factor(.data$year), .data$value),
      inherit.aes = FALSE, shape = 95, size = 8)
  }
  if (!is.null(facet_var) && facet_var %in% names(agg)) {
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", facet_var)))
  }
  p
}

# ── investment / availability windows ─────────────────────────────────────────

# Collect per-process availability windows from a repository/model:
# build window  = [start, end]   (defaults: horizon range when slots are empty)
# operation til = end + olife
# Years over which a process carries EXOGENOUS capacity. Stock is a per-year
# series (`pTechStock[t,r,y]`), not a lifetime, so its span is simply the years
# it is declared for -- and a process with stock but no investment window has
# no `@vintage` row at all, which is why reading `@vintage` alone lost it.
#' @noRd
.proc_stock_years <- function(p, region) {
  cap <- tryCatch(methods::slot(p, "capacity"), error = function(e) NULL)
  if (!is.data.frame(cap) || nrow(cap) == 0 || !"year" %in% names(cap)) {
    return(list(NA_real_, NA_real_))
  }
  cols <- intersect(c("stock", "out.stock", "inp.stock", "stg.stock"),
                    names(cap))
  if (length(cols) == 0) return(list(NA_real_, NA_real_))
  has <- Reduce(`|`, lapply(cols, function(cc) {
    v <- suppressWarnings(as.numeric(cap[[cc]])); !is.na(v) & v != 0
  }))
  if ("region" %in% names(cap) && !identical(region, "(all)")) {
    has <- has & (is.na(cap$region) | cap$region == region)
  }
  if (!any(has)) return(list(NA_real_, NA_real_))
  yr <- suppressWarnings(as.numeric(cap$year[has]))
  yr <- yr[is.finite(yr)]
  if (length(yr) == 0) return(list(NA_real_, NA_real_))
  list(min(yr), max(yr))
}

.proc_windows <- function(x, horizon = NULL) {
  hspan <- if (!is.null(horizon) && nrow(horizon@intervals) > 0)
    range(horizon@intervals$mid) else c(NA_integer_, NA_integer_)
  hend <- if (!is.null(horizon) && nrow(horizon@intervals) > 0)
    max(horizon@intervals$end) else NA_real_
  procs <- c(tryCatch(getObjects(x, "technology"), error = function(e) list()),
             tryCatch(getObjects(x, "storage"),    error = function(e) list()))
  rows <- list()
  for (p in procs) {
    # lifespan lives in `@vintage`; `.lifespan_resolve()` returns keyed,
    # conflict-checked rows (one per vintage/region/cluster key)
    gslot <- function(col) {
      d <- .lifespan_resolve(p, col)
      if (nrow(d) > 0) d else NULL
    }
    st <- gslot("start"); en <- gslot("end"); ol <- gslot("olife")
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
      # An `end` before the first milestone closes the investment window: the
      # process is exogenous over this horizon. Emitting b0 > b1 would draw the
      # bar backwards, which is how a stock plant came to look as though it
      # expired the year before the only year solved.
      if (is.finite(b0) && is.finite(b1) && b1 < b0) {
        b0 <- NA_real_; b1 <- NA_real_
      }
      sy <- .proc_stock_years(p, r)
      rows[[length(rows) + 1L]] <- data.frame(
        process = p@name, region = r,
        build_start = b0, build_end = b1,
        # No `olife` is an INFINITE life (`mTechOlifeInf`), not a zero one:
        # run the tail to the end of the horizon rather than collapsing it
        # onto the build year, which read as "operates for no time".
        oper_end = if (!is.finite(b1)) NA_real_
                   else if (is.finite(ll)) b1 + ll
                   else if (is.finite(hend)) hend else b1,
        stock_start = sy[[1L]], stock_end = sy[[2L]],
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
#' @param object a `model` or `repository` object.
#' @param region character vector to filter regions, or `NULL` (all).
#' @param horizon a `horizon` object used for defaults; taken from the model's
#'   config automatically (optional for a repository).
#'
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' autoplot(mod, type = "windows")
#' autoplot(repo, horizon = newHorizon(2020:2050))
#' }
#' @keywords internal
plot_process_windows <- function(object, region = NULL, horizon = NULL) {
  check_package("ggplot2")
  if (inherits(object, "model") && is.null(horizon)) {
    horizon <- tryCatch(object@config@horizon, error = function(e) NULL)
  }
  w <- .proc_windows(object, horizon = horizon)
  if (is.null(w) || nrow(w) == 0)
    stop("No technologies/storages with availability data found.")
  if (!is.null(region))
    w <- w[w$region %in% c(region, "(all)"), , drop = FALSE]
  # facet only when windows actually differ across regions
  wcols <- c("build_start", "build_end", "oper_end", "stock_start", "stock_end")
  vary <- length(unique(w$region)) > 1 &&
    nrow(unique(w[, c("process", wcols)])) <
    nrow(unique(w[, c("process", "region", wcols)]))
  # A process with no investment window still has to sort somewhere: order on
  # whichever of its bars starts first.
  w$.ord <- pmin(w$build_start, w$stock_start, na.rm = TRUE)
  w$process <- stats::reorder(w$process, w$.ord)
  # Stock declared for a single year has no width to draw as a segment.
  pt <- w[is.finite(w$stock_start) & w$stock_start == w$stock_end, ,
          drop = FALSE]

  p <- ggplot2::ggplot(w) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$stock_start, xend = .data$stock_end,
                   y = .data$process, yend = .data$process),
      linewidth = 4, colour = "grey35", na.rm = TRUE) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$build_end, xend = .data$oper_end,
                   y = .data$process, yend = .data$process),
      linewidth = 4, alpha = 0.3, colour = "steelblue", na.rm = TRUE) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$build_start, xend = .data$build_end,
                   y = .data$process, yend = .data$process),
      linewidth = 4, colour = "steelblue", na.rm = TRUE) +
    ggplot2::labs(x = "year", y = NULL,
                  title = "Process investment and operating windows",
                  subtitle = paste("grey: exogenous stock · solid: new",
                                   "capacity can be built · translucent:",
                                   "last vintage operates")) +
    theme_energyRt()
  if (nrow(pt) > 0) {
    p <- p + ggplot2::geom_point(
      data = pt,
      ggplot2::aes(x = .data$stock_start, y = .data$process),
      size = 3, colour = "grey35", na.rm = TRUE)
  }
  if (vary) p <- p + ggplot2::facet_wrap(~region)
  p
}

# Availability windows collapsed to one row per process-topology group
# (min build_start .. max build_end solid, tail to max oper_end), sized for
# large models where the per-process chart is an unreadable band. `groups`
# comes from .proc_groups() (R/report.R).
#' @noRd
.plot_windows_grouped <- function(w, groups) {
  if (is.null(w) || nrow(w) == 0 || length(groups) == 0) return(NULL)
  labs_ <- vapply(groups, function(g) g$index %||% g$label, character(1))
  labs_ <- paste0(labs_, " (", vapply(groups, function(g) g$n, integer(1)),
                  ")")
  map <- unlist(lapply(seq_along(groups), function(i)
    stats::setNames(rep(labs_[i], length(groups[[i]]$members)),
                    groups[[i]]$members)))
  w$grp <- unname(map[w$process])
  w <- w[!is.na(w$grp), , drop = FALSE]
  if (nrow(w) == 0) return(NULL)
  # An all-NA group must stay NA, not become Inf: min/max of nothing warns and
  # returns +/-Inf, which ggplot then tries to draw.
  rng <- function(v, f) {
    v <- v[is.finite(v)]
    if (length(v) == 0) NA_real_ else f(v)
  }
  agg <- do.call(rbind, lapply(split(w, w$grp), function(d) data.frame(
    grp = d$grp[1],
    build_start = rng(d$build_start, min),
    build_end   = rng(d$build_end, max),
    oper_end    = rng(d$oper_end, max),
    stock_start = rng(d$stock_start, min),
    stock_end   = rng(d$stock_end, max),
    stringsAsFactors = FALSE)))
  agg$.ord <- pmin(agg$build_start, agg$stock_start, na.rm = TRUE)
  agg$grp <- stats::reorder(agg$grp, agg$.ord)
  pt <- agg[is.finite(agg$stock_start) & agg$stock_start == agg$stock_end, ,
            drop = FALSE]
  p <- ggplot2::ggplot(agg) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$stock_start, xend = .data$stock_end,
                   y = .data$grp, yend = .data$grp),
      linewidth = 4, colour = "grey35", na.rm = TRUE) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$build_end, xend = .data$oper_end,
                   y = .data$grp, yend = .data$grp),
      linewidth = 4, alpha = 0.3, colour = "steelblue", na.rm = TRUE) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$build_start, xend = .data$build_end,
                   y = .data$grp, yend = .data$grp),
      linewidth = 4, colour = "steelblue", na.rm = TRUE) +
    ggplot2::labs(x = "year", y = NULL,
                  title = "Process investment and operating windows (by structure)",
                  subtitle = paste("grey: exogenous stock · solid: new",
                                   "capacity can be built · translucent:",
                                   "last vintage operates · (n) = processes ·",
                                   "Gk = group, see index table")) +
    theme_energyRt()
  if (nrow(pt) > 0) {
    p <- p + ggplot2::geom_point(
      data = pt, ggplot2::aes(x = .data$stock_start, y = .data$grp),
      size = 3, colour = "grey35", na.rm = TRUE)
  }
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
