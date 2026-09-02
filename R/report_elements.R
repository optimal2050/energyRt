# =========================================================================== #
# report_elements.R -- report() methods for the element classes: one shared
# method body dispatching to per-class param builders, one shipped template
# per class (inst/templates/report_<class>.Rmd). Containers and processes
# keep their own methods in R/report.R.
# =========================================================================== #
#' @include report.R
NULL

.REPORT_ELEMENT_CLASSES <- c(
  "commodity", "supply", "demand", "trade", "import", "export", "weather",
  "tax", "subsidy", "constraint", "calendar", "horizon", "config", "costs")

# The object's own default template (`misc$report`, written by techspec and
# settable by hand). tryCatch is the no-@misc guard (horizon).
#' @noRd
.report_misc_template <- function(object) {
  v <- tryCatch(object@misc$report, error = function(e) NULL)
  if (is.character(v) && length(v) == 1L && nzchar(v)) v else NULL
}

# draw() schematic to a temp png; NULL when draw fails or draws nothing.
# The same tempfile/png dance the process methods use inline.
#' @noRd
.report_draw_png <- function(object) {
  tryCatch({
    tf <- tempfile(pattern = "report_draw_", fileext = ".png")
    grDevices::png(tf, width = 900, height = 600, res = 150, bg = "white")
    draw(object)
    grDevices::dev.off()
    if (file.exists(tf) && file.info(tf)$size > 5000) tf else NULL
  }, error = function(e) {
    tryCatch(grDevices::dev.off(), error = function(e2) NULL)
    NULL
  })
}

# Collapse a slot to a deduped comma string ("" when empty/absent).
#' @noRd
.slot_chr_report <- function(x, sl) {
  v <- tryCatch(methods::slot(x, sl), error = function(e) NULL)
  v <- as.character(v)
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v) == 0) "" else paste(unique(v), collapse = ", ")
}

# One `setting | value` row (the .report_model idiom).
#' @noRd
.el_pair <- function(k, v) {
  data.frame(setting = k, value = as.character(v), stringsAsFactors = FALSE)
}

# Params for one element object. Common contract first, then the per-class
# extras merged over it; a class without a wave builder yet contributes
# nothing beyond the common set.
#' @noRd
.element_report_params <- function(object, declared = NULL,
                                   image_file = NULL, container = NULL,
                                   levcost = NULL) {
  cls <- class(object)[1]
  want <- function(p) is.null(declared) || p %in% declared
  gg_ok <- requireNamespace("ggplot2", quietly = TRUE)

  nm <- tryCatch(object@name, error = function(e) NULL)
  nm <- if (is.character(nm) && length(nm) == 1L && nzchar(nm)) nm else
    paste0("(unnamed ", cls, ")")
  desc <- tryCatch(object@desc, error = function(e) "")
  if (is.null(desc) || length(desc) == 0 || is.na(desc[1])) desc <- ""

  common <- list(
    title = paste0(toupper(substr(cls, 1, 1)), substr(cls, 2, nchar(cls)),
                   ": ", nm),
    name = nm, desc = desc[1], class = cls,
    image_file = image_file %||%
      tryCatch(.object_image_file(object), error = function(e) NULL),
    logos = tryCatch(.container_logo_files(object),
                     error = function(e) NULL),
    info_df = NULL, main_plot = NULL)

  if (cls %in% c("supply", "demand", "trade", "import", "export") &&
      want("draw_file")) {
    common$draw_file <- .report_draw_png(object)
  }
  if (gg_ok && want("main_plot")) {
    common$main_plot <- tryCatch(ggplot2::autoplot(object),
                                 error = function(e) NULL)
  }

  extras <- switch(cls,
    supply = .el_params_supply(object, want),
    demand = .el_params_demand(object, want),
    import = .el_params_flowio(object, want, "import"),
    export = .el_params_flowio(object, want, "export"),
    commodity = .el_params_commodity(object, want),
    weather = .el_params_weather(object, want, gg_ok),
    trade = .el_params_trade(object, want, container = container,
                             levcost = levcost),
    tax = .el_params_lever(object, want, "tax"),
    subsidy = .el_params_lever(object, want, "subsidy"),
    constraint = .el_params_constraint(object, want),
    calendar = .el_params_calendar(object, want),
    horizon = .el_params_horizon(object, want),
    config = .el_params_config(object, want, gg_ok),
    costs = .el_params_costs(object, want),
    list())

  out <- common
  out[names(extras)] <- extras
  out
}

# The one method body shared by every element class. `levcost`/`cost_unit`
# are accepted for signature symmetry with the process methods; only trade
# uses `levcost` (a section in its datasheet) -- silently ignored elsewhere.
#' @noRd
.report_element_method <- function(object, template = NULL,
    image_file = NULL, file = NULL,
    format = c("html", "pdf", "tex", "docx"),
    levcost = NULL, cost_unit = NULL, open = interactive(),
    reports_path = NULL, force = FALSE, ...) {
  if (missing(format)) format <- "html"
  cls <- class(object)[1]
  tmpl <- template %||% .report_misc_template(object) %||% cls
  dots <- list(...)
  container <- dots[[".container"]]
  dots[[".container"]] <- NULL

  # branding resolved eagerly: it enters the render key, so a changed logo
  # or image re-renders an otherwise up-to-date report
  logos_r <- tryCatch(.container_logo_files(object), error = function(e) NULL)
  img_r <- image_file %||%
    tryCatch(.object_image_file(object), error = function(e) NULL)

  params_fn <- function(declared = NULL) {
    .element_report_params(object, declared, image_file = img_r,
                           container = container, levcost = levcost)
  }
  nm <- tryCatch(object@name, error = function(e) NULL)
  stub <- if (is.character(nm) && length(nm) == 1L && nzchar(nm)) nm else cls
  .report_render(tmpl, params_fn, file, format, dots, stub, open = open,
                 class = cls, owner = object, reports_path = reports_path,
                 force = force, engine = paste0("report/", cls),
                 key_args = .branding_key(logos_r, img_r))
}

# ── per-class builders ──────────────────────────────────────────────────────

# data.frame slot, pruned; NULL when empty/absent
.el_slot_df <- function(object, sl) {
  d <- tryCatch(methods::slot(object, sl), error = function(e) NULL)
  if (!is.data.frame(d)) return(NULL)
  .report_drop_empty_cols(d)
}

.el_params_supply <- function(object, want) {
  info <- do.call(rbind, list(
    .el_pair("commodity", .slot_chr_report(object, "commodity")),
    .el_pair("unit", .slot_chr_report(object, "unit")),
    .el_pair("regions", .slot_chr_report(object, "region")),
    .el_pair("weather", paste(unique(as.character(
      tryCatch(object@weather$weather, error = function(e) character(0)))),
      collapse = ", ")),
    .el_pair("reserve", if (NROW(tryCatch(object@reserve,
      error = function(e) NULL)) > 0) "yes" else "no")))
  gg_ok <- requireNamespace("ggplot2", quietly = TRUE)
  list(info_df = .report_drop_empty_cols(info),
       supply_df = if (want("supply_df")) .el_slot_df(object, "supply"),
       reserve_df = if (want("reserve_df")) .el_slot_df(object, "reserve"),
       weather_df = if (want("weather_df")) .el_slot_df(object, "weather"),
       cluster_df = if (want("cluster_df")) .el_slot_df(object, "cluster"),
       bar_plot = if (gg_ok && want("bar_plot")) tryCatch(
         ggplot2::autoplot(object, style = "bar"),
         error = function(e) NULL),
       cost_plot = if (gg_ok && want("cost_plot")) tryCatch(
         ggplot2::autoplot(object, style = "bar", type = "cost"),
         error = function(e) NULL),
       regions_plot = if (gg_ok && want("regions_plot")) tryCatch(
         ggplot2::autoplot(object, style = "regions"),
         error = function(e) NULL))
}

.el_params_demand <- function(object, want) {
  tot <- tryCatch({
    v <- object@demand$demand
    v <- v[!is.na(v)]
    if (length(v) > 0) signif(sum(v), 4) else NA
  }, error = function(e) NA)
  info <- do.call(rbind, list(
    .el_pair("commodity", .slot_chr_report(object, "commodity")),
    .el_pair("unit", .slot_chr_report(object, "unit")),
    .el_pair("regions", .slot_chr_report(object, "region")),
    .el_pair("total demand", if (is.na(tot)) "" else tot)))
  list(info_df = .report_drop_empty_cols(info),
       demand_df = if (want("demand_df")) .el_slot_df(object, "demand"))
}

# import / export share one shape: a commodity flow priced in a data slot
# named after the class
.el_params_flowio <- function(object, want, slot_name) {
  d <- tryCatch(methods::slot(object, slot_name), error = function(e) NULL)
  price_rng <- tryCatch({
    pc <- intersect(c("price", "cost"), names(d))
    if (length(pc) > 0) {
      v <- suppressWarnings(as.numeric(d[[pc[1]]]))
      v <- v[is.finite(v)]
      if (length(v) > 0)
        paste(unique(signif(range(v), 4)), collapse = " - ") else ""
    } else ""
  }, error = function(e) "")
  info <- do.call(rbind, list(
    .el_pair("commodity", .slot_chr_report(object, "commodity")),
    .el_pair("unit", .slot_chr_report(object, "unit")),
    .el_pair("price/cost", price_rng)))
  list(info_df = .report_drop_empty_cols(info),
       flow_df = if (want("flow_df")) .el_slot_df(object, slot_name),
       reserve_df = if (want("reserve_df")) .el_slot_df(object, "reserve"))
}
.el_params_commodity <- function(object, want) {
  lim <- tryCatch(as.character(object@limtype)[1], error = function(e) "")
  lim_note <- switch(toupper(lim %||% ""),
    LO = paste("limtype LO: production may exceed consumption",
               "(free disposal)"),
    UP = "limtype UP: consumption may exceed production",
    FX = "limtype FX: strict balance, production equals consumption",
    NULL)
  info <- do.call(rbind, list(
    .el_pair("unit", .slot_chr_report(object, "unit")),
    .el_pair("timeframe", .slot_chr_report(object, "timeframe")),
    .el_pair("geoframe", .slot_chr_report(object, "geoframe")),
    .el_pair("limtype", lim),
    .el_pair("emission factors", NROW(tryCatch(object@emis,
      error = function(e) NULL))),
    .el_pair("aggregated members", NROW(tryCatch(object@agg,
      error = function(e) NULL)))))
  props <- tryCatch(.report_drop_empty_cols(
    as.data.frame(commodity_properties(object))), error = function(e) NULL)
  list(info_df = .report_drop_empty_cols(info),
       limtype_note = lim_note,
       props_df = if (want("props_df")) props,
       emis_df = if (want("emis_df")) .el_slot_df(object, "emis"),
       agg_df = if (want("agg_df")) .el_slot_df(object, "agg"))
}

.el_params_weather <- function(object, want, gg_ok) {
  w <- tryCatch(object@weather, error = function(e) NULL)
  vcol <- intersect(c("value", "wval"), names(w))
  rng <- if (length(vcol) > 0) {
    v <- suppressWarnings(as.numeric(w[[vcol[1]]]))
    v <- v[is.finite(v)]
    if (length(v) > 0) paste(signif(range(v), 4), collapse = " - ") else ""
  } else ""
  info <- do.call(rbind, list(
    .el_pair("unit", .slot_chr_report(object, "unit")),
    .el_pair("timeframe", .slot_chr_report(object, "timeframe")),
    .el_pair("regions", .slot_chr_report(object, "region")),
    .el_pair("default value", .slot_chr_report(object, "defVal")),
    .el_pair("observations", NROW(w)),
    .el_pair("value range", rng)))
  stats <- if (want("stats_df") && is.data.frame(w) && length(vcol) > 0 &&
               "region" %in% names(w)) tryCatch({
    v <- suppressWarnings(as.numeric(w[[vcol[1]]]))
    ag <- stats::aggregate(list(value = v), by = list(region = w$region),
      FUN = function(z) c(min = min(z, na.rm = TRUE),
                          mean = mean(z, na.rm = TRUE),
                          max = max(z, na.rm = TRUE)))
    data.frame(region = ag$region, min = signif(ag$value[, "min"], 4),
               mean = signif(ag$value[, "mean"], 4),
               max = signif(ag$value[, "max"], 4),
               stringsAsFactors = FALSE)
  }, error = function(e) NULL) else NULL
  main <- if (gg_ok && want("main_plot")) {
    tryCatch(ggplot2::autoplot(object, style = "heatmap"),
      error = function(e) tryCatch(ggplot2::autoplot(object, style = "line"),
                                   error = function(e2) NULL))
  } else NULL
  list(info_df = .report_drop_empty_cols(info),
       main_plot = main,
       stats_df = stats,
       weather_head_df = if (want("weather_head_df"))
         .report_drop_empty_cols(utils::head(as.data.frame(w), 200)))
}

.el_params_trade <- function(object, want, container = NULL,
                             levcost = NULL) {
  info <- do.call(rbind, list(
    .el_pair("commodity", .slot_chr_report(object, "commodity")),
    .el_pair("routes", NROW(tryCatch(object@routes,
      error = function(e) NULL))),
    .el_pair("cap2act", .slot_chr_report(object, "cap2act")),
    .el_pair("optimizeRetirement", as.character(isTRUE(tryCatch(
      object@optimizeRetirement, error = function(e) FALSE)))),
    .el_pair("aux commodities", paste(unique(as.character(tryCatch(
      object@aux$acomm, error = function(e) character(0)))),
      collapse = ", "))))
  map_plot <- NULL
  if (want("map_plot") && !is.null(container) &&
      requireNamespace("geoscales", quietly = TRUE) &&
      requireNamespace("sf", quietly = TRUE) &&
      requireNamespace("ggplot2", quietly = TRUE)) {
    map_plot <- tryCatch(plot_trade_map(object), error = function(e) NULL)
  }
  lc_df <- if (want("levcost_df") && !is.null(levcost)) tryCatch({
    inst <- if (inherits(levcost, "levcost")) levcost else
      .report_pick_instance(levcost)
    npv <- inst[["levcost_npv"]]
    if (!is.null(npv) && length(npv) > 0) {
      data.frame(item = "levelized cost of transport (NPV)",
                 value = signif(as.numeric(npv)[1], 5),
                 stringsAsFactors = FALSE)
    } else NULL
  }, error = function(e) NULL) else NULL
  list(info_df = .report_drop_empty_cols(info),
       routes_df = if (want("routes_df")) .el_slot_df(object, "routes"),
       trade_df = if (want("trade_df")) .el_slot_df(object, "trade"),
       aeff_df = if (want("aeff_df")) .el_slot_df(object, "aeff"),
       invcost_df = if (want("invcost_df"))
         .clean_cost_df_report(tryCatch(object@invcost,
           error = function(e) NULL), "invcost"),
       fixom_df = if (want("fixom_df"))
         .clean_cost_df_report(tryCatch(object@fixom,
           error = function(e) NULL), "fixom"),
       varom_df = if (want("varom_df"))
         .clean_cost_df_report(tryCatch(object@varom,
           error = function(e) NULL), "varom"),
       capacity_df = if (want("capacity_df"))
         .el_slot_df(object, "capacity"),
       map_plot = map_plot, levcost_df = lc_df)
}

.el_params_lever <- function(object, want, slot_name) {
  info <- do.call(rbind, list(
    .el_pair("commodity", .slot_chr_report(object, "comm")),
    .el_pair("regions", .slot_chr_report(object, "region")),
    .el_pair("default value", .slot_chr_report(object, "defVal"))))
  list(info_df = .report_drop_empty_cols(info),
       rate_df = if (want("rate_df")) .el_slot_df(object, slot_name))
}

.el_params_constraint <- function(object, want) {
  eq <- tryCatch(as.character(object@eq)[1], error = function(e) "")
  eq_disp <- switch(toupper(eq %||% ""), LO = ">=", UP = "<=", FX = "==", eq)
  fe <- tryCatch(object@for.each, error = function(e) NULL)
  fe_str <- if (is.list(fe) && length(fe) > 0) {
    paste(vapply(names(fe), function(k) {
      n <- length(fe[[k]])
      if (n > 0) paste0(k, " (", n, ")") else k
    }, character(1)), collapse = ", ")
  } else ""
  info <- do.call(rbind, list(
    .el_pair("type", paste0(eq, " (", eq_disp, ")")),
    .el_pair("for each", fe_str),
    .el_pair("default value", .slot_chr_report(object, "defVal")),
    .el_pair("interpolation", .slot_chr_report(object, "interpolation"))))
  lhs <- tryCatch(object@lhs, error = function(e) list())
  summands <- if (want("summands_df") && length(lhs) > 0) tryCatch({
    rows <- lapply(seq_along(lhs), function(i) {
      s <- lhs[[i]]
      fs <- tryCatch(s@for.sum, error = function(e) NULL)
      fs_str <- if (is.list(fs) && length(fs) > 0)
        paste(names(fs), collapse = ", ") else ""
      mult <- tryCatch({
        m <- s@mult
        if (is.data.frame(m) && nrow(m) > 0)
          paste0("table (", nrow(m), " rows)") else
          .slot_chr_report(s, "defVal")
      }, error = function(e) "")
      data.frame(
        summand = names(lhs)[i] %||% as.character(i),
        variable = .slot_chr_report(s, "variable"),
        for.sum = fs_str,
        timeframe = .slot_chr_report(s, "timeframe"),
        geoframe = .slot_chr_report(s, "geoframe"),
        mult = mult, stringsAsFactors = FALSE)
    })
    .report_drop_empty_cols(do.call(rbind, rows))
  }, error = function(e) NULL) else NULL
  eq_text <- tryCatch(paste(
    paste(vapply(seq_along(lhs), function(i)
      .slot_chr_report(lhs[[i]], "variable"), character(1)),
      collapse = " + "),
    eq_disp, "rhs"), error = function(e) NULL)
  list(info_df = .report_drop_empty_cols(info),
       eq_text = eq_text,
       summands_df = summands,
       rhs_df = if (want("rhs_df")) .el_slot_df(object, "rhs"))
}
# ── hoisted config/calendar builders (shared with .report_model, which
#    must keep byte-identical output) ─────────────────────────────────────────

#' @noRd
.config_report_rows <- function(cfg) {
  horizon <- tryCatch(cfg@horizon, error = function(e) NULL)
  cal <- tryCatch(cfg@calendar, error = function(e) NULL)
  pair <- .el_pair
  cfg_rows <- list(pair("regions", paste(cfg@region, collapse = ", ")))
  if (!is.null(horizon) && nrow(horizon@intervals) > 0) {
    cfg_rows <- c(cfg_rows,
      list(pair("horizon", paste(range(horizon@period), collapse = " - ")),
           pair("milestone years",
                paste(horizon@intervals$mid, collapse = ", "))))
  }
  if (!is.null(cal)) {
    ns <- tryCatch(nrow(cal@timeslice_share), error = function(e) NA)
    cfg_rows <- c(cfg_rows, list(pair("calendar",
      paste0(if (nzchar(cal@name)) cal@name else "(unnamed)",
             " (", ns, " timeslices)"))))
  }
  # Both rates, each under its own name -- they do different jobs and a
  # model may well set them differently.
  dsc <- tryCatch(cfg@discount, error = function(e) NULL)
  if (is.data.frame(dsc) && nrow(dsc) > 0) {
    for (col in c("wacc", "sdr")) {
      if (!col %in% names(dsc)) next
      v <- suppressWarnings(as.numeric(dsc[[col]]))
      v <- v[is.finite(v)]
      if (length(v) > 0) {
        cfg_rows <- c(cfg_rows, list(pair(col,
          paste(unique(signif(v, 4)), collapse = ", "))))
      }
    }
  }
  cfg_rows <- c(cfg_rows, list(pair("optimizeRetirement",
    as.character(isTRUE(cfg@optimizeRetirement)))))
  do.call(rbind, cfg_rows)
}

# tidy per-region/year discount rates (the config rows only carry the
# distinct values)
#' @noRd
.config_discount_df <- function(cfg) {
  dsc <- tryCatch(cfg@discount, error = function(e) NULL)
  if (!is.data.frame(dsc) || nrow(dsc) == 0) return(NULL)
  keep <- intersect(c("region", "year", "wacc", "sdr"), names(dsc))
  .report_drop_empty_cols(dsc[, keep, drop = FALSE])
}

#' @noRd
.calendar_frames_df <- function(cal) {
  if (length(cal@timeslices_in_frame) == 0) return(NULL)
  data.frame(timeframe = names(cal@timeslices_in_frame),
             timeslices = as.integer(cal@timeslices_in_frame),
             stringsAsFactors = FALSE)
}

.el_params_calendar <- function(object, want) {
  info <- do.call(rbind, list(
    .el_pair("timeframes", length(tryCatch(object@timeframes,
      error = function(e) NULL))),
    .el_pair("default timeframe",
             .slot_chr_report(object, "default_timeframe")),
    .el_pair("year fraction", .slot_chr_report(object, "year_fraction")),
    .el_pair("timeslices", NROW(tryCatch(object@timeslice_share,
      error = function(e) NULL)))))
  list(info_df = .report_drop_empty_cols(info),
       frames_df = if (want("frames_df")) .calendar_frames_df(object),
       share_df = if (want("share_df")) tryCatch(.report_drop_empty_cols(
         utils::head(as.data.frame(object@timeslice_share), 200)),
         error = function(e) NULL))
}

.el_params_horizon <- function(object, want) {
  iv <- tryCatch(object@intervals, error = function(e) NULL)
  info <- do.call(rbind, list(
    .el_pair("period", paste(range(tryCatch(object@period,
      error = function(e) NA)), collapse = " - ")),
    .el_pair("intervals", NROW(iv)),
    .el_pair("milestone years", paste(tryCatch(iv$mid,
      error = function(e) character(0)), collapse = ", "))))
  list(info_df = .report_drop_empty_cols(info),
       intervals_df = if (want("intervals_df") && is.data.frame(iv))
         .report_drop_empty_cols(as.data.frame(iv)))
}

.el_params_config <- function(object, want, gg_ok) {
  cal <- tryCatch(object@calendar, error = function(e) NULL)
  hor <- tryCatch(object@horizon, error = function(e) NULL)
  geo_map <- NULL
  if (gg_ok && want("geo_map") &&
      requireNamespace("geoscales", quietly = TRUE) &&
      requireNamespace("sf", quietly = TRUE)) {
    geo_map <- tryCatch(plot_geoscale(getGeoscale(object), type = "map"),
                        error = function(e) NULL)
  }
  list(info_df = .config_report_rows(object),
       config_df = .config_report_rows(object),
       discount_df = if (want("discount_df")) .config_discount_df(object),
       frames_df = if (want("frames_df") && !is.null(cal))
         .calendar_frames_df(cal),
       calendar_plot = if (gg_ok && want("calendar_plot") && !is.null(cal))
         tryCatch(ggplot2::autoplot(cal), error = function(e) NULL),
       horizon_plot = if (gg_ok && want("horizon_plot") && !is.null(hor))
         tryCatch(ggplot2::autoplot(hor), error = function(e) NULL),
       intervals_df = if (want("intervals_df") && !is.null(hor))
         tryCatch(.report_drop_empty_cols(as.data.frame(hor@intervals)),
                  error = function(e) NULL),
       geo_map = geo_map,
       main_plot = NULL)
}

.el_params_costs <- function(object, want) {
  info <- do.call(rbind, list(
    .el_pair("variable", .slot_chr_report(object, "variable")),
    .el_pair("default value", .slot_chr_report(object, "defVal")),
    .el_pair("subset rows", NROW(tryCatch(object@subset,
      error = function(e) NULL))),
    .el_pair("multiplier rows", NROW(tryCatch(object@mult,
      error = function(e) NULL)))))
  list(info_df = .report_drop_empty_cols(info),
       subset_df = if (want("subset_df")) .el_slot_df(object, "subset"),
       mult_df = if (want("mult_df")) .el_slot_df(object, "mult"))
}

#' @describeIn report Element datasheets. Every element class -- commodity,
#'   supply, demand, trade, import, export, weather, tax, subsidy,
#'   constraint, calendar, horizon, config, costs -- has a `report()`
#'   method rendering a one-object datasheet from its shipped
#'   `report_<class>.Rmd` template. The default template is the class name;
#'   an object-level default can be stored in `misc$report` (the techspec
#'   `report:` key), and an explicit `template =` always wins. From a
#'   container, `report(mod, name = "X")` finds the element of any class
#'   (pass `class = ` to disambiguate same-named objects); the levelized
#'   cost section applies to technology, storage and trade only.
#' @export
setMethod("report", "commodity", .report_element_method)

#' @rdname report
#' @export
setMethod("report", "supply", .report_element_method)

#' @rdname report
#' @export
setMethod("report", "demand", .report_element_method)

#' @rdname report
#' @export
setMethod("report", "trade", .report_element_method)

#' @rdname report
#' @export
setMethod("report", "import", .report_element_method)

#' @rdname report
#' @export
setMethod("report", "export", .report_element_method)

#' @rdname report
#' @export
setMethod("report", "weather", .report_element_method)

#' @rdname report
#' @export
setMethod("report", "tax", .report_element_method)

#' @rdname report
#' @export
setMethod("report", "subsidy", .report_element_method)

#' @rdname report
#' @export
setMethod("report", "constraint", .report_element_method)

#' @rdname report
#' @export
setMethod("report", "calendar", .report_element_method)

#' @rdname report
#' @export
setMethod("report", "horizon", .report_element_method)

#' @rdname report
#' @export
setMethod("report", "config", .report_element_method)

#' @rdname report
#' @export
setMethod("report", "costs", .report_element_method)
