# =========================================================================== #
# operability.R -- declared processes that cannot operate, where and when.
#
# Read off an interpolated scenario (solved or not), without solving: the
# region closure, the span and domain maps, the bounds and the user
# constraints. Two kinds of reason:
#   structural  the model does not carry what the process needs (an input with
#               no source in the region; no stock and no way to invest in a
#               year);
#   bound       a declared bound or a one-term user constraint fixes the
#               capacity or activity at zero.
# Restrictions that act through the solution (a CO2 cap, a price) are not
# covered: they depend on what else is built.
# =========================================================================== #

#' Declared processes that cannot operate
#'
#' Lists the processes of an interpolated scenario that cannot operate in a
#' region and year, and why. Only what follows directly from the model data
#' is reported: an input commodity with no source in the region, no capacity
#' in a year (no stock left and no investment window), or a bound or one-term
#' user constraint that fixes the capacity, availability or activity at zero.
#' Limits that act through the solution, such as an emission cap or a tax, are
#' not covered.
#'
#' @param object an interpolated `scenario` (solved or not).
#'
#' @return a data frame with columns `process`, `class`, `region`, `years`
#'   (comma-separated milestone years, or `"all"`), `kind` (`"structural"` or
#'   `"bound"`), `reason` and `detail`; zero rows when every process can
#'   operate.
#' @export
#' @examples
#' cal <- newCalendar(
#'   timetable = make_timetable(struct = list(ANNUAL = "ANNUAL")),
#'   name = "annual")
#' mod <- newModel(
#'   name = "op", region = "R1", horizon = newHorizon(2025), discount = 0,
#'   calendar = cal,
#'   repo = newRepository("r",
#'     newCommodity("ELC"),
#'     newSupply("SUP", commodity = "ELC", supply = data.frame(cost = 1)),
#'     newTechnology("T", input = list(comm = "ELC"),
#'                   output = list(comm = "ELC"), cap2act = 1,
#'                   capacity = data.frame(stock = 1, cap.up = 0)),
#'     newDemand("D", commodity = "ELC", demand = data.frame(demand = 1))))
#' scen <- interpolate_model(mod, name = "op_example")
#' process_operability(scen)
process_operability <- function(object) {
  if (!methods::is(object, "scenario")) {
    stop("`process_operability()` needs an interpolated scenario; ",
         "interpolate the model first.", call. = FALSE)
  }
  years <- as.integer(object@settings@horizon@intervals$mid)
  out <- list(
    .op_region_gaps(object),
    .op_no_capacity(object, years, "mTechSpan", "tech", "technology"),
    .op_no_capacity(object, years, "mStorageSpan", "stg", "storage"),
    .op_bound_zero(object, years, "pTechCap", "tech", "technology", "cap",
                   "capacity bound `cap.up` / `cap.fx` = 0"),
    .op_bound_zero(object, years, "pStorageStgCap", "stg", "storage", "cap",
                   "reservoir bound `stg.cap.up` / `stg.cap.fx` = 0"),
    .op_af_zero(object, years, "pTechAf", "tech", "technology", "mTechTimeslice",
                "availability `af.up` / `af.fx` = 0 in every timeslice"),
    .op_af_zero(object, years, "pSupAva", "sup", "supply", "mSupTimeslice",
                "availability `ava.up` / `ava.fx` = 0 in every timeslice"),
    .op_constraints(object, years))
  out <- out[!vapply(out, is.null, logical(1))]
  empty <- data.frame(process = character(), class = character(),
                      region = character(), years = character(),
                      kind = character(), reason = character(),
                      detail = character(), stringsAsFactors = FALSE)
  if (!length(out)) return(empty)
  out <- do.call(rbind, out)
  out <- out[order(out$kind, out$reason, out$class, out$process, out$region), ,
             drop = FALSE]
  rownames(out) <- NULL
  out
}

.op_years <- function(y, all) {
  y <- sort(unique(as.integer(y)))
  if (!length(y) || all(all %in% y)) "all" else paste(y, collapse = ", ")
}

.op_row <- function(process, class, region, years, kind, reason, detail = "") {
  data.frame(process = as.character(process), class = class,
             region = as.character(region), years = years, kind = kind,
             reason = reason, detail = as.character(detail),
             stringsAsFactors = FALSE)
}

# an input commodity with no source in the region (the cells interpolation
# dropped; recorded on the scenario)
.op_region_gaps <- function(scen) {
  g <- scen@misc$region_gaps
  if (is.null(g) || !nrow(g)) return(NULL)
  .op_row(g$process, g$class, g$region, "all", "structural",
          "input has no source in the region",
          paste("missing:", g$missing))
}

# a (process, region) with no capacity possible in a milestone year: no stock
# there, and no year within one lifetime before it in which new capacity may
# be built (an investment window with a non-zero new-capacity bound)
.op_no_capacity <- function(scen, years, span, key, cls) {
  sp <- .gds(scen, span)
  if (is.null(sp) || !nrow(sp) || !all(c(key, "region") %in% colnames(sp)))
    return(NULL)
  pairs <- unique(sp[!is.na(sp[[key]]) & !is.na(sp$region), c(key, "region")])
  if (!nrow(pairs)) return(NULL)
  pre <- if (cls == "storage") "pStorage" else "pTech"
  stock_pars <- if (cls == "storage") paste0(pre, c("InpStock", "StgStock", "OutStock")) else
    paste0(pre, "Stock")
  ncap_par <- if (cls == "storage") "pStorageStgNewCap" else "pTechNewCap"
  new_map <- if (cls == "storage") "mStorageNew" else "mTechNew"
  keyed <- function(d) paste(d[[key]], d$region, sep = "|")
  # stock years
  st <- NULL
  for (p in stock_pars) {
    d <- .gds(scen, p)
    if (is.null(d) || !nrow(d)) next
    d <- d[!is.na(d$value) & d$value > 0, , drop = FALSE]
    if (!nrow(d)) next
    d <- .op_expand(scen, d, key, cls)
    if (nrow(d) && "year" %in% colnames(d)) st <- rbind(st, d[, c(key, "region", "year")])
  }
  # investment years with a non-zero new-capacity bound
  nw <- .gds(scen, new_map)
  if (!is.null(nw) && nrow(nw)) {
    nw <- nw[, c(key, "region", "year")]
    z <- .gds(scen, ncap_par)
    if (!is.null(z) && nrow(z) && "type" %in% colnames(z)) {
      z <- z[as.character(z$type) %in% c("up", "fx") & !is.na(z$value) & z$value <= 0, , drop = FALSE]
      if (nrow(z)) {
        z <- .op_expand(scen, z, key, cls)
        if (nrow(z) && "year" %in% colnames(z))
          nw <- dplyr::anti_join(nw, unique(z[, c(key, "region", "year")]),
                                 by = c(key, "region", "year"))
      }
    }
  }
  ol <- .gds(scen, paste0(pre, "Olife"))
  olife <- function(p, r) {
    if (is.null(ol) || !nrow(ol)) return(Inf)
    m <- (is.na(ol[[key]]) | ol[[key]] == p) &
      (!"region" %in% colnames(ol) | is.na(ol$region) | ol$region == r)
    v <- ol$value[m]
    if (length(v)) max(v) else Inf
  }
  st_k <- if (is.null(st)) list() else split(as.integer(st$year), keyed(st))
  nw_k <- if (is.null(nw) || !nrow(nw)) list() else split(as.integer(nw$year), keyed(nw))
  res <- list()
  for (i in seq_len(nrow(pairs))) {
    p <- as.character(pairs[[key]][i]); r <- as.character(pairs$region[i])
    k <- paste(p, r, sep = "|")
    ok <- unique(as.integer(st_k[[k]] %||% integer(0)))
    built <- nw_k[[k]] %||% integer(0)
    if (length(built)) {
      L <- olife(p, r)
      ok <- c(ok, years[vapply(years, function(y) any(built <= y & y < built + L),
                               logical(1))])
    }
    miss <- setdiff(years, ok)
    if (!length(miss)) next
    detail <- if (!length(ok)) "no stock and no investment window" else {
      lo <- min(ok); hi <- max(ok)
      paste(c(if (any(miss < lo)) sprintf("before %d: no stock, investment window not open", lo),
              if (any(miss > hi)) sprintf("after %d: stock retired, investment window closed", hi),
              if (any(miss > lo & miss < hi)) "gap between stock and new capacity"),
            collapse = "; ")
    }
    res[[length(res) + 1L]] <- .op_row(
      p, cls, r, .op_years(miss, years), "structural", "no capacity in the year",
      detail)
  }
  if (length(res)) do.call(rbind, res) else NULL
}

# expand NA wildcards of a bound parameter's (key, region, year) against the
# process span
.op_expand <- function(scen, d, key, cls) {
  span <- .gds(scen, if (cls == "storage") "mStorageSpan" else
                     if (cls == "supply") "mSupSpan" else "mTechSpan")
  if (is.null(span)) return(d)
  span <- span[, intersect(c(key, "region", "year"), colnames(span)), drop = FALSE]
  by <- intersect(c(key, "region", "year"), colnames(d))
  fixed <- by[vapply(by, function(b) !anyNA(d[[b]]), logical(1))]
  wild <- setdiff(intersect(by, colnames(span)), fixed)
  if (!length(wild)) return(d)
  d2 <- d[, setdiff(colnames(d), wild), drop = FALSE]
  merge(d2, unique(span), by = intersect(fixed, colnames(span)))
}

# a capacity bound fixed at zero
.op_bound_zero <- function(scen, years, par, key, cls, what, reason) {
  d <- .gds(scen, par)
  if (is.null(d) || !nrow(d) || !"type" %in% colnames(d)) return(NULL)
  d <- d[as.character(d$type) %in% c("up", "fx") & !is.na(d$value) &
           d$value <= 0, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  d <- .op_expand(scen, d, key, cls)
  if (!nrow(d) || !"year" %in% colnames(d)) return(NULL)
  d <- unique(d[, c(key, "region", "year")])
  res <- lapply(split(d, paste(d[[key]], d$region)), function(x)
    .op_row(x[[key]][1], cls, x$region[1], .op_years(x$year, years), "bound",
            reason))
  do.call(rbind, res)
}

# an availability fixed at zero in every timeslice of a year
.op_af_zero <- function(scen, years, par, key, cls, tsmap, reason) {
  d <- .gds(scen, par)
  if (is.null(d) || !nrow(d) || !"type" %in% colnames(d)) return(NULL)
  z <- d[as.character(d$type) %in% c("up", "fx") & !is.na(d$value) &
           d$value <= 0, , drop = FALSE]
  if (!nrow(z)) return(NULL)
  z <- .op_expand(scen, z, key, cls)
  if (!nrow(z) || !"year" %in% colnames(z)) return(NULL)
  # the slices the process runs in; a wildcard timeslice covers them all
  slices <- if (!is.null(tsmap)) .gds(scen, tsmap) else NULL
  all_sl <- as.character(scen@modInp@sets$timeslice)
  n_of <- function(p) {
    if (is.null(slices)) return(length(all_sl))
    sum(as.character(slices[[key]]) == p)
  }
  res <- list()
  for (g in split(z, paste(z[[key]], z$region, z$year))) {
    p <- as.character(g[[key]][1])
    full <- anyNA(g$timeslice) || length(unique(g$timeslice)) >= n_of(p)
    if (full) res[[length(res) + 1L]] <- data.frame(
      process = p, region = as.character(g$region[1]), year = g$year[1],
      stringsAsFactors = FALSE)
  }
  if (!length(res)) return(NULL)
  r <- do.call(rbind, res)
  do.call(rbind, lapply(split(r, paste(r$process, r$region)), function(x)
    .op_row(x$process[1], cls, x$region[1], .op_years(x$year, years), "bound",
            reason)))
}

# a one-term user constraint holding a capacity or activity at or below zero
.op_constraint_vars <- c(vTechCap = "tech", vTechAct = "tech",
                         vStorageStgCap = "stg", vStorageOutCap = "stg",
                         vStorageInpCap = "stg", vSupOut = "sup")

.op_constraints <- function(scen, years) {
  res <- list()
  objs <- .model_objects(.scen_build_model(scen))
  for (k in objs) {
    if (!methods::is(k, "constraint") || length(k@lhs) != 1L) next
    if (!as.character(k@eq) %in% c("<=", "==")) next
    tm <- k@lhs[[1]]
    key <- .op_constraint_vars[tm@variable]
    if (is.na(key)) next
    rhs0 <- if (nrow(k@rhs)) all(abs(k@rhs$rhs) <= 1e-9, na.rm = TRUE) else
      abs(k@defVal) <= 1e-9
    mult_pos <- if (nrow(tm@mult)) all(tm@mult$mult > 0, na.rm = TRUE) else
      tm@defVal > 0
    if (!rhs0 || !mult_pos) next
    procs <- tm@for.sum[[key]]
    if (is.null(procs) || all(is.na(procs))) procs <- "(all)"
    fe <- k@for.each
    reg <- if ("region" %in% colnames(fe)) .nonblank(fe$region) else character(0)
    yrs <- if ("year" %in% colnames(fe)) .nonblank(fe$year) else character(0)
    cls <- c(tech = "technology", stg = "storage", sup = "supply")[[key]]
    res[[length(res) + 1L]] <- .op_row(
      procs, cls, if (length(reg)) paste(reg, collapse = ", ") else "all",
      if (length(yrs)) paste(yrs, collapse = ", ") else "all", "bound",
      "user constraint holds it at zero",
      sprintf("%s: %s %s 0", k@name, tm@variable, as.character(k@eq)))
  }
  if (length(res)) do.call(rbind, res) else NULL
}
