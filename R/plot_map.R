# plot_map ####################################################################
# Region-level maps of model results.
#
# Everything here is presentation: results come out of `getMix()`/`getData()`
# as they already do, are optionally rolled up to a coarser level of the
# model's geoscale, and are drawn. Nothing touches `modInp` or the solver.
#
# Aggregation is delegated to `geoscales::geo_recast()`. energyRt owns only the
# part geoscales cannot know about: which variables must not be summed
# naively. That is read off the variable catalogue (`.variables`, built from
# data-raw/variables.yml), never hardcoded -- the same approach
# `.is_state_var()` takes for the temporal roll-up in R/get_data.R.

#' @include geoscale.R plot_scenario.R
NULL

# Variable-catalogue helpers ---------------------------------------------------

#' Is this variable an inter-regional flow?
#'
#' `vTradeIr` is the only variable declared `role: flow` -- and, independently,
#' the only one carrying two `region` dimensions. Both tests agree, so the
#' declared role is the criterion: a future inter-regional variable is covered
#' by declaring what it is, not by editing a list here.
#'
#' Summing such a variable over an aggregated region double-counts, because a
#' flow between two regions that end up in the same group is internal to that
#' group and must cancel. See `.geo_net_trade()`.
#'
#' @param nm character vector of variable names.
#' @return logical vector.
#' @noRd
.is_flow_var <- function(nm) {
  spec <- tryCatch(.variables, error = function(e) NULL)
  if (is.null(spec)) return(rep(FALSE, length(nm)))
  vapply(nm, function(v) {
    identical(tryCatch(spec[[v]]$role, error = function(e) NULL), "flow")
  }, logical(1), USE.NAMES = FALSE)
}

#' Does this variable carry a region dimension?
#' @noRd
.has_region_dim <- function(nm) {
  spec <- tryCatch(.variables, error = function(e) NULL)
  if (is.null(spec)) return(rep(FALSE, length(nm)))
  vapply(nm, function(v) {
    d <- tryCatch(spec[[v]]$dimSets, error = function(e) NULL)
    !is.null(d) && "region" %in% d
  }, logical(1), USE.NAMES = FALSE)
}

# Aggregation ------------------------------------------------------------------

#' Roll results up to a coarser level of a geoscale
#'
#' Extensive quantities are summed. Inter-regional flows are netted instead:
#' a flow whose endpoints land in the same target group is internal and
#' cancels, leaving only what crosses the new boundary.
#'
#' Note the roles in the variable catalogue are all extensive, so unlike the
#' temporal roll-up there is no "do not sum this" list. In particular
#' `role: stock` is a *temporal* exclusion only -- a storage level must not be
#' summed over timeslices, but summing it across regions is perfectly meaningful.
#'
#' @param df data.frame of results.
#' @param gs a `geoscales::Geoscale`.
#' @param from,to level names.
#' @param name optional variable name, used to look up its role.
#' @param key name of the region column in `df`.
#'
#' @return A data.frame keyed by `to`.
#' @noRd
.geo_aggregate <- function(df, gs, from, to, name = NULL, key = NULL) {
  check_package("geoscales")
  if (identical(from, to)) return(df)

  if (!is.null(name) && length(name) == 1L && .is_flow_var(name)) {
    return(.geo_net_trade(df, gs, from = from, to = to))
  }
  if (all(c("src", "dst") %in% names(df))) {
    # No name to look up (a bare frame), but the two-endpoint shape is
    # unambiguous on its own.
    return(.geo_net_trade(df, gs, from = from, to = to))
  }

  key <- key %||% if (from %in% names(df)) from else "region"
  geoscales::geo_recast(df, gs, from = from, to = to, key = key,
                        rule = "sum", na_action = "keep")
}

#' Net inter-regional flows across an aggregation
#'
#' Maps both endpoints to the target level. Rows whose endpoints land in the
#' same group are internal to it and are dropped. Each surviving row is
#' expanded into a debit on its source group and a credit on its destination
#' group, so the result is a **net** quantity per region.
#'
#' This is the operation `getMix()` avoids by skipping trade entirely when
#' `region = NULL` ("they cancel out in an all-region sum") -- here they are
#' cancelled explicitly, at whatever level was asked for.
#'
#' @return A data.frame keyed by `to`, with `value` the net inflow.
#' @noRd
.geo_net_trade <- function(df, gs, from, to) {
  check_package("geoscales")
  if (!all(c("src", "dst") %in% names(df))) {
    stop("`.geo_net_trade()` needs `src` and `dst` columns.", call. = FALSE)
  }
  map <- .geo_level_map(gs, from = from, to = to)

  d <- df
  d$.src <- unname(map[as.character(d$src)])
  d$.dst <- unname(map[as.character(d$dst)])
  d <- d[!is.na(d$.src) & !is.na(d$.dst), , drop = FALSE]
  # Internal to the aggregate: cancels.
  d <- d[d$.src != d$.dst, , drop = FALSE]
  if (nrow(d) == 0L) {
    out <- df[0, setdiff(names(df), c("src", "dst")), drop = FALSE]
    out[[to]] <- character()
    return(out)
  }

  id <- setdiff(names(df), c("src", "dst", "value"))
  exports <- d[, c(id, "value"), drop = FALSE]
  exports[[to]] <- d$.src
  exports$value <- -d$value

  imports <- d[, c(id, "value"), drop = FALSE]
  imports[[to]] <- d$.dst

  both <- rbind(exports, imports)
  grp <- c(to, id)
  agg <- stats::aggregate(both["value"], by = both[grp], FUN = sum)
  agg[, c(grp, "value"), drop = FALSE]
}

#' Named map from codes at `from` to codes at `to`
#' @noRd
.geo_level_map <- function(gs, from, to) {
  fam <- geoscales::geo_family(gs, parent = to, child = from)
  stats::setNames(fam$parent, fam$child)
}

# Maps -------------------------------------------------------------------------

#' Map model results by region
#'
#' Draws a choropleth of solved results over the model's geography, optionally
#' aggregated to a coarser level of the geoscale.
#'
#' Requires a geoscale with geometry attached (see [setGeoscale()] and
#' `geoscales::geo_attach_geometry()`), plus `sf` and `ggplot2`.
#'
#' @param object a solved `scenario`.
#' @param type what to map, passed to [getMix()]: `"generation"`, `"capacity"`,
#'   `"new_capacity"` or `"fuel"`.
#' @param level level of the geoscale to draw. Defaults to the finest, i.e. the
#'   model's own regions. A coarser level aggregates first.
#' @param comm balanced commodity for `"generation"`.
#' @param year integer year, or `NULL` for the sum over all milestone years.
#' @param process optional regular expression selecting processes to include.
#' @param geoscale a `geoscales::Geoscale`, or `NULL` to use the scenario's.
#' @param palette viridis palette option.
#' @param ... passed to `ggplot2::geom_sf()`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' scen <- solve(interpolate(mod, "BASE"))
#' plot_map(scen, "capacity")
#' plot_map(scen, "capacity", level = "zone")
#' }
#'
#' @family geoscale
#' @export
plot_map <- function(object,
                    type = c("generation", "capacity", "new_capacity", "fuel"),
                    level = NULL, comm = "ELC", year = NULL,
                    process = NULL, geoscale = NULL,
                    palette = "D", ...) {
  type <- match.arg(type)
  check_package("ggplot2")
  check_package("sf")
  gs <- .resolve_geoscale(object, geoscale)

  finest <- .geo_default_level(gs)
  level <- level %||% finest

  d <- getMix(object, type = type, comm = comm, year = year)
  if (is.null(d) || nrow(d) == 0L) {
    stop("No results for type '", type, "'.", call. = FALSE)
  }
  if (!is.null(process)) {
    d <- d[grepl(process, d$process), , drop = FALSE]
    if (nrow(d) == 0L) {
      stop("No process matched '", process, "'.", call. = FALSE)
    }
  }

  # [nested-regions] A commodity balanced at a coarse `@geolevel` puts rows at
  # that level (national steel demand lands on the nation, not on a state).
  # Mixing them with the fine rows would double-count, and they have no
  # geometry of their own at `finest` anyway, so drop them and say so.
  atoms <- geoscales::geo_regions(gs, finest)
  coarse <- setdiff(unique(as.character(d$region)), atoms)
  if (length(coarse) > 0) {
    d <- d[d$region %in% atoms, , drop = FALSE]
    warning("Dropped ", length(coarse), " row group(s) at a coarser level (",
            paste(utils::head(sort(coarse), 3), collapse = ", "),
            if (length(coarse) > 3) ", ..." else "",
            "): they are balanced above '", finest, "' and cannot be mapped ",
            "there.", call. = FALSE)
    if (nrow(d) == 0L) {
      stop("No results for type '", type, "' at level '", finest, "'.",
           call. = FALSE)
    }
  }

  # One number per region: sum the processes, and the years unless one was
  # asked for (getMix has already filtered to it).
  d <- stats::aggregate(d["value"], by = d["region"], FUN = sum)

  if (!identical(level, finest)) {
    d <- .geo_aggregate(d, gs, from = finest, to = level, key = "region")
  } else {
    names(d)[names(d) == "region"] <- level
  }

  # Drawing belongs to geoscales: it owns the geometry, the levels and the
  # dissolving. energyRt's job ends here, having decided WHAT to plot -- which
  # variable, netted or summed, at which level.
  geoscales::geo_plot(
    gs, data = d, level = level, fill = "value",
    palette = palette,
    title = paste0(type, if (!is.null(year)) paste0(", ", year) else ""),
    subtitle = paste0("by ", level),
    fill_label = NULL,
    ...
  )
}
