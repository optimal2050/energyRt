# plot_map ####################################################################
# Region-level maps of model results.
#
# Everything here is presentation: results come out of `getMix()`/`getData()`
# as they already do, are optionally rolled up to a coarser level of the
# model's geoscale, and are drawn. Nothing touches `modInp` or the solver.
#
# Aggregation is delegated to `geoscales::recast_geoscale()`. energyRt owns only the
# part geoscales cannot know about: which variables must not be summed
# naively. That is read off the variable catalogue (`.variables`, built from
# data-raw/variables.yml), never hardcoded -- the same approach
# `.is_state_var()` takes for the temporal roll-up in R/get_data.R.

#' @include geoscale.R plot_scenario.R
NULL

# Variable-catalogue helpers ---------------------------------------------------

#' Is this variable carried BETWEEN regions?
#'
#' `vTradeIr` is the only variable declared `role: interregional` -- and,
#' independently, the only one carrying two `region` dimensions. Both tests
#' agree, so the declared role is the criterion: a future interregional variable
#' is covered by declaring what it is, not by editing a list here.
#'
#' The role was called `flow` until 0.85, which was a misnomer: `vTechOut` is
#' `source`, `vTechInp` is `sink`, `vTechInv` is `cost`, and all of them are
#' flows in the ordinary sense, region-indexed, and sum across regions perfectly
#' well. What singles this one out is the second region index.
#'
#' Summing such a variable over an aggregated region double-counts, because a
#' transfer between two regions that end up in the same group is internal to
#' that group and must cancel. See `.geo_net_trade()`.
#'
#' @param nm character vector of variable names.
#' @return logical vector.
#' @noRd
.is_interregional_var <- function(nm) {
  spec <- tryCatch(.variables, error = function(e) NULL)
  if (is.null(spec)) return(rep(FALSE, length(nm)))
  vapply(nm, function(v) {
    identical(tryCatch(spec[[v]]$role, error = function(e) NULL), "interregional")
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

  if (!is.null(name) && length(name) == 1L && .is_interregional_var(name)) {
    return(.geo_net_trade(df, gs, from = from, to = to))
  }
  if (all(c("src", "dst") %in% names(df))) {
    # No name to look up (a bare frame), but the two-endpoint shape is
    # unambiguous on its own.
    return(.geo_net_trade(df, gs, from = from, to = to))
  }

  key <- key %||% if (from %in% names(df)) from else "region"
  geoscales::recast_geoscale(df, gs, from = from, to = to, key = key,
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
  fam <- geoscales::geoscale_family(gs, parent = to, child = from)
  stats::setNames(fam$parent, fam$child)
}

# Maps -------------------------------------------------------------------------

#' Map model results by region
#'
#' Draws a choropleth of solved results over the model's geography, optionally
#' aggregated to a coarser level of the geoscale.
#'
#' Requires a geoscale with geometry attached (see [setGeoscale()] and
#' `geoscales::attach_geometry_geoscale()`), plus `sf` and `ggplot2`.
#'
#' @param object a solved `scenario`.
#' @param type what to map, passed to [getMix()]: `"generation"`, `"capacity"`,
#'   `"new_capacity"` or `"fuel"`. Ignored when `name` is given.
#' @param level level of the geoscale to draw. Defaults to the finest, i.e. the
#'   model's own regions. A coarser level aggregates first.
#' @param comm balanced commodity for `"generation"`. On the `name =` path a
#'   `comm` filter applies only when passed explicitly.
#' @param year integer year, or `NULL` for the sum over all milestone years.
#' @param process optional regular expression selecting processes to include.
#' @param geoscale a `geoscales::Geoscale`, or `NULL` to use the scenario's.
#' @param palette viridis palette option.
#' @param name optional name of any solved variable carrying a `region`
#'   dimension (e.g. `"vTechNewCap"`, `"vTechRetiredStock"`): the map is
#'   drawn from `getData()` instead of [getMix()]. Variables without a
#'   region dimension (the `vTrade*` capacity/retirement family) cannot be
#'   mapped.
#' @param facet `"none"` (default; one map, values summed over years unless
#'   `year =` selects one) or `"year"` (one panel per milestone year).
#' @param ... passed to `ggplot2::geom_sf()`.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' \dontrun{
#' scen <- solve(interpolate(mod, "BASE"))
#' plot_map(scen, "capacity")
#' plot_map(scen, "capacity", level = "zone")
#' plot_map(scen, name = "vTechNewCap", facet = "year")
#' plot_map(scen, name = "vTechRetiredStock")
#' }
#'
#' @family geoscale
#' @export
plot_map <- function(object,
                    type = c("generation", "capacity", "new_capacity", "fuel"),
                    level = NULL, comm = "ELC", year = NULL,
                    process = NULL, geoscale = NULL,
                    palette = "D", name = NULL,
                    facet = c("none", "year"), ...) {
  facet <- match.arg(facet)
  # guard before any geoscale/data work: a clear answer for the un-mappable
  if (!is.null(name) && !.has_region_dim(name)) {
    dims <- tryCatch(.variables[[name]]$dimSets, error = function(e) NULL)
    stop("'", name, "' carries no region dimension",
         if (!is.null(dims)) paste0(" (dims: ",
                                    paste(dims, collapse = ", "), ")"),
         " and cannot be mapped. The vTrade* capacity/retirement family ",
         "is keyed by route only -- use getData() + bar charts instead.",
         call. = FALSE)
  }
  check_package("ggplot2")
  check_package("sf")
  gs <- .resolve_geoscale(object, geoscale)

  finest <- .geo_default_level(gs)
  level <- level %||% finest

  if (is.null(name)) {
    type <- match.arg(type)
    label <- type
    d <- getMix(object, type = type, comm = comm, year = year)
    if (is.null(d) || nrow(d) == 0L) {
      stop("No results for type '", type, "'.", call. = FALSE)
    }
  } else {
    label <- name
    d <- tryCatch(as.data.frame(
      getData(object, name = name, merge = TRUE, drop.zeros = TRUE)),
      error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0L || !"value" %in% names(d)) {
      stop("No results for '", name, "'.", call. = FALSE)
    }
    if (!missing(comm) && "comm" %in% names(d)) {
      d <- d[d$comm %in% comm, , drop = FALSE]
    }
    if (!is.null(year) && "year" %in% names(d)) {
      d <- d[d$year %in% year, , drop = FALSE]
    }
    if (nrow(d) == 0L) {
      stop("No results for '", name, "' after filtering.", call. = FALSE)
    }
  }
  if (!is.null(process) && "process" %in% names(d)) {
    d <- d[grepl(process, d$process), , drop = FALSE]
    if (nrow(d) == 0L) {
      stop("No process matched '", process, "'.", call. = FALSE)
    }
  }
  if (facet == "year" && !"year" %in% names(d)) facet <- "none"

  # Interregional variables (src/dst endpoints) are netted, never summed.
  if (all(c("src", "dst") %in% names(d))) {
    keep <- c("src", "dst", if (facet == "year") "year", "value")
    d <- stats::aggregate(d["value"],
                          by = d[setdiff(keep, "value")], FUN = sum)
    if (identical(level, finest)) {
      id <- setdiff(names(d), c("src", "dst", "value"))
      exp_ <- d[, c(id, "value"), drop = FALSE]
      exp_[[level]] <- as.character(d$src)
      exp_$value <- -exp_$value
      imp_ <- d[, c(id, "value"), drop = FALSE]
      imp_[[level]] <- as.character(d$dst)
      both <- rbind(exp_, imp_)
      d <- stats::aggregate(both["value"],
                            by = both[c(level, id)], FUN = sum)
    } else {
      d <- .geo_net_trade(d, gs, from = finest, to = level)
    }
    return(.plot_map_draw(d, gs, level, facet, palette, label, year, ...))
  }

  # [nested-regions] A commodity balanced at a coarse `@geoframe` puts rows at
  # that level (national steel demand lands on the nation, not on a state).
  # Mixing them with the fine rows would double-count, and they have no
  # geometry of their own at `finest` anyway, so drop them and say so.
  atoms <- geoscales::geoscale_regions(gs, finest)
  coarse <- setdiff(unique(as.character(d$region)), atoms)
  if (length(coarse) > 0) {
    d <- d[d$region %in% atoms, , drop = FALSE]
    warning("Dropped ", length(coarse), " row group(s) at a coarser level (",
            paste(utils::head(sort(coarse), 3), collapse = ", "),
            if (length(coarse) > 3) ", ..." else "",
            "): they are balanced above '", finest, "' and cannot be mapped ",
            "there.", call. = FALSE)
    if (nrow(d) == 0L) {
      stop("No results for '", label, "' at level '", finest, "'.",
           call. = FALSE)
    }
  }

  # One number per region (per year when faceting): sum processes, and the
  # years unless one was asked for or facet = "year" keeps them apart.
  grp <- c("region", if (facet == "year") "year")
  d <- stats::aggregate(d["value"], by = d[grp], FUN = sum)

  if (!identical(level, finest)) {
    if (facet == "year") {
      # recast per year so the year column survives the roll-up
      parts <- lapply(split(d, d$year), function(dd) {
        yy <- dd$year[1]
        out <- .geo_aggregate(dd[, c("region", "value")], gs,
                              from = finest, to = level, key = "region")
        out$year <- yy
        out
      })
      d <- do.call(rbind, parts)
      rownames(d) <- NULL
    } else {
      d <- .geo_aggregate(d, gs, from = finest, to = level, key = "region")
    }
  } else {
    names(d)[names(d) == "region"] <- level
  }

  .plot_map_draw(d, gs, level, facet, palette, label, year, ...)
}

# Draw a prepared per-region table: a single geoscales choropleth, or a
# year-faceted composition via geom_geoscale() (geoscale_plot cannot facet).
#' @noRd
.plot_map_draw <- function(d, gs, level, facet, palette, label, year, ...) {
  title <- paste0(label, if (!is.null(year) && length(year) == 1L)
    paste0(", ", year) else "")
  if (facet == "year") {
    # one geometry copy per milestone year, each carrying that year's
    # values -- built from geoscales' exported geometry accessor, since a
    # single geom_geoscale() layer aggregates the year dimension away
    sfd <- tryCatch({
      shp <- geoscales::geoscale_geometry(gs, level)
      parts <- lapply(split(d, d$year), function(dd) {
        s <- shp
        v <- stats::setNames(dd$value, as.character(dd[[level]]))
        s$value <- unname(v[as.character(s[[level]])])
        s$year <- dd$year[1]
        s
      })
      do.call(rbind, parts)
    }, error = function(e) NULL)
    if (!is.null(sfd)) {
      return(
        ggplot2::ggplot() +
          ggplot2::geom_sf(data = sfd,
                           ggplot2::aes(fill = .data$value,
                                        geometry = .data$geometry),
                           inherit.aes = FALSE, ...) +
          ggplot2::facet_wrap(~year) +
          geoscales::theme_geoscale() +
          ggplot2::scale_fill_viridis_c(option = palette, name = NULL,
                                        na.value = "grey92") +
          ggplot2::labs(title = title, subtitle = paste0("by ", level)))
    }
    message("plot_map(): could not build a year-faceted map (no attached ",
            "geometry?); summing over years instead.")
    grp <- setdiff(names(d), c("year", "value"))
    d <- stats::aggregate(d["value"], by = d[grp], FUN = sum)
  }
  # Drawing belongs to geoscales: it owns the geometry, the levels and the
  # dissolving. energyRt's job ends here, having decided WHAT to plot --
  # which variable, netted or summed, at which level.
  geoscales::geoscale_plot(
    gs, data = d, geoframe = level, fill = "value",
    palette = palette,
    title = title,
    subtitle = paste0("by ", level),
    fill_label = NULL,
    ...
  )
}

#' Draw a model's geoscale
#'
#' Visualises the geography attached to a model, scenario, config, or a bare
#' `geoscales::Geoscale`: a membership map of one geoframe (`"map"`), a
#' layered view of ALL geoframes stacked in a cabinet/perspective projection
#' with the coarsest level at the bottom (`"stack"`), or the geometry-free
#' `"icicle"` diagram.
#'
#' `"map"` and `"stack"` need attached geometry (see
#' `geoscales::attach_geometry_geoscale()`) and the `sf` package; `"icicle"`
#' works without either and is the fallback the report templates degrade to.
#'
#' @param object a `model`, `scenario`, `config`, or `geoscales::Geoscale`.
#' @param type `"map"`, `"stack"`, or `"icicle"`.
#' @param geoframe for `"map"`: the geoframe whose membership colours the
#'   atoms; default = the coarsest level above the atoms.
#' @param view stack projection (`"cabinet"`, `"perspective"`, ... -- see
#'   `geoscales::geoscale_autoplot()`).
#' @param direction `"down"` (default) draws the coarsest geoframe at the
#'   bottom of the stack; `"up"` at the top.
#' @param geoscale a `geoscales::Geoscale` override, or `NULL` to resolve
#'   from `object`.
#' @param ... passed to the geoscales plotting functions.
#'
#' @return A `ggplot` object.
#' @family geoscale
#' @export
plot_geoscale <- function(object, type = c("map", "stack", "icicle"),
                          geoframe = NULL, view = "cabinet",
                          direction = "down", geoscale = NULL, ...) {
  type <- match.arg(type)
  check_package("geoscales")
  gs <- if (is_geoscale(object)) object else
    .resolve_geoscale(object, geoscale)
  frames <- geoscales::geoscale_geoframes(gs)   # coarsest first, atoms last

  if (type == "icicle") {
    return(geoscales::geoscale_autoplot(gs, type = "icicle", ...))
  }

  check_package("ggplot2")
  check_package("sf")

  if (type == "stack") {
    fa <- tryCatch(names(formals(geoscales::geoscale_autoplot)),
                   error = function(e) character(0))
    if (!all(c("view", "direction") %in% fa)) {
      message("plot_geoscale(): this geoscales version has no stack view; ",
              "drawing the icicle instead.")
      return(geoscales::geoscale_autoplot(gs, type = "icicle", ...))
    }
    return(geoscales::geoscale_autoplot(gs, type = "stack", view = view,
                                        direction = direction, ...))
  }

  # type == "map": atoms coloured by their group at `geoframe`
  finest <- .geo_default_level(gs)
  if (is.null(geoframe)) {
    above <- setdiff(frames, finest)
    geoframe <- if (length(above) > 0) above[length(above)] else finest
  }
  d <- data.frame(atom = geoscales::geoscale_regions(gs, finest),
                  stringsAsFactors = FALSE)
  names(d) <- finest
  d$group <- if (identical(geoframe, finest)) d[[finest]] else
    unname(.geo_level_map(gs, from = finest, to = geoframe)[d[[finest]]])
  geoscales::geoscale_plot(gs, data = d, geoframe = finest, fill = "group",
                           title = paste0("geoscale: ",
                                          paste(frames, collapse = " > ")),
                           subtitle = paste0("coloured by ", geoframe),
                           fill_label = geoframe, ...)
}
