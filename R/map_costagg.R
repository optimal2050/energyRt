# =========================================================================== #
# map_costagg.R  —  top-level cost-aggregation mapping builders (family "cost_agg")
#
# One `map_<Name>(scen, fmp) -> scen` per cost-aggregation map. Registered in
# `.cost_agg_builders`. Reuses `.set_map` / get_data_slot from mapping_engine.R.
# =========================================================================== #

# Full region x year grid (the domain of the system-cost variable).
.cost_region_year <- function(scen) {
  tidyr::expand_grid(
    region = scen@modInp@sets[["region"]],
    year   = as.integer(scen@modInp@sets[["year"]])
  ) |> as.data.frame()
}

# mvTotalCost: total system cost domain = full region x year grid.
map_mvTotalCost <- function(scen, fmp) {
  .set_map(scen, "mvTotalCost", .cost_region_year(scen), fmp)
}

# mvTotalUserCosts: domain of user-defined cost constraints. For each user cost
# map (`mCosts*`), take its region/year footprint; collapse to the full grid when
# any cost spans it, otherwise union the per-cost footprints. Stays empty when the
# model declares no user costs (matching legacy).
map_mvTotalUserCosts <- function(scen, fmp) {
  dregionyear <- .cost_region_year(scen)
  cost_nms <- grep("^mCosts", names(scen@modInp@parameters), value = TRUE)
  footprints <- lapply(cost_nms, function(x) {
    xx <- get_data_slot(scen@modInp@parameters[[x]])
    if (is.null(xx) || nrow(xx) == 0) return(NULL)
    xx <- as.data.frame(xx) |>
      dplyr::select(dplyr::any_of(c("region", "year"))) |>
      dplyr::distinct()
    if (nrow(xx) == nrow(dregionyear) || ncol(xx) == 0) return(dregionyear)
    if (is.null(xx$region)) {
      return(dplyr::filter(dregionyear, .data$year %in% unique(xx$year)))
    } else if (is.null(xx$year)) {
      return(dplyr::filter(dregionyear, .data$region %in% unique(xx$region)))
    }
    xx
  })
  footprints <- Filter(Negate(is.null), footprints)
  df <- NULL
  if (any(vapply(footprints, nrow, integer(1)) == nrow(dregionyear))) {
    df <- dregionyear
  } else if (length(footprints) > 0) {
    df <- dplyr::distinct(dplyr::bind_rows(footprints))
  }
  if (!is.null(df) && nrow(df) > 0) {
    scen <- .set_map(scen, "mvTotalUserCosts", df, fmp)
  }
  scen
}

# Empty-legacy maps: feature not implemented in the current pipeline; the maps
# exist in modInp.yml but are intentionally never populated (faithful to legacy).
map_mvTradeCost    <- function(scen, fmp) scen
map_mvTradeRowCost <- function(scen, fmp) scen

# -- registry for the cost_agg family -------------------------------------- #
.cost_agg_builders <- list(
  mvTotalCost      = map_mvTotalCost,
  mvTotalUserCosts = map_mvTotalUserCosts,
  mvTradeCost      = map_mvTradeCost,
  mvTradeRowCost   = map_mvTradeRowCost
)
