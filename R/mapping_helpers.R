# =========================================================================== #
# mapping_helpers.R
#
# Shared primitives for the per-mapping builder functions (R/map_<family>.R).
# These are the reusable pieces a `map_<Name>(scen, fmp)` function calls; the
# per-mapping function supplies the variation (source parameter, window, bound
# types, ...) as plain arguments instead of a spec/def-table row.
#
# (Other primitives — `.set_map`, `merge0`, `get_data_slot`, `d2p`,
# `.reduce_*`, `.io_row_maps`, `apply_to_scenario_data` — currently live in
# mapping_engine.R / obj2modInp*.R and are reachable in the package namespace;
# they migrate here as the recipes they belong to are retired.)
# =========================================================================== #

# .gds(): read an already-built map/parameter from the scenario as a plain
# data.frame, NULL when absent or empty. The per-mapping builders use this to read
# the dependency maps they intersect/aggregate (every intermediate is a persisted
# parameter, so build order is the only requirement).
.gds <- function(scen, nm) {
  p <- scen@modInp@parameters[[nm]]
  if (is.null(p)) return(NULL)
  d <- get_data_slot(p)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  as.data.frame(d)
}

# .filt_cr(): restrict commodity flows to feasible (commodity, region) pairs via
# the commodity-region closure map mCommReg. Empty mCommReg -> empty result (no
# commodity is reachable). `region_col` names the region column when it is not
# literally "region" (e.g. trade `src`/`dst`).
.filt_cr <- function(scen, df, region_col = "region") {
  if (is.null(df) || nrow(df) == 0) return(df)
  df <- as.data.frame(df)
  cr <- .gds(scen, "mCommReg")
  if (is.null(cr) || nrow(cr) == 0) return(df[0, , drop = FALSE])
  by <- if (region_col == "region") {
    c("comm", "region")
  } else {
    c("comm", stats::setNames("region", region_col))
  }
  dplyr::semi_join(df, cr, by = by)
}

# value_on_window(): domain of a derived "value" map = the points where one or
# more interpolated p* parameters carry a defined (non-NA) value, projected onto
# the map's dimensions and optionally intersected with a lifespan window map.
#
# Domain membership is STRUCTURAL: a defined value belongs to the domain even when
# it equals the parameter default (e.g. a zero cost). Only NA values carry no
# domain information and are dropped. (Port of the former `.build_value_map_std`.)
#
# @param source character vector of source p* parameter names.
# @param window optional lifespan window map name to intersect with (NULL = none).
# @param gate   optional scenario settings flag that must be TRUE to build the map.
value_on_window <- function(scen, name, source, window = NULL, gate = NULL, fmp) {
  p <- scen@modInp@parameters[[name]]
  if (is.null(p)) return(scen)
  if (!is.null(gate) && !isTRUE(slot(scen@settings, gate))) return(scen)
  dims <- p@dimSets

  src <- NULL
  for (sp in source) {
    sp_par <- scen@modInp@parameters[[sp]]
    if (is.null(sp_par)) next
    sd <- get_data_slot(sp_par)
    if (is.null(sd) || nrow(sd) == 0) next
    sd <- as.data.frame(sd)
    if (!is.null(sd$value)) sd <- sd[!is.na(sd$value), , drop = FALSE]
    if (nrow(sd) == 0) next
    sd <- sd |> dplyr::select(dplyr::any_of(dims)) |> dplyr::distinct()
    src <- dplyr::bind_rows(src, sd)
  }
  if (is.null(src) || nrow(src) == 0) return(scen)
  src <- dplyr::distinct(src)

  if (!is.null(window)) {
    win_par <- scen@modInp@parameters[[window]]
    win <- if (is.null(win_par)) NULL else get_data_slot(win_par)
    if (is.null(win) || nrow(win) == 0) return(scen)
    df <- merge0(as.data.frame(win), src)
  } else {
    df <- src
  }
  .set_map(scen, name, df, fmp)
}

# weather_map(): set of (weather, entity[, comm]) keys carrying a weather-dependent
# availability bound, from an interpolated bounds parameter, selecting the relevant
# bound `types` and projecting onto the map's dimensions. (Port of the former
# `.build_weather_map`; "Up" maps take up/fx, "Lo" maps take lo/fx.)
weather_map <- function(scen, name, source, types, fmp) {
  p <- scen@modInp@parameters[[name]]
  if (is.null(p)) return(scen)
  sp <- scen@modInp@parameters[[source]]
  if (is.null(sp)) return(scen)
  sd <- get_data_slot(sp)
  if (is.null(sd) || nrow(sd) == 0) return(scen)
  sd <- as.data.frame(sd)
  if (!is.null(sd$type)) sd <- sd[sd$type %in% types, , drop = FALSE]
  if (nrow(sd) == 0) return(scen)
  df <- dplyr::distinct(dplyr::select(sd, dplyr::any_of(p@dimSets)))
  .set_map(scen, name, df, fmp)
}
