# =============================================================================
# Deprecated names
# =============================================================================
# Thin aliases for renamed functions: each warns once per session through
# .en_deprecate_once() and forwards. Removed before 1.0.

#' @noRd
.gg_renamed <- function(old, new) {
  .en_deprecate_once(old, sprintf(
    "`%s()` was renamed `%s()`; the old name will be removed before 1.0.",
    old, new))
}

#' Deprecated plotting names
#'
#' The figure functions that return a ggplot object are named `gg_*`; the
#' former `plot_*` names forward to them with a one-time warning.
#' `plot_geoscale()` drew a model's geoscale and has no `gg_` successor in
#' energyRt: call [geoscales::gg_geoscale()] on `getGeoscale(object)`
#' instead, with `type = "map"`, `"stack"` or `"icicle"`.
#'
#' @param object,... Passed to the renamed function.
#' @param type,geoframe,view,direction,geoscale The former `plot_geoscale()`
#'   arguments.
#' @return What the renamed function returns.
#' @name energyRt-deprecated
#' @keywords internal
NULL

#' @rdname energyRt-deprecated
#' @export
plot_map <- function(object, ...) {
  .gg_renamed("plot_map", "gg_map")
  gg_map(object, ...)
}

#' @rdname energyRt-deprecated
#' @export
plot_heatmap <- function(object, ...) {
  .gg_renamed("plot_heatmap", "gg_heatmap")
  gg_heatmap(object, ...)
}

#' @rdname energyRt-deprecated
#' @export
plot_trade_map <- function(object, ...) {
  .gg_renamed("plot_trade_map", "gg_trade_map")
  gg_trade_map(object, ...)
}

#' @rdname energyRt-deprecated
#' @export
plot_trade_network <- function(object, ...) {
  .gg_renamed("plot_trade_network", "gg_trade_network")
  gg_trade_network(object, ...)
}

#' @rdname energyRt-deprecated
#' @export
plot_share_frontier <- function(object, ...) {
  .gg_renamed("plot_share_frontier", "gg_share_frontier")
  gg_share_frontier(object, ...)
}

#' @rdname energyRt-deprecated
#' @export
plot_geoscale <- function(object, type = c("map", "stack", "icicle"),
                          geoframe = NULL, view = "cabinet",
                          direction = "down", geoscale = NULL, ...) {
  .en_deprecate_once("plot_geoscale", paste0(
    "`plot_geoscale()` is deprecated: call `geoscales::gg_geoscale()` on ",
    "`getGeoscale(object)`."))
  type <- match.arg(type)
  check_package("geoscales")
  gs <- if (is_geoscale(object)) object else .resolve_geoscale(object, geoscale)
  switch(type,
    icicle = geoscales::gg_geoscale(gs, type = "icicle", ...),
    stack = geoscales::gg_geoscale(gs, type = "stack", view = view,
                                   direction = direction, ...),
    map = .gg_membership_map(gs, geoframe = geoframe, ...))
}
