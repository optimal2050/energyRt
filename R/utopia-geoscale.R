# utopia-geoscale #############################################################
# Builds a `geoscales::Geoscale` for UTOPIA on demand.
#
# The hierarchy table ships as `utopia$geo` (a plain data.frame); the Geoscale
# is assembled here so that `data/` carries no class from a Suggests-only
# package.

#' @include geoscale.R utopia-data.R
NULL

#' A geoscale for the UTOPIA reference model
#'
#' Builds a [geoscales::Geoscale] over UTOPIA's eleven regions, nested
#' `nation -> zone -> region`, and attaches one of the reference map layouts.
#'
#' The hierarchy comes from `utopia$geo` and is keyed by region name, so it is
#' valid for every layout in `utopia$map` — the layouts place `R1`…`R11`
#' differently but share their names.
#'
#' @param layout Which layout in `utopia$map` to take geometry from:
#'   `"honeycomb"` (default, the one the vignettes draw), `"squares"`,
#'   `"island"` or `"continent"`. `NULL` builds the hierarchy with no geometry,
#'   which needs neither `sf` nor a map.
#' @param region Optional subset of regions to keep, e.g. `c("R1","R2","R3")`
#'   to match the three-region UTOPIA model.
#' @param area Add an `area` weight measured from the geometry. The layouts
#'   carry no CRS, so this is planar area in the coordinates' own units.
#'
#' @return A `geoscales::Geoscale`.
#'
#' @examples
#' \dontrun{
#' gs <- utopia_geoscale()
#' geoscales::geoscale_children(gs, "zone", "WEST")
#'
#' # the three-region model used in vignette("utopia-build")
#' gs3 <- utopia_geoscale(region = c("R1", "R2", "R3"))
#' mod <- newModel("UTOPIA", region = c("R1", "R2", "R3"), geoscale = gs3)
#' }
#'
#' @family geoscale
#' @family utopia
#' @export
utopia_geoscale <- function(layout = "honeycomb", region = NULL,
                            area = TRUE) {
  check_package("geoscales")
  # `utopia` is LazyData. Under `load_all()` it lands in the namespace, but in
  # an INSTALLED package it lives in the lazy-load database instead, so
  # `get(..., asNamespace())` finds nothing and the function fails only once
  # installed. Try the namespace, then fall back to the data database.
  utopia <- get0("utopia", envir = asNamespace("energyRt"), ifnotfound = NULL)
  if (is.null(utopia)) {
    .e <- new.env(parent = emptyenv())
    utils::data("utopia", package = "energyRt", envir = .e)
    utopia <- get("utopia", envir = .e)
  }

  geo <- utopia$geo
  if (!is.null(region)) {
    unknown <- setdiff(region, geo$region)
    if (length(unknown) > 0) {
      stop("Unknown UTOPIA region(s): ", paste(unknown, collapse = ", "),
           call. = FALSE)
    }
    geo <- geo[geo$region %in% region, , drop = FALSE]
  }

  gs <- geoscales::geoscale_from_leaftable(
    geo,
    geoframes = c("nation", "zone", "region"),
    key = "region",
    weights = character(),
    name = "utopia",
    desc = "UTOPIA reference regions, nested nation -> zone -> region",
    labels = "name"
  )

  if (is.null(layout)) return(gs)

  if (!layout %in% names(utopia$map)) {
    stop("Unknown layout '", layout, "'. Available: ",
         paste(names(utopia$map), collapse = ", "), call. = FALSE)
  }
  check_package("sf")
  gs <- geoscales::attach_geometry_geoscale(gs, utopia$map[[layout]],
                                            by = "region",
                                            geoframe = "region")
  if (isTRUE(area)) {
    # The reference layouts carry no CRS, so `add_area_geoscale()` measures planar area
    # and warns. That is the honest result for a synthetic map; suppress only
    # that one warning rather than let it fire on every call.
    withCallingHandlers(
      gs <- geoscales::add_area_geoscale(gs, name = "area"),
      warning = function(w) {
        if (grepl("no CRS", conditionMessage(w))) invokeRestart("muffleWarning")
      }
    )
  }
  gs
}
