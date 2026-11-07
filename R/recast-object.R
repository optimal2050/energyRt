# =========================================================================== #
# Recasting ONE object onto another scale.
#
# Aggregation is an object operation. A model-level verb that loops slots and
# reaches into other objects for a weight makes the result depend on what else
# the model holds and on the order it was assembled; a method that sees only
# the object and the scale cannot.
#
# The scale's KEY names the column being moved -- "region" for a GeoScale,
# "timeslice" for a Calendar -- so space and time are one method, not two.
# =========================================================================== #

#' @include recast.R
NULL

#' Move one object onto another scale
#'
#' @param object A modelling object (`technology`, `storage`, `weather`, ...).
#' @param scale The scale to move along: a `GeoScale` or a `Calendar`. Its key
#'   names the column that is rewritten.
#' @param target Name of the frame to move to.
#' @param source Frame the object is currently expressed at. `NULL` (default)
#'   takes the scale's finest.
#' @param as Which route to take. `"clusters"` (default) and `"objects"`
#'   RELABEL: the former units survive as clusters of one object, or as
#'   separate objects, and nothing is recomputed. `"merge"` and `"split"`
#'   RECOMPUTE values onto the coarser or finer scale, and are the only routes
#'   that consult `rules`, `weight` and `resolve_conflicts`.
#' @param rules Named vector overriding the declared rule for a column.
#' @param weight Weight for the intensive rules. `NULL` takes the object's own
#'   size where it carries one, and the scale's declared weight where it does
#'   not.
#' @param regions The member set an `NA` in a key column stands for. `NULL`
#'   takes the scale's finest members.
#' @param suffix For `as = "objects"`, appended to each minted object's name.
#' @param resolve_conflicts What to do with a value that has no merged form:
#'   `FALSE` stops, `"force"` takes the mean, `"drop"` leaves it unset.
#' @returns The object on the new scale, or for `as = "objects"` a list of them.
#' @keywords internal
#' @noRd
setGeneric("recast_object", function(object, scale, target, source = NULL,
                                     as = c("clusters", "objects",
                                            "merge", "split"),
                                     rules = NULL, weight = NULL,
                                     regions = NULL, suffix = NULL,
                                     resolve_conflicts = FALSE)
  standardGeneric("recast_object"))

#' @noRd
setMethod(
  "recast_object", "ANY",
  function(object, scale, target, source = NULL,
           as = c("clusters", "objects", "merge", "split"),
           rules = NULL, weight = NULL, regions = NULL, suffix = NULL,
           resolve_conflicts = FALSE) {
    as <- match.arg(as)
    if (!identical(as, "merge")) {
      stop("`as = \"", as, "\"` is not built yet; only \"merge\" is.",
           call. = FALSE)
    }
    if (is.null(regions)) {
      regions <- as.character(geoscales::geoscale_regions(scale))
    }
    cl1 <- class(object)[1]
    w <- if (is.null(weight)) .agg_weight(object) else weight
    cfs <- list()
    for (sl in methods::slotNames(object)) {
      v <- methods::slot(object, sl)
      if (!is.data.frame(v) || !nrow(v) || !"region" %in% names(v)) next
      v2 <- .agg_slot(v, scale, target, w, regions, cls = cl1, slot = sl,
                      resolve = resolve_conflicts,
                      what = paste0(cl1, " '", object@name, "' @", sl))
      cf <- attr(v2, "agg_conflicts")
      attr(v2, "agg_conflicts") <- NULL
      if (!is.null(cf)) {
        cf$slot <- sl
        cfs[[length(cfs) + 1L]] <- cf
      }
      methods::slot(object, sl) <- v2
    }
    object <- .agg_record(object, target, cfs)
    if (.hasSlot(object, "region")) {
      r <- methods::slot(object, "region")
      if (is.character(r) && length(r)) {
        methods::slot(object, "region") <-
          unique(stats::na.omit(.agg_map_codes(r, scale, target)))
      }
    }
    object
  })
