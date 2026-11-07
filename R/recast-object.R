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
    if (identical(as, "clusters")) {
      return(.recast_to_clusters(object, scale, target))
    }
    if (!identical(as, "merge")) {
      stop("`as = \"", as, "\"` is not built yet; \"clusters\" and ",
           "\"merge\" are.", call. = FALSE)
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

#' Remap the key columns of a summand's `mult`, keeping its coefficients
#'
#' A coefficient that differs across the regions being merged has no merged
#' form: the constraint reads `sum(m[r] * x[r])`, so the merged coefficient
#' would have to be `m` weighted by the variable itself, which is not known
#' before the solve. Equal coefficients pass through; a difference stops.
#' @noRd
.agg_summand_mult <- function(d, gs, level, resolve = "stop", what = "") {
  keys <- intersect(c("region", "src", "dst"), names(d))
  if (!length(keys) || !nrow(d)) return(d)
  for (k in keys) d[[k]] <- .agg_map_codes(d[[k]], gs, level)
  d <- d[!is.na(d[[keys[1]]]), , drop = FALSE]
  if (!nrow(d) || !"value" %in% names(d)) return(d)
  g <- interaction(d[keys], drop = TRUE)
  bad <- names(which(vapply(split(d$value, g),
                            function(z) length(unique(z)) > 1L, logical(1))))
  if (length(bad)) {
    msg <- paste0(what, ": the coefficient differs across the regions merged ",
                  "into ", paste(bad, collapse = ", "))
    if (identical(resolve, "stop")) {
      stop(msg, ".\nA merged coefficient would have to be weighted by the ",
           "variable it multiplies, which is not known before the solve. ",
           "Declare one coefficient for the group, or pass ",
           "`resolve_conflicts`.", call. = FALSE)
    }
    warning(msg, ".\nResolved by taking the mean.", call. = FALSE)
  }
  out <- stats::aggregate(list(value = d$value), by = as.list(d[keys]),
                          FUN = function(z) mean(z, na.rm = TRUE))
  out[, c(keys, "value"), drop = FALSE]
}

#' @noRd
setMethod(
  "recast_object", "constraint",
  function(object, scale, target, source = NULL,
           as = c("clusters", "objects", "merge", "split"),
           rules = NULL, weight = NULL, regions = NULL, suffix = NULL,
           resolve_conflicts = FALSE) {
    as <- match.arg(as)
    # `@for.each` and `@rhs` are data.frames the default method already moves
    object <- callNextMethod()
    mode <- .agg_resolve_mode(resolve_conflicts)
    # `@lhs` is a LIST, so no slot loop reaches it: a term's coefficients and
    # the regions it sums over would otherwise survive naming regions that
    # have been merged away.
    for (i in seq_along(object@lhs)) {
      s <- object@lhs[[i]]
      if (is.data.frame(s@mult) && nrow(s@mult)) {
        s@mult <- .agg_summand_mult(
          s@mult, scale, target, mode,
          paste0("constraint '", object@name, "' term ", i))
      }
      for (d in intersect(names(s@for.sum), c("region", "src", "dst"))) {
        v <- s@for.sum[[d]]
        if (is.character(v) && length(v)) {
          s@for.sum[[d]] <- unique(stats::na.omit(.agg_map_codes(v, scale, target)))
        }
      }
      object@lhs[[i]] <- s
    }
    object
  })

#' Relabel an object onto a coarser frame, keeping every unit as a cluster
#'
#' The relabel route: nothing is recomputed and nothing is lost. Each former
#' unit becomes a cluster of one object at `target`, which is what a resource
#' tier is for -- grades that must not pool. A profile with no size of its own
#' has no defensible weighted mean, so this is usually the honest route for
#' `weather`.
#' @noRd
.recast_to_clusters <- function(object, gs, level) {
  if (.hasSlot(object, "cluster")) {
    cd <- methods::slot(object, "cluster")
    if (is.data.frame(cd) && nrow(cd)) {
      stop(class(object)[1], " '", object@name, "' already declares clusters; ",
           "composing them with a region relabel is not built yet.",
           call. = FALSE)
    }
  }
  seen <- character()
  for (sl in methods::slotNames(object)) {
    v <- methods::slot(object, sl)
    if (!is.data.frame(v) || !nrow(v) || !"region" %in% names(v)) next
    if (identical(sl, "cluster")) next
    if (!"cluster" %in% names(v)) {
      stop(class(object)[1], " '", object@name, "' @", sl, " is keyed by ",
           "region but cannot carry a cluster, so relabelling would lose the ",
           "distinction between the regions merged.", call. = FALSE)
    }
    keep <- !is.na(v$region)
    # an NA region is the wildcard "every region": it stays one row, applying
    # to every cluster, and must not become a cluster of its own
    v$cluster[keep & is.na(v$cluster)] <- as.character(v$region[keep & is.na(v$cluster)])
    seen <- c(seen, as.character(v$region[keep]))
    v$region[keep] <- .agg_map_codes(v$region[keep], gs, level)
    methods::slot(object, sl) <- v
  }
  seen <- unique(stats::na.omit(seen))
  if (.hasSlot(object, "cluster") && length(seen)) {
    proto <- methods::slot(methods::new(class(object)[1]), "cluster")
    cd <- proto[rep(NA_integer_, length(seen)), , drop = FALSE]
    rownames(cd) <- NULL
    cd$cluster <- seen
    if ("desc" %in% names(cd)) cd$desc <- paste0("was region ", seen)
    if ("region" %in% names(cd)) {
      cd$region <- .agg_map_codes(seen, gs, level)
    }
    methods::slot(object, "cluster") <- cd
  }
  if (.hasSlot(object, "region")) {
    r <- methods::slot(object, "region")
    if (is.character(r) && length(r)) {
      methods::slot(object, "region") <-
        unique(stats::na.omit(.agg_map_codes(r, gs, level)))
    }
  }
  .agg_record(object, level, list())
}
