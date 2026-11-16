# =============================================================================
# verify_aggregation(): what survived a coarsening, and what could not
# =============================================================================
# `aggregate_model_regions()` records WHAT it did in `@misc$aggregation`; this
# answers whether the result still says the same thing. The comparison is by
# class, slot and column rather than by object, because aggregation renames and
# merges objects: corridors become one coarse corridor, and the ones internal to
# a coarse region disappear. A per-object diff would read all of that as loss.
#
# The verdict depends on the column's own rule, the same table the aggregation
# used: a `sum` column must still total the same, while a rate, a price or an
# invariant must only stay inside the range of the values it came from -- a
# weighted mean cannot leave that range, so a value outside it is a defect.

#' @noRd
.va_totals <- function(mod) {
  objs <- .level_model_objects(mod)
  out <- list()
  counts <- table(vapply(objs, function(o) class(o)[1], character(1)))
  for (nm in names(objs)) {
    o <- objs[[nm]]
    cls <- class(o)[1]
    for (sl in .instance_slots(o)) {
      if (identical(sl, "misc")) next
      v <- tryCatch(methods::slot(o, sl), error = function(e) NULL)
      if (!is.data.frame(v) || !nrow(v)) next
      vals <- .agg_values(v)
      if (!length(vals)) next
      rules <- .agg_rules(vals, cls, sl)
      for (cn in vals) {
        x <- suppressWarnings(as.numeric(v[[cn]]))
        x <- x[is.finite(x)]
        if (!length(x)) next
        k <- paste(cls, sl, cn, sep = "|")
        p <- out[[k]]
        out[[k]] <- list(
          rule = unname(rules[[cn]]),
          total = (if (is.null(p)) 0 else p$total) + sum(x),
          min = min(c(if (is.null(p)) NULL else p$min, x)),
          max = max(c(if (is.null(p)) NULL else p$max, x)),
          n = (if (is.null(p)) 0L else p$n) + length(x))
      }
    }
  }
  list(cols = out, counts = counts)
}

#' Check what an aggregation conserved
#'
#' Compares a model with a coarser version of itself, column by column, and
#' says which quantities survived. The comparison is by class, slot and column,
#' not by object: aggregation merges corridors and drops the ones internal to a
#' coarse region, so a per-object diff would read that as loss.
#'
#' Each column is judged by its own aggregation rule, read from the same table
#' the aggregation used:
#'
#' * `sum` -- an extensive quantity (a stock, a capacity, a demand, an
#'   availability): its total must be unchanged, and any difference is
#'   reported with its relative size.
#' * `weighted_mean`, `mean`, `copy` -- a rate, a price or an invariant: a
#'   weighted mean cannot leave the range of the values it came from, so the
#'   verdict is whether the coarse range still sits inside the fine one.
#'
#' A `trade` row whose object count fell is reported as `by construction`
#' rather than as a difference: a corridor between two regions of one coarse
#' region is internal to it and is dropped, so its capacity is not expected to
#' survive. That is the aggregation's meaning, not an error -- which is exactly
#' why it is named rather than hidden.
#'
#' @param fine the model before aggregation.
#' @param coarse the model [aggregate_model_regions()] returned.
#' @param tol relative tolerance for an extensive total (default `1e-6`).
#'
#' @return A `data.frame` with one row per class, slot and column: the rule,
#'   the fine and coarse totals, the absolute and relative difference, the two
#'   ranges and a verdict. Printed as a short summary with the exceptions
#'   listed.
#' @family validation
#' @seealso [aggregate_model_regions()], [check_model_levels()],
#'   [verify_solution()]
#' @export
verify_aggregation <- function(fine, coarse, tol = 1e-6) {
  stopifnot(inherits(fine, "model"), inherits(coarse, "model"))
  a <- .va_totals(fine)
  b <- .va_totals(coarse)
  keys <- union(names(a$cols), names(b$cols))
  if (!length(keys)) {
    out <- data.frame()
    class(out) <- c("aggregation_check", "data.frame")
    return(out)
  }
  parts <- do.call(rbind, strsplit(keys, "|", fixed = TRUE))
  get <- function(side, k, f, d = NA_real_) {
    v <- side$cols[[k]]
    if (is.null(v)) d else v[[f]]
  }
  rule <- vapply(keys, function(k)
    get(a, k, "rule", NA) %||% get(b, k, "rule", NA), character(1),
    USE.NAMES = FALSE)
  fin <- vapply(keys, function(k) get(a, k, "total"), numeric(1),
                USE.NAMES = FALSE)
  crs <- vapply(keys, function(k) get(b, k, "total"), numeric(1),
                USE.NAMES = FALSE)
  out <- data.frame(
    class = parts[, 1], slot = parts[, 2], column = parts[, 3],
    rule = rule, fine = fin, coarse = crs,
    diff = crs - fin,
    rel = ifelse(abs(fin) > 0, (crs - fin) / abs(fin), NA_real_),
    fine_min = vapply(keys, function(k) get(a, k, "min"), numeric(1),
                      USE.NAMES = FALSE),
    fine_max = vapply(keys, function(k) get(a, k, "max"), numeric(1),
                      USE.NAMES = FALSE),
    coarse_min = vapply(keys, function(k) get(b, k, "min"), numeric(1),
                        USE.NAMES = FALSE),
    coarse_max = vapply(keys, function(k) get(b, k, "max"), numeric(1),
                        USE.NAMES = FALSE),
    stringsAsFactors = FALSE)
  # a class that lost objects cannot be expected to total the same
  shrunk <- vapply(out$class, function(cl) {
    na <- if (cl %in% names(a$counts)) a$counts[[cl]] else 0L
    nb <- if (cl %in% names(b$counts)) b$counts[[cl]] else 0L
    nb < na
  }, logical(1), USE.NAMES = FALSE)
  ext <- !is.na(out$rule) & out$rule == "sum"
  tol_ok <- abs(out$diff) <= tol * pmax(abs(out$fine), 1)
  inside <- (is.na(out$coarse_min) | is.na(out$fine_min)) |
    (out$coarse_min >= out$fine_min - tol * pmax(abs(out$fine_min), 1) &
       out$coarse_max <= out$fine_max + tol * pmax(abs(out$fine_max), 1))
  out$verdict <- ifelse(
    is.na(out$coarse) & shrunk, "by construction",
    ifelse(is.na(out$coarse), "dropped",
           ifelse(ext & tol_ok, "conserved",
                  ifelse(ext & shrunk, "by construction",
                         ifelse(ext, "differs",
                                ifelse(inside, "within range",
                                       "outside range"))))))
  # The total of a RATE is not a quantity -- summing prices says nothing -- so
  # only an extensive column reports one. An intensive column is judged on its
  # range, which is what the columns beside it carry.
  out$fine[!ext] <- NA_real_
  out$coarse[!ext] <- NA_real_
  out$diff[!ext] <- NA_real_
  out$rel[!ext] <- NA_real_
  out <- out[order(match(out$verdict,
                         c("differs", "outside range", "dropped",
                           "by construction", "conserved", "within range")),
                   out$class, out$slot, out$column), ]
  rownames(out) <- NULL
  class(out) <- c("aggregation_check", "data.frame")
  out
}

#' @method print aggregation_check
#' @export
print.aggregation_check <- function(x, ...) {
  if (!nrow(x)) {
    cat("Aggregation: nothing numeric to compare.\n")
    return(invisible(x))
  }
  tb <- table(x$verdict)
  cat("Aggregation: ",
      paste(names(tb), unname(tb), sep = " ", collapse = ", "),
      " (", nrow(x), " columns)\n", sep = "")
  flag <- x[x$verdict %in% c("differs", "outside range", "dropped"), ,
            drop = FALSE]
  if (nrow(flag)) {
    cat("\n")
    print(as.data.frame(unclass(flag)[c("class", "slot", "column", "rule",
                                        "fine", "coarse", "rel", "verdict")]),
          row.names = FALSE)
    cat("\nA `sum` that differs is a conservation defect; a rate outside the",
        "fine range\ncannot come from a weighted mean.\n")
  }
  byc <- x[x$verdict == "by construction", , drop = FALSE]
  if (nrow(byc)) {
    cat("\nNot conserved by construction (the class lost objects -- a ",
        "corridor internal\nto a coarse region is dropped): ",
        paste(unique(paste0(byc$class, "@", byc$slot, "$", byc$column)),
              collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}
