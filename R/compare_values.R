# =========================================================================== #
# compare_values.R -- the tolerance-aware value-diff kernel shared by the
# comparison layer (compare_scenarios / compare_inputs) and the golden tests
# (tests/testthat/helper-goldens.R). Internal: exported comparison functions
# wrap it with classed results.
# =========================================================================== #

# Full-join comparison of two keyed value tables.
#
# `base` and `other` are data.frames with identical key columns plus one
# `value` column. Rows are matched on the keys; a row differs when
#   abs(value - base) > tol_abs + tol_rel * pmax(abs(value), abs(base))
# -- the golden-test formula (helper-goldens.R), so test tolerances and
# comparison-layer tolerances mean the same thing.
#
# `missing` controls one-sided rows (present on only one side):
#   "fill" -- treat the absent side as `fill` (default 0; the golden
#             behavior: a missing row is a zero value) and tolerance-test it;
#   "flag" -- keep the absent side NA, add a `side` column
#             ("both"/"base"/"other"), and exclude one-sided rows from the
#             tolerance test (`differs` = FALSE for them) -- callers report
#             them separately (e.g. compare_inputs' rows-mismatch status).
#
# Returns a data.frame: <keys...>, base, value, diff, rel_diff, differs
# (+ side when missing = "flag"). A key-column mismatch is an error naming
# both column sets; callers catch it and record a "structural" status.
#' @noRd
.compare_values <- function(base, other, by = NULL,
                            tol_rel = 1e-6, tol_abs = 1e-6,
                            missing = c("fill", "flag"), fill = 0) {
  missing <- match.arg(missing)
  stopifnot(is.data.frame(base), is.data.frame(other),
            "value" %in% names(base), "value" %in% names(other))
  base <- as.data.frame(base)
  other <- as.data.frame(other)
  kb <- setdiff(names(base), "value")
  ko <- setdiff(names(other), "value")
  if (is.null(by)) {
    if (!setequal(kb, ko)) {
      stop("key columns differ: base has {", paste(kb, collapse = ", "),
           "}, other has {", paste(ko, collapse = ", "), "}")
    }
    by <- kb
  } else if (!all(by %in% kb) || !all(by %in% ko)) {
    stop("`by` columns missing from a side: ",
         paste(setdiff(by, intersect(kb, ko)), collapse = ", "))
  }
  names(base)[names(base) == "value"] <- "base"
  b <- base[, c(by, "base"), drop = FALSE]
  o <- other[, c(by, "value"), drop = FALSE]
  m <- if (length(by) == 0) {
    cbind(b, o)                      # scalar tables: one row each
  } else {
    merge(b, o, by = by, all = TRUE)
  }
  side <- ifelse(is.na(m$base), "other",
                 ifelse(is.na(m$value), "base", "both"))
  if (missing == "fill") {
    m$base[is.na(m$base)] <- fill
    m$value[is.na(m$value)] <- fill
  }
  m$diff <- m$value - m$base
  m$rel_diff <- m$diff / pmax(abs(m$base), abs(m$value), tol_abs)
  m$differs <- !is.na(m$diff) &
    abs(m$diff) > tol_abs + tol_rel * pmax(abs(m$value), abs(m$base))
  if (missing == "flag") m$side <- side
  rownames(m) <- NULL
  m
}

# One solved variable of a scenario, aggregated to a comparison level.
#
# level "annual_region": summed over timeslices (remaining dims + region +
# year kept); "annual_total": additionally summed over regions -- refused
# (returns NULL) for variables with role "interregional", which must never
# be summed spatially (see the goldens schema note in helper-goldens.R).
# Returns a data.frame of the grouping dims + `value`, or NULL when the
# variable is absent/empty.
#' @noRd
.cmp_var_table <- function(scen, name,
                           level = c("annual_region", "annual_total")) {
  level <- match.arg(level)
  v <- tryCatch(scen@modOut@variables[[name]], error = function(e) NULL)
  if (is.null(v)) return(NULL)
  if (level == "annual_total" && identical(v@role, "interregional")) {
    return(NULL)
  }
  d <- tryCatch(
    as.data.frame(getData(scen, name = name, merge = TRUE,
                          drop.zeros = FALSE)),
    error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0 || !"value" %in% names(d)) return(NULL)
  drop_cols <- c("value", "name", "scenario", "timeslice",
                 if (level == "annual_total") "region")
  grp <- setdiff(names(d), drop_cols)
  if (length(grp) == 0) {
    return(data.frame(value = sum(d$value, na.rm = TRUE)))
  }
  out <- stats::aggregate(d["value"], by = d[grp], FUN = function(z)
    sum(z, na.rm = TRUE))
  out <- out[do.call(order, out[grp]), , drop = FALSE]
  rownames(out) <- NULL
  out
}
