# =========================================================================== #
# discount_rates.R  —  the model's two rates.
#
# A model has exactly TWO rates, and they do different jobs:
#
#   wacc  weighted average cost of capital. Annuitises investment into the
#         equivalent annual cost (`R/eac.R`). A financing rate, so it may differ
#         per process (`@invcost$wacc` overrides the model-wide value).
#   sdr   social discount rate. Discounts the stream of annual system costs in
#         the objective. A property of the model, never of a process.
#
# There is no third rate. `discount` survives as an ARGUMENT only — the
# shorthand for the simple/teaching/company case where one rate plays both
# roles — and is expanded into equal `wacc`/`sdr` before it ever reaches a slot.
# No class carries a `discount` column and no parameter carries a `discount`
# value, so there is no precedence chain for a reader to get wrong.
#
# Per row a user supplies EITHER `discount` OR both of `wacc`/`sdr` — never a
# partial pair (which would silently zero one of the two jobs) and never both
# forms (which would be ambiguous). Both are errors.
# =========================================================================== #

# Columns of `config@discount` that hold a rate.
.RATE_COLS <- c("wacc", "sdr")

# Row-wise "was this supplied?" for a column that may be absent entirely.
.rate_given <- function(d, col) {
  if (!col %in% names(d)) return(rep(FALSE, nrow(d)))
  !is.na(suppressWarnings(as.numeric(d[[col]])))
}

# Describe offending rows compactly for an error message.
.rate_rows <- function(d, i) {
  keys <- intersect(c("region", "year"), names(d))
  if (length(keys) == 0) {
    return(paste0("row(s) ", paste(which(i), collapse = ", ")))
  }
  parts <- lapply(keys, function(k) {
    v <- as.character(d[[k]][i])
    v[is.na(v)] <- "<any>"
    paste0(k, " ", v)
  })
  paste(unique(do.call(paste, c(parts, list(sep = ", ")))), collapse = "; ")
}

#' Translate a `discount` argument into a table of `wacc`/`sdr` rates
#'
#' `discount` is an argument, not a slot column. A bare number (or a table with
#' a `discount` column) means "one rate for both jobs" and is expanded into
#' equal `wacc` and `sdr`; a table may instead give `wacc` and `sdr` explicitly,
#' by region and year. Mixing the two forms in one row, or giving only one half
#' of the pair, is an error.
#'
#' @param x a number, or a data.frame with `region`/`year` and either `discount`
#'   or both `wacc` and `sdr`.
#' @return a data.frame with `wacc` and `sdr` populated on every row.
#' @keywords internal
.discount_arg_to_rates <- function(x) {
  if (is.null(x)) return(NULL)

  # Bare number(s): one rate doing both jobs.
  if (!is.data.frame(x) && !is.list(x)) {
    if (!is.numeric(x)) {
      stop("`discount` must be a number or a data.frame, not ", class(x)[1], ".")
    }
    return(data.frame(wacc = as.numeric(x), sdr = as.numeric(x),
                      stringsAsFactors = FALSE))
  }

  d <- as.data.frame(x, stringsAsFactors = FALSE)
  if (nrow(d) == 0) return(d[, setdiff(names(d), "discount"), drop = FALSE])

  has_d <- .rate_given(d, "discount")
  has_w <- .rate_given(d, "wacc")
  has_s <- .rate_given(d, "sdr")

  if (!any(has_d | has_w | has_s)) {
    stop("`discount` table has no rates: supply `discount`, or `wacc` and `sdr`.")
  }

  both <- has_d & (has_w | has_s)
  if (any(both)) {
    stop("`discount` cannot be combined with `wacc`/`sdr` in the same row (",
         .rate_rows(d, both), ").\n",
         "  `discount` is the shorthand for `wacc` = `sdr` = that value. ",
         "Supply either `discount`, or both `wacc` and `sdr`.")
  }

  half <- !has_d & (has_w != has_s)
  if (any(half)) {
    missed <- if (has_w[half][1]) "sdr" else "wacc"
    stop("`wacc` and `sdr` must both be supplied (", .rate_rows(d, half),
         " is missing `", missed, "`).\n",
         "  `wacc` annuitises investment; `sdr` discounts the objective. ",
         "Use `discount` if one rate should do both.")
  }

  for (col in .RATE_COLS) {
    if (!col %in% names(d)) d[[col]] <- NA_real_
    d[[col]] <- as.numeric(d[[col]])
    d[[col]][has_d] <- as.numeric(d$discount[has_d])
  }
  d[, setdiff(names(d), "discount"), drop = FALSE]
}

# Rewrite a `discount` argument in place in an argument list, so that
# `.data2slots()` only ever sees `wacc`/`sdr` columns.
.discount_args <- function(args) {
  if (!"discount" %in% names(args)) return(args)
  args[["discount"]] <- .discount_arg_to_rates(args[["discount"]])
  args
}

# Model-wide rate accessors. Both columns are guaranteed populated once the
# argument has been through `.discount_arg_to_rates()`, so these are plain
# lookups with no fallback logic.
.model_wacc <- function(cfg) .rate_column(cfg, "wacc")
.model_sdr  <- function(cfg) .rate_column(cfg, "sdr")

.rate_column <- function(cfg, col) {
  d <- tryCatch(cfg@discount, error = function(e) NULL)
  if (!is.data.frame(d) || nrow(d) == 0 || !col %in% names(d)) return(numeric())
  v <- suppressWarnings(as.numeric(d[[col]]))
  v[!is.na(v)]
}
