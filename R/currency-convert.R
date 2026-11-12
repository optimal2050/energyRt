## Year-tagged money: the exchange-rate and deflator lookups, and the factor
## `convert()` applies on top of its own coefficient ratio.
##
## `convert()` computes `coef_from / coef_to`, one scalar per unit. A currency
## coefficient is its rate at the table's base year, so an untagged conversion
## is a pure exchange rate and nothing here runs. A YEAR, though, cannot live in
## a per-unit scalar: moving money from one currency-year to another needs a
## pivot -- the year whose exchange rate is used -- and the pivot is a property
## of the PAIR, not of either side. So the correction is pairwise:
##
##   cf = [FX_A->B(p) / FX_A->B(base)] * D_A(p)/D_A(yA) * D_B(yB)/D_B(p)
##
## which multiplies the existing ratio to give
##
##   (scale_A/scale_B) * FX_A->B(p) * D_A(p)/D_A(yA) * D_B(yB)/D_B(p)
##
## The two routes are the two pivots worth choosing: `p = yB` deflates in the
## source currency and converts at the target year (the World Bank and IPCC
## convention, and the default here), `p = yA` converts first and deflates in
## the target currency. They disagree by exactly the change in the real
## exchange rate over the interval, which is zero only under purchasing power
## parity -- so neither is a rounding of the other and the gap is reported.
##
## It collapses the way it must:
##   * one currency, two years   -> D_A(yB)/D_A(yA), the same on both routes
##   * two currencies, one year  -> FX at that year, the same on both routes
##   * nothing tagged            -> exactly 1, so untagged answers are untouched

#' @include currency-data.R
NULL

# User additions, keyed the same way the datasets are. Package-local rather
# than in `.GlobalEnv`: `add_to_convert()` assigns to the global environment
# and a stray `convert_data` there silently replaces the registry, which is a
# defect this does not copy.
.currency_env <- new.env(parent = emptyenv())

.CURRENCY_KEYS <- list(fx = c("code", "year"),
                       deflator = c("code", "basis", "year"),
                       codes = "spelling")

# The shipped table with any user rows laid over it. A user row replaces the
# shipped row with the same key rather than adding a duplicate.
#' @noRd
.currency_data <- function(what = c("fx", "deflator", "codes")) {
  what <- match.arg(what)
  base <- switch(what, fx = currency_fx, deflator = currency_deflator,
                 codes = currency_codes)
  add <- .currency_env[[what]]
  if (is.null(add) || !nrow(add)) return(base)
  k <- .CURRENCY_KEYS[[what]]
  bk <- do.call(paste, c(base[, k, drop = FALSE], sep = "\r"))
  ak <- do.call(paste, c(add[, k, drop = FALSE], sep = "\r"))
  rbind(base[!(bk %in% ak), , drop = FALSE], add[, names(base), drop = FALSE])
}

# The year `convert_data$base$Currency`'s coefficients encode. A fact about the
# shipped data, not a preference: the pairwise factor divides it out, so a
# coefficient generated at one year can serve a base year set to another.
#' @noRd
.currency_generated_at <- function() {
  as.integer(attr(currency_fx, "base_year"))
}

#' The year untagged money is read at
#'
#' Money without a year is read at this year -- the latest the shipped exchange
#' rate table covers for a quorum of currencies, and the year
#' `convert_data$base$Currency` is generated from. Set it to pin conversions to
#' a different year.
#'
#' @param year a four-digit year, or `NULL` to read the current setting.
#' @return The base year, invisibly when setting.
#' @seealso [currency_fx], [convert()]
#' @export
#' @examples
#' currency_base_year()
currency_base_year <- function(year = NULL) {
  if (is.null(year)) {
    y <- .currency_env$base_year
    if (!is.null(y)) return(as.integer(y))
    return(.currency_generated_at())
  }
  y <- suppressWarnings(as.integer(year))
  if (length(y) != 1L || is.na(y)) {
    stop("`year` must be one four-digit year", call. = FALSE)
  }
  .currency_env$base_year <- y
  invisible(y)
}

#' Add an exchange rate or a price index
#'
#' The shipped tables stop at the year they were built from. These add a row,
#' or replace one, for the current session -- a rate newer than the release, a
#' currency the tables do not carry, or a deflator basis of your own.
#'
#' @param code ISO 4217 currency code.
#' @param year calendar year.
#' @param lcu_per_usd units of the currency per US dollar. Note the direction:
#'   it is what the source series publishes, and the reciprocal of
#'   `convert_data`'s coefficients. Adding one for a year the tables do not
#'   reach also adds the numeraire's own row for that year, which is 1 by
#'   definition, so a cross rate has both of its sides.
#' @param index a price index. Only ratios between years are used, so its base
#'   does not matter, but it must share a base with the other years of the same
#'   currency and basis.
#' @param basis `"gdp"` or `"cpi"`, or a name of your own.
#' @param area the area the index is for, recorded for provenance.
#' @param source free text recording where the number came from.
#' @return Invisibly, the added rows.
#' @rdname add_currency
#' @export
#' @examples
#' add_currency_fx("EUR", 2026, 0.92, source = "my own estimate")
#' convert("MEUR2026", "MUSD2026")
add_currency_fx <- function(code, year, lcu_per_usd, source = "user") {
  d <- data.frame(code = as.character(code), year = as.integer(year),
                  lcu_per_usd = as.numeric(lcu_per_usd),
                  source = as.character(source), stringsAsFactors = FALSE)
  if (any(!is.finite(d$lcu_per_usd) | d$lcu_per_usd <= 0)) {
    stop("`lcu_per_usd` must be positive", call. = FALSE)
  }
  .currency_env$fx <- .merge_rows(.currency_env$fx, d, .CURRENCY_KEYS$fx)
  # A cross rate needs both sides, and the numeraire is 1 in every year by
  # definition rather than by measurement -- so adding next year's euro rate
  # is enough on its own. Without this, every use of this function for the
  # common case (a rate newer than the release) needs a line of boilerplate
  # saying that a dollar is worth a dollar.
  num <- d$year[d$code != "USD"]
  num <- num[!(num %in% .currency_data("fx")$year[
    .currency_data("fx")$code == "USD"])]
  if (length(num)) {
    .currency_env$fx <- .merge_rows(
      .currency_env$fx,
      data.frame(code = "USD", year = unique(num), lcu_per_usd = 1,
                 source = "numeraire", stringsAsFactors = FALSE),
      .CURRENCY_KEYS$fx)
  }
  invisible(d)
}

#' @rdname add_currency
#' @export
add_currency_deflator <- function(code, year, index, basis = "gdp",
                                  area = NA_character_, source = "user") {
  d <- data.frame(code = as.character(code), area = as.character(area),
                  year = as.integer(year), basis = as.character(basis),
                  index = as.numeric(index), source = as.character(source),
                  stringsAsFactors = FALSE)
  if (any(!is.finite(d$index) | d$index <= 0)) {
    stop("`index` must be positive", call. = FALSE)
  }
  .currency_env$deflator <- .merge_rows(.currency_env$deflator, d,
                                        .CURRENCY_KEYS$deflator)
  invisible(d)
}

#' @noRd
.merge_rows <- function(old, new, k) {
  if (is.null(old) || !nrow(old)) return(new)
  ok <- do.call(paste, c(old[, k, drop = FALSE], sep = "\r"))
  nk <- do.call(paste, c(new[, k, drop = FALSE], sep = "\r"))
  rbind(old[!(ok %in% nk), , drop = FALSE], new)
}

# Clear session additions. Used by the tests; not exported.
#' @noRd
.currency_reset <- function() {
  rm(list = ls(.currency_env), envir = .currency_env)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Provenance
# ---------------------------------------------------------------------------

# `normalize_model_currency()` has to report WHICH rows a factor was built
# from. The lookups are the only thing that knows, so rather than re-deriving
# them from the formula -- two implementations of one rule, free to drift --
# each lookup records the row it returned while a trace is open. The trace is
# opened around the route actually taken, not around `.currency_factor()`,
# which evaluates both.
#' @noRd
.currency_trace_begin <- function() {
  .currency_env$trace <- list(fx = list(), deflator = list())
  invisible(NULL)
}

#' @noRd
.currency_trace_row <- function(what, row) {
  tr <- .currency_env$trace
  if (is.null(tr)) return(invisible(NULL))
  k <- paste(unlist(row[1, .CURRENCY_KEYS[[what]]]), collapse = "|")
  tr[[what]][[k]] <- row
  .currency_env$trace <- tr
  invisible(NULL)
}

# The rows consulted since the trace opened, one data.frame per table, and the
# trace closed. NULL for a table nothing read: a same-currency factor needs no
# exchange rate and a same-year one needs no deflator.
#' @noRd
.currency_trace_end <- function() {
  tr <- .currency_env$trace
  .currency_env$trace <- NULL
  if (is.null(tr)) return(NULL)
  lapply(tr, function(x) {
    if (!length(x)) return(NULL)
    out <- do.call(rbind, x)
    out <- out[order(out$code, out$year), , drop = FALSE]
    row.names(out) <- NULL
    out
  })
}

# ---------------------------------------------------------------------------
# Lookups
# ---------------------------------------------------------------------------

# Units of `code` per US dollar in `year`. Errors rather than extrapolating:
# a euro in 1995 is a question with no answer, not one to guess at.
#' @noRd
.fx_rate <- function(code, year) {
  d <- .currency_data("fx")
  i <- which(d$code == code & d$year == year)
  if (!length(i)) {
    yy <- d$year[d$code == code]
    stop("no exchange rate for ", code, " in ", year,
         if (length(yy)) paste0(" (have ", min(yy), "-", max(yy), ")")
         else " (currency not in the table)",
         "\nAdd one with add_currency_fx().", call. = FALSE)
  }
  .currency_trace_row("fx", d[i[1], , drop = FALSE])
  d$lcu_per_usd[i[1]]
}

#' @noRd
.deflator <- function(code, year, basis) {
  d <- .currency_data("deflator")
  i <- which(d$code == code & d$year == year & d$basis == basis)
  if (!length(i)) {
    yy <- d$year[d$code == code & d$basis == basis]
    stop("no ", basis, " deflator for ", code, " in ", year,
         if (length(yy)) paste0(" (have ", min(yy), "-", max(yy), ")")
         else paste0(" (no ", basis, " series for ", code, ")"),
         "\nAdd one with add_currency_deflator().", call. = FALSE)
  }
  .currency_trace_row("deflator", d[i[1], , drop = FALSE])
  d$index[i[1]]
}

# Units of `b` per unit of `a` in `year`.
#' @noRd
.fx_cross <- function(a, b, year) {
  if (identical(a, b)) return(1)
  .fx_rate(b, year) / .fx_rate(a, year)
}

# ---------------------------------------------------------------------------
# The factor
# ---------------------------------------------------------------------------

.CURRENCY_ROUTES <- c("deflate_then_fx", "fx_then_deflate")

#' @noRd
.currency_one_route <- function(a, b, ya, yb, route, basis, gen) {
  p <- if (identical(route, "fx_then_deflate")) ya else yb
  # `gen` divides out the rate already baked into the unit coefficients, so
  # what remains is the rate at the pivot.
  fx <- .fx_cross(a, b, p) / .fx_cross(a, b, gen)
  da <- if (identical(ya, p)) 1 else
    .deflator(a, p, basis) / .deflator(a, ya, basis)
  db <- if (identical(yb, p)) 1 else
    .deflator(b, yb, basis) / .deflator(b, p, basis)
  fx * da * db
}

# The correction `convert()` multiplies its coefficient ratio by. Returns 1
# for anything that is not a year-tagged currency conversion, which is what
# keeps every existing answer intact.
#' @noRd
.currency_factor <- function(h_from, h_to, currency_route = "deflate_then_fx",
                             basis = "gdp", from_year = NULL, to_year = NULL,
                             tol = NULL) {
  a <- h_from$currency
  b <- h_to$currency
  if (is.null(a) || is.null(b)) return(1)      # not money on both sides
  # Untagged means "the base year" on either side. When the base year is the
  # year the coefficients were generated at -- the default -- every term below
  # cancels and the factor is exactly 1, so untagged answers are bit-identical.
  base <- currency_base_year()
  ya <- as.integer((from_year %||% h_from$year) %||% base)
  yb <- as.integer((to_year %||% h_to$year) %||% base)
  gen <- .currency_generated_at()
  both <- identical(currency_route, "both")
  if (!both && !(currency_route %in% .CURRENCY_ROUTES)) {
    stop("`currency_route` must be one of ",
         paste(c(.CURRENCY_ROUTES, "both"), collapse = ", "), call. = FALSE)
  }
  if (identical(ya, yb) && identical(ya, gen)) return(1)   # the common path
  r <- vapply(.CURRENCY_ROUTES, function(rt)
    .currency_one_route(a, b, ya, yb, rt, basis, gen), numeric(1))
  if (both) return(r)
  if (is.null(tol)) tol <- getOption("energyRt.currency_route_tol", 0.02)
  spread <- max(r) / min(r) - 1
  if (is.finite(spread) && spread > tol) {
    # The numbers reported are the RATIO between routes, not the factor
    # applied: the factor is relative to the base year and means nothing to a
    # caller, who sees the converted value.
    warning(sprintf(
      "%s%d -> %s%d: the two routes differ by %.1f%% (a factor of %.4g). Using %s.
The gap IS the change in the real exchange rate over the interval, so neither route is a rounding of the other.
Pass currency_route = \"both\" to see each, or raise options(energyRt.currency_route_tol=) to quieten this.",
      a, ya, b, yb, 100 * spread, max(r) / min(r),
      if (identical(currency_route, "fx_then_deflate")) .CURRENCY_ROUTES[2]
      else .CURRENCY_ROUTES[1]), call. = FALSE)
  }
  r[[if (identical(currency_route, "fx_then_deflate")) 2L else 1L]]
}
