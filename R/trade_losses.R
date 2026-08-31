# =========================================================================== #
# Piecewise-linear transmission losses, by capacity tranches.
#
# A `trade` object loses a FIXED fraction of what it carries: `teff` is the share
# of the flow leaving `src` that arrives at `dst`. Real transmission losses are
# QUADRATIC in flow,
#
#     loss(f) = r * f^2
#
# so the loss FRACTION is `r*f`, rising linearly with loading. Under a single
# `teff` a half-loaded and a fully-loaded line lose the same proportion, where
# physically the fraction should double.
#
# The fix here splits the line's capacity into TRANCHES: parallel sub-lines, each
# with its own share of the capacity and its own `teff`, the loss rate rising
# from tranche to tranche. The loss curve becomes piecewise-linear and CONVEX,
# and that convexity is what makes it work without integer variables -- a lossier
# tranche delivers strictly less per unit sent, so cost minimisation fills the
# cheap one first of its own accord.
#
# With shares `w_1..w_T` summing to 1 and cumulative boundaries
# `alpha_0 = 0`, `alpha_t = w_1 + ... + w_t`:
#
#     lambda_t = loss_full * (alpha_{t-1} + alpha_t)     # tranche t loss fraction
#     teff_t   = 1 - lambda_t
#
# `lambda_t` is the secant slope of `r*f^2` across the tranche, so the fit is
# EXACT at every breakpoint and `sum_t w_t * lambda_t == loss_full` identically.
#
# WHY SHARES AND NOT MEGAWATTS. `loss_full = r * F` -- the loss fraction at rated
# load -- is invariant under parallel-circuit expansion: a second circuit halves
# `r` and doubles `F`. So relative tranches carry over to an expanded line with
# no recomputation, which is what lets losses work when capacity is a decision
# variable. A formulation with absolute breakpoints cannot do this: it needs a
# finite capacity bound to place them.
#
# WHAT THIS DOES NOT COVER. Convexity is an assumption about the rest of the
# model, not a theorem. The merit order holds because `teff` falls across
# tranches while every other per-unit coefficient is equal. Give the tranches
# different `varom`, or force flow onto one with a binding `af.lo` / `ava.fx`,
# and the ordering can invert. It is also only STRICT while the delivered
# commodity has value: if supply at `src` is free, `varom` is zero and capacity
# is costless, the split between tranches is a tie -- degenerate rather than
# wrong, but the reported split is then arbitrary.
# =========================================================================== #

#' Loss tranches for a transmission line
#'
#' @description
#' Derive a piecewise-linear approximation of a line's quadratic losses, as a set
#' of parallel capacity tranches with rising loss rates.
#' `r lifecycle::badge("experimental")`
#'
#' @details
#' Returns the two (or three) data frames a `trade` object needs to describe the
#' tranches: a `cluster` declaration carrying the shares, a `trade` frame
#' carrying each tranche's `teff`, and -- when `fix` is given -- a `capacity`
#' frame splitting a fixed rating between them.
#'
#' The tranches are declared as **fractions** of the line's capacity rather than
#' absolute quantities. That is what makes them invariant under parallel-circuit
#' expansion, so they need no recomputation when capacity is optimised; see the
#' note at the top of `R/trade_losses.R`.
#'
#' `loss` is the documented input: the dimensionless fraction of the sent flow
#' lost when the line runs at its rating. It is the quantity the arithmetic
#' actually needs, and the one that stays put under expansion. A `resistance`
#' may be given instead, but only together with the `capacity` it was measured
#' against, because `loss = resistance * capacity` -- a resistance alone does not
#' determine a loss fraction. The two must be in units whose product is
#' dimensionless; for a per-unit resistance on base `S_base`, pass
#' `capacity = F / S_base`.
#'
#' @param shares numeric, the fraction of the line's capacity each tranche
#'   occupies. Must be positive and sum to 1. A single share reproduces the
#'   ordinary flat-`teff` line exactly.
#' @param loss numeric, the loss fraction at rated load (dimensionless).
#' @param resistance,capacity numeric, an alternative to `loss`: their product is
#'   the loss fraction at rated load. Both or neither.
#' @param teff numeric, a base efficiency multiplied through every tranche, for
#'   losses that are not resistive (default 1).
#' @param labels character, tranche names. Default `T1`, `T2`, ...
#' @param fix numeric, a fixed line rating. When given, the result carries a
#'   `capacity` frame with `cap.fx = shares * fix`.
#' @param desc character, per-tranche descriptions.
#'
#' @return An object of class `loss_tranches`: a list with `cluster`, `trade` and
#'   (optionally) `capacity` data frames, ready to pass to [newACLine()],
#'   [newDCLink()] or [newTrade()].
#'
#' @seealso [newACLine()], [newDCLink()], [newTrade()]
#' @family class trade
#' @export
#'
#' @examples
#' # A line losing 3% at full load, in three tranches
#' lossTranches(shares = c(0.4, 0.35, 0.25), loss = 0.03)
lossTranches <- function(shares,
                         loss = NULL,
                         resistance = NULL,
                         capacity = NULL,
                         teff = 1,
                         labels = NULL,
                         fix = NULL,
                         desc = NULL) {
  if (missing(shares) || !length(shares)) {
    stop("lossTranches(): `shares` is required -- the fraction of the line's ",
         "capacity each tranche occupies.", call. = FALSE)
  }
  w <- as.numeric(shares)
  if (any(!is.finite(w) | w <= 0)) {
    stop("lossTranches(): every `share` must be finite and greater than zero; ",
         "got ", paste(format(w), collapse = ", "), ".", call. = FALSE)
  }
  if (abs(sum(w) - 1) > 1e-8) {
    stop("lossTranches(): `shares` sum to ", format(sum(w)), ", not 1. They are ",
         "FRACTIONS of the capacity, not absolute capacities -- the derived ",
         "efficiencies are calibrated only when they sum to 1.", call. = FALSE)
  }

  # `loss` XOR (`resistance` AND `capacity`). Both supplied is an error rather
  # than a preference: silently picking one would hide a disagreement.
  has_loss <- !is.null(loss)
  has_rc <- !is.null(resistance) || !is.null(capacity)
  if (has_loss && has_rc) {
    stop("lossTranches(): give either `loss` or `resistance` + `capacity`, not ",
         "both -- if the two disagree there is no way to tell which was meant.",
         call. = FALSE)
  }
  if (!has_loss) {
    if (is.null(resistance) || is.null(capacity)) {
      stop("lossTranches(): supply `loss` (the loss fraction at rated load), or ",
           "`resistance` together with the `capacity` it was measured against. ",
           "A resistance alone does not determine a loss fraction.",
           call. = FALSE)
    }
    loss <- as.numeric(resistance) * as.numeric(capacity)
    if (!is.finite(loss) || loss <= 0 || loss >= 1) {
      stop("lossTranches(): `resistance * capacity` = ", format(loss),
           ", which is not a loss fraction. The two must be in units whose ",
           "product is dimensionless -- for a per-unit resistance on base ",
           "S_base, pass `capacity = F / S_base`.", call. = FALSE)
    }
  }
  loss_full <- as.numeric(loss)
  if (length(loss_full) != 1L || !is.finite(loss_full) || loss_full <= 0) {
    stop("lossTranches(): `loss` must be a single positive number; got ",
         format(loss_full), ".", call. = FALSE)
  }

  alpha <- cumsum(w)
  alpha0 <- c(0, utils::head(alpha, -1))
  lambda <- loss_full * (alpha0 + alpha)
  if (max(lambda) >= 1) {
    lim <- 1 / (alpha0[length(alpha0)] + alpha[length(alpha)])
    stop("lossTranches(): with these shares the last tranche loses ",
         format(max(lambda)), " of what it carries, so its efficiency would be ",
         "zero or negative. `loss` must be below ", format(lim), ".",
         call. = FALSE)
  }
  eff <- as.numeric(teff) * (1 - lambda)

  n <- length(w)
  if (is.null(labels)) labels <- sprintf("T%d", seq_len(n))
  labels <- as.character(labels)
  if (length(labels) != n) {
    stop("lossTranches(): `labels` has ", length(labels), " entries for ", n,
         " shares.", call. = FALSE)
  }
  if (anyDuplicated(labels)) {
    stop("lossTranches(): `labels` must be unique; got ",
         paste(labels, collapse = ", "), ".", call. = FALSE)
  }
  if (is.null(desc)) {
    desc <- sprintf("loss tranche %d of %d: %.4g-%.4g of capacity, %.4g%% lost",
                    seq_len(n), n, alpha0, alpha, 100 * lambda)
  }

  out <- list(
    cluster = data.frame(cluster = labels, desc = as.character(desc),
                         share = w, order = seq_len(n),
                         stringsAsFactors = FALSE),
    # No `src` / `dst`: `.line_common()` broadcasts an endpoint-free frame onto
    # both routes. No `resistance` either -- it would join the merge key there
    # and break the join.
    trade = data.frame(cluster = labels, teff = eff, stringsAsFactors = FALSE),
    capacity = NULL)
  if (!is.null(fix)) {
    if (length(fix) != 1L || !is.finite(fix) || fix <= 0) {
      stop("lossTranches(): `fix` must be a single positive rating; got ",
           format(fix), ".", call. = FALSE)
    }
    out$capacity <- data.frame(cluster = labels, cap.fx = w * as.numeric(fix),
                               stringsAsFactors = FALSE)
  }
  attr(out, "loss_full") <- loss_full
  attr(out, "alpha") <- alpha
  attr(out, "alpha0") <- alpha0
  attr(out, "lambda") <- lambda
  class(out) <- "loss_tranches"
  out
}

#' @exportS3Method print loss_tranches
print.loss_tranches <- function(x, ...) {
  lf <- attr(x, "loss_full")
  a0 <- attr(x, "alpha0"); a1 <- attr(x, "alpha")
  lam <- attr(x, "lambda")
  cat("Loss tranches: ", length(lam), " segment(s), ",
      format(100 * lf, digits = 4), "% lost at rated load\n", sep = "")
  d <- data.frame(cluster = x$cluster$cluster,
                  share = x$cluster$share,
                  from = a0, to = a1,
                  loss = lam, teff = x$trade$teff)
  if (!is.null(x$capacity)) d$cap.fx <- x$capacity$cap.fx
  print(d, row.names = FALSE, digits = 5)
  # The calibration identity: the flow-weighted loss at full load must come back
  # to `loss_full`, which is the one property a reviewer wants to see and cannot
  # eyeball from the table.
  cat("check: sum(share x loss) = ", format(sum(x$cluster$share * lam)),
      "  (= loss at rated load, ", format(lf), ")\n", sep = "")
  invisible(x)
}
