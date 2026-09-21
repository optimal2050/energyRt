# Class trade ####
#' An S4 class to represent inter-regional trade
#'
#' @name class-trade
#'
#' @inherit newTrade details
#'
#' @md
#' @slot name `r get_slot_doc("trade", "name")`
#' @slot desc `r get_slot_doc("trade", "desc")`
#' @slot commodity `r get_slot_doc("trade", "commodity")`
#' @slot routes `r get_slot_doc("trade", "routes")`
#' @slot trade `r get_slot_doc("trade", "trade")`
#' @slot aux `r get_slot_doc("trade", "aux")`
#' @slot aeff `r get_slot_doc("trade", "aeff")`
#' @slot invcost `r get_slot_doc("trade", "invcost")`
#' @slot fixom `r get_slot_doc("trade", "fixom")`
#' @slot varom `r get_slot_doc("trade", "varom")`
#' @slot capacity `r get_slot_doc("trade", "capacity")`
#' @slot vintage `r get_slot_doc("trade", "vintage")`
#' @slot cap2act `r get_slot_doc("trade", "cap2act")`
#' @slot optimizeRetirement `r get_slot_doc("trade", "optimizeRetirement")`
#' @slot misc `r get_slot_doc("trade", "misc")`
#'
#' @include class-storage.R
#'
#' @export
#'
setClass("trade",
  representation(
    # General information
    name = "character", # Short name
    desc = "character", # Details
    commodity = "character", # Vector if NULL that
    routes = "data.frame",
    # Performance parameters
    trade = "data.frame",
    aux = "data.frame", #
    aeff = "data.frame", #  Commodity efficiency
    invcost = "data.frame",
    fixom = "data.frame", # !!!ToDo: add fixom
    varom = "data.frame", # !!!ToDO: add varom
    cluster = "data.frame", # parallel sub-lines (loss tranches)
    vintage = "data.frame",
    # stock = "data.frame", # !!!ToDo: deprecate (move to @capacity)
    capacity = "data.frame", # !!!ToDo: not implemented yet
    cap2act = "numeric", #
    optimizeRetirement = "logical", # !!!ToDo: add early retirement
    misc = "list"
  ),
  # Default values and structure of slots
  prototype(
    # General information
    name = "", # name in sets
    desc = "",
    commodity = NULL, #
    routes = data.frame(
      src = character(),
      dst = character(),
      stringsAsFactors = FALSE
    ),
    trade = data.frame(
      vintage = character(),
      cluster = character(),
      src = character(),
      dst = character(),
      year = integer(),
      timeslice = character(),
      ava.up = numeric(),
      ava.fx = numeric(),
      ava.lo = numeric(),
      # Relative flow bounds -- a fraction of the object's own capacity, as
      # technology@af is. `ava.*` above is absolute, and cannot rate a line of a
      # multi-route object because the right number depends on the capacity the
      # solver is choosing.
      af.up = numeric(),
      af.fx = numeric(),
      af.lo = numeric(),
      # cost = numeric(), # !!!ToDo: move to varom
      # markup = numeric(), # !!!ToDo: move to varom
      teff = numeric(),
      # Electrical characteristics of the line. Inert unless the model is
      # interpolated with `kvl = TRUE`; a finite `reactance` is what marks a
      # route as a passive AC branch (see newACLine / newDCLink).
      reactance = numeric(),
      resistance = numeric(),
      stringsAsFactors = FALSE
    ),
    fixom = data.frame(
      vintage = character(),
      cluster = character(),
      region = character(),
      year = integer(),
      fixom = numeric(),
      stringsAsFactors = FALSE
    ),
    varom = data.frame(
      vintage = character(),
      cluster = character(),
      src = character(),
      dst = character(),
      year = integer(),
      timeslice = character(),
      varom = numeric(),
      markup = numeric(),
      stringsAsFactors = FALSE
    ),
    # `wacc` overrides the model-wide rate when annuitising this corridor;
    # `payback` shortens the cost-recovery period below `@vintage$olife`; `eac`
    # supplies the annuity directly, bypassing both.
    invcost = data.frame(
      vintage = character(),
      cluster = character(),
      region = character(),
      year = integer(),
      invcost = numeric(),
      wacc = numeric(),
      payback = numeric(),
      eac = numeric(),
      retcost = numeric(),
      stringsAsFactors = FALSE
    ),
    # Cluster declaration. A cluster is a parallel sub-line of the same corridor
    # -- in practice a LOSS TRANCHE: one segment of a piecewise-linear
    # approximation of the quadratic loss curve, with its own share of the
    # capacity and its own `teff`. Real losses go as `r * f^2`, so the loss
    # FRACTION rises with loading, which a single `teff` cannot express.
    #
    # `share` is the fraction of the line's capacity the tranche occupies; the
    # shares must sum to 1, because the derived efficiencies are only calibrated
    # when they do. `order` fixes the fill order (1 = lowest-loss); without it
    # labels sort alphabetically and "T10" would precede "T2".
    #
    # There is deliberately NO `region` column. A trade object has no `@region`
    # slot -- its scope comes from the route endpoints -- so a region here could
    # restrict nothing, and its absence rejects the mistake at construction
    # rather than deep inside variant expansion. Same technique, and the same
    # reasoning, as the missing `region` on `@capacity`.
    #
    # Optional: when empty, cluster labels are harvested from the `cluster`
    # column of the variant slots. When populated it is AUTHORITATIVE.
    # See `lossTranches()`, which builds this table and the matching `teff`.
    cluster = data.frame(
      cluster = character(),
      desc = character(),
      share = numeric(),
      order = integer(),
      stringsAsFactors = FALSE
    ),
    # Lifespan / vintage table, replacing the former `start`/`end`/`olife` slots
    # (whose ToDo notes asked for exactly this consistency with the other
    # processes). One row per vintage; `start`/`end` = user-defined window
    # (NA side = unbounded).
    # No `region` column, unlike technology and storage: a trade's lifespan
    # belongs to the route, not to a region. `pTradeOlife` is declared
    # `{trade}` and `mTradeSpan`/`mTradeNew` are (trade, year) in every solver
    # template, so `start`/`end`/`olife` have nowhere to carry a region.
    # `cluster` selects a tranche declared in `@cluster` (above).
    vintage = data.frame(
      vintage = character(),
      cluster = character(),
      start = integer(),
      end = integer(),
      olife = integer(),
      stringsAsFactors = FALSE
    ),
    capacity = data.frame(
      vintage = character(),
      cluster = character(),
      # region = character(),
      year = integer(),
      stock = numeric(),
      cap.lo = numeric(),
      cap.up = numeric(),
      cap.fx = numeric(),
      ncap.lo = numeric(),
      ncap.up = numeric(),
      ncap.fx = numeric(),
      ret.lo = numeric(),
      ret.up = numeric(),
      ret.fx = numeric(),
      stringsAsFactors = FALSE
    ),
    aux = data.frame(
      acomm = character(),
      unit = character(),
      stringsAsFactors = FALSE
    ),
    # Auxiliary commodity parameters
    aeff = data.frame(
      vintage = character(),
      cluster = character(),
      acomm = character(),
      src = character(),
      dst = character(),
      year = integer(),
      timeslice = character(),
      csrc2aout = numeric(),
      csrc2ainp = numeric(),
      cdst2aout = numeric(),
      cdst2ainp = numeric(),
      stringsAsFactors = FALSE
    ),
    cap2act = 1, #
    optimizeRetirement = FALSE,
    misc = list()
  ),
  S3methods = FALSE
)

setMethod("initialize", "trade", function(.Object, ...) {
  .Object
})


#' Create new trade object
#'
#' @description Constructor for trade object.
#'
#' @details Trade objects are used to represent inter-regional exchange in the model.
#' Without trade, every region is isolated and can only use its own resources.
#' The class defines trade routes, efficiency, costs,
#' and other parameters related to the process. Number of routes per trade object is not
#' limited. One trade object can have a part or entire trade network of the model.
#' However, it has a distinct name and all the routs will be optimized together.
#' Create separate trade objects to optimize different parts of the trade network
#' (aka transmission lines).
#'
#' @md
#' @param name `r get_slot_doc("trade", "name")`
#' @param desc `r get_slot_doc("trade", "desc")`
#' @param commodity `r get_slot_doc("trade", "commodity")`
#' @param routes `r get_slot_doc("trade", "routes")`
#' @param trade `r get_slot_doc("trade", "trade")`
#' @param fixom `r get_slot_doc("trade", "fixom")`
#' @param varom `r get_slot_doc("trade", "varom")`
#' @param invcost `r get_slot_doc("trade", "invcost")`
#' @param vintage `r get_slot_doc("trade", "vintage")`
#' @param olife deprecated, use the `olife` column of `vintage`.
#' @param start deprecated, use the `start` column of `vintage`.
#' @param end deprecated, use the `end` column of `vintage`.
#' @param capacity `r get_slot_doc("trade", "capacity")`
#' @param aux `r get_slot_doc("trade", "aux")`
#' @param aeff `r get_slot_doc("trade", "aeff")`
#' @param cap2act `r get_slot_doc("trade", "cap2act")`
#' @param optimizeRetirement `r get_slot_doc("trade", "optimizeRetirement")`
#' @param misc `r get_slot_doc("trade", "misc")`
#'
#' @return trade object with given specifications.
#' @export
#' @rdname newTrade
#' @family trade process constructor
#' @examples
#' PIPELINE1 <- newTrade(
#'   name = "PIPELINE1",
#'   desc = "Some transport pipeline",
#'   commodity = "OIL",
#'   routes = data.frame(
#'     src = c("R1", "R2"),
#'     dst = c("R2", "R3")
#'   ),
#'   trade = data.frame(
#'     src = c("R1", "R2"),
#'     dst = c("R2", "R3"),
#'     teff = c(0.99, 0.98)
#'   ),
#'   olife = list(olife = 60)
#' )
#' draw(PIPELINE1)
#'
#' PIPELINE2 <- newTrade(
#'   name = "PIPELINE2",
#'   desc = "Some transport pipeline",
#'   commodity = "OIL",
#'   routes = data.frame(
#'     src = c("R1", "R1", "R2", "R3"),
#'     dst = c("R2", "R3", "R3", "R2")
#'   ),
#'   trade = data.frame(
#'     src = c("R1", "R1", "R2", "R3"),
#'     dst = c("R2", "R3", "R3", "R2"),
#'     teff = c(0.912, 0.913, 0.923, 0.932)
#'   ),
#'   aux = data.frame(
#'     acomm = c("ELC", "CH4"),
#'     unit = c("MWh", "kt")
#'   ),
#'   aeff = data.frame(
#'     acomm = c("ELC", "CH4", "ELC", "CH4"),
#'     src = c("R1", "R1", "R2", "R3"),
#'     dst = c("R2", "R2", "R3", "R2"),
#'     csrc2ainp = c(.5, NA, .3, NA),
#'     cdst2ainp = c(.4, NA, .6, NA),
#'     csrc2aout = c(NA, .1, NA, .2)
#'   ),
#'   olife = list(olife = 60)
#' )
#' draw(PIPELINE2, node = "R1")
#' draw(PIPELINE2, node = "R2")
#' draw(PIPELINE2, node = "R3")
newTrade <- function(
    name = "",
    desc = "",
    commodity = character(),
    routes = data.frame(),
    trade = data.frame(),
    fixom = data.frame(),
    varom = data.frame(),
    cluster = data.frame(),
    invcost = data.frame(),
    olife = data.frame(),
    # `-Inf` / `Inf` mean "no restriction"; `.tech_lifespan_args()` treats an
    # all-infinite bound as absent, so these produce no `vintage` row.
    start = data.frame(start = -Inf, stringsAsFactors = FALSE),
    end = data.frame(end = Inf, stringsAsFactors = FALSE),
    vintage = data.frame(),
    capacity = data.frame(),
    aux = data.frame(),
    aeff = data.frame(),
    cap2act = 1,
    optimizeRetirement = FALSE,
    misc = list(),
    ...
) {
  args <- list(
    desc = desc,
    commodity = commodity,
    routes = routes,
    trade = trade,
    fixom = fixom,
    varom = varom,
    cluster = cluster,
    invcost = invcost,
    vintage = vintage,
    olife = olife,
    start = start,
    end = end,
    capacity = capacity,
    aux = aux,
    aeff = aeff,
    cap2act = cap2act,
    optimizeRetirement = optimizeRetirement,
    misc = misc,
    ...
  )
  args <- .trade_removed_args(args)
  args <- .trade_vintage_region(args)
  args <- .tech_lifespan_args(args, "trade")
  do.call(.data2slots, c(list("trade", name), args))
}




## Methods ####################################################################
#' Update trade object
#' @rdname update
#' @name update
#'
#' @family trade update
#' @keywords trade update
#' @method trade update
#' @export
setMethod("update", signature(object = "trade"), function(object, ...) {
  args <- .trade_removed_args(list(...))
  args <- .trade_vintage_region(args)
  args <- .tech_lifespan_args(args, "trade")
  do.call(.data2slots, c(list("trade", object), args))
})


# ---------------------------------------------------------------------------
# (was R/trade_losses.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

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
