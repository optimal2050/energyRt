# =========================================================================== #
# Stepped price curves for supply, export and import.
#
# Each of these classes carries a SINGLE price per (region, year, timeslice) --
# `@supply$cost`, `@export$price`, `@import$price`. A real supply curve is
# stepped: cheap resource grades are exhausted first and the next ones cost more.
# A real export market is the mirror image: it pays less as you push more volume
# into it.
#
# A curve is expressed here as CLUSTERS -- parallel sub-objects of the same
# process, one per price step, each with its own share of the quantity and its
# own price. Variant expansion then turns them into ordinary `supply` / `export`
# / `import` objects before interpolation, and several objects on one commodity
# already sum into one balance (`eqSupOutTot`, `eqImportTot`, `eqExportTot`). So
# a curve needs no new variable, no new equation and no solver-template change.
#
# WHY IT NEEDS NO INTEGER VARIABLES. The steps are ordered by price and the
# solver is free to use any of them, but a cheaper step is strictly better per
# unit, so cost minimisation fills them in order by itself. That is convexity,
# and it is why the DIRECTION of the curve is enforced below: for supply and
# import the price must rise with quantity, for export it must fall. An export
# curve written the wrong way round is non-convex -- the solver jumps to the
# top-priced step and skips the rest, inventing revenue no market would pay.
#
# WHY MIDPOINTS. `range` describes a continuous linear marginal-price curve. The
# average price over a band of a linear curve is the price at its MIDPOINT, so
# pricing each step at its band midpoint reproduces the area under the curve
# exactly: `sum_t w_t * price_t == mean(range)` identically. Pricing at the band
# edges would systematically over- or under-charge.
#
# WHAT CAN BREAK THE MERIT ORDER. The ordering is a property of the whole model,
# not of this helper. It holds while every other per-unit coefficient is equal
# across steps. A LOWER bound (`ava.lo`, `exp.lo`, `imp.lo`) forces flow onto a
# specific step regardless of price; so can a binding cumulative reserve. And it
# is only STRICT while the commodity has value -- with free disposal at the far
# end the split between steps can be a tie.
# =========================================================================== #

# Per-step prices and shares, shared by all three helpers.
.price_curve <- function(range = NULL, nsteps = NULL, shares = NULL,
                         price = NULL, labels = NULL, desc = NULL,
                         what = "asSupplyCurve") {
  if (is.null(shares)) {
    n <- if (!is.null(nsteps)) as.integer(nsteps) else
      if (!is.null(price)) length(price) else NULL
    if (is.null(n)) {
      stop(what, "(): give `nsteps` (with `range`) or an explicit `shares` / ",
           "`price` vector -- there is no default number of steps.",
           call. = FALSE)
    }
    if (length(n) != 1L || is.na(n) || n < 1L) {
      stop(what, "(): `nsteps` must be a single positive integer; got ",
           format(nsteps), ".", call. = FALSE)
    }
    shares <- rep(1 / n, n)
  }
  w <- as.numeric(shares)
  if (any(!is.finite(w) | w <= 0)) {
    stop(what, "(): every share must be finite and greater than zero; got ",
         paste(format(w), collapse = ", "), ".", call. = FALSE)
  }
  if (abs(sum(w) - 1) > 1e-8) {
    stop(what, "(): `shares` sum to ", format(sum(w)), ", not 1. They are ",
         "FRACTIONS of the object's quantity, not absolute amounts.",
         call. = FALSE)
  }
  n <- length(w)

  # `range` XOR `price`. Both is an error rather than a preference: if the two
  # disagree there is no way to tell which was meant.
  if (!is.null(range) && !is.null(price)) {
    stop(what, "(): give either `range` or an explicit `price` vector, not ",
         "both.", call. = FALSE)
  }
  alpha <- cumsum(w)
  alpha0 <- c(0, utils::head(alpha, -1))
  if (!is.null(price)) {
    p <- as.numeric(price)
    if (length(p) != n) {
      stop(what, "(): `price` has ", length(p), " entries for ", n, " steps.",
           call. = FALSE)
    }
  } else {
    if (is.null(range) || length(range) != 2L || any(!is.finite(range))) {
      stop(what, "(): `range` must be two finite prices, c(first, last).",
           call. = FALSE)
    }
    # Band MIDPOINTS -- exact for a linear curve; see the note at the top.
    p <- range[1] + (range[2] - range[1]) * (alpha0 + alpha) / 2
  }

  if (is.null(labels)) labels <- sprintf("S%d", seq_len(n))
  labels <- as.character(labels)
  if (length(labels) != n) {
    stop(what, "(): `labels` has ", length(labels), " entries for ", n,
         " steps.", call. = FALSE)
  }
  if (anyDuplicated(labels)) {
    stop(what, "(): `labels` must be unique; got ",
         paste(labels, collapse = ", "), ".", call. = FALSE)
  }
  if (is.null(desc)) {
    desc <- sprintf("step %d of %d: %.4g-%.4g of quantity at %.6g",
                    seq_len(n), n, alpha0, alpha, p)
  }
  list(share = w, price = p, label = labels, desc = as.character(desc),
       alpha0 = alpha0, alpha = alpha)
}

# Refuse a curve whose prices move the wrong way. `dir` is +1 for a rising
# curve (supply, import) and -1 for a falling one (export).
.check_curve_direction <- function(p, dir, what) {
  if (length(p) < 2L) return(invisible(TRUE))
  d <- diff(p)
  bad <- if (dir > 0) any(d < -1e-12) else any(d > 1e-12)
  if (!bad) return(invisible(TRUE))
  if (dir > 0) {
    stop(what, "(): the prices ", paste(format(p), collapse = ", "),
         " do not rise. A supply or import curve must be non-decreasing: the ",
         "solver fills the cheapest step first, so a cheaper step placed after ",
         "a dearer one is never reached and the curve is not convex.",
         call. = FALSE)
  }
  stop(what, "(): the prices ", paste(format(p), collapse = ", "),
       " do not fall. An export curve must be non-increasing -- the market pays ",
       "less as you push more volume into it. A rising one is NOT convex: ",
       "export revenue is a negative cost, so the solver would jump straight to ",
       "the highest-priced step and skip the rest, inventing revenue no market ",
       "would pay.", call. = FALSE)
}

# Split every finite quantity bound across the steps, and write the price.
# `dfslot` is the main data slot, `bounds` its bound-column stem ("ava" / "exp"
# / "imp"), `pricecol` the price column, `resslot` the cumulative slot or NULL.
.apply_curve <- function(obj, cur, dfslot, bounds, pricecol, resslot, what) {
  n <- length(cur$share)
  bcols <- paste0(bounds, c(".lo", ".up", ".fx"))

  d <- as.data.frame(slot(obj, dfslot))
  if (!nrow(d)) d <- d[1, , drop = FALSE][0, , drop = FALSE]
  # An object with no rows at all still needs one, to carry the price.
  if (!nrow(d)) {
    d <- slot(obj, dfslot)[NA_integer_, , drop = FALSE]
    rownames(d) <- NULL
  }
  have_q <- any(vapply(intersect(bcols, names(d)),
                       function(cn) any(is.finite(d[[cn]])), logical(1)))

  res <- if (!is.null(resslot) && .hasSlot(obj, resslot))
    as.data.frame(slot(obj, resslot)) else NULL
  rcols <- c("res.lo", "res.up", "res.fx")
  have_r <- !is.null(res) && nrow(res) > 0 &&
    any(vapply(intersect(rcols, names(res)),
               function(cn) any(is.finite(res[[cn]])), logical(1)))

  if (!have_q && !have_r) {
    stop(what, "(): the object has no finite quantity bound to split -- ",
         "looked at `", paste(bcols, collapse = "`, `"),
         if (!is.null(resslot)) paste0("` in @", dfslot, " and `",
                                       paste(rcols, collapse = "`, `"),
                                       "` in @", resslot) else
           paste0("` in @", dfslot),
         ". A curve over an unbounded quantity is meaningless: the solver would ",
         "take the cheapest step without limit and the dearer steps could ",
         "never be reached.", call. = FALSE)
  }

  # Replicate each row once per step, scaling the bounds and setting the price.
  parts <- lapply(seq_len(n), function(k) {
    x <- d
    x$cluster <- cur$label[k]
    for (cn in intersect(bcols, names(x))) x[[cn]] <- x[[cn]] * cur$share[k]
    x[[pricecol]] <- cur$price[k]
    x
  })
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  slot(obj, dfslot) <- out

  if (have_r) {
    rparts <- lapply(seq_len(n), function(k) {
      x <- res
      x$cluster <- cur$label[k]
      for (cn in intersect(rcols, names(x))) x[[cn]] <- x[[cn]] * cur$share[k]
      x
    })
    r <- do.call(rbind, rparts)
    rownames(r) <- NULL
    slot(obj, resslot) <- r
  }

  slot(obj, "cluster") <- data.frame(
    cluster = cur$label, desc = cur$desc, share = cur$share,
    order = seq_len(n), stringsAsFactors = FALSE)
  obj
}

#' Stepped supply, export and import curves
#'
#' @description
#' Turn a single-price `supply`, `export` or `import` object into a stepped
#' price curve: parallel steps, each with its own share of the quantity and its
#' own price. `r lifecycle::badge("experimental")`
#'
#' @details
#' `range` describes a continuous linear marginal-price curve and each step is
#' priced at the MIDPOINT of its band, which reproduces the area under that curve
#' exactly -- the quantity-weighted mean of the step prices is `mean(range)`.
#' Pass an explicit `price` vector instead for a curve that is not linear.
#'
#' The steps become `cluster` variants, so one object expands into one ordinary
#' object per step before interpolation and they all sum into the same commodity
#' balance. No new variable, equation or solver-template change is involved.
#'
#' **Direction matters and is enforced.** `asSupplyCurve()` and
#' `asImportCurve()` require a non-decreasing curve, `asExportCurve()` a
#' non-increasing one. This is not style: the solver fills the cheapest step
#' first (or, for export, the best-paid first, since export revenue is a negative
#' cost), and a curve running the other way is not convex.
#'
#' Every finite quantity bound the object carries is split across the steps --
#' `ava.*` / `exp.*` / `imp.*` and the cumulative `@reserve`. An object with no
#' finite bound is refused, because an unbounded curve is meaningless.
#'
#' @param obj a `supply`, `export` or `import` object.
#' @param range numeric of length 2, the first and last price of a linear curve.
#' @param nsteps integer, the number of steps. Ignored when `shares` is given.
#' @param shares numeric, each step's fraction of the quantity. Must be positive
#'   and sum to 1. Default equal.
#' @param price numeric, explicit per-step prices, as an alternative to `range`.
#' @param labels character, step names. Default `S1`, `S2`, ...
#' @param desc character, per-step descriptions.
#'
#' @return An object of the same class, carrying a `cluster` axis.
#' @seealso [newSupply()], [newExport()], [newImport()], [lossTranches()]
#' @rdname supply-curve
#' @export
#'
#' @examples
#' SUP <- newSupply("COA", commodity = "COA",
#'                  supply = data.frame(ava.up = 100))
#' SUP <- asSupplyCurve(SUP, range = c(10, 50), nsteps = 5)
#' SUP@cluster
asSupplyCurve <- function(obj, range = NULL, nsteps = NULL, shares = NULL,
                          price = NULL, labels = NULL, desc = NULL) {
  if (!methods::is(obj, "supply")) {
    stop("asSupplyCurve(): expects a `supply` object; got ", class(obj)[1],
         ". Use asExportCurve() / asImportCurve() for those.", call. = FALSE)
  }
  cur <- .price_curve(range, nsteps, shares, price, labels, desc, "asSupplyCurve")
  .check_curve_direction(cur$price, +1, "asSupplyCurve")
  .apply_curve(obj, cur, "supply", "ava", "cost", "reserve", "asSupplyCurve")
}

#' @rdname supply-curve
#' @export
asImportCurve <- function(obj, range = NULL, nsteps = NULL, shares = NULL,
                          price = NULL, labels = NULL, desc = NULL) {
  if (!methods::is(obj, "import")) {
    stop("asImportCurve(): expects an `import` object; got ", class(obj)[1], ".",
         call. = FALSE)
  }
  cur <- .price_curve(range, nsteps, shares, price, labels, desc, "asImportCurve")
  .check_curve_direction(cur$price, +1, "asImportCurve")
  .apply_curve(obj, cur, "import", "imp", "price", "reserve", "asImportCurve")
}

#' @rdname supply-curve
#' @export
asExportCurve <- function(obj, range = NULL, nsteps = NULL, shares = NULL,
                          price = NULL, labels = NULL, desc = NULL) {
  if (!methods::is(obj, "export")) {
    stop("asExportCurve(): expects an `export` object; got ", class(obj)[1], ".",
         call. = FALSE)
  }
  cur <- .price_curve(range, nsteps, shares, price, labels, desc, "asExportCurve")
  .check_curve_direction(cur$price, -1, "asExportCurve")
  .apply_curve(obj, cur, "export", "exp", "price", "reserve", "asExportCurve")
}
