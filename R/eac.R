# =========================================================================== #
# eac.R  —  Equivalent Annual Cost (EAC) of investment.
#
# pXEac(key, region, year) = pXInvcost annuitised by the capital-recovery factor
# of the cost of capital over the cost-recovery period:
#   eac = invcost / n                          (rate 0, finite n)
#   eac = invcost * d(1+d)^n / ((1+d)^n - 1)   (rate d>0, finite n)
#   eac = invcost * d                          (rate d>0, infinite n)
#
# The rate is the WACC — `@invcost$wacc` for this process if given, else the
# model-wide `pWacc`. The social discount rate never appears here; it discounts
# the cost stream in the objective (pDiscountFactor) and nothing else.
#
# The period `n` is `@invcost$payback` if given, else `@vintage$olife`. Payback
# is the COST-RECOVERY life: the same value also narrows the eqXEac charging
# window in the solver, so the annuity is paid over `payback` years and not one
# year more, while the technical life still governs when capacity operates.
#
# A user-supplied `@invcost$eac` bypasses all of it: the slot loop seeds pXEac
# from that column (modInp.yml binds `colName: eac`), and the rows it supplies
# survive the computed values below.
#
# Runs after interpolate_parameters (reads the interpolated pXInvcost / pWacc /
# pXOlife / pXPayback) and before the value recipe (mTradeEac reads pTradeEac's
# value domain). At this stage tech/storage cost params are still region-FOLDED
# (region = NA wildcard) while pWacc is region-explicit, so joins must be
# wildcard-aware.
# =========================================================================== #

# Wildcard-aware left join: join `df` to `src` (which carries `val_cols`) on
# `keys`, but a key that is fully-NA on one side (a folded wildcard) is not used
# to match; instead the side that holds explicit values supplies that column.
.eac_merge <- function(df, src, val_cols, keys) {
  src <- as.data.frame(src)
  has <- function(d, k) k %in% names(d) && !all(is.na(d[[k]]))
  use <- keys[vapply(keys, function(k) has(df, k) && has(src, k), logical(1))]
  src_keep <- c(use, val_cols)
  for (k in setdiff(keys, use)) {
    # src holds explicit values for a key df cannot match on -> let src supply it.
    if (has(src, k) && (!k %in% names(df) || all(is.na(df[[k]])))) {
      src_keep <- c(src_keep, k)
      if (k %in% names(df)) df <- df[, setdiff(names(df), k), drop = FALSE]
    }
  }
  dplyr::left_join(df, src[, unique(src_keep), drop = FALSE], by = use)
}

# Join one parameter's values onto `df` under `nm`, or fill NA if it is absent.
.eac_join <- function(scen, df, par, nm, keys) {
  P <- scen@modInp@parameters
  src <- if (is.null(P[[par]])) NULL else as.data.frame(get_data_slot(P[[par]]))
  if (is.null(src) || nrow(src) == 0) {
    df[[nm]] <- NA_real_
    return(df)
  }
  names(src)[names(src) == "value"] <- nm
  .eac_merge(df, src, nm, keys)
}

# Capital-recovery factor: the annual payment that repays 1 unit of capital over
# `n` years at rate `d`. Vectorised, and safe at d == 0 and n == Inf.
.crf <- function(d, n) {
  out <- 1 / n
  fl <- d != 0 & is.finite(n)
  out[fl] <- d[fl] * (1 + d[fl])^n[fl] / ((1 + d[fl])^n[fl] - 1)
  fl <- d != 0 & is.infinite(n)
  out[fl] <- d[fl]
  out
}

# `payback` must be a positive number of years no longer than the technical
# life. Beyond `olife` the charge would fall outside the eqXEac window map (a
# "span" window that olife does not extend), so it would be silently truncated
# rather than paid.
.check_payback <- function(df, key) {
  bad <- !is.na(df$payback) & df$payback <= 0
  if (any(bad)) {
    stop("`payback` must be positive (", key, " ",
         paste(unique(df[[key]][bad]), collapse = ", "), ").")
  }
  bad <- !is.na(df$payback) & is.finite(df$olife) & df$payback > df$olife
  if (any(bad)) {
    stop("`payback` cannot exceed `olife` (", key, " ",
         paste(unique(df[[key]][bad]), collapse = ", "),
         "): the annuity would outlive the capacity paying it.\n",
         "  `payback` is the cost-recovery period; leave it unset to recover ",
         "the investment over the full operational life.")
  }
  invisible(NULL)
}

# Compute one family's EAC param from its invcost / wacc / olife / payback.
.eac_one <- function(scen, key, inv_par, olife_par, new_par, eac_par,
                     wacc_par, payback_par) {
  P <- scen@modInp@parameters
  if (is.null(P[[eac_par]]) || is.null(P[[inv_par]])) return(scen)
  inv <- as.data.frame(get_data_slot(P[[inv_par]]))
  if (is.null(inv) || nrow(inv) == 0) return(scen)
  names(inv)[names(inv) == "value"] <- "invcost"

  # Domain = the investment-window map (scope-restricted (key, region, year));
  # invcost is broadcast onto it (it is region-folded at this stage). Falls back
  # to the raw invcost domain when the New map is unavailable.
  new <- if (is.null(P[[new_par]])) NULL else
    as.data.frame(get_data_slot(P[[new_par]]))
  if (!is.null(new) && nrow(new) > 0) {
    df <- .eac_merge(new, inv, "invcost", c(key, "region", "year"))
    df <- df[!is.na(df$invcost), , drop = FALSE]
  } else {
    df <- inv
  }
  if (nrow(df) == 0) return(scen)

  # Rate: process-specific cost of capital, else the model-wide one. NA means
  # "not supplied" -- an explicit 0 is a legitimate zero-interest rate and is
  # kept (hence `dropIfEmpty: false` on both parameters in modInp.yml).
  df <- .eac_join(scen, df, wacc_par, "pwacc", c(key, "region", "year"))
  df <- .eac_join(scen, df, "pWacc", "mwacc", c("region", "year"))
  df$rate <- dplyr::coalesce(df$pwacc, df$mwacc, 0)

  # Period: cost-recovery life if given, else operational life (absent -> Inf,
  # i.e. a perpetual annuity).
  df <- .eac_join(scen, df, olife_par, "olife", c(key, "region"))
  df <- .eac_join(scen, df, payback_par, "payback", c(key, "region", "year"))
  df$payback[!is.na(df$payback) & df$payback == 0] <- NA_real_
  .check_payback(df, key)
  df$olife[is.na(df$olife)] <- Inf
  df$life <- dplyr::coalesce(df$payback, df$olife)

  df$eac <- df$invcost * .crf(df$rate, df$life)

  # A user-supplied EAC wins, row for row. pXEac is seeded from `@invcost$eac`
  # by the slot loop, so anything already in it is user input. Joined the same
  # wildcard-aware way as everything else: the seed is region-FOLDED at this
  # stage while `df` is region-explicit, so an exact join would match nothing.
  df <- .eac_join(scen, df, eac_par, "ueac", c(key, "region", "year"))
  df$eac <- dplyr::coalesce(df$ueac, df$eac)

  dims <- P[[eac_par]]@dimSets
  out <- unique(df[, c(dims, "eac"), drop = FALSE])
  names(out)[names(out) == "eac"] <- "value"

  # Keep a supplied `eac` for processes that have NO invcost.
  #
  # `df` was filtered to invcost-bearing rows above, so `out` covers only those
  # processes. Writing it back wholesale ERASED every process whose capital cost
  # came from `@invcost$eac` alone -- the charge simply left the objective, with
  # no warning, and the capacity became free. The early return at the top of this
  # function is what hid it: when NO process in the class has an invcost we never
  # reach here, so it only bit MIXED models (some annuitised, some overnight),
  # which is the ordinary case for a converted model.
  #
  # Matched on the process key alone, not the full dims: at this stage the seed
  # is region-FOLDED while `out` is region-explicit (same reason the `ueac` join
  # above goes through .eac_merge), so a full-key anti-join would match nothing
  # and duplicate every row. Within a process that supplies both, the `ueac`
  # coalesce above already gives the supplied value precedence row by row.
  seed <- as.data.frame(get_data_slot(P[[eac_par]]))
  if (!is.null(seed) && nrow(seed) > 0 && key %in% names(seed)) {
    keep <- seed[!seed[[key]] %in% unique(df[[key]]), , drop = FALSE]
    if (nrow(keep) > 0) {
      for (cl in setdiff(names(out), names(keep))) keep[[cl]] <- NA
      out <- rbind(out, keep[, names(out), drop = FALSE])
    }
  }

  scen@modInp@parameters[[eac_par]] <- .fold_write_back(P[[eac_par]], out)
  scen
}

# Warn when a payback period cannot be represented on the milestone grid. The
# eqXEac window admits a milestone whole or not at all, so a payback that does
# not land on a period boundary is charged over the enclosing whole periods --
# e.g. 10 years on a 1 + 10 + 10 ... grid is charged over 11. Silent otherwise it
# would look like an exact cost recovery when it is not.
#
# Only vintages whose full window fits inside the horizon are checked; a late
# vintage always has its window truncated by the end of the horizon, which is
# inherent (olife behaves the same way) and not a grid mismatch.
.check_payback_grid <- function(scen) {
  yrs <- sort(as.integer(scen@modInp@sets$year))
  if (length(yrs) < 2) return(invisible(NULL))
  plen <- tryCatch(
    as.data.frame(get_data_slot(scen@modInp@parameters[["pPeriodLen"]])),
    error = function(e) NULL)
  if (is.null(plen) || nrow(plen) == 0) return(invisible(NULL))
  len <- plen$value[match(yrs, as.integer(plen$year))]
  len[is.na(len)] <- 1
  ord <- yrs - min(yrs) + 1L
  span <- sum(len)

  for (pn in c("pTechPayback", "pStoragePayback", "pTradePayback")) {
    p <- scen@modInp@parameters[[pn]]
    if (is.null(p)) next
    d <- tryCatch(as.data.frame(get_data_slot(p)), error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0) next
    for (v in sort(unique(d$value[!is.na(d$value) & d$value > 0]))) {
      fits <- ord + v - 1 <= span
      if (!any(fits)) next
      tot <- vapply(which(fits), function(i) {
        sum(len[ord >= ord[i] & ord < ord[i] + v])
      }, numeric(1))
      off <- sort(unique(tot[!vapply(tot, function(x) isTRUE(all.equal(x, v)),
                                     logical(1))]))
      if (length(off)) {
        message("payback = ", v, " does not land on a milestone boundary; ",
                "the annuity is charged over ", paste(off, collapse = "/"),
                " years depending on the vintage.")
      }
    }
  }
  invisible(NULL)
}

# `pXPayback` narrows the eqXEac charging window. GLPK, GAMS, JuMP/Julia and
# Pyomo-Concrete all implement it (ported from GLPK 2026-08-19).
#
# Pyomo-ABSTRACT does not, and cannot without a bigger change: its eqXEac is
# still the pre-vintaging `pXEac * vXCap` form, which charges the annuity on
# TOTAL capacity -- pre-existing stock included -- rather than summing over
# still-alive vintages. Adding a payback window to that would be meaningless.
# An engine left on the olife window would quietly annuitise over the payback
# period while charging for the full operational life, over-recovering the
# investment, so it is refused instead.
.assert_payback_supported <- function(scen, engine) {
  used <- character()
  for (pn in c("pTechPayback", "pStoragePayback", "pTradePayback")) {
    p <- scen@modInp@parameters[[pn]]
    if (is.null(p)) next
    d <- as.data.frame(get_data_slot(p))
    if (!is.null(d) && nrow(d) > 0 && any(!is.na(d$value) & d$value > 0)) {
      used <- c(used, pn)
    }
  }
  if (length(used) == 0) return(invisible(NULL))
  stop("`payback` is not implemented for ", engine, " (set in ",
       paste(used, collapse = ", "), ").\n",
       "  It is available on GLPK, GAMS, JuMP/Julia and Pyomo-Concrete. ",
       "Pyomo-Abstract still uses the pre-vintaging `pXEac * vXCap` form.\n",
       "  Use one of those engines, or drop `@invcost$payback` and use ",
       "`@vintage$olife` for the cost-recovery period.")
}

compute_eac_parameters <- function(scen) {
  scen <- .eac_one(scen, "tech", "pTechInvcost", "pTechOlife", "mTechNew",
                   "pTechEac", "pTechWacc", "pTechPayback")
  scen <- .eac_one(scen, "stg", "pStorageInvcost", "pStorageOlife",
                   "mStorageNew", "pStorageEac", "pStorageWacc",
                   "pStoragePayback")
  scen <- .eac_one(scen, "trade", "pTradeInvcost", "pTradeOlife", "mTradeNew",
                   "pTradeEac", "pTradeWacc", "pTradePayback")
  .check_payback_grid(scen)
  scen
}
