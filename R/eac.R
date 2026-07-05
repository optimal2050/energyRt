# =========================================================================== #
# eac.R  —  Equivalent Annual Cost (EAC) of investment.
#
# pXEac(key, region, year) = pXInvcost annuitised by the capital-recovery factor
# of the region/year discount rate over the object's operational life:
#   eac = invcost / olife                          (discount 0, finite olife)
#   eac = invcost * d(1+d)^n / ((1+d)^n - 1)        (discount d>0, finite olife n)
#   eac = invcost * d                               (discount d>0, infinite olife)
#
# Faithful port of the legacy `.obj2modInp` EAC blocks (obj2modInp.R:1858
# technology, :974 storage). The live generic `ob2mi` slot loop copies the RAW
# invcost into pXEac (slot meta colName "invcost"), so EAC equals invcost until
# this pass overwrites it with the annuitised value. The active EAC equations use
# pXEac directly (e.g. eqTechEac: vTechEac = pTechEac * vTechCap), so without this
# the objective over-counts capital cost by ~1/CRF.
#
# Runs after interpolate_parameters (reads the interpolated pXInvcost / pDiscount /
# pXOlife) and before the value recipe (mTradeEac reads pTradeEac's value domain).
# At this stage tech/storage cost params are still region-FOLDED (region = NA
# wildcard) while pDiscount is region-explicit, so the join must be wildcard-aware.
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

# Compute one family's EAC param from its invcost / discount / olife inputs.
.eac_one <- function(scen, key, inv_par, olife_par, new_par, eac_par) {
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

  # discount per (region, year); absent -> 0 (straight-line annuity).
  disc <- if (is.null(P[["pDiscount"]])) NULL else
    as.data.frame(get_data_slot(P[["pDiscount"]]))
  if (!is.null(disc) && nrow(disc) > 0) {
    names(disc)[names(disc) == "value"] <- "discount"
    df <- .eac_merge(df, disc, "discount", c("region", "year"))
  } else {
    df$discount <- NA_real_
  }
  df$discount[is.na(df$discount)] <- 0

  # operational life per (key[, region]); absent -> Inf (no capital recovery).
  ol <- if (is.null(P[[olife_par]])) NULL else
    as.data.frame(get_data_slot(P[[olife_par]]))
  if (!is.null(ol) && nrow(ol) > 0) {
    names(ol)[names(ol) == "value"] <- "olife"
    df <- .eac_merge(df, ol, "olife", c(key, "region"))
  } else {
    df$olife <- NA_real_
  }
  df$olife[is.na(df$olife)] <- Inf

  # Capital-recovery factor (legacy obj2modInp.R:1873-1881).
  df$eac <- df$invcost / df$olife
  fl <- df$discount != 0 & is.finite(df$olife)
  df$eac[fl] <- df$invcost[fl] *
    (df$discount[fl] * (1 + df$discount[fl])^df$olife[fl] /
       ((1 + df$discount[fl])^df$olife[fl] - 1))
  fl <- df$discount != 0 & is.infinite(df$olife)
  df$eac[fl] <- df$invcost[fl] * df$discount[fl]

  dims <- P[[eac_par]]@dimSets
  out <- unique(df[, c(dims, "eac"), drop = FALSE])
  names(out)[names(out) == "eac"] <- "value"

  scen@modInp@parameters[[eac_par]] <- .fold_write_back(P[[eac_par]], out)
  scen
}

# NOTE: a directly-supplied `@invcost$eac` column (legacy "temporary fix"
# override) is not honoured here; the live slot loop reads colName "invcost" only,
# so direct-eac is pre-existing-unsupported, not a regression from this pass.
compute_eac_parameters <- function(scen) {
  scen <- .eac_one(scen, "tech",  "pTechInvcost",    "pTechOlife",    "mTechNew",    "pTechEac")
  scen <- .eac_one(scen, "stg",   "pStorageInvcost", "pStorageOlife", "mStorageNew", "pStorageEac")
  scen <- .eac_one(scen, "trade", "pTradeInvcost",   "pTradeOlife",   "mTradeNew",   "pTradeEac")
  scen
}
