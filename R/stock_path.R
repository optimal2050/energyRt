# =========================================================================== #
# The exogenous stock PATH: commissioning stream + per-period survival
#
# `stock(y)` as a user writes it carries TWO meanings at once -- the original
# endowment and the fleet still standing -- and that conflation is what made
# requirement 1 (exogenous phase-out) and requirement 2 (endogenous early
# retirement) mutually exclusive. The old pair
#
#     retiredCum(y) <= stock(y)          # reads stock as the ORIGINAL endowment
#     retStock(y)*plen = cum(y)-cum(y-1) # flow >= 0, so cum is NON-DECREASING
#
# forced retiredCum to zero in EVERY year whenever the schedule declined to
# zero, because the cap in the final year bound the whole path. A fleet could
# be scheduled out, or optimised out, never both.
#
# Splitting the path separates them:
#
#     stockNew(y)  = max(0, stock(y) - stock(y-1)),   = stock(first) at the start
#     stockSurv(y) = (stock(y) - stockNew(y)) / stock(y-1),   1 when stock(y-1)=0
#
# so that the fleet obeys a RECURSIVE balance (eqXStockCap) with no cumulative
# variable, and therefore no non-decreasing trap:
#
#     stockCap(y) = stockSurv(y)*stockCap(y-1) + stockNew(y) - retStock(y)*plen(y)
#
# `stockSurv` thins whatever is ACTUALLY standing, so it can never demand more
# capacity than exists and can never make the model infeasible. It also uses
# only CONSECUTIVE milestones, so both parameters -- and hence the physics --
# are invariant under a change of base year. Retiring against a `stock0` taken
# from the first modelled year was not.
#
# A constant fleet gives stockSurv = 1 and stockNew = 0 after the first year,
# which reproduces the previous behaviour exactly.
# =========================================================================== #

.stock_path_one <- function(scen, key, stock_par, new_par, surv_par) {
  P <- scen@modInp@parameters
  if (is.null(P[[stock_par]]) || is.null(P[[new_par]]) || is.null(P[[surv_par]]))
    return(scen)
  st <- as.data.frame(get_data_slot(P[[stock_par]]))
  if (is.null(st) || nrow(st) == 0 || !("value" %in% names(st))) return(scen)
  names(st)[names(st) == "value"] <- "stock"
  st <- st[!is.na(st$stock), , drop = FALSE]
  if (nrow(st) == 0) return(scen)

  grp <- intersect(c(key, "region"), names(st))

  # `stock` is a numpar with defVal 0, so interpolation DROPS every row whose
  # value is zero -- including the year a declining schedule finally reaches
  # nothing. Read sparsely, a fleet scheduled 100 -> 80 -> 0 would look like
  # 100 -> 80 -> (unspecified, hence unchanged) and never leave. The equations
  # always read the parameter densely against its default, so the derivation
  # must too: rebuild the full milestone grid per key and fill the gaps with 0.
  yrs <- sort(unique(as.integer(scen@modInp@sets$year)))
  if (length(yrs) == 0) return(scen)
  st <- merge(unique(st[, grp, drop = FALSE]),
              data.frame(year = yrs), by = NULL) |>
    merge(st, by = c(grp, "year"), all.x = TRUE)
  st$stock[is.na(st$stock)] <- 0
  d <- dplyr::as_tibble(st) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(grp, "year")))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
    dplyr::mutate(
      .prev = dplyr::lag(.data$stock),
      # first milestone of the span: the whole fleet is commissioned there
      .new  = ifelse(is.na(.data$.prev), .data$stock,
                     pmax(0, .data$stock - .data$.prev)),
      # nothing to survive from an empty (or absent) previous fleet
      .surv = ifelse(is.na(.data$.prev) | .data$.prev <= 0, 1,
                     (.data$stock - .data$.new) / .data$.prev)) |>
    dplyr::ungroup() |>
    as.data.frame()

  keys <- c(grp, "year")
  # A dense grid was needed to READ the schedule; writing it back dense would
  # bloat every model that has a stock. Both parameters carry the neutral value
  # as their default (0 commissioned, all of it surviving), so only departures
  # from it need to be written.
  for (spec in list(list(new_par, ".new", 0), list(surv_par, ".surv", 1))) {
    o <- d[, c(keys, spec[[2]]), drop = FALSE]
    names(o)[names(o) == spec[[2]]] <- "value"
    o <- o[!is.na(o$value) & o$value != spec[[3]], , drop = FALSE]
    scen@modInp@parameters[[spec[[1]]]] <-
      .fold_write_back(P[[spec[[1]]]], o)
  }
  scen
}

#' Derive the exogenous stock path parameters
#'
#' Runs after interpolation, next to `compute_eac_parameters()`, and turns each
#' class's `stock` level into a commissioning stream and a per-period survival
#' share. See the file header for why the split is necessary.
#'
#' @param scen scenario object.
#' @return the scenario with `pXStockNew` / `pXStockSurv` filled.
#' @keywords internal
compute_stock_parameters <- function(scen) {
  fam <- list(
    c("tech",  "pTechStock"),
    c("stg",   "pStorageOutStock"),
    c("stg",   "pStorageInpStock"),
    c("stg",   "pStorageStgStock"),
    c("trade", "pTradeStock"))
  for (f in fam) {
    scen <- .stock_path_one(scen, f[[1]], f[[2]],
                            paste0(f[[2]], "New"), paste0(f[[2]], "Surv"))
  }
  scen
}
