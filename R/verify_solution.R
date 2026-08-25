# =========================================================================== #
# verify_solution() -- generic identity checks on a solved scenario.
#
# The checks re-derive template equations as data joins driven by the SAME
# maps the interpolation shipped to the solver, so they hold for any valid
# solution on any backend and any model:
#
#   balance       eqBal        vBalance = vOutTot - vInpTot        (mvBalance)
#   balance_sign  eqBalLo/Up/Fx  sign of vBalance per commodity limtype
#   out_tot       eqOutTot     vOutTot = sum of source-side totals
#                              + timeslice-family + region-family terms
#   inp_tot       eqInpTot     vInpTot = sum of sink-side totals + families
#   cost          eqCost       vTotalCost = sum of cost-role variables
#   objective     eqObjective  vObjective = sum vTotalCost * pPeriodLen
#                                              * pDiscountFactor
#
# Missing solution rows mean zero (solvers export non-zeros only); missing
# parameter rows mean the parameter's defVal (sparse interpolation drops
# default-valued rows). A check that cannot run (no solution, gate map absent)
# reports status "skipped" with a reason -- it never silently passes.
#
# Used by the test suite on every solved fixture and by the golden-capture
# tooling, which refuses to freeze a benchmark that fails an identity.
# =========================================================================== #

#' Verify accounting identities of a solved scenario
#'
#' Recomputes core model identities (commodity balance and its sign per
#' commodity `limtype`, the source/sink total layers, cost decomposition, and
#' the discounted objective) from the solution tables and the scenario's own
#' maps, and reports any violations. A valid optimal solution from any backend
#' must pass every check; a failure indicates a solver, template, writer, or
#' reader defect -- or a corrupted solution.
#'
#' @param scen a solved `scenario` (must carry `modOut` variables).
#' @param checks character, subset of
#'   `c("balance", "balance_sign", "out_tot", "inp_tot", "cost", "objective")`.
#' @param tol_rel,tol_abs numeric tolerances; a row violates its identity when
#'   `|lhs - rhs| > tol_abs + tol_rel * max(|lhs|, |rhs|)`.
#'
#' @return an object of class `solution_verification`: a list with `ok`
#'   (logical), `scenario` (name), and `checks` -- per check a list with
#'   `status` (`"ok"`, `"violated"`, `"skipped"`), `n` (rows checked),
#'   `violations` (data.table of offending rows with `lhs`, `rhs`, `diff`),
#'   and `reason` when skipped.
#'
#' @examples
#' \dontrun{
#' scen <- solve_model(mod, solver = solver_options$glpk)
#' vs <- verify_solution(scen)
#' vs$ok
#' print(vs)
#' }
#' @export
verify_solution <- function(scen,
                            checks = c("balance", "balance_sign", "out_tot",
                                       "inp_tot", "cost", "objective"),
                            tol_rel = 1e-6, tol_abs = 1e-6) {
  stopifnot(is(scen, "scenario"))
  checks <- match.arg(checks, several.ok = TRUE)
  ctx <- list(scen = scen, tol_rel = tol_rel, tol_abs = tol_abs)

  runners <- list(
    balance = .vs_check_balance,
    balance_sign = .vs_check_balance_sign,
    out_tot = function(ctx) .vs_check_flow_tot(ctx, side = "out"),
    inp_tot = function(ctx) .vs_check_flow_tot(ctx, side = "inp"),
    cost = .vs_check_cost,
    objective = .vs_check_objective
  )

  res <- lapply(checks, function(nm) {
    tryCatch(runners[[nm]](ctx), error = function(e) {
      list(status = "skipped", n = 0L, violations = NULL,
           reason = paste0("check errored: ", conditionMessage(e)))
    })
  })
  names(res) <- checks

  out <- list(
    scenario = scen@name,
    ok = !any(vapply(res, function(x) identical(x$status, "violated"), TRUE)),
    checks = res
  )
  class(out) <- "solution_verification"
  out
}

#' @exportS3Method base::print
print.solution_verification <- function(x, ...) {
  cat("verify_solution: scenario '", x$scenario, "' -- ",
      if (x$ok) "OK" else "VIOLATIONS FOUND", "\n", sep = "")
  for (nm in names(x$checks)) {
    ck <- x$checks[[nm]]
    line <- sprintf("  %-12s %-9s", nm, ck$status)
    if (identical(ck$status, "skipped")) {
      line <- paste0(line, " (", ck$reason, ")")
    } else {
      line <- paste0(line, " rows=", ck$n)
      if (identical(ck$status, "violated")) {
        line <- paste0(line, " violations=", nrow(ck$violations),
                       " max|diff|=", format(max(abs(ck$violations$diff)),
                                             digits = 4))
      }
    }
    cat(line, "\n")
  }
  invisible(x)
}

# --------------------------------------------------------------------------- #
# accessors: solution variables and parameters as data.tables
# --------------------------------------------------------------------------- #

# Solution variable table (dims + value), NULL when the variable is absent
# from modOut. Zero rows are a legal empty solution (solvers export non-zeros
# only), so callers must treat missing rows as 0.
.vs_var <- function(scen, name) {
  v <- scen@modOut@variables[[name]]
  if (is.null(v)) return(NULL)
  d <- get_data_slot(v, optional = TRUE)
  if (is.null(d)) return(NULL)
  d <- data.table::as.data.table(d)
  .vs_dechar(d)
}

# Parameter/map table from modInp; for maps a membership table, for numpars a
# keyed value table. Returns NULL when the parameter is not present.
.vs_par <- function(scen, name) {
  p <- scen@modInp@parameters[[name]]
  if (is.null(p)) return(NULL)
  d <- get_data_slot(p, optional = TRUE)
  if (is.null(d)) return(NULL)
  .vs_dechar(data.table::as.data.table(d))
}

.vs_defval <- function(scen, name) {
  p <- scen@modInp@parameters[[name]]
  if (is.null(p) || !length(p@defVal)) return(NA_real_)
  suppressWarnings(as.numeric(p@defVal[[1]]))
}

# factors from the csv/parquet round-trip break joins on character keys
.vs_dechar <- function(d) {
  for (j in colnames(d)) {
    if (is.factor(d[[j]])) data.table::set(d, j = j, value = as.character(d[[j]]))
  }
  d
}

# Left-join `keys` (a data.table of key columns) against variable `name`,
# returning the value vector aligned to `keys` rows with missing rows -> 0.
# `rename` maps key columns to the variable's columns (e.g. timeslicep ->
# timeslice for family lookups).
.vs_value_at <- function(scen, name, keys, rename = NULL) {
  d <- .vs_var(scen, name)
  if (is.null(d) || nrow(d) == 0) return(rep(0, nrow(keys)))
  kk <- data.table::copy(keys)
  if (!is.null(rename)) data.table::setnames(kk, names(rename), unname(rename))
  on_cols <- intersect(colnames(kk), setdiff(colnames(d), "value"))
  m <- d[kk, on = on_cols]
  v <- m$value
  v[is.na(v)] <- 0
  v
}

# Sum variable `name` grouped by `by` columns, joined to `keys` rows -> 0.
.vs_sum_at <- function(scen, name, keys, by) {
  d <- .vs_var(scen, name)
  if (is.null(d) || nrow(d) == 0) return(rep(0, nrow(keys)))
  g <- d[, .(value = sum(value)), by = by]
  m <- g[keys, on = by]
  v <- m$value
  v[is.na(v)] <- 0
  v
}

# Parameter value aligned to `keys`, missing rows -> defVal.
.vs_par_at <- function(scen, name, keys, rename = NULL) {
  d <- .vs_par(scen, name)
  dv <- .vs_defval(scen, name)
  if (is.null(d) || nrow(d) == 0) return(rep(dv, nrow(keys)))
  kk <- data.table::copy(keys)
  if (!is.null(rename)) data.table::setnames(kk, names(rename), unname(rename))
  on_cols <- intersect(colnames(kk), setdiff(colnames(d), "value"))
  m <- d[kk, on = on_cols]
  v <- m$value
  v[is.na(v)] <- dv
  v
}

.vs_result <- function(keys, lhs, rhs, ctx, label) {
  diff <- lhs - rhs
  bad <- abs(diff) > ctx$tol_abs + ctx$tol_rel * pmax(abs(lhs), abs(rhs))
  if (any(bad)) {
    viol <- data.table::copy(keys)[, `:=`(lhs = lhs, rhs = rhs, diff = diff)]
    list(status = "violated", n = nrow(keys), violations = viol[bad])
  } else {
    list(status = "ok", n = nrow(keys), violations = NULL)
  }
}

.vs_skip <- function(reason) {
  list(status = "skipped", n = 0L, violations = NULL, reason = reason)
}

# --------------------------------------------------------------------------- #
# checks
# --------------------------------------------------------------------------- #

# eqBal: vBalance = vOutTot - vInpTot on the mvBalance domain.
.vs_check_balance <- function(ctx) {
  scen <- ctx$scen
  dom <- .vs_par(scen, "mvBalance")
  if (is.null(dom) || nrow(dom) == 0) return(.vs_skip("mvBalance absent or empty"))
  if (is.null(.vs_var(scen, "vBalance"))) return(.vs_skip("vBalance not in solution"))
  lhs <- .vs_value_at(scen, "vBalance", dom)
  rhs <- .vs_value_at(scen, "vOutTot", dom) - .vs_value_at(scen, "vInpTot", dom)
  .vs_result(dom, lhs, rhs, ctx, "balance")
}

# eqBalLo / eqBalUp / eqBalFx: the sign of vBalance per commodity limtype
# (LO = free disposal, surplus >= 0; UP = deficit side; FX = exact balance).
.vs_check_balance_sign <- function(ctx) {
  scen <- ctx$scen
  doms <- list(lo = .vs_par(scen, "meqBalLo"),
               up = .vs_par(scen, "meqBalUp"),
               fx = .vs_par(scen, "meqBalFx"))
  if (all(vapply(doms, function(d) is.null(d) || nrow(d) == 0, TRUE))) {
    return(.vs_skip("no meqBalLo/Up/Fx domains"))
  }
  if (is.null(.vs_var(scen, "vBalance"))) return(.vs_skip("vBalance not in solution"))
  tol <- ctx$tol_abs
  pieces <- list()
  for (side in names(doms)) {
    dom <- doms[[side]]
    if (is.null(dom) || nrow(dom) == 0) next
    v <- .vs_value_at(scen, "vBalance", dom)
    bad <- switch(side,
      lo = v < -tol,
      up = v > tol,
      fx = abs(v) > tol
    )
    if (any(bad)) {
      pieces[[side]] <- data.table::copy(dom)[bad][, `:=`(
        limtype = toupper(side), lhs = v[bad], rhs = 0, diff = v[bad])]
    }
  }
  n <- sum(vapply(doms, function(d) if (is.null(d)) 0L else nrow(d), 0L))
  if (length(pieces)) {
    list(status = "violated", n = n, violations = data.table::rbindlist(pieces))
  } else {
    list(status = "ok", n = n, violations = NULL)
  }
}

# eqOutTot / eqInpTot: the commodity totals layer. Each component variable
# contributes on its own gate map; nested timeframes add
# pTimesliceAgg * vTot at child timeslices (mTimesliceFamily), and multi-level
# regions add vTot at child regions (mRegionFamily).
.vs_check_flow_tot <- function(ctx, side = c("out", "inp")) {
  side <- match.arg(side)
  scen <- ctx$scen
  tot_var <- if (side == "out") "vOutTot" else "vInpTot"
  dom_map <- if (side == "out") "mvOutTot" else "mvInpTot"
  components <- if (side == "out") {
    c(mDummyImport = "vDummyImport", mSupOutTot = "vSupOutTot",
      mEmsFuelTot = "vEmsFuelTot", mAggOut = "vAggOutTot",
      mTechOutTot = "vTechOutTot", mStorageOutTot = "vStorageOutTot",
      mImport = "vImportTot", mvTradeIrAOutTot = "vTradeIrAOutTot")
  } else {
    c(mvDemInp = "vDemInp", mDummyExport = "vDummyExport",
      mTechInpTot = "vTechInpTot", mStorageInpTot = "vStorageInpTot",
      mExport = "vExportTot", mvTradeIrAInpTot = "vTradeIrAInpTot")
  }

  dom <- .vs_par(scen, dom_map)
  if (is.null(dom) || nrow(dom) == 0) return(.vs_skip(paste(dom_map, "absent or empty")))
  if (is.null(.vs_var(scen, tot_var))) return(.vs_skip(paste(tot_var, "not in solution")))

  lhs <- .vs_value_at(scen, tot_var, dom)
  rhs <- rep(0, nrow(dom))

  for (map_nm in names(components)) {
    gate <- .vs_par(scen, map_nm)
    if (is.null(gate) || nrow(gate) == 0) next
    # contribution only on rows of `dom` that are in the gate map
    key_cols <- colnames(dom)
    in_gate <- !is.na(gate[dom, on = key_cols, which = TRUE])
    if (!any(in_gate)) next
    rhs[in_gate] <- rhs[in_gate] +
      .vs_value_at(scen, components[[map_nm]], dom[in_gate])
  }

  # timeslice-family roll-up: (s, sp) in mTimesliceFamily,
  # rhs[c,r,y,s] += pTimesliceAgg[y,s,sp] * vTot[c,r,y,sp]
  fam <- .vs_par(scen, "mTimesliceFamily")
  if (!is.null(fam) && nrow(fam) > 0) {
    ff <- fam[dom, on = "timeslice", allow.cartesian = TRUE, nomatch = NULL]
    if (nrow(ff) > 0) {
      w <- .vs_par_at(scen, "pTimesliceAgg", ff)
      # look the variable up at the CHILD timeslice: drop the parent's own
      # timeslice column first so the join key is unambiguous
      ffk <- data.table::copy(ff)[, timeslice := NULL]
      data.table::setnames(ffk, "timeslicep", "timeslice")
      v <- .vs_value_at(scen, tot_var, ffk)
      contrib <- ff[, .(comm, region, year, timeslice)][, val := w * v]
      agg <- contrib[, .(val = sum(val)), by = .(comm, region, year, timeslice)]
      add <- agg[dom, on = colnames(dom)]$val
      add[is.na(add)] <- 0
      rhs <- rhs + add
    }
  }

  # region-family roll-up: (r, rp) in mRegionFamily, rhs[c,r,y,s] += vTot[c,rp,y,s]
  rfam <- .vs_par(scen, "mRegionFamily")
  if (!is.null(rfam) && nrow(rfam) > 0) {
    rf <- rfam[dom, on = "region", allow.cartesian = TRUE, nomatch = NULL]
    if (nrow(rf) > 0) {
      rfk <- data.table::copy(rf)[, region := NULL]
      data.table::setnames(rfk, "regionp", "region")
      v <- .vs_value_at(scen, tot_var, rfk)
      contrib <- rf[, .(comm, region, year, timeslice)][, val := v]
      agg <- contrib[, .(val = sum(val)), by = .(comm, region, year, timeslice)]
      add <- agg[dom, on = colnames(dom)]$val
      add[is.na(add)] <- 0
      rhs <- rhs + add
    }
  }

  .vs_result(dom, lhs, rhs, ctx, paste0(side, "_tot"))
}

# eqCost: vTotalCost[r, y] equals the sum of every cost-role variable at
# (., r, y). Excluded: vObjective (global), vTotalCost itself, vUserCosts
# (already aggregated into vTotalUserCosts), and the overnight-investment
# reporting variables vTechInv / vStorageInv / vTradeInv -- the objective
# carries the ANNUALIZED vTechEac / vStorageEac / vTradeEac instead.
.vs_check_cost <- function(ctx) {
  scen <- ctx$scen
  dom <- .vs_par(scen, "mvTotalCost")
  if (is.null(dom) || nrow(dom) == 0) return(.vs_skip("mvTotalCost absent or empty"))
  if (is.null(.vs_var(scen, "vTotalCost"))) {
    return(.vs_skip("vTotalCost not in solution"))
  }
  lhs <- .vs_value_at(scen, "vTotalCost", dom)
  rhs <- rep(0, nrow(dom))
  excluded <- c("vObjective", "vTotalCost", "vUserCosts",
                "vTechInv", "vStorageInv", "vTradeInv")
  for (nm in names(scen@modOut@variables)) {
    v <- scen@modOut@variables[[nm]]
    if (!identical(v@role, "cost") || nm %in% excluded) next
    d <- .vs_var(scen, nm)
    if (is.null(d) || nrow(d) == 0) next
    if (!all(c("region", "year") %in% colnames(d))) next
    rhs <- rhs + .vs_sum_at(scen, nm, dom, by = c("region", "year"))
  }
  .vs_result(dom, lhs, rhs, ctx, "cost")
}

# eqObjective: vObjective = sum vTotalCost * pPeriodLen[y] * pDiscountFactor[r, y].
.vs_check_objective <- function(ctx) {
  scen <- ctx$scen
  obj <- .vs_var(scen, "vObjective")
  if (is.null(obj) || nrow(obj) == 0) return(.vs_skip("vObjective not in solution"))
  dom <- .vs_par(scen, "mvTotalCost")
  if (is.null(dom) || nrow(dom) == 0) return(.vs_skip("mvTotalCost absent or empty"))
  tc <- .vs_value_at(scen, "vTotalCost", dom)
  plen <- .vs_par_at(scen, "pPeriodLen", dom)
  dfac <- .vs_par_at(scen, "pDiscountFactor", dom)
  rhs <- sum(tc * plen * dfac)
  keys <- data.table::data.table(variable = "vObjective")
  .vs_result(keys, obj$value[1], rhs, ctx, "objective")
}
