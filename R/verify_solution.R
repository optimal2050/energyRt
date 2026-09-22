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
#' @param checks character. Individual check names, or the sentinels
#'   `"default"` (the cheap set, run everywhere) and `"all"`. See
#'   [verify_checks()] for the vocabulary, each check's group, and which data
#'   path it derives its expectation from. Defaults to `"default"`.
#' @param tol_rel,tol_abs numeric tolerances; a row violates its identity when
#'   `|lhs - rhs| > tol_abs + tol_rel * max(|lhs|, |rhs|)`.
#' @param verbose logical, report each check and its elapsed time as it runs.
#'   The opt-in checks join the whole solution table on a large model, where
#'   silence for minutes is not useful.
#'
#' @return an object of class `solution_verification`: a list with `ok`
#'   (logical), `scenario` (name), `checks` -- per check a list with
#'   `status` (`"ok"`, `"violated"`, `"skipped"`), `n` (rows checked),
#'   `violations` (data.table of offending rows with `lhs`, `rhs`, `diff`),
#'   `stats`, `reason` when skipped, and `report` for report-only checks --
#'   and `divergence`.
#'
#'   `print()` takes `detail = c("full", "issues")`; `"issues"` shows only
#'   violated, skipped and reporting checks. The full set requested is always
#'   computed either way -- an issues-only *compute* mode could hide a check
#'   that silently did not run.
#'
#'   `divergence` is a data.frame, one row per check, worst first: `status`,
#'   `n_compared`, `n_violated`, `max_abs`, `median_abs`, `mean_abs` and
#'   `max_rel`. It summarises how far each identity came from closing **even
#'   when the check passed**, so tolerances can be set from the noise the
#'   backends actually produce rather than guessed. `max_abs` is what a
#'   `tol_abs` must clear; `median_abs` beside it separates one bad cell from a
#'   uniformly loose check. Aggregate identities (`objective`, `cost`) sum over
#'   the whole model, so their absolute divergence grows with model size while
#'   `max_rel` stays flat -- which is why the relative term carries the
#'   tolerance on large models.
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
                            checks = "default",
                            tol_rel = 1e-6, tol_abs = 1e-6,
                            verbose = FALSE) {
  stopifnot(is(scen, "scenario"))
  # NB the vocabulary is passed EXPLICITLY rather than left as the formal
  # default: `match.arg(several.ok = TRUE)` returns every choice when the
  # argument is its own default, which would silently make "all" the default
  # set once an opt-in check exists.
  checks <- match.arg(checks, c("default", "all", .vs_check_names()),
                      several.ok = TRUE)
  checks <- .vs_expand_checks(checks)
  ctx <- list(scen = scen, tol_rel = tol_rel, tol_abs = tol_abs)

  reg <- .vs_registry()
  # The object-path checks read `scen@model@data` and would happily run on an
  # unsolved scenario -- which would make `verify_solution()` return ok = TRUE
  # for a scenario carrying no solution at all. That is the "nothing verified"
  # defect `ok`/`n_ran` exist to prevent, so every check here is gated on the
  # solution being present. Checking inputs WITHOUT solving is `validate_*`'s
  # half of the verb split, not `verify_*`'s.
  has_sol <- .vs_has_solution(scen)
  res <- lapply(checks, function(nm) {
    if (verbose) {
      message("verify_solution: ", nm, " (", reg[[nm]]$group, ") ... ",
              appendLF = FALSE)
    }
    t0 <- proc.time()[["elapsed"]]
    r <- if (isTRUE(reg[[nm]]$needs_solution) && !has_sol) {
      .vs_skip("scenario carries no solution")
    } else tryCatch(reg[[nm]]$run(ctx), error = function(e) {
      list(status = "skipped", n = 0L, violations = NULL,
           reason = paste0("check errored: ", conditionMessage(e)))
    })
    if (verbose) {
      message(r$status %||% "?", " (",
              format(proc.time()[["elapsed"]] - t0, digits = 2), "s)")
    }
    r
  })
  names(res) <- checks

  .status <- vapply(res, function(x) x$status %||% "skipped", "")
  .n_ran <- sum(.status == "ok")

  # `ok` requires that the identities actually RAN. Counting only "violated"
  # made an empty solution pass: every check skips for want of rows, nothing is
  # violated, and `ok` came back TRUE -- which is how a scenario of zeros
  # reported OPTIMAL sailed through `expect_true(vs$ok)`. A verification that
  # verified nothing is not a pass.
  out <- list(
    scenario = scen@name,
    ok = sum(.status == "violated") == 0L && .n_ran > 0L,
    n_ran = .n_ran,
    n_skipped = sum(.status == "skipped"),
    checks = res,
    divergence = .vs_divergence(res)
  )
  class(out) <- "solution_verification"
  out
}

# --------------------------------------------------------------------------- #
# check registry
#
# `path` records which data a check derives its expectation from, and is the
# load-bearing distinction: "modInp" checks re-derive equations from the SAME
# maps that built them, so they cannot detect a bug in map CREATION -- a
# missing domain verifies clean. Only "objects" checks, which go back to
# `scen@model@data`, can.
#
# `tier` splits cheap from expensive. The defaults must stay in seconds: on a
# 41-node full-year model the opt-in checks join the whole solution table.
# --------------------------------------------------------------------------- #
.vs_registry <- function() {
  list(
    balance = list(
      run = .vs_check_balance, group = "equations", path = "modInp",
      tier = "default", needs_solution = TRUE, desc = "vBalance = vOutTot - vInpTot"),
    balance_sign = list(
      run = .vs_check_balance_sign, group = "equations", path = "modInp",
      tier = "default", needs_solution = TRUE, desc = "sign of vBalance per commodity limtype"),
    out_tot = list(
      run = function(ctx) .vs_check_flow_tot(ctx, side = "out"),
      group = "equations", path = "modInp", tier = "default", needs_solution = TRUE,
      desc = "vOutTot = sum of source-side totals"),
    inp_tot = list(
      run = function(ctx) .vs_check_flow_tot(ctx, side = "inp"),
      group = "equations", path = "modInp", tier = "default", needs_solution = TRUE,
      desc = "vInpTot = sum of sink-side totals"),
    cost = list(
      run = .vs_check_cost, group = "equations", path = "modInp",
      tier = "default", needs_solution = TRUE, desc = "vTotalCost = sum of cost-role variables"),
    objective = list(
      run = .vs_check_objective, group = "equations", path = "modInp",
      tier = "default", needs_solution = TRUE, desc = "discounted objective"),
    positivity = list(
      run = .vs_check_positivity, group = "equations", path = "registry",
      tier = "default", needs_solution = TRUE,
      desc = "variables declared positive hold no negative values"),
    inputs_present = list(
      run = .vs_check_inputs_present, group = "inputs", path = "objects",
      tier = "default", needs_solution = TRUE,
      desc = "every declared object reaches the sets and the parameters"),
    units = list(
      run = .vs_check_units, group = "units", path = "objects",
      tier = "default", needs_solution = TRUE,
      desc = "unresolved unit report (never fails)"),
    inputs_values = list(
      run = .vs_check_inputs_values, group = "inputs", path = "objects",
      tier = "optin", needs_solution = TRUE,
      desc = "declared values survive interpolation into modInp"),
    inputs_bounds = list(
      run = .vs_check_inputs_bounds, group = "inputs", path = "objects",
      tier = "optin", needs_solution = TRUE,
      desc = "declared bounds are honoured by the solution"),
    storage_dynamics = list(
      run = .vs_check_storage_dynamics, group = "equations", path = "modInp",
      tier = "optin", needs_solution = TRUE,
      desc = "eqStorageLevel, including the fullYear cycle closure"),
    capacity_accumulation = list(
      run = .vs_check_capacity_accumulation, group = "equations",
      path = "modInp", tier = "optin", needs_solution = TRUE,
      desc = "eqTechCap: stock + vintages over the olife window, less retirements"),
    eac = list(
      run = .vs_check_eac, group = "equations", path = "modInp",
      tier = "optin", needs_solution = TRUE,
      desc = "eqTechEac: vTechEac = pTechEac x vTechCap"),
    flow_chain = list(
      run = .vs_check_flow_chain, group = "equations", path = "modInp",
      tier = "optin", needs_solution = TRUE,
      desc = "input -> use -> activity -> output, incl. grouped inputs")
  )
}

.vs_check_names <- function() names(.vs_registry())

# "default" and "all" are sentinels, so the cheap set can grow without every
# caller restating it and the nightly tier can ask for everything by name.
# Explicit check names keep working exactly as before.
.vs_expand_checks <- function(checks) {
  reg <- .vs_registry()
  out <- character()
  for (nm in checks) {
    out <- c(out, switch(nm,
      default = names(reg)[vapply(reg, function(x) x$tier == "default",
                                  logical(1))],
      all = names(reg),
      nm))
  }
  unique(out)
}

#' Checks available to `verify_solution()`
#'
#' @return a data.frame with one row per check: `check`, `group`, `path`
#'   (`"modInp"`, `"objects"` or `"registry"` -- which data the expectation is
#'   derived from) and `tier` (`"default"` or `"optin"`).
#' @export
verify_checks <- function() {
  reg <- .vs_registry()
  data.frame(
    check = names(reg),
    group = vapply(reg, function(x) x$group, ""),
    path  = vapply(reg, function(x) x$path, ""),
    tier  = vapply(reg, function(x) x$tier, ""),
    desc  = vapply(reg, function(x) x$desc, ""),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

# One row per check, worst first. Reports how far each identity came from
# closing EVEN WHEN IT PASSED, so the tolerances can be set from the observed
# noise floor instead of guessed. `max_abs` orders it because that is what a
# tolerance has to clear; `median_abs` next to it separates one bad cell from a
# check that is uniformly loose.
.vs_divergence <- function(res) {
  rows <- lapply(names(res), function(nm) {
    ck <- res[[nm]]
    st <- ck$stats %||% .vs_stats(numeric(0))
    data.frame(
      check       = nm,
      status      = ck$status %||% "skipped",
      n_compared  = st$n_cmp,
      n_violated  = if (is.null(ck$violations)) 0L else nrow(ck$violations),
      max_abs     = st$max_abs,
      median_abs  = st$median_abs,
      mean_abs    = st$mean_abs,
      max_rel     = st$max_rel,
      max_scaled  = st$max_scaled %||% NA_real_,
      stringsAsFactors = FALSE
    )
  })
  d <- do.call(rbind, rows)
  # NA (skipped) last, worst-diverging first
  d[order(is.na(d$max_abs), -d$max_abs, na.last = TRUE), , drop = FALSE]
}

#' @exportS3Method base::print
print.solution_verification <- function(x, detail = c("full", "issues"), ...) {
  detail <- match.arg(detail)
  cat("verify_solution: scenario '", x$scenario, "' -- ",
      if (x$ok) "OK" else if ((x$n_ran %||% 1L) == 0L) "NOTHING VERIFIED"
      else "VIOLATIONS FOUND", "
", sep = "")
  if ((x$n_skipped %||% 0L) > 0L) {
    cat("  ", x$n_ran %||% NA, " check(s) ran, ", x$n_skipped,
        " skipped
", sep = "")
  }
  # `detail` selects the VIEW; every requested check was computed either way.
  # An issues-only compute mode could hide a check that silently did not run,
  # which is the defect `ok`/`n_ran` were fixed for.
  nms <- names(x$checks)
  if (identical(detail, "issues")) {
    keep <- vapply(x$checks, function(ck) {
      identical(ck$status, "violated") || identical(ck$status, "skipped") ||
        length(ck$report) > 0L
    }, logical(1))
    nms <- nms[keep]
    if (!length(nms)) cat("  (no issues; detail = \"full\" shows all checks)\n")
  }
  for (nm in nms) {
    ck <- x$checks[[nm]]
    line <- sprintf("  %-16s %-9s", nm, ck$status)
    if (identical(ck$status, "skipped")) {
      line <- paste0(line, " (", ck$reason, ")")
    } else {
      line <- paste0(line, " rows=", ck$n)
      if (identical(ck$status, "violated")) {
        line <- paste0(line, " violations=", nrow(ck$violations),
                       " max|diff|=", format(max(abs(ck$violations$diff)),
                                             digits = 4))
      }
      if (length(ck$report)) {
        line <- paste0(line, " reported=", nrow(ck$report))
      }
    }
    cat(line, "\n")
    # a report-only check should say WHERE the gap is, not only how big
    if (!is.null(ck$coverage) && nrow(ck$coverage)) {
      cv <- ck$coverage[ck$coverage$unresolved > 0, , drop = FALSE]
      if (nrow(cv)) {
        cat("      ", paste(sprintf("%s %d/%d", cv$class, cv$unresolved,
                                    cv$params), collapse = "  "), "
",
            sep = "")
      }
    }
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
  # A folded parameter stores NA wildcards; the identities need every member
  # explicit, as the solver saw them.
  fi <- p@misc[["fold_info"]]
  if (!is.null(fi) && isTRUE(fi[["folded"]])) {
    d <- unfold_scenario_parameter(scen, p)
  }
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
  .a <- .vs_align_keys(d, kk, on_cols); d <- .a$d; kk <- .a$kk
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
# Align the types of shared join keys. A parameter can carry `year` as
# character where its gating map has integer (folding introduces character
# artificial members), and data.table refuses the join outright. Coerce the
# differing key to character on both sides; keys are identifiers, so the
# character form joins identically and no value column is touched.
.vs_align_keys <- function(d, kk, on_cols) {
  for (cc in on_cols) {
    if (!identical(class(d[[cc]])[1], class(kk[[cc]])[1])) {
      d <- data.table::copy(d)
      data.table::set(d, j = cc, value = as.character(d[[cc]]))
      data.table::set(kk, j = cc, value = as.character(kk[[cc]]))
    }
  }
  list(d = d, kk = kk)
}

.vs_par_at <- function(scen, name, keys, rename = NULL) {
  d <- .vs_par(scen, name)
  dv <- .vs_defval(scen, name)
  if (is.null(d) || nrow(d) == 0) return(rep(dv, nrow(keys)))
  kk <- data.table::copy(keys)
  if (!is.null(rename)) data.table::setnames(kk, names(rename), unname(rename))
  on_cols <- intersect(colnames(kk), setdiff(colnames(d), "value"))
  .a <- .vs_align_keys(d, kk, on_cols); d <- .a$d; kk <- .a$kk
  m <- d[kk, on = on_cols]
  v <- m$value
  v[is.na(v)] <- dv
  v
}

# Divergence summary over EVERY compared row, not just the violating ones.
# A check that passes still says how close it came, which is what makes the
# tolerances choosable from evidence instead of guessed: run the suite, read
# `$divergence`, and set `tol_abs`/`tol_rel` above the noise floor the backends
# actually produce rather than at a round number.
.vs_stats <- function(diff, lhs = NULL, rhs = NULL, scale = NULL) {
  a <- abs(as.numeric(diff))
  a <- a[is.finite(a)]
  if (!length(a)) {
    return(list(n_cmp = 0L, max_abs = NA_real_, median_abs = NA_real_,
                mean_abs = NA_real_, max_rel = NA_real_,
                max_scaled = NA_real_))
  }
  rel <- NA_real_
  if (!is.null(lhs) && !is.null(rhs)) {
    sc <- pmax(abs(as.numeric(lhs)), abs(as.numeric(rhs)))
    keep <- is.finite(a) & is.finite(sc) & sc > 0
    if (any(keep)) rel <- max(a[keep] / sc[keep])
  }
  # `max_rel` divides by the ROW's own magnitude, which degenerates wherever a
  # value legitimately reaches zero: an empty storage level reports a 100%
  # relative error on noise of 1e-10. `max_scaled` divides by the SERIES scale
  # instead, and is the figure that says whether a check is really drifting.
  scl <- NA_real_
  if (!is.null(scale)) {
    d2 <- abs(as.numeric(diff))
    sv <- rep_len(abs(as.numeric(scale)), length(d2))
    keep <- is.finite(d2) & is.finite(sv) & sv > 0
    if (any(keep)) scl <- max(d2[keep] / sv[keep])
  }
  list(n_cmp = length(a), max_abs = max(a),
       median_abs = stats::median(a), mean_abs = mean(a), max_rel = rel,
       max_scaled = scl)
}

# `scale` widens the relative term to the magnitude of the SERIES rather than
# the row. Without it a check whose values pass through zero -- a storage level
# emptying -- has no usable relative term at those rows, so the whole check
# leans on `tol_abs`, which does not grow with the model. Measured: the storage
# residual holds at ~1e-10 of the level scale at every resolution, but in
# absolute terms reaches 1.3e-5 at 8760 timeslices, 13x over `tol_abs`.
.vs_result <- function(keys, lhs, rhs, ctx, label, scale = NULL) {
  diff <- lhs - rhs
  ref <- pmax(abs(lhs), abs(rhs))
  if (!is.null(scale)) ref <- pmax(ref, abs(as.numeric(scale)))
  bad <- abs(diff) > ctx$tol_abs + ctx$tol_rel * ref
  st <- .vs_stats(diff, lhs, rhs, scale = scale)
  if (any(bad)) {
    viol <- data.table::copy(keys)[, `:=`(lhs = lhs, rhs = rhs, diff = diff)]
    list(status = "violated", n = nrow(keys), violations = viol[bad], stats = st)
  } else {
    list(status = "ok", n = nrow(keys), violations = NULL, stats = st)
  }
}

.vs_skip <- function(reason) {
  list(status = "skipped", n = 0L, violations = NULL, reason = reason,
       stats = .vs_stats(numeric(0)))
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
  # the signed slack of every domain row, kept for the divergence summary --
  # a one-sided bound only diverges on its own side, so `lo` slack above zero
  # and `up` slack below it are compliance, not error
  slack <- list()
  for (side in names(doms)) {
    dom <- doms[[side]]
    if (is.null(dom) || nrow(dom) == 0) next
    v <- .vs_value_at(scen, "vBalance", dom)
    bad <- switch(side,
      lo = v < -tol,
      up = v > tol,
      fx = abs(v) > tol
    )
    slack[[side]] <- switch(side,
      lo = pmin(v, 0), up = pmax(v, 0), fx = v)
    if (any(bad)) {
      pieces[[side]] <- data.table::copy(dom)[bad][, `:=`(
        limtype = toupper(side), lhs = v[bad], rhs = 0, diff = v[bad])]
    }
  }
  n <- sum(vapply(doms, function(d) if (is.null(d)) 0L else nrow(d), 0L))
  st <- .vs_stats(unlist(slack, use.names = FALSE))
  if (length(pieces)) {
    list(status = "violated", n = n,
         violations = data.table::rbindlist(pieces), stats = st)
  } else {
    list(status = "ok", n = n, violations = NULL, stats = st)
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

# --------------------------------------------------------------------------- #
# Stage 2 -- cheap defaults
# --------------------------------------------------------------------------- #

# B9. Every variable the registry declares `positive` must hold no negative
# value. Registry-driven, so it covers all 68 of them without naming any, and
# costs one pass over the solution.
.vs_check_positivity <- function(ctx) {
  scen <- ctx$scen
  reg <- .variables
  pos <- names(reg)[vapply(reg, function(x) isTRUE(x$positive), logical(1))]
  rows <- list()
  n_tot <- 0L
  worst <- numeric(0)
  for (nm in pos) {
    d <- .vs_var(scen, nm)
    if (is.null(d) || !nrow(d) || !"value" %in% names(d)) next
    v <- as.numeric(d$value)
    v <- v[is.finite(v)]
    if (!length(v)) next
    n_tot <- n_tot + length(v)
    worst <- c(worst, pmin(v, 0))
    bad <- which(as.numeric(d$value) < -(ctx$tol_abs))
    if (length(bad)) {
      kc <- setdiff(names(d), "value")
      key <- if (length(kc)) {
        do.call(paste, c(lapply(kc, function(k) as.character(d[[k]][bad])),
                         list(sep = "/")))
      } else rep("", length(bad))
      # NB `data.table(key =)` SETS THE KEY rather than making a column of
      # that name, so the coordinate column cannot be called `key`.
      rows[[length(rows) + 1L]] <- data.table::data.table(
        variable = nm, dims = key,
        lhs = as.numeric(d$value[bad]), rhs = 0,
        diff = as.numeric(d$value[bad]))
    }
  }
  if (!n_tot) return(.vs_skip("no solution variables with a positivity claim"))
  st <- .vs_stats(worst, worst, rep(0, length(worst)))
  if (length(rows)) {
    list(status = "violated", n = n_tot,
         violations = data.table::rbindlist(rows), stats = st)
  } else {
    list(status = "ok", n = n_tot, violations = NULL, stats = st)
  }
}

# A1. Nothing declared is silently absent: every object in the model reaches
# both its set and at least one parameter. OBJECTS path -- the expectation
# comes from `scen@model@data`, not from the maps, which is what lets it catch
# a map that was never built (dangling weather refs, coarse-geoframe supply
# going costless, the inert `costs` class).
.vs_obj_set_map <- c(
  commodity = "comm", supply = "sup", demand = "dem", technology = "tech",
  storage = "stg", trade = "trade", weather = "weather", import = "imp",
  export = "expp", group = "group")

.vs_check_inputs_present <- function(ctx) {
  scen <- ctx$scen
  obs <- tryCatch(getObjects(scen@model), error = function(e) NULL)
  if (is.null(obs) || !length(obs)) return(.vs_skip("model carries no objects"))
  sets <- scen@modInp@sets
  pars <- scen@modInp@parameters
  # maps first: membership tables are small, and one hit ends the scan
  pnames <- names(pars)
  pnames <- c(pnames[startsWith(pnames, "m")], pnames[!startsWith(pnames, "m")])

  bad <- list()
  n_tot <- 0L
  for (o in obs) {
    cls <- class(o)[1]
    # single-bracket: `[[` on a named vector ERRORS for an absent name, so a
    # class with no set of its own (lever, costs, constraint) would abort the
    # whole check rather than be skipped
    sn <- unname(.vs_obj_set_map[cls])
    if (is.na(sn)) next
    nm <- o@name
    n_tot <- n_tot + 1L
    in_set <- nm %in% as.character(sets[[sn]])
    in_par <- FALSE
    for (pn in pnames) {
      d <- tryCatch(get_data_slot(pars[[pn]], optional = TRUE),
                    error = function(e) NULL)
      if (is.null(d) || !nrow(d) || !sn %in% names(d)) next
      if (any(as.character(d[[sn]]) == nm, na.rm = TRUE)) { in_par <- TRUE; break }
    }
    if (!in_set || !in_par) {
      bad[[length(bad) + 1L]] <- data.table::data.table(
        object = nm, class = cls, set = sn,
        in_set = in_set, in_parameters = in_par,
        lhs = 1, rhs = as.numeric(in_set && in_par),
        diff = 1 - as.numeric(in_set && in_par))
    }
  }
  if (!n_tot) return(.vs_skip("no objects map to a declared set"))
  st <- .vs_stats(rep(0, n_tot))
  if (length(bad)) {
    v <- data.table::rbindlist(bad)
    list(status = "violated", n = n_tot, violations = v,
         stats = .vs_stats(v$diff, v$lhs, v$rhs))
  } else {
    list(status = "ok", n = n_tot, violations = NULL, stats = st)
  }
}

# D1. Unresolved-unit report. Units are optional by design and most models
# declare them partially, so this NEVER fails -- it returns status "ok" with a
# `report`. An unresolved unit is one still carrying a `{token}` placeholder
# after substitution.
.vs_check_units <- function(ctx) {
  obs <- tryCatch(getObjects(ctx$scen@model), error = function(e) NULL)
  if (is.null(obs) || !length(obs)) return(.vs_skip("model carries no objects"))
  rows <- list()
  seen <- list()
  n_tot <- 0L
  for (o in obs) {
    u <- tryCatch(as.data.frame(getUnits(o, complete = TRUE)),
                  error = function(e) NULL)
    if (is.null(u) || !nrow(u) || !"unit" %in% names(u)) next
    n_tot <- n_tot + nrow(u)
    un <- as.character(u$unit)
    open <- is.na(un) | grepl("[{]", un)
    seen[[length(seen) + 1L]] <- data.frame(
      class = class(o)[1], object = o@name, params = nrow(u),
      unresolved = sum(open), stringsAsFactors = FALSE)
    if (any(open)) {
      rows[[length(rows) + 1L]] <- data.frame(
        object = o@name, class = class(o)[1],
        slot = if ("slot" %in% names(u)) as.character(u$slot)[open] else NA_character_,
        parameter = if ("parameter" %in% names(u)) as.character(u$parameter)[open] else NA_character_,
        unit = un[open], stringsAsFactors = FALSE)
    }
  }
  if (!n_tot) return(.vs_skip("no object exposes unit formulas"))
  rep_df <- if (length(rows)) do.call(rbind, rows) else NULL
  # Per-class coverage, so the report says WHERE to declare units rather than
  # only how many are missing. Chain coherence (D2/D3/D4) is not checked: it
  # needs a unit algebra -- `.substitute_units()` performs string substitution,
  # not composition -- and no shipped model declares a unit to verify it on.
  cov <- if (length(seen)) {
    d <- do.call(rbind, seen)
    a <- stats::aggregate(cbind(params, unresolved) ~ class, data = d, FUN = sum)
    o <- stats::aggregate(object ~ class, data = d,
                          FUN = function(x) length(unique(x)))
    m <- merge(o, a, by = "class")
    names(m)[names(m) == "object"] <- "objects"
    m$declared <- m$params - m$unresolved
    m$pct_open <- round(100 * m$unresolved / m$params)
    m[order(-m$unresolved), c("class", "objects", "params", "declared",
                              "unresolved", "pct_open")]
  } else NULL
  list(status = "ok", n = n_tot, violations = NULL, report = rep_df,
       coverage = cov, stats = .vs_stats(rep(0, n_tot)))
}

# A scenario is "solved" when at least one modOut variable carries rows. An
# all-empty modOut is the unsolved case, not a solution of zeros.
.vs_has_solution <- function(scen) {
  mo <- tryCatch(scen@modOut, error = function(e) NULL)
  # an interpolated-but-unsolved scenario carries modOut = NULL, not an empty
  # modOut object
  if (is.null(mo) || !isS4(mo)) return(FALSE)
  v <- tryCatch(mo@variables, error = function(e) NULL)
  if (!length(v)) return(FALSE)
  for (x in v) {
    d <- tryCatch(get_data_slot(x, optional = TRUE), error = function(e) NULL)
    if (!is.null(d) && NROW(d) > 0L) return(TRUE)
  }
  FALSE
}

# --------------------------------------------------------------------------- #
# Stage 3 -- the objects path
#
# These derive their expectation from `scen@model@data` by re-interpolating
# each object slot independently (`getData(obj, interpolate = TRUE)`, driven by
# each parameter's own rule via `.param_interp_map()`), then compare against
# what `modInp` actually carries. That independence is the point: a check that
# reads the maps cannot notice a map that was never built.
# --------------------------------------------------------------------------- #

# A key column left NA in an object slot is a WILDCARD -- `region = NA` means
# every region -- so the expectation cannot be joined on equality. Rows are
# grouped by their NA pattern (few distinct patterns in practice) and each
# group joined on the columns it actually pins, which keeps this a handful of
# joins rather than a row-by-row scan.
#
# Returns the matched pairs with `.exp` and `value`, plus the ids of
# expectation rows that matched nothing.
.vs_match_wild <- function(exp, par, keys) {
  exp <- data.table::as.data.table(exp)
  par <- data.table::as.data.table(par)
  exp[, `.rid` := seq_len(.N)]
  if (!length(keys)) {
    m <- cbind(par, `.exp` = exp$.exp[1], `.rid` = exp$.rid[1])
    return(list(matched = m, unmatched = if (nrow(par)) integer(0) else exp$.rid))
  }
  nam <- is.na(as.data.frame(exp)[, keys, drop = FALSE])
  pat <- apply(nam, 1L, function(r) paste(as.integer(r), collapse = ""))
  out <- list()
  for (p in unique(pat)) {
    idx <- which(pat == p)
    isna <- strsplit(p, "", fixed = TRUE)[[1]] == "1"
    kk <- keys[!isna]
    e <- exp[idx, c(kk, ".exp", ".rid"), with = FALSE]
    m <- if (length(kk)) {
      merge(e, par, by = kk, allow.cartesian = TRUE)
    } else {
      # nothing pinned: the expectation applies to every parameter row
      cbind(par[rep(seq_len(nrow(par)), each = nrow(e))],
            e[rep(seq_len(nrow(e)), times = nrow(par)),
              c(".exp", ".rid"), with = FALSE])
    }
    if (nrow(m)) out[[length(out) + 1L]] <- m
  }
  matched <- if (length(out)) data.table::rbindlist(out, fill = TRUE) else NULL
  hit <- if (is.null(matched)) integer(0) else unique(matched$.rid)
  list(matched = matched, unmatched = setdiff(exp$.rid, hit))
}

# One object's declared slots, re-interpolated over the model's milestone
# years. Returns a list of (slot, column, param, defVal, table) entries; only
# columns the object actually gives a value are included, since an all-NA
# column carries no declaration to verify.
# Exact row signature over key columns, with NA as its own level, so a
# declared `comm = NA` wildcard is not confused with a declared `comm = COA`.
.vs_key_sig <- function(df, cols) {
  if (!length(cols)) return(rep("", NROW(df)))
  do.call(paste, c(lapply(cols, function(cc) {
    v <- as.character(df[[cc]])
    ifelse(is.na(v), "\001NA", v)
  }), list(sep = "\002")))
}

.vs_object_expectations <- function(scen, obj, years) {
  cls <- class(obj)[1]
  mp <- .param_interp_map()
  sl <- tryCatch(
    getData(obj, interpolate = TRUE, years = years, merge = FALSE),
    error = function(e) NULL)
  raw <- tryCatch(getData(obj, merge = FALSE), error = function(e) NULL)
  if (is.null(sl) || !is.list(sl) || is.data.frame(sl)) return(list())
  out <- list()
  for (snm in names(sl)) {
    d <- tryCatch(as.data.frame(sl[[snm]]), error = function(e) NULL)
    if (is.null(d) || !nrow(d)) next
    d$class <- NULL
    rw <- tryCatch(as.data.frame(raw[[snm]]), error = function(e) NULL)
    vcols <- names(d)[vapply(d, is.numeric, logical(1))]
    vcols <- setdiff(vcols, .known_set_dims)
    for (vc in vcols) {
      if (!any(!is.na(d[[vc]]))) next            # nothing declared
      info <- mp[[paste(cls, snm, vc, sep = "\r")]]
      if (is.null(info) || is.null(info$param)) next
      keep <- !is.na(d[[vc]])
      kcols <- intersect(names(d), .known_set_dims)
      # `getData(interpolate = TRUE)` materialises a partially-populated
      # column's NA cells at the parameter's defVal. Those zeros are NOT
      # declarations, and treating them as such turns a `comm = NA` row into a
      # wildcard that contradicts the sibling row which DID declare a value.
      # The raw slot is the record of what was actually given.
      if (!is.null(rw) && vc %in% names(rw) && nrow(rw)) {
        kk <- setdiff(intersect(intersect(names(rw), names(d)),
                                .known_set_dims), "year")
        decl <- unique(.vs_key_sig(rw[!is.na(rw[[vc]]), , drop = FALSE], kk))
        keep <- keep & (.vs_key_sig(d, kk) %in% decl)
      }
      if (!any(keep)) next
      tb <- data.table::as.data.table(d[keep, c(kcols, vc), drop = FALSE])
      data.table::setnames(tb, vc, ".exp")
      # A slot may re-use the object's OWN set-dim name for the OTHER member
      # of a pair. `commodity@emis` names the emitted commodity `comm` while
      # pEmissionFactor keys it `comm = CO2, commp = COA`; `commodity@agg`
      # uses the opposite orientation. The convention is not recoverable from
      # the object alone, so the conflict is recorded here and resolved
      # against the parameter at compare time.
      own <- unname(.class_set_dim[cls])
      conflict <- FALSE
      if (!is.na(own) && own %in% names(tb)) {
        vv <- as.character(tb[[own]])
        conflict <- !all(is.na(vv) | vv == obj@name)
      }
      out[[length(out) + 1L]] <- list(
        slot = snm, column = vc, param = info$param,
        defVal = suppressWarnings(as.numeric(info$defVal[1])),
        own = own,
        conflict = conflict, objname = obj@name,
        dims = info$dims %||% character(0),
        bound = {
          sfx <- sub("^.*[.]", "", vc)
          if (sfx %in% c("lo", "up", "fx")) sfx else NA_character_
        },
        table = tb)
    }
  }
  out
}

.vs_milestone_years <- function(scen) {
  y <- suppressWarnings(as.integer(scen@modInp@sets$year))
  y[!is.na(y)]
}

# A3. Declared values survive interpolation. Re-interpolates each object slot
# independently and diffs against the modInp parameter it feeds. Catches
# drops, mis-keying, and silent multiplication.
#
# Sparse interpolation legitimately omits rows whose value equals the
# parameter's defVal, so an unmatched expectation is only a violation when the
# declared value differs from that default.
# Several bound columns (`X.lo`, `X.up`, `X.fx`) share ONE packed parameter
# that separates them with a `type` column. Comparing a `.lo` declaration
# against the whole table would match the `up` row too, so the parameter is
# narrowed to the bound in hand.
# Resolve which of a paired dim (`X` / `Xp`) carries the object itself, by
# looking at where the object's name actually appears in the parameter.
# Only the KEY ORIENTATION is taken from the parameter -- every value still
# comes from the object, so a wrong or missing value is still caught. When
# the name appears in neither column or in both, the pair is ambiguous and
# the expectation is dropped rather than guessed at.
.vs_align_own <- function(ex, par) {
  tb <- ex$table
  if (!isTRUE(ex$conflict)) return(tb)
  own <- ex$own
  pair <- paste0(own, "p")
  if (is.na(own) || !all(c(own, pair) %in% names(par))) return(NULL)
  in_own  <- any(as.character(par[[own]])  == ex$objname, na.rm = TRUE)
  in_pair <- any(as.character(par[[pair]]) == ex$objname, na.rm = TRUE)
  if (in_own == in_pair) return(NULL)            # ambiguous, or absent
  if (in_own) {
    data.table::setnames(tb, own, pair)          # slot column is the OTHER
    tb[[own]] <- ex$objname
  } else {
    tb[[pair]] <- ex$objname                     # slot column is already `own`
  }
  tb
}

.vs_par_bound <- function(par, bound) {
  if (is.null(par) || is.na(bound) || !"type" %in% names(par)) return(par)
  out <- par[as.character(par$type) == bound]
  out$type <- NULL
  out
}

.vs_check_inputs_values <- function(ctx) {
  scen <- ctx$scen
  obs <- tryCatch(getObjects(scen@model), error = function(e) NULL)
  if (is.null(obs) || !length(obs)) return(.vs_skip("model carries no objects"))
  years <- .vs_milestone_years(scen)
  bad <- list()
  n_tot <- 0L
  n_amb <- 0L
  diffs <- numeric(0)
  lhs_all <- numeric(0); rhs_all <- numeric(0)
  for (o in obs) {
    for (ex in .vs_object_expectations(scen, o, years)) {
      par <- .vs_par_bound(.vs_par(scen, ex$param), ex$bound)
      dv <- ex$defVal
      if (is.null(par) || !nrow(par) || !"value" %in% names(par)) {
        # the parameter never reached modInp at all
        nd <- if (is.na(dv)) ex$table else
          ex$table[abs(.exp - dv) > ctx$tol_abs]
        if (nrow(nd)) {
          bad[[length(bad) + 1L]] <- data.table::data.table(
            object = o@name, parameter = ex$param, slot = ex$slot,
            column = ex$column, issue = "parameter absent from modInp",
            lhs = nd$.exp, rhs = NA_real_, diff = NA_real_)
        }
        next
      }
      tb <- .vs_align_own(ex, par)
      if (is.null(tb)) { n_amb <- n_amb + 1L; next }
      keys <- intersect(names(tb), names(par))
      keys <- setdiff(keys, c("value", ".exp"))
      mm <- .vs_match_wild(tb, par, keys)
      n_tot <- n_tot + nrow(tb)
      if (!is.null(mm$matched) && nrow(mm$matched)) {
        m <- mm$matched
        d <- as.numeric(m$value) - as.numeric(m$.exp)
        diffs <- c(diffs, d)
        lhs_all <- c(lhs_all, as.numeric(m$.exp))
        rhs_all <- c(rhs_all, as.numeric(m$value))
        viol <- abs(d) > ctx$tol_abs +
          ctx$tol_rel * pmax(abs(as.numeric(m$.exp)), abs(as.numeric(m$value)))
        if (any(viol)) {
          bad[[length(bad) + 1L]] <- data.table::data.table(
            object = o@name, parameter = ex$param, slot = ex$slot,
            column = ex$column, issue = "value changed by interpolation",
            lhs = as.numeric(m$.exp)[viol], rhs = as.numeric(m$value)[viol],
            diff = d[viol])
        }
      }
      if (length(mm$unmatched)) {
        um <- tb[mm$unmatched]
        # absent is legal exactly when the declared value IS the default
        um <- if (is.na(dv)) um else um[abs(.exp - dv) > ctx$tol_abs]
        if (nrow(um)) {
          bad[[length(bad) + 1L]] <- data.table::data.table(
            object = o@name, parameter = ex$param, slot = ex$slot,
            column = ex$column, issue = "declared value absent from modInp",
            lhs = um$.exp, rhs = NA_real_, diff = NA_real_)
        }
      }
    }
  }
  if (!n_tot) return(.vs_skip("no object slot maps to a modInp parameter"))
  st <- .vs_stats(diffs, lhs_all, rhs_all)
  # ambiguous pairs are REPORTED, so the check never looks like it covered
  # more than it did
  rep_df <- if (n_amb) data.frame(
    issue = "ambiguous paired dimension, not compared", n = n_amb,
    stringsAsFactors = FALSE) else NULL
  if (length(bad)) {
    list(status = "violated", n = n_tot,
         violations = data.table::rbindlist(bad, fill = TRUE),
         report = rep_df, stats = st)
  } else {
    list(status = "ok", n = n_tot, violations = NULL, report = rep_df,
         stats = st)
  }
}

# A2. Declared bounds are honoured by the solution. The bound is read from the
# OBJECT, never from the interpolated parameter, so a bound that never reached
# the solver is caught rather than confirmed.
#
# The `.lo`/`.up`/`.fx` suffix gives the direction and the parameter name gives
# the variable (`pTechCap` -> `vTechCap`); a bound whose parameter has no
# matching solution variable (`pStorageDuration`, the `af` ratios) is skipped
# rather than guessed at. Only rows PRESENT in the solution are tested: a
# solver exports non-zeros only, so absent rows cannot be distinguished from
# zeros without rebuilding the full domain.
.vs_check_inputs_bounds <- function(ctx) {
  scen <- ctx$scen
  obs <- tryCatch(getObjects(scen@model), error = function(e) NULL)
  if (is.null(obs) || !length(obs)) return(.vs_skip("model carries no objects"))
  years <- .vs_milestone_years(scen)
  bad <- list()
  n_tot <- 0L
  n_bounds <- 0L
  diffs <- numeric(0); lhs_all <- numeric(0); rhs_all <- numeric(0)
  for (o in obs) {
    for (ex in .vs_object_expectations(scen, o, years)) {
      dir <- ex$bound
      if (is.na(dir)) next
      n_bounds <- n_bounds + 1L
      vname <- sub("^p", "v", ex$param)
      if (!vname %in% names(.variables)) next
      vd <- .vs_var(scen, vname)
      if (is.null(vd) || !nrow(vd) || !"value" %in% names(vd)) next
      tb <- .vs_align_own(ex, vd)
      if (is.null(tb)) next
      keys <- intersect(names(tb), names(vd))
      keys <- setdiff(keys, c("value", ".exp"))
      mm <- .vs_match_wild(tb, vd, keys)
      if (is.null(mm$matched) || !nrow(mm$matched)) next
      m <- mm$matched
      b <- as.numeric(m$.exp); v <- as.numeric(m$value)
      tol <- ctx$tol_abs + ctx$tol_rel * pmax(abs(b), abs(v))
      slack <- switch(dir,
                      up = v - b,            # >0 breaks an upper bound
                      lo = b - v,            # >0 breaks a lower bound
                      fx = abs(v - b))
      viol <- is.finite(slack) & slack > tol
      n_tot <- n_tot + length(v)
      diffs <- c(diffs, pmax(slack, 0))
      lhs_all <- c(lhs_all, v); rhs_all <- c(rhs_all, b)
      if (any(viol)) {
        bad[[length(bad) + 1L]] <- data.table::data.table(
          object = o@name, variable = vname, bound = ex$column,
          direction = dir, lhs = v[viol], rhs = b[viol], diff = slack[viol])
      }
    }
  }
  if (!n_bounds) return(.vs_skip("model declares no bounds"))
  if (!n_tot) {
    return(.vs_skip(
      "no declared bound has a matching solution variable to test"))
  }
  st <- .vs_stats(diffs, lhs_all, rhs_all)
  if (length(bad)) {
    list(status = "violated", n = n_tot,
         violations = data.table::rbindlist(bad, fill = TRUE), stats = st)
  } else {
    list(status = "ok", n = n_tot, violations = NULL, stats = st)
  }
}

# --------------------------------------------------------------------------- #
# Stage 4 -- equation depth
# --------------------------------------------------------------------------- #

# Sum a storage flow variable, weighted by its efficiency parameter, to
# (stg, region, year, timeslice). The equation sums over the INPUT/OUTPUT
# commodity, which is not the stored commodity, so the per-commodity keys
# collapse before the lookup.
.vs_stg_flow <- function(scen, vname, pname, divide = FALSE) {
  d <- .vs_var(scen, vname)
  if (is.null(d) || !nrow(d)) return(NULL)
  kk <- d[, c("stg", "comm", "region", "year", "timeslice"), with = FALSE]
  eff <- .vs_par_at(scen, pname, kk)
  val <- if (divide) as.numeric(d$value) / eff else as.numeric(d$value) * eff
  d2 <- data.table::data.table(
    stg = d$stg, region = d$region, year = d$year, timeslice = d$timeslice,
    value = val)
  d2[, list(value = sum(value)), by = c("stg", "region", "year", "timeslice")]
}

.vs_lookup <- function(tab, keys, by) {
  if (is.null(tab) || !nrow(tab)) return(rep(0, nrow(keys)))
  m <- tab[keys, on = by]
  v <- m$value
  v[is.na(v)] <- 0
  v
}

# B7. Storage dynamics, exactly as `eqStorageLevel` states them:
#
#   level[s] = startLevel[s]
#            + ncap2stg[s] * newCap
#            + SUM_ci inpEff[sp] * inp[sp]
#            + stgEff[s]^share[s] * level[sp]
#            - SUM_co out[sp] / outEff[sp]
#
# `meqStorageLevel` supplies the (sp, s) chronology INCLUDING the wrap that
# closes a `@fullYear` store, so cycle closure is tested by the same rows as
# the rest of the series -- the `fullYear` bug the goldens could not see.
#
# Both levels are read from the SOLUTION, so each row is a one-step residual.
# Propagating the level forward instead would compound round-off over the
# whole chain; the residual form does not, which is why one tolerance serves
# here as it does for the flat identities.
.vs_check_storage_dynamics <- function(ctx) {
  scen <- ctx$scen
  keys <- .vs_par(scen, "meqStorageLevel")
  if (is.null(keys) || !nrow(keys)) {
    return(.vs_skip("no meqStorageLevel map (model has no storage)"))
  }
  lv <- .vs_var(scen, "vStorageLevel")
  if (is.null(lv) || !nrow(lv)) {
    return(.vs_skip("vStorageLevel absent from the solution"))
  }
  kcur <- keys[, c("stg", "comm", "region", "year", "timeslice"), with = FALSE]
  kprv <- data.table::data.table(
    stg = keys$stg, comm = keys$comm, region = keys$region,
    year = keys$year, timeslice = keys$timeslicep)

  lhs <- .vs_value_at(scen, "vStorageLevel", kcur)
  lev_prev <- .vs_value_at(scen, "vStorageLevel", kprv)
  start <- .vs_par_at(scen, "pStorageStartLevel", kcur)
  n2s <- .vs_par_at(scen, "pStorageNCap2Stg", kcur)
  ncap <- .vs_value_at(scen, "vStorageOutNewCap", kcur)
  stgeff <- .vs_par_at(scen, "pStorageStgEff", kcur)
  share <- .vs_par_at(scen, "pTimesliceShare",
                      data.table::data.table(timeslice = keys$timeslice))

  by <- c("stg", "region", "year", "timeslice")
  kp <- kprv[, by, with = FALSE]
  inp <- .vs_lookup(.vs_stg_flow(scen, "vStorageInp", "pStorageInpEff"), kp, by)
  out <- .vs_lookup(.vs_stg_flow(scen, "vStorageOut", "pStorageOutEff",
                                 divide = TRUE), kp, by)

  rhs <- start + n2s * ncap + inp + (stgeff^share) * lev_prev - out

  # A level that empties has no magnitude of its own to be measured against,
  # but its series does.
  gcols <- c("stg", "comm", "region", "year")
  # `max(numeric(0))` is -Inf with a warning; an all-NA group must contribute
  # no scale rather than a warning and a nonsense bound
  sc <- lv[, list(value = {
    v <- abs(as.numeric(value)); v <- v[is.finite(v)]
    if (length(v)) max(v) else 0
  }), by = gcols]
  scale <- .vs_lookup(sc, keys[, gcols, with = FALSE], gcols)

  .vs_result(keys, lhs, rhs, ctx, "storage_dynamics", scale = scale)
}

# --------------------------------------------------------------------------- #
# B2. Capacity accumulation -- eqTechCap.
#
#   vTechCap[t,r,y] = vTechStockCap[t,r,y]
#     + SUM_{yp in mTechNew, ord(y) >= ord(yp),
#            ord(y) < olife + ord(yp) or (t,r) in mTechOlifeInf}
#         ( pPeriodLen[yp] * vTechNewCap[t,r,yp]
#           - SUM_{ye : (t,r,yp,ye) in mvTechRetiredNewCap, ord(y) >= ord(ye)}
#               vTechRetiredNewCap[t,r,yp,ye] * pPeriodLen[ye] )
#
# `vTechNewCap` is a RATE (capacity per year), which is why it is multiplied by
# `pPeriodLen` -- the defect that under-charged capital by exactly that factor
# lived in this arithmetic.
# --------------------------------------------------------------------------- #
.vs_check_capacity_accumulation <- function(ctx) {
  scen <- ctx$scen
  span <- .vs_par(scen, "mTechSpan")
  if (is.null(span) || !nrow(span)) {
    return(.vs_skip("no mTechSpan map (model has no technologies)"))
  }
  cap <- .vs_var(scen, "vTechCap")
  if (is.null(cap) || !nrow(cap)) {
    return(.vs_skip("vTechCap absent from the solution"))
  }
  new <- .vs_par(scen, "mTechNew")
  yrs <- sort(unique(suppressWarnings(as.integer(scen@modInp@sets$year))))
  ord <- stats::setNames(seq_along(yrs), as.character(yrs))

  keys <- data.table::copy(span)
  lhs <- .vs_value_at(scen, "vTechCap", keys)
  stock <- .vs_value_at(scen, "vTechStockCap", keys)

  add <- rep(0, nrow(keys))
  if (!is.null(new) && nrow(new)) {
    inf <- .vs_par(scen, "mTechOlifeInf")
    infkey <- if (is.null(inf) || !nrow(inf)) character(0) else
      paste(inf$tech, inf$region, sep = "\r")
    olife <- .vs_par_at(scen, "pTechOlife",
                        keys[, c("tech", "region"), with = FALSE])
    # every (span row) x (vintage year) pair, then the olife window filter
    kk <- data.table::copy(keys)[, `:=`(.row = seq_len(.N),
                                        .olife = olife)]
    nn <- data.table::copy(new)
    data.table::setnames(nn, "year", "yp")
    m <- merge(kk, nn, by = c("tech", "region"), allow.cartesian = TRUE)
    if (nrow(m)) {
      oy <- ord[as.character(m$year)]
      op <- ord[as.character(m$yp)]
      is_inf <- paste(m$tech, m$region, sep = "\r") %in% infkey
      keep <- !is.na(oy) & !is.na(op) & oy >= op &
        (oy < m$.olife + op | is_inf)
      m <- m[keep]
    }
    if (nrow(m)) {
      knew <- data.table::data.table(tech = m$tech, region = m$region,
                                     year = m$yp)
      ncap <- .vs_value_at(scen, "vTechNewCap", knew)
      plen <- .vs_par_at(scen, "pPeriodLen",
                         data.table::data.table(year = m$yp))
      m[, `.add` := plen * ncap]

      ret <- .vs_par(scen, "mvTechRetiredNewCap")
      if (!is.null(ret) && nrow(ret)) {
        rr <- data.table::copy(ret)
        # (tech, region, year, year.1) = (t, r, yp, ye)
        data.table::setnames(rr, c("year", "year.1"), c("yp", "ye"))
        mr <- merge(m[, c("tech", "region", "year", "yp", ".row"), with = FALSE],
                    rr, by = c("tech", "region", "yp"), allow.cartesian = TRUE)
        if (nrow(mr)) {
          keep <- ord[as.character(mr$year)] >= ord[as.character(mr$ye)]
          mr <- mr[!is.na(keep) & keep]
        }
        if (nrow(mr)) {
          kret <- data.table::data.table(tech = mr$tech, region = mr$region,
                                         year = mr$yp, `year.1` = mr$ye)
          rv <- .vs_value_at(scen, "vTechRetiredNewCap", kret)
          rl <- .vs_par_at(scen, "pPeriodLen",
                           data.table::data.table(year = mr$ye))
          mr[, `.ret` := rv * rl]
          agg <- mr[, list(.ret = sum(.ret)), by = ".row"]
          m <- merge(m, agg, by = ".row", all.x = TRUE)
          m[is.na(.ret), `.ret` := 0]
          m[, `.add` := .add - .ret]
        }
      }
      a <- m[, list(v = sum(.add)), by = ".row"]
      add[a$.row] <- a$v
    }
  }
  rhs <- stock + add
  .vs_result(keys, lhs, rhs, ctx, "capacity_accumulation")
}

# --------------------------------------------------------------------------- #
# B3. EAC -- eqTechEac: vTechEac = pTechEac * vTechCap.
#
# The annuity itself is computed at interpolation and stored in `pTechEac`;
# this checks that the solver's charge matches the capacity it was applied to.
# Recomputing the annuity from invcost/wacc/olife is a MODEL-INPUT question
# and belongs with the levcost machinery, not here.
# --------------------------------------------------------------------------- #
.vs_check_eac <- function(ctx) {
  scen <- ctx$scen
  eac <- .vs_var(scen, "vTechEac")
  if (is.null(eac) || !nrow(eac)) {
    return(.vs_skip("vTechEac absent from the solution"))
  }
  keys <- .vs_par(scen, "mTechSpan")
  if (is.null(keys) || !nrow(keys)) return(.vs_skip("no mTechSpan map"))
  lhs <- .vs_value_at(scen, "vTechEac", keys)
  rhs <- .vs_par_at(scen, "pTechEac", keys) *
    .vs_value_at(scen, "vTechCap", keys)
  .vs_result(keys, lhs, rhs, ctx, "eac")
}

# --------------------------------------------------------------------------- #
# B5. Flow chain -- input -> use -> activity -> output.
#
#   single in : vTechInp * pTechCinp2use
#   group  in : pTechGinp2use * SUM_{c in g} vTechInp * pTechCinp2ginp
#   single out: vTechOut / (pTechUse2cact * pTechCact2cout)
#   group  out: SUM_{cp in gp} of the same
#
# The grouped-input form is the co-firing case, where a technology produced
# output with zero fuel because the group maps were never built.
# --------------------------------------------------------------------------- #

# Expand group keys to their member commodities via mTechGroupComm.
#
# Only the columns needed for the join are carried across, and the member
# commodity is renamed. A flow-chain key table has its OWN `comm` (the single
# commodity on the other side) and, for Grp2Grp, two `group` columns; merging
# the raw table against mTechGroupComm collides on both names, silently drops
# `comm` from the join key, and then matches every member against the same
# solution row -- a two-fuel group counted one fuel twice.
.vs_group_expand <- function(scen, keys, gcol) {
  gm <- .vs_par(scen, "mTechGroupComm")
  if (is.null(gm) || !nrow(gm)) return(NULL)
  gm <- data.table::copy(gm)
  data.table::setnames(gm, "comm", ".member")
  k <- data.table::data.table(
    tech = keys$tech, region = keys$region, year = keys$year,
    timeslice = keys$timeslice, group = keys[[gcol]],
    `.row` = seq_len(nrow(keys)))
  merge(k, gm, by = c("tech", "group"), allow.cartesian = TRUE)
}

.vs_use_from_inp <- function(scen, keys, ccol, grouped) {
  if (!grouped) {
    kk <- data.table::data.table(tech = keys$tech, comm = keys[[ccol]],
                                 region = keys$region, year = keys$year,
                                 timeslice = keys$timeslice)
    return(.vs_value_at(scen, "vTechInp", kk) *
             .vs_par_at(scen, "pTechCinp2use", kk))
  }
  e <- .vs_group_expand(scen, keys, ccol)
  out <- rep(0, nrow(keys))
  if (is.null(e) || !nrow(e)) return(out)
  kk <- data.table::data.table(tech = e$tech, comm = e$.member,
                               region = e$region, year = e$year,
                               timeslice = e$timeslice)
  e[, `.v` := .vs_value_at(scen, "vTechInp", kk) *
      .vs_par_at(scen, "pTechCinp2ginp", kk)]
  a <- e[, list(v = sum(.v)), by = ".row"]
  out[a$.row] <- a$v
  kg <- data.table::data.table(tech = keys$tech, group = keys[[ccol]],
                               region = keys$region, year = keys$year,
                               timeslice = keys$timeslice)
  out * .vs_par_at(scen, "pTechGinp2use", kg)
}

.vs_use_from_out <- function(scen, keys, ccol, grouped) {
  term <- function(kk) {
    .vs_value_at(scen, "vTechOut", kk) /
      (.vs_par_at(scen, "pTechUse2cact", kk) *
         .vs_par_at(scen, "pTechCact2cout", kk))
  }
  if (!grouped) {
    kk <- data.table::data.table(tech = keys$tech, comm = keys[[ccol]],
                                 region = keys$region, year = keys$year,
                                 timeslice = keys$timeslice)
    return(term(kk))
  }
  e <- .vs_group_expand(scen, keys, ccol)
  out <- rep(0, nrow(keys))
  if (is.null(e) || !nrow(e)) return(out)
  kk <- data.table::data.table(tech = e$tech, comm = e$.member,
                               region = e$region, year = e$year,
                               timeslice = e$timeslice)
  e[, `.v` := term(kk)]
  a <- e[, list(v = sum(.v)), by = ".row"]
  out[a$.row] <- a$v
  out
}

.vs_check_flow_chain <- function(ctx) {
  scen <- ctx$scen
  forms <- list(
    list(map = "meqTechSng2Sng", inp = "comm",  gin = FALSE,
         out = "comm.1",  gout = FALSE),
    list(map = "meqTechGrp2Sng", inp = "group", gin = TRUE,
         out = "comm",    gout = FALSE),
    list(map = "meqTechSng2Grp", inp = "comm",  gin = FALSE,
         out = "group",   gout = TRUE),
    list(map = "meqTechGrp2Grp", inp = "group", gin = TRUE,
         out = "group.1", gout = TRUE)
  )
  rows <- list(); n_tot <- 0L
  diffs <- numeric(0); L <- numeric(0); R <- numeric(0)
  for (f in forms) {
    keys <- .vs_par(scen, f$map)
    if (is.null(keys) || !nrow(keys)) next
    lhs <- .vs_use_from_inp(scen, keys, f$inp, f$gin)
    rhs <- .vs_use_from_out(scen, keys, f$out, f$gout)
    d <- lhs - rhs
    n_tot <- n_tot + length(d)
    diffs <- c(diffs, d); L <- c(L, lhs); R <- c(R, rhs)
    bad <- abs(d) > ctx$tol_abs + ctx$tol_rel * pmax(abs(lhs), abs(rhs))
    if (any(bad)) {
      rows[[length(rows) + 1L]] <- data.table::copy(keys)[bad][
        , `:=`(form = f$map, lhs = lhs[bad], rhs = rhs[bad], diff = d[bad])]
    }
  }
  if (!n_tot) return(.vs_skip("no technology flow-chain map is populated"))
  st <- .vs_stats(diffs, L, R)
  if (length(rows)) {
    list(status = "violated", n = n_tot,
         violations = data.table::rbindlist(rows, fill = TRUE), stats = st)
  } else {
    list(status = "ok", n = n_tot, violations = NULL, stats = st)
  }
}
