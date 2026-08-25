# =========================================================================== #
# compare_inputs.R -- diff two scenarios' model inputs (modInp).
# Promoted from dev-scripts/compare_modInp.R (arsenal engine replaced by the
# .compare_values() kernel; waldo is optional detail rendering only).
# =========================================================================== #

#' Compare two scenarios' model inputs
#'
#' Mechanically diffs the interpolated model input (`modInp`) of two
#' scenarios: parameter presence, per-parameter metadata (`dimSets` --
#' distinguishing order-only changes -- `type`, `defVal`, `interpolation`,
#' `desc`), parameter data (key-based, row-order insensitive, with numeric
#' tolerance), model sets, the compiled user constraints / user cost terms,
#' and `@misc`. The classic use is explaining WHY two solved scenarios
#' differ, or guarding a refactor of the interpolation pipeline.
#'
#' Data is compared as stored by default; `unfold = TRUE` collapses
#' fold-vs-dense representation drift first (requires scenario inputs).
#'
#' @param a,b `scenario` or `modInp` objects.
#' @param tol numeric, absolute tolerance for value comparisons.
#' @param unfold logical, unfold folded parameters before comparing
#'   (scenario inputs only).
#' @param names character(2), display names of the two sides.
#'
#' @return An `inputs_cmp` object (a list) with components `names`,
#'   `summary` (one row per common parameter: `parameter, status, n_A, n_B,
#'   n_diff, max_abs_diff, meta`; status is one of `identical`, `meta-diff`,
#'   `value-diff`, `rows-mismatch`, `rows-only-A`, `rows-only-B`,
#'   `structural`, `diff`), `params` (`only_A` / `only_B` / `common` with
#'   per-parameter detail), `sets`, `equations`, `misc`, and `tol`.
#'   `print()` gives a compact report; with \pkg{waldo} installed it renders
#'   textual diffs for the first differing parameters.
#'
#' @seealso [compare_scenarios()] for solved-results comparison,
#'   [compare_models()] for declaration-level model diffs.
#' @family comparison
#' @export
compare_inputs <- function(a, b, tol = 1e-8, unfold = FALSE,
                           names = c("A", "B")) {
  scenA <- if (methods::is(a, "scenario")) a else NULL
  scenB <- if (methods::is(b, "scenario")) b else NULL
  if (unfold && (is.null(scenA) || is.null(scenB))) {
    stop("unfold = TRUE requires scenario inputs (needs scenario context).")
  }
  mA <- .ci_as_modInp(a)
  mB <- .ci_as_modInp(b)

  pna <- base::names(mA@parameters)
  pnb <- base::names(mB@parameters)
  only_A <- setdiff(pna, pnb)
  only_B <- setdiff(pnb, pna)
  common <- intersect(pna, pnb)

  per <- list()
  rows <- vector("list", length(common))
  for (i in seq_along(common)) {
    nm <- common[[i]]
    pa <- mA@parameters[[nm]]
    pb <- mB@parameters[[nm]]
    meta <- .ci_param_meta(pa, pb, tol)
    data <- .ci_param_data(pa, pb, tol, unfold, scenA, scenB)
    status <- if (data$status != "identical") data$status
      else if (length(meta)) "meta-diff" else "identical"
    per[[nm]] <- list(status = status, meta_diffs = meta, data = data,
                      pa = pa, pb = pb)
    rows[[i]] <- data.frame(
      parameter = nm, status = status,
      n_A = data$n_A, n_B = data$n_B,
      n_diff = data$n_value_diff %||% NA_integer_,
      max_abs_diff = data$max_abs_diff %||% NA_real_,
      meta = paste(base::names(meta), collapse = ";"),
      stringsAsFactors = FALSE)
  }
  summary_df <- if (length(rows)) do.call(rbind, rows) else
    data.frame(parameter = character(), status = character(),
               stringsAsFactors = FALSE)

  structure(list(
    names = names,
    summary = summary_df,
    params = list(only_A = only_A, only_B = only_B, common = per),
    sets = .ci_sets(mA, mB),
    equations = .ci_equations(mA, mB),
    misc = .ci_misc(mA, mB),
    tol = tol, unfold = unfold,
    scenA = scenA, scenB = scenB
  ), class = "inputs_cmp")
}

# --------------------------------------------------------------------------- #
# helpers
# --------------------------------------------------------------------------- #

.ci_as_modInp <- function(x) {
  if (methods::is(x, "scenario")) return(.upgrade_modInp(x@modInp))
  if (methods::is(x, "modInp")) return(.upgrade_modInp(x))
  stop("compare_inputs(): inputs must be 'scenario' or 'modInp' objects.")
}

# slot-safe getter: objects saved under older class versions may lack slots
.ci_slot <- function(obj, name, default = NULL) {
  if (.hasSlot(obj, name)) methods::slot(obj, name) else default
}

# Extract a parameter's data slot as a plain data.frame (empty -> NULL).
.ci_pdata <- function(p, scen = NULL, unfold = FALSE) {
  if (is.null(p)) return(NULL)
  if (unfold && !is.null(scen)) {
    d <- tryCatch(unfold_scenario_parameter(scen, p),
                  error = function(e) get_data_slot(p))
  } else {
    d <- get_data_slot(p)
  }
  if (is.null(d)) return(NULL)
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  if (nrow(d) == 0) return(NULL)
  d
}

# normalise a df for order-insensitive comparison: stable column order, all
# non-numeric cols -> character, sort rows by every column.
.ci_norm <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  d <- d[, order(names(d)), drop = FALSE]
  for (j in names(d)) {
    if (!is.numeric(d[[j]])) d[[j]] <- as.character(d[[j]])
  }
  d <- d[do.call(order, c(unname(as.list(d)), list(method = "radix"))), ,
         drop = FALSE]
  rownames(d) <- NULL
  d
}

# key / value column split for a parameter data.frame.
# Type-driven (NOT name-matched against dimSets): dimSets may legitimately
# repeat a set name (e.g. dimSets comm,region,year,slice,slice -> data
# columns slice, slice.1), so intersect(dimSets, names) would mis-drop the
# duplicate column into the value slot. For set/map there is no value column
# (all columns are keys); for numpar/bounds the only value column is `value`.
.ci_keysplit <- function(d, type = NA_character_) {
  valCols <- if (isTRUE(as.character(type) %in% c("numpar", "bounds")))
    intersect("value", names(d)) else character(0)
  list(key = setdiff(names(d), valCols), val = valCols)
}

# --------------------------------------------------------------------------- #
# parameter metadata diff (scalar slots)
# --------------------------------------------------------------------------- #
.ci_param_meta <- function(pa, pb, tol) {
  diffs <- list()
  add <- function(slot, a, b) diffs[[slot]] <<- list(A = a, B = b)

  da <- pa@dimSets %||% character(0)
  db <- pb@dimSets %||% character(0)
  if (!identical(da, db)) {
    if (setequal(da, db)) {
      add("dimSets(order)", paste(da, collapse = ","),
          paste(db, collapse = ","))
    } else {
      add("dimSets", paste(da, collapse = ","), paste(db, collapse = ","))
    }
  }

  ta <- as.character(pa@type)
  tb <- as.character(pb@type)
  if (!identical(ta, tb)) add("type", ta, tb)

  va <- pa@defVal %||% numeric(0)
  vb <- pb@defVal %||% numeric(0)
  if (length(va) != length(vb) ||
      !isTRUE(all.equal(va, vb, tolerance = tol, check.attributes = FALSE))) {
    add("defVal", paste(va, collapse = ","), paste(vb, collapse = ","))
  }

  ia <- pa@interpolation %||% character(0)
  ib <- pb@interpolation %||% character(0)
  if (!identical(ia, ib)) {
    add("interpolation", paste(ia, collapse = ","), paste(ib, collapse = ","))
  }

  if (!identical(pa@desc %||% "", pb@desc %||% "")) {
    add("desc", pa@desc %||% "", pb@desc %||% "")
  }

  diffs
}

# --------------------------------------------------------------------------- #
# parameter data diff, on the shared value kernel
# --------------------------------------------------------------------------- #
.ci_param_data <- function(pa, pb, tol, unfold, scenA, scenB) {
  da <- .ci_pdata(pa, scenA, unfold)
  db <- .ci_pdata(pb, scenB, unfold)
  n_a <- if (is.null(da)) 0L else nrow(da)
  n_b <- if (is.null(db)) 0L else nrow(db)

  if (is.null(da) && is.null(db)) {
    return(list(status = "identical", n_A = 0L, n_B = 0L,
                n_value_diff = 0L, max_abs_diff = 0, note = "both empty"))
  }
  if (is.null(da) || is.null(db)) {
    return(list(status = if (is.null(da)) "rows-only-B" else "rows-only-A",
                n_A = n_a, n_B = n_b, n_value_diff = NA_integer_,
                max_abs_diff = NA_real_, note = "one side empty"))
  }
  if (!setequal(names(da), names(db))) {
    return(list(status = "structural", n_A = n_a, n_B = n_b,
                n_value_diff = NA_integer_, max_abs_diff = NA_real_,
                note = sprintf("columns differ: A={%s} B={%s}",
                               paste(names(da), collapse = ","),
                               paste(names(db), collapse = ","))))
  }

  ks <- .ci_keysplit(da, as.character(pa@type %||% pb@type))

  if (length(ks$val)) {
    # numpar / bounds: keyed value comparison
    out <- tryCatch(
      .compare_values(da, db, by = ks$key, tol_rel = 0, tol_abs = tol,
                      missing = "flag"),
      error = function(e) NULL)
    if (is.null(out)) {
      # e.g. duplicated keys defeating the join: order-insensitive fallback
      same <- isTRUE(all.equal(.ci_norm(da), .ci_norm(db),
                               tolerance = tol, check.attributes = FALSE))
      return(list(status = if (same) "identical" else "diff",
                  n_A = n_a, n_B = n_b, n_value_diff = NA_integer_,
                  max_abs_diff = NA_real_,
                  note = "keyed join failed; used all.equal"))
    }
    n_only_A <- sum(out$side == "base")
    n_only_B <- sum(out$side == "other")
    ndiff <- sum(out$differs)
    max_abs <- if (ndiff > 0) max(abs(out$diff[out$differs])) else 0
    rows_only <- (n_only_A + n_only_B) > 0
    status <- if (ndiff == 0 && !rows_only) "identical"
      else if (ndiff == 0) "rows-mismatch" else "value-diff"
    return(list(status = status, n_A = n_a, n_B = n_b,
                n_value_diff = ndiff, max_abs_diff = max_abs,
                rows_only_A = n_only_A, rows_only_B = n_only_B))
  }

  # set / map: every column is a key -- a row-set comparison
  ka <- do.call(paste, c(lapply(da[ks$key], as.character), sep = "\r"))
  kb <- do.call(paste, c(lapply(db[ks$key], as.character), sep = "\r"))
  n_only_A <- length(setdiff(ka, kb))
  n_only_B <- length(setdiff(kb, ka))
  status <- if (n_only_A + n_only_B == 0) "identical" else "rows-mismatch"
  list(status = status, n_A = n_a, n_B = n_b,
       n_value_diff = 0L, max_abs_diff = 0,
       rows_only_A = n_only_A, rows_only_B = n_only_B)
}

# detailed sorted waldo diff for one parameter (bounded use; waldo optional)
.ci_waldo <- function(pa, pb, tol, unfold, scenA, scenB, names) {
  da <- .ci_norm(.ci_pdata(pa, scenA, unfold))
  db <- .ci_norm(.ci_pdata(pb, scenB, unfold))
  waldo::compare(da, db, tolerance = tol,
                 x_arg = names[1], y_arg = names[2])
}

# --------------------------------------------------------------------------- #
# non-parameter slots
# --------------------------------------------------------------------------- #
.ci_sets <- function(a, b) {
  sa <- .ci_slot(a, "sets", list()) %||% list()
  sb <- .ci_slot(b, "sets", list()) %||% list()
  na <- names(sa)
  nb <- names(sb)
  out <- list(only_A = setdiff(na, nb), only_B = setdiff(nb, na),
              members = list())
  for (s in intersect(na, nb)) {
    va <- as.character(sa[[s]])
    vb <- as.character(sb[[s]])
    if (!setequal(va, vb)) {
      out$members[[s]] <- list(only_A = setdiff(va, vb),
                               only_B = setdiff(vb, va))
    }
  }
  out
}

.ci_equations <- function(a, b) {
  ga <- .ci_slot(a, "user_constraints", list())
  gb <- .ci_slot(b, "user_constraints", list())
  ca <- .ci_slot(a, "user_costs", character(0))
  cb <- .ci_slot(b, "user_costs", character(0))
  list(cns_only_A = setdiff(names(ga), names(gb)),
       cns_only_B = setdiff(names(gb), names(ga)),
       cns_changed = Filter(function(nm)
         !isTRUE(all.equal(ga[[nm]], gb[[nm]])),
         intersect(names(ga), names(gb))),
       costs_only_A = setdiff(ca, cb),
       costs_only_B = setdiff(cb, ca))
}

.ci_misc <- function(a, b) {
  ma <- .ci_slot(a, "misc", list())
  mb <- .ci_slot(b, "misc", list())
  na <- names(ma)
  nb <- names(mb)
  changed <- Filter(function(nm) !isTRUE(all.equal(ma[[nm]], mb[[nm]])),
                    intersect(na, nb))
  list(only_A = setdiff(na, nb), only_B = setdiff(nb, na), changed = changed)
}

# --------------------------------------------------------------------------- #
# printer
# --------------------------------------------------------------------------- #

#' @method print inputs_cmp
#' @param x an `inputs_cmp` object.
#' @param max_diffs integer, number of differing parameters to render with a
#'   detailed \pkg{waldo} diff (when waldo is installed).
#' @param ... ignored.
#' @rdname compare_inputs
#' @export
print.inputs_cmp <- function(x, max_diffs = 5L, ...) {
  nm <- x$names
  bar <- strrep("=", 75)
  cat(bar, "\n",
      sprintf("compare_inputs:  A = %s   vs   B = %s   (tol=%g, unfold=%s)\n",
              nm[1], nm[2], x$tol, x$unfold),
      bar, "\n", sep = "")

  sdf <- x$summary
  nident <- sum(sdf$status == "identical")
  ndiff <- sum(sdf$status != "identical")
  cat(sprintf(
    "Parameters: %d common  | identical: %d  differing: %d  | only-%s: %d  only-%s: %d\n",
    nrow(sdf), nident, ndiff, nm[1], length(x$params$only_A),
    nm[2], length(x$params$only_B)))
  if (length(x$params$only_A)) {
    cat("  only in ", nm[1], ": ",
        paste(x$params$only_A, collapse = ", "), "\n", sep = "")
  }
  if (length(x$params$only_B)) {
    cat("  only in ", nm[2], ": ",
        paste(x$params$only_B, collapse = ", "), "\n", sep = "")
  }

  if (ndiff) {
    cat("\n--- Differing parameters ---\n")
    d <- sdf[sdf$status != "identical", , drop = FALSE]
    d <- d[order(d$status, d$parameter), , drop = FALSE]
    print(d, row.names = FALSE)
  } else {
    cat("\nAll common parameters identical.\n")
  }

  s <- x$sets
  if (length(s$only_A) || length(s$only_B) || length(s$members)) {
    cat("\n--- Sets ---\n")
    if (length(s$only_A)) cat("  only in ", nm[1], ": ",
                              paste(s$only_A, collapse = ", "), "\n", sep = "")
    if (length(s$only_B)) cat("  only in ", nm[2], ": ",
                              paste(s$only_B, collapse = ", "), "\n", sep = "")
    for (sn in base::names(s$members)) {
      m <- s$members[[sn]]
      cat(sprintf("  %-14s  +%s:[%s]  +%s:[%s]\n", sn,
                  nm[1], paste(m$only_A, collapse = ","),
                  nm[2], paste(m$only_B, collapse = ",")))
    }
  } else cat("\nSets: identical.\n")

  e <- x$equations
  if (length(e$cns_only_A) || length(e$cns_only_B) ||
      length(e$cns_changed) || length(e$costs_only_A) ||
      length(e$costs_only_B)) {
    cat("\n--- User constraints / costs ---\n")
    if (length(e$cns_only_A)) cat("  constraints only ", nm[1], ": ",
      paste(e$cns_only_A, collapse = ", "), "\n", sep = "")
    if (length(e$cns_only_B)) cat("  constraints only ", nm[2], ": ",
      paste(e$cns_only_B, collapse = ", "), "\n", sep = "")
    if (length(e$cns_changed)) cat("  constraints changed: ",
      paste(e$cns_changed, collapse = ", "), "\n", sep = "")
    if (length(e$costs_only_A)) cat("  cost terms only ", nm[1], ": ",
      paste(e$costs_only_A, collapse = ", "), "\n", sep = "")
    if (length(e$costs_only_B)) cat("  cost terms only ", nm[2], ": ",
      paste(e$costs_only_B, collapse = ", "), "\n", sep = "")
  } else cat("User constraints / costs: identical.\n")

  m <- x$misc
  if (length(m$only_A) || length(m$only_B) || length(m$changed)) {
    cat("\n--- misc ---\n")
    if (length(m$only_A)) cat("  only ", nm[1], ": ",
                              paste(m$only_A, collapse = ", "), "\n", sep = "")
    if (length(m$only_B)) cat("  only ", nm[2], ": ",
                              paste(m$only_B, collapse = ", "), "\n", sep = "")
    if (length(m$changed)) cat("  changed: ",
                               paste(m$changed, collapse = ", "), "\n",
                               sep = "")
  } else cat("misc: identical.\n")

  diffp <- base::names(Filter(function(p) length(p$meta_diffs),
                              x$params$common))
  if (length(diffp)) {
    cat("\n--- Metadata diffs ---\n")
    for (nmp in diffp) {
      md <- x$params$common[[nmp]]$meta_diffs
      cat(sprintf("  %s:\n", nmp))
      for (sl in base::names(md)) {
        cat(sprintf("      %-16s %s = %s | %s = %s\n", sl,
                    nm[1], md[[sl]]$A, nm[2], md[[sl]]$B))
      }
    }
  }

  wp <- base::names(Filter(function(p)
    p$data$status %in% c("value-diff", "rows-mismatch", "diff", "structural"),
    x$params$common))
  if (length(wp)) {
    if (requireNamespace("waldo", quietly = TRUE)) {
      show <- utils::head(wp, max_diffs)
      cat("\n--- Data diffs (waldo, first ", length(show), " of ",
          length(wp), ") ---\n", sep = "")
      for (nmp in show) {
        p <- x$params$common[[nmp]]
        cat(strrep("-", 60), "\n", nmp, "  [", p$data$status, "]\n",
            sep = "")
        print(.ci_waldo(p$pa, p$pb, x$tol, x$unfold, x$scenA, x$scenB, nm))
      }
    } else {
      cat("\n(install the 'waldo' package for detailed per-parameter",
          "data diffs)\n")
    }
  }
  cat("\n", bar, "\n", sep = "")
  invisible(x)
}
