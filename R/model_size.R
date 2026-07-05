# =========================================================================== #
# model_size.R  —  size estimate of an interpolated scenario, and the result of
# folding.
#
# Reports, for `scen@modInp`:
#   * total value-parameter rows (numpar + bounds) and the top-N parameters,
#   * an estimate of the model's #variables / #constraints from the row counts of
#     the variable-gating (`gates_var`) and equation-gating (`gates_eq`) maps in
#     the mapping spec (a variable/equation instance exists per gating-map tuple),
#   * what folding saved: the folded row total vs the equivalent unfolded total
#     (each wildcard re-expanded over its membership), plus how many parameters
#     fold on each dimension.
#
# Non-zeros are out of scope (they need the assembled constraint matrix / solver
# output, not the modInp). Surfaced standalone (`model_size()` + print method),
# by `summary(scenario)`, and at the end of a `verbose` `interp_mod()` run.
# =========================================================================== #

# Row count of a parameter, preferring the cached nValues (avoids reading on-disk
# data) and falling back to the materialised slot.
.ms_rows <- function(p) {
  nv <- p@misc$nValues
  if (!is.null(nv) && length(nv) == 1L && !is.na(nv) && nv >= 0) return(as.integer(nv))
  d <- get_data_slot(p)
  if (is.null(d)) 0L else nrow(d)
}

# Total rows of the value parameters (numpar + bounds). Used by `interp_mod` to
# record the pre-fold total (so the exact fold saving is known) and by `model_size`.
.value_param_rows <- function(scen) {
  P <- scen@modInp@parameters
  sum(vapply(names(P), function(nm) {
    p <- P[[nm]]
    if (is.null(p) || !as.character(p@type) %in% c("numpar", "bounds")) return(0L)
    .ms_rows(p)
  }, integer(1)))
}

#' Size estimate of an interpolated scenario and the result of folding
#'
#' @param scen a scenario built by [interpolate_model()].
#' @param top_n number of largest parameters to list.
#' @return an S3 `model_size` object (see `print.model_size()`).
#' @export
model_size <- function(scen, top_n = 15L) {
  P <- scen@modInp@parameters
  spec <- tryCatch(load_mapping_spec(), error = function(e) list())

  nm    <- names(P)
  type  <- vapply(nm, function(x) if (is.null(P[[x]])) NA_character_ else
    as.character(P[[x]]@type), character(1))
  rows  <- vapply(nm, function(x) if (is.null(P[[x]])) 0L else .ms_rows(P[[x]]),
                  integer(1))
  is_val <- type %in% c("numpar", "bounds")

  # --- variable / constraint estimate from gating-map row counts -------------- #
  n_var <- 0L; n_con <- 0L
  for (x in nm[type == "map"]) {
    sp <- spec[[x]]; if (is.null(sp)) next
    r <- rows[[x]]
    if (length(sp$gates_var) && any(nzchar(sp$gates_var))) n_var <- n_var + r
    if (length(sp$gates_eq)  && any(nzchar(sp$gates_eq)))  n_con <- n_con + r
  }

  # --- fold result ------------------------------------------------------------ #
  # How many value parameters carry a wildcard on each foldable dim.
  fold_by_dim <- stats::setNames(integer(length(.foldable_dims)), .foldable_dims)
  for (x in nm[is_val]) {
    d <- as.data.frame(get_data_slot(P[[x]]))
    if (nrow(d) == 0) next
    for (dd in intersect(.foldable_dims, names(d))) {
      if (any(is.na(d[[dd]]) | is_any(d[[dd]]))) fold_by_dim[[dd]] <- fold_by_dim[[dd]] + 1L
    }
  }
  folded_total <- sum(rows[is_val])
  # Exact saving of the fold STEP: interp_mod records the value-parameter total
  # just before `fold_scenario_parameters` (R/interp.R). NULL for an unfolded build
  # or a scenario loaded without the record -> report no saving.
  rb <- scen@misc$fold_rows_before
  before_fold <- if (!is.null(rb) && length(rb) == 1L && !is.na(rb) && rb >= folded_total)
    as.integer(rb) else NA_integer_

  ord <- order(rows[is_val], decreasing = TRUE)
  top <- data.frame(parameter = nm[is_val][ord], rows = rows[is_val][ord],
                    type = type[is_val][ord], stringsAsFactors = FALSE)

  structure(list(
    name           = scen@name,
    n_param        = sum(is_val),
    n_map          = sum(type == "map"),
    n_set          = sum(type == "set"),
    param_rows     = folded_total,
    before_fold    = before_fold,
    rows_saved     = if (is.na(before_fold)) NA_integer_ else before_fold - folded_total,
    n_var_est      = n_var,
    n_con_est      = n_con,
    fold_by_dim    = fold_by_dim[fold_by_dim > 0],
    top            = utils::head(top, top_n)
  ), class = "model_size")
}

#' @method print model_size
#' @export
print.model_size <- function(x, ...) {
  cat(sprintf("model_size: %s\n", x$name))
  cat(sprintf("  parameters : %d value, %d maps, %d sets\n",
              x$n_param, x$n_map, x$n_set))
  if (!is.na(x$before_fold)) {
    pct <- if (x$before_fold > 0)
      sprintf(" = %.0f%%", 100 * x$rows_saved / x$before_fold) else ""
    cat(sprintf("  param rows : %s  (fold step: %s -> %s, saved %s%s)\n",
                format(x$param_rows, big.mark = ","),
                format(x$before_fold, big.mark = ","),
                format(x$param_rows, big.mark = ","),
                format(x$rows_saved, big.mark = ","), pct))
  } else {
    cat(sprintf("  param rows : %s\n", format(x$param_rows, big.mark = ",")))
  }
  if (length(x$fold_by_dim))
    cat(sprintf("  folded dims: %s\n",
                paste(names(x$fold_by_dim), x$fold_by_dim, sep = "=",
                      collapse = "  ")))
  cat(sprintf("  estimate   : ~%s variables, ~%s constraints (from gating maps)\n",
              format(x$n_var_est, big.mark = ","),
              format(x$n_con_est, big.mark = ",")))
  cat("  top parameters by rows:\n")
  tp <- x$top
  for (i in seq_len(nrow(tp)))
    cat(sprintf("    %-18s %s\n", tp$parameter[i],
                format(tp$rows[i], big.mark = ",")))
  invisible(x)
}

#' Compare interpolation settings (size & build time) for a model
#'
#' Interpolates `mod` in memory under each of several setting combinations and
#' tabulates how big the resulting model is: total value-parameter rows, the
#' parameter / map / set counts, the variable & constraint estimate (from
#' [model_size()]), the in-memory size, and the build time. A quick way to see
#' how `fold` / `sparse` / `prune` trade off model size and speed before
#' committing to a solve.
#'
#' All builds use `ondisk = FALSE` (so sizes are measured in memory and are
#' directly comparable) and `overwrite = TRUE`.
#'
#' @param mod a `model`.
#' @param settings a *named* list of setting combinations; each element is a list
#'   of arguments forwarded to [interpolate_model()], e.g.
#'   `list(fold = TRUE, sparse = TRUE, prune = TRUE)`. When `NULL` a default grid
#'   (dense / sparse / sparse+prune / fold+sparse+prune / fold-all) is used.
#' @param ... arguments forwarded to EVERY `interp_mod()` call, e.g.
#'   `horizon = newHorizon(period = 2024)`.
#' @param name base scenario name; each build is `"<name>_<setting>"`.
#' @param verbose forwarded to [interpolate_model()] (per-build progress).
#' @param top_n number of largest parameters to keep per build.
#' @param keep_scen if `TRUE`, the interpolated scenarios are returned in
#'   `$scen` (handy to feed a later solve step). Off by default to save memory.
#'
#' @return an object of class `interp_settings_cmp`: a list with
#'   * `summary` — one row per setting (ok, seconds, mb, param_rows, counts,
#'     variable/constraint estimates, error),
#'   * `top` — wide data.frame of the largest parameters' row counts per setting,
#'   * `details` — named list of per-build [model_size()] objects,
#'   * `scen` — named list of scenarios when `keep_scen = TRUE`,
#'   * `settings` — the settings grid used.
#'
#' @examples
#' \dontrun{
#' cmp <- compare_interp_settings(mod, horizon = newHorizon(period = 2024))
#' cmp                       # prints the comparison table
#' # custom grid:
#' compare_interp_settings(mod,
#'   settings = list(
#'     none    = list(fold = FALSE, sparse = FALSE, prune = FALSE),
#'     all     = list(fold = c("region","slice","year","comm","tech","stg","trade"),
#'                    sparse = TRUE, prune = TRUE)),
#'   horizon = newHorizon(period = 2024))
#' }
#' @seealso [model_size()], [interpolate_model()]
#' @export
compare_interp_settings <- function(mod, settings = NULL, ...,
                                    name = "cmp", verbose = FALSE,
                                    top_n = 12L, keep_scen = FALSE) {
  if (is.null(settings)) {
    settings <- list(
      dense             = list(fold = FALSE, sparse = FALSE, prune = FALSE),
      sparse            = list(fold = FALSE, sparse = TRUE,  prune = FALSE),
      sparse_prune      = list(fold = FALSE, sparse = TRUE,  prune = TRUE),
      fold_sparse_prune = list(fold = TRUE,  sparse = TRUE,  prune = TRUE),
      foldall_sparse_prune = list(
        fold = c("region", "slice", "year", "comm", "tech", "stg", "trade"),
        sparse = TRUE, prune = TRUE)
    )
  }
  if (is.null(names(settings)) || any(!nzchar(names(settings)))) {
    names(settings) <- paste0("set", seq_along(settings))
  }
  dots <- list(...)

  details <- list()
  scens   <- list()
  rows    <- list()
  for (k in seq_along(settings)) {
    nm  <- names(settings)[k]
    arg <- settings[[k]]
    if (isTRUE(verbose)) message("interp [", nm, "] ...")
    t0  <- Sys.time()
    scen <- tryCatch(
      do.call(interp_mod, c(
        list(mod, name = paste0(name, "_", nm), ondisk = FALSE,
             overwrite = TRUE, verbose = verbose),
        arg, dots
      )),
      error = function(e) e
    )
    secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
    if (inherits(scen, "error")) {
      rows[[nm]] <- data.frame(
        setting = nm, ok = FALSE, seconds = secs, mb = NA_real_,
        param_rows = NA_integer_, n_param = NA_integer_, n_map = NA_integer_,
        n_set = NA_integer_, n_var_est = NA_integer_, n_con_est = NA_integer_,
        error = conditionMessage(scen), stringsAsFactors = FALSE)
      next
    }
    ms <- model_size(scen, top_n = top_n)
    details[[nm]] <- ms
    if (isTRUE(keep_scen)) scens[[nm]] <- scen
    rows[[nm]] <- data.frame(
      setting = nm, ok = TRUE, seconds = secs,
      mb = round(as.numeric(utils::object.size(scen@modInp)) / 1024^2, 1),
      param_rows = ms$param_rows, n_param = ms$n_param, n_map = ms$n_map,
      n_set = ms$n_set, n_var_est = ms$n_var_est, n_con_est = ms$n_con_est,
      error = NA_character_, stringsAsFactors = FALSE)
  }
  summary <- do.call(rbind, rows)
  rownames(summary) <- NULL

  # Wide table of the largest parameters' rows across builds, ordered by the
  # biggest row count any build produced for that parameter.
  top <- NULL
  if (length(details)) {
    allp <- unique(unlist(lapply(details, function(d) d$top$parameter)))
    top  <- data.frame(parameter = allp, stringsAsFactors = FALSE)
    for (nm in names(details)) {
      d <- details[[nm]]
      top[[nm]] <- d$top$rows[match(allp, d$top$parameter)]
    }
    mx <- apply(top[, -1, drop = FALSE], 1,
                function(z) suppressWarnings(max(z, na.rm = TRUE)))
    top <- top[order(-mx), , drop = FALSE]
    rownames(top) <- NULL
  }

  structure(list(summary = summary, top = top, details = details,
                 scen = if (isTRUE(keep_scen)) scens else NULL,
                 settings = settings),
            class = "interp_settings_cmp")
}

#' @method print interp_settings_cmp
#' @export
print.interp_settings_cmp <- function(x, top = 12L, ...) {
  cat("interp settings comparison\n")
  print(x$summary, row.names = FALSE)
  if (!is.null(x$top) && nrow(x$top) > 0) {
    cat("\ntop parameters (rows) per setting:\n")
    print(utils::head(x$top, top), row.names = FALSE)
  }
  invisible(x)
}

#' Compare interpolation settings AND solvers for a model
#'
#' Interpolates `mod` under each setting combination (via
#' [compare_interp_settings()]) and then solves every build with each supplied
#' solver option, tabulating the objective, solve time and status. Because the
#' solution must not depend on storage settings, the objective should be
#' identical across `fold` / `sparse` / `prune` for a given solver -- the print
#' method flags any per-solver disagreement.
#'
#' @param mod a `model`.
#' @param settings a named list of `interp_mod()` setting combinations (see
#'   [compare_interp_settings()]); `NULL` uses the default grid.
#' @param solvers a list of solver-option objects, e.g.
#'   `list(solver_options$glpk, solver_options$julia_highs)`. Names are taken
#'   from the list names when present, otherwise from each option's `$name`.
#' @param ... forwarded to every `interp_mod()` build (e.g.
#'   `horizon = newHorizon(period = 2024)`).
#' @param name base scenario name.
#' @param verbose forwarded to `interp_mod()`.
#' @param tmp.dir solver working directory (passed to [solve_scen()]); `NULL`
#'   lets the solver pick one.
#'
#' @return an object of class `solve_settings_cmp`: `summary` (one row per
#'   setting x solver: ok, seconds, objective, error), `interp` (the interp-size
#'   table), plus `top` / `details` from the interpolation comparison.
#'
#' @examples
#' \dontrun{
#' compare_solve_settings(mod,
#'   solvers = list(solver_options$glpk),
#'   horizon = newHorizon(period = 2024))
#' }
#' @seealso [compare_interp_settings()], [solve_scen()], [model_size()]
#' @export
compare_solve_settings <- function(mod, settings = NULL, solvers, ...,
                                   name = "cmp", verbose = FALSE,
                                   tmp.dir = NULL) {
  if (!is.list(solvers) || length(solvers) == 0) {
    stop("`solvers` must be a non-empty list, e.g. list(solver_options$glpk).")
  }
  snames <- names(solvers)
  if (is.null(snames)) snames <- rep("", length(solvers))
  for (i in seq_along(solvers)) {
    if (!nzchar(snames[i])) {
      lab <- tryCatch(solvers[[i]]$name, error = function(e) NULL)
      snames[i] <- if (is.null(lab) || !nzchar(lab)) paste0("solver", i) else lab
    }
  }

  # Interpolate every setting once, keeping the scenarios to solve.
  ic <- compare_interp_settings(mod, settings, ..., name = name,
                                verbose = verbose, keep_scen = TRUE)

  rows <- list()
  for (st in names(ic$scen)) {
    scen <- ic$scen[[st]]
    for (i in seq_along(solvers)) {
      sv <- solvers[[i]]
      sl <- snames[i]
      if (isTRUE(verbose)) message("solve [", st, " x ", sl, "] ...")
      t0 <- Sys.time()
      res <- tryCatch(
        solve_scen(scen, solver = sv, force = TRUE, tmp.dir = tmp.dir),
        error = function(e) e)
      secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
      if (inherits(res, "error")) {
        rows[[length(rows) + 1L]] <- data.frame(
          setting = st, solver = sl, ok = FALSE, seconds = secs,
          objective = NA_real_, error = conditionMessage(res),
          stringsAsFactors = FALSE)
      } else {
        obj <- tryCatch(getData(res, "vObjective", merge = TRUE)$value[1],
                        error = function(e) NA_real_)
        rows[[length(rows) + 1L]] <- data.frame(
          setting = st, solver = sl, ok = TRUE, seconds = secs,
          objective = obj, error = NA_character_, stringsAsFactors = FALSE)
      }
    }
  }
  summary <- do.call(rbind, rows)
  rownames(summary) <- NULL

  structure(list(summary = summary, interp = ic$summary,
                 top = ic$top, details = ic$details),
            class = "solve_settings_cmp")
}

#' @method print solve_settings_cmp
#' @export
print.solve_settings_cmp <- function(x, ...) {
  cat("solve settings comparison\n")
  print(x$summary, row.names = FALSE)
  # Objective invariance: per solver, the objective must not vary with storage
  # settings (fold/sparse/prune). Flag any disagreement (numeric tolerance).
  ok <- x$summary[x$summary$ok & !is.na(x$summary$objective), , drop = FALSE]
  if (nrow(ok) > 0) {
    cat("\nobjective by solver (should match across settings):\n")
    for (sl in unique(ok$solver)) {
      v <- ok$objective[ok$solver == sl]
      tag <- if (length(v) > 1 &&
                 diff(range(v)) > 1e-6 * max(1, abs(stats::median(v)))) {
        "  <-- DIFFER!"
      } else {
        ""
      }
      cat(sprintf("  %-14s %s%s\n", sl,
                  paste(format(v, digits = 8), collapse = "  "), tag))
    }
  }
  invisible(x)
}
