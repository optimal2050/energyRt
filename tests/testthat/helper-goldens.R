# =========================================================================== #
# Golden-benchmark helpers: capture and compare tracked solution values at
# several time/geo aggregation levels, with tolerances per comparison kind.
#
# Goldens are tracked JSON files in tests/testthat/goldens/, written ONLY by
# tools/test/make_goldens.R (which refuses to freeze a benchmark that fails
# verify_solution()). Tests read them with golden_read() and compare with
# expect_matches_golden(); a missing golden file or entry skips with a
# message naming the capture command, so a fresh checkout fails soft.
#
# Tracked-value schema per scenario entry:
#   objective: scalar
#   values: list per tracked variable, each with levels
#     annual_region  -- summed over timeslices, by remaining dims + region+year
#     annual_total   -- additionally summed over regions (skipped for
#                       interregional-role variables, which must never be
#                       summed spatially)
#   captured: {date, version, solver}
# Finest-level (per-timeslice) values are deliberately NOT stored: goldens
# must be comparable across solvers, and per-timeslice values of degenerate
# optima are not. Same-solver determinism is asserted via the objective at
# TOL_SAME_SOLVER instead.
# =========================================================================== #

# objective reproduced by the SAME solver -- deterministic, tight
TOL_SAME_SOLVER <- 1e-9
# objective across solvers (LP, all backends must agree)
TOL_XSOLVER_OBJ <- 1e-6
# aggregated quantities across solvers (alternative optima may shift flows
# between equivalent paths; aggregates are stable, raw values are not)
TOL_XSOLVER_AGG <- 1e-5
TOL_ABS_FLOOR <- 1e-6

golden_path <- function(suite) {
  testthat::test_path("goldens", paste0(suite, ".json"))
}

golden_read <- function(suite) {
  p <- golden_path(suite)
  if (!file.exists(p)) return(NULL)
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  jsonlite::fromJSON(p, simplifyVector = FALSE)
}

skip_if_no_golden <- function(suite, entry = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    testthat::skip("jsonlite not installed")
  }
  g <- golden_read(suite)
  if (is.null(g)) {
    testthat::skip(paste0("no golden file '", suite, "' -- run: Rscript ",
                          "tools/test/make_goldens.R --suite=", suite))
  }
  if (!is.null(entry) && is.null(g[[entry]])) {
    testthat::skip(paste0("golden '", suite, "' has no entry '", entry,
                          "' -- re-run make_goldens.R"))
  }
  invisible(g)
}

# default variables tracked in goldens: one per role family that is stable
# under alternative optima once aggregated
.golden_default_vars <- c("vTechNewCap", "vTechOut", "vTechInp",
                          "vSupOut", "vDemInp", "vEmsFuelTot")

# Capture the tracked-value table of a solved scenario.
capture_tracked_values <- function(scen, variables = .golden_default_vars,
                                   digits = 10) {
  vals <- list()
  for (nm in variables) {
    v <- scen@modOut@variables[[nm]]
    if (is.null(v)) next
    d <- get_data_slot(v, optional = TRUE)
    if (is.null(d) || nrow(d) == 0) next
    d <- data.table::as.data.table(d)
    for (j in colnames(d)) {
      if (is.factor(d[[j]])) data.table::set(d, j = j, value = as.character(d[[j]]))
    }
    grp_ar <- setdiff(colnames(d), c("timeslice", "value"))
    ar <- d[, .(value = signif(sum(value), digits)), by = grp_ar]
    data.table::setorderv(ar, grp_ar)
    lv <- list(annual_region = ar)
    if (!identical(v@role, "interregional") && "region" %in% colnames(d)) {
      grp_at <- setdiff(grp_ar, "region")
      at <- d[, .(value = signif(sum(value), digits)), by = grp_at]
      data.table::setorderv(at, grp_at)
      lv$annual_total <- at
    }
    vals[[nm]] <- lv
  }
  obj <- scen@modOut@variables[["vObjective"]]
  objective <- if (!is.null(obj)) {
    d <- get_data_slot(obj, optional = TRUE)
    if (!is.null(d) && nrow(d) > 0) signif(d$value[1], 12) else NA_real_
  } else NA_real_
  list(objective = objective, values = vals)
}

# Compare one golden level table (list-of-rows from JSON) against a freshly
# captured data.table. Every golden row must exist with a matching value and
# no extra non-zero rows may appear.
.golden_compare_level <- function(golden_rows, fresh, tol_rel, tol_abs, label) {
  gd <- data.table::rbindlist(lapply(golden_rows, data.table::as.data.table),
                              use.names = TRUE, fill = TRUE)
  if ("year" %in% colnames(gd)) gd[, year := as.integer(year)]
  keys <- setdiff(colnames(gd), "value")
  m <- merge(gd, fresh, by = keys, all = TRUE, suffixes = c(".golden", ".new"))
  m[is.na(value.golden), value.golden := 0]
  m[is.na(value.new), value.new := 0]
  bad <- abs(m$value.new - m$value.golden) >
    tol_abs + tol_rel * pmax(abs(m$value.new), abs(m$value.golden))
  if (any(bad)) {
    testthat::fail(paste0(
      label, ": ", sum(bad), " of ", nrow(m), " tracked values differ; first: ",
      paste(utils::capture.output(print(m[bad][1])), collapse = " ")))
  } else {
    testthat::succeed()
  }
  invisible(!any(bad))
}

# Assert a solved scenario matches its golden entry. `kind` picks tolerances:
# "same_solver" for the reference backend, "cross_solver" for others.
expect_matches_golden <- function(scen, suite, entry,
                                  kind = c("same_solver", "cross_solver"),
                                  variables = .golden_default_vars) {
  kind <- match.arg(kind)
  g <- skip_if_no_golden(suite, entry)
  ge <- g[[entry]]
  tol_obj <- if (kind == "same_solver") TOL_SAME_SOLVER else TOL_XSOLVER_OBJ
  tol_agg <- if (kind == "same_solver") TOL_SAME_SOLVER else TOL_XSOLVER_AGG

  fresh <- capture_tracked_values(scen, variables = variables)
  testthat::expect_equal(fresh$objective, as.numeric(ge$objective),
                         tolerance = tol_obj,
                         label = paste0(entry, " objective"))
  for (nm in names(ge$values)) {
    for (lvl in names(ge$values[[nm]])) {
      fr <- fresh$values[[nm]][[lvl]]
      if (is.null(fr)) {
        testthat::fail(paste0(entry, ": tracked variable ", nm, " (", lvl,
                              ") missing from fresh solution"))
        next
      }
      .golden_compare_level(ge$values[[nm]][[lvl]], fr,
                            tol_rel = tol_agg, tol_abs = TOL_ABS_FLOOR,
                            label = paste0(entry, ":", nm, ":", lvl))
    }
  }
  invisible(fresh)
}
