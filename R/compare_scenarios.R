# =========================================================================== #
# compare_scenarios.R -- results comparison across solved scenarios (or runs
# of one scenario), with print / autoplot methods; the comparative report
# methods (Stage 4) live here too.
# =========================================================================== #
#' @include report.R

#' Compare solved scenarios (or runs of one scenario)
#'
#' Builds a results comparison across a named list of solved scenarios, or
#' across recorded runs of a single scenario (see [scenario_runs()]). One
#' element is the **base** (default: the first); every other element is
#' compared against it: objectives and run metadata in an overview table,
#' and selected solved variables value-by-value through a tolerance-aware
#' full join at an annual aggregation level.
#'
#' Variables default to the role-driven selection the scenario report uses:
#' every solved variable the catalogue declares as a cost (minus
#' `vObjective` / `vTotalCost`) plus the key quantity variables
#' (`vTechCap`, `vTechNewCap`, `vTechOut`, `vEmsFuelTot`) that are present.
#'
#' @param scen a **named** list of solved scenarios, or a single scenario
#'   when `runs` is given.
#' @param runs character, run ids of `scen` to compare (as listed by
#'   [scenario_runs()]); each is read into a local copy via
#'   [read_solution()] -- the caller's scenario keeps its active run.
#' @param name character, variables to compare (overrides the role-driven
#'   default).
#' @param base integer position or element name of the baseline (default:
#'   the first element).
#' @param level aggregation level for value comparisons:
#'   `"annual_region"` (sum over timeslices) or `"annual_total"` (also over
#'   regions; variables with role `"interregional"` are skipped there).
#' @param tol_rel,tol_abs numeric comparison tolerances (a value differs
#'   when `abs(diff) > tol_abs + tol_rel * max(abs(values))`).
#' @param inputs logical, also diff the model inputs of every scenario
#'   against the base via [compare_inputs()].
#' @param top_n integer, number of largest absolute differences kept in the
#'   `top` table.
#'
#' @return A `scenarios_cmp` object (a list) with components:
#'   `overview` (one row per scenario: run/solver/status/objective and
#'   objective differences vs base), `values` (per variable x scenario:
#'   `n_diff`, `max_abs_diff`, `max_rel_diff`, status), `deltas` (named
#'   list per variable of row-level differences), `top` (the `top_n`
#'   largest absolute differences), `inputs` (list of [compare_inputs()]
#'   results when `inputs = TRUE`), and the carried `scen` list / `base` /
#'   `level` / `tol`. The object holds the full scenario list so
#'   `autoplot()` and `report()` can rebuild mixes from it.
#'   `print()` gives a compact report; `autoplot()` draws comparison
#'   charts; `report()` renders the comparative report.
#'
#' @seealso [compare_inputs()], [compare_models()], [getMix()] (which also
#'   accepts named scenario lists), [scenario_runs()].
#' @family comparison
#' @export
compare_scenarios <- function(scen, runs = NULL, name = NULL, base = 1L,
                              level = c("annual_region", "annual_total"),
                              tol_rel = 1e-6, tol_abs = 1e-6,
                              inputs = FALSE, top_n = 10L) {
  level <- match.arg(level)
  runs_mode <- !is.null(runs)

  # -- normalize to a named list of scenarios ------------------------------
  if (runs_mode) {
    if (!methods::is(scen, "scenario")) {
      stop("`runs =` requires a single scenario; got ", class(scen)[1])
    }
    rr <- tryCatch(scenario_runs(scen), error = function(e) NULL)
    bad <- setdiff(runs, rr$run)
    if (is.null(rr) || length(bad) > 0) {
      stop("unknown run(s): ", paste0('"', bad, '"', collapse = ", "),
           ". Available runs: ",
           if (is.null(rr) || nrow(rr) == 0) "(none)" else
             paste0('"', rr$run, '"', collapse = ", "))
    }
    active <- rr$run[which(rr$active)][1] %||% NA_character_
    sl <- lapply(runs, function(r) {
      if (identical(r, active)) scen else
        suppressMessages(read_solution(scen, run = r))
    })
    names(sl) <- runs
  } else {
    if (methods::is(scen, "scenario")) {
      stop("give a named list of scenarios, or a single scenario with ",
           "`runs =` (see scenario_runs())")
    }
    stopifnot(is.list(scen), length(scen) >= 2)
    ok <- vapply(scen, function(x) methods::is(x, "scenario"), logical(1))
    if (!all(ok)) {
      stop("all elements must be scenarios; got: ",
           paste(vapply(scen[!ok], function(x) class(x)[1], ""),
                 collapse = ", "))
    }
    nms <- names(scen) %||% rep("", length(scen))
    fix <- !nzchar(nms)
    nms[fix] <- vapply(scen[fix], function(x) x@name, "")
    if (anyDuplicated(nms)) {
      stop("scenario names must be unique; duplicated: ",
           paste(unique(nms[duplicated(nms)]), collapse = ", "),
           ". Name the list elements explicitly.")
    }
    sl <- stats::setNames(scen, nms)
  }

  # -- base resolution (reorder base first) --------------------------------
  base_nm <- if (is.character(base)) base else names(sl)[base]
  if (is.na(base_nm) || !base_nm %in% names(sl)) {
    stop("`base` must name one of: ", paste(names(sl), collapse = ", "))
  }
  sl <- sl[c(base_nm, setdiff(names(sl), base_nm))]
  base_scen <- sl[[1]]

  # -- overview ------------------------------------------------------------
  ov_row <- function(nm, s) {
    rr <- tryCatch(scenario_runs(s), error = function(e) NULL)
    a <- if (!is.null(rr) && any(rr$active))
      rr[which(rr$active)[1], , drop = FALSE] else NULL
    g <- function(col) {
      v <- if (!is.null(a) && col %in% names(a)) a[[col]][1] else NA
      if (is.null(v)) NA else v
    }
    objective <- suppressWarnings(as.numeric(g("objective")))
    if (is.na(objective)) {
      objective <- tryCatch({
        d <- getData(s, name = "vObjective", merge = TRUE)
        as.numeric(d$value[1])
      }, error = function(e) NA_real_)
    }
    data.frame(
      scenario = nm,
      run = as.character(g("run") %||% NA_character_),
      model = tryCatch(s@model@name, error = function(e) NA_character_),
      model_hash8 = tryCatch(substr(model_hash(s@model), 1, 8),
                             error = function(e) NA_character_),
      solver = as.character(g("solver_name") %||% NA_character_),
      status = as.character(g("status") %||% NA_character_),
      optimal = isTRUE(s@status$optimal),
      started = as.character(g("started") %||% NA_character_),
      duration_sec = suppressWarnings(as.numeric(g("duration_sec"))),
      objective = objective,
      stringsAsFactors = FALSE)
  }
  overview <- do.call(rbind, Map(ov_row, names(sl), sl))
  rownames(overview) <- NULL
  overview$obj_diff <- overview$objective - overview$objective[1]
  overview$obj_rel <- overview$obj_diff /
    pmax(abs(overview$objective[1]), tol_abs)

  # -- variable selection --------------------------------------------------
  vars <- if (!is.null(name)) {
    name
  } else {
    vs <- tryCatch(base_scen@modOut@variables, error = function(e) list())
    cost_vars <- names(vs)[vapply(vs, function(v)
      identical(tryCatch(v@role, error = function(e) NA_character_), "cost"),
      logical(1))]
    cost_vars <- setdiff(cost_vars, c("vObjective", "vTotalCost"))
    has_data <- function(nm) {
      d <- tryCatch(get_data_slot(vs[[nm]], optional = TRUE),
                    error = function(e) NULL)
      !is.null(d) && nrow(d) > 0
    }
    cand <- unique(c(Filter(has_data, cost_vars),
                     Filter(has_data, intersect(
                       c("vTechCap", "vTechNewCap", "vTechOut",
                         "vEmsFuelTot"), names(vs)))))
    cand
  }

  # -- value comparisons vs base -------------------------------------------
  deltas <- list()
  val_rows <- list()
  for (v in vars) {
    bt <- .cmp_var_table(base_scen, v, level)
    dparts <- list()
    for (nm in names(sl)[-1]) {
      ot <- .cmp_var_table(sl[[nm]], v, level)
      if (is.null(bt) && is.null(ot)) {
        val_rows[[length(val_rows) + 1L]] <- data.frame(
          variable = v, scenario = nm, n_base = 0L, n_scen = 0L,
          n_diff = 0L, max_abs_diff = 0, max_rel_diff = 0,
          status = "missing", stringsAsFactors = FALSE)
        next
      }
      if (is.null(bt) || is.null(ot)) {
        val_rows[[length(val_rows) + 1L]] <- data.frame(
          variable = v, scenario = nm,
          n_base = if (is.null(bt)) 0L else nrow(bt),
          n_scen = if (is.null(ot)) 0L else nrow(ot),
          n_diff = NA_integer_, max_abs_diff = NA_real_,
          max_rel_diff = NA_real_, status = "missing",
          stringsAsFactors = FALSE)
        next
      }
      out <- tryCatch(
        .compare_values(bt, ot, tol_rel = tol_rel, tol_abs = tol_abs),
        error = function(e) NULL)
      if (is.null(out)) {
        val_rows[[length(val_rows) + 1L]] <- data.frame(
          variable = v, scenario = nm, n_base = nrow(bt),
          n_scen = nrow(ot), n_diff = NA_integer_,
          max_abs_diff = NA_real_, max_rel_diff = NA_real_,
          status = "structural", stringsAsFactors = FALSE)
        next
      }
      ndiff <- sum(out$differs)
      val_rows[[length(val_rows) + 1L]] <- data.frame(
        variable = v, scenario = nm, n_base = nrow(bt), n_scen = nrow(ot),
        n_diff = ndiff,
        max_abs_diff = if (ndiff > 0) max(abs(out$diff[out$differs])) else 0,
        max_rel_diff = if (ndiff > 0)
          max(abs(out$rel_diff[out$differs])) else 0,
        status = if (ndiff > 0) "value-diff" else "identical",
        stringsAsFactors = FALSE)
      if (ndiff > 0) {
        dparts[[nm]] <- cbind(
          data.frame(scenario = nm, stringsAsFactors = FALSE), out)
      }
    }
    if (length(dparts) > 0) deltas[[v]] <- do.call(rbind, dparts)
  }
  values <- if (length(val_rows) > 0) do.call(rbind, val_rows) else
    data.frame(variable = character(), scenario = character(),
               stringsAsFactors = FALSE)
  rownames(values) <- NULL
  for (v in names(deltas)) rownames(deltas[[v]]) <- NULL

  # -- top differences across all variables --------------------------------
  top <- NULL
  if (length(deltas) > 0) {
    parts <- lapply(names(deltas), function(v) {
      d <- deltas[[v]]
      d <- d[d$differs, , drop = FALSE]
      if (nrow(d) == 0) return(NULL)
      keys <- setdiff(names(d), c("scenario", "base", "value", "diff",
                                  "rel_diff", "differs", "side"))
      where <- if (length(keys) > 0) {
        apply(d[, keys, drop = FALSE], 1, function(r)
          paste(paste0(keys, "=", r), collapse = " "))
      } else ""
      data.frame(variable = v, scenario = d$scenario, where = where,
                 base = d$base, value = d$value, diff = d$diff,
                 rel_diff = d$rel_diff, stringsAsFactors = FALSE)
    })
    parts <- Filter(Negate(is.null), parts)
    if (length(parts) > 0) {
      top <- do.call(rbind, parts)
      top <- top[order(-abs(top$diff)), , drop = FALSE]
      top <- utils::head(top, top_n)
      rownames(top) <- NULL
    }
  }

  # -- optional input diffs ------------------------------------------------
  inputs_cmp <- NULL
  if (isTRUE(inputs)) {
    inputs_cmp <- lapply(names(sl)[-1], function(nm)
      tryCatch(compare_inputs(base_scen, sl[[nm]],
                              names = c(names(sl)[1], nm)),
               error = function(e) NULL))
    names(inputs_cmp) <- names(sl)[-1]
  }

  structure(list(
    overview = overview, values = values, deltas = deltas, top = top,
    inputs = inputs_cmp, variables = vars,
    scen = sl, base = names(sl)[1], level = level,
    tol = list(rel = tol_rel, abs = tol_abs), runs_mode = runs_mode
  ), class = "scenarios_cmp")
}

#' @method print scenarios_cmp
#' @param x a `scenarios_cmp` object.
#' @param top integer, number of top differences to print.
#' @param ... ignored.
#' @rdname compare_scenarios
#' @export
print.scenarios_cmp <- function(x, top = 10L, ...) {
  cat("scenario comparison (base: ", x$base,
      if (x$runs_mode) ", runs of one scenario", ")\n", sep = "")
  ov <- x$overview
  # prune wide columns that carry no information across rows
  for (cc in c("model", "model_hash8", "started", "status")) {
    v <- ov[[cc]]
    if (length(unique(v[!is.na(v)])) <= 1) ov[[cc]] <- NULL
  }
  print(ov, row.names = FALSE)
  tol_obj <- x$tol$abs + x$tol$rel * max(abs(ov$objective), na.rm = TRUE)
  for (i in seq_len(nrow(ov))[-1]) {
    if (!is.na(ov$obj_diff[i]) && abs(ov$obj_diff[i]) > tol_obj) {
      cat("  objective ", ov$scenario[i], " differs from base by ",
          format(signif(ov$obj_diff[i], 5), big.mark = ","), "  <-- DIFFER\n",
          sep = "")
    }
  }
  vd <- x$values[!x$values$status %in% "identical", , drop = FALSE]
  if (nrow(vd) > 0) {
    cat("\n--- Differing variables ---\n")
    print(vd, row.names = FALSE)
  } else {
    cat("\nAll compared values within tolerance.\n")
  }
  if (!is.null(x$top) && nrow(x$top) > 0) {
    cat("\n--- Largest differences ---\n")
    print(utils::head(x$top, top), row.names = FALSE)
  }
  if (!is.null(x$inputs)) {
    for (nm in names(x$inputs)) {
      ic <- x$inputs[[nm]]
      if (is.null(ic)) next
      nd <- sum(ic$summary$status != "identical")
      cat("\ninputs ", x$base, " vs ", nm, ": ", nd,
          " differing parameter(s); print(x$inputs[[\"", nm,
          "\"]]) for detail\n", sep = "")
    }
  }
  invisible(x)
}

#' @exportS3Method ggplot2::autoplot
#' @param object a `scenarios_cmp` object.
#' @param type chart type: `"objective"` (bars per scenario), one of the
#'   [getMix()] types (`"generation"`, `"capacity"`, `"new_capacity"`,
#'   `"fuel"`), `"emissions"`, `"cost"` (role-driven cost breakdown per
#'   scenario), or `"delta"` (largest value differences).
#' @param comm,region,year filters forwarded to [getMix()].
#' @param position `"facet"` (stacked mixes, one panel per scenario) or
#'   `"dodge"` (scenarios side by side, aggregated over process).
#' @param variable for `type = "delta"`: restrict to one variable.
#' @rdname compare_scenarios
autoplot.scenarios_cmp <- function(object,
    type = c("objective", "generation", "capacity", "new_capacity", "fuel",
             "emissions", "cost", "delta"),
    comm = "ELC", region = NULL, year = NULL,
    position = c("facet", "dodge"), variable = NULL, ...) {
  check_package("ggplot2")
  type <- match.arg(type)
  position <- match.arg(position)

  if (type == "objective") {
    ov <- object$overview
    return(ggplot2::ggplot(ov,
             ggplot2::aes(.data$scenario, .data$objective)) +
      ggplot2::geom_col(fill = "#1f497d") +
      ggplot2::labs(x = NULL, y = "objective",
                    title = "Total discounted cost") +
      theme_energyRt())
  }

  if (type %in% c("generation", "capacity", "new_capacity", "fuel")) {
    mx <- getMix(object$scen, type = type, comm = comm, region = region,
                 year = year)
    if (is.null(mx) || nrow(mx) == 0) stop("getMix() returned no data")
    if (position == "dodge") {
      agg <- stats::aggregate(value ~ scenario + year, mx, sum)
      return(ggplot2::ggplot(agg,
               ggplot2::aes(factor(.data$year), .data$value,
                            fill = .data$scenario)) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::labs(x = NULL, y = type, fill = NULL) +
        theme_energyRt())
    }
    p <- ggplot2::ggplot(mx,
           ggplot2::aes(factor(.data$year), .data$value,
                        fill = .data$process)) +
      ggplot2::geom_col() +
      ggplot2::labs(x = NULL, y = type, fill = NULL) +
      theme_energyRt()
    n_reg <- length(unique(mx$region))
    p <- if (n_reg > 1) {
      p + ggplot2::facet_grid(region ~ scenario)
    } else {
      p + ggplot2::facet_wrap(~scenario)
    }
    return(p)
  }

  if (type == "emissions") {
    d <- tryCatch(as.data.frame(
      getData(object$scen, name = "vEmsFuelTot", merge = TRUE,
              drop.zeros = FALSE)), error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0) stop("no vEmsFuelTot in the solutions")
    agg <- stats::aggregate(value ~ scenario + year, d, sum)
    return(ggplot2::ggplot(agg,
             ggplot2::aes(factor(.data$year), .data$value,
                          fill = .data$scenario)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::labs(x = NULL, y = "emissions", fill = NULL) +
      theme_energyRt())
  }

  if (type == "cost") {
    parts <- lapply(names(object$scen), function(nm) {
      s <- object$scen[[nm]]
      vs <- tryCatch(s@modOut@variables, error = function(e) list())
      cv <- names(vs)[vapply(vs, function(v)
        identical(tryCatch(v@role, error = function(e) NA_character_),
                  "cost"), logical(1))]
      cv <- setdiff(cv, c("vObjective", "vTotalCost"))
      rows <- lapply(cv, function(v) {
        d <- tryCatch(as.data.frame(
          getData(s, name = v, merge = TRUE, drop.zeros = FALSE)),
          error = function(e) NULL)
        if (is.null(d) || nrow(d) == 0) return(NULL)
        data.frame(scenario = nm, variable = v,
                   value = sum(d$value, na.rm = TRUE),
                   stringsAsFactors = FALSE)
      })
      do.call(rbind, Filter(Negate(is.null), rows))
    })
    cd <- do.call(rbind, Filter(Negate(is.null), parts))
    if (is.null(cd) || nrow(cd) == 0) stop("no cost-role variables solved")
    return(ggplot2::ggplot(cd,
             ggplot2::aes(.data$scenario, .data$value,
                          fill = .data$variable)) +
      ggplot2::geom_col() +
      ggplot2::labs(x = NULL, y = "cost", fill = NULL) +
      theme_energyRt())
  }

  # type == "delta"
  tp <- object$top
  if (!is.null(variable)) tp <- tp[tp$variable %in% variable, , drop = FALSE]
  if (is.null(tp) || nrow(tp) == 0) stop("no differences to plot")
  tp$label <- paste0(tp$scenario, ": ", tp$where)
  ggplot2::ggplot(tp,
    ggplot2::aes(.data$diff, stats::reorder(.data$label, abs(.data$diff)))) +
    ggplot2::geom_col(ggplot2::aes(fill = .data$diff > 0),
                      show.legend = FALSE) +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::labs(x = "difference vs base", y = NULL) +
    theme_energyRt()
}

# =========================================================================== #
# comparative report ####
# =========================================================================== #

setOldClass("scenarios_cmp")

#' @rdname report
#' @export
setMethod("report", "list",
  function(object, template = NULL, image_file = NULL, file = NULL,
           format = c("html", "pdf", "tex", "docx"), levcost = NULL,
           cost_unit = NULL, open = interactive(), reports_path = NULL,
           force = FALSE, ...) {
    if (missing(format)) format <- "html"
    ok <- vapply(object, function(x) methods::is(x, "scenario"), logical(1))
    if (length(object) < 2 || !all(ok)) {
      stop("report() on a list expects a named list of two or more solved ",
           "scenarios; got classes: ",
           paste(vapply(object, function(x) class(x)[1], ""),
                 collapse = ", "))
    }
    # split ... into compare_scenarios() knobs vs render/template args
    dots <- list(...)
    cmp_args <- c("runs", "base", "level", "tol_rel", "tol_abs", "inputs",
                  "top_n", "variables")
    ca <- dots[intersect(names(dots), cmp_args)]
    names(ca)[names(ca) == "variables"] <- "name"
    render_dots <- dots[setdiff(names(dots), cmp_args)]
    cmp <- do.call(compare_scenarios, c(list(scen = object), ca))
    do.call(report, c(list(cmp, template = template,
      image_file = image_file, file = file, format = format, open = open,
      reports_path = reports_path, force = force), render_dots))
  })

#' @rdname report
#' @export
setMethod("report", "scenarios_cmp",
  function(object, template = NULL, image_file = NULL, file = NULL,
           format = c("html", "pdf", "tex", "docx"), levcost = NULL,
           cost_unit = NULL, open = interactive(), reports_path = NULL,
           force = FALSE, ...) {
    if (missing(format)) format <- "html"
    dots <- list(...)
    .report_scenarios(object, template, image_file, file, format, dots,
                      open = open, reports_path = reports_path,
                      force = force)
  })

.report_scenarios <- function(cmp, template, image_file, file, format, dots,
                              open = interactive(), reports_path = NULL,
                              force = FALSE) {
  nms <- names(cmp$scen)
  stub <- substr(paste0("cmp_", paste(nms, collapse = "_")), 1, 60)

  # Everything below is deferred: built only when a render happens.
  params_fn <- function() {
    gg_ok <- requireNamespace("ggplot2", quietly = TRUE)
    ap <- function(...) if (gg_ok)
      tryCatch(autoplot.scenarios_cmp(cmp, ...), error = function(e) NULL)
      else NULL

    # emissions comparison table
    emis_df <- tryCatch({
      d <- as.data.frame(getData(cmp$scen, name = "vEmsFuelTot",
                                 merge = TRUE, drop.zeros = FALSE))
      if (nrow(d) == 0) NULL else {
        by <- intersect(c("scenario", "comm", "year"), names(d))
        ag <- stats::aggregate(d["value"], by = d[by], FUN = sum)
        ag$value <- signif(ag$value, 5)
        ag
      }
    }, error = function(e) NULL)

    # role-driven cost breakdown per scenario (variable x year, row-bound)
    cost_df <- tryCatch({
      parts <- lapply(nms, function(nm) {
        s <- cmp$scen[[nm]]
        vs <- tryCatch(s@modOut@variables, error = function(e) list())
        cv <- names(vs)[vapply(vs, function(v)
          identical(tryCatch(v@role, error = function(e) NA_character_),
                    "cost"), logical(1))]
        cv <- setdiff(cv, c("vObjective", "vTotalCost"))
        rows <- lapply(cv, function(v) {
          d <- tryCatch(as.data.frame(
            getData(s, name = v, merge = TRUE, drop.zeros = FALSE)),
            error = function(e) NULL)
          if (is.null(d) || nrow(d) == 0) return(NULL)
          ag <- if ("year" %in% names(d))
            stats::aggregate(value ~ year, d, sum) else
            data.frame(year = NA_integer_,
                       value = sum(d$value, na.rm = TRUE))
          if (all(ag$value == 0)) return(NULL)
          cbind(data.frame(scenario = nm, variable = v,
                           stringsAsFactors = FALSE), ag)
        })
        do.call(rbind, Filter(Negate(is.null), rows))
      })
      out <- do.call(rbind, Filter(Negate(is.null), parts))
      if (!is.null(out)) out$value <- signif(out$value, 5)
      out
    }, error = function(e) NULL)

    # solution checks per scenario
    verify_df <- tryCatch({
      parts <- lapply(nms, function(nm) {
        vr <- tryCatch(verify_solution(cmp$scen[[nm]]),
                       error = function(e) NULL)
        if (is.null(vr)) return(NULL)
        do.call(rbind, lapply(names(vr$checks), function(k) {
          ck <- vr$checks[[k]]
          data.frame(scenario = nm, check = k,
                     status = as.character(ck$status),
                     violations = if (!is.null(ck$violations))
                       nrow(ck$violations) else 0L,
                     stringsAsFactors = FALSE)
        }))
      })
      do.call(rbind, Filter(Negate(is.null), parts))
    }, error = function(e) NULL)

    # input-diff text blocks
    inputs_txt <- if (!is.null(cmp$inputs)) tryCatch({
      unlist(lapply(names(cmp$inputs), function(nm) {
        ic <- cmp$inputs[[nm]]
        if (is.null(ic)) return(NULL)
        utils::capture.output(print(ic, max_diffs = 3))
      }))
    }, error = function(e) NULL) else NULL

    # only differing rows reach the report's delta tables
    delta_tbls <- lapply(cmp$deltas, function(d) {
      d <- d[d$differs, setdiff(names(d), c("differs", "side")),
             drop = FALSE]
      num <- vapply(d, is.numeric, logical(1))
      d[num] <- lapply(d[num], signif, 5)
      utils::head(d, 50L)
    })
    delta_tbls <- delta_tbls[vapply(delta_tbls, nrow, 0L) > 0]

    list(
      title = paste0("Scenario comparison: ", paste(nms, collapse = " vs ")),
      cmp_name = paste(nms, collapse = " vs "),
      base_name = cmp$base,
      image_file = image_file,
      overview_df = cmp$overview,
      objective_plot = ap("objective"),
      gen_plot = ap("generation"),
      cap_plot = ap("capacity"),
      newcap_plot = ap("new_capacity"),
      emis_plot = ap("emissions"),
      emis_df = emis_df,
      cost_df = cost_df,
      costs_plot = ap("cost"),
      values_df = cmp$values,
      top_df = cmp$top,
      delta_tbls = delta_tbls,
      verify_df = verify_df,
      inputs_txt = inputs_txt)
  }

  tmpl <- if (is.null(template)) "scenarios" else template
  # owner = NULL: a multi-owner report is not cached -- it always renders
  .report_render(tmpl, params_fn, file, format, dots, stub, open = open,
                 class = "scenarios", owner = NULL,
                 reports_path = reports_path, force = force, engine = NULL)
}
