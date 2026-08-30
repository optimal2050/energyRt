# =============================================================================#
# solve_myopic.R — sequential (myopic) solving driver on the carry primitives
# (R/carry.R) and the layout-3 storage substrate (own-problem variants).
#
# Each step solves a WINDOW of the horizon's milestone intervals, the decided
# outcomes accumulate in a carry_ledger, and every later step re-interpolates
# the PRISTINE model with the full ledger applied — carried capacity as
# exogenous (sunk) stock, budgets debited. By default the whole sequence is
# stored as ONE scenario whose steps are own-problem variants.
# =============================================================================#

#' Split a horizon into sequential solve windows
#'
#' @description
#' Slices a horizon's milestone intervals into consecutive windows for
#' sequential solving. Each window's horizon is built from the ORIGINAL
#' interval rows (start/mid/end preserved exactly); `step` intervals are
#' DECIDED per window and `overlap` further intervals are appended as
#' look-ahead (solved for guidance, decisions discarded).
#'
#' @param horizon a horizon object.
#' @param step integer, milestone intervals decided per window (a scalar, or
#'   a vector of window sizes summing to at most the interval count).
#' @param overlap integer, look-ahead intervals appended to each window.
#'
#' @return a list of window specs: `horizon` (the window's horizon),
#'   `decided` (its decided milestone years), `lookahead` (the look-ahead
#'   milestone years, possibly empty).
#' @seealso [solve_myopic()]
#' @export
horizon_windows <- function(horizon, step = 1L, overlap = 0L) {
  stopifnot(is(horizon, "horizon"))
  iv <- as.data.frame(horizon@intervals)
  n <- nrow(iv)
  if (n == 0) stop("The horizon has no intervals")
  sizes <- if (length(step) == 1L) {
    rep(as.integer(step), ceiling(n / as.integer(step)))
  } else {
    as.integer(step)
  }
  if (sum(sizes) < n) {
    stop("`step` sizes cover ", sum(sizes), " of ", n, " intervals")
  }
  out <- list()
  i0 <- 1L
  k <- 0L
  for (sz in sizes) {
    if (i0 > n) break
    k <- k + 1L
    idx_d <- i0:min(i0 + sz - 1L, n)
    idx_la <- if (overlap > 0L && max(idx_d) < n) {
      (max(idx_d) + 1L):min(max(idx_d) + as.integer(overlap), n)
    } else {
      integer(0)
    }
    rows <- iv[c(idx_d, idx_la), , drop = FALSE]
    h <- newHorizon(intervals = rows,
                    force_BY_interval_to_1_year = FALSE,
                    name = sprintf("w%02d_%d", k, iv$mid[idx_d[1]]))
    out[[k]] <- list(horizon = h,
                     decided = as.integer(iv$mid[idx_d]),
                     lookahead = as.integer(iv$mid[idx_la]))
    i0 <- max(idx_d) + 1L
  }
  out
}

# one loud warning when the model carries user constraints that a windowed
# solve applies within each window only (cumulative budgets, cross-year links)
.myopic_warn_intertemporal <- function(mod) {
  bad <- character(0)
  for (rp in names(mod@data)) {
    repo <- mod@data[[rp]]
    if (!isS4(repo)) next
    for (nm in names(repo@data)) {
      o <- repo@data[[nm]]
      if (!is(o, "constraint")) next
      fe <- tryCatch(o@for.each, error = function(e) NULL)
      if (is.null(fe) || !("year" %in% names(fe))) {
        bad <- c(bad, nm)
      }
    }
  }
  if (length(bad)) {
    warning(
      "Sequential solving applies each user constraint WITHIN its own ",
      "window; these constraints have no per-year indexing and look ",
      "cumulative or cross-year: ", paste(unique(bad), collapse = ", "),
      ". Their horizon-wide meaning is NOT preserved across steps — ",
      "restate them per window (or per year) if that matters.",
      call. = FALSE)
  }
  invisible(bad)
}

# add/refresh driver metadata in a variant.yml written by save_scenario()
.myopic_tag_variant <- function(scen, vlab, sequence, k, decided) {
  vy <- fp(scen@path, "runs", vlab, "variant.yml")
  if (!file.exists(vy)) return(invisible(NULL))
  mf <- tryCatch(yaml::read_yaml(vy), error = function(e) NULL)
  if (is.null(mf)) return(invisible(NULL))
  mf$type <- "myopic_step"
  mf$sequence <- sequence
  mf$step <- k
  mf$decided <- as.integer(decided)
  yaml::write_yaml(mf, vy)
  invisible(vy)
}

#' Solve a model sequentially (myopic foresight)
#'
#' @description
#' Solves the model's horizon window by window instead of all at once.
#' Each window sees only its own milestone years (plus optional look-ahead);
#' capacity built in earlier windows is carried forward as exogenous —
#' sunk, investment-cost-free — `stock`, aged by each process's lifetime and
#' net of its recorded retirements, and cumulative supply reserves are
#' debited by past use (see [solution_ledger()] / [apply_ledger()]).
#'
#' By default (`store = "variants"`) the whole sequence is ONE scenario:
#' every step is an own-problem variant (`runs/s01-2020/…`), so the steps
#' share a folder, appear together in [scenario_runs()], and are switchable
#' with [read_solution()]. `store = "scenarios"` writes one scenario folder
#' per step instead.
#'
#' Because carried capacity pays no EAC, per-step objective values are not
#' comparable to a perfect-foresight objective; the capacity and activity
#' PATHS are the meaningful comparison, and [myopic_objective()] recomputes
#' a globally discounted total cost over the decided years.
#'
#' @param mod a model.
#' @param name character, sequence name (default `"<model>_myopic"`); names
#'   the scenario (or the per-step scenario prefix).
#' @param ... forwarded to [interpolate_model()] for every step (settings /
#'   calendar / config overrides).
#' @param horizon the full horizon to split (default: the model's).
#' @param step,overlap window shape, see [horizon_windows()].
#' @param carry what to carry between steps, see [apply_ledger()].
#' @param solver solver specification (as in [solve_scenario()]).
#' @param tolerance carried values below it are treated as zero.
#' @param store `"variants"` (one scenario, steps as own-problem variants)
#'   or `"scenarios"` (one scenario folder per step).
#' @param keep_scenarios logical, keep each step's solved scenario object in
#'   the result (needed by `getData()` on the result).
#' @param on_error `"stop"` (default) or `"return"` (return the partial
#'   result with the failed step recorded).
#' @param verbose logical.
#'
#' @return an object of class `myopic`: `steps` (one row per step: step,
#'   run id, decided years, status, objective), `scenarios` (the solved
#'   step objects when kept), `ledger` (the accumulated [solution_ledger()]),
#'   plus the call parameters. `getData()` on it stitches per-step results
#'   (each year contributed by the step that decided it).
#'
#' @seealso [horizon_windows()], [solution_ledger()], [apply_ledger()],
#'   [myopic_objective()]
#' @export
solve_myopic <- function(mod, name = NULL, ...,
                         horizon = NULL, step = 1L, overlap = 0L,
                         carry = c("capacity", "budgets"),
                         solver = NULL, tolerance = 1e-6,
                         store = c("variants", "scenarios"),
                         keep_scenarios = TRUE,
                         on_error = c("stop", "return"),
                         verbose = TRUE) {
  .log_t0 <- Sys.time()
  stopifnot(is(mod, "model"))
  store <- match.arg(store)
  on_error <- match.arg(on_error)
  name <- name %||% paste0(mod@name, "_myopic")
  horizon <- horizon %||% tryCatch(mod@config@horizon, error = function(e) NULL)
  if (is.null(horizon) || nrow(horizon@intervals) == 0) {
    stop("No horizon: pass `horizon =` or give the model one.")
  }
  windows <- horizon_windows(horizon, step = step, overlap = overlap)
  .myopic_warn_intertemporal(mod)
  # one folder for the whole sequence: the default path derivation includes
  # the (per-window) horizon name, so pin the path explicitly
  seq_path <- fp(get_scenarios_path(), .path_slug(name, mod@name))

  ledger <- .ledger_new()
  scens <- list()
  steps <- tibble(step = integer(0), run = character(0),
                  decided = list(), status = character(0),
                  objective = numeric(0))
  failed <- FALSE

  for (k in seq_along(windows)) {
    w <- windows[[k]]
    vlab <- sprintf("s%02d-%d", k, min(w$decided))
    scen_name <- if (store == "variants") {
      name
    } else {
      sprintf("%s-%s", name, vlab)
    }
    if (verbose) {
      message("myopic step ", k, "/", length(windows), ": ",
              paste(w$decided, collapse = ", "),
              if (length(w$lookahead)) paste0(" (+ look-ahead ",
                                              paste(w$lookahead,
                                                    collapse = ", "), ")"))
    }
    mod_k <- if (k == 1L) {
      mod
    } else {
      apply_ledger(mod, ledger, w$horizon, carry = carry,
                         tolerance = tolerance, verbose = FALSE)
    }
    sol <- tryCatch({
      scen_k <- if (store == "variants") {
        suppressMessages(interpolate_model(
          mod_k, name = scen_name, ..., w$horizon,
          path = seq_path, overwrite = TRUE))
      } else {
        suppressMessages(interpolate_model(
          mod_k, name = scen_name, ..., w$horizon, overwrite = TRUE))
      }
      solve_scenario(scen_k, solver = solver, echo = FALSE, force = TRUE,
                 variant = if (store == "variants") vlab else NULL)
    }, error = function(e) e)

    ok <- !inherits(sol, "error") && isTRUE(sol@status$optimal)
    obj_k <- if (ok) {
      tryCatch(as.numeric(
        get_variable(sol, "vObjective", data = TRUE)$value[1]),
        error = function(e) NA_real_)
    } else {
      NA_real_
    }
    run_id <- if (store == "variants") {
      fp(vlab, if (ok) sol@misc$run else "?")
    } else {
      scen_name
    }
    steps <- bind_rows(steps, tibble(
      step = k, run = run_id, decided = list(w$decided),
      status = if (ok) "solved" else "failed", objective = obj_k))

    if (!ok) {
      msg <- paste0(
        "Myopic step ", k, " (", paste(w$decided, collapse = ", "),
        ") did not solve", if (inherits(sol, "error"))
          paste0(": ", conditionMessage(sol)) else " to optimal", ".\n",
        "  Hints: a declining capacity bound below the carried stock, or a ",
        "cumulative constraint applied within the window (see the warning ",
        "at start), can make a step infeasible. Inspect the step with ",
        "solve_myopic(..., on_error = \"return\")$scenarios.")
      if (on_error == "stop") stop(msg, call. = FALSE)
      warning(msg, call. = FALSE)
      failed <- TRUE
      if (!inherits(sol, "error") && keep_scenarios) {
        scens[[vlab]] <- sol
      }
      break
    }

    saved <- suppressMessages(suppressWarnings(
      save_scenario(sol, verbose = FALSE)))
    if (store == "variants") {
      .myopic_tag_variant(saved, vlab, sequence = name, k = k,
                          decided = w$decided)
    }
    ledger <- solution_ledger(sol, years = w$decided, ledger = ledger)
    if (keep_scenarios) scens[[vlab]] <- sol
  }

  .en_log("myopic", name,
          status = if (failed) "failed" else "ok",
          duration = difftime(Sys.time(), .log_t0, units = "secs"),
          model = mod@name, steps = nrow(steps),
          step = step, overlap = overlap, store = store)

  structure(list(
    name = name, model = mod, steps = steps, scenarios = scens,
    ledger = ledger, carry = carry, tolerance = tolerance,
    store = store, horizon = horizon,
    path = if (length(scens)) scens[[length(scens)]]@path else NULL,
    failed = failed
  ), class = "myopic")
}

#' @method print myopic
#' @export
print.myopic <- function(x, ...) {
  cat("myopic sequence '", x$name, "': ", nrow(x$steps), " step(s), store = ",
      x$store, "\n", sep = "")
  df <- as.data.frame(x$steps)
  df$decided <- vapply(df$decided, function(y) paste(y, collapse = ","), "")
  print(df, row.names = FALSE)
  if (isTRUE(x$failed)) cat("(sequence stopped at a failed step)\n")
  invisible(x)
}

#' @export
getData.myopic <- function(scen, name = NULL, ..., merge = TRUE) {
  x <- scen
  if (length(x$scenarios) == 0) {
    stop("The myopic result kept no scenario objects ",
         "(keep_scenarios = FALSE); re-run with keep_scenarios = TRUE.")
  }
  if (!is.null(name) && "vObjective" %in% name) {
    message("vObjective is per-step and discounted to each step's own base ",
            "year; it is not merged. Use myopic_objective().")
    name <- setdiff(name, "vObjective")
    if (length(name) == 0) return(invisible(NULL))
  }
  out <- list()
  solved <- x$steps[x$steps$status == "solved", , drop = FALSE]
  for (i in seq_len(nrow(solved))) {
    k <- solved$step[i]
    sc <- x$scenarios[[k]]
    if (is.null(sc)) next
    d <- suppressMessages(getData(sc, name, ..., merge = TRUE))
    if (is.null(d) || nrow(d) == 0) next
    if ("year" %in% names(d)) {
      d <- d[d$year %in% solved$decided[[i]], , drop = FALSE]
    }
    if (nrow(d)) {
      d$step <- k
      out[[length(out) + 1L]] <- d
    }
  }
  if (length(out) == 0) return(NULL)
  bind_rows(out)
}

#' Globally discounted total cost of a myopic sequence
#'
#' @description
#' Per-step objectives are discounted to each step's own base year and
#' therefore not additive. This recomputes one comparable number: the sum of
#' each decided year's (undiscounted) `vTotalCost`, weighted by its period
#' length and discounted to the sequence's first milestone at rate `sdr`.
#' Note the documented under-count relative to perfect foresight: capacity
#' carried between steps enters as investment-cost-free stock, so its
#' annuities after its building step are not in `vTotalCost`.
#'
#' @param x a `myopic` result.
#' @param sdr numeric social discount rate (default 0).
#' @return numeric scalar.
#' @export
myopic_objective <- function(x, sdr = 0) {
  stopifnot(inherits(x, "myopic"))
  tc <- getData(x, "vTotalCost", merge = TRUE)
  if (is.null(tc) || nrow(tc) == 0) return(NA_real_)
  iv <- as.data.frame(x$horizon@intervals)
  plen <- stats::setNames(as.numeric(iv$end - iv$start + 1L),
                          as.character(iv$mid))
  y0 <- min(iv$mid)
  df <- (1 + sdr)^(-(tc$year - y0))
  sum(tc$value * plen[as.character(tc$year)] * df)
}
