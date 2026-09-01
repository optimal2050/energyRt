# =============================================================================#
# solve_guided.R — guided perfect-foresight driver.
#
# A full-horizon perfect-foresight solve is often intractable. This driver
# reaches one by solving CHEAP problems first and turning their answers into
# capacity TARGETS for the expensive one:
#
#   base      {BY}            optional; the optimised base-year fleet is written
#                             back as `stock` so the past is not re-optimised
#   endpoint  {BY, LY}        the end-state fleet, most flexible: whatever
#                             survives from BY, everything else built at LY
#   transition all milestones optional; a reduced calendar carries the endpoint
#                             target across the whole path cheaply
#   pairs     {y_k, LY}       each interior milestone refined against the
#                             endpoint — the step that distinguishes this from
#                             myopic solving, because every subproblem still
#                             sees where it has to arrive
#   final     full            the deliverable: one full-horizon solve with the
#                             accumulated targets as bounds
#
# Two properties follow from the last stage being a single ordinary solve.
# Its `vObjective` is directly comparable to a true perfect-foresight
# objective — unlike solve_myopic(), whose carried stock pays no EAC — and the
# comparison has a known sign: guided >= PF, the gap being the price of the
# guidance. `guided_gap()` reports it.
#
# Targets enter as `cap.lo` bounds via apply_targets() (R/carry.R); the decided
# past enters as `stock` via apply_ledger(). The distinction matters: stock is
# sunk and pays no EAC, a target still builds and still pays.
# =============================================================================#

# Two-milestone horizon from rows `k` and `n` of an interval table.
#
# The focus row is WIDENED to cover the gap up to the endpoint, so the two
# periods together span the calendar rather than leaving the intervening years
# unweighted (`pPeriodLen` is row-local: slicing rows silently drops the gap).
# The base-year row is never widened — its one-year length is what makes the
# whole exogenous fleet commission there, and stretching it would reweight the
# base year's costs.
.guided_pair_horizon <- function(iv, k, n, name = NULL) {
  rows <- iv[c(k, n), , drop = FALSE]
  if (k > 1L && rows$end[1] < rows$start[2] - 1L) {
    rows$end[1] <- rows$start[2] - 1L
  }
  newHorizon(intervals = rows, force_BY_interval_to_1_year = FALSE,
             name = name %||% sprintf("g_%d_%d", rows$mid[1], rows$mid[2]))
}

#' Two-milestone windows for a guided solve
#'
#' @description
#' Builds the `{y_k, last}` windows the `pairs` stage of [solve_guided()]
#' solves: each interior milestone paired with the horizon's last milestone, so
#' every subproblem optimises one milestone's expansion while still seeing the
#' end state. The guided counterpart of [horizon_windows()], which slices a
#' horizon into CONSECUTIVE windows instead.
#'
#' The focus interval is widened to cover the years up to the endpoint, so the
#' window's period lengths span the calendar (`pPeriodLen` is row-local, so a
#' bare row slice would leave the intervening years unweighted).
#'
#' @param horizon a horizon object with at least two milestone intervals.
#' @param direction `"backward"` (default) walks the last interior milestone
#'   down to the first, so each step's future is already decided;
#'   `"forward"` walks up from the base year.
#' @param include_base logical, include the base-year milestone as a focus
#'   window. `FALSE` (default) leaves the base year to the `base` stage, which
#'   pins it as `stock`.
#'
#' @return a list of window specs: `horizon`, `focus` (the milestone being
#'   decided) and `endpoint`.
#' @seealso [solve_guided()], [horizon_windows()]
#' @export
guided_windows <- function(horizon, direction = c("backward", "forward"),
                           include_base = FALSE) {
  stopifnot(is(horizon, "horizon"))
  direction <- match.arg(direction)
  iv <- as.data.frame(horizon@intervals)
  n <- nrow(iv)
  if (n < 2L) {
    stop("A guided solve needs at least two milestone years; this horizon ",
         "has ", n, ".", call. = FALSE)
  }
  ks <- seq_len(n - 1L)                       # every milestone but the last
  if (!isTRUE(include_base)) ks <- setdiff(ks, 1L)
  if (identical(direction, "backward")) ks <- rev(ks)
  lapply(ks, function(k) {
    list(horizon = .guided_pair_horizon(iv, k, n),
         focus = as.integer(iv$mid[k]),
         endpoint = as.integer(iv$mid[n]))
  })
}

# add/refresh driver metadata in a variant.yml written by save_scenario()
.guided_tag_variant <- function(scen, vlab, sequence, stage, years) {
  vy <- fp(scen@path, "runs", vlab, "variant.yml")
  if (!file.exists(vy)) return(invisible(NULL))
  mf <- tryCatch(yaml::read_yaml(vy), error = function(e) NULL)
  if (is.null(mf)) return(invisible(NULL))
  mf$type <- "guided_stage"
  mf$sequence <- sequence
  mf$stage <- stage
  mf$years <- as.integer(years)
  yaml::write_yaml(mf, vy)
  invisible(vy)
}

# Solve one guided stage, relaxing the guidance if it makes the problem
# infeasible. A target harvested from a reduced calendar can demand capacity
# the full problem cannot deliver (a cap the guide never saw), so the ladder
# is: the requested slack, then progressively looser, then no targets at all.
.guided_solve_stage <- function(mod, targets, h, name, vlab, seq_path, slack,
                                basis, mode, solver, calendar, on_infeasible,
                                extra, verbose) {
  ladder <- if (identical(on_infeasible, "relax")) {
    unique(pmin(1, c(slack, slack * 2, slack * 4, 1)))
  } else {
    slack
  }
  # Narrow to this window's milestones first. Walking the pairs backward, the
  # accumulated targets legitimately hold years this window does not solve;
  # apply_targets() would drop them with a warning meant for a caller who
  # mistyped a year, not for the driver doing this by construction.
  if (!is.null(targets) && nrow(targets)) {
    mids <- as.integer(h@intervals$mid)
    prov <- attr(targets, "provenance")
    targets <- targets[targets$year %in% mids, , drop = FALSE]
    attr(targets, "provenance") <- prov
  }
  last_err <- NULL
  for (sl in ladder) {
    mod_k <- if (is.null(targets) || nrow(targets) == 0 || sl >= 1) {
      mod
    } else {
      apply_targets(mod, targets, h, slack = sl, basis = basis, mode = mode,
                    verbose = FALSE)
    }
    args <- c(list(mod_k, name = name), extra,
              list(h, path = seq_path, overwrite = TRUE))
    if (!is.null(calendar)) args <- c(args, list(calendar))
    sol <- tryCatch({
      scen_k <- suppressMessages(do.call(interpolate_model, args))
      solve_scenario(scen_k, solver = solver, echo = FALSE, force = TRUE,
                     variant = vlab)
    }, error = function(e) e)
    ok <- !inherits(sol, "error") && isTRUE(sol@status$optimal)
    if (ok) {
      return(list(scenario = sol, slack = sl, ok = TRUE, error = NULL))
    }
    last_err <- sol
    if (verbose && length(ladder) > 1) {
      message("  stage '", name, "' did not solve at slack = ", sl,
              if (sl < 1) "; relaxing the guidance" else "")
    }
  }
  list(scenario = if (inherits(last_err, "error")) NULL else last_err,
       slack = NA_real_, ok = FALSE, error = last_err)
}

#' Solve a model with guided perfect foresight
#'
#' @description
#' Reaches a full-horizon solution by solving cheap endpoint problems first and
#' using their capacity as targets for the expensive full solve. Unlike
#' [solve_myopic()], every subproblem sees the horizon's last milestone, so the
#' transition is disciplined by where it must arrive rather than by local
#' optimality — and the final stage is one ordinary full-horizon solve, whose
#' objective is therefore directly comparable to a true perfect-foresight one.
#'
#' @details
#' Stages, each stored as an own-problem variant of one scenario:
#'
#' * `"base"` — solve the base year alone and write the result back as `stock`,
#'   so the past enters later stages as sunk capacity rather than being
#'   re-optimised.
#' * `"endpoint"` — solve `{base, last}`. Whatever survives from the base year
#'   stands; everything else is built at the last milestone. This is the
#'   end-state fleet. The base-year row is kept so discounting stays anchored
#'   there.
#' * `"transition"` — optionally carry the endpoint target across every
#'   milestone on a reduced `calendar_guide`, cheaply.
#' * `"pairs"` — refine each interior milestone against the endpoint, via
#'   [guided_windows()].
#' * `"final"` — the deliverable: the full horizon with every accumulated
#'   target imposed as a bound.
#'
#' Targets bound the STANDING FLEET (`cap.lo`) by default. `basis = "ncap"`
#' bounds the build rate instead and divides by the period length, because
#' `ncap.*` bounds are multiplied by `pPeriodLen` in the equations while the
#' solved variable is already an annual rate.
#'
#' @param mod a model object.
#' @param name character, the sequence name (a scenario folder).
#' @param ... passed to [interpolate_model()] for every stage.
#' @param horizon a horizon; defaults to the model's own. At least two
#'   milestone years are required.
#' @param stages character, which stages to run, in order. Defaults to
#'   `c("endpoint", "pairs", "final")`; add `"base"` and/or `"transition"`.
#' @param slack numeric in `[0, 1]`, how far below a target the solver may go
#'   (the bound is `(1 - slack) * target`). `0.2` by default.
#' @param basis `"cap"` (default) or `"ncap"` — see Details.
#' @param mode `"lo"` (default) writes lower bounds; `"fx"` fixes capacity.
#' @param calendar_guide an optional reduced [newCalendar()] used for the guide
#'   stages (`base`, `endpoint`, `transition`, `pairs`) but never for `final`.
#' @param direction `"backward"` (default) or `"forward"`, for the `pairs`
#'   stage.
#' @param solver a solver option list; the scenario's default when `NULL`.
#' @param on_infeasible `"relax"` (default) retries a stage with progressively
#'   looser guidance before giving up; `"stop"` fails immediately.
#' @param keep_scenarios logical, keep each stage's scenario object in the
#'   result.
#' @param verbose logical.
#'
#' @return a `guided` object: `stages` (a tibble of what ran), `scenarios`,
#'   `targets`, `ledger`, and `final` — the full-horizon solved scenario.
#' @seealso [solve_myopic()], [apply_targets()], [guided_gap()]
#' @export
solve_guided <- function(mod, name = NULL, ...,
                         horizon = NULL,
                         stages = c("endpoint", "pairs", "final"),
                         slack = 0.2,
                         basis = c("cap", "ncap"),
                         mode = c("lo", "fx"),
                         calendar_guide = NULL,
                         direction = c("backward", "forward"),
                         solver = NULL,
                         on_infeasible = c("relax", "stop"),
                         keep_scenarios = TRUE,
                         verbose = TRUE) {
  stopifnot(is(mod, "model"))
  basis <- match.arg(basis)
  mode <- match.arg(mode)
  direction <- match.arg(direction)
  on_infeasible <- match.arg(on_infeasible)
  stages <- match.arg(stages, c("base", "endpoint", "transition", "pairs",
                                "final"), several.ok = TRUE)
  if (is.null(name)) name <- paste0(mod@name, "_guided")
  .assert_object_name(name, "name")

  h_full <- horizon %||% mod@config@horizon
  if (is.null(h_full) || nrow(h_full@intervals) < 2L) {
    stop("A guided solve needs a horizon with at least two milestone years.",
         call. = FALSE)
  }
  iv <- as.data.frame(h_full@intervals)
  n <- nrow(iv)
  by_year <- as.integer(iv$mid[1])
  ly_year <- as.integer(iv$mid[n])
  extra <- list(...)

  .myopic_warn_intertemporal(mod)

  seq_path <- fp(get_scenarios_path(), .path_slug(name, mod@name))
  ledger <- .ledger_new()
  targets <- NULL
  scens <- list()
  final <- NULL
  failed <- FALSE
  st <- tibble(stage = character(0), run = character(0), years = list(),
               slack = numeric(0), status = character(0),
               objective = numeric(0))

  record <- function(st, stage, vlab, years, res) {
    obj <- if (isTRUE(res$ok)) {
      tryCatch(as.numeric(
        get_variable(res$scenario, "vObjective", data = TRUE)$value[1]),
        error = function(e) NA_real_)
    } else {
      NA_real_
    }
    bind_rows(st, tibble(
      stage = stage,
      run = if (isTRUE(res$ok)) fp(vlab, res$scenario@misc$run %||% "?") else
        NA_character_,
      years = list(as.integer(years)), slack = res$slack,
      status = if (isTRUE(res$ok)) "solved" else "failed", objective = obj))
  }

  run_stage <- function(stage, h, vlab, years, use_targets, cal) {
    if (verbose) {
      message("guided stage '", stage, "': ",
              paste(years, collapse = ", "),
              if (!is.null(cal)) " (reduced calendar)" else "")
    }
    mod_k <- apply_ledger(mod, ledger, h, verbose = FALSE)
    res <- .guided_solve_stage(
      mod_k, if (use_targets) targets else NULL, h, name, vlab, seq_path,
      slack, basis, mode, solver, cal, on_infeasible, extra, verbose)
    if (!res$ok) {
      msg <- paste0(
        "Guided stage '", stage, "' (", paste(years, collapse = ", "),
        ") did not solve",
        if (inherits(res$error, "error"))
          paste0(": ", conditionMessage(res$error)) else " to optimal", ".\n",
        "  Hints: a capacity target can exceed what another constraint ",
        "permits. Try a larger `slack`, or on_infeasible = \"relax\".")
      if (identical(on_infeasible, "stop")) stop(msg, call. = FALSE)
      warning(msg, call. = FALSE)
      return(res)
    }
    sol <- suppressMessages(suppressWarnings(
      save_scenario(res$scenario, verbose = FALSE)))
    .guided_tag_variant(sol, vlab, sequence = name, stage = stage,
                        years = years)
    res$scenario <- sol
    if (keep_scenarios) scens[[vlab]] <<- sol
    res
  }

  # --- base: pin the past as stock ---------------------------------------- #
  if ("base" %in% stages) {
    h_by <- newHorizon(intervals = iv[1, , drop = FALSE],
                       force_BY_interval_to_1_year = FALSE,
                       name = sprintf("g_by_%d", by_year))
    res <- run_stage("base", h_by, "s0_base", by_year, FALSE, calendar_guide)
    st <- record(st, "base", "s0_base", by_year, res)
    if (!res$ok) failed <- TRUE else {
      ledger <- solution_ledger(res$scenario, years = by_year, ledger = ledger)
    }
  }

  # --- endpoint: the end-state fleet --------------------------------------- #
  if (!failed && "endpoint" %in% stages) {
    h_ep <- .guided_pair_horizon(iv, 1L, n, name = sprintf("g_ep_%d", ly_year))
    res <- run_stage("endpoint", h_ep, "s1_endpoint", c(by_year, ly_year),
                     FALSE, calendar_guide)
    st <- record(st, "endpoint", "s1_endpoint", c(by_year, ly_year), res)
    if (!res$ok) failed <- TRUE else {
      targets <- solution_targets(res$scenario, years = ly_year)
    }
  }

  # --- transition: carry the endpoint across the path, cheaply ------------- #
  if (!failed && "transition" %in% stages) {
    res <- run_stage("transition", h_full, "s2_transition",
                     as.integer(iv$mid), TRUE, calendar_guide)
    st <- record(st, "transition", "s2_transition", as.integer(iv$mid), res)
    if (!res$ok) failed <- TRUE else {
      targets <- solution_targets(res$scenario)
    }
  }

  # --- pairs: refine each interior milestone against the endpoint ---------- #
  if (!failed && "pairs" %in% stages) {
    wins <- guided_windows(h_full, direction = direction)
    for (w in wins) {
      vlab <- sprintf("s3_pair_%d", w$focus)
      res <- run_stage(paste0("pair:", w$focus), w$horizon, vlab,
                       c(w$focus, w$endpoint), TRUE, calendar_guide)
      st <- record(st, paste0("pair:", w$focus), vlab,
                   c(w$focus, w$endpoint), res)
      if (!res$ok) {
        failed <- TRUE
        break
      }
      # keep this milestone's own answer; the endpoint row stays as harvested
      tg_k <- solution_targets(res$scenario, years = w$focus)
      targets <- if (is.null(targets)) tg_k else {
        prov <- attr(targets, "provenance") %||% attr(tg_k, "provenance")
        keep <- targets[targets$year != w$focus, , drop = FALSE]
        out <- bind_rows(keep, tg_k)
        attr(out, "provenance") <- prov
        structure(out, class = c("capacity_targets", class(as_tibble(out))))
      }
    }
  }

  # --- final: the deliverable ---------------------------------------------- #
  if (!failed && "final" %in% stages) {
    res <- run_stage("final", h_full, "s4_final", as.integer(iv$mid), TRUE,
                     NULL)
    st <- record(st, "final", "s4_final", as.integer(iv$mid), res)
    if (!res$ok) failed <- TRUE else final <- res$scenario
  }

  .en_log("guided", name, duration = NA,
          model = mod@name, stages = paste(stages, collapse = "+"),
          slack = slack, failed = failed)

  structure(list(
    name = name, model = mod, stages = st, scenarios = scens,
    targets = targets, ledger = ledger, final = final,
    horizon = h_full, slack = slack, basis = basis, mode = mode,
    direction = direction,
    path = if (length(scens)) scens[[length(scens)]]@path else NULL,
    failed = failed
  ), class = "guided")
}

#' @method print guided
#' @export
print.guided <- function(x, ...) {
  cat("guided sequence '", x$name, "': ", nrow(x$stages), " stage(s), slack = ",
      x$slack, ", basis = ", x$basis, "\n", sep = "")
  df <- as.data.frame(x$stages)
  if (nrow(df)) {
    df$years <- vapply(df$years, function(y) paste(y, collapse = ","), "")
    print(df, row.names = FALSE)
  }
  if (is.null(x$final)) {
    cat("(no final full-horizon solve",
        if (isTRUE(x$failed)) " — the sequence stopped at a failed stage" else
          " — 'final' was not in `stages`", ")\n", sep = "")
  }
  invisible(x)
}

#' @export
getData.guided <- function(scen, name = NULL, ..., merge = TRUE) {
  x <- scen
  if (is.null(x$final)) {
    stop("This guided result has no final full-horizon solve; ",
         "re-run with \"final\" in `stages`, or call getData() on one of ",
         "`$scenarios`.", call. = FALSE)
  }
  getData(x$final, name, ..., merge = merge)
}

#' Cost of the guidance
#'
#' @description
#' The relative gap between a guided solve's objective and a true
#' perfect-foresight one. Because the guided result's final stage is a single
#' full-horizon solve with extra bounds, the two objectives are directly
#' comparable and the gap is non-negative by construction: bounds can only
#' raise the optimum. A gap of zero means the guidance never bound.
#'
#' @param x a `guided` result with a `final` scenario.
#' @param pf a solved perfect-foresight scenario of the same model, or its
#'   objective as a number.
#'
#' @return a list with `guided`, `pf` and `gap` (the relative difference).
#' @seealso [solve_guided()]
#' @export
guided_gap <- function(x, pf) {
  stopifnot(inherits(x, "guided"))
  if (is.null(x$final)) {
    stop("This guided result has no final full-horizon solve to compare.",
         call. = FALSE)
  }
  g <- as.numeric(getData(x$final, "vObjective", merge = TRUE)$value[1])
  p <- if (is(pf, "scenario")) {
    as.numeric(getData(pf, "vObjective", merge = TRUE)$value[1])
  } else {
    as.numeric(pf)[1]
  }
  list(guided = g, pf = p, gap = (g - p) / p)
}
