# =============================================================================#
# solve_by_region.R — solve a multi-region model one region (or group) at a
# time.
#
# Each region is an ordinary sub-territory solve: `subset_model_regions()`
# narrows the model, the scenario is solved, and the run is stored as an
# own-problem variant of ONE scenario, so `scenario_runs()` lists them and
# `read_solution(run =)` switches between them.
#
# Three boundary modes:
#
#   "none"  severed routes are dropped. Regional quantities are extensive and
#           nothing crosses, so the per-region objectives sum to the full
#           model's.
#   "user"  the caller supplies `boundary_prices`.
#   "auto"  `boundary_window()` sizes a stepped curve from the region's demand.
#
# Under "user" and "auto" each region faces an exterior the others do not see,
# so the objectives are not additive; the result carries `additive`.
# =============================================================================#

# add/refresh driver metadata in a variant.yml written by save_scenario()
.by_region_tag_variant <- function(scen, vlab, sequence, regions, trade) {
  vy <- fp(scen@path, "runs", vlab, "variant.yml")
  if (!file.exists(vy)) return(invisible(NULL))
  mf <- tryCatch(yaml::read_yaml(vy), error = function(e) NULL)
  if (is.null(mf)) return(invisible(NULL))
  mf$type <- "region_sample"
  mf$sequence <- sequence
  mf$regions <- as.character(regions)
  mf$trade <- trade
  yaml::write_yaml(mf, vy)
  invisible(vy)
}

#' Solve a model region by region
#'
#' @description
#' Solves a multi-region model as a series of sub-territory problems — one per
#' region by default, or one per group. Each run is stored as an own-problem
#' variant of a single scenario, so the whole set lives in one folder and is
#' listed by [scenario_runs()].
#'
#' The intended use is supply-curve evaluation, where each region is priced
#' against the rest of the system rather than co-optimised with it.
#'
#' @details
#' `trade` decides what replaces the routes that leave a region:
#'
#' * `"none"` (default) — routes are dropped and the region is solved in
#'   autarky. Because regional quantities are extensive and nothing crosses,
#'   the per-region objectives **add up** to the full model's.
#' * `"user"` — `boundary_prices` is passed through to
#'   [subset_model_regions()].
#' * `"auto"` — [boundary_window()] builds a stepped import/export curve sized
#'   at `share` of the region's own demand in each timeslice. A reference
#'   `price` is required: the window can size the quantity from the model but
#'   cannot invent a price.
#'
#' With `trade != "none"` each region faces an exterior the others cannot see,
#' so the objectives no longer sum to anything meaningful — the result's
#' `additive` field records this.
#'
#' @param mod a model object.
#' @param name character, the sequence name (one scenario folder).
#' @param ... passed to [interpolate_model()] for every region.
#' @param regions character, the regions to solve; defaults to every region of
#'   the model, one at a time.
#' @param group a list of character vectors to solve groups of regions
#'   together instead of one at a time; overrides `regions`.
#' @param trade one of `"none"`, `"user"`, `"auto"` — see Details.
#' @param boundary_prices a `boundary_prices` data.frame for `trade = "user"`.
#' @param share,nsteps,price,spread passed to [boundary_window()] for
#'   `trade = "auto"`.
#' @param solver a solver option list; the scenario's default when `NULL`.
#' @param on_error `"stop"` (default) or `"continue"` — with `"continue"` a
#'   region that fails to solve is recorded and the run carries on, so one bad
#'   region does not lose the rest.
#' @param keep_scenarios logical, keep each region's scenario object.
#' @param verbose logical.
#'
#' @return a `by_region` object: `runs` (a tibble of what ran, with
#'   `boundary` counting the boundary objects each region faces -- a k-step
#'   price curve contributes k), `scenarios`, `additive`, and `failed`.
#' @seealso [subset_model_regions()], [boundary_window()], [solve_guided()]
#' @export
solve_by_region <- function(mod, name = NULL, ...,
                            regions = NULL, group = NULL,
                            trade = c("none", "user", "auto"),
                            boundary_prices = NULL,
                            share = 0.1, nsteps = 3, price = NULL,
                            spread = 0.5,
                            solver = NULL,
                            on_error = c("stop", "continue"),
                            keep_scenarios = TRUE, verbose = TRUE) {
  stopifnot(is(mod, "model"))
  trade <- match.arg(trade)
  on_error <- match.arg(on_error)
  if (is.null(name)) name <- paste0(mod@name, "_by_region")
  .assert_object_name(name, "name")

  all_r <- as.character(mod@config@region)
  if (!length(all_r)) {
    stop("The model declares no regions.", call. = FALSE)
  }
  groups <- if (!is.null(group)) {
    if (!is.list(group)) group <- as.list(group)
    lapply(group, as.character)
  } else {
    as.list(as.character(regions %||% all_r))
  }
  unknown <- setdiff(unlist(groups), all_r)
  if (length(unknown)) {
    stop("region(s) not declared in the model: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  if (trade == "user" && is.null(boundary_prices)) {
    stop("trade = \"user\" needs `boundary_prices`; use trade = \"auto\" to ",
         "generate a generic window, or trade = \"none\" for autarky.",
         call. = FALSE)
  }
  if (trade == "none" && !is.null(boundary_prices)) {
    warning("`boundary_prices` given with trade = \"none\"; ignored.",
            call. = FALSE)
  }
  extra <- list(...)
  seq_path <- fp(get_scenarios_path(), .path_slug(name, mod@name))

  scens <- list()
  failed <- FALSE
  runs <- tibble(region = character(0), run = character(0),
                 regions = list(), status = character(0),
                 objective = numeric(0), boundary = integer(0))

  for (k in seq_along(groups)) {
    rg <- groups[[k]]
    vlab <- .path_slug(rg)
    if (verbose) {
      message("region ", k, "/", length(groups), ": ",
              paste(rg, collapse = ", "))
    }

    bp <- switch(trade,
      none = NULL,
      user = boundary_prices,
      auto = boundary_window(mod, regions = rg, share = share,
                             nsteps = nsteps, price = price, spread = spread)
    )
    sol <- tryCatch({
      mod_r <- subset_model_regions(mod, region = rg, boundary_prices = bp,
                                    verbose = FALSE)
      args <- c(list(mod_r, name = name), extra,
                list(path = seq_path, overwrite = TRUE))
      scen_r <- suppressMessages(do.call(interpolate_model, args))
      solve_scenario(scen_r, solver = solver, echo = FALSE, force = TRUE,
                     variant = vlab)
    }, error = function(e) e)

    ok <- !inherits(sol, "error") && isTRUE(sol@status$optimal)
    obj <- if (ok) {
      tryCatch(as.numeric(
        get_variable(sol, "vObjective", data = TRUE)$value[1]),
        error = function(e) NA_real_)
    } else {
      NA_real_
    }
    # counted after variant expansion: a k-step price curve is k objects
    n_stub <- if (ok) {
      tryCatch(length(sol@model@data[["boundary_stubs"]]@data),
               error = function(e) 0L)
    } else {
      0L
    }

    if (!ok) {
      msg <- paste0(
        "Region ", paste(rg, collapse = ", "), " did not solve",
        if (inherits(sol, "error")) paste0(": ", conditionMessage(sol)) else
          " to optimal", ".\n",
        "  Hints: a region solved in autarky must be able to meet its own ",
        "demand. Give it a trade window (trade = \"auto\", or ",
        "`boundary_prices`), or check its supply.")
      if (on_error == "stop") stop(msg, call. = FALSE)
      warning(msg, call. = FALSE)
      failed <- TRUE
      runs <- bind_rows(runs, tibble(
        region = paste(rg, collapse = "+"), run = NA_character_,
        regions = list(rg), status = "failed", objective = NA_real_,
        boundary = 0L))
      next
    }

    saved <- suppressMessages(suppressWarnings(
      save_scenario(sol, verbose = FALSE)))
    .by_region_tag_variant(saved, vlab, sequence = name, regions = rg,
                           trade = trade)
    if (keep_scenarios) scens[[vlab]] <- saved
    runs <- bind_rows(runs, tibble(
      region = paste(rg, collapse = "+"),
      run = fp(vlab, saved@misc$run %||% "?"),
      regions = list(rg), status = "solved", objective = obj,
      boundary = as.integer(n_stub)))
  }

  .en_log("by_region", name, duration = NA, model = mod@name,
          regions = length(groups), trade = trade, failed = failed)

  structure(list(
    name = name, model = mod, runs = runs, scenarios = scens,
    trade = trade, share = share, additive = identical(trade, "none"),
    path = if (length(scens)) scens[[length(scens)]]@path else NULL,
    failed = failed
  ), class = "by_region")
}

#' @method print by_region
#' @export
print.by_region <- function(x, ...) {
  cat("by_region '", x$name, "': ", nrow(x$runs), " run(s), trade = ",
      x$trade, "\n", sep = "")
  df <- as.data.frame(x$runs)
  if (nrow(df)) {
    df$regions <- vapply(df$regions, function(r) paste(r, collapse = ","), "")
    print(df, row.names = FALSE)
  }
  if (isTRUE(x$additive)) {
    ok <- x$runs$status == "solved"
    if (any(ok)) {
      cat("sum of objectives: ", format(sum(x$runs$objective[ok])),
          " (regions are disjoint and untraded, so this is the ",
          "whole-system cost)\n", sep = "")
    }
  } else {
    cat("objectives are NOT additive: each region was solved against a trade ",
        "window the others do not see.\n", sep = "")
  }
  if (isTRUE(x$failed)) cat("(at least one region failed)\n")
  invisible(x)
}

#' @export
getData.by_region <- function(scen, name = NULL, ..., merge = TRUE) {
  x <- scen
  if (length(x$scenarios) == 0) {
    stop("This by_region result kept no scenario objects ",
         "(keep_scenarios = FALSE); re-run with keep_scenarios = TRUE.",
         call. = FALSE)
  }
  out <- list()
  for (k in names(x$scenarios)) {
    d <- suppressMessages(getData(x$scenarios[[k]], name, ..., merge = TRUE))
    if (is.null(d) || nrow(d) == 0) next
    if (!"region" %in% names(d)) d$region <- k
    out[[length(out) + 1L]] <- d
  }
  if (!length(out)) return(NULL)
  bind_rows(out)
}
