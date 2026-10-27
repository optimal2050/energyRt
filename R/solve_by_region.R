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
.by_region_tag_variant <- function(scen, vlab, sequence, regions, trade,
                                   params = NULL) {
  vy <- fp(scen@path, "runs", vlab, "variant.yml")
  if (!file.exists(vy)) return(invisible(NULL))
  mf <- tryCatch(yaml::read_yaml(vy), error = function(e) NULL)
  if (is.null(mf)) return(invisible(NULL))
  mf$type <- "region_sample"
  mf$sequence <- sequence
  mf$regions <- as.character(regions)
  mf$trade <- trade
  if (length(params)) mf$params <- params
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
#' @param object a model object.
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
#' @param overwrite logical; `FALSE` (default) refuses to run when a
#'   variant of the same label already exists on disk with a different type
#'   or different parameters. `TRUE` skips that guard and writes into it.
#'
#' @return a `by_region` object: `runs` (a tibble of what ran, with
#'   `boundary` counting the boundary objects each region faces -- a k-step
#'   price curve contributes k), `scenarios`, `additive`, and `failed`.
#' @seealso [subset_model_regions()], [boundary_window()], [solve_guided()]
#' @export
#' @examples
#' \dontrun{
#' open_project(tempdir())
#'
#' ek  <- topia$modules$electricity$R3
#' mod <- newModel("TOPIA", data = ek$repo,
#'                 calendar = topia$modules$calendars$s4_h24,
#'                 region = ek$regions,
#'                 horizon = topia$modules$horizons$base,
#'                 discount = 0.05)
#' mod <- setGeoscale(mod, ek$geoscale)
#'
#' # one solve per region, each seeing priced boundary flows to its neighbours
#' res <- solve_by_region(mod, name = "BY_REGION", trade = "auto", price = 60,
#'                        solver = solver_options$glpk)
#' res$summary
#' }
solve_by_region <- function(object, name = NULL, ...,
                            regions = NULL, group = NULL,
                            trade = c("none", "user", "auto"),
                            boundary_prices = NULL,
                            share = 0.1, nsteps = 3, price = NULL,
                            spread = 0.5,
                            solver = NULL,
                            on_error = c("stop", "continue"),
                            overwrite = FALSE,
                            keep_scenarios = TRUE, verbose = TRUE) {
  stopifnot(is(object, "model"))
  trade <- match.arg(trade)
  on_error <- match.arg(on_error)
  if (is.null(name)) name <- paste0(object@name, "_by_region")
  .assert_object_name(name, "name")

  all_r <- as.character(object@config@region)
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
  # The units must PARTITION. A region appearing twice is solved twice and
  # counted twice -- in the stacked getData() output and in the "sum of
  # objectives" that print.by_region presents as the whole-system cost. Refused
  # rather than warned: there is no reading of an overlap under which that sum
  # is the system's.
  dup <- unique(unlist(groups)[duplicated(unlist(groups))])
  if (length(dup)) {
    stop("region(s) appear in more than one unit of work: ",
         paste(dup, collapse = ", "), ".\n",
         "  Regions are solved as disjoint pieces, so a repeat is counted ",
         "twice. Split them so each region appears once.", call. = FALSE)
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
  seq_path <- fp(get_scenarios_path(), .path_slug(name, object@name))

  scens <- list()
  failed <- FALSE
  runs <- tibble(region = character(0), run = character(0),
                 regions = list(), status = character(0),
                 objective = numeric(0), boundary = integer(0))

  # The grouping is part of the configuration, not just of the label: labels
  # are slugs, so two different groupings can normalise to the same one and
  # would otherwise overwrite each other's variants silently.
  vparams <- .variant_params(
    trade = trade, share = share, nsteps = nsteps, price = price,
    spread = spread,
    groups = vapply(groups, paste, "", collapse = "+"))

  # Silent unless the caller registered a handler (see
  # `set_progress_bar()`); a sequence solve is the longest-running
  # operation in the package, so it reports like obj2disk() does.
  .prg <- if (length(groups) > 1L) progressr::progressor(along = groups) else NULL
  if (!is.null(.prg)) {
    .prg_old <- .prg_claim()
    on.exit(.prg_release(.prg_old), add = TRUE)
  }
  for (k in seq_along(groups)) {
    if (!is.null(.prg)) .prg(sprintf("region %s", .path_slug(groups[[k]])))
    rg <- groups[[k]]
    vlab <- .path_slug(rg)
    .variant_guard(seq_path, vlab, "region_sample", vparams, overwrite)
    if (verbose) {
      message("region ", k, "/", length(groups), ": ",
              paste(rg, collapse = ", "))
    }

    bp <- switch(trade,
      none = NULL,
      user = boundary_prices,
      auto = boundary_window(object, regions = rg, share = share,
                             nsteps = nsteps, price = price, spread = spread)
    )
    sol <- tryCatch({
      mod_r <- subset_model_regions(object, region = rg, boundary_prices = bp,
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
      tryCatch(length(.scen_build_model(sol)@data[["boundary_stubs"]]@data),
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

    # Hand the scenario the FULL model plus this unit's region set, rather than
    # the cut model it was interpolated from.
    #
    # A scenario stores the model as given, and what was given here was already
    # cut -- while `@model` lives on the shell, which save_scenario() writes
    # only on the first save. So the folder kept the FIRST unit's subset, and a
    # reloaded variant was served another unit's declarations: region R2's
    # results beside a model declaring R1, whose DEM_R2 rows are empty.
    # `verify_solution()` re-derives identities from `@model@data` and would
    # check R2 against a model with no R2 demand; `levcost()` and `report()`
    # would price and describe R1.
    #
    # `.scen_build_model()` re-cuts from `misc$spatial$regions`, which is
    # region-based and needs no geoscale, so each variant rebuilds its own cut
    # on reload. `misc$model_id` must be left alone: a NULL one is that
    # function's marker for "@model is already built and cut", and clearing it
    # makes the rebuild return the full model uncut.
    sol@model <- object
    sol@misc$spatial <- list(regions = rg, boundary_prices = bp)
    saved <- suppressMessages(suppressWarnings(
      save_scenario(sol, verbose = FALSE)))
    .by_region_tag_variant(saved, vlab, sequence = name, regions = rg,
                           params = vparams,
                           trade = trade)
    if (keep_scenarios) scens[[vlab]] <- saved
    runs <- bind_rows(runs, tibble(
      region = paste(rg, collapse = "+"),
      run = fp(vlab, saved@misc$run %||% "?"),
      regions = list(rg), status = "solved", objective = obj,
      boundary = as.integer(n_stub)))
  }

  .en_log("by_region", name, duration = NA, model = object@name,
          regions = length(groups), trade = trade, failed = failed)

  structure(list(
    name = name, model = object, runs = runs, scenarios = scens,
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
getData.by_region <- function(object, name = NULL, ..., merge = TRUE) {
  x <- object
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
