# =============================================================================#
# base_year.R — a milestone that is given rather than decided.
#
# Every year in `horizon@intervals$mid` becomes a solver year, and there is no
# "present but fixed" state: a year is either in the LP or absent from it. So a
# base year whose values are known had to be pinned from inside the same LP --
# an investment ban, retirement bounds, availability bounds keyed to
# `min(years)` -- or isolated into a second model built for the purpose.
#
# Marking it instead splits one horizon three ways: the milestones before the
# base year, which are history and enter no LP at all; the base year alone,
# which is solved on its own; and the rest, which is what the projection
# optimises. What the base year decided crosses as `stock` (exogenous, sunk,
# no EAC), and every year that left -- history and base year alike -- still
# anchors interpolation, because the interpolation grid already includes anchor
# years outside the solved set.
#
# Slicing the interval table, rather than flagging a year inside it, is what
# keeps this consistent: `mMilestoneFirst`, `mMilestoneNext`, the invest/stock
# windows and the stock path all derive from `intervals$mid` or `sets$year`, so
# a slice moves them together. A flag would leave `mMilestoneFirst` naming a
# year the LP does not contain, which is where the stock initial condition is
# read.
# =============================================================================#

#' @include class-horizon.R carry.R
NULL

#' The base year a horizon marks, or NULL
#'
#' Read through the object's attributes rather than through `@base_year`:
#' `.hasSlot()` tests the CLASS, which has the slot, while a horizon
#' deserialized from a store written before it does not carry the data, and
#' `@base_year` on one of those errors with "no slot of name". The same guard
#' `.upgrade_model_storages()` uses for `inp2stg`.
#' @noRd
.base_year <- function(horizon) {
  if (is.null(horizon) || !isS4(horizon)) return(NULL)
  if (!("base_year" %in% names(attributes(horizon)))) return(NULL)
  by <- as.integer(attr(horizon, "base_year"))
  by <- by[!is.na(by)]
  if (!length(by)) NULL else by[[1]]
}

# Split a marked horizon into the problems it describes:
#
#   $by       the base year alone, solved on its own
#   $main     the projection, which is what the solver optimises
#   $history  the milestones BEFORE the base year, as an integer vector --
#             optimised nowhere. Deliberately not a horizon: nothing should be
#             able to hand it to a solver.
#
# NULL when the horizon marks no base year, which is how every caller tests
# for the mark.
#
# The one definition of the split, shared by solve_base_year(),
# interpolate_model() and solve_guided()'s base stage. Two traps it exists to
# hold in one place:
#
#   * each half is a ROW SLICE of the original interval table, so every
#     milestone it keeps has exactly the start/mid/end it had -- period
#     lengths, and therefore cost weights, are untouched. And
#     `force_BY_interval_to_1_year` is off, or each half would synthesise a
#     fresh one-year first interval and reweight the very years being split.
#
#   * both halves keep the ORIGINAL `@period`, reassigned after construction
#     because newHorizon() trims it to the slice. The bounds path interpolates
#     across `range(approxim$year)`, which IS `horizon@period`, so a trimmed
#     period would clamp every bound's extrapolation to the slice's first
#     milestone -- including bounds anchored in the dropped history.
#
# Dropping a declared milestone is never silent: with history, one message
# names the years that left. Their data is not lost -- a year outside the
# solved set still drives interpolation, whose grid is the milestones extended
# by the anchor years present in the data.
#' @noRd
.by_split <- function(horizon) {
  by <- .base_year(horizon)
  if (is.null(by)) return(NULL)
  iv <- as.data.frame(horizon@intervals)
  mids <- as.integer(iv$mid)
  if (!(by %in% mids)) {
    stop("The horizon marks base year ", by, ", which is not among its ",
         "milestones (", paste(mids, collapse = ", "), ").", call. = FALSE)
  }
  if (!any(mids > by)) {
    stop("The horizon marks its last milestone (", by, ") as the base year, ",
         "leaving nothing to project.", call. = FALSE)
  }
  cut <- function(rows, suffix) {
    h <- newHorizon(intervals = rows, force_BY_interval_to_1_year = FALSE,
                    name = paste0(.horizon_name(horizon), suffix),
                    desc = horizon@desc)
    # each half's interpolation range is the WHOLE horizon's, not the slice's
    # -- see Details
    h@period <- horizon@period
    h
  }
  history <- sort(mids[mids < by])
  # One line, always: a declared milestone leaving the model must be said out
  # loud, and so must the fact that its data is still read.
  if (length(history)) {
    .say("base year ", by, ": ", length(history),
            " milestone(s) before it (", paste(history, collapse = ", "),
            ") are historical and enter no LP; their data still anchors ",
            "interpolation")
  }
  list(by = cut(iv[mids == by, , drop = FALSE], "_by"),
       main = cut(iv[mids > by, , drop = FALSE], "_main"),
       history = history)
}

# A horizon's name, or one derived from its milestones when it has none: the
# halves need distinct names because a scenario's folder is named after its
# horizon, so two unnamed halves would collide.
.horizon_name <- function(horizon) {
  nm <- as.character(horizon@name)
  nm <- nm[nzchar(nm)]
  if (length(nm)) return(nm[1])
  mids <- as.integer(horizon@intervals$mid)
  if (!length(mids)) return("h")
  sprintf("Y%d_%d", min(mids), max(mids))
}

# --- the base-year solve ----------------------------------------------------#

# Observed history has known retirements, not chosen ones. Left on,
# `optimizeRetirement` lets a base year retire its own observed fleet to save
# fixed cost, and the ledger then consumes those retirements FIFO against the
# OLDEST carried vintages (.carried_at()) -- arbitrary for a past year, and it
# mis-dates everything carried forward. Off by default; `TRUE` is for a base
# period that genuinely contains a decided retirement.
# The same split solve_model() makes (R/solve.R): arguments belonging to the
# solver run rather than to interpolation.
.by_solve_args <- function() {
  c("solver.dir", "transient", "tmp.dir", "tmp.del", "force", "read.solution",
    "wait", "verbose", "echo", "run", "run.conflict", "n.threads", "invisible",
    "show.output.on.console", "open.folder")
}

.by_freeze_retirement <- function(mod) {
  objs <- tryCatch(.model_objects(mod), error = function(e) list())
  n <- 0L
  for (nm in names(mod@data)) {
    repo <- mod@data[[nm]]
    if (!isS4(repo) || !methods::.hasSlot(repo, "data")) next
    for (i in seq_along(repo@data)) {
      o <- repo@data[[i]]
      if (!methods::.hasSlot(o, "optimizeRetirement")) next
      if (!isTRUE(o@optimizeRetirement)) next
      o@optimizeRetirement <- FALSE
      repo@data[[i]] <- o
      n <- n + 1L
    }
    mod@data[[nm]] <- repo
  }
  attr(mod, "n_frozen") <- n
  mod
}

#' Solve the base year of a marked horizon
#'
#' @description
#' Solves the base year on its own, as an own-problem variant, and returns the
#' ledger of what it decided. This is the solve the projection does not
#' contain: with `base_year` marked, the projection's LP has no base-year
#' variables at all.
#'
#' @details
#' Endogenous retirement is switched off for this solve unless
#' `optimize_retirement = TRUE`. A past year's retirements are observed, not
#' chosen, and a base year left free to retire its own fleet mis-dates every
#' vintage carried forward.
#'
#' What crosses to the projection is **only what this solve decided** — the
#' ledger's `newcap`. The model's own `@stock`, which is where observed history
#' lives and which already carries its vintages, is left alone.
#'
#' The LP is the base year alone. Milestones before it are history and are
#' solved here no more than they are in the projection; their data still
#' anchors interpolation.
#'
#' @param object a model, or an interpolated scenario, whose horizon marks a
#'   `base_year`.
#' @param name scenario name; defaults to the object's own.
#' @param solver solver options, as [solve_scenario()] takes.
#' @param optimize_retirement allow the base year to retire stock endogenously.
#' @param variant the variant label for the base-year run.
#' @param save store the solved scenario, so a projection can refer to it by
#'   name later. `TRUE` by default: a base-year solution nothing can reopen
#'   cannot be carried into a scenario built in another session.
#' @param ... passed to [interpolate_model()] and [solve_scenario()].
#' @param verbose logical.
#'
#' @return A list with `scenario`, `ledger`, `horizon` and `base_year`.
#' @seealso [solution_ledger()], [newHorizon()], [horizon_windows()]
#' @export
#' @examples
#' \dontrun{
#' h <- newHorizon(intervals = iv, base_year = 2025, name = "h25")
#' mod <- setHorizon(mod, h)
#' by <- solve_base_year(mod, solver = solver_options$glpk)
#' by$ledger
#' }
solve_base_year <- function(object, name = NULL, solver = NULL,
                            optimize_retirement = FALSE, variant = "by",
                            save = TRUE, ...,
                            verbose = isVerbose()) {
  mod <- if (is(object, "scenario")) object@model else object
  if (!is(mod, "model")) {
    stop("`solve_base_year()` expects a model or an interpolated scenario; ",
         "got ", class(object)[1], ".", call. = FALSE)
  }
  horizon <- tryCatch(getHorizon(object), error = function(e) NULL)
  hh <- .by_split(horizon)
  if (is.null(hh)) {
    stop("This horizon marks no base year. Declare one with ",
         "newHorizon(..., base_year = ) before solving it separately.",
         call. = FALSE)
  }
  by <- .base_year(horizon)
  if (!isTRUE(optimize_retirement)) {
    mod <- .by_freeze_retirement(mod)
    nfz <- attr(mod, "n_frozen") %||% 0L
    if (isTRUE(verbose) && nfz > 0L) {
      .say("Base year: endogenous retirement switched off for ", nfz,
              " process(es) (observed retirements are data, not decisions).")
    }
  }
  nm <- name %||% paste0(mod@name, "_by")
  # `...` carries both interpolation and solver arguments; route them the way
  # solve_model() does, or interpolate_model() warns about every solver knob
  dots <- list(...)
  is_solve <- names(dots) %in% .by_solve_args()
  scen <- suppressMessages(do.call(interpolate_model, c(
    list(mod, name = nm, hh$by), dots[!is_solve])))
  sol <- do.call(solve_scenario, c(
    list(scen, solver = solver, variant = variant), dots[is_solve]))
  if (isTRUE(save) && isTRUE(sol@status$optimal)) {
    sol <- suppressMessages(save_scenario(sol, verbose = FALSE))
  }
  list(scenario = sol, horizon = hh$by, base_year = by,
       ledger = if (isTRUE(sol@status$optimal)) {
         solution_ledger(sol, years = by)
       } else {
         NULL
       })
}

# --- the carry: only what the base year DECIDED -----------------------------#

# Add the base year's decided capacity to the projection's stock, leaving the
# model's own declared `@stock` as it is.
#
# This is deliberately NOT apply_ledger(carry = "capacity"). That clears the
# stock column and rebuilds it as `user_path + carried_path`, where
# `user_path` rolls a single anchor level forward through survival ratios
# re-derived from the schedule's shape (.user_stock_roll()). For a model whose
# `@stock` already states a vintage-correct level at every milestone -- which
# is how observed history is expressed, and what both the vintaged `@stock`
# tables in the field do -- that round-trips an exact table through ratio
# arithmetic and loses wherever the two disagree, silently.
#
# So only the keys the base year actually built are touched, and for those the
# declared schedule is evaluated (not rolled) and the decided capacity added.
# A key the base year did not build is left bit-identical.
.by_carry_newcap <- function(mod, ledger, horizon, tolerance = 1e-6,
                             verbose = TRUE) {
  if (is.null(ledger) || is.null(ledger$newcap) || !nrow(ledger$newcap)) {
    if (isTRUE(verbose)) {
      .say("Base year: nothing was built, so the projection's stock is ",
              "unchanged.")
    }
    return(mod)
  }
  mids <- as.integer(horizon@intervals$mid)
  fams <- .carry_families()
  keys <- unique(ledger$newcap[, c("family", "proc", "region")])
  touched <- 0L
  for (i in seq_len(nrow(keys))) {
    fk <- as.character(keys$family[i])
    fam <- fams[[fk]]
    if (is.null(fam)) next
    proc <- as.character(keys$proc[i])
    region <- keys$region[i]
    carried <- vapply(mids, function(y) {
      .carried_at(ledger, fk, proc, region, y)
    }, numeric(1))
    if (all(carried < tolerance)) next
    hit <- .model_find_object(mod, proc, fam$class)
    if (is.null(hit)) next
    o <- hit$obj
    cap <- o@capacity
    hold_end <- fam$class %in% c("storage", "trade")
    # the declared schedule for this key, evaluated at the projection's
    # milestones -- the same rule interpolation would apply
    sched <- NULL
    if (is.data.frame(cap) && nrow(cap) && fam$stock_col %in% names(cap)) {
      rows <- rep(TRUE, nrow(cap))
      if ("region" %in% names(cap) && !is.na(region)) {
        rows <- rows & (is.na(cap$region) | cap$region %in% region)
      }
      keep <- rows & !is.na(cap[[fam$stock_col]])
      if (any(keep)) {
        sched <- data.frame(year = as.integer(cap$year[keep]),
                            value = as.numeric(cap[[fam$stock_col]][keep]))
      }
    }
    declared <- .user_stock_eval(sched, mids, hold_end)
    total <- as.numeric(declared) + carried
    total[total < tolerance] <- 0
    o@capacity <- .by_write_stock(cap, fam$stock_col, region, mids, total)
    mod <- .model_replace_object(mod, hit$repo, o)
    touched <- touched + 1L
  }
  if (isTRUE(verbose)) {
    .say("Base year carried into the projection: ", touched,
            " process(es) gained decided capacity as stock; every other ",
            "declared stock row is untouched.")
  }
  mod
}

# Replace this key's stock column with dense rows at `mids`. Only rows of the
# same key are cleared; another region's schedule is left alone.
.by_write_stock <- function(cap, col, region, mids, values) {
  if (!is.data.frame(cap)) cap <- data.frame()
  if (nrow(cap) && col %in% names(cap)) {
    rows <- rep(TRUE, nrow(cap))
    if ("region" %in% names(cap) && !is.na(region)) {
      rows <- rows & (is.na(cap$region) | cap$region %in% region)
    }
    cap[[col]][rows] <- NA_real_
    other <- setdiff(names(cap), c("year", "region"))
    empty <- rows & !apply(cap[, other, drop = FALSE], 1,
                           function(r) any(!is.na(r)))
    if (any(empty)) cap <- cap[!empty, , drop = FALSE]
  }
  add <- data.frame(year = as.integer(mids))
  if (!is.na(region)) add$region <- as.character(region)
  add[[col]] <- as.numeric(values)
  bind_rows(cap, add)
}

# --- resolving a base-year source ------------------------------------------#
#
# A base-year solution can be handed over in four shapes, and all four end in
# the same place: the ledger of what that solve DECIDED.
#
#   * the list `solve_base_year()` returns (it carries `$ledger`)
#   * a carry ledger on its own
#   * a solved `scenario` -- its own solution when it is positioned on one that
#     covers the base year, otherwise its `by` variant run
#   * "<scenario>" or "<scenario>/<run>" by name, loaded from the store. A
#     variant run id carries its own slash ("by/glpk"), and a scenario name
#     cannot, so only the FIRST slash separates the two.
#
# Returns list(ledger, scenario, run, hash), or NULL for "no source given".
# `hash` is over the ledger's `newcap`, so re-solving the base year to a
# different answer makes a recorded link detectably stale.
#' @noRd
.by_source <- function(src, year) {
  if (is.null(src) || isFALSE(src)) return(NULL)
  # a recorded link that names no scenario (the source was a bare ledger)
  if (is.character(src) &&
      (length(src) != 1L || is.na(src) || !nzchar(src))) return(NULL)
  mk <- function(ledger, scen_name = NA_character_, run = NA_character_) {
    if (is.null(ledger)) return(NULL)
    nc <- ledger$newcap
    list(ledger = ledger,
         scenario = as.character(scen_name %||% NA_character_)[1],
         run = as.character(run %||% NA_character_)[1],
         hash = rlang::hash(if (is.null(nc)) list() else as.data.frame(nc)))
  }
  if (inherits(src, "carry_ledger")) return(mk(src))
  if (is.list(src) && !isS4(src)) {
    if (!inherits(src$ledger, "carry_ledger")) {
      stop("`base_year_scenario` is a list carrying no `$ledger`. Pass the ",
           "result of solve_base_year(), a solved scenario, or a scenario ",
           "name.", call. = FALSE)
    }
    # the run id is the VARIANT's ("by/glpk"), not the solve label alone, or
    # the recorded link names a run that read_solution() cannot find
    return(mk(src$ledger,
              tryCatch(src$scenario@name, error = function(e) NA_character_),
              tryCatch(.run_id(.run_variant(src$scenario),
                               src$scenario@misc$run %||% ""),
                       error = function(e) NA_character_)))
  }
  if (is.character(src) && length(src) == 1L) src <- .by_load_source(src)
  if (!is(src, "scenario")) {
    stop("`base_year_scenario` must be a scenario, a scenario name, ",
         "\"<scenario>/<run>\", or the result of solve_base_year(); got ",
         class(src)[1], ".", call. = FALSE)
  }
  # already positioned on a solution that covers the base year
  mids <- as.integer(tryCatch(src@settings@horizon@intervals$mid,
                              error = function(e) integer(0)))
  if (isTRUE(src@status$optimal) && year %in% mids) {
    return(mk(solution_ledger(src, years = year), src@name,
              .run_id(.run_variant(src), src@misc$run %||% "")))
  }
  run <- .by_find_run(src)
  if (is.null(run)) return(NULL)
  s <- suppressMessages(read_solution(src, run = run, verbose = FALSE,
                                      ondisk = FALSE))
  if (!isTRUE(s@status$optimal)) return(NULL)
  mk(solution_ledger(s, years = year), src@name, run)
}

# A scenario's own base-year run: the `by` variant, preferring a run whose
# solution was read back ("solved") over one whose optimality is unrecorded.
#' @noRd
.by_find_run <- function(scen) {
  rr <- tryCatch(scenario_runs(scen), error = function(e) NULL)
  if (is.null(rr) || !nrow(rr)) return(NULL)
  hit <- rr[rr$variant == "by" & rr$status %in% c("solved", "finished"), ,
            drop = FALSE]
  if (!nrow(hit)) return(NULL)
  hit <- hit[order(hit$status != "solved"), , drop = FALSE]
  as.character(hit$run[1])
}

# "<scenario>" or "<scenario>/<run>" -> a scenario positioned on that run.
#' @noRd
.by_load_source <- function(spec) {
  parts <- strsplit(spec, "/", fixed = TRUE)[[1]]
  nm <- parts[1]
  run <- if (length(parts) > 1L) paste(parts[-1], collapse = "/") else NULL
  e <- new.env(parent = emptyenv())
  tryCatch(
    suppressMessages(load_scenario(nm, name = "by_src", env = e,
                                   overwrite = TRUE, verbose = FALSE)),
    error = function(err) {
      stop("`base_year_scenario = \"", spec, "\"`: ", conditionMessage(err),
           call. = FALSE)
    })
  s <- get("by_src", envir = e)
  if (!is.null(run)) {
    s <- suppressMessages(read_solution(s, run = run, verbose = FALSE,
                                        ondisk = FALSE))
  }
  s
}

# --- the recorded link ------------------------------------------------------#

# The base-year link a scenario carries: from the object when it has one,
# otherwise from `scenario.yml` (a scenario loaded thin keeps its
# `@misc`, but one read straight off disk by another route may not).
# NULL when the scenario records no base year.
#' @noRd
.by_link_of <- function(scen) {
  by <- tryCatch(scen@misc$base_year, error = function(e) NULL)
  if (is.null(by)) {
    mfp <- tryCatch(fp(scen@path, "scenario.yml"), error = function(e) "")
    if (nzchar(mfp) && file.exists(mfp)) {
      by <- tryCatch(yaml::read_yaml(mfp)$base_year, error = function(e) NULL)
    }
  }
  if (is.null(by) || is.null(by$year)) return(NULL)
  by$year <- as.integer(by$year)
  by$history <- as.integer(by$history %||% integer(0))
  by$scenario <- as.character(by$scenario %||% "")[1]
  by$run <- as.character(by$run %||% "")[1]
  by$hash <- as.character(by$hash %||% "")[1]
  by
}

# The scenario a link points at, positioned on its base-year run; NULL when
# the link names no solved source, or it can no longer be opened.
#' @noRd
.by_link_open <- function(link) {
  if (is.null(link) || is.na(link$scenario) || !nzchar(link$scenario)) {
    return(NULL)
  }
  spec <- if (nzchar(link$run %||% "")) {
    paste0(link$scenario, "/", link$run)
  } else {
    link$scenario
  }
  s <- tryCatch(.by_load_source(spec), error = function(e) NULL)
  if (is.null(s) || !is(s, "scenario") || !isTRUE(s@status$optimal)) NULL else s
}

# TRUE when the source a link points at no longer holds the solution it
# recorded -- the base year was re-solved to a different answer.
#' @noRd
.by_link_stale <- function(link, scen) {
  if (is.null(link) || !nzchar(link$hash %||% "")) return(FALSE)
  s <- .by_link_open(link)
  if (is.null(s)) return(FALSE)
  now <- tryCatch(.by_source(s, link$year), error = function(e) NULL)
  !is.null(now) && !identical(now$hash, link$hash)
}
