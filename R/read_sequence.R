# =============================================================================#
# read_sequence.R -- rebuilding a driver's sequence from the variants on disk.
#
# `solve_myopic()` and friends return an object whose `$scenarios` are live
# objects; `getData()`, `sample_summary()`, `myopic_objective()` and
# `guided_gap()` all read that list. Nothing reconstructs it, so once the
# session ends the variants are on disk and the sequence is not: the results
# are there and unreachable through the methods written for them.
#
# The manifests carry what is needed. `variant.yml` names the method (`type:`),
# the sequence it belongs to, which unit of work the variant is, and -- since
# the `params:` block -- how the driver was configured. `run.yml` adds status
# and objective per solve.
#
# What CANNOT come back are the driver's intermediates: the carry ledger, the
# capacity targets, the sampling spec's timeslice membership. They are working
# state, never persisted. A rebuilt object declares them in `$missing` rather
# than leaving a NULL to be discovered by whatever dereferences it first.
# =============================================================================#

#' @include runs.R
NULL

# Every variant of a scenario, with its manifest and its first solved run.
.seq_variants <- function(scen) {
  rd <- .run_dirs(scen)
  rd <- rd[nzchar(rd$variant), , drop = FALSE]
  if (!nrow(rd)) return(NULL)
  rows <- list()
  for (v in unique(rd$variant)) {
    vy <- fp(scen@path, "runs", v, "variant.yml")
    mf <- tryCatch(yaml::read_yaml(vy), error = function(e) NULL)
    if (is.null(mf)) next
    sub <- rd[rd$variant == v, , drop = FALSE]
    # a variant holds one solve in practice; prefer a solved one
    pick <- NA_integer_
    st <- rep(NA_character_, nrow(sub))
    ob <- rep(NA_real_, nrow(sub))
    for (i in seq_len(nrow(sub))) {
      rec <- tryCatch(yaml::read_yaml(fp(sub$dir[i], "run.yml")),
                      error = function(e) NULL)
      st[i] <- as.character(rec$status %||% "unknown")
      ob[i] <- suppressWarnings(as.numeric(rec$objective %||% NA_real_))
      if (is.na(pick) && identical(st[i], "solved")) pick <- i
    }
    if (is.na(pick)) pick <- 1L
    rows[[length(rows) + 1L]] <- list(
      variant = v, solve = sub$solve[pick],
      run = .run_id(v, sub$solve[pick]),
      type = as.character(mf$type %||% "custom"),
      sequence = as.character(mf$sequence %||% ""),
      status = st[pick], objective = ob[pick], mf = mf)
  }
  if (!length(rows)) NULL else rows
}

# Position a copy of the scenario on one variant's run. Errors are the
# caller's to report: a sequence with an unreadable member is worth saying so
# about, not worth silently shortening.
.seq_load <- function(scen, run) {
  suppressMessages(read_solution(scen, run = run, echo = FALSE,
                                 ondisk = FALSE))
}

# The full horizon of a myopic sequence: the union of its steps' windows.
# `solve_myopic()` keeps the horizon it was given; on disk only the per-window
# horizons survive, and `myopic_objective()` needs interval lengths and the
# first milestone, both of which the union reproduces.
.seq_horizon <- function(scens) {
  iv <- NULL
  for (s in scens) {
    h <- tryCatch(s@settings@horizon, error = function(e) NULL)
    if (is.null(h)) next
    d <- tryCatch(as.data.frame(h@intervals), error = function(e) NULL)
    if (is.null(d) || !nrow(d)) next
    iv <- if (is.null(iv)) d else rbind(iv, d)
  }
  if (is.null(iv) || !nrow(iv)) return(NULL)
  iv <- iv[!duplicated(iv$mid), , drop = FALSE]
  iv <- iv[order(iv$mid), , drop = FALSE]
  h <- tryCatch(newHorizon(as.integer(iv$mid),
                           intervals = as.integer(iv$end - iv$start + 1L)),
                error = function(e) NULL)
  h %||% NULL
}

#' Rebuild a solve sequence from the variants on disk
#'
#' @description
#' Reassembles the object a driver returned — [solve_myopic()],
#' [solve_by_sample()], [solve_by_region()] or [solve_guided()] — from a
#' scenario's variants, so `getData()`, [sample_summary()],
#' [myopic_objective()] and [guided_gap()] work on a scenario reloaded from
#' disk rather than only on the live result.
#'
#' @details
#' Each driver stores one variant per unit of work, tagged in
#' `runs/<variant>/variant.yml` with the method (`type:`), the sequence it
#' belongs to and how the driver was configured (`params:`). This groups those
#' variants back into a sequence and reads each one's solution.
#'
#' A scenario can hold more than one sequence. With several present, name the
#' one you want with `sequence`, or narrow by `type`.
#'
#' @section What cannot be rebuilt:
#' A driver's working state is never written to disk: the carry ledger
#' (myopic, guided), the capacity targets (guided), and the timeslice
#' membership of a sampling spec (by_sample). Those fields are `NULL` in the
#' result and named in `$missing`, and `$reconstructed` is `TRUE`. The
#' functions that read `$scenarios` — `getData()` and the summaries — do not
#' use them; `apply_ledger()` and anything else that continues a sequence
#' does, so a rebuilt sequence is for reading, not for resuming.
#'
#' @param scen a scenario object, or the name of a stored scenario.
#' @param sequence character, the sequence name to rebuild (as recorded in
#'   `variant.yml`). Needed only when the scenario holds more than one.
#' @param type character, restrict to one driver: `"myopic_step"`,
#'   `"calendar_sample"`, `"region_sample"` or `"guided_stage"`.
#' @param verbose logical.
#'
#' @return an object of the driver's class (`myopic`, `by_sample`,
#'   `by_region` or `guided`), with `$reconstructed = TRUE`.
#' @seealso [scenario_runs()], [scenario_solutions()]
#' @examples
#' \dontrun{
#' scen <- getScenario("ELC_myopic")
#' seq <- read_sequence(scen)
#' myopic_objective(seq)
#' }
#' @export
read_sequence <- function(scen, sequence = NULL, type = NULL, verbose = TRUE) {
  if (is.character(scen)) scen <- getScenario(scen, verbose = FALSE)
  stopifnot(is(scen, "scenario"))
  vs <- .seq_variants(scen)
  if (is.null(vs)) {
    stop("Scenario '", scen@name, "' has no own-problem variants, so it holds ",
         "no driver sequence. scenario_runs() lists what it does have.",
         call. = FALSE)
  }
  known <- c("myopic_step", "calendar_sample", "region_sample", "guided_stage")
  vs <- Filter(function(v) v$type %in% known, vs)
  if (!is.null(type)) vs <- Filter(function(v) identical(v$type, type), vs)
  if (!is.null(sequence)) {
    vs <- Filter(function(v) identical(v$sequence, as.character(sequence)), vs)
  }
  if (!length(vs)) {
    stop("No driver-produced variants matched",
         if (!is.null(sequence)) paste0(" sequence '", sequence, "'"),
         if (!is.null(type)) paste0(" type '", type, "'"), ".", call. = FALSE)
  }
  # A scenario may hold several sequences; picking one silently would hide
  # the others and the choice would be arbitrary.
  key <- vapply(vs, function(v) paste(v$type, v$sequence, sep = "/"),
                character(1))
  if (length(unique(key)) > 1L) {
    stop("Scenario '", scen@name, "' holds ", length(unique(key)),
         " sequences: ", paste(unique(key), collapse = ", "),
         ".\n  Name one with sequence = or type = .", call. = FALSE)
  }

  tp <- vs[[1]]$type
  sq <- vs[[1]]$sequence
  params <- vs[[1]]$mf$params %||% list()

  if (verbose) {
    message("Rebuilding ", tp, " sequence '", sq, "' from ", length(vs),
            " variant(s) of '", scen@name, "'.")
  }
  scens <- lapply(vs, function(v) .seq_load(scen, v$run))

  out <- switch(
    tp,
    myopic_step = {
      k <- vapply(vs, function(v) as.integer(v$mf$step %||% NA_integer_),
                  integer(1))
      o <- order(k)
      vs <- vs[o]; scens <- scens[o]; k <- k[o]
      steps <- tibble(
        step = k,
        run = vapply(vs, `[[`, "", "run"),
        decided = lapply(vs, function(v) as.integer(v$mf$decided)),
        status = vapply(vs, `[[`, "", "status"),
        objective = vapply(vs, function(v) as.numeric(v$objective), numeric(1)))
      # `$scenarios[[k]]` is indexed by STEP by getData.myopic(), so the list
      # has to be positional, not named by variant.
      sl <- vector("list", max(k, na.rm = TRUE))
      for (i in seq_along(k)) sl[[k[i]]] <- scens[[i]]
      list(name = sq, model = scen@model, steps = steps, scenarios = sl,
           ledger = NULL, carry = params$carry, tolerance = params$tolerance,
           store = params$store %||% "variants",
           horizon = .seq_horizon(scens),
           missing = c("ledger"))
    },
    calendar_sample = {
      nms <- vapply(vs, `[[`, "", "variant")
      names(scens) <- nms
      runs <- tibble(
        sample = nms, run = vapply(vs, `[[`, "", "run"),
        slices = vapply(vs, function(v) as.integer(v$mf$slices %||% NA), integer(1)),
        year_fraction = vapply(vs, function(v)
          as.numeric(v$mf$year_fraction %||% NA_real_), numeric(1)),
        status = vapply(vs, `[[`, "", "status"),
        objective = vapply(vs, function(v) as.numeric(v$objective), numeric(1)))
      list(name = sq, model = scen@model, runs = runs, scenarios = scens,
           samples = NULL, method = params$method %||% vs[[1]]$mf$method,
           calendar = tryCatch(scen@settings@calendar, error = function(e) NULL),
           missing = c("samples"))
    },
    region_sample = {
      nms <- vapply(vs, `[[`, "", "variant")
      names(scens) <- nms
      runs <- tibble(
        region = nms, run = vapply(vs, `[[`, "", "run"),
        regions = lapply(vs, function(v) as.character(v$mf$regions)),
        status = vapply(vs, `[[`, "", "status"),
        objective = vapply(vs, function(v) as.numeric(v$objective), numeric(1)),
        boundary = NA_integer_)
      trade <- as.character(params$trade %||% vs[[1]]$mf$trade %||% "none")
      list(name = sq, model = scen@model, runs = runs, scenarios = scens,
           trade = trade, share = params$share,
           additive = identical(trade, "none"),
           missing = character(0))
    },
    guided_stage = {
      nms <- vapply(vs, `[[`, "", "variant")
      names(scens) <- nms
      st <- tibble(
        stage = vapply(vs, function(v) as.character(v$mf$stage %||% ""),
                       character(1)),
        run = vapply(vs, `[[`, "", "run"),
        years = lapply(vs, function(v) as.integer(v$mf$years)),
        status = vapply(vs, `[[`, "", "status"),
        objective = vapply(vs, function(v) as.numeric(v$objective), numeric(1)))
      fin <- which(st$stage == "final")
      list(name = sq, model = scen@model, stages = st, scenarios = scens,
           targets = NULL, ledger = NULL,
           final = if (length(fin)) scens[[fin[1]]] else NULL,
           horizon = .seq_horizon(scens), slack = params$slack,
           basis = params$basis, mode = params$mode,
           direction = params$direction,
           missing = c("targets", "ledger"))
    })

  out$path <- scen@path
  out$failed <- !any(vapply(vs, function(v) identical(v$status, "solved"),
                            logical(1)))
  out$params <- params
  out$reconstructed <- TRUE
  cls <- c(myopic_step = "myopic", calendar_sample = "by_sample",
           region_sample = "by_region", guided_stage = "guided")[[tp]]
  if (verbose && length(out$missing)) {
    message("  Not on disk, so not rebuilt: ", paste(out$missing, collapse = ", "),
            " (working state). The sequence can be read, not resumed.")
  }
  structure(out, class = cls)
}
