# =============================================================================#
# solve_by_sample.R — solve a model on samples of its calendar.
#
# Each sample is an ordinary sampled-calendar solve (the recipe
# `interpolate_model()` already accepts), stored as an own-problem variant of
# one scenario. Three sampling methods: consecutive blocks tiling the year,
# disjoint random draws, and bootstrap draws with replacement.
#
# Samples are REPLICATES, not shards. Sub-annual slices carry
# `pTimesliceWeight = 1/year_fraction` (the top slice is full-year magnitude,
# weight 1), so every sample estimates the whole year and the set is
# combined into a distribution. Summing across samples is wrong -- the opposite
# of solve_by_region(), where disjoint samples add up.
#
# Two properties of the calendar constructor shape the sampler.
#
# Levels are never dropped. `.complete_calendar()` re-derives leaf members from
# the surviving level columns, so removing a collapsed level renames the leaves
# (`WIN_h00` -> `h00`) and a model keyed on the original labels matches nothing
# and solves to zero without warning. A sample must retain at least two members
# at every non-ANNUAL level.
#
# Samples keep the full calendar's timeslice names. A selection need not be a
# complete cross: the constructor builds an irregular calendar from the rows
# chosen, and `.sample_calendar()` names its timeslices after the full
# calendar, so a label that becomes unique in the sample is not shortened.
# `calendar_samples()` still draws whole members of `level`.
# =============================================================================#

# The calendar's level columns, coarsest first, excluding ANNUAL.
.cal_levels <- function(cal) {
  lv <- names(cal@timeframes)
  setdiff(lv, "ANNUAL")
}

# The leaf timeslices in calendar (chronological) row order.
.cal_leaves <- function(cal) {
  as.character(cal@timeframes[[length(cal@timeframes)]])
}

#' Build a calendar-sample specification
#'
#' @description
#' Splits a calendar into samples for [solve_by_sample()] by drawing whole
#' members of one level — seasons, weeks, days — and keeping every finer
#' timeslice beneath them. The result is a plain table you can inspect, edit
#' and pass back.
#'
#' @details
#' `method` chooses the traversal:
#'
#' * `"sequential"` — consecutive blocks of `sample_size` members in calendar
#'   order, tiling the year. Deterministic and full-coverage; the week-by-week
#'   or month-by-month crawl.
#' * `"random"` — `n` disjoint draws of `sample_size` members (the year sampled
#'   without replacement); coverage may be partial.
#' * `"bootstrap"` — `n` independent draws of `sample_size` members **with**
#'   replacement. A member drawn twice cannot be a repeated row (a calendar
#'   holds each timeslice once), so repeats are carried as a `count` and become
#'   a scaled share when the sample's calendar is built.
#'
#' Samples are drawn as whole members of `level`. A hand-built selection need
#' not be a complete cross: its calendar is irregular and keeps the full
#' calendar's timeslice names. Each sample must keep at least two members at
#' every non-ANNUAL level, so `sample_size` of 1 is refused for the coarsest
#' level.
#'
#' @param calendar a calendar object.
#' @param level character, the level whose members are drawn; the coarsest
#'   non-ANNUAL level by default (e.g. `"SEASON"` for `s4_h24`).
#' @param sample_size integer, members of `level` per sample.
#' @param n integer, number of samples.
#' @param method one of `"sequential"`, `"random"`, `"bootstrap"`.
#' @param seed optional integer seed for the random methods.
#'
#' @return a `calendar_samples` data.frame with columns `sample`, `timeslice`
#'   and `count`.
#' @seealso [solve_by_sample()]
#' @export
#' @examples
#' cal <- topia$modules$calendars$s4_h24
#'
#' # two sequential blocks of timeslices, one row per (sample, timeslice)
#' sam <- calendar_samples(cal, n = 2, seed = 1)
#' sam
#' head(as.data.frame(sam), 3)
#'
#' # two random samples of two seasons each, drawn without replacement
#' calendar_samples(cal, level = "SEASON", sample_size = 2, n = 2,
#'                  method = "random", seed = 42)
calendar_samples <- function(calendar, level = NULL, sample_size = NULL,
                             n = NULL,
                             method = c("sequential", "random", "bootstrap"),
                             seed = NULL) {
  stopifnot(is(calendar, "calendar"))
  method <- match.arg(method)
  lv <- .cal_levels(calendar)
  if (!length(lv)) {
    stop("The calendar has no level below ANNUAL; nothing to sample.",
         call. = FALSE)
  }
  level <- level %||% lv[1]
  if (!level %in% lv) {
    stop("`level` must be one of: ", paste(lv, collapse = ", "), ".",
         call. = FALSE)
  }
  tt <- as.data.frame(calendar@timetable)
  members <- unique(as.character(tt[[level]]))   # calendar (row) order
  nm <- length(members)
  if (nm < 2L) {
    stop("Level '", level, "' has ", nm, " member(s); nothing to sample.",
         call. = FALSE)
  }
  if (is.null(sample_size) && is.null(n)) {
    stop("Give `sample_size` (members of '", level, "' per sample) or `n` ",
         "(number of samples).", call. = FALSE)
  }
  if (is.null(sample_size)) sample_size <- max(2L, floor(nm / as.integer(n)))
  sample_size <- as.integer(sample_size)
  if (sample_size < 2L) {
    stop("`sample_size` must be at least 2: a sample keeping one member of '",
         level, "' collapses that level, and the calendar constructor ",
         "refuses it.", call. = FALSE)
  }
  if (sample_size > nm) {
    stop("`sample_size` (", sample_size, ") exceeds the ", nm,
         " member(s) of level '", level, "'.", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)

  draws <- switch(method,
    sequential = {
      grp <- split(seq_len(nm), ceiling(seq_len(nm) / sample_size))
      # a trailing block of one member would collapse the level: fold it back
      if (length(grp) > 1L && length(grp[[length(grp)]]) < 2L) {
        grp[[length(grp) - 1L]] <- c(grp[[length(grp) - 1L]],
                                     grp[[length(grp)]])
        grp[[length(grp)]] <- NULL
      }
      lapply(grp, function(i) members[i])
    },
    random = {
      k <- if (is.null(n)) floor(nm / sample_size) else as.integer(n)
      if (k * sample_size > nm) {
        stop("`random` draws are disjoint: ", k, " x ", sample_size,
             " exceeds the ", nm, " member(s) of '", level,
             "'. Reduce `n` or `sample_size`, or use ",
             "method = \"bootstrap\".", call. = FALSE)
      }
      pool <- sample(members)
      lapply(seq_len(k), function(i)
        pool[((i - 1L) * sample_size + 1L):(i * sample_size)])
    },
    bootstrap = {
      k <- if (is.null(n)) 1L else as.integer(n)
      lapply(seq_len(k), function(i) {
        repeat {
          d <- sample(members, sample_size, replace = TRUE)
          # a draw that lands on one distinct member collapses the level
          if (length(unique(d)) >= 2L) return(d)
        }
      })
    }
  )

  out <- list()
  for (i in seq_along(draws)) {
    tb <- table(draws[[i]])
    rows <- tt[as.character(tt[[level]]) %in% names(tb), , drop = FALSE]
    out[[i]] <- data.frame(
      sample = sprintf("s%02d", i),
      timeslice = as.character(rows$timeslice),
      count = as.integer(tb[as.character(rows[[level]])]),
      stringsAsFactors = FALSE)
  }
  res <- do.call(rbind, out)
  structure(res, class = c("calendar_samples", class(res)),
            method = method, level = level, calendar = calendar@name)
}

# Normalise the three accepted `samples` forms to (sample, timeslice, count).
.as_samples <- function(samples, calendar) {
  if (is(samples, "calendar")) {
    tt <- as.data.frame(samples@timetable)
    if (!"sample" %in% names(tt)) {
      stop("A calendar passed as `samples` must carry a `sample` column in ",
           "its @timetable flagging each timeslice's sample.", call. = FALSE)
    }
    samples <- data.frame(sample = as.character(tt$sample),
                          timeslice = as.character(tt$timeslice),
                          stringsAsFactors = FALSE)
    samples <- samples[!is.na(samples$sample) & nzchar(samples$sample), ,
                       drop = FALSE]
  }
  stopifnot(is.data.frame(samples))
  if (!all(c("sample", "timeslice") %in% names(samples))) {
    stop("`samples` must have `sample` and `timeslice` columns.",
         call. = FALSE)
  }
  if (!"count" %in% names(samples)) samples$count <- 1L
  samples$sample <- as.character(samples$sample)
  samples$timeslice <- as.character(samples$timeslice)
  samples$count <- as.integer(samples$count)
  unknown <- setdiff(samples$timeslice, .cal_leaves(calendar))
  if (length(unknown)) {
    stop("`samples` names ", length(unknown), " timeslice(s) that are not ",
         "leaf slices of the calendar: ",
         paste(utils::head(unknown, 4), collapse = ", "),
         if (length(unknown) > 4) ", ..." else "", call. = FALSE)
  }
  samples
}

# One sample's calendar. Never drops a level: a level left with a single member
# would be re-derived away by .complete_calendar(), renaming the leaves and
# detaching the model's data from them.
.sample_calendar <- function(cal, slices, counts = NULL, name = "sample") {
  tt <- data.table::as.data.table(cal@timetable)
  keep <- as.character(tt$timeslice) %in% slices
  if (!any(keep)) {
    stop("The sample selects no timeslice of the calendar.", call. = FALSE)
  }
  sub <- tt[keep, ]
  # a bootstrap repeat is a scaled share, not a repeated row
  if (!is.null(counts)) {
    m <- counts[match(as.character(sub$timeslice), names(counts))]
    m[is.na(m)] <- 1L
    sub$share <- sub$share * as.numeric(m)
  }
  # every non-ANNUAL level must keep >= 2 members
  for (lv in intersect(.cal_levels(cal), names(sub))) {
    nmem <- length(unique(as.character(sub[[lv]])))
    if (nmem < 2L) {
      stop("A calendar sample must keep at least two members at every level; ",
           "level '", lv, "' has ", nmem, " in this sample.\n",
           "  The smallest legal window is two members of the coarsest level ",
           "(e.g. two seasons of s4_h24, two weeks of w52_h24). Dropping the ",
           "level is not an option: it renames the leaf timeslices and the ",
           "model's data would silently stop matching.", call. = FALSE)
    }
  }
  sub <- sub[, setdiff(names(sub), "weight"), with = FALSE]
  x <- newCalendar(name = name, timetable = sub,
                   year_fraction = sum(sub$share))
  .complete_calendar(x, year_fraction = x@year_fraction, ref = cal@timetable)
}

# add/refresh driver metadata in a variant.yml written by save_scenario()
.by_sample_tag_variant <- function(scen, vlab, sequence, method, slices, yf,
                                   params = NULL) {
  vy <- fp(scen@path, "runs", vlab, "variant.yml")
  if (!file.exists(vy)) return(invisible(NULL))
  mf <- tryCatch(yaml::read_yaml(vy), error = function(e) NULL)
  if (is.null(mf)) return(invisible(NULL))
  mf$type <- "calendar_sample"
  mf$sequence <- sequence
  mf$method <- method
  mf$slices <- as.integer(slices)
  mf$year_fraction <- as.numeric(yf)
  if (length(params)) mf$params <- params
  yaml::write_yaml(mf, vy)
  invisible(vy)
}

#' Solve a model on samples of its calendar
#'
#' @description
#' Solves a model repeatedly on samples of its calendar — consecutive blocks
#' tiling the year, disjoint random draws, or bootstrap draws — storing each run
#' as an own-problem variant of one scenario. The set is then combined into
#' distributions rather than a single point answer.
#'
#' @details
#' Each sample is annualised (sub-annual slices carry
#' `pTimesliceWeight = 1/year_fraction`), so every
#' sample estimates the **whole year**: the runs are replicates and their spread
#' is the sampling uncertainty. They are not pieces of a year and must not be
#' summed — unlike [solve_by_region()], whose disjoint samples do add up.
#'
#' A sample must keep at least two members at every non-ANNUAL level of the
#' calendar. A one-season (or one-week, or one-day) window collapses its outer
#' level and is refused, because the only way to build it would rename the leaf
#' timeslices and silently detach the model's data.
#'
#' @param object a model object.
#' @param name character, the sequence name (one scenario folder).
#' @param ... passed to [interpolate_model()] for every sample.
#' @param samples a `calendar_samples` table, a `data.frame(sample, timeslice)`,
#'   or a calendar carrying a `sample` column. When `NULL`, built by
#'   [calendar_samples()] from `sample_size` / `n` / `method`.
#' @param level,sample_size,n,method,seed passed to [calendar_samples()] when
#'   `samples` is `NULL`.
#' @param calendar the calendar to sample; the model's own when `NULL`.
#' @param solver a solver option list; the scenario's default when `NULL`.
#' @param ncap_floor numeric in `[0, 1]`. `0` (default) leaves every sample
#'   free to choose its own capacity, and the ensemble is a distribution over
#'   different investment plans. Above `0` the samples are solved a second
#'   time against a floor of `ncap_floor` times the ensemble's own new
#'   capacity, so the spread becomes period variation at one plan rather than
#'   variation between plans. `0.9` lets a sample trim a tenth; `1` holds the
#'   consensus exactly. It is a floor, not a fix: a sample may still build
#'   ABOVE it, and that is the signal worth reading -- it marks a period the
#'   consensus plan cannot serve.
#' @param ncap_stat `"mean"` (default) or `"median"`, the statistic the
#'   consensus is taken with. Used only when `ncap_floor > 0`.
#' @param on_error `"stop"` (default) or `"continue"` — with `"continue"` a
#'   sample that fails to solve is recorded and the ensemble carries on.
#' @param keep_scenarios logical, keep each sample's scenario object.
#' @param verbose logical.
#' @param overwrite logical; `FALSE` (default) refuses to run when a
#'   variant of the same label already exists on disk with a different type
#'   or different parameters. `TRUE` skips that guard and writes into it.
#'
#' @return a `by_sample` object: `runs` (a tibble of what ran), `scenarios`,
#'   `samples` and `method`.
#' @seealso [calendar_samples()], [sample_summary()], [solve_by_region()]
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
#' # two random two-season samples of the calendar, each solved as a full year
#' res <- solve_by_sample(mod, name = "BY_SAMPLE", level = "SEASON",
#'                        sample_size = 2, n = 2, method = "random", seed = 1,
#'                        solver = solver_options$glpk)
#'
#' # the spread across samples is the estimate's uncertainty, not a partition
#' sample_summary(res, "vTechCap", by = "tech")
#' }
solve_by_sample <- function(object, name = NULL, ...,
                            samples = NULL, level = NULL,
                            sample_size = NULL, n = NULL,
                            method = c("sequential", "random", "bootstrap"),
                            seed = NULL, calendar = NULL,
                            solver = NULL, base = NULL,
                            ncap_floor = 0, ncap_stat = c("mean", "median"),
                            on_error = c("stop", "continue"),
                            overwrite = FALSE,
                            keep_scenarios = TRUE, verbose = TRUE) {
  # A SCENARIO names the full problem the samples are drawn from: its model
  # drives the sequence and the scenario itself becomes the base, so each
  # sample's tables are stored as a row filter over it rather than as a copy.
  if (is(object, "scenario")) {
    base <- base %||% object
    object <- object@model
  }
  stopifnot(is(object, "model"))
  if (!is.null(base) && !is(base, "scenario")) {
    stop("`base` must be a saved scenario of the same model, or NULL.",
         call. = FALSE)
  }
  method <- match.arg(method)
  on_error <- match.arg(on_error)
  ncap_stat <- match.arg(ncap_stat)
  if (!is.numeric(ncap_floor) || length(ncap_floor) != 1L ||
      is.na(ncap_floor) || ncap_floor < 0) {
    stop("`ncap_floor` must be one number in [0, 1].", call. = FALSE)
  }
  if (ncap_floor > 1) {
    # Above 1 is not a relaxation of the consensus but a demand to build more
    # than any sample chose -- a different request, and silently clamping it
    # would answer a question nobody asked.
    stop("`ncap_floor` is a fraction of the ensemble's own new capacity, so ",
         "it cannot exceed 1 (got ", ncap_floor, ").", call. = FALSE)
  }
  if (is.null(name)) name <- paste0(object@name, "_by_sample")
  .assert_object_name(name, "name")

  cal <- calendar %||% tryCatch(object@config@calendar, error = function(e) NULL)
  if (is.null(cal) || !is(cal, "calendar")) {
    stop("The model has no calendar to sample; pass `calendar =`.",
         call. = FALSE)
  }
  spec <- if (is.null(samples)) {
    calendar_samples(cal, level = level, sample_size = sample_size, n = n,
                     method = method, seed = seed)
  } else {
    .as_samples(samples, cal)
  }
  if (!is.null(attr(spec, "method"))) method <- attr(spec, "method")
  ids <- unique(spec$sample)
  if (!length(ids)) stop("`samples` is empty.", call. = FALSE)

  # a live defect worth naming before a long ensemble: storage discharge is
  # infeasible on a year_fraction < 1 calendar when out.af.* rows are declared
  if (.has_out_af(object)) {
    warning("This model declares storage `out.af.*` rows, which are ",
            "infeasible on a sampled (year_fraction < 1) calendar. Samples ",
            "are likely to fail; declare the availability on the charging ",
            "side (`inp.af.*`) instead.", call. = FALSE)
  }

  extra <- list(...)
  seq_path <- fp(get_scenarios_path(), .path_slug(name, object@name))

  vparams <- .variant_params(
    method = method, level = level, sample_size = sample_size, n = n,
    seed = seed, calendar = cal, ncap_floor = ncap_floor,
    ncap_stat = if (ncap_floor > 0) ncap_stat else NULL)

  # One pass over the samples. Run twice when a floor is asked for: the free
  # pass decides, the floored pass re-dispatches every sample against the
  # ensemble's own consensus. `mod` and the variant suffix are all that differ.
  one_pass <- function(mod, sfx = "", pass = "free", base = NULL) {
  scens <- list()
  failed <- FALSE
  runs <- tibble(sample = character(0), run = character(0),
                 slices = integer(0), year_fraction = numeric(0),
                 status = character(0), objective = numeric(0),
                 pass = character(0))
  for (k in seq_along(ids)) {
    sid <- ids[k]
    vlab <- paste0(sid, sfx)
    .variant_guard(seq_path, vlab, "calendar_sample", vparams, overwrite)
    rows <- spec[spec$sample == sid, , drop = FALSE]
    cnt <- stats::setNames(rows$count, rows$timeslice)
    if (verbose) {
      message("sample ", k, "/", length(ids), ": ", vlab, " (",
              nrow(rows), " slice(s)", if (nzchar(sfx)) ", floored" else "", ")")
    }
    sol <- tryCatch({
      cal_k <- .sample_calendar(cal, rows$timeslice, counts = cnt,
                               name = paste0("cs_", sid))
      args <- c(list(mod, name = name), extra,
                list(cal_k, path = seq_path, overwrite = TRUE),
                if (is.null(base)) list() else list(base = base))
      scen_k <- suppressMessages(do.call(interpolate_model, args))
      solve_scenario(scen_k, solver = solver, echo = FALSE, force = TRUE,
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
    yf <- if (ok) {
      tryCatch(sol@settings@calendar@year_fraction, error = function(e) NA_real_)
    } else {
      NA_real_
    }

    if (!ok) {
      msg <- paste0("Calendar sample '", vlab, "' did not solve",
                    if (inherits(sol, "error"))
                      paste0(": ", conditionMessage(sol)) else " to optimal",
                    ".")
      if (on_error == "stop") stop(msg, call. = FALSE)
      warning(msg, call. = FALSE)
      failed <- TRUE
      runs <- bind_rows(runs, tibble(
        sample = sid, run = NA_character_, slices = nrow(rows),
        year_fraction = NA_real_, status = "failed", objective = NA_real_,
        pass = pass))
      next
    }

    saved <- suppressMessages(suppressWarnings(
      save_scenario(sol, verbose = FALSE)))
    .by_sample_tag_variant(saved, vlab, sequence = name, method = method,
                           params = vparams,
                           slices = nrow(rows), yf = yf)
    if (keep_scenarios) scens[[sid]] <- saved
    runs <- bind_rows(runs, tibble(
      sample = sid, run = fp(vlab, saved@misc$run %||% "?"),
      slices = nrow(rows), year_fraction = yf,
      status = "solved", objective = obj, pass = pass))
  }
  list(scens = scens, runs = runs, failed = failed)
  }

  free <- one_pass(object, sfx = "", pass = "free", base = base)
  scens <- free$scens
  runs <- free$runs
  failed <- free$failed
  floored <- NULL

  if (ncap_floor > 0 && length(free$scens)) {
    tg <- .ensemble_consensus(free$scens, stat = ncap_stat)
    if (is.null(tg) || !nrow(tg)) {
      warning("No new capacity was built in any sample, so there is no ",
              "consensus to floor; returning the free pass.", call. = FALSE)
    } else {
      if (verbose) {
        message("consensus floor: ", ncap_floor, " x ", ncap_stat, " of ",
                nrow(tg), " (process, region, year) build rate(s)")
      }
      hz <- tryCatch(object@config@horizon, error = function(e) NULL)
      mod_f <- apply_targets(object, tg, horizon = hz,
                             slack = 1 - ncap_floor, mode = "lo",
                             basis = "ncap", verbose = FALSE)
      # No base for the floored pass: apply_targets() writes bounds into the
      # model's @capacity slots, so its tables are not the base's filtered --
      # interpolate_model() would refuse and say so once per sample.
      floored <- one_pass(mod_f, sfx = "_f", pass = "floor")
      scens <- floored$scens
      runs <- bind_rows(free$runs, floored$runs)
      failed <- failed || floored$failed
    }
  }

  .en_log("by_sample", name, duration = NA, model = object@name,
          samples = length(ids), method = method, failed = failed)

  structure(list(
    name = name, model = object, runs = runs, scenarios = scens,
    # the floored solves ARE the answer when a floor was asked for, so they are
    # `$scenarios`; the free pass stays reachable because it is what the
    # consensus was taken from
    scenarios_free = if (is.null(floored)) NULL else free$scens,
    ncap_floor = ncap_floor,
    ncap_stat = if (ncap_floor > 0) ncap_stat else NA_character_,
    samples = spec, method = method, calendar = cal,
    path = if (length(scens)) scens[[length(scens)]]@path else NULL,
    failed = failed
  ), class = "by_sample")
}

# The ensemble's agreed build rate: `stat` of each sample's `vTechNewCap` (and
# the storage families' equivalents) per process, region and year.
#
# Taken across ALL samples rather than from a donor or a chain, so the result
# does not depend on the order they were solved and the samples stay
# exchangeable -- which is what keeps sample_summary()'s quantiles quantiles
# over replicates.
#
# A sample that built nothing for a given key contributes a ZERO, not a
# missing value: `.capacity_targets()` drops non-positive rows, so averaging
# only the rows present would read "three of five samples built X" as "every
# sample built X" and floor the ensemble above what it agreed on.
#' @noRd
.ensemble_consensus <- function(scens, stat = c("mean", "median")) {
  stat <- match.arg(stat)
  n <- length(scens)
  if (!n) return(NULL)
  parts <- lapply(scens, function(sc) {
    tryCatch(.capacity_targets(sc, var = "newcap"), error = function(e) NULL)
  })
  parts <- Filter(function(d) !is.null(d) && nrow(d), parts)
  if (!length(parts)) return(NULL)
  all <- bind_rows(parts)
  key <- paste(all$family, all$proc, all$region, all$year, sep = "
")
  f <- if (identical(stat, "median")) stats::median else mean
  agg <- vapply(split(all$value, key), function(v) {
    f(c(v, rep(0, n - length(v))))        # absent sample = built nothing
  }, numeric(1))
  first <- all[!duplicated(key), , drop = FALSE]
  first <- first[match(names(agg), key[!duplicated(key)]), , drop = FALSE]
  first$value <- as.numeric(agg)
  first <- first[!is.na(first$value) & first$value > 0, , drop = FALSE]
  structure(first, class = unique(c("capacity_targets", class(first))))
}

# TRUE when any storage declares an out.af.* row
.has_out_af <- function(mod) {
  for (rp in names(mod@data)) {
    repo <- mod@data[[rp]]
    if (!isS4(repo)) next
    for (nm in names(repo@data)) {
      o <- repo@data[[nm]]
      if (!is(o, "storage")) next
      af <- tryCatch(as.data.frame(o@af), error = function(e) NULL)
      if (is.null(af) || !nrow(af)) next
      cols <- grep("^out\\.af\\.", names(af), value = TRUE)
      for (cc in cols) if (any(!is.na(af[[cc]]))) return(TRUE)
    }
  }
  FALSE
}

#' Summarise an ensemble across samples
#'
#' @description
#' Median and quantiles of a variable across the samples of a
#' [solve_by_sample()] ensemble — the input for range plots of capacity.
#'
#' Each sample estimates the whole year, so these are quantiles of repeated
#' estimates, not of a partitioned total.
#'
#' `mean` is weighted by each sample's `year_fraction`, because samples need
#' not cover equal shares of the year — `method = "sequential"` folds a
#' trailing singleton block into its predecessor, so one sample can be larger
#' than the rest. The quantiles are **unweighted** order statistics of the
#' replicates; a message says so whenever the coverage varies.
#'
#' @param x a `by_sample` result.
#' @param name character, the variable to summarise (e.g. `"vTechCap"`).
#' @param probs numeric, quantiles to report.
#' @param by character, columns to group by; defaults to every key column the
#'   variable carries apart from `sample` and `value`.
#' @param ... passed to [getData()].
#'
#' @return a data.frame with the grouping columns, `n`, `mean`
#'   (coverage-weighted) and one column per requested quantile (unweighted).
#' @seealso [solve_by_sample()]
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
#' res <- solve_by_sample(mod, name = "BY_SAMPLE", level = "SEASON",
#'                        sample_size = 2, n = 2, method = "random", seed = 1,
#'                        solver = solver_options$glpk)
#'
#' sample_summary(res, "vTechCap", by = "tech")
#'
#' # a wider band, by region as well
#' sample_summary(res, "vTechNewCap", probs = c(0.1, 0.5, 0.9),
#'                by = c("tech", "region"))
#' }
sample_summary <- function(x, name, probs = c(0.05, 0.5, 0.95), by = NULL,
                           ...) {
  stopifnot(inherits(x, "by_sample"))
  d <- getData(x, name, ...)
  if (is.null(d) || !nrow(d)) return(NULL)
  d <- as.data.frame(d)
  keys <- by %||% setdiff(names(d), c("sample", "value", "scenario", "name"))
  keys <- intersect(keys, names(d))
  grp <- if (length(keys)) d[, keys, drop = FALSE] else
    data.frame(.all = "all", stringsAsFactors = FALSE)
  # Samples are not equal-sized: `method = "sequential"` folds a trailing
  # singleton into the previous block, so one block can cover five weeks where
  # the others cover four. Each sample's `year_fraction` is its share of the
  # year, so it is the weight the mean owes them; an unweighted mean gives the
  # five-week block the same single vote.
  wv <- NULL
  yf <- NULL
  if ("sample" %in% names(d) && !is.null(x$runs) &&
      !is.null(x$runs[["year_fraction"]])) {
    yf <- stats::setNames(as.numeric(x$runs$year_fraction),
                          as.character(x$runs$sample))
    cand <- yf[as.character(d$sample)]
    if (!anyNA(cand) && any(cand > 0, na.rm = TRUE)) wv <- cand
  }
  if (!is.null(wv) && length(unique(signif(yf, 12))) > 1L) {
    message("Samples cover unequal shares of the year (year_fraction ",
            paste(format(range(yf), digits = 4), collapse = " to "),
            "); `mean` is weighted by coverage, the quantiles are not.")
  }

  byg <- lapply(grp, as.character)
  sp <- split(d$value, byg, drop = TRUE)
  spw <- if (is.null(wv)) NULL else split(wv, byg, drop = TRUE)
  out <- lapply(names(sp), function(g) {
    v <- sp[[g]]
    # Unweighted on purpose: a weighted quantile needs an interpolation rule,
    # and reporting one invented here would be worse than reporting the plain
    # order statistics of the replicates.
    q <- stats::quantile(v, probs = probs, names = FALSE, na.rm = TRUE)
    row <- as.data.frame(as.list(stats::setNames(
      q, paste0("q", format(100 * probs, trim = TRUE)))))
    row$n <- length(v)
    row$mean <- if (is.null(spw)) {
      mean(v, na.rm = TRUE)
    } else {
      w <- spw[[g]]
      ok <- !is.na(v) & !is.na(w) & w > 0
      if (any(ok)) sum(v[ok] * w[ok]) / sum(w[ok]) else NA_real_
    }
    row
  })
  res <- do.call(rbind, out)
  keydf <- unique(grp)
  keydf <- keydf[order(do.call(paste, c(lapply(keydf, as.character),
                                        sep = "\r"))), , drop = FALSE]
  cbind(keydf[seq_len(nrow(res)), , drop = FALSE], res)
}

#' @method print by_sample
#' @export
print.by_sample <- function(x, ...) {
  cat("by_sample '", x$name, "': ", nrow(x$runs), " sample(s), method = ",
      x$method,
      if (isTRUE((x$ncap_floor %||% 0) > 0)) {
        paste0(", ncap_floor = ", x$ncap_floor, " of the ", x$ncap_stat)
      } else {
        ""
      },
      "\n", sep = "")
  df <- as.data.frame(x$runs)
  if (nrow(df)) print(df, row.names = FALSE)
  ok <- x$runs$status == "solved"
  if (any(ok)) {
    o <- x$runs$objective[ok]
    cat(sprintf("objective across samples: median %s, range %s - %s\n",
                format(stats::median(o)), format(min(o)), format(max(o))))
  }
  cat("each sample is annualised and estimates the WHOLE year: these are ",
      "replicates, not pieces -- do not sum them.\n", sep = "")
  if (isTRUE((x$ncap_floor %||% 0) > 0)) {
    cat("capacity is floored at the ensemble consensus, so the spread is ",
        "period variation at ONE plan; a sample above the floor marks a ",
        "period that plan cannot serve. $scenarios_free holds the free ",
        "pass.\n", sep = "")
  }
  if (isTRUE(x$failed)) cat("(at least one sample failed)\n")
  invisible(x)
}

#' @export
getData.by_sample <- function(object, name = NULL, ..., merge = TRUE) {
  x <- object
  if (length(x$scenarios) == 0) {
    stop("This by_sample result kept no scenario objects ",
         "(keep_scenarios = FALSE); re-run with keep_scenarios = TRUE.",
         call. = FALSE)
  }
  out <- list()
  for (k in names(x$scenarios)) {
    d <- suppressMessages(getData(x$scenarios[[k]], name, ..., merge = TRUE))
    if (is.null(d) || nrow(d) == 0) next
    d$sample <- k
    out[[length(out) + 1L]] <- d
  }
  if (!length(out)) return(NULL)
  bind_rows(out)
}

#' @method print calendar_samples
#' @export
print.calendar_samples <- function(x, ...) {
  ids <- unique(x$sample)
  cat("calendar_samples: ", length(ids), " sample(s), method = ",
      attr(x, "method") %||% "?", "\n", sep = "")
  sz <- vapply(ids, function(i) sum(x$count[x$sample == i]), numeric(1))
  cat("  slices per sample: ", paste(range(sz), collapse = "-"),
      " | distinct slices used: ", length(unique(x$timeslice)), "\n", sep = "")
  invisible(x)
}
