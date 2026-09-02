# =============================================================================#
# solve_by_sample.R — solve a model on samples of its calendar.
#
# Each sample is an ordinary sampled-calendar solve (the recipe
# `interpolate_model()` already accepts), stored as an own-problem variant of
# one scenario. Three sampling methods: consecutive blocks tiling the year,
# disjoint random draws, and bootstrap draws with replacement.
#
# Samples are REPLICATES, not shards. `pTimesliceWeight = 1/year_fraction`
# annualises each one, so every sample estimates the whole year and the set is
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
# Samples are RECTANGULAR: whole members of one level, with the full cross of
# finer levels beneath. A ragged selection is silently re-expanded to the full
# cross (24 scattered leaves of s4_h24 come back as 64), which leaves the
# scaled shares attached to only some rows and the interpolation reports
# duplicate keys and NA indices. Sampling therefore draws members of `level`,
# and `.sample_calendar()` refuses a ragged hand-built selection.
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
#' Samples are drawn as whole members of `level` because a calendar sample must
#' be a complete cross: a ragged selection of individual leaf timeslices is
#' silently re-expanded by the calendar constructor. Each sample must also keep
#' at least two members at every non-ANNUAL level, so `sample_size` of 1 is
#' refused for the coarsest level.
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
  # the selection must be a complete cross of the surviving level members:
  # a ragged one is re-expanded by .complete_calendar(), which puts back rows
  # the sample never chose and leaves the scaled shares on only some of them
  lvs <- intersect(.cal_levels(cal), names(sub))
  if (length(lvs)) {
    want <- prod(vapply(lvs, function(l)
      length(unique(as.character(sub[[l]]))), numeric(1)))
    if (nrow(sub) != want) {
      stop("A calendar sample must be a complete cross of its level members: ",
           "this one selects ", nrow(sub), " timeslice(s) where the surviving ",
           "members of ", paste(lvs, collapse = " x "), " span ", want, ".
",
           "  Draw whole members of a level (calendar_samples(level = )) ",
           "rather than individual timeslices.", call. = FALSE)
    }
  }
  sub <- sub[, setdiff(names(sub), "weight"), with = FALSE]
  newCalendar(name = name, timetable = sub,
              year_fraction = sum(sub$share))
}

# add/refresh driver metadata in a variant.yml written by save_scenario()
.by_sample_tag_variant <- function(scen, vlab, sequence, method, slices, yf) {
  vy <- fp(scen@path, "runs", vlab, "variant.yml")
  if (!file.exists(vy)) return(invisible(NULL))
  mf <- tryCatch(yaml::read_yaml(vy), error = function(e) NULL)
  if (is.null(mf)) return(invisible(NULL))
  mf$type <- "calendar_sample"
  mf$sequence <- sequence
  mf$method <- method
  mf$slices <- as.integer(slices)
  mf$year_fraction <- as.numeric(yf)
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
#' Each sample is annualised (`pTimesliceWeight = 1/year_fraction`), so every
#' sample estimates the **whole year**: the runs are replicates and their spread
#' is the sampling uncertainty. They are not pieces of a year and must not be
#' summed — unlike [solve_by_region()], whose disjoint samples do add up.
#'
#' A sample must keep at least two members at every non-ANNUAL level of the
#' calendar. A one-season (or one-week, or one-day) window collapses its outer
#' level and is refused, because the only way to build it would rename the leaf
#' timeslices and silently detach the model's data.
#'
#' @param mod a model object.
#' @param name character, the sequence name (one scenario folder).
#' @param ... passed to [interpolate_model()] for every sample.
#' @param samples a `calendar_samples` table, a `data.frame(sample, timeslice)`,
#'   or a calendar carrying a `sample` column. When `NULL`, built by
#'   [calendar_samples()] from `sample_size` / `n` / `method`.
#' @param level,sample_size,n,method,seed passed to [calendar_samples()] when
#'   `samples` is `NULL`.
#' @param calendar the calendar to sample; the model's own when `NULL`.
#' @param solver a solver option list; the scenario's default when `NULL`.
#' @param on_error `"stop"` (default) or `"continue"` — with `"continue"` a
#'   sample that fails to solve is recorded and the ensemble carries on.
#' @param keep_scenarios logical, keep each sample's scenario object.
#' @param verbose logical.
#'
#' @return a `by_sample` object: `runs` (a tibble of what ran), `scenarios`,
#'   `samples` and `method`.
#' @seealso [calendar_samples()], [sample_summary()], [solve_by_region()]
#' @export
solve_by_sample <- function(mod, name = NULL, ...,
                            samples = NULL, level = NULL,
                            sample_size = NULL, n = NULL,
                            method = c("sequential", "random", "bootstrap"),
                            seed = NULL, calendar = NULL,
                            solver = NULL,
                            on_error = c("stop", "continue"),
                            keep_scenarios = TRUE, verbose = TRUE) {
  stopifnot(is(mod, "model"))
  method <- match.arg(method)
  on_error <- match.arg(on_error)
  if (is.null(name)) name <- paste0(mod@name, "_by_sample")
  .assert_object_name(name, "name")

  cal <- calendar %||% tryCatch(mod@config@calendar, error = function(e) NULL)
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
  if (.has_out_af(mod)) {
    warning("This model declares storage `out.af.*` rows, which are ",
            "infeasible on a sampled (year_fraction < 1) calendar. Samples ",
            "are likely to fail; declare the availability on the charging ",
            "side (`inp.af.*`) instead.", call. = FALSE)
  }

  extra <- list(...)
  seq_path <- fp(get_scenarios_path(), .path_slug(name, mod@name))
  scens <- list()
  failed <- FALSE
  runs <- tibble(sample = character(0), run = character(0),
                 slices = integer(0), year_fraction = numeric(0),
                 status = character(0), objective = numeric(0))

  for (k in seq_along(ids)) {
    sid <- ids[k]
    rows <- spec[spec$sample == sid, , drop = FALSE]
    cnt <- stats::setNames(rows$count, rows$timeslice)
    if (verbose) {
      message("sample ", k, "/", length(ids), ": ", sid, " (",
              nrow(rows), " slice(s))")
    }
    sol <- tryCatch({
      cal_k <- .sample_calendar(cal, rows$timeslice, counts = cnt,
                               name = paste0("cs_", sid))
      args <- c(list(mod, name = name), extra,
                list(cal_k, path = seq_path, overwrite = TRUE))
      scen_k <- suppressMessages(do.call(interpolate_model, args))
      solve_scenario(scen_k, solver = solver, echo = FALSE, force = TRUE,
                     variant = sid)
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
      msg <- paste0("Calendar sample '", sid, "' did not solve",
                    if (inherits(sol, "error"))
                      paste0(": ", conditionMessage(sol)) else " to optimal",
                    ".")
      if (on_error == "stop") stop(msg, call. = FALSE)
      warning(msg, call. = FALSE)
      failed <- TRUE
      runs <- bind_rows(runs, tibble(
        sample = sid, run = NA_character_, slices = nrow(rows),
        year_fraction = NA_real_, status = "failed", objective = NA_real_))
      next
    }

    saved <- suppressMessages(suppressWarnings(
      save_scenario(sol, verbose = FALSE)))
    .by_sample_tag_variant(saved, sid, sequence = name, method = method,
                           slices = nrow(rows), yf = yf)
    if (keep_scenarios) scens[[sid]] <- saved
    runs <- bind_rows(runs, tibble(
      sample = sid, run = fp(sid, saved@misc$run %||% "?"),
      slices = nrow(rows), year_fraction = yf,
      status = "solved", objective = obj))
  }

  .en_log("by_sample", name, duration = NA, model = mod@name,
          samples = length(ids), method = method, failed = failed)

  structure(list(
    name = name, model = mod, runs = runs, scenarios = scens,
    samples = spec, method = method, calendar = cal,
    path = if (length(scens)) scens[[length(scens)]]@path else NULL,
    failed = failed
  ), class = "by_sample")
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
#' @param x a `by_sample` result.
#' @param name character, the variable to summarise (e.g. `"vTechCap"`).
#' @param probs numeric, quantiles to report.
#' @param by character, columns to group by; defaults to every key column the
#'   variable carries apart from `sample` and `value`.
#' @param ... passed to [getData()].
#'
#' @return a data.frame with the grouping columns, `n`, `mean` and one column
#'   per requested quantile.
#' @seealso [solve_by_sample()]
#' @export
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
  sp <- split(d$value, lapply(grp, as.character), drop = TRUE)
  out <- lapply(names(sp), function(g) {
    v <- sp[[g]]
    q <- stats::quantile(v, probs = probs, names = FALSE, na.rm = TRUE)
    row <- as.data.frame(as.list(stats::setNames(
      q, paste0("q", format(100 * probs, trim = TRUE)))))
    row$n <- length(v)
    row$mean <- mean(v, na.rm = TRUE)
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
      x$method, "\n", sep = "")
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
  if (isTRUE(x$failed)) cat("(at least one sample failed)\n")
  invisible(x)
}

#' @export
getData.by_sample <- function(scen, name = NULL, ..., merge = TRUE) {
  x <- scen
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
