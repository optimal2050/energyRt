# =========================================================================== #
# interp_progress.R  —  progress reporting for interp_mod().
#
# Two layers, both opt-in:
#   * `verbose = TRUE` renders an always-visible cli banner (selected options) and
#     one cli step line per pipeline stage (including the fold/prune/densify
#     routines as they run), plus a closing `model_size()` summary.
#   * a `progressr` progressor wraps the per-object interpolation loop, so a user
#     who registers a handler (`progressr::handlers(...)`) gets a customizable
#     progress bar — matching the legacy pipeline's behaviour. Silent when no
#     handler is set, so it never disturbs the default / test path.
# =========================================================================== #

# Module clock so each stage reports its own elapsed time. The currently-running
# stage is the last `>` (info) line with no matching `v` (done) line yet, so a
# user always sees which stage is in progress -- important for the slow stages
# (ob2mi, densify, the model-size summary) on large models.
.interp_clock <- new.env(parent = emptyenv())

.interp_fmt_secs <- function(s) {
  if (is.na(s)) "" else if (s < 60) sprintf("%.1fs", s)
  else if (s < 3600) sprintf("%dm %02ds", s %/% 60, round(s %% 60))
  else sprintf("%dh %02dm", s %/% 3600, (s %% 3600) %/% 60)
}

# TRUE when we can rewrite the current terminal line in place (interactive TTY).
# On a dynamic TTY the stage prints `> msg` with no newline and is completed in
# place as `v msg (1.4s)`; otherwise (logs, tests, non-interactive) it falls back
# to two separate lines.
.interp_dyn_tty <- function() {
  tryCatch(isTRUE(cli::is_dynamic_tty()), error = function(e) interactive())
}

# Mark the currently-running stage (if any) as done, with elapsed time. On a
# dynamic TTY this overwrites the open `> msg` line in place with `v msg (time)`.
.interp_step_done <- function(verbose = TRUE) {
  if (is.null(.interp_clock$msg)) return(invisible())
  dt <- as.numeric(Sys.time() - .interp_clock$t, units = "secs")

  # Record before printing, so a non-verbose run still accounts for the stage.
  # `peak_mb` is the PER-STAGE heap high-water mark (the mark is reset at each
  # stage start in `.interp_step`); `rss_mb` / `peak_rss_mb` are OS process
  # memory (see `.en_rss` -- the peak is process-lifetime, a running maximum).
  m1 <- tryCatch(.en_mem(), error = function(e) NULL)
  .interp_clock$stages <- c(.interp_clock$stages %||% list(), list(list(
    stage = .interp_clock$msg,
    secs = round(dt, 2),
    mem_mb = if (is.null(m1)) NA_real_ else m1$mem_mb,
    peak_mb = if (is.null(m1)) NA_real_ else m1$peak_mb,
    d_mem_mb = if (is.null(m1) || is.null(.interp_clock$m0)) NA_real_
               else round(m1$mem_mb - .interp_clock$m0$mem_mb, 1),
    rss_mb = if (is.null(m1)) NA_real_ else m1$rss_mb,
    peak_rss_mb = if (is.null(m1)) NA_real_ else m1$peak_rss_mb)))

  if (!isTRUE(verbose)) {
    .interp_clock$open <- FALSE
    .interp_clock$msg <- NULL
    return(invisible())
  }
  done <- paste0(.interp_clock$msg, " ",
                 cli::col_grey("(", .interp_fmt_secs(dt), ")"))
  if (isTRUE(.interp_clock$open)) {
    # rewrite the open `> msg` line: `\r` to column 0, then `v msg (time)`. The
    # done text is always longer than `> msg`, so it fully overwrites it.
    cat("\r", cli::col_green(cli::symbol$tick), " ", done, "\n", sep = "")
  } else {
    cli::cli_alert_success(done, .envir = emptyenv())
  }
  .interp_clock$open <- FALSE
  .interp_clock$msg <- NULL
  invisible()
}

# Start-of-run banner: the resolved storage / sparse / prune / fold / validate
# choices. No-op unless verbose. Also resets the stage clock.
.interp_banner <- function(scen, sparse, prune, fold_dims, validate, ondisk,
                           verbose) {
  if (!isTRUE(verbose)) return(invisible())
  .interp_clock$msg  <- NULL
  .interp_clock$open <- FALSE
  .interp_clock$t    <- Sys.time()
  cli::cli_h1(paste0("interpolate_model: ", scen@name))
  cli::cli_dl(c(
    storage  = if (isTRUE(ondisk)) paste0("on-disk (", scen@path, ")") else "in-memory",
    sparse   = as.character(isTRUE(sparse)),
    prune    = as.character(isTRUE(prune)),
    fold     = if (length(fold_dims)) paste(fold_dims, collapse = ", ") else "none",
    validate = as.character(isTRUE(validate))
  ))
  invisible()
}

# One pipeline-stage line. Closes the previous stage (with its elapsed time),
# then announces this one. `msg` is treated literally (no glue interpolation).
#' @param oneline when TRUE (default) and on a dynamic TTY, the stage prints on a
#'   single line completed in place. Set FALSE for stages that emit their own
#'   output while running (e.g. a progressr bar), so that output gets a clean
#'   line and the stage falls back to the two-line info/success form.
#' @noRd
# Stage accounting is ALWAYS on; `verbose` only controls whether it is printed.
# The timings are what the operation log reports, and a log that only works in
# verbose runs is no log at all.
.interp_stages_reset <- function() {
  .interp_clock$stages <- list()
  .interp_clock$maps <- list()
  .interp_clock$params <- list()
  .interp_clock$memo <- NULL
  invisible()
}

# --------------------------------------------------------------------------- #
# Per-interpolation memoization for model-derived lookups (process classes,
# invest-year windows, process->timeslice tables). These are recomputed by
# dozens of map builders while the underlying model objects are fixed for the
# whole run (variants are expanded before any builder runs). Entries are
# guarded by a `fingerprint` compared with identical(), so a memo populated by
# one scenario can never serve another: any change in the inputs the caller
# fingerprints invalidates the entry. The cache lives in `.interp_clock` and
# is dropped by `.interp_stages_reset()` at the start of every interpolation.
# --------------------------------------------------------------------------- #
.interp_memo <- function(key, fingerprint, compute) {
  cache <- .interp_clock$memo
  if (is.null(cache)) {
    cache <- new.env(parent = emptyenv())
    .interp_clock$memo <- cache
  }
  hit <- cache[[key]]
  if (!is.null(hit) && identical(hit$fp, fingerprint)) return(hit$value)
  value <- compute()
  cache[[key]] <- list(fp = fingerprint, value = value)
  value
}

# Fingerprint of the model's object roster: object names per repository.
# Any object added, dropped, or renamed (variant expansion, transform
# materialization) changes it.
.interp_model_fp <- function(scen) {
  lapply(scen@model@data, function(r) names(r@data))
}

.interp_stages <- function() {
  st <- .interp_clock$stages
  if (is.null(st) || !length(st)) return(NULL)
  do.call(rbind, lapply(st, as.data.frame, stringsAsFactors = FALSE))
}

# Per-map / per-parameter tick accounting (same always-on contract as the
# stage table). Retrieve with .interp_map_table() / .interp_param_table()
# right after interpolate_model() in the same session.
.interp_map_tick <- function(name, recipe, secs) {
  .interp_clock$maps[[length(.interp_clock$maps) + 1L]] <-
    list(map = name, recipe = recipe, secs = secs)
  invisible()
}

.interp_param_tick <- function(name, secs) {
  .interp_clock$params[[length(.interp_clock$params) + 1L]] <-
    list(parameter = name, secs = secs)
  invisible()
}

.interp_map_table <- function() {
  x <- .interp_clock$maps
  if (is.null(x) || !length(x)) return(NULL)
  do.call(rbind, lapply(x, as.data.frame, stringsAsFactors = FALSE))
}

.interp_param_table <- function() {
  x <- .interp_clock$params
  if (is.null(x) || !length(x)) return(NULL)
  do.call(rbind, lapply(x, as.data.frame, stringsAsFactors = FALSE))
}

.interp_step <- function(verbose, msg, oneline = TRUE) {
  .interp_step_done(verbose)          # close prior stage with its timing
  .interp_clock$t   <- Sys.time()
  .interp_clock$msg <- msg
  # reset = TRUE re-arms the heap high-water mark, so this stage's closing
  # probe reports the stage's OWN peak, not the session's.
  .interp_clock$m0  <- tryCatch(.en_mem(reset = TRUE), error = function(e) NULL)
  if (!isTRUE(verbose)) {
    .interp_clock$open <- FALSE
    return(invisible())
  }
  if (isTRUE(oneline) && .interp_dyn_tty()) {
    # open the line (no newline); .interp_step_done() completes it in place
    cat(cli::col_cyan(cli::symbol$info), " ", msg, sep = "")
    utils::flush.console()
    .interp_clock$open <- TRUE
  } else {
    cli::cli_alert_info(msg, .envir = emptyenv())
    .interp_clock$open <- FALSE
  }
  invisible()
}

# --------------------------------------------------------------------------- #
# Interpolation profile log: the stage / per-map / per-parameter timing +
# memory tables, appended to CSV files after EVERY interpolation. The
# in-session tables (`.interp_stages()` & co) vanish with the session; these
# files are the record performance choices are made from. Sink resolution:
#   1. `get_profile_dir()` when set;
#   2. the operation log's directory (`get_log_file()`), when logging is on;
#   3. `<project>/logs/` when a project registry EXISTS on disk.
# No sink resolvable (plain library use, tests) = silent no-op, and a failed
# write must never fail the interpolation.
# --------------------------------------------------------------------------- #
.interp_profile_dir <- function() {
  d <- tryCatch(get_profile_dir(), error = function(e) "")
  if (!is.null(d) && nzchar(d)) return(d)
  lf <- tryCatch(get_log_file(), error = function(e) "")
  if (!is.null(lf) && nzchar(lf)) return(dirname(lf))
  rf <- tryCatch(get_registry_file(), error = function(e) "")
  if (!is.null(rf) && nzchar(rf) && file.exists(rf)) {
    return(file.path(dirname(rf), "logs"))
  }
  NULL
}

.interp_profile_append <- function(dir, file, df) {
  if (is.null(df) || !NROW(df)) return(invisible())
  f <- file.path(dir, file)
  suppressWarnings(
    utils::write.table(df, f, sep = ",", append = file.exists(f),
                       col.names = !file.exists(f), row.names = FALSE,
                       qmethod = "double"))
  invisible()
}

.interp_profile_log <- function(scen, mod, ondisk, wall) {
  tryCatch({
    if (startsWith(scen@name %||% "", ".")) return(invisible())
    dir <- .interp_profile_dir()
    if (is.null(dir)) return(invisible())
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE,
                                     showWarnings = FALSE)
    run_id <- paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), "-", scen@name)
    m <- .en_mem()
    run <- data.frame(
      run_id = run_id,
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      scenario = scen@name,
      model = tryCatch(mod@name, error = function(e) ""),
      calendar = tryCatch(scen@settings@calendar@name, error = function(e) ""),
      ondisk = isTRUE(ondisk),
      dt_threads = tryCatch(data.table::getDTthreads(),
                            error = function(e) NA_integer_),
      total_secs = round(as.numeric(wall), 1),
      mem_mb = m$mem_mb,
      rss_mb = m$rss_mb,
      peak_rss_mb = m$peak_rss_mb,
      energyRt = as.character(utils::packageVersion("energyRt")),
      stringsAsFactors = FALSE)
    .interp_profile_append(dir, "interp_runs.csv", run)
    st <- .interp_stages()
    if (!is.null(st)) {
      st <- cbind(run_id = run_id, st, stringsAsFactors = FALSE)
      .interp_profile_append(dir, "interp_stages.csv", st)
    }
    mp <- .interp_map_table()
    if (!is.null(mp)) {
      mp <- cbind(run_id = run_id, mp, stringsAsFactors = FALSE)
      .interp_profile_append(dir, "interp_maps.csv", mp)
    }
    pp <- .interp_param_table()
    if (!is.null(pp)) {
      pp <- cbind(run_id = run_id, pp, stringsAsFactors = FALSE)
      .interp_profile_append(dir, "interp_params.csv", pp)
    }
    invisible()
  }, error = function(e) {
    warning("Interpolation profile log could not be written: ",
            conditionMessage(e), call. = FALSE)
    invisible()
  })
}

# Closing size + fold summary. The model_size() computation itself is timed as a
# stage (it scans every parameter's rows, which is slow on very large models).
.interp_footer <- function(scen, verbose) {
  if (!isTRUE(verbose)) return(invisible())
  .interp_step(verbose, "computing model-size summary")
  ms <- model_size(scen)              # the slow scan
  .interp_step_done(verbose)          # close it with its own elapsed time
  cli::cli_rule()
  print(ms)
  invisible()
}
