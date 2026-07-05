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
  if (!isTRUE(verbose) || is.null(.interp_clock$msg)) return(invisible())
  dt <- as.numeric(Sys.time() - .interp_clock$t, units = "secs")
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
.interp_step <- function(verbose, msg, oneline = TRUE) {
  if (!isTRUE(verbose)) return(invisible())
  .interp_step_done(verbose)          # close prior stage with its timing
  .interp_clock$t   <- Sys.time()
  .interp_clock$msg <- msg
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
