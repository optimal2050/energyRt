# Gated emitters ##############################################################
#
# Every user-facing message in the package goes through one of these, so the
# `verbose` option is the single control and no message can be unsilenceable.
# The levels are the ones `isVerbose()` reads:
#
#   level 1  an ACTION reports itself once -- what was written, loaded, solved
#            or removed, and where. On by default.
#   level 2  per-object and per-parameter detail: a carry record, a fold's row
#            counts, a region subset's membership.
#
# `warning()` and `stop()` are NOT routed here. A condition is not a message:
# it is part of the function's contract, callers can handle it, and silencing
# it would hide a problem. `packageStartupMessage()` stays as it is, since
# `suppressPackageStartupMessages()` is its established control.

#' @noRd
.say <- function(..., level = 1L) {
  if (isVerbose(level)) message(...)
  invisible(NULL)
}

# Level-2 shorthand, so a call site reads as its level.
#' @noRd
.say2 <- function(...) .say(..., level = 2L)

# cli's alerts interpolate `{...}` in the CALLING frame, so `.envir` has to be
# forwarded or a wrapped call would look for the variables in here.
#' @noRd
.alert_info <- function(..., level = 1L, .envir = parent.frame()) {
  if (isVerbose(level)) cli::cli_alert_info(..., .envir = .envir)
  invisible(NULL)
}

#' @noRd
.alert_success <- function(..., level = 1L, .envir = parent.frame()) {
  if (isVerbose(level)) cli::cli_alert_success(..., .envir = .envir)
  invisible(NULL)
}

#' @noRd
.alert_warning <- function(..., level = 1L, .envir = parent.frame()) {
  if (isVerbose(level)) cli::cli_alert_warning(..., .envir = .envir)
  invisible(NULL)
}

#' @noRd
.alert_danger <- function(..., level = 1L, .envir = parent.frame()) {
  if (isVerbose(level)) cli::cli_alert_danger(..., .envir = .envir)
  invisible(NULL)
}
