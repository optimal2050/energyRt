# =============================================================================#
# log.R — optional operation log.
#
# One CSV file, off by default: `set_log_file(path)` turns it on, and the
# instrumented operations (interpolate_model, solve_scenario, solve_myopic)
# append one line each — what ran, on what, how it ended, how long it took.
# The log documents a SEQUENCE of operations across a session or a script;
# per-run solver provenance stays in run.yml, per-scenario save history in
# the scenario's logfile.csv. Logging must never fail an operation: every
# write is wrapped, problems surface as (at most) a warning.
# =============================================================================#

.en_log_header <- c("timestamp", "op", "object", "status", "duration_sec",
                    "mem_mb", "peak_mb", "details")

# R-heap memory, in MB. `Sys.procmem()` would give process RSS but is not
# available on every platform (absent on Windows R 4.5), so gc() is what we can
# rely on: `used` is the heap in use now, `max used` the high-water mark since
# the last `gc(reset = TRUE)`.
#
# The peak is the number that answers "will this fit"; the current value only
# says what survived. Both are logged because the gap between them is itself
# informative -- a stage that peaks far above what it keeps is one that
# materialises something large and throws it away.
.en_mem <- function() {
  g <- tryCatch(gc(verbose = FALSE, reset = FALSE), error = function(e) NULL)
  if (is.null(g)) return(list(mem_mb = NA_real_, peak_mb = NA_real_))
  # gc() returns: used | (Mb) | gc trigger | (Mb) | max used | (Mb)
  # The MB figure always sits in the column immediately after its counter, so
  # locate the counter by name and step one across. Matching the "(Mb)" headers
  # directly is ambiguous -- there are three of them.
  cn <- colnames(g)
  mb_after <- function(counter) {
    j <- which(cn == counter)
    if (!length(j) || j[[1]] + 1L > ncol(g)) return(NA_real_)
    sum(g[, j[[1]] + 1L], na.rm = TRUE)
  }
  list(mem_mb  = round(mb_after("used"), 1),
       peak_mb = round(mb_after("max used"), 1))
}

# Append one operation line to the log file; silent no-op when logging is
# off (empty `log_file` option) or `object` is scratch (dot-prefixed names,
# e.g. levcost mini-scenarios — they would flood the log).
.en_log <- function(op, object, status = "ok", duration = NA_real_,
                    mem_mb = NULL, peak_mb = NULL, ...) {
  file <- get_log_file()
  if (is.null(file) || !nzchar(file)) return(invisible(FALSE))
  if (startsWith(object %||% "", ".")) return(invisible(FALSE))
  ok <- tryCatch({
    m <- .en_mem()
    kv <- list(...)
    kv <- kv[!vapply(kv, is.null, logical(1))]
    details <- paste(names(kv),
                     vapply(kv, function(v) paste(format(v), collapse = "+"),
                            character(1)),
                     sep = "=", collapse = "; ")
    row <- data.frame(
      timestamp = .registry_now(),
      op = op,
      object = object %||% "",
      status = status,
      duration_sec = round(as.numeric(duration), 2),
      mem_mb = if (is.null(mem_mb)) m$mem_mb else round(as.numeric(mem_mb), 1),
      peak_mb = if (is.null(peak_mb)) m$peak_mb else round(as.numeric(peak_mb), 1),
      details = details,
      stringsAsFactors = FALSE)
    dir_ <- dirname(file)
    if (!dir.exists(dir_)) {
      dir.create(dir_, recursive = TRUE, showWarnings = FALSE)
    }
    suppressWarnings(
      utils::write.table(row, file, sep = ",", append = file.exists(file),
                         col.names = !file.exists(file), row.names = FALSE,
                         qmethod = "double"))
    TRUE
  }, error = function(e) {
    warning("Operation log could not be written to '", file, "': ",
            conditionMessage(e), call. = FALSE)
    FALSE
  })
  invisible(ok)
}

#' Read the operation log
#'
#' @description
#' The operation log is an optional, project-wide record of what ran:
#' [set_log_file()] names a CSV file, and from then on
#' [interpolate_model()], [solve_scenario()] (and through it
#' [solve_model()]), and [solve_myopic()] append one line each —
#' `timestamp, op, object, status, duration_sec, details`. `read_log()`
#' reads it back as a tibble with parsed timestamps.
#'
#' Levcost's internal mini-model solves are not logged (their scratch
#' scenarios are dot-prefixed). Per-run solver detail lives in each run's
#' `run.yml` (see [scenario_runs()]); the log records the sequence.
#'
#' @param file character, the log file (default: the `log_file` option).
#' @return a tibble, one row per logged operation (empty when the file does
#'   not exist).
#' @rdname log
#' @export
#' @examples
#' \dontrun{
#' set_log_file("energyRt_log.csv")
#' scen <- interpolate_model(mod) |> solve_scenario()
#' read_log()
#' }
read_log <- function(file = get_log_file()) {
  if (is.null(file) || !nzchar(file) || !file.exists(file)) {
    num <- c("duration_sec", "mem_mb", "peak_mb")
    return(as_tibble(setNames(lapply(.en_log_header, function(h) {
      if (h == "timestamp") as.POSIXct(character(0), tz = "UTC")
      else if (h %in% num) numeric(0) else character(0)
    }), .en_log_header)))
  }
  d <- utils::read.csv(file, stringsAsFactors = FALSE,
                       colClasses = "character") |> as_tibble()
  d$timestamp <- as.POSIXct(d$timestamp, format = "%Y-%m-%dT%H:%M:%SZ",
                            tz = "UTC")
  for (nm in c("duration_sec", "mem_mb", "peak_mb")) {
    if (nm %in% names(d)) d[[nm]] <- suppressWarnings(as.numeric(d[[nm]]))
  }
  d
}
