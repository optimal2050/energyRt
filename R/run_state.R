# =============================================================================#
# run_state.R — is this run still being written?
#
# A run record says `running` from the moment a solve starts until the process
# that started it writes the closing record. If that process dies without R-level
# handling -- a segfault, an OS kill, a lost session, a detached solve whose
# console was closed -- the record says `running` for ever, and nothing in the
# package noticed: a re-solve overwrote it, a cleanup deleted the live solve's
# scratch, and a read either errored obscurely or served the PREVIOUS solve's
# numbers.
#
# So a record identifies its process, and the state of a run is derived rather
# than taken at face value. The creation time is what makes the pid
# trustworthy: pids are reused, so "pid 37480 exists" is not "my solver is
# still running" -- only "pid 37480 exists AND started when the record says"
# is.
#
# The pid state has three values, not two, because it is only testable on the
# host that recorded it. `unknown` is treated as busy: guessing in the
# permissive direction is exactly what let a cleanup delete the files of a
# solve running on another machine against a shared store.
# =============================================================================#

#' @include save-scenario.R utils.R
NULL

.run_host <- function() unname(Sys.info()["nodename"])

# This process, as a run record identifies it. `ps` is optional (it is used the
# same way in R/log.R); without it the pid is still recorded, and the state
# degrades to `unknown` rather than being guessed from a pid alone.
.run_proc_stamp <- function() {
  ct <- NULL
  if (requireNamespace("ps", quietly = TRUE)) {
    ct <- tryCatch(ps::ps_create_time(ps::ps_handle()), error = function(e) NULL)
  }
  list(pid = Sys.getpid(), pid_started = .run_stamp_fmt(ct))
}

# The creation time as a whole-second UTC timestamp, like the record's own
# `started`. NOT a number: `yaml::write_yaml()` writes a double with eight
# significant digits, so 1793116738.5 comes back as 1793116700 -- a 38-second
# error, which read a live process as dead. A string round-trips exactly and
# is legible in the manifest.
.run_stamp_fmt <- function(ct) {
  if (is.null(ct) || !length(ct) || is.na(ct)) return("")
  format(ct, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

.run_stamp_num <- function(x) {
  x <- as.character(x %||% "")
  if (!nzchar(x)) return(NA_real_)
  as.numeric(tryCatch(as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
                      error = function(e) NA))
}

#' Is the process behind a run record still running?
#'
#' `"alive"`, `"dead"` or `"unknown"`. Only a `running` record can be alive;
#' anything else has already closed itself.
#' @noRd
.get_pid_state <- function(rec) {
  if (is.null(rec) || !identical(as.character(rec$status %||% ""), "running")) {
    return("dead")
  }
  # A detached run records the pid of the session that LAUNCHED it, because
  # system(wait = FALSE) does not hand back the solver's pid. Testing that pid
  # would answer the wrong question -- the launcher can outlive the solve, or
  # exit while it continues -- so a detached run is never judged by it.
  if (isTRUE(rec$detached)) return("unknown")
  host <- as.character(rec$hostname %||% "")
  if (!nzchar(host) || !identical(host, .run_host())) return("unknown")
  pid <- suppressWarnings(as.integer(rec$pid %||% NA))
  if (is.na(pid)) return("unknown")   # a record written before pids were kept
  if (!requireNamespace("ps", quietly = TRUE)) return("unknown")
  h <- tryCatch(ps::ps_handle(pid), error = function(e) NULL)
  if (is.null(h)) return("dead")
  want <- .run_stamp_num(rec$pid_started)
  # A stamp that cannot be read is not evidence either way: the pid exists,
  # but whether it is still the SAME process is exactly what the stamp was
  # for. `unknown` refuses, and says which part was missing.
  if (is.na(want)) {
    return(if (nzchar(as.character(rec$pid_started %||% ""))) "unknown"
           else "alive")
  }
  got <- .run_stamp_num(.run_stamp_fmt(
    tryCatch(ps::ps_create_time(h), error = function(e) NULL)))
  if (is.na(got)) return("alive")
  # both sides are truncated to whole seconds, so allow two
  if (abs(got - want) < 2) "alive" else "dead"
}

# Why a pid state came out `unknown`, for a message that can be acted on.
.run_unknown_reason <- function(rec) {
  if (isTRUE(rec$detached)) {
    return(paste0("it was launched detached (wait = FALSE), so the solver is ",
                  "a separate process this session cannot see"))
  }
  host <- as.character(rec$hostname %||% "")
  if (nzchar(host) && !identical(host, .run_host())) {
    return(paste0("it is recorded on ", host,
                  ", so the owning process cannot be checked from here"))
  }
  if (is.na(suppressWarnings(as.integer(rec$pid %||% NA)))) {
    return("the record carries no process id (it predates this check)")
  }
  if (nzchar(as.character(rec$pid_started %||% "")) &&
      is.na(.run_stamp_num(rec$pid_started))) {
    return(paste0("its process start time ('", rec$pid_started,
                  "') cannot be read, so the process id cannot be trusted"))
  }
  "the `ps` package is not installed, so processes cannot be inspected"
}

# The state a run is IN, as against the status its record last wrote. A
# `running` record whose process is gone, having produced no solution, is a
# crash -- the one state the record can never write for itself.
.get_run_state <- function(rec, has_solution = FALSE) {
  st <- as.character(rec$status %||% "unknown")
  if (!identical(st, "running")) return(st)
  switch(.get_pid_state(rec),
         alive = "running",
         unknown = "running",
         dead = if (isTRUE(has_solution)) "finished" else "crashed")
}

.run_elapsed <- function(rec) {
  t0 <- tryCatch(as.POSIXct(rec$started, format = "%Y-%m-%dT%H:%M:%S",
                            tz = "UTC"), error = function(e) NA)
  if (is.na(t0)) return("")
  d <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
  if (d < 60) sprintf("%dm", as.integer(round(d)))
  else sprintf("%dh%02dm", as.integer(d) %/% 60L, as.integer(d) %% 60L)
}

# A run whose solver has finished writing, judged by the reader's own probe
# file. This matters most for a DETACHED run: the process that would close the
# record is the R session, while the thing still running is the solver, so the
# record can stay `running` for ever. A complete output/ is the fact that
# settles it.
.run_output_complete <- function(run_dir) {
  file.exists(fp(.run_solver_dir(run_dir), "output", "variable_list.csv"))
}

# The record of one run of a scenario, or NULL. Reads the yaml directly rather
# than going through scenario_run_info(), which errors when the run is absent.
.run_record_of <- function(scen, run) {
  id <- tryCatch(.parse_run_id(run, scen), error = function(e) NULL)
  if (is.null(id)) return(NULL)
  ry <- fp(.run_dir(scen, id$variant, id$solve), "run.yml")
  if (!file.exists(ry)) return(NULL)
  tryCatch(yaml::read_yaml(ry), error = function(e) NULL)
}

# Refuse an operation that would disturb a run still in flight.
#
# `unknown` refuses as firmly as `alive`. The alternative -- proceeding when
# the pid state cannot be established -- is what the permissive behaviour was,
# and it is unrecoverable: a half-written output/ read as a solution, or a
# live solve's working files deleted.
.run_busy_guard <- function(scen, run, what, force = FALSE) {
  if (isTRUE(force)) return(invisible(FALSE))
  rec <- .run_record_of(scen, run)
  if (is.null(rec)) return(invisible(FALSE))
  id <- tryCatch(.parse_run_id(run, scen), error = function(e) NULL)
  if (!is.null(id) &&
      .run_output_complete(.run_dir(scen, id$variant, id$solve))) {
    return(invisible(FALSE))   # the solver finished; this is collectable
  }
  live <- .get_pid_state(rec)
  if (!live %in% c("alive", "unknown")) return(invisible(FALSE))
  who <- paste0(rec$hostname %||% "?", "/", rec$user %||% "?")
  why <- if (identical(live, "unknown")) {
    paste0("; ", .run_unknown_reason(rec))
  } else {
    ""
  }
  stop(what, ": run '", run, "' of scenario '", scen@name,
       "' is still running on ", who, ", started ", rec$started %||% "?",
       " (", .run_elapsed(rec), " ago)", why,
       ".\n  Wait for it, or clear_run(<scenario>, \"", run,
       "\") if you know it is gone.", call. = FALSE)
}

#' Release a run that is marked running
#'
#' @description
#' A run whose process died without closing its own record stays marked
#' `running`, and that mark refuses reads, re-solves, drops and output seals.
#' `clear_run()` is the override: it records the run as `abandoned` so those
#' operations proceed again.
#'
#' @details
#' Nothing is deleted and no solver output is touched — only the record's
#' `status` changes. `get_status()` reports a run whose process is demonstrably
#' gone as `crashed` without any help, so this is needed for the cases that
#' cannot be demonstrated: a run recorded on another machine, or one written
#' before process ids were kept.
#'
#' To remove the run and its files instead, use [drop_scenario_run()].
#'
#' @param object a scenario, or the PATH to a saved scenario directory. A
#'   path is what the cases this function exists for can offer: a store
#'   written on another machine, or one in an on-disk layout this version no
#'   longer loads. Only the directory is read.
#' @param run the run id, as [scenario_runs()] lists it.
#' @param verbose logical.
#' @return The run record, invisibly.
#' @seealso [scenario_runs()], [get_status()], [drop_scenario_run()]
#' @export
clear_run <- function(object, run, verbose = isVerbose()) {
  object <- .as_run_locator(object)
  id <- .parse_run_id(run, object)
  ry <- fp(.run_dir(object, id$variant, id$solve), "run.yml")
  if (!file.exists(ry)) {
    stop("No run record at '", ry, "'.\n  Available runs:\n",
         .run_list_hint(object), call. = FALSE)
  }
  rec <- yaml::read_yaml(ry)
  if (!identical(as.character(rec$status %||% ""), "running")) {
    if (isTRUE(verbose)) {
      .say("Run '", run, "' is not marked running (status: ",
              rec$status %||% "unknown", "); nothing to clear.")
    }
    return(invisible(rec))
  }
  live <- .get_pid_state(rec)
  if (identical(live, "alive")) {
    warning("Run '", run, "' has a LIVE process (pid ", rec$pid,
            " on this host). Clearing its record does not stop the solver, ",
            "and the solver will still write its own closing record.",
            call. = FALSE)
  }
  rec$status <- "abandoned"
  rec$abandoned_at <- .registry_now()
  rec$updated <- .registry_now()
  yaml::write_yaml(rec, ry)
  # the record is the source of truth; the registry is its cache, and a
  # listing that still says `running` is the whole reason this verb was needed
  .registry_sync_run(dirname(ry), object@name)
  if (isTRUE(verbose)) {
    .say("Run '", run, "' of scenario '", object@name,
            "' marked abandoned (was running on ", rec$hostname %||% "?", ").")
  }
  invisible(rec)
}

# A run is addressed by its DIRECTORY, so `clear_run()` needs a path and not a
# working scenario: it rewrites one `run.yml` and reads nothing else, and
# `.parse_run_id()` and `.run_dir()` look only at `@path`. Requiring a loadable
# scenario shut the verb out of exactly the cases it exists for -- a store
# recorded on another machine, or one in a layout this version no longer reads
# -- so a path is carried in a bare scenario, which those helpers accept.
#' @noRd
.as_run_locator <- function(object) {
  if (is(object, "scenario")) return(object)
  if (!is.character(object) || length(object) != 1L || !nzchar(object)) {
    stop("clear_run(): `object` must be a scenario, or the path to a saved ",
         "scenario directory.", call. = FALSE)
  }
  if (!dir.exists(object)) {
    stop("clear_run(): no directory at '", object, "'.", call. = FALSE)
  }
  s <- new("scenario")
  s@path <- gsub("[\\/]+", "/", object)
  # the name the registry knows the folder by -- its manifest's, else the
  # folder's, which is the rule `refresh_registry()` applies
  # a store this verb is for may well have no manifest at all, so its absence
  # is the normal case and not a condition to report
  manifest <- fp(s@path, "scenario.yml")
  mf <- if (file.exists(manifest)) {
    tryCatch(yaml::read_yaml(manifest), error = function(e) NULL)
  } else {
    NULL
  }
  s@name <- as.character(mf$name %||% basename(s@path))
  s
}

# Every run.yml under a stored scenario. Both layouts .run_dirs() knows are
# covered -- runs/<solve>/ and runs/<variant>/<solve>/ -- and this is the one
# walker, so a path-based caller and get_status() cannot disagree about which
# runs exist.
.run_yaml_paths <- function(path) {
  root <- fp(path, "runs")
  if (!dir.exists(root)) return(character(0))
  out <- character(0)
  for (d1 in list.dirs(root, recursive = FALSE)) {
    if (file.exists(fp(d1, "run.yml"))) {
      out <- c(out, fp(d1, "run.yml"))
      next
    }
    for (d2 in list.dirs(d1, recursive = FALSE)) {
      if (file.exists(fp(d2, "run.yml"))) out <- c(out, fp(d2, "run.yml"))
    }
  }
  out
}

# The runs of a stored scenario that are still being written, by label.
.runs_in_flight <- function(path) {
  out <- character(0)
  for (f in .run_yaml_paths(path)) {
    rec <- tryCatch(yaml::read_yaml(f), error = function(e) NULL)
    if (.run_output_complete(dirname(f))) next
    if (.get_pid_state(rec) %in% c("alive", "unknown")) {
      out <- c(out, as.character(rec$label %||% basename(dirname(f))))
    }
  }
  unique(out)
}
