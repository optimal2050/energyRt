# =============================================================================#
# runs.R — per-run folders and provenance (storage layout 3).
#
# A scenario's solve artifacts live under `runs/<variant>/<solve>/`:
#   variant — WHICH problem was solved. The base variant (folder named by the
#             manifest's `default_variant`, "default" unless overridden) uses
#             the scenario-level modInp; own-problem variants (stage S5) carry
#             their own modInp store and a variant.yml.
#   solve   — HOW it was solved: one backend/solver/options combination.
#             Owns `run.yml` (provenance), `script/` (the solver working dir,
#             with the solver's own `script/output/` untouched), and `modOut/`
#             (the curated parquet store for the run active at save time).
# The run identifier accepted by user-facing functions is either the solve
# label alone (resolved against the default variant) or "<variant>/<solve>".
# =============================================================================#

.RUN_DEFAULT_VARIANT <- "default"

# active variant of the in-memory scenario ("default" unless set by S5 tooling)
.run_variant <- function(scen) {
  v <- scen@misc$variant
  if (is.character(v) && length(v) == 1L && nzchar(v)) v else .RUN_DEFAULT_VARIANT
}

.run_dir <- function(scen, variant, solve) {
  gsub("[\\/]+", "/", fp(scen@path, "runs", variant, solve))
}

# "glpk" -> (default variant, "glpk"); "cal-d24/glpk" -> ("cal-d24", "glpk")
.parse_run_id <- function(run, scen) {
  stopifnot(is.character(run), length(run) == 1L, nzchar(run))
  parts <- strsplit(run, "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 1L) {
    list(variant = .run_variant(scen), solve = parts[1])
  } else if (length(parts) == 2L) {
    list(variant = parts[1], solve = parts[2])
  } else {
    stop("Invalid run identifier '", run,
         "': use '<solve>' or '<variant>/<solve>'")
  }
}

# on-disk run directories: tibble(variant, solve, dir, has_record)
.run_dirs <- function(scen) {
  root <- fp(scen@path, "runs")
  out <- tibble(variant = character(0), solve = character(0),
                dir = character(0), has_record = logical(0))
  if (!dir.exists(root)) return(out)
  for (vd in list.dirs(root, recursive = FALSE)) {
    for (sd in list.dirs(vd, recursive = FALSE)) {
      out <- bind_rows(out, tibble(
        variant = basename(vd), solve = basename(sd),
        dir = gsub("[\\/]+", "/", sd),
        has_record = file.exists(fp(sd, "run.yml"))
      ))
    }
  }
  out
}

# Write the run record at solve start. Warns (does not lock) when a fresh
# `running` record from another session is found in the same run dir.
.run_record_start <- function(scen, arg) {
  run_dir <- arg$run.dir
  if (is_empty(run_dir)) return(invisible(NULL))
  ry <- fp(run_dir, "run.yml")
  if (file.exists(ry)) {
    prev <- tryCatch(yaml::read_yaml(ry), error = function(e) NULL)
    if (!is.null(prev) && identical(prev$status, "running")) {
      started <- tryCatch(
        as.POSIXct(prev$started, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
        error = function(e) NA)
      fresh <- !is.na(started) &&
        difftime(Sys.time(), started, units = "hours") < 24
      if (fresh) {
        warning("Run '", basename(run_dir), "' has a 'running' record from ",
                prev$hostname %||% "?", "/", prev$user %||% "?",
                " started ", prev$started,
                ". A concurrent solve of the same run overwrites it.",
                call. = FALSE)
      }
    }
  }
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  solver <- scen@settings@solver
  rec <- list(
    label = arg$run.label %||% basename(run_dir),
    variant = arg$run.variant %||% .RUN_DEFAULT_VARIANT,
    scenario = scen@name,
    solver_name = solver$name %||% "",
    lang = solver$lang %||% "",
    solver = solver$solver %||% "",
    cmdline = paste(solver$cmdline %||% "", collapse = " "),
    started = .registry_now(),
    status = "running",
    modinp = "shared",
    energyRt_version = as.character(utils::packageVersion("energyRt")),
    hostname = unname(Sys.info()["nodename"]),
    user = unname(Sys.info()["user"])
  )
  yaml::write_yaml(rec, ry)
  invisible(ry)
}

# Finalize the run record after the solve (or on failure/interrupt).
.run_record_finish <- function(scen, arg, status) {
  run_dir <- arg$run.dir
  if (is_empty(run_dir)) return(invisible(NULL))
  ry <- fp(run_dir, "run.yml")
  rec <- if (file.exists(ry)) {
    tryCatch(yaml::read_yaml(ry), error = function(e) list())
  } else {
    list()
  }
  rec$finished <- .registry_now()
  started <- tryCatch(
    as.POSIXct(rec$started, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    error = function(e) NA)
  if (!is.na(started)) {
    rec$duration_sec <- round(as.numeric(
      difftime(Sys.time(), started, units = "secs")), 1)
  }
  rec$status <- status
  rec$stage <- tryCatch(
    if (length(scen@modOut@stage)) scen@modOut@stage[1] else "",
    error = function(e) "")
  obj <- tryCatch({
    v <- get_variable(scen, "vObjective", data = TRUE)
    if (is.data.frame(v) && nrow(v) && "value" %in% names(v)) {
      as.numeric(v$value[1])
    } else NA_real_
  }, error = function(e) NA_real_)
  rec$objective <- obj
  yaml::write_yaml(rec, ry)
  invisible(ry)
}

#' List, inspect, and drop a scenario's runs
#'
#' @description
#' A solved scenario's runs live under `<scenario>/runs/<variant>/<solve>/`,
#' each with a `run.yml` provenance record (see `solve_scen()`).
#'
#' * `scenario_runs()` lists them as a tibble — one row per run, including
#'   legacy `script/<solver>/` directories from older layouts
#'   (`status = "legacy"`).
#' * `scenario_run_info()` returns one run's full `run.yml` record as a list.
#' * `scenario_drop_run()` deletes a run directory. The active run (the one
#'   currently loaded in the scenario object) is refused unless `force = TRUE`.
#'
#' @param scen a scenario object with a non-empty `@path`.
#' @param run character, run identifier: a solve label (resolved against the
#'   default variant), or `"<variant>/<solve>"`.
#' @param force logical, allow dropping the active run.
#'
#' @return `scenario_runs()` a tibble (variant, solve, status, solver_name,
#'   lang, started, duration_sec, objective, modinp, active);
#'   `scenario_run_info()` a named list; `scenario_drop_run()` the dropped
#'   directory, invisibly.
#'
#' @rdname scenario_runs
#' @export
scenario_runs <- function(scen) {
  stopifnot(is(scen, "scenario"))
  active_variant <- .run_variant(scen)
  active_solve <- scen@misc$run %||% ""
  rows <- list()

  rd <- .run_dirs(scen)
  for (i in seq_len(nrow(rd))) {
    rec <- if (rd$has_record[i]) {
      tryCatch(yaml::read_yaml(fp(rd$dir[i], "run.yml")),
               error = function(e) list())
    } else {
      list()
    }
    rows[[length(rows) + 1L]] <- tibble(
      variant = rd$variant[i],
      solve = rd$solve[i],
      status = rec$status %||% "unknown",
      solver_name = rec$solver_name %||% "",
      lang = rec$lang %||% "",
      started = rec$started %||% "",
      duration_sec = as.numeric(rec$duration_sec %||% NA_real_),
      objective = as.numeric(rec$objective %||% NA_real_),
      modinp = rec$modinp %||% "shared",
      active = rd$variant[i] == active_variant && rd$solve[i] == active_solve
    )
  }

  # legacy layout: <scen>/script/<solver>/ (a `solver` csv marks a real run;
  # timestamp dirs from tmp.del runs are listed too, fields empty)
  legacy_root <- fp(scen@path, "script")
  if (dir.exists(legacy_root)) {
    for (sd in list.dirs(legacy_root, recursive = FALSE)) {
      solver_name <- ""
      sf <- fp(sd, "solver")
      if (file.exists(sf)) {
        sv <- tryCatch(utils::read.csv(sf, stringsAsFactors = FALSE),
                       error = function(e) NULL)
        if (!is.null(sv) && all(c("name", "value") %in% names(sv))) {
          solver_name <- sv$value[sv$name == "name"][1] %||% ""
        }
      }
      rows[[length(rows) + 1L]] <- tibble(
        variant = "", solve = basename(sd), status = "legacy",
        solver_name = solver_name %||% "", lang = "", started = "",
        duration_sec = NA_real_, objective = NA_real_, modinp = "shared",
        active = identical(
          gsub("[\\/]+", "/", sd),
          gsub("[\\/]+", "/", scen@misc$tmp.dir %||% ""))
      )
    }
  }

  if (!length(rows)) {
    return(tibble(
      variant = character(0), solve = character(0), status = character(0),
      solver_name = character(0), lang = character(0), started = character(0),
      duration_sec = numeric(0), objective = numeric(0),
      modinp = character(0), active = logical(0)
    ))
  }
  bind_rows(rows)
}

#' @rdname scenario_runs
#' @export
scenario_run_info <- function(scen, run) {
  stopifnot(is(scen, "scenario"))
  id <- .parse_run_id(run, scen)
  ry <- fp(.run_dir(scen, id$variant, id$solve), "run.yml")
  if (!file.exists(ry)) {
    stop("No run record at '", ry, "'.\n  Available runs:\n",
         .run_list_hint(scen))
  }
  yaml::read_yaml(ry)
}

#' @rdname scenario_runs
#' @export
scenario_drop_run <- function(scen, run, force = FALSE) {
  stopifnot(is(scen, "scenario"))
  id <- .parse_run_id(run, scen)
  run_dir <- .run_dir(scen, id$variant, id$solve)
  if (!dir.exists(run_dir)) {
    stop("Run directory '", run_dir, "' does not exist.\n  Available runs:\n",
         .run_list_hint(scen))
  }
  is_active <- identical(id$variant, .run_variant(scen)) &&
    identical(id$solve, scen@misc$run %||% "")
  if (is_active && !force) {
    stop("Run '", id$variant, "/", id$solve, "' is the scenario's active ",
         "run. Use force = TRUE to drop it anyway.")
  }
  if (unlink(run_dir, recursive = TRUE, force = TRUE) != 0) {
    stop("Could not delete '", run_dir, "'")
  }
  message("Dropped run '", id$variant, "/", id$solve, "'")
  invisible(run_dir)
}

.run_list_hint <- function(scen) {
  rd <- .run_dirs(scen)
  if (!nrow(rd)) return("    (none)")
  paste0("    ", rd$variant, "/", rd$solve, collapse = "\n")
}

# ---- scenario manifest (scenario.yml) + registry hook -----------------------#

# Written by save_scenario(). Minimal, stable field set. `model` is the
# manifest's model block: list(name, hash, source = "embedded"|"ref"
# [, path]), built by save_scenario().
.write_scenario_manifest <- function(scen, format, model = NULL) {
  mf_path <- fp(scen@path, "scenario.yml")
  prev <- if (file.exists(mf_path)) {
    tryCatch(yaml::read_yaml(mf_path), error = function(e) NULL)
  } else {
    NULL
  }
  now <- .registry_now()
  mf <- list(
    layout = .SCENARIO_LAYOUT,
    class = "scenario",
    name = scen@name,
    created = prev$created %||% now,
    updated = now,
    energyRt_version = as.character(utils::packageVersion("energyRt")),
    format = format,
    calendar = tryCatch(scen@settings@calendar@name, error = function(e) ""),
    horizon = tryCatch(scen@settings@horizon@name, error = function(e) ""),
    default_variant = .RUN_DEFAULT_VARIANT,
    active = list(
      variant = .run_variant(scen),
      solve = scen@misc$run %||% ""
    )
  )
  mf$model <- model %||% prev$model
  yaml::write_yaml(mf, mf_path)
  invisible(mf_path)
}

# Add/refresh this scenario's registry rows (scenario + its recorded runs).
# Registry trouble must never fail a save: warn and continue.
.registry_record_scenario <- function(scen) {
  tryCatch({
    reg <- registry_load()
    rel <- .registry_rel_path(scen@path)
    reg <- registry_add(reg, "scenario", scen@name, path = rel)
    rd <- .run_dirs(scen)
    for (i in which(rd$has_record)) {
      run_name <- fp(rd$variant[i], rd$solve[i])
      reg <- registry_add(reg, "run", run_name,
                          path = fp(rel, "runs", run_name),
                          parent = scen@name)
    }
    registry_save(reg)
  }, error = function(e) {
    warning("Could not update the project registry (",
            conditionMessage(e), ")", call. = FALSE)
  })
  invisible(NULL)
}
