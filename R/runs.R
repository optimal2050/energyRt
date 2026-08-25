# =============================================================================#
# runs.R — per-run folders and provenance (storage layout 3).
#
# A scenario's solve artifacts live under `runs/`:
#   runs/<solve>/            base-problem run: solved on the scenario-level
#                            modInp; the folder name is the solve label
#                            (solver dir name like "glpk"/"julia_highs", or a
#                            user label). Owns `run.yml` (provenance),
#                            `solver/` (the solver working directory, with the
#                            solver's own `solver/output/` untouched), and
#                            `modOut/` (the curated parquet solution store).
#   runs/<variant>/<solve>/  own-problem variant (stage S5): the variant
#                            folder carries `variant.yml` + its own modInp
#                            store, its children are its solves.
# Folder kind is self-describing: a dir with `run.yml` is a solve, a dir
# holding solve dirs (and, from S5, a `variant.yml`) is a variant. There is no
# "default" folder — the scenario manifest's `default:` field names the
# default run, and the user may change it.
# The run identifier is `"<solve>"` (base) or `"<variant>/<solve>"`.
# =============================================================================#

# active variant of the in-memory scenario; "" = base problem
.run_variant <- function(scen) {
  v <- scen@misc$variant
  if (is.character(v) && length(v) == 1L && nzchar(v)) v else ""
}

# run identifier from its parts: "glpk" or "cal-d24/glpk"
.run_id <- function(variant, solve) {
  if (nzchar(variant %||% "")) fp(variant, solve) else solve
}

.run_dir <- function(scen, variant, solve) {
  d <- if (nzchar(variant %||% "")) {
    fp(scen@path, "runs", variant, solve)
  } else {
    fp(scen@path, "runs", solve)
  }
  gsub("[\\/]+", "/", d)
}

# The solver working directory of a run. Named `solver/`; falls back to the
# interim S2/S3-era name `script/` when only that exists (upgraded by
# upgrade_scenario_layout()).
.run_solver_dir <- function(run_dir) {
  d <- fp(run_dir, "solver")
  if (!dir.exists(d) && dir.exists(fp(run_dir, "script"))) {
    return(gsub("[\\/]+", "/", fp(run_dir, "script")))
  }
  gsub("[\\/]+", "/", d)
}

# "glpk" -> (base, "glpk"); "cal-d24/glpk" -> ("cal-d24", "glpk")
.parse_run_id <- function(run, scen) {
  stopifnot(is.character(run), length(run) == 1L, nzchar(run))
  parts <- strsplit(run, "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) == 1L) {
    list(variant = "", solve = parts[1])
  } else if (length(parts) == 2L) {
    list(variant = parts[1], solve = parts[2])
  } else {
    stop("Invalid run identifier '", run,
         "': use '<solve>' or '<variant>/<solve>'")
  }
}

# on-disk run directories: tibble(variant, solve, dir, has_record).
# A first-level dir with a run.yml is a base solve; one without (but with
# children carrying run.yml) is a variant — this also reads interim trees
# that still have the retired `default/` wrapper as a variant of that name.
.run_dirs <- function(scen) {
  root <- fp(scen@path, "runs")
  out <- tibble(variant = character(0), solve = character(0),
                dir = character(0), has_record = logical(0))
  if (!dir.exists(root)) return(out)
  for (d1 in list.dirs(root, recursive = FALSE)) {
    if (file.exists(fp(d1, "run.yml"))) {
      out <- bind_rows(out, tibble(
        variant = "", solve = basename(d1),
        dir = gsub("[\\/]+", "/", d1), has_record = TRUE
      ))
      next
    }
    children <- list.dirs(d1, recursive = FALSE)
    child_rec <- file.exists(fp(children, "run.yml"))
    if (any(child_rec) || file.exists(fp(d1, "variant.yml"))) {
      for (sd in children[child_rec]) {
        out <- bind_rows(out, tibble(
          variant = basename(d1), solve = basename(sd),
          dir = gsub("[\\/]+", "/", sd), has_record = TRUE
        ))
      }
    } else if (length(children) == 0L &&
               dir.exists(.run_solver_dir(d1))) {
      # a solve dir whose run.yml is missing (crash before the record):
      # still list it so the user can see and drop it
      out <- bind_rows(out, tibble(
        variant = "", solve = basename(d1),
        dir = gsub("[\\/]+", "/", d1), has_record = FALSE
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
    variant = arg$run.variant %||% "",
    scenario = scen@name,
    solver_name = solver$name %||% "",
    lang = solver$lang %||% "",
    solver = solver$solver %||% "",
    cmdline = paste(solver$cmdline %||% "", collapse = " "),
    started = .registry_now(),
    status = "running",
    modinp = if (nzchar(arg$run.variant %||% "")) "own" else "shared",
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
#' A solved scenario's runs live under `<scenario>/runs/` — base-problem runs
#' directly (`runs/glpk/`), own-problem variants as named folders holding
#' their solves (`runs/cal-d24/glpk/`) — each with a `run.yml` provenance
#' record (see `solve_scenario()`). The scenario manifest's `default:` field
#' names the default run.
#'
#' * `scenario_runs()` lists them as a tibble — one row per run, including
#'   legacy `script/<solver>/` directories from older layouts
#'   (`status = "legacy"`).
#' * `scenario_run_info()` returns one run's full `run.yml` record as a list.
#' * `drop_scenario_run()` deletes a run directory. The active run (the one
#'   currently loaded in the scenario object) is refused unless `force = TRUE`.
#'
#' @param scen a scenario object with a non-empty `@path`.
#' @param run character, run identifier: `"<solve>"` for a base-problem run
#'   or `"<variant>/<solve>"`.
#' @param force logical, allow dropping the active run.
#'
#' @return `scenario_runs()` a tibble (run, variant, solve, status,
#'   solver_name, lang, started, duration_sec, objective, modinp, active);
#'   `scenario_run_info()` a named list; `drop_scenario_run()` the dropped
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
      run = .run_id(rd$variant[i], rd$solve[i]),
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
  # timestamp dirs from transient runs are listed too, fields empty)
  legacy_root <- fp(scen@path, "script")
  if (dir.exists(legacy_root)) {
    active_dir <- gsub("[\\/]+", "/",
                       scen@misc$solver.dir %||% scen@misc$tmp.dir %||% "")
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
        run = basename(sd), variant = "", solve = basename(sd),
        status = "legacy",
        solver_name = solver_name %||% "", lang = "", started = "",
        duration_sec = NA_real_, objective = NA_real_, modinp = "shared",
        active = identical(gsub("[\\/]+", "/", sd), active_dir)
      )
    }
  }

  if (!length(rows)) {
    return(tibble(
      run = character(0), variant = character(0), solve = character(0),
      status = character(0), solver_name = character(0), lang = character(0),
      started = character(0), duration_sec = numeric(0),
      objective = numeric(0), modinp = character(0), active = logical(0)
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
drop_scenario_run <- function(scen, run, force = FALSE) {
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
    stop("Run '", .run_id(id$variant, id$solve), "' is the scenario's ",
         "active run. Use force = TRUE to drop it anyway.")
  }
  if (unlink(run_dir, recursive = TRUE, force = TRUE) != 0) {
    stop("Could not delete '", run_dir, "'")
  }
  message("Dropped run '", .run_id(id$variant, id$solve), "'")
  invisible(run_dir)
}

.run_list_hint <- function(scen) {
  rd <- .run_dirs(scen)
  if (!nrow(rd)) return("    (none)")
  paste0("    ", mapply(.run_id, rd$variant, rd$solve), collapse = "\n")
}

# ---- own-problem variants (S5) ----------------------------------------------#
# An own-problem variant carries its own interpolated problem:
#   runs/<variant>/variant.yml    manifest (type, calendar, horizon, ...)
#   runs/<variant>/modInp/        its parameter store (written once, shared by
#                                 every solve of the variant)
#   runs/<variant>/variant.RData  swap data: the thinned modInp skeleton + the
#                                 settings snapshot
# The BASE problem gets the symmetric swap file <scen>/problem.RData at save
# time, so read_solution(run=) can switch the in-memory problem in both
# directions without re-loading the scenario shell.

# Drop settings@sourceCode blocks identical to the package templates (the
# save_scenario dedup, reused for swap snapshots). Returns the thinned
# settings + the dropped keys.
.settings_thin <- function(settings) {
  sc <- settings@sourceCode
  dropped <- character(0)
  if (length(sc)) {
    same <- vapply(names(sc),
                   function(k) identical(sc[[k]], .modelCode[[k]]),
                   logical(1))
    dropped <- names(sc)[same]
    if (length(dropped)) settings@sourceCode[dropped] <- NULL
  }
  list(settings = settings, dropped = dropped)
}

.settings_restore <- function(settings, dropped) {
  for (k in dropped) {
    if (is.null(settings@sourceCode[[k]])) {
      settings@sourceCode[[k]] <- .modelCode[[k]]
    }
  }
  settings
}

# Rebase a thinned modInp (parameter store paths) onto its store root.
.modinp_rebase <- function(mi, mi_root) {
  mi_root <- gsub("[\\/]+", "/", mi_root)
  if (length(get_ondisk_slots(mi))) mi@misc$path <- mi_root
  for (nm in names(mi@parameters)) {
    p <- mi@parameters[[nm]]
    if (isS4(p) && length(get_ondisk_slots(p))) {
      p@misc$path <- fp(mi_root, "parameters", nm)
      mi@parameters[[nm]] <- p
    }
  }
  mi
}

# The modInp store root of the ACTIVE problem: the variant's own store for an
# own-problem variant, the scenario-level store for the base problem.
.problem_modinp_root <- function(scen, root = scen@path) {
  v <- .run_variant(scen)
  if (nzchar(v)) fp(root, "runs", v, "modInp") else fp(root, "modInp")
}

# variant.yml, written by save_scenario() for an own-problem variant. Extra
# fields written by drivers (type, sequence, step, sample) are preserved.
.write_variant_manifest <- function(scen, vdir) {
  mf_path <- fp(vdir, "variant.yml")
  prev <- if (file.exists(mf_path)) {
    tryCatch(yaml::read_yaml(mf_path), error = function(e) NULL)
  } else {
    NULL
  }
  now <- .registry_now()
  mf <- list(
    layout = .SCENARIO_LAYOUT,
    class = "variant",
    name = .run_variant(scen),
    scenario = scen@name,
    type = prev$type %||% "custom",
    calendar = tryCatch(scen@settings@calendar@name, error = function(e) ""),
    horizon = tryCatch(scen@settings@horizon@name, error = function(e) ""),
    created = prev$created %||% now,
    updated = now,
    energyRt_version = as.character(utils::packageVersion("energyRt")),
    modinp = "own"
  )
  for (k in setdiff(names(prev), names(mf))) mf[[k]] <- prev[[k]]
  yaml::write_yaml(mf, mf_path)
  invisible(mf_path)
}

# Swap the in-memory problem (settings + modInp) to `variant` ("" = base),
# reading the swap file written at save time. Errors when the target problem
# was never saved.
.variant_swap <- function(scen, variant) {
  if (identical(variant, .run_variant(scen))) return(scen)
  swap_file <- if (nzchar(variant)) {
    fp(scen@path, "runs", variant, "variant.RData")
  } else {
    fp(scen@path, "problem.RData")
  }
  if (!file.exists(swap_file)) {
    stop("The ", if (nzchar(variant)) paste0("variant '", variant, "'") else
           "base problem", " of scenario '", scen@name, "' has no saved ",
         "problem data (", swap_file, ").\n",
         "  Save the scenario while that problem is active ",
         "(save_scenario()), then switch.")
  }
  e <- new.env(parent = emptyenv())
  nm <- load(swap_file, envir = e)
  vdata <- get(nm[1], envir = e)
  stopifnot(is.list(vdata), !is.null(vdata$modInp), !is.null(vdata$settings))
  scen@settings <- .settings_restore(vdata$settings,
                                     vdata$sourceCode_default %||%
                                       character(0))
  scen@misc$variant <- if (nzchar(variant)) variant else NULL
  scen@modInp <- .modinp_rebase(vdata$modInp,
                                .problem_modinp_root(scen))
  scen
}

# Write the swap file for the ACTIVE problem (called by save_scenario after
# the modInp store is on disk; `mi` is the thinned modInp).
.write_problem_swap <- function(scen, mi) {
  v <- .run_variant(scen)
  thin <- .settings_thin(scen@settings)
  # save_scenario may already have thinned the sourceCode before this runs —
  # the swap must remember EVERY dropped block to restore on switch
  dropped <- union(thin$dropped,
                   scen@misc$sourceCode_default %||% character(0))
  vdata <- list(settings = thin$settings,
                sourceCode_default = dropped,
                modInp = mi)
  if (nzchar(v)) {
    vdir <- fp(scen@path, "runs", v)
    dir.create(vdir, recursive = TRUE, showWarnings = FALSE)
    save(vdata, file = fp(vdir, "variant.RData"))
    .write_variant_manifest(scen, vdir)
  } else {
    save(vdata, file = fp(scen@path, "problem.RData"))
  }
  invisible(NULL)
}

# ---- scenario manifest (scenario.yml) + registry hook -----------------------#

# Written by save_scenario(). Minimal, stable field set. `default:` names the
# scenario's default run (a run id, user-changeable in the file); a save sets
# it to the active run. `model` is the manifest's model block:
# list(name, hash, source = "embedded"|"ref" [, path]), built by
# save_scenario().
.write_scenario_manifest <- function(scen, format, model = NULL,
                                     datasets = NULL) {
  mf_path <- fp(scen@path, "scenario.yml")
  prev <- if (file.exists(mf_path)) {
    tryCatch(yaml::read_yaml(mf_path), error = function(e) NULL)
  } else {
    NULL
  }
  now <- .registry_now()
  active <- if (nzchar(scen@misc$run %||% "")) {
    .run_id(.run_variant(scen), scen@misc$run)
  } else {
    ""
  }
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
    default = if (nzchar(active)) active else prev$default %||% ""
  )
  mf$model <- model %||% prev$model
  mf$datasets <- datasets %||% prev$datasets
  yaml::write_yaml(mf, mf_path)
  invisible(mf_path)
}

# The manifest's default run id, parsed; NULL when absent/empty.
.scenario_default_run <- function(scen) {
  mf_path <- fp(scen@path, "scenario.yml")
  if (!file.exists(mf_path)) return(NULL)
  mf <- tryCatch(yaml::read_yaml(mf_path), error = function(e) NULL)
  d <- mf$default %||% ""
  if (!nzchar(d)) return(NULL)
  tryCatch(.parse_run_id(d, scen), error = function(e) NULL)
}

# Add/refresh this scenario's registry rows (scenario + its recorded runs).
# Registry trouble must never fail a save: warn and continue.
.registry_record_scenario <- function(scen) {
  tryCatch({
    reg <- load_registry()
    rel <- .registry_rel_path(scen@path)
    reg <- add_to_registry(reg, "scenario", scen@name, path = rel)
    rd <- .run_dirs(scen)
    for (i in which(rd$has_record)) {
      run_name <- .run_id(rd$variant[i], rd$solve[i])
      reg <- add_to_registry(reg, "run", run_name,
                          path = fp(rel, "runs", run_name),
                          parent = scen@name)
    }
    save_registry(reg)
  }, error = function(e) {
    warning("Could not update the project registry (",
            conditionMessage(e), ")", call. = FALSE)
  })
  invisible(NULL)
}
