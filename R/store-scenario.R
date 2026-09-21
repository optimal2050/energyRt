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

# The solver working directory of a run.
#
# New runs write it FLAT -- straight into the run folder, beside `run.yml` --
# because `runs/<solve>/solver/` restated the solve name for seven characters,
# and the exchange writes one file per symbol under it. On Windows those paths
# reach the 260-character limit, and the solver's own name is part of the run
# folder, so the same model wrote fine under `julia_highs` and failed under
# `julia_highs_barrier`.
#
# Older layouts are still read: `solver/` (layout 3) and the interim S2/S3
# `script/`. An existing scenario therefore opens unchanged.
.run_solver_dir <- function(run_dir) {
  for (sub in c("solver", "script")) {
    d <- fp(run_dir, sub)
    if (dir.exists(d)) return(gsub("[\\/]+", "/", d))
  }
  gsub("[\\/]+", "/", run_dir)
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
    } else if (dir.exists(.run_solver_dir(d1)) &&
               !any(dir.exists(fp(children, "runs")))) {
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
  # a sealed scenario accepts no new recorded runs (see ?seal_scenario)
  .scenario_seal_guard(scen)
  ry <- fp(run_dir, "run.yml")
  prev <- NULL
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
    user = unname(Sys.info()["user"]),
    saved = prev$saved %||% .registry_now(),
    updated = .registry_now()
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
  # resource footprint of the solve (R-heap now + high-water mark; see
  # .en_mem in R/log.R) and record bookkeeping
  m <- .en_mem()
  rec$mem_mb <- m$mem_mb
  rec$peak_mb <- m$peak_mb
  rec$updated <- .registry_now()
  yaml::write_yaml(rec, ry)
  invisible(ry)
}

# Reconcile a run's record after `read_solution()` has read an optimal solution
# for it. `solve_scenario()` writes the record through `.run_record_finish()`,
# but a solution read later -- a re-read of an earlier run, or one solved
# outside energyRt and unpacked into its `output/` -- otherwise leaves the
# record saying `not-optimal` while the returned object says solved.
#
# Deliberately narrower than `.run_record_finish()`: only `status`, `stage` and
# `objective` are written. `duration_sec`, `mem_mb` and `peak_mb` describe the
# run that produced the solution, and recomputing them from the reading session
# would replace real measurements with meaningless ones.
.run_record_read <- function(scen, solver_dir) {
  if (!nzchar(scen@misc$run %||% "")) return(invisible(NULL))
  run_dir <- tryCatch(
    .run_dir(scen, .run_variant(scen), scen@misc$run),
    error = function(e) NULL)
  if (is.null(run_dir) || !dir.exists(run_dir)) return(invisible(NULL))
  # Only the run actually read from: `solver.dir=` can point anywhere, while
  # `scen@misc$run` may still name an unrelated run from an earlier read.
  same <- tryCatch(identical(
    normalizePath(.run_solver_dir(run_dir), winslash = "/", mustWork = FALSE),
    normalizePath(solver_dir, winslash = "/", mustWork = FALSE)),
    error = function(e) FALSE)
  if (!same) return(invisible(NULL))
  ry <- fp(run_dir, "run.yml")
  if (!file.exists(ry)) return(invisible(NULL))
  rec <- tryCatch(yaml::read_yaml(ry), error = function(e) NULL)
  if (is.null(rec)) return(invisible(NULL))
  obj <- tryCatch({
    v <- get_variable(scen, "vObjective", data = TRUE)
    if (is.data.frame(v) && nrow(v) && "value" %in% names(v)) {
      as.numeric(v$value[1])
    } else NA_real_
  }, error = function(e) NA_real_)
  if (identical(rec$status, "solved") &&
      isTRUE(all.equal(rec$objective, obj))) {
    return(invisible(NULL)) # already current; do not churn `updated`
  }
  rec$status <- "solved"
  rec$stage <- "solved"
  if (!is.na(obj)) rec$objective <- obj
  rec$updated <- .registry_now()
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
#'   solver_name, lang, started, duration_sec, peak_mb, updated,
#'   objective, modinp, active);
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
      peak_mb = as.numeric(rec$peak_mb %||% NA_real_),
      updated = rec$updated %||% "",
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
        duration_sec = NA_real_, peak_mb = NA_real_, updated = "",
        objective = NA_real_, modinp = "shared",
        active = identical(gsub("[\\/]+", "/", sd), active_dir)
      )
    }
  }

  if (!length(rows)) {
    return(tibble(
      run = character(0), variant = character(0), solve = character(0),
      status = character(0), solver_name = character(0), lang = character(0),
      started = character(0), duration_sec = numeric(0),
      peak_mb = numeric(0), updated = character(0),
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
  # a sealed scenario keeps its runs (see ?seal_scenario)
  .scenario_seal_guard(scen)
  id <- .parse_run_id(run, scen)
  run_dir <- .run_dir(scen, id$variant, id$solve)
  if (!dir.exists(run_dir)) {
    stop("Run directory '", run_dir, "' does not exist.\n  Available runs:\n",
         .run_list_hint(scen))
  }
  is_active <- identical(id$variant, .run_variant(scen)) &&
    identical(id$solve, scen@misc$run %||% "")
  if (is_active && !force) {
    msg <- paste0("Run '", .run_id(id$variant, id$solve),
                  "' is the scenario's active run. ")
    # The dangerous case, worth saying out loud: the shell points at this
    # run's store, so dropping it leaves every read failing with "On-disk
    # data expected but not found". Promotion is what makes a run droppable.
    if (dir.exists(fp(run_dir, "modOut", "variables")) &&
        !.modout_is_promoted(fp(scen@path, "modOut"))) {
      msg <- paste0(
        msg, "It holds the only copy of the scenario's solution: dropping it ",
        "would leave the scenario pointing at a store that is gone. ",
        "promote_solution(scen) copies it to <scenario>/modOut first, after ",
        "which the run is scratch. ")
    }
    stop(msg, "Use force = TRUE to drop it anyway.")
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
# `<mi_root>/parameters/<name>` is the store convention; stores written by
# interpolate_model(ondisk = TRUE) before the `fmp()` path fix hold (some)
# tables flat at `<mi_root>/<name>` -- keep those readable by falling back to
# the flat location when the canonical one is absent.
.modinp_rebase <- function(mi, mi_root) {
  mi_root <- gsub("[\\/]+", "/", mi_root)
  if (length(get_ondisk_slots(mi))) mi@misc$path <- mi_root
  for (nm in names(mi@parameters)) {
    p <- mi@parameters[[nm]]
    if (isS4(p) && length(get_ondisk_slots(p))) {
      canonical <- fp(mi_root, "parameters", nm)
      p@misc$path <- if (!dir.exists(canonical) &&
                         dir.exists(fp(mi_root, nm))) {
        fp(mi_root, nm)
      } else {
        canonical
      }
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
# The `params:` block of a driver's variant.yml: the call arguments that say
# HOW the method was configured, as opposed to the flat keys, which say WHICH
# unit of work the variant is (step 2, sample s01, region R1_R2).
#
# NULLs are dropped: an argument that was not given and one recorded as null
# are different claims, and yaml writes the latter as an empty key.
.variant_params <- function(...) {
  p <- list(...)
  p <- p[!vapply(p, is.null, logical(1))]
  lapply(p, function(v) {
    if (is.factor(v)) as.character(v)
    else if (isS4(v)) tryCatch(v@name, error = function(e) NULL)
    else v
  })
}

# Which recorded parameters differ, formatted for the refusal below.
# all.equal() rather than identical(): a yaml round-trip turns 1L into 1, and
# refusing over that would make the guard useless.
.params_fmt <- function(v) {
  if (is.null(v) || !length(v)) return("<unset>")
  paste(format(unlist(v)), collapse = ",")
}

.params_diff <- function(old, new) {
  out <- character(0)
  for (k in union(names(old), names(new))) {
    same <- tryCatch(isTRUE(all.equal(old[[k]], new[[k]],
                                      check.attributes = FALSE)),
                     error = function(e) FALSE)
    if (!same) {
      out <- c(out, paste0(k, " (", .params_fmt(old[[k]]), " vs ",
                           .params_fmt(new[[k]]), ")"))
    }
  }
  out
}

# Refuse to write over a variant that a DIFFERENT configuration produced.
#
# Variant labels are unit-derived -- s01, R1_R2, s02-2030 -- so a second run of
# the same driver with another seed, grouping or window writes straight over
# the first sequence's variants, and nothing about the folder says so. The same
# method with the same `params:` is a legitimate redo and passes.
.variant_guard <- function(path, vlab, type, params, overwrite = FALSE) {
  if (isTRUE(overwrite)) return(invisible(TRUE))
  vy <- fp(path, "runs", vlab, "variant.yml")
  if (!file.exists(vy)) return(invisible(TRUE))
  mf <- tryCatch(yaml::read_yaml(vy), error = function(e) NULL)
  if (is.null(mf)) return(invisible(TRUE))
  otype <- as.character(mf$type %||% "")
  d <- .params_diff(mf$params %||% list(), params %||% list())
  if (identical(otype, as.character(type)) && !length(d)) {
    return(invisible(TRUE))
  }
  stop("Variant '", vlab, "' already exists in '", path, "'",
       if (nzchar(mf$sequence %||% "")) {
         paste0(", from sequence '", mf$sequence, "'")
       } else "",
       " (", if (nzchar(otype)) otype else "unknown type", ")",
       if (!identical(otype, as.character(type))) {
         paste0(" -- a different method than ", type)
       } else "",
       if (length(d)) {
         paste0(", with different settings: ",
                paste(utils::head(d, 4), collapse = "; "),
                if (length(d) > 4) ", ..." else "")
       } else "",
       ".
  Solving would replace that sequence's results. Give this run a ",
       "different `name = `, or overwrite = TRUE to replace it.",
       call. = FALSE)
}

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

# Swap the in-memory problem (settings + modInp) to `variant` ("" = base).
# Variants read their runs/<v>/variant.RData cartridge; the BASE problem
# lives in scen.RData itself (its one home — problem.RData is retired; a
# legacy problem.RData is still honored as fallback). Errors when the
# target problem was never saved.
.variant_swap <- function(scen, variant) {
  if (identical(variant, .run_variant(scen))) return(scen)
  vdata <- NULL
  if (nzchar(variant)) {
    swap_file <- fp(scen@path, "runs", variant, "variant.RData")
    if (file.exists(swap_file)) {
      e <- new.env(parent = emptyenv())
      nm <- load(swap_file, envir = e)
      vdata <- get(nm[1], envir = e)
    }
  } else {
    shell_file <- fp(scen@path, "scen.RData")
    if (file.exists(shell_file)) {
      e <- new.env(parent = emptyenv())
      nm <- load(shell_file, envir = e)
      sh <- get(nm[1], envir = e)
      # a LEGACY shell saved while a variant was active (old semantics)
      # carries that variant's problem, not the base's — skip to the
      # problem.RData fallback in that case
      sh_variant <- sh@misc[["variant"]] %||% ""
      if (is(sh, "scenario") && !isFALSE(sh@misc$has_base) &&
          !nzchar(sh_variant) &&
          length(sh@modInp@parameters)) {
        vdata <- list(settings = sh@settings,
                      sourceCode_default =
                        sh@misc$sourceCode_default %||% character(0),
                      modInp = sh@modInp)
      }
    }
    if (is.null(vdata)) {
      # pre-rework folders kept the base cartridge in problem.RData
      legacy <- fp(scen@path, "problem.RData")
      if (file.exists(legacy)) {
        e <- new.env(parent = emptyenv())
        nm <- load(legacy, envir = e)
        vdata <- get(nm[1], envir = e)
      }
    }
  }
  if (is.null(vdata)) {
    stop("The ", if (nzchar(variant)) paste0("variant '", variant, "'") else
           "base problem", " of scenario '", scen@name, "' has no saved ",
         "problem data.\n",
         "  Save the scenario while that problem is active ",
         "(save_scenario()), then switch.")
  }
  stopifnot(is.list(vdata), !is.null(vdata$modInp), !is.null(vdata$settings))
  # the stored settings may carry dataset refs (thinned geoscale) — a live
  # object never holds a stub
  scen@settings <- .resolve_dataset_refs(
    .settings_restore(vdata$settings,
                      vdata$sourceCode_default %||% character(0)),
    verbose = FALSE)
  scen@misc$variant <- if (nzchar(variant)) variant else NULL
  scen@modInp <- .upgrade_modInp(
    .modinp_rebase(vdata$modInp, .problem_modinp_root(scen)))
  scen
}

# Write the ACTIVE problem's cartridge — VARIANTS only (called by
# save_scenario after the modInp store is on disk; `mi` is the thinned
# modInp). The base problem needs no cartridge: scen.RData is its home.
.write_problem_swap <- function(scen, mi) {
  v <- .run_variant(scen)
  if (!nzchar(v)) return(invisible(NULL))
  thin <- .settings_thin(scen@settings)
  # save_scenario may already have thinned the sourceCode before this runs —
  # the swap must remember EVERY dropped block to restore on switch
  dropped <- union(thin$dropped,
                   scen@misc$sourceCode_default %||% character(0))
  vdata <- list(settings = thin$settings,
                sourceCode_default = dropped,
                modInp = mi)
  vdir <- fp(scen@path, "runs", v)
  dir.create(vdir, recursive = TRUE, showWarnings = FALSE)
  save(vdata, file = fp(vdir, "variant.RData"))
  .write_variant_manifest(scen, vdir)
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
  # lifecycle state (seal/mark, R/seal.R) survives re-saves
  for (k in c("sealed", "sealed_at", "sealed_hash",
              "marked_delete", "delete_importance", "marked_at")) {
    if (!is.null(prev[[k]])) mf[[k]] <- prev[[k]]
  }
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
    reg <- .registry_open()
    rel <- .registry_rel_path(scen@path)
    reg <- add_to_registry(reg, "scenario", scen@name, path = rel)
    rd <- .run_dirs(scen)
    for (i in which(rd$has_record)) {
      # plain solve label in `name`, the problem in `variant` — the
      # addressing id "<variant>/<solve>" is assembled by .run_id(), never
      # stored as a compound name
      reg <- add_to_registry(reg, "run", rd$solve[i],
                          path = fp(rel, "runs",
                                    .run_id(rd$variant[i], rd$solve[i])),
                          parent = scen@name, variant = rd$variant[i])
    }
    save_registry(reg)
  }, error = function(e) {
    warning("Could not update the project registry (",
            conditionMessage(e), ")", call. = FALSE)
  })
  invisible(NULL)
}


# ---------------------------------------------------------------------------
# (was R/import_solution.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =============================================================================#
# import_solution.R -- choosing a solver attempt and making it stick.
#
# `read_solution()` populates `@modOut` and `save_scenario()` writes it; with
# neither called, a solved run leaves `output/` behind and no `modOut/`. With
# several attempts there is no table to choose from either: `run.yml`'s
# `objective` is computed from the IN-MEMORY solution at finish time
# (`.run_record_finish()`), so it is NA for exactly the runs that were never
# imported.
#
# It is recoverable without importing anything: `output/log.csv` carries the
# solution status, and the objective is one small `output/vObjective.<ext>`
# file whose extension comes from that run's own `solver.csv`. Peeking is what
# makes an informed choice possible -- nothing here ranks or auto-selects.
#
# Nothing in this file writes to `output/`.
# =============================================================================#

#' @include read.R
NULL

# Status and objective of one run, read from its `output/` without importing.
# Never errors: a corrupt or half-written dump degrades the row to NA.
.peek_run_solution <- function(run_dir) {
  out <- list(solver_status = NA_character_, objective = NA_real_,
              has_output = FALSE, output_mb = 0)
  solver_dir <- tryCatch(.run_solver_dir(run_dir), error = function(e) NULL)
  if (is.null(solver_dir)) return(out)
  odir <- .art_output_dir(solver_dir, run_dir)
  if (!dir.exists(odir)) return(out)
  out$has_output <- TRUE
  out$output_mb <- round(dir_size(odir, missing = "zero") / 1024^2, 3)

  # solution status: `log.csv` records it as a numeric code, 1 == optimal --
  # the same reading `read_solution()` makes when it sets `@stage`.
  lg <- tryCatch(utils::read.csv(fp(odir, "log.csv"), stringsAsFactors = FALSE),
                 error = function(e) NULL)
  if (!is.null(lg) && all(c("parameter", "value") %in% names(lg))) {
    v <- lg$value[lg$parameter == "solution status"]
    out$solver_status <- if (!length(v)) {
      "no status"
    } else if (identical(as.character(v[1]), "1")) {
      "optimal"
    } else {
      paste0("not optimal (", v[1], ")")
    }
  }

  # objective: one tiny file, whose extension follows the run's own
  # import_format -- not the scenario's, which may differ or be empty.
  imf <- tolower(tryCatch({
    m <- .read_solver_meta(solver_dir)
    x <- m$value[m$name == "import_format"]
    if (length(x)) as.character(x[1]) else ""
  }, error = function(e) ""))
  ext <- if (imf == "parquet") ".parquet" else
    if (imf %in% c("feather", "ipc", "arrow")) ".arrow" else ".csv"
  f <- fp(odir, paste0("vObjective", ext))
  if (file.exists(f)) {
    d <- tryCatch(
      if (identical(ext, ".csv")) {
        utils::read.csv(f, stringsAsFactors = FALSE)
      } else {
        as.data.frame(.read_exchange_table(f))
      }, error = function(e) NULL)
    if (!is.null(d) && nrow(d)) {
      col <- if ("value" %in% names(d)) "value" else names(d)[ncol(d)]
      out$objective <- suppressWarnings(as.numeric(d[[col]][1]))
    }
  }
  out
}

#' What solutions does this scenario have, and which is worth importing?
#'
#' @description
#' One row per run, joining what the run record holds with what can be read
#' back from a run's raw `output/` without importing it. Read-only: it writes
#' nothing, not even the objectives it recovers.
#'
#' @details
#' `run.yml`'s `objective` is computed from the in-memory solution when the
#' solve finishes, so it is `NA` for any run whose solution was never imported
#' — which is exactly the set you need to compare after several attempts.
#' This recovers it per run from the single `vObjective` file in `output/`,
#' and `objective_src` says which reading you are looking at: `"record"` from
#' `run.yml`, `"output"` peeked from the dump, `NA` when neither is available.
#'
#' `solver_status` likewise comes from the run's `output/log.csv` and is the
#' solver's own verdict, independent of `status`, which is what the driver
#' recorded. The two disagree exactly when a solve finished but was never read
#' back.
#'
#' Nothing here ranks the runs or picks one. Objectives are comparable only
#' within one problem: a `"<variant>/<solve>"` run solves a *different* problem
#' (its own `modInp`), so compare within a `variant`, not across.
#'
#' @param scen a scenario object.
#' @param peek logical; `FALSE` skips reading `output/` and leaves
#'   `solver_status` / peeked objectives as `NA`. Faster on a scenario with
#'   many large runs.
#'
#' @return a tibble: `run`, `variant`, `solve`, `status`, `solver_status`,
#'   `objective`, `objective_src`, `solver_name`, `lang`, `solver`,
#'   `duration_sec`, `imported`, `has_output`, `output_mb`, `active`.
#' @seealso [import_solution()], [scenario_runs()], [scenario_artifacts()]
#' @examples
#' \dontrun{
#' scenario_solutions(scen)                      # what is there
#' import_solution(scen, "julia_highs_barrier")  # make one of them stick
#' }
#' @export
scenario_solutions <- function(scen, peek = TRUE) {
  stopifnot(is(scen, "scenario"))
  empty <- tibble(
    run = character(0), variant = character(0), solve = character(0),
    status = character(0), solver_status = character(0),
    objective = numeric(0), objective_src = character(0),
    solver_name = character(0), lang = character(0), solver = character(0),
    duration_sec = numeric(0), imported = logical(0), has_output = logical(0),
    output_mb = numeric(0), active = logical(0))

  rd <- .run_dirs(scen)
  if (!nrow(rd)) return(empty)
  act_v <- .run_variant(scen)
  act_s <- scen@misc$run %||% ""

  rows <- lapply(seq_len(nrow(rd)), function(i) {
    dir <- rd$dir[i]
    rec <- if (isTRUE(rd$has_record[i])) {
      tryCatch(yaml::read_yaml(fp(dir, "run.yml")), error = function(e) NULL)
    } else {
      NULL
    }
    pk <- if (isTRUE(peek)) .peek_run_solution(dir) else
      list(solver_status = NA_character_, objective = NA_real_,
           has_output = dir.exists(.art_output_dir(.run_solver_dir(dir), dir)),
           output_mb = NA_real_)

    obj <- suppressWarnings(as.numeric(rec$objective %||% NA_real_))
    src <- if (!is.na(obj)) "record" else if (!is.na(pk$objective)) "output" else
      NA_character_
    if (is.na(obj)) obj <- pk$objective

    tibble(
      run = .run_id(rd$variant[i], rd$solve[i]),
      variant = rd$variant[i], solve = rd$solve[i],
      status = as.character(rec$status %||% if (isTRUE(rd$has_record[i]))
        "unknown" else "no-record"),
      solver_status = pk$solver_status,
      objective = obj, objective_src = src,
      solver_name = as.character(rec$solver_name %||% NA_character_),
      lang = as.character(rec$lang %||% NA_character_),
      solver = as.character(rec$solver %||% NA_character_),
      duration_sec = suppressWarnings(as.numeric(rec$duration_sec %||% NA_real_)),
      imported = dir.exists(fp(dir, "modOut", "variables")),
      has_output = pk$has_output,
      output_mb = pk$output_mb,
      active = identical(rd$variant[i], act_v) &&
        identical(rd$solve[i], act_s))
  })
  bind_rows(c(list(empty), rows))
}

# The run's own solver identity, from the `solver.csv` it was solved with.
# `cmdline` is deliberately excluded: it is an absolute, machine-local path
# that every backend regenerates when empty, and `strip_user_info()` clears it
# on purpose. Restoring it would put a username back into a saved object.
.solver_identity <- function(solver_dir) {
  m <- tryCatch(.read_solver_meta(solver_dir), error = function(e) NULL)
  if (is.null(m) || !all(c("name", "value") %in% names(m))) return(NULL)
  keep <- c("name", "lang", "solver", "method", "import_format",
            "export_format")
  out <- list()
  for (k in keep) {
    v <- m$value[m$name == k]
    if (length(v) && nzchar(as.character(v[1]))) out[[k]] <- as.character(v[1])
  }
  if (!length(out)) NULL else out
}

#' Import a run's solution and make it stick
#'
#' @description
#' Reads the solution of one run into `modOut` and saves it, so the scenario
#' keeps it after the session ends. This is the step between a finished solve
#' and a scenario that still has its results tomorrow: `solve_scenario()` never
#' saves, and `read_solution()` alone leaves the solution in memory.
#'
#' @details
#' Pick the run with [scenario_solutions()], which shows each attempt's status,
#' objective, solver and backend. Nothing is chosen for you.
#'
#' The solver's `output/` is left alone. Importing makes it *removable* — it is
#' no longer the only copy — but removing it is [drop_solver_outputs()]'s job,
#' and `cleanup = FALSE` here means this function never does it.
#'
#' **Why this is a verb rather than two calls.** When `read_solution()` cannot
#' read `output/variable_list.csv` it returns the scenario *unchanged*,
#' discarding the run switch. Saving that would write an empty `modOut/`, which
#' flips the run to "imported" and would let the solution's only copy be
#' cleaned up. This refuses to save a read that produced no variables.
#'
#' @param scen a scenario object.
#' @param run character, the run to import (`"<solve>"` or
#'   `"<variant>/<solve>"`).
#' @param ... passed to [read_solution()].
#' @param restore_solver logical; take the run's solver identity (name, lang,
#'   solver, method, formats) from its `solver.csv`. Never the command line.
#' @param save logical; write the result with [save_scenario()]. `FALSE`
#'   returns the scenario with the solution in memory only — which is the
#'   situation this function exists to end, so it is on by default.
#' @param promote logical; after importing, make this the scenario's own
#'   solution with [promote_solution()] — copied to `<scenario>/modOut/` with
#'   its provenance, after which the run tree is scratch. Off by default,
#'   since which attempt is final is a choice.
#' @param cleanup logical; remove the run's regenerable solver files
#'   afterwards. Off by default.
#' @param verbose logical.
#'
#' @return the scenario, invisibly, with `modOut` populated and `run` active.
#' @seealso [scenario_solutions()], [read_solution()], [drop_solver_outputs()]
#' @export
import_solution <- function(scen, run, ..., restore_solver = TRUE,
                            save = TRUE, promote = FALSE, cleanup = FALSE,
                            verbose = TRUE) {
  stopifnot(is(scen, "scenario"))
  if (missing(run) || !length(run) || !nzchar(run[1])) {
    stop("`run` is required: name the run to import.\n  Available runs:\n",
         .run_list_hint(scen), call. = FALSE)
  }
  run <- as.character(run[1])
  id <- .parse_run_id(run, scen)
  run_dir <- tryCatch(.run_dir(scen, id$variant, id$solve),
                      error = function(e) NULL)
  if (is.null(run_dir) || !dir.exists(run_dir)) {
    stop("No run '", run, "' in scenario '", scen@name, "'.\n",
         "  Available runs:\n", .run_list_hint(scen), call. = FALSE)
  }

  out <- read_solution(scen, run = run, echo = FALSE, ...)

  # The guard. `read_solution()` hands back the ORIGINAL object when the
  # dump cannot be read, so "no variables" is the signal that nothing was
  # imported -- and saving then would write an empty store over a run whose
  # `output/` is still the only copy.
  nfilled <- sum(vapply(out@modOut@variables, function(v) {
    d <- tryCatch(get_data_slot(v), error = function(e) NULL)
    !is.null(d) && nrow(d) > 0
  }, logical(1)))
  if (nfilled == 0L) {
    stop("Nothing was imported from run '", run, "': its output/ produced no ",
         "variables, so the scenario was left untouched.\n",
         "  scenario_solutions() shows what each run holds; the solver's ",
         "output/ has not been modified.", call. = FALSE)
  }

  if (isTRUE(restore_solver)) {
    sid <- .solver_identity(.run_solver_dir(run_dir))
    if (!is.null(sid)) {
      for (k in names(sid)) out@settings@solver[[k]] <- sid[[k]]
      # regenerated by every backend; never carried between machines
      out@settings@solver$cmdline <- NULL
    }
  }

  if (isTRUE(save)) out <- save_scenario(out, verbose = FALSE)

  # Promotion needs the run's store on disk, so it follows the save.
  if (isTRUE(promote)) {
    out <- promote_solution(out, run = run, save = TRUE, verbose = verbose)
  }

  if (isTRUE(cleanup)) {
    tryCatch(drop_solver_outputs(out, runs = run, dry_run = FALSE,
                                 verbose = verbose),
             error = function(e) {
               warning("Imported, but the clean-up failed: ",
                       conditionMessage(e), call. = FALSE)
             })
  }

  if (isTRUE(verbose)) {
    obj <- tryCatch(get_variable(out, "vObjective")$value[1],
                    error = function(e) NA_real_)
    message("Imported run '", run, "' of '", out@name, "': ", nfilled,
            " variables", if (!is.na(obj)) paste0(", objective ",
                                                  format(obj, digits = 10)),
            if (isTRUE(save)) "; saved." else
              "; NOT saved (save = FALSE).")
  }
  invisible(out)
}


# ---------------------------------------------------------------------------
# (was R/promote_solution.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =============================================================================#
# promote_solution.R -- the scenario's chosen solution, stored beside modInp.
#
# A solution lives in the run that produced it,
# `runs/[<variant>/]<solve>/modOut/`, and the shell's `@modOut` points there.
# Deleting `runs/` therefore BRICKS the scenario: the paths survive the rebase
# and every read fails with "On-disk data expected but not found".
#
# Promoting copies the chosen run's store to `<scenario>/modOut/`, a sibling of
# `modInp/`, and clears the active run so the rebase resolves there. After
# that the run tree is scratch and can be dropped.
#
# `modOut.yml` travels with the store because `run.yml` is the ONLY home of the
# solve's provenance -- solver, timings, host, the version that produced it --
# and dies with the run folder. It is written LAST, so its presence is also
# what distinguishes a complete store from a half-written one.
# =============================================================================#

#' @include read.R
NULL

.MODOUT_MANIFEST <- "modOut.yml"

.modout_manifest_path <- function(store_dir) fp(store_dir, .MODOUT_MANIFEST)

.modout_manifest_read <- function(store_dir) {
  f <- .modout_manifest_path(store_dir)
  if (!file.exists(f)) return(NULL)
  tryCatch(yaml::read_yaml(f), error = function(e) NULL)
}

# A promoted store is one carrying the manifest. That is the discriminator
# `upgrade_scenario_layout()` needs: a top-level `modOut/` WITHOUT it is the
# layout-2 artifact the upgrader migrates into a run, and one WITH it is a
# deliberate promotion that must be left alone.
.modout_is_promoted <- function(store_dir) {
  !is.null(.modout_manifest_read(store_dir))
}

# Written last, once the payload is in place.
.modout_manifest_write <- function(store_dir, run_id = "", rec = NULL,
                                   stage = "solved", objective = NA_real_,
                                   format = get_storage_format()) {
  vdir <- fp(store_dir, "variables")
  nms <- if (dir.exists(vdir)) {
    sort(basename(list.dirs(vdir, recursive = FALSE)))
  } else {
    character(0)
  }
  mf <- list(
    class = "modOut",
    format = format,
    from_run = as.character(run_id %||% ""),
    promoted = .registry_now(),
    stage = as.character(stage %||% ""),
    objective = if (is.na(objective)) NULL else as.numeric(objective),
    variables = as.list(nms),
    energyRt_version = as.character(utils::packageVersion("energyRt"))
  )
  # Provenance of the SOLVE, copied from the run record before it can be
  # deleted. Row counts are deliberately not recorded: reading every variable
  # to count them would cost as much as the promotion itself, and the
  # manifest-written-last rule already carries completeness.
  if (!is.null(rec)) {
    mf$solver <- list(name = rec$solver_name %||% "", lang = rec$lang %||% "",
                      solver = rec$solver %||% "")
    mf$solved <- list(
      started = rec$started %||% "", finished = rec$finished %||% "",
      duration_sec = rec$duration_sec %||% "",
      mem_mb = rec$mem_mb %||% "", peak_mb = rec$peak_mb %||% "")
    mf$solved_with_version <- rec$energyRt_version %||% ""
    mf$hostname <- rec$hostname %||% ""
    mf$user <- rec$user %||% ""
  }
  mf <- mf[!vapply(mf, is.null, logical(1))]
  yaml::write_yaml(mf, .modout_manifest_path(store_dir))
  invisible(mf)
}

# Move a staged store into place. The previous one is set aside first and
# removed only once the new one has landed, so an interrupted swap leaves one
# of the two, never neither.
.store_swap_in <- function(staging, final) {
  prev <- paste0(final, ".prev")
  unlink(prev, recursive = TRUE, force = TRUE)
  if (dir.exists(final) && !file.rename(final, prev)) {
    stop("Could not replace the store at '", final,
         "': something is holding the folder open. The new one is staged at '",
         staging, "'.", call. = FALSE)
  }
  if (!file.rename(staging, final)) {
    if (dir.exists(prev)) file.rename(prev, final)
    stop("Could not move the staged store into '", final, "'.", call. = FALSE)
  }
  unlink(prev, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

#' Make a run's solution the scenario's own
#'
#' @description
#' Copies an imported run's solution to `<scenario>/modOut/`, beside `modInp/`,
#' and clears the active run so the scenario reads from there. After this the
#' `runs/` tree is scratch: it can be dropped without losing the solution.
#'
#' @details
#' Until a solution is promoted it lives only inside the run that produced it,
#' and deleting that run leaves the scenario pointing at a store that is no
#' longer there — every read then fails. Promotion is what makes the run tree
#' disposable.
#'
#' The solve's provenance travels with the store in `modOut.yml` — solver,
#' objective, stage and timings — because `run.yml` is its only other home and
#' goes with the run folder. The manifest is written last, so its presence
#' also marks the store as complete.
#'
#' Runs are untouched: promoting copies, it does not move. Use
#' [drop_scenario_run()] or [drop_solver_outputs()] afterwards to reclaim the
#' space, and [scenario_solutions()] to choose which run to promote.
#'
#' @section Variants cannot be promoted:
#' An own-problem variant carries its own `modInp`, settings and manifest
#' inside `runs/<variant>/`. Its results belong to a different problem from
#' the scenario's, so lifting only the solution would pair variant results
#' with base-problem parameters. Promote a base-problem run, or keep the
#' variant's run folder.
#'
#' @param scen a scenario object.
#' @param run character, the run to promote (`"<solve>"`). `NULL` (default)
#'   takes the scenario's active run.
#' @param save logical; write the scenario shell afterwards, so a reload
#'   resolves to the promoted store. On by default — without it the store
#'   exists and nothing points at it.
#' @param verbose logical.
#'
#' @return the scenario, invisibly, reading from `<scenario>/modOut/`.
#' @seealso [import_solution()], [scenario_solutions()], [drop_scenario_run()]
#' @examples
#' \dontrun{
#' scen <- import_solution(scen, "glpk", promote = TRUE)
#' drop_scenario_run(scen, "glpk", force = TRUE)   # the run is now scratch
#' }
#' @export
promote_solution <- function(scen, run = NULL, save = TRUE, verbose = TRUE) {
  stopifnot(is(scen, "scenario"))
  # `@misc$run` is the SOLVE label only; the variant lives beside it, so
  # defaulting from the active run has to rejoin the two or a variant-active
  # scenario resolves to a base run that does not exist.
  run <- as.character(
    run %||% .run_id(.run_variant(scen), scen@misc$run %||% ""))
  if (!nzchar(run)) {
    stop("No run to promote: the scenario has no active run and none was ",
         "named.\n  scenario_solutions() lists what it has.", call. = FALSE)
  }
  id <- .parse_run_id(run, scen)
  if (nzchar(id$variant %||% "")) {
    stop("Run '", run, "' belongs to variant '", id$variant,
         "', an own problem: its modInp, settings and manifest live in ",
         "runs/", id$variant, "/, and its results are not comparable with ",
         "the scenario's own problem.\n  Promote a base-problem run, or keep ",
         "that variant's run folder.", call. = FALSE)
  }
  run_dir <- tryCatch(.run_dir(scen, "", id$solve), error = function(e) NULL)
  if (is.null(run_dir) || !dir.exists(run_dir)) {
    stop("No run '", run, "' in scenario '", scen@name, "'.\n",
         "  Available runs:\n", .run_list_hint(scen), call. = FALSE)
  }
  src <- fp(run_dir, "modOut")
  if (!dir.exists(fp(src, "variables"))) {
    stop("Run '", run, "' has no imported solution to promote.\n",
         "  import_solution(scen, \"", run, "\") reads it from the solver's ",
         "output/ first.", call. = FALSE)
  }

  final <- fp(scen@path, "modOut")
  staging <- paste0(final, ".incoming")
  unlink(staging, recursive = TRUE, force = TRUE)
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  .sui_copy_tree(src, staging)
  # a stale manifest copied from the source would claim the wrong provenance
  unlink(.modout_manifest_path(staging), force = TRUE)

  rec <- tryCatch(yaml::read_yaml(fp(run_dir, "run.yml")),
                  error = function(e) NULL)
  obj <- tryCatch(as.numeric(rec$objective %||% NA_real_),
                  error = function(e) NA_real_)
  .modout_manifest_write(staging, run_id = run, rec = rec,
                         stage = rec$stage %||% "solved", objective = obj)
  .store_swap_in(staging, final)

  # Point the object at the promoted store and clear the active run, which is
  # what makes the load-time rebase resolve to `<scenario>/modOut`.
  mo <- .modout_from_store(
    final,
    sets = tryCatch(scen@modOut@sets, error = function(e) list()),
    stage = rec$stage %||% "solved")
  if (!is.null(mo)) scen@modOut <- mo
  scen@misc$run <- ""
  scen@misc$variant <- NULL

  if (isTRUE(save)) scen <- save_scenario(scen, verbose = FALSE)
  if (isTRUE(verbose)) {
    message("Promoted run '", run, "' to '", final, "'. The runs/ tree is now ",
            "scratch: drop_scenario_run() or drop_solver_outputs() reclaim it.")
  }
  invisible(scen)
}


# ---------------------------------------------------------------------------
# (was R/paths_rebase.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =============================================================================#
# paths_rebase.R — path rebasing on load + the layout upgrade tool
# (storage layout 3, stage S4b).
#
# Saved objects record where their parquet stores were AT SAVE TIME, relative
# to the then-working directory. Instead of trusting those strings,
# load_scenario() rebuilds every stored path deterministically from the
# folder actually being loaded — so a moved or renamed scenario folder, or a
# different getwd(), just works.
# =============================================================================#

# Rebuild every stored path of a loaded scenario from `root` (the directory
# the scenario was loaded from). Layout-aware: the solution store lives under
# the active run (layout 3) or at the top level (layout 2 / pre-run saves).
.scenario_rebase_paths <- function(scen, root) {
  root <- gsub("[\\/]+", "/", root)
  old_root <- gsub("[\\/]+", "/", scen@path %||% "")
  scen@path <- root

  # modInp + each parameter (an own-problem variant's store lives under its
  # variant dir, the base problem's at the scenario level)
  scen@modInp <- .modinp_rebase(scen@modInp, .problem_modinp_root(scen, root))

  # modOut + each variable (NULL until the first solve)
  if (isS4(scen@modOut)) {
    mo_root <- if (nzchar(scen@misc$run %||% "")) {
      fp(.run_dir(scen, .run_variant(scen), scen@misc$run), "modOut")
    } else {
      fp(root, "modOut")
    }
    if (length(get_ondisk_slots(scen@modOut))) {
      scen@modOut@misc$path <- mo_root
    }
    for (nm in names(scen@modOut@variables)) {
      v <- scen@modOut@variables[[nm]]
      if (isS4(v) && length(get_ondisk_slots(v))) {
        v@misc$path <- fp(mo_root, "variables", nm)
        scen@modOut@variables[[nm]] <- v
      }
    }
  }

  # embedded model (a model-store reference is resolved by load_scenario and
  # rebased by load_model)
  if (is.null(scen@model@misc$model_ref)) {
    scen@model <- .model_rebase(scen@model, fp(root, "model"))
  }

  # legacy external/working dirs recorded inside the old scenario folder
  # (layout-2 `script/<solver>` or an old misc$tmp.dir): re-prefix when they
  # pointed inside the folder that moved; leave truly external dirs alone
  for (key in c("solver.dir", "tmp.dir")) {
    d <- scen@misc[[key]]
    if (is.character(d) && length(d) == 1L && nzchar(d) &&
        nzchar(old_root) && !identical(old_root, root)) {
      d <- gsub("[\\/]+", "/", d)
      if (startsWith(d, paste0(old_root, "/"))) {
        scen@misc[[key]] <- fp(root, substring(d, nchar(old_root) + 2L))
      }
    }
  }
  scen
}

#' Upgrade a scenario folder to the current on-disk layout
#'
#' @description
#' Migrates a scenario directory in place to layout 3 with the current run
#' structure. Idempotent — running it on an up-to-date folder changes
#' nothing. It handles, in one pass:
#'
#' * layout 2: each `script/<solver>/` directory (recognized by its `solver`
#'   csv) becomes `runs/<solver>/solver/` with a backfilled `run.yml`
#'   (`status: "legacy"`, solver identity from the csv, timestamps from file
#'   modification times); the top-level `modOut/` store moves under the
#'   active run. Transient timestamp directories are left where they are.
#' * interim layout 3 (early builds): the `runs/default/<solve>/` wrapper is
#'   flattened to `runs/<solve>/`, and each run's `script/` working dir is
#'   renamed to `solver/`.
#'
#' The scenario object is re-saved with rebased paths, `scenario.yml` (with
#' its `default:` run) and the `layout` marker are written, and the project
#' registry is updated.
#'
#' @param path character, the scenario directory.
#' @param verbose logical.
#' @return the upgraded scenario object, invisibly.
#' @export
upgrade_scenario_layout <- function(path, verbose = TRUE) {
  path <- gsub("[\\/]+", "/", path)
  if (!dir.exists(path) || !file.exists(fp(path, "scen.RData"))) {
    stop("'", path, "' is not a saved scenario directory (no scen.RData)")
  }
  say <- function(...) if (verbose) message(...)

  scen <- load_scenario(path, env = NULL, verbose = FALSE)

  .mv <- function(from, to) {
    # rename with a copy fallback (cross-volume moves)
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    if (suppressWarnings(file.rename(from, to))) return(invisible(TRUE))
    dir.create(to, recursive = TRUE, showWarnings = FALSE)
    ff <- list.files(from, recursive = TRUE, all.files = TRUE,
                     full.names = FALSE, include.dirs = TRUE)
    for (f in ff) {
      src <- fp(from, f)
      dst <- fp(to, f)
      if (dir.exists(src)) {
        dir.create(dst, recursive = TRUE, showWarnings = FALSE)
      } else {
        dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
        file.copy(src, dst, overwrite = TRUE, copy.date = TRUE)
      }
    }
    unlink(from, recursive = TRUE, force = TRUE)
    invisible(TRUE)
  }

  # -- interim layout 3: flatten runs/default/<solve>/ -> runs/<solve>/ ------
  interim_root <- fp(path, "runs", "default")
  if (dir.exists(interim_root) &&
      !file.exists(fp(interim_root, "run.yml")) &&
      !file.exists(fp(interim_root, "variant.yml"))) {
    for (sd in list.dirs(interim_root, recursive = FALSE)) {
      target <- fp(path, "runs", basename(sd))
      if (dir.exists(target)) {
        say("Keeping '", basename(sd), "' under runs/default/ (a run of ",
            "that name already exists at the base level)")
        next
      }
      say("Flattening runs/default/", basename(sd), " -> runs/",
          basename(sd))
      .mv(sd, target)
      # fix the record's variant field
      ry <- fp(target, "run.yml")
      if (file.exists(ry)) {
        rec <- tryCatch(yaml::read_yaml(ry), error = function(e) NULL)
        if (!is.null(rec)) {
          rec$variant <- ""
          yaml::write_yaml(rec, ry)
        }
      }
    }
    if (length(list.files(interim_root)) == 0) unlink(interim_root,
                                                      recursive = TRUE)
    if (identical(scen@misc$variant, "default")) scen@misc$variant <- ""
  }

  # -- rename each run's script/ working dir to solver/ ----------------------
  rd <- .run_dirs(scen)
  for (i in seq_len(nrow(rd))) {
    old_sd <- fp(rd$dir[i], "script")
    new_sd <- fp(rd$dir[i], "solver")
    if (dir.exists(old_sd) && !dir.exists(new_sd)) {
      say("Renaming ", .run_id(rd$variant[i], rd$solve[i]),
          "/script -> solver")
      .mv(old_sd, new_sd)
    }
  }

  # -- layout 2: script/<solver>/ -> runs/<solver>/solver/ + run.yml ---------
  legacy_root <- fp(path, "script")
  migrated_active <- NULL
  if (dir.exists(legacy_root)) {
    old_active <- gsub("[\\/]+", "/",
                       scen@misc$solver.dir %||% scen@misc$tmp.dir %||% "")
    for (sd in list.dirs(legacy_root, recursive = FALSE)) {
      # The solve's metadata marks a real run directory. It is `solver.csv`
      # since the run folder was flattened and `solver` before that, so both
      # names identify one -- a folder written either side of the change still
      # migrates.
      sf <- .solver_meta_path(sd)
      if (is.na(sf)) next # transient/unknown dir: leave in place
      lbl <- basename(sd)
      run_dir <- .run_dir(scen, "", lbl)
      if (file.exists(fp(run_dir, "run.yml"))) next # already migrated
      say("Migrating script/", lbl, " -> runs/", lbl, "/solver")
      .mv(sd, fp(run_dir, "solver"))
      sv <- tryCatch(utils::read.csv(.solver_meta_path(fp(run_dir, "solver")),
                                     stringsAsFactors = FALSE),
                     error = function(e) NULL)
      getv <- function(k) {
        if (is.null(sv)) return("")
        v <- sv$value[sv$name == k]
        if (length(v)) v[1] else ""
      }
      out_log <- fp(run_dir, "solver", "output", "log.csv")
      mt <- file.info(fp(run_dir, "solver"))$mtime
      rec <- list(
        label = lbl, variant = "", scenario = scen@name,
        solver_name = getv("name"), lang = getv("lang"),
        solver = getv("solver"), cmdline = getv("cmdline"),
        started = format(mt, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        status = if (file.exists(out_log)) "legacy" else "legacy-unsolved",
        modinp = "shared",
        energyRt_version = as.character(utils::packageVersion("energyRt"))
      )
      yaml::write_yaml(rec, fp(run_dir, "run.yml"))
      if (identical(gsub("[\\/]+", "/", fp(path, "script", lbl)),
                    old_active) || is.null(migrated_active)) {
        migrated_active <- lbl
      }
    }
    if (length(list.files(legacy_root)) == 0) unlink(legacy_root,
                                                     recursive = TRUE)
  }

  # -- top-level modOut/ moves under the active run --------------------------
  #
  # Unless it is a PROMOTED store. `promote_solution()` writes the scenario's
  # chosen solution here deliberately and clears the active run, which is the
  # same shape a layout-2 scenario has -- `modOut.yml` is what tells them
  # apart. Migrating a promoted store into a run would silently undo the
  # promotion and make runs/ load-bearing again.
  top_modout <- fp(path, "modOut")
  if (.modout_is_promoted(top_modout)) {
    say("Keeping the promoted modOut/ (modOut.yml present)")
  } else
  if (is.null(scen@misc$run %||% NULL) || !nzchar(scen@misc$run %||% "")) {
    if (!is.null(migrated_active)) {
      scen@misc$variant <- ""
      scen@misc$run <- migrated_active
      scen@misc$tmp.dir <- NULL
      scen@misc$solver.dir <- NULL
    }
  }
  if (dir.exists(top_modout) && nzchar(scen@misc$run %||% "") &&
      !.modout_is_promoted(top_modout)) {
    target <- fp(.run_dir(scen, .run_variant(scen), scen@misc$run), "modOut")
    if (!dir.exists(target)) {
      say("Moving modOut/ under runs/",
          .run_id(.run_variant(scen), scen@misc$run))
      .mv(top_modout, target)
    }
  }

  # -- finalize: rebase paths, then re-save the shell through save_scenario()
  # (which re-applies the sourceCode dedup and model-ref logic that loading
  # undid, and writes layout marker + manifest + registry rows)
  scen <- .scenario_rebase_paths(scen, path)
  scen <- suppressMessages(save_scenario(scen, path = path, verbose = FALSE))

  # -- fold a legacy problem.RData into scen.RData (its one home now): the
  # shell takes the base problem's shells and the separate cartridge goes
  legacy <- fp(path, "problem.RData")
  if (file.exists(legacy)) {
    say("Folding problem.RData into scen.RData")
    e <- new.env(parent = emptyenv())
    vdata <- get(load(legacy, envir = e)[1], envir = e)
    es <- new.env(parent = emptyenv())
    sh <- get(load(fp(path, "scen.RData"), envir = es)[1], envir = es)
    sh@settings <- vdata$settings
    sh@misc$sourceCode_default <- vdata$sourceCode_default %||% character(0)
    sh@modInp <- vdata$modInp
    sh@misc$variant <- NULL
    sh@misc$run <- NULL
    sh@misc$has_base <- TRUE
    es2 <- new.env(parent = emptyenv())
    es2$scen <- sh
    save(list = "scen", envir = es2, file = fp(path, "scen.RData"))
    unlink(legacy)
  }

  say("Scenario '", scen@name, "' upgraded to layout ", .SCENARIO_LAYOUT)
  invisible(scen)
}


# ---------------------------------------------------------------------------
# (was R/store_deps.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =============================================================================#
# store_deps.R — the reference graph over the four stores.
#
# `save_scenario(embed_model = NULL)` stores the model and records
# `{name, hash, source}` rather than embedding a copy, so referencing is the
# normal state and removing a store entry can break entries that point at it.
# The edges are already written by the save paths; this reads them back the
# other way round (`prepare_for_sharing()` reads the same fields outbound).
#
#   scenario.yml     model:        {name, hash, source}
#                    datasets:     [{name, hash, source}, ...]
#   model.yml        repositories: [...]
#                    datasets:     [...]
#   repository.yml   datasets:     [...]
#
# Read from MANIFESTS, not the registry: the registry is a derived index that
# can be stale, and refreshing it writes. A delete guard cannot depend on that.
#
# `source == "embedded"` carries its own copy and is never a dependency — that
# distinction is the whole point of the guard.
# =============================================================================#

.dep_cols <- function() {
  tibble(type = character(0), name = character(0), path = character(0),
         via = character(0), target = character(0), hash = character(0))
}

# Outgoing references of one manifest, as list(via, target, hash) entries.
.dep_edges <- function(type, mf) {
  out <- list()
  add <- function(via, e) {
    if (!is.list(e) || !identical(as.character(e$source %||% ""), "ref")) {
      return(invisible(NULL))
    }
    nm <- as.character(e$name %||% "")
    if (!nzchar(nm)) return(invisible(NULL))
    out[[length(out) + 1L]] <<-
      list(via = via, target = nm, hash = as.character(e$hash %||% ""))
    invisible(NULL)
  }
  if (identical(type, "scenario")) add("model", mf$model)
  if (identical(type, "model")) {
    for (e in mf$repositories %||% list()) add("repository", e)
  }
  # datasets hang off scenarios, models and repositories alike
  for (e in mf$datasets %||% list()) add("dataset", e)
  out
}

# Every reference in the project, one row per edge. One first-level pass over
# the four store roots — the same walk delete_marked() and refresh_registry()
# already make.
.store_dep_index <- function() {
  kinds <- .entry_kinds()
  rows <- list()
  for (tp in names(kinds)) {
    root <- kinds[[tp]]$root()
    if (!dir.exists(root)) next
    for (d in list.dirs(root, recursive = FALSE)) {
      mf <- tryCatch(yaml::read_yaml(fp(d, kinds[[tp]]$manifest)),
                     error = function(e) NULL)
      if (is.null(mf)) next
      edges <- .dep_edges(tp, mf)
      if (!length(edges)) next
      nm <- as.character(mf$name %||% basename(d))
      for (e in edges) {
        rows[[length(rows) + 1L]] <- tibble(
          type = tp, name = nm, path = gsub("[\\/]+", "/", d),
          via = e$via, target = e$target, hash = e$hash)
      }
    }
  }
  if (length(rows)) bind_rows(rows) else .dep_cols()
}

# Rows of an index that point at one entry, with the version check applied.
# `current` is NA when the dependent recorded no hash to check against.
.dep_of <- function(idx, type, name, current_hash = "") {
  out <- idx[idx$via == type & idx$target == name, , drop = FALSE]
  out$target <- NULL
  out$current <- if (nrow(out)) {
    ifelse(!nzchar(out$hash), NA,
           nzchar(current_hash) & startsWith(current_hash, out$hash))
  } else {
    logical(0)
  }
  out[order(out$type, out$name), , drop = FALSE]
}

#' What depends on a stored model, repository or dataset
#'
#' @description
#' Lists the store entries that *reference* `x` rather than embedding a copy of
#' it — the entries that would break if it were deleted. Read-only.
#'
#' @details
#' Referencing is the default: [save_scenario()] stores the model and records
#' `{name, hash, source}` instead of embedding it, and the same holds one level
#' down for a model's repositories and for the big tables of the dataset store.
#' A dependent recorded with `source = "embedded"` holds its own copy and is
#' never listed here.
#'
#' The edges are read from the manifests (`scenario.yml`, `model.yml`,
#' `repository.yml`), not from the project registry, which is a derived index
#' that can be stale.
#'
#' `current` compares the hash the dependent recorded against the hash the
#' entry holds now. `FALSE` means the dependent is pinned to a superseded
#' version — store entries update in place — and will load the current one with
#' a "results may not reproduce" warning. `NA` means the dependent recorded no
#' hash to check.
#'
#' An entry with no dependents is an orphan as far as this project is
#' concerned: nothing here points at it.
#'
#' @param x an energyRt object, or the name of a store entry.
#' @param type the entry type when `x` is a bare name: `"model"`,
#'   `"repository"`, `"dataset"` or `"scenario"`. Inferred from an object.
#'
#' @return a tibble: `type` and `name` of the dependent, its `path`, `via` (the
#'   kind of reference), the `hash` it recorded and whether that is `current`.
#' @seealso [delete_marked()], [mark_delete()]
#' @examples
#' \dontrun{
#' store_dependents("UTOPIA", type = "model")   # which scenarios use it
#' nrow(store_dependents("UTOPIA", type = "model")) == 0   # an orphan?
#' }
#' @export
store_dependents <- function(x, type = NULL) {
  type <- .entry_type_of(x, type)
  e <- .entry_resolve(type, x)
  cur <- as.character(.entry_manifest(type, e$path)$hash %||% "")
  .dep_of(.store_dep_index(), type, e$name, cur)
}

# The interactive half of the delete guard. Never reached in a non-interactive
# session: menu() would read EOF and abort a scripted sweep, which is why the
# guard refuses first and asks second.
.dep_prompt <- function(type, name, deps) {
  if (!interactive()) return(FALSE)
  who <- paste0(deps$type, " '", deps$name, "'")
  shown <- utils::head(who, 3)
  message(type, " '", name, "' is referenced by ", nrow(deps), " entr",
          if (nrow(deps) == 1L) "y" else "ies", ":\n  ",
          paste(shown, collapse = ", "),
          if (nrow(deps) > 3L) {
            paste0(" ... (", nrow(deps) - 3L, " more)")
          } else "")
  ans <- tryCatch(
    utils::menu(c("Keep it", "Delete it anyway"),
                title = paste0("Delete ", type, " '", name, "'?")),
    error = function(e) 0L)
  identical(as.integer(ans), 2L)
}
