# =============================================================================#
# solve.R -- solver framework (write / run / read) + the interp_mod pipeline
# entry points solve_model()/solve_scenario() (merged here from the former solve_new.R).
# The shared framework (.executeScenario) is also used by the archived legacy
# entry points (depreciated/R/solve_legacy.R). The public legacy names
# interpolate_model()/solve_model()/solve_scenario()/`solve` are repurposed in
# R/legacy_api_shims.R.
# =============================================================================#

# ---- "smart" path helpers ----------------------------------------------------
# Build a filesystem-safe slug from parts, dropping empty/NA and case-insensitive
# duplicate components (e.g. when the scenario and model share a name).
.path_slug <- function(..., sep = "_") {
  parts <- unlist(list(...), use.names = FALSE)
  parts <- parts[!is.na(parts)]
  parts <- trimws(as.character(parts))
  parts <- parts[nzchar(parts)]
  parts <- gsub("[^A-Za-z0-9]+", "-", parts) # path-safe
  parts <- gsub("(^-+)|(-+$)", "", parts)
  parts <- parts[nzchar(parts)]
  parts <- parts[!duplicated(parts)] # drop exact repeats (e.g. scenario==model)
  paste(parts, collapse = sep)
}

# Smart solver-directory name: {backend}_{solver}_{method}. Prefers an explicit
# `solver$name` (curated, e.g. "julia_highs"); otherwise derives from the
# backend (solver$backend or solver$lang), solver, and method (solver$method or
# inferred from a "barrier"/"simplex" hint).
.solver_dir_name <- function(solver) {
  if (is.null(solver)) {
    return("")
  }
  if (!is.null(solver$name) && nzchar(solver$name)) {
    return(solver$name)
  }
  method <- solver$method
  if (is.null(method) || !nzchar(method)) {
    hint <- paste(c(solver$name, names(solver)), collapse = " ")
    method <- if (grepl("barrier", hint, ignore.case = TRUE)) {
      "barrier"
    } else if (grepl("simplex", hint, ignore.case = TRUE)) {
      "simplex"
    } else {
      NULL
    }
  }
  backend <- solver$backend
  if (is.null(backend) || !nzchar(backend)) backend <- solver$lang
  nm <- .path_slug(backend, solver$solver, method)
  if (!nzchar(nm)) "solver" else nm
}

# Smart scenario-folder name: {scenario}_{model}_{calendar}_{horizon}.
# Empty and duplicate components are dropped.
.scenario_dir_name <- function(scenario_name, model_name = "",
                               calendar_name = "", horizon_name = "") {
  nm <- .path_slug(scenario_name, model_name, calendar_name, horizon_name)
  if (!nzchar(nm)) as.character(scenario_name)[1] else nm
}

# Map the deprecated tmp.* argument names onto the current vocabulary.
# `tmp.dir` -> `solver.dir`, `tmp.del` -> `transient`; `tmp.path`/`tmp.name`
# (never documented, reachable only via `...`) are dropped.
.solver_dir_aliases <- function(arg) {
  if (!is.null(arg[["tmp.dir"]])) {
    rlang::warn(
      "The `tmp.dir` argument is deprecated; use `solver.dir`.",
      .frequency = "once", .frequency_id = "energyRt-tmp.dir-deprecated")
    if (is.null(arg[["solver.dir"]])) arg[["solver.dir"]] <- arg[["tmp.dir"]]
    arg[["tmp.dir"]] <- NULL
  }
  if (!is.null(arg[["tmp.del"]])) {
    rlang::warn(
      "The `tmp.del` argument is deprecated; use `transient`.",
      .frequency = "once", .frequency_id = "energyRt-tmp.del-deprecated")
    if (is.null(arg[["transient"]])) arg[["transient"]] <- arg[["tmp.del"]]
    arg[["tmp.del"]] <- NULL
  }
  if (!is.null(arg[["tmp.path"]]) || !is.null(arg[["tmp.name"]])) {
    warning("`tmp.path`/`tmp.name` are no longer supported; pass the full ",
            "directory as `solver.dir`.", call. = FALSE)
    arg[["tmp.path"]] <- NULL
    arg[["tmp.name"]] <- NULL
  }
  arg
}

# Resolve the solver working directory for a solve/write. Three modes:
#   - arg$solver.dir set: EXTERNAL mode — honored verbatim, no run record;
#   - arg$transient: throwaway timestamp dir under <scen>/solver/, deleted
#     after the solve, no run record;
#   - default: the run's directory runs/[<variant>/]<label>/solver, with a
#     run.yml record (run.record = TRUE). The label is arg$run.label or the
#     solver dir name; run.conflict = "overwrite"|"suffix"|"error" governs an
#     existing recorded run under the same label.
# Nothing is derived from stored paths: the run dir always comes from
# (scen@path, variant, label) via .run_dir().
.resolve_solver_dir <- function(scen = NULL, arg = NULL) {
  arg$run.record <- FALSE

  # external mode
  if (!is.null(arg[["solver.dir"]]) && length(arg[["solver.dir"]]) > 0) {
    arg[["solver.dir"]] <- gsub("[\\/]+", "/", arg[["solver.dir"]])
    return(arg)
  }

  if (is.null(scen) || is_empty(scen@path)) {
    stop("Cannot resolve a solver directory without a scenario path; ",
         "pass `solver.dir` explicitly.")
  }

  # transient: throwaway dir, deleted after the solve
  if (isTRUE(arg[["transient"]])) {
    ts <- format(Sys.time(), "%Y%m%d%H%M%S%Z", tz = "UTC")
    arg[["solver.dir"]] <- gsub("[\\/]+", "/", fp(scen@path, "solver", ts))
    return(arg)
  }

  # recorded run
  label <- if (!is_empty(arg[["run.label"]])) {
    arg[["run.label"]]
  } else {
    .solver_dir_name(arg[["solver"]])
  }
  if (!nzchar(label)) label <- format(Sys.time(), "%Y%m%d%H%M%S%Z", tz = "UTC")
  variant <- .run_variant(scen)
  conflict <- arg[["run.conflict"]] %||% "overwrite"
  run_dir <- .run_dir(scen, variant, label)
  if (conflict != "overwrite" && file.exists(fp(run_dir, "run.yml"))) {
    if (conflict == "error") {
      stop("Run '", .run_id(variant, label), "' already exists in '",
           scen@path, "'. Pass run.conflict = \"overwrite\" or ",
           "\"suffix\", or a different `run` label.")
    }
    # "suffix": first free -2, -3, ... variant of the label
    k <- 2L
    repeat {
      cand <- paste0(label, "-", k)
      cand_dir <- .run_dir(scen, variant, cand)
      if (!file.exists(fp(cand_dir, "run.yml"))) {
        label <- cand
        run_dir <- cand_dir
        break
      }
      k <- k + 1L
    }
  }
  arg$run.dir <- run_dir
  arg$run.label <- label
  arg$run.variant <- variant
  arg$run.record <- TRUE
  arg[["solver.dir"]] <- .run_solver_dir(run_dir)
  arg
}

# Deprecated former name of .resolve_solver_dir() (internal; kept because
# archived scripts in depreciated/ reference it).
get_tmp_dir <- function(scen = NULL, arg = NULL) {
  .Deprecated(".resolve_solver_dir")
  .resolve_solver_dir(scen, arg)
}

####### Internal functions ##########
.executeScenario <- function(
    scen,
    # tmp.dir = NULL,
    # solver = NULL,
    ...
    # interpolate = FALSE,
    # read.solution = FALSE,
    # write = FALSE
    ) {
  # - solves scen, interpolate if required (NULL), force (TRUE), or no interpolation (FALSE, error if not interpolated)
  ## arguments
  # solver.dir - external solver working directory (default: the run's solver/)
  # echo = TRUE - print working data
  # open.folder = FALSE - open folder before the run
  # show.output.on.console = FALSE & invisible = FALSE arg for command system
  # only.listing = FALSE (!depreciated?) generate only listing file (works for gams only)
  # read.solution = TRUE read result
  # transient - throwaway solver dir, deleted after the solve
  # browser()
  arg <- .solver_dir_aliases(list(...))
  # if (is_empty(arg[["solver.dir"]])) arg[["solver.dir"]] <- NULL
  # if (is_empty(arg[["solver"]])) arg[["solver"]] <- NULL
  if (is_empty(arg[["read.solution"]])) arg[["read.solution"]] <- FALSE
  if (is_empty(arg[["write"]])) arg[["write"]] <- FALSE

  # arg <- get_tmp_dir(scen, arg)
  # if (is.null(arg$solver.dir)) {
  #   browser()
  #   stop("tmp.dir is not specified")
  # }
  # browser()
  if (is_empty(arg$echo)) arg$echo <- TRUE
  if (is_empty(arg[["solver"]])) {
    if (is_empty(scen@settings@solver)) {
      # arg[["solver"]] <- list(lang = "PYOMO")
      arg[["solver"]] <- get_default_solver()
      scen@settings@solver <- arg[["solver"]]
    } else {
      arg[["solver"]] <- scen@settings@solver
    }
    # scen@settings@solver <- list(lang = "PYOMO")
  } else if (is.character(arg[["solver"]])) {
    scen@settings@solver <- list(name = arg[["solver"]], lang = arg[["solver"]])
    arg[["solver"]] <- scen@settings@solver
  } else if (is.list(arg[["solver"]])) {
    scen@settings@solver <- arg[["solver"]]
  }
  # The two should be the same object by now; a mismatch means one of the
  # branches above failed to keep them in sync. Reported only under
  # `options(en.debug = 1)` -- this used to drop into `browser()`, which halts
  # non-interactive and batch solves.
  if (isDebug() && !identical(scen@settings@solver, arg[["solver"]])) {
    cli::cli_warn(
      "Solver settings disagree: `scen@settings@solver` != the solver argument."
    )
  }
  if (is_empty(arg$open.folder)) arg$open.folder <- FALSE
  if (is_empty(arg$show.output.on.console)) arg$show.output.on.console <- FALSE
  # if (is.null(arg$invisible)) arg$invisible <- FALSE
  if (is_empty(arg$read.solution)) arg$read.solution <- TRUE
  if (is_empty(arg$transient)) arg$transient <- arg$read.solution
  # arg$write <- write
  if (is_empty(arg$wait)) {
    if (is_empty(scen@settings@solver$wait)) {
      arg$wait <- TRUE
    } else {
      arg$wait <- scen@settings@solver$wait
    }
  } else if (is_empty(arg$invisible)) {
    arg$invisible <- arg$wait
  }
  scen@settings@solver$wait <- arg$wait
  if (is_empty(arg$invisible)) {
    if (is_empty(scen@settings@solver$invisible)) {
      arg$invisible <- TRUE
    } else {
      arg$invisible <- scen@settings@solver$invisible
    }
  }
  scen@settings@solver$invisible <- arg$invisible
  # `run` is historically the "run the solver" switch. A character value is the
  # run (solve) label instead: solve into runs/<variant>/<label>/ (layout 3).
  if (is.character(arg[["run"]]) && length(arg[["run"]]) == 1L && nzchar(arg[["run"]])) {
    arg$run.label <- arg[["run"]]
    arg[["run"]] <- TRUE
  }
  if (is_empty(arg[["run"]])) arg[["run"]] <- TRUE
  if (is_empty(arg$n.threads)) arg$n.threads <- 1

  # if (is.null(arg$onefile)) arg$onefile <- FALSE
  # if (!is.null(arg$dir.result)) {
  #   warning("solve_model: parameter `dir.result` is depreciated, use `tmp.dir` instead")
  #   if (is.null(arg$solver.dir)) {
  #     arg$solver.dir <- arg$dir.result
  #   } else {
  #     stop("check `dir.result` and `tmp.dir` - only one should be used")
  #   }
  # } else {
  #   # temporary - will be depreciated
  #   arg$dir.result <- arg$solver.dir
  # }
  # browser()
  arg <- .resolve_solver_dir(scen, arg)

  if (is.null(scen)) {
    if (arg$interpolate | arg$write) {
      stop("scenario object not found")
    }
  } else {
    if (isTRUE(arg$run.record)) {
      # recorded run: the directory is derived from (path, variant, run) on
      # demand — no stored dir path
      scen@misc$variant <- arg$run.variant
      scen@misc$run <- arg$run.label
      scen@misc$solver.dir <- NULL
      scen@misc$tmp.dir <- NULL
    } else {
      # external or transient dir: remember it (it is not derivable)
      scen@misc$solver.dir <- arg$solver.dir
      scen@misc$tmp.dir <- NULL
    }
    tmp_name <- scen@name
  }
  # arg$dir.result <- .fix_path(arg$dir.result)
  # arg$solver.dir <- .fix_path(arg$solver.dir)
  # if (!is.null(scen)) scen@misc$tmp.dir <- .fix_path(scen@misc$tmp.dir)

  # if (is.null(arg$solver.dir)) {
  #   arg$solver.dir <- .fp(
  #     fp(getwd(), "solwork"),
  #     paste(arg[["solver"]]$lang, tmp_name, # scen@name,
  #       format(Sys.time(), "%Y%m%d%H%M%S%Z", tz = Sys.timezone()),
  #       sep = "_"
  #     )
  #   )
  # }
  # arg$dir.result <- arg$solver.dir

  # interpolate
  # browser()
  if (isTRUE(arg$interpolate)) {
    scen <- energyRt::interpolate(scen, ...)
    arg$write <- TRUE
    # interpolate <- FALSE
    arg$interpolate <- FALSE
  }

  # write
  # browser()
  # dir.create(arg$solver.dir, recursive = TRUE, showWarnings = FALSE)
  # if (arg$open.folder) shell.exec(arg$solver.dir)
  if (is.null(arg$solver.dir) || length(arg$solver.dir) == 0) {
    stop("tmp.dir is not specified")
  }
  if (!isTRUE(arg$write) & !dir.exists(arg$solver.dir)) {
    stop(paste(
      "tmp.dir does not exist:\n  ",
      arg$solver.dir, "\n  ",
      "hint: run 'write_script' for the specified solver and 'tmp.dir'"
      ))
  }
  if (arg$write) {
    dir.create(arg$solver.dir, recursive = TRUE, showWarnings = FALSE)
    if (arg$echo) cat("Solver directory: ", arg$solver.dir, "\n")
    if (arg$echo) cat("Writing files: ")
    solver_solver_time <- proc.time()[3]
    if (any(grep("^gams$", scen@settings@solver$lang, ignore.case = TRUE))) {
      # if (is.null(arg$trim)) arg$trim <- FALSE
      # scen <- .write_model_GAMS(arg, scen, trim = arg$trim)
      scen <- .write_model_GAMS(arg, scen, trim = FALSE)
    } else if (any(grep("^(glpk|cbcb)$", scen@settings@solver$lang,
      ignore.case = TRUE
    ))) {
      scen <- .write_model_GLPK_CBC(arg, scen)
    } else if (any(grep("^pyomo", scen@settings@solver$lang, ignore.case = TRUE))) {
      scen <- .write_model_PYOMO(arg, scen)
    } else if (any(grep("^jump$", scen@settings@solver$lang, ignore.case = TRUE))) {
      scen <- .write_model_JuMP(arg, scen)
    } else {
      stop("Unknown solver ", scen@settings@solver$lang)
    }

    ## Write solver parameter
    nn <- grep("^(inc[1-5]|files|code_files|code[[:digit:]]*)$",
      names(scen@settings@solver),
      value = TRUE, invert = TRUE
    )
    tmp <- data.frame(
      name = nn,
      value = sapply(
        scen@settings@solver[nn],
        function(x) paste0(c(x, recursive = TRUE), collapse = " ")
      ),
      stringsAsFactors = FALSE
    )
    tmp <- rbind(tmp, data.frame(
      name = paste0("code", seq_along(scen@settings@solver$code)),
      value = scen@settings@solver$code, stringsAsFactors = FALSE
    ))
    write.csv(tmp, file = fp(arg$solver.dir, "solver"), row.names = FALSE)

    if (arg$echo) {
      cat(round(proc.time()[3] - solver_solver_time, 2), "s\n", sep = "")
      flush.console()
    }
    scen@status$script <- TRUE
  }
  record <- isTRUE(arg$run.record) && isTRUE(arg[["run"]])
  if (record) .run_record_start(scen, arg)
  if (isTRUE(arg[["run"]])) {
    if (record) {
      # finalize the run record on error/interrupt before rethrowing, so a
      # crashed solve never leaves a stale `running` record behind
      tryCatch(
        .call_solver(arg, scen),
        error = function(e) {
          .run_record_finish(scen, arg, status = "failed")
          stop(e)
        },
        interrupt = function(e) {
          .run_record_finish(scen, arg, status = "interrupted")
          stop("Solver run interrupted", call. = FALSE)
        }
      )
    } else {
      .call_solver(arg, scen)
    }
  }
  if (isTRUE(arg$read.solution) && isTRUE(arg[["run"]])) {
    scen <- read_solution(scen, solver.dir = arg$solver.dir, echo = arg$echo)
  }
  if (record) {
    status <- if (!isTRUE(arg$read.solution)) {
      "finished" # solver ran; solution not read, so optimality is unknown
    } else if (isTRUE(scen@status$optimal)) {
      "solved"
    } else {
      "not-optimal"
    }
    .run_record_finish(scen, arg, status = status)
  }

  return(scen)
}

.call_solver <- function(arg, scen) {
  # browser()
  HOMEDIR <- getwd()
  if (!arg[["run"]]) {
    return()
  }
  if (arg$echo) cat("Starting ", scen@settings@solver$lang, "\n")
  gams_run_time <- proc.time()[3]

  # Remote backend: submit to NEOS instead of running a local solver process.
  # The WRITE phase already produced the model in arg$solver.dir; .neos_call_solver
  # submits it, waits, and drops the returned solution CSVs into tmp.dir/output/
  # so read_solution() is unchanged. (Pyomo-on-NEOS is NOT here: it is an
  # ordinary local `python` run whose remoteness lives inside the generated .py.)
  if (identical(scen@settings@solver$backend, "neos")) {
    rs <- .neos_call_solver(arg, scen)
    if (rs != 0) stop(paste("NEOS solve error code", rs))
    if (arg$echo) cat("", round(proc.time()[3] - gams_run_time, 2), "s\n", sep = "")
    return(invisible())
  }

  # Restore the working directory on EVERY exit path, not just the two handlers
  # below -- an early `return()` used to leave the session sitting in the solver
  # run folder.
  on.exit(setwd(HOMEDIR), add = TRUE)

  tryCatch(
    {
      setwd(arg$solver.dir)
      if (.Platform$OS.type == "windows") {
        if (arg$invisible || !isTRUE(arg$echo)) {
          cmd <- ""
        } else {
          cmd <- if_else(interactive(),  "cmd /k", "")
        }
        rs <- system(paste(cmd, scen@settings@solver$cmdline), #' gams energyRt.gms', arg$gamsCompileParameter),
          invisible = arg$invisible, wait = arg$wait,
          # `echo = FALSE` silences the solver's own console output too
          ignore.stdout = !isTRUE(arg$echo)
        )
      } else {
        rs <- system(paste(scen@settings@solver$cmdline),
          wait = arg$wait,
          # `echo = FALSE` silences the solver's own console output too
          ignore.stdout = !isTRUE(arg$echo)
        )
      }
      setwd(HOMEDIR)
    },
    interrupt = function(x) {
      if (arg$transient) unlink(arg$solver.dir, recursive = TRUE)
      setwd(HOMEDIR)
      stop("Solver has been interrupted")
    },
    error = function(x) {
      if (arg$transient) unlink(arg$solver.dir, recursive = TRUE)
      setwd(HOMEDIR)
      stop(x)
    }
  )
  if (rs != 0) stop(paste("Solution error code", rs))
  if (arg$echo) cat("", round(proc.time()[3] - gams_run_time, 2), "s\n", sep = "")
}

# Submit a written GAMS scenario to NEOS and stage the results for read_solution.
# Returns 0 on a "Normal" completion, non-zero otherwise. Requires env var
# NEOS_EMAIL. Uses TEXT data (inlined `$include`s, no gdx) so no local GAMS/gdx
# library is needed. Fields on the solver option: $neos_solver (NEOS solver name,
# default $solver or CPLEX), $neos_category (default "milp"), $neos_max_wait.
.neos_call_solver <- function(arg, scen) {
  s <- scen@settings@solver
  if (!identical(toupper(s$lang), "GAMS")) {
    stop("backend = 'neos' is currently supported only for GAMS solver options ",
         "(e.g. solver_options$neos_gams_cplex). For Python use ",
         "solver_options$neos_pyomo_cplex (an ordinary local run that submits ",
         "to NEOS itself).", call. = FALSE)
  }
  email <- get_neos_email()
  if (is.null(email)) {
    stop("No NEOS email set. Use set_neos_email('you@example.com') or set the ",
         "NEOS_EMAIL environment variable.", call. = FALSE)
  }
  gms <- file.path(arg$solver.dir, "energyRt.gms")
  if (!file.exists(gms)) stop("NEOS: 'energyRt.gms' not found in ", arg$solver.dir)

  # Inline model + text data into one self-contained .gms (flat NEOS workspace).
  model <- neos_gams_inline(gms, arg$solver.dir, flatten = TRUE)
  if (nchar(model, type = "bytes") > 16777216L) {
    stop(sprintf(paste0("NEOS job input is %.1f MB, over the ~16 MB cap. Reduce ",
      "the model (sample the calendar / prune) before submitting."),
      nchar(model, type = "bytes") / 1e6), call. = FALSE)
  }

  xml <- neos_build_gams_xml(
    model = model, email = email,
    solver = s$neos_solver %||% s$solver %||% "CPLEX",
    category = s$neos_category %||% "milp",
    gdx = "", wantgdx = "yes", wantlst = "yes",
    comments = paste0("energyRt ", scen@name))

  if (arg$echo) cat("Submitting to NEOS ...\n")
  h <- neos_submit_job(xml, timeout = 300)
  if (arg$echo) cat("  NEOS job ", h$job, "\n", sep = "")
  neos_wait(h$job, h$password, poll = 5,
            max_wait = s$neos_max_wait %||% 1800, verbose = isTRUE(arg$echo))
  cc <- neos_completion_code(h$job, h$password)
  if (arg$echo) cat("  NEOS completion: ", cc, "\n", sep = "")

  # Fetch the returned workspace zip; copy its solution CSVs into tmp.dir/output/
  # (the model wrote them flat since we flattened `output/`), which is exactly
  # where read_solution() looks (variable_list.csv, log.csv, per-variable CSVs).
  zb <- neos_get_output_file(h$job, h$password, "solver-output.zip", timeout = 300)
  zf <- file.path(arg$solver.dir, "neos-output.zip")
  writeBin(zb, zf)
  ux <- file.path(arg$solver.dir, "_neos_unzip")
  unlink(ux, recursive = TRUE); dir.create(ux, showWarnings = FALSE)
  utils::unzip(zf, exdir = ux)
  outdir <- file.path(arg$solver.dir, "output")
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  csvs <- list.files(ux, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  if (!length(csvs)) {
    stop("NEOS job ", h$job, " returned no CSV output (completion: ", cc,
         "). See the .lst in ", ux, call. = FALSE)
  }
  file.copy(csvs, outdir, overwrite = TRUE)

  if (identical(cc, "Normal")) 0L else 1L
}

# NEW version ####

# =============================================================================#
# Solve model / scenario objects via the interpolate_model() mapping pipeline.
# (Merged from the former solve_new.R.) solve_model()/solve_scenario() build
# the interpolated scenario with interpolate_model() and reuse the
# write/run/read framework above (.executeScenario()). The transitional names
# solve_mod()/solve_scen() are deprecated aliases in legacy_api_shims.R.
# =============================================================================#



# Finalise an `interp_mod()`-built scenario so the write / solve framework can
# run on it: attach the solver source-code templates and flag the scenario as
# interpolated. Mirrors the tail of `interpolate()`.
.finalize_interp <- function(scen, check = TRUE) {
  if (is_empty(scen@settings@sourceCode)) {
    scen@settings@sourceCode <- .modelCode
  }
  scen@status$interpolated <- TRUE
  scen@status$script <- FALSE
  if (isTRUE(check)) scen <- .check_scen_par(scen)
  scen
}

# Load on-disk parameter data back into the in-memory `@data` slots so the
# `write_*` helpers (which read `@data` directly) see the full model. No-op for
# an in-memory scenario. The parameter store on disk is left untouched.
.materialize_modInp <- function(scen) {
  if (!isOnDisk(scen@modInp)) {
    return(scen)
  }
  for (nm in names(scen@modInp@parameters)) {
    p <- scen@modInp@parameters[[nm]]
    if (!isOnDisk(p)) next
    d <- get_data_slot(p) # reads from the on-disk parameter store
    if (is.null(d)) d <- p@data
    # Normalise column classes after the parquet/csv round-trip: an all-NA folded
    # column (region/timeslice) comes back as `vctrs_unspecified` and a whole-number
    # value narrows to integer, so force the in-memory classes (character dims,
    # integer year, numeric value) to match an in-memory build exactly.
    p@data <- force_cols_classes(d)
    scen@modInp@parameters[[nm]] <- mark_inMemory(p)
  }
  scen@modInp <- mark_inMemory(scen@modInp)
  scen
}

#' Solve a model or an interpolated scenario
#'
#' `solve_model()` interpolates a model with [interpolate_model()] and solves
#' it (an already-interpolated scenario passed to it is solved as-is; an
#' un-interpolated one is interpolated first). `solve_scenario()` solves a
#' scenario that was already interpolated with [interpolate_model()] and
#' errors otherwise. Both reuse the write / run / read framework
#' (`.executeScenario()`); all solver backends (GLPK/GMPL, GAMS, Pyomo, JuMP)
#' work unchanged. The transitional names `solve_mod()` / `solve_scen()` are
#' deprecated aliases.
#'
#' @param obj a model object (`solve_model()`) or an interpolated scenario
#'   object (`solve_scenario()`).
#' @param name character name of the scenario to create / return.
#' @param solver a character or list with solver settings. When `NULL`, the
#'   scenario's own solver settings or `get_default_solver()` are used.
#' @param ondisk,fold passed to [interpolate_model()]. Defaults (`FALSE`/`FALSE`) keep
#'   parameters in memory and unfolded, matching the shape the writers expect.
#' @param solver.dir character, an EXTERNAL solver working directory. When
#'   set, the solve runs there verbatim and records no run; the default
#'   (`NULL`) solves into the scenario's own run directory
#'   `runs/[<variant>/]<label>/solver/`.
#' @param transient logical, use a throwaway timestamp directory deleted
#'   after the run (no run record).
#' @param tmp.dir,tmp.del deprecated aliases of `solver.dir` / `transient`.
#' @param force logical, re-solve a scenario already solved to optimal.
#' @param variant character (`solve_scenario()` only): declare the scenario's
#'   interpolated problem an OWN-PROBLEM VARIANT of this name. Its solves
#'   land in `runs/<variant>/<solve>/`, and [save_scenario()] stores the
#'   variant's problem (`modInp`, settings snapshot, `variant.yml`) under
#'   `runs/<variant>/` — once, shared by all its solves — leaving the
#'   scenario-level (base) problem untouched. Switch between problems with
#'   [read_solution()] (`run = "<variant>/<solve>"` vs `run = "<solve>"`).
#' @param ... for `solve_model()`, arguments are routed to
#'   [interpolate_model()] (settings / calendar / horizon / model data) or to
#'   the solver run (`solver.dir`, `transient`, `force`, `read.solution`,
#'   `wait`, `echo`, `run`, `run.conflict`, `n.threads`, ...). For
#'   `solve_scenario()`, arguments are passed to `.executeScenario()`. Set
#'   `echo = FALSE` for a quiet run: it silences both the progress messages
#'   and the solver's own console output.
#'
#'   `run` doubles as the run label: `TRUE`/`FALSE` keeps its historical
#'   meaning (run the solver or only write the script), while a character
#'   value names the run — the solve lands in `<scenario>/runs/<run>/` (or
#'   `runs/<variant>/<run>/` for an own-problem variant) with a `run.yml`
#'   provenance record (default label: the solver directory name, e.g.
#'   `"glpk"`). `run.conflict` controls an existing recorded run under the
#'   same label: `"overwrite"` (default), `"suffix"` (append `-2`, `-3`,
#'   ...), `"error"`. See [scenario_runs()] to list runs and
#'   [read_solution()] to switch between them.
#'
#' @seealso [interpolate_model()], [read_solution()]
#' @return a scenario object with the solution.
#' @rdname solve_model
#' @export
solve_model <- function(obj, name = NULL, solver = NULL,
                        ondisk = FALSE, fold = FALSE, ...) {
  if (inherits(obj, "scenario")) {
    # convenience: interpolate an un-interpolated scenario before solving; an
    # already-interpolated scenario is solved as-is (its build knobs preserved)
    if (!isTRUE(obj@status$interpolated)) {
      obj <- interpolate_model(obj@model, name = obj@name)
    }
    return(do.call(solve_scenario, c(list(obj = obj, solver = solver),
                                     list(...))))
  }
  if (!inherits(obj, "model")) {
    stop("`solve_model()` expects a model or scenario object. ",
         "Use `solve_scenario()` for an interpolated scenario.")
  }
  dots <- list(...)
  # Arguments that belong to the solver run rather than to interpolation
  # (tmp.dir/tmp.del are the deprecated aliases of solver.dir/transient).
  solve_args <- c("solver.dir", "transient", "tmp.dir", "tmp.del", "force",
                  "read.solution", "wait",
                  "echo", "run", "run.conflict", "n.threads", "invisible",
                  "show.output.on.console", "open.folder")
  is_solve <- names(dots) %in% solve_args
  iarg <- dots[!is_solve]
  sarg <- dots[is_solve]

  scen <- do.call(
    interpolate_model,
    c(list(mod = obj, name = name, ondisk = ondisk, fold = fold), iarg)
  )

  do.call(solve_scenario, c(list(obj = scen, solver = solver), sarg))
}

#' @rdname solve_model
#' @export
solve_scenario <- function(obj, name = obj@name, solver = NULL, solver.dir = NULL,
                       transient = FALSE, force = FALSE, kvl = NULL,
                       variant = NULL, ...,
                       tmp.dir = NULL, tmp.del = NULL) {
  if (!is.null(variant)) {
    # own-problem variant: the interpolated problem in `obj` belongs to this
    # named variant — its solves land in runs/<variant>/<solve>/ and
    # save_scenario() stores its modInp under runs/<variant>/modInp/
    stopifnot(is.character(variant), length(variant) == 1L)
    obj@misc$variant <- if (nzchar(variant)) variant else NULL
  }
  if (!is.null(tmp.dir)) {
    rlang::warn(
      "The `tmp.dir` argument is deprecated; use `solver.dir`.",
      .frequency = "once", .frequency_id = "energyRt-tmp.dir-deprecated")
    if (is.null(solver.dir)) solver.dir <- tmp.dir
  }
  if (!is.null(tmp.del)) {
    rlang::warn(
      "The `tmp.del` argument is deprecated; use `transient`.",
      .frequency = "once", .frequency_id = "energyRt-tmp.del-deprecated")
    transient <- tmp.del
  }
  if (!inherits(obj, "scenario")) {
    stop("`solve_scenario()` expects a scenario built by `interpolate_model()`. ",
         "Use `solve_model()` for a model object.")
  }
  if (!isTRUE(obj@status$interpolated)) {
    stop("Scenario is not interpolated. Build it with `interpolate_model()` ",
         "or call `solve_model()` on the model.")
  }
  # `kvl` here GUARDS rather than builds -- the cycle constraints are baked in at
  # interpolation time, so this is the last chance to notice that a solve is
  # about to differ from what was asked for. KVL changes the answer, and the
  # difference is silent otherwise: a scenario built without it simply solves
  # the transport relaxation and reports nothing unusual.
  .kvl_guard(obj, kvl)
  if (isTRUE(obj@status$optimal) && !isTRUE(force)) {
    message("The scenario is already solved to optimal.\n",
            "Use 'force = TRUE' to solve it again.")
    return(obj)
  }

  scen <- .finalize_interp(obj)
  scen@name <- name

  # The `write_*` helpers read each parameter's in-memory `@data` slot. For an
  # on-disk scenario those slots are empty (data lives in the parameter store),
  # so the model would be written out empty. Load the parameter data back into
  # memory before writing.
  scen <- .materialize_modInp(scen)

  # A folded scenario (interp_mod(fold = TRUE)) carries NA wildcards in the
  # trimmable dimensions. Make it solver-ready by replacing the wildcard with the
  # artificial set member and substituting it into the chosen backend's model
  # code. No-op for an unfolded scenario. Substitution is implemented for GLPK
  # (`[]`), JuMP (`[(...)]` / `haskey`) and Pyomo (`.get((...))`); GAMS still needs
  # declaration-aware `()` handling.
  .blk <- .fold_code_block(if (!is.null(solver)) {
    if (is.list(solver)) solver$lang else solver
  } else scen@settings@solver$lang)
  if (.blk %in% c("GLPK", "JuMP", "PYOMOConcrete")) {
    scen <- apply_fold_artificial(scen, backends = .blk)
  }

  arg <- list(...)
  arg$interpolate <- FALSE
  arg$write <- TRUE
  arg$force <- force
  if (!is.null(solver)) arg[["solver"]] <- solver
  if (!is.null(solver.dir)) arg$solver.dir <- solver.dir
  arg$transient <- transient
  if (is.null(arg$read.solution)) arg$read.solution <- TRUE
  arg$scen <- scen

  scen <- do.call(.executeScenario, arg)

  if (isTRUE(transient)) {
    # The internally resolved throwaway dir is not in `arg` (resolution happens
    # inside .executeScenario) but survives on the returned scenario.
    td <- if (!is.null(solver.dir)) solver.dir else scen@misc[["solver.dir"]]
    if (!is.null(td)) {
      unlink(td, recursive = TRUE)
      scen@misc[["solver.dir"]] <- NULL
      # transient dirs are <scen>/solver/<UTC stamp> and its only tenants;
      # drop the wrapper once empty so the scenario folder stays clean
      parent <- dirname(td)
      if (identical(basename(parent), "solver") && dir.exists(parent) &&
          !length(list.files(parent, all.files = TRUE, no.. = TRUE))) {
        unlink(parent, recursive = TRUE)
      }
    }
  }
  scen
}
