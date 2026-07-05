# =============================================================================#
# solve.R -- SHARED solver framework (write / run / read).
# Used by BOTH the new interp_mod/solve_scen pipeline (R/solve_new.R) and the
# archived legacy entry points (depreciated/R/solve_legacy.R). The legacy
# solve_model()/solve_scenario()/`solve` methods were split out during the
# legacy retirement; public names are repurposed in R/legacy_api_shims.R.
# =============================================================================#

get_tmp_dir <- function(scen = NULL, arg = NULL) {
  # solver directory (tmp.dir) convention name
  # tmp.dir - full path to the directory for the solver's files
    # tmp.path - path where the tmp.dir will be created
    # tmp.name - name of the directory for the solver's files
    # tmp.dir == fp(tmp.path, tmp.name)
  # tmp.del - if TRUE, the tmp.dir will be deleted after the scenario is solved
  # return: arg with tmp.dir and tmp.del
  # browser()
  tmp.path <- tmp.name <- NULL

  # 1. tmp.dir is given
  if (!is.null(arg[["tmp.dir"]]) && length(arg[["tmp.dir"]]) > 0) {
    arg[["tmp.dir"]] <- gsub("[\\/]+", "/", arg[["tmp.dir"]])
    return(arg)
  }

  # if (isTRUE(arg[["tmp.del"]]) && is.null(arg[["tmp.dir"]])) {
  if (isTRUE(arg[["tmp.del"]])) {
    arg[["tmp.name"]] <-  format(Sys.time(), "%Y%m%d%H%M%S%Z", tz = "UTC")
    # return(arg)
  }
  # browser()
  # 2. scen@misc$tmp.dir is given
  if (!is.null(scen)) {
    # if (!is.null(scen@misc$tmp.dir) && length(scen@misc$tmp.dir) > 0) {
    if (!is_empty(scen@misc$tmp.dir)) {
      # browser()
      if (identical(basename(scen@misc$tmp.dir), arg[["solver"]]$name)) {
        if (!is.null(scen@misc$tmp.dir)) {
          arg[["tmp.dir"]] <- scen@misc$tmp.dir
        # } else {
        #   arg[["tmp.dir"]] <- get_
          return(arg)
        }
      }
    }
    if (!is_empty(scen@path)) {
      tmp.path <- fp(scen@path, "script")
      # return(arg)
    }
  }

  # 3. tmp.path + tmp.name
  # tmp.path
  if (!is_empty(arg[["tmp.path"]])) {
    tmp.path <- arg[["tmp.path"]]
    arg[["tmp.path"]] <- NULL
    tmp.path <- gsub("[\\/]+", "/", tmp.path)
  }
  # if (is.null(tmp.path) || length(tmp.path) == 0) {
  if (is_empty(tmp.path)) {
    tmp.path <- fp(get_scenarios_path(), scen@name, "script")
    # if (!is.null(arg[["solver"]])) {
    #   tmp.path <- fp(tmp.path, arg[["solver"]]$name)
    # }
  }

  # tmp.name
  if (!is_empty(arg[["tmp.name"]])) {
    tmp.name <- arg[["tmp.name"]]
    arg[["tmp.name"]] <- NULL
  } else if (!is_empty(arg[["solver"]])) {
    if (!is_empty(arg[["solver"]]$name)) {
      tmp.name <- arg[["solver"]]$name
    } else {
      tmp.name <- paste(arg[["solver"]]$lang, arg[["solver"]]$solver, sep = "_")
    }
  # } else if (isTRUE(arg[["tmp.del"]])) {
    # tmp.name <- format(Sys.time(), "%Y%m%d%H%M%S%Z", tz = "UTC")
  } else {
    # tmp.name <- NULL
    tmp.name <- format(Sys.time(), "%Y%m%d%H%M%S%Z", tz = "UTC")
  }

  tmp.dir <- fp(tmp.path, tmp.name)
  tmp.dir <- gsub("[\\/]+", "/", tmp.dir)
  arg[["tmp.dir"]] <- tmp.dir
  return(arg)
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
  # tmp.dir - solver working directory
  # echo = TRUE - print working data
  # open.folder = FALSE - open folder before the run
  # show.output.on.console = FALSE & invisible = FALSE arg for command system
  # only.listing = FALSE (!depreciated?) generate only listing file (works for gams only)
  # read.solution = TRUE read result
  # tmp.del delete results
  # browser()
  arg <- list(...)
  # if (is_empty(arg[["tmp.dir"]])) arg[["tmp.dir"]] <- NULL
  # if (is_empty(arg[["solver"]])) arg[["solver"]] <- NULL
  if (is_empty(arg[["read.solution"]])) arg[["read.solution"]] <- FALSE
  if (is_empty(arg[["write"]])) arg[["write"]] <- FALSE

  # arg <- get_tmp_dir(scen, arg)
  # if (is.null(arg$tmp.dir)) {
  #   browser()
  #   stop("tmp.dir is not specified")
  # }
  # browser()
  if (is_empty(arg$echo)) arg$echo <- TRUE
  if (is_empty(arg$solver)) {
    if (is_empty(scen@settings@solver)) {
      # arg$solver <- list(lang = "PYOMO")
      arg$solver <- get_default_solver()
      scen@settings@solver <- arg$solver
    } else {
      arg$solver <- scen@settings@solver
    }
    # scen@settings@solver <- list(lang = "PYOMO")
  } else if (is.character(arg$solver)) {
    scen@settings@solver <- list(name = arg$solver, lang = arg$solver)
    arg$solver <- scen@settings@solver
  } else if (is.list(arg$solver)) {
    scen@settings@solver <- arg$solver
  }
  if (!identical(scen@settings@solver, arg$solver)) browser() #!!! Debug
  if (is_empty(arg$open.folder)) arg$open.folder <- FALSE
  if (is_empty(arg$show.output.on.console)) arg$show.output.on.console <- FALSE
  # if (is.null(arg$invisible)) arg$invisible <- FALSE
  if (is_empty(arg$read.solution)) arg$read.solution <- TRUE
  if (is_empty(arg$tmp.del)) arg$tmp.del <- arg$read.solution
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
  if (is_empty(arg$run)) arg$run <- TRUE
  if (is_empty(arg$n.threads)) arg$n.threads <- 1

  # if (is.null(arg$onefile)) arg$onefile <- FALSE
  # if (!is.null(arg$dir.result)) {
  #   warning("solve_model: parameter `dir.result` is depreciated, use `tmp.dir` instead")
  #   if (is.null(arg$tmp.dir)) {
  #     arg$tmp.dir <- arg$dir.result
  #   } else {
  #     stop("check `dir.result` and `tmp.dir` - only one should be used")
  #   }
  # } else {
  #   # temporary - will be depreciated
  #   arg$dir.result <- arg$tmp.dir
  # }
  # browser()
  arg <- get_tmp_dir(scen, arg)

  if (is.null(scen)) {
    if (arg$interpolate | arg$write) {
      stop("scenario object not found")
    }
  } else {
    scen@misc$tmp.dir <- arg$tmp.dir
    tmp_name <- scen@name
  }
  # arg$dir.result <- .fix_path(arg$dir.result)
  # arg$tmp.dir <- .fix_path(arg$tmp.dir)
  # if (!is.null(scen)) scen@misc$tmp.dir <- .fix_path(scen@misc$tmp.dir)

  # if (is.null(arg$tmp.dir)) {
  #   arg$tmp.dir <- .fp(
  #     fp(getwd(), "solwork"),
  #     paste(arg$solver$lang, tmp_name, # scen@name,
  #       format(Sys.time(), "%Y%m%d%H%M%S%Z", tz = Sys.timezone()),
  #       sep = "_"
  #     )
  #   )
  # }
  # arg$dir.result <- arg$tmp.dir

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
  # dir.create(arg$tmp.dir, recursive = TRUE, showWarnings = FALSE)
  # if (arg$open.folder) shell.exec(arg$tmp.dir)
  if (is.null(arg$tmp.dir) || length(arg$tmp.dir) == 0) {
    stop("tmp.dir is not specified")
  }
  if (!isTRUE(arg$write) & !dir.exists(arg$tmp.dir)) {
    stop(paste(
      "tmp.dir does not exist:\n  ",
      arg$tmp.dir, "\n  ",
      "hint: run 'write_script' for the specified solver and 'tmp.dir'"
      ))
  }
  if (arg$write) {
    dir.create(arg$tmp.dir, recursive = TRUE, showWarnings = FALSE)
    if (arg$echo) cat("Solver directory: ", arg$tmp.dir, "\n")
    if (arg$echo) cat("Writing files: ")
    solver_solver_time <- proc.time()[3]
    if (any(grep("^gams$", scen@settings@solver$lang, ignore.case = TRUE))) {
      # if (is.null(arg$trim)) arg$trim <- FALSE
      # scen <- .write_model_GAMS(arg, scen, trim = arg$trim)
      scen <- .write_model_GAMS(arg, scen, trim = FALSE)
    } else if (any(grep("^(glpk|cbcb)$", scen@settings@solver$lang,
      ignore.case = TRUE
    ))) {
      if (isTRUE(arg$glpk_writer_v2)) {
        scen <- .write_model_GLPK_CBC2(arg, scen)
      } else {
        scen <- .write_model_GLPK_CBC(arg, scen)
      }
    } else if (any(grep("^pyomo", scen@settings@solver$lang, ignore.case = TRUE))) {
      scen <- .write_model_PYOMO(arg, scen)
    } else if (any(grep("^jump$", scen@settings@solver$lang, ignore.case = TRUE))) {
      scen <- .write_model_JuMP(arg, scen)
    } else {
      stop("Unknown solver ", scen@settings@solver$lang)
    }

    ## Write solver parameter
    nn <- grep("^(inc[1-5]|files|code[[:digit:]]*)$",
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
    write.csv(tmp, file = fp(arg$tmp.dir, "solver"), row.names = FALSE)

    if (arg$echo) {
      cat(round(proc.time()[3] - solver_solver_time, 2), "s\n", sep = "")
      flush.console()
    }
    scen@status$script <- TRUE
  }
  # browser()
  if (isTRUE(arg$run)) .call_solver(arg, scen)
  if (isTRUE(arg$read.solution) && isTRUE(arg$run)) scen <- read_solution(scen)

  return(scen)
}

.call_solver <- function(arg, scen) {
  # browser()
  HOMEDIR <- getwd()
  if (!arg$run) {
    return()
  }
  if (arg$echo) cat("Starting ", scen@settings@solver$lang, "\n")
  gams_run_time <- proc.time()[3]

  # Remote backend: submit to NEOS instead of running a local solver process.
  # The WRITE phase already produced the model in arg$tmp.dir; .neos_call_solver
  # submits it, waits, and drops the returned solution CSVs into tmp.dir/output/
  # so read_solution() is unchanged. (Pyomo-on-NEOS is NOT here: it is an
  # ordinary local `python` run whose remoteness lives inside the generated .py.)
  if (identical(scen@settings@solver$backend, "neos")) {
    rs <- .neos_call_solver(arg, scen)
    if (rs != 0) stop(paste("NEOS solve error code", rs))
    if (arg$echo) cat("", round(proc.time()[3] - gams_run_time, 2), "s\n", sep = "")
    return(invisible())
  }

  tryCatch(
    {
      setwd(arg$tmp.dir)
      if (.Platform$OS.type == "windows") {
        if (arg$invisible) {
          cmd <- ""
        } else {
          cmd <- if_else(interactive(),  "cmd /k", "")
        }
        rs <- system(paste(cmd, scen@settings@solver$cmdline), #' gams energyRt.gms', arg$gamsCompileParameter),
          invisible = arg$invisible, wait = arg$wait
          # show.output.on.console = arg$show.output.on.console
        )
      } else {
        # browser()
        rs <- system(paste(scen@settings@solver$cmdline),
          # invisible = arg$invisible,
          wait = arg$wait
          # show.output.on.console = arg$show.output.on.console
        )
      }
      setwd(HOMEDIR)
    },
    interrupt = function(x) {
      if (arg$tmp.del) unlink(arg$tmp.dir, recursive = TRUE)
      setwd(HOMEDIR)
      stop("Solver has been interrupted")
    },
    error = function(x) {
      if (arg$tmp.del) unlink(arg$tmp.dir, recursive = TRUE)
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
  gms <- file.path(arg$tmp.dir, "energyRt.gms")
  if (!file.exists(gms)) stop("NEOS: 'energyRt.gms' not found in ", arg$tmp.dir)

  # Inline model + text data into one self-contained .gms (flat NEOS workspace).
  model <- neos_gams_inline(gms, arg$tmp.dir, flatten = TRUE)
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
  zf <- file.path(arg$tmp.dir, "neos-output.zip")
  writeBin(zb, zf)
  ux <- file.path(arg$tmp.dir, "_neos_unzip")
  unlink(ux, recursive = TRUE); dir.create(ux, showWarnings = FALSE)
  utils::unzip(zf, exdir = ux)
  outdir <- file.path(arg$tmp.dir, "output")
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

# Functions and methods to solve model and scenario objects

# solve_model <- function() {}

# solve_scenario <- function() {}

# solver_link <- function() {} # replace .executeScenario


