# Solve model and scenario objects ####
# Functions and methods. Multiple methods in this file aim adopting the
# generic `base::solve(a, b, ...)` method to `solve(obj, name, ...)`

#' Functions and methods to solve model and scenario objects
#'
#' The function interpolates model, writes the script in a directory, runs the external software to solve the model, reads the solution results, and returns a scenario object with the solution.
#'
#' @param obj model or scenario object
#' @param name character name of scenario to return
#' @param solver a character or list with solver settings
#' @param tmp.dir character path to temporary directory
#' @param tmp.del logical delete temporary directory after the run
#' @param ...
#'
#' @seealso [read_solution()]
#'
#' @rdname solve
#' @return
#' When the first argument is a model object, the function
#' @export
solve_model <- function(
    obj,
    name = NULL,
    # name = paste("scen", obj@name, sep = "_"),
    # solver = NULL,
    # tmp.path = fp(getwd(), "/solwork"),
    # tmp.time = format(Sys.time(), "%Y%m%d%H%M%S%Z", tz = Sys.timezone()),
    # tmp.name = paste(solver, obj@name, name, tmp.time, sep = "_"),
    # path = NULL,
    # sol.dir = NULL,
    # sol.del = TRUE,
    # tmp.dir = NULL,
    # tmp.del = TRUE,
    # force = FALSE,
    ...) {
  # 'solve*' return a scenario object, objects from '...' are either
  # passed to 'interpolate' or used to overwrite the newly created
  # scenario object.
  browser()

  if (!inherits(obj, c("model", "scenario")))
    stop("The first argument must be either model or scenario object")

  arg <- list(..., name = name)
              # name = name, solver = solver, path = path,
              # tmp.del = tmp.del, tmp.dir = tmp.dir)
  # if (!is.null(arg$name)) name <- arg$name
  if (is.null(arg$tmp.del)) arg$tmp.del <- TRUE
  if (is.null(arg$force)) arg$force <- FALSE

  # browser()

  if (inherits(obj, "scenario")) {
    # stop("The first argument must be a model object")
    if (obj@status$optimal) {
      if (!arg$force) {
        message(
          "The scenario is already solved to optimal.\n",
          "Use 'force = TRUE' to solve it again.\n",
          "Use 'interpolate(..., force = TRUE)' to interpolate it again.\n")
        return(obj)
      } else {
        # message(".")
        if (is.null(arg$name)) arg$name <- obj@name
      }
    }
  }

  if (is.null(arg$name)) arg$name <- paste("scen", obj@name, sep = "_")
  # arg <- list(..., name = name, solver = solver, path = path,
  #             tmp.del = tmp.del, tmp.dir = tmp.dir)

  # arg$solver <- solver
  # browser()
  new_solver <- FALSE
  if (is.null(arg$solver)) {
    # if (inherits(obj, "model") && !is.null(obj@config@solver)) {
    #   arg$solver <- obj@config@solver
    # } else
    if (inherits(obj, "scenario") && !is.null(obj@settings@solver)) {
      # new_solver <- FALSE
      arg$solver <- obj@settings@solver
    } else {
      new_solver <- TRUE
      arg$solver <- get_default_solver()
    }
  }
  if (is.null(arg$path)) {
    if (inherits(obj, "scenario")) {
      if (is_empty(obj@path)) {
        arg$path <- fp(get_scenarios_path(), make_scenario_dirname(obj))
      } else {
        arg$path <- obj@path
      }
    } else {
      # arg$path <- fp(get_scenarios_path(), arg$name)
      arg$path <- NULL
    }
  }
  # Filter from '...' objects to pass to 'interpolate'
  obj_to_interpolate <- c(
    "repository", "list", newRepository("")@permit, # model data
    "config", "settings", "calendar", "horizon" # settings
    ) |> unique()

  ii <- names(arg) %in% c(
    obj_to_interpolate, "data",
    "name", "desc", "misc", "inMemory", "path", # scenario
    "force",
    # settings
    "discountFirstYear", "optimizeRetirement", "defVal", "interpolation",
    "debug", "sourceCode", "region"
    # "solver" # !!! add later
    )
  ii <- ii |
    (sapply(arg, function(x) class(x)[1]) %in% c(
    c(obj_to_interpolate, "list"))
    )

  browser()
  # Interpolate if necessary
  to_interpolate <- TRUE
  if (inherits(obj, "scenario")) {
    if (isTRUE(obj@status$interpolated)) to_interpolate <- FALSE
    if (new_solver) to_interpolate <- TRUE
    if (isTRUE(arg$force)) to_interpolate <- TRUE
  }

  if (to_interpolate) {
    scen <- do.call(interpolate, c(list(object = obj), arg[ii]))
  } else {
    scen <- obj; rm(obj)
  }

  # scen <- interpolate(obj, arg[ii])
  # browser()
  # the remaining objects will be passed to .executeScenario
  arg <- c(arg[!ii], arg["solver"], force = arg[["force"]])
  arg$interpolate <- FALSE
  arg$write <- !scen@status$script

  # get name for the tmp.dir
  arg$name <- scen@name

  arg <- get_tmp_dir(scen, arg)
  # tmp.dir <- arg$tmp.dir
  # tmp.del <- arg$tmp.del

  # Run the scenario
  solve.time.start <- proc.time()[3]
  if (is.null(arg$echo)) arg$echo <- TRUE

  if (is.null(arg$name)) {
    name <- paste("scen", scen@name, sep = "_")
    warning('Scenario name is not specified, using default name: ', arg$name)
  }
  # browser()
  # if (is.null(arg$tmp.dir) || arg$tmp.dir == "") {
  if (is_empty(arg$tmp.dir)) {
    stop("Incorrect directory tmp.dir: ", arg$tmp.dir)
  }
  if (isTRUE(arg$echo)) {
    tmp.msg <- sub(getwd(), "", arg$tmp.dir)
    cat("Solver directory: ", tmp.msg, "\n")
    cat("Starting time: ", format(Sys.time()), "\n")
  }
  # scen <- interpolate(obj, name = name)
  # browser()
  arg$scen <- scen
  # arg$name <- scen@name
  # arg$solver <- solver
  # arg$tmp.dir <- tmp.dir
  # arg$tmp.del <- tmp.del
  if (is.null(arg$read.solution)) {
    if (is.null(arg$wait) || isTRUE(arg$wait)) {
      arg$read.solution <- TRUE
    } else {
      arg$read.solution <- FALSE
    }
  }
  # browser()
  scen <- do.call(.executeScenario, arg)
  # scen <- .executeScenario(scen,
  #   name = name, solver = solver,
  #   tmp.dir = tmp.dir, tmp.del = tmp.del, ..., read.solution = TRUE
  # )
  if (arg$tmp.del) unlink(arg$tmp.dir, recursive = TRUE)
  scen
}

# a function to use in solve methods
solve.model <- function(a, b, ...) {
  arg <- list(...)
  # browser()
  if (missing(b)) {
    if (!is.null(arg$name)) {
      b <- arg$name
      arg$name <- NULL
    } else {
      b <- NULL
    }
  }
  if (!is.null(arg$obj)) stop("'obj' is 'a' argument in `solve(a, b, ..)` method")
  if (!is.null(arg$name)) stop("'name' is 'b' argument in `solve(a, b, ..)` method")
  arg$obj <- a
  if (!is.null(b)) arg$name <- b
  arg$interpolate <- TRUE
  arg$write <- TRUE
  # browser()
  do.call(solve_model, arg)
}

## solve(model, character) ####
#' @rdname solve
#' @export
setMethod("solve", signature(a = "model", b = "character"), solve.model)

## solve(model, missing) ####
#' @export
#' @noRd
setMethod("solve", signature(a = "model", b = "missing"), solve.model)

# .S3method("solve", "model", .solve_model)

#' Solve scenario
#'
#' @export
#' @rdname solve
solve_scenario <- function(
    obj,
    name = obj@name,
    # solver = obj@settings@solver,
    # path = obj@path,
    # tmp.dir = obj@misc$tmp.dir,
    # tmp.del = FALSE,
    # force = FALSE,
    ...) {
  # browser()
  arg <- list(name = name, ...)
  if (obj@status$optimal) {
    if (isFALSE(arg$force)) {
      message("The scenario is already solved to optimal.\nUse 'force = TRUE' to solve it again")
      return(obj)
    }
  }
  # if (is_empty(arg$solver)) arg$solver <- obj@settings@solver
  # if (is_empty(arg$path)) arg$path <- obj@path
  if (is_empty(arg$tmp.del)) arg$tmp.del <- FALSE
  # if (is_empty(arg$force)) arg$force <- FALSE
  # if (is_empty(arg$tmp.dir)) arg <- get_tmp_dir(obj, arg)
  arg$obj <- obj

  do.call(solve_model, arg)

  # solve_model(obj,
  #             name = name,
  #             solver = obj@settings@solver,
  #             path = obj@path,
  #             tmp.dir = obj@misc$tmp.dir,
  #             tmp.del = FALSE,
  #             force = FALSE,
  #             ...)
}


# solve_scenario <- function(obj = NULL, tmp.dir = NULL, solver = NULL, ...) {
#   scen <- obj
#   browser()
#   arg <- list(...)
#   if (is.null(tmp.dir)) {
#     if (is.null(scen)) {
#       stop("At least one of two parameters ('scen' or 'tmp.dir') should be specified")
#     } else {
#       tmp.dir <- scen@misc$tmp.dir
#     }
#   } else {
#     if (!is.null(scen)) scen@misc$tmp.dir <- tmp.dir
#   }
#   if (is.character(solver)) solver <- list(lang = solver)
#   solv_par <- read.csv(paste0(.fix_path(tmp.dir), "solver"), stringsAsFactors = FALSE)
#   solver_list <- list()
#   for (i in seq_len(nrow(solv_par))) {
#     tmp <- solv_par[i, "value"]
#     if (tmp %in% c("TRUE", "FALSE")) tmp <- (tmp == "TRUE")
#     solver_list[[solv_par[i, "name"]]] <- tmp
#   }
#   browser()
#   if (!is.null(scen) && !is.null(scen@settings@solver)) {
#     for (i in grep("^(inc[1-5]|files)$", names(scen@settings@solver),
#       value = TRUE, invert = TRUE
#     )) {
#       solver_list[[i]] <- scen@settings@solver[[i]]
#     }
#   }
#   for (i in grep("^(inc[1-5]|files)$", names(solver), value = TRUE, invert = TRUE)) {
#     solver_list[[i]] <- solver[[i]]
#   }
#   if (is.null(scen)) {
#     scen <- new("scenario")
#   }
#
#   arg$scen <- scen
#   arg$tmp.dir <- tmp.dir
#   arg$solver <- solver_list
#   arg$run <- TRUE
#   arg$write <- FALSE
#   do.call(.executeScenario, arg)
#
#   # .executeScenario(
#   #   scen = scen, run = TRUE, solver = solver_list,
#   #   tmp.dir = tmp.dir, write = FALSE, ...
#   # )
# }

# a function to use in solve methods
solve.scenario <- function(a, b, ...) {
  # browser()
  if (missing(b)) b <- NULL
  arg <- list(...)
  if (!is.null(arg$obj)) stop("'obj' is 'a' argument in `solve(a, b, ..)` method")
  if (!is.null(arg$name)) stop("'name' is 'b' argument in `solve(a, b, ..)` method")
  arg$obj <- a
  if (!is.null(b)) arg$name <- b else arg$name <- arg$obj@name
  if (is_empty(arg[["run"]])) arg$run <- TRUE
  do.call(solve_scenario, arg)
}

## solve(scenario, character) ####
#' @rdname solve
#' @export
setMethod("solve", signature(a = "scenario", b = "character"), solve.scenario)

## solve(scenario, missing) ####
#' @export
#' @noRd
setMethod("solve", signature(a = "scenario", b = "missing"), solve.scenario)

## solve(missing, missing) ####
#' @export
#' @noRd
setMethod("solve", signature(a = "missing", b = "missing"), function(...) {
  # browser()
  arg <- list(...)
  if (is.null(arg$obj)) do.call(NextMethod, arg)
  if (is(arg$obj, "scenario")) {
    return(do.call(solve_scenario, arg))
  } else if (is(arg$obj, "model")) {
    return(do.call(solve_model, arg))
  } else {
    NextMethod(arg)
  }
})

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
  browser()
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
  browser()
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
      scen <- .write_model_GLPK_CBC(arg, scen)
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
  browser()
  HOMEDIR <- getwd()
  if (!arg$run) {
    return()
  }
  if (arg$echo) cat("Starting ", scen@settings@solver$lang, "\n")
  gams_run_time <- proc.time()[3]
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

.generate_gpr_gams_file <- function(tmp.dir) {
  # Generates GAMS-project file
  fn <- file(paste(tmp.dir, "/energyRt_project.gpr", sep = ""), "w")
  cat(c(
    "[RP:MDL]", "1=", "", "[OPENWINDOW_1]",
    "FILE0=energyRt.gms",
    "FILE1=energyRt.lst",
    "FILE2=input/data.gdx",
    "FILE2=output/output.gdx",
    # gsub('[/][/]*', '\\\\', paste('FILE0=', tmp.dir, '/energyRt.gms', sep = '')),
    # gsub('[/][/]*', '\\\\', paste('FILE1=', tmp.dir, '/energyRt.lst', sep = '')),
    "", "MAXIM=1",
    "TOP=50", "LEFT=50", "HEIGHT=400", "WIDTH=400", ""
  ), sep = "\n", file = fn)
  close(fn)
}
