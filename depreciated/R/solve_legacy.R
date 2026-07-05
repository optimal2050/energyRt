# =============================================================================#
# ARCHIVED legacy solver entry points: solve_model(), solve_scenario(), and the
# `solve` S4 methods. Split out of R/solve.R during the legacy-pipeline
# retirement. The shared write/run/read framework (get_tmp_dir / .executeScenario
# / .call_solver) stays live in R/solve.R. Repurposed public names live in
# R/legacy_api_shims.R (routing to the new interp_mod/solve_mod/solve_scen).
# =============================================================================#

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
  # browser()

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

  # browser()
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
