#' Write scenario object as a Python, Julia, GAMS, or MathProg script with data files to a directory
#'
#' @param object scenario object, must be interpolated
#' @param x scenario object, as `scen` (the `write_sc()` spelling).
#' @param solver.dir character, an external directory to write into; default
#'   (`NULL`) writes into the run's `solver/` directory under the scenario
#'   (`tmp.dir` is the deprecated alias).
#' @param solver list of character with solver specification.
#' @param ... additional solver parameters
#' @family write scenario
#' @rdname write
#' @seealso [solve()] to run the script, solve the scenario. [read_solution] to read model solution.
#'
#' @export
#' @examples
#' \dontrun{
#' open_project(tempdir())
#'
#' ek   <- topia$modules$electricity$R3
#' mod  <- newModel("TOPIA", data = ek$repo,
#'                  calendar = topia$modules$calendars$s4_h24,
#'                  region = ek$regions,
#'                  horizon = topia$modules$horizons$base,
#'                  discount = 0.05)
#' scen <- interpolate_model(mod, name = "BASE")
#'
#' # write the backend's model and data files without solving, to read them
#' # or to run the solver by hand
#' write_script(scen, solver = solver_options$glpk, solver.dir = tempdir())
#' }
write_script <- function(object, solver.dir = NULL, solver = NULL, ...) {
  if (!isTRUE(object@status$interpolated)) {
    stop("Scenario must be interpolated before writing the script.")
  }
  # deprecated aliases in `...` are mapped by .executeScenario itself
  .executeScenario(object,
                   solver.dir = solver.dir, solver = solver,
                   run = FALSE, write = TRUE, ...)
}

#' @export
#' @rdname write
write_sc <- function(x, solver.dir = NULL, solver = NULL, ...) {
  write_script(x, solver.dir, solver, ...)
}

# @method write scenario
# @family write scenario
# @rdname write
# @name write
#
# @export
# setMethod("write", signature(x = "scenario"), definition = write_script)



# write <- function(scen, ...) UseMethod("write")




.write_inc_solver <- function(scen, arg, def_inc_solver, type, templ) {
  if (!is.null(scen@settings@solver$inc_solver) && !is.null(scen@settings@solver$solver)) {
    stop("have to define only one argument from scen@settings@solver$inc_solver & scen@settings@solver$solver")
  }
  if (!is.null(scen@settings@solver$solver)) {
    scen@settings@solver$inc_solver <- gsub(templ, scen@settings@solver$solver, def_inc_solver)
  }
  if (is.null(scen@settings@solver$inc_solver) && is.null(scen@settings@solver$solver)) {
    scen@settings@solver$inc_solver <- def_inc_solver
  }
  fn <- file(file.path(arg$solver.dir, paste0("inc_solver", type)), "w")
  cat(scen@settings@solver$inc_solver, file = fn, sep = "\n")
  close(fn)
}

# writes "include" files to tmp.dir ("inc1" ... "inc5" - see GAMS code)
.write_inc_files <- function(arg, scen, type) { # renamed .add_five_includes
  if (is.null(type)) {
    fl <- c(names(scen@settings@solver$files))
    for (i in 1:5) {
      if (is.null(scen@settings@solver[[paste0("inc", i)]])) {
        fl <- c(fl, paste0("inc", i))
      }
    }
    if (is.null(fl)) {
      tmp <- paste0(
        "There is (are) ", length(fl),
        " files, that not suitable for lang ", scen@settings@solver@lang,
        '. File(s) list: "', paste0(fl, collapse = '", "'), '"'
      )
      stop(tmp)
    }
  }
  for (i in 1:5) {
    fn <- file(file.path(arg$solver.dir, paste0("inc", i, type)), "w")
    cat(scen@settings@solver[[paste0("inc", i)]], sep = "\n", file = fn)
    close(fn)
  }
  for (i in names(scen@settings@solver$files)) {
    fn <- file(file.path(arg$solver.dir, i), "w")
    cat(scen@settings@solver$files[[i]], sep = "\n", file = fn)
    close(fn)
  }
}


# Sets, parameters, + to use in write_* and interpolation functions ####
.set_al <- c("acomm", "stg", "trade", "expp", "imp", "tech", "dem", "sup", "weather", "region", "year", "timeslice", "group", "comm", "cns", "stgp", "tradep", "exppp", "impp", "techp", "demp", "supp", "weatherp", "regionp", "yearp", "timeslicep", "groupp", "commp", "cnsp", "stge", "tradee", "exppe", "impe", "teche", "deme", "supe", "weathere", "regione", "yeare", "timeslicee", "groupe", "comme", "cnse", "stgn", "traden", "exppn", "impn", "techn", "demn", "supn", "weathern", "regionn", "yearn", "timeslicen", "groupn", "commn", "cnsn", "src", "dst", "levy")
.alias_set <- c("ca", "st1", "t1", "e", "i", "t", "d", "s1", "wth1", "r", "y", "s", "g", "c", "cn1", "st1p", "t1p", "ep", "ip", "tp", "dp", "s1p", "wth1p", "rp", "yp", "sp", "gp", "cp", "cn1p", "st1e", "t1e", "ee", "ie", "te", "de", "s1e", "wth1e", "re", "ye", "se", "ge", "ce", "cn1e", "st1n", "t1n", "en", "in", "tn", "dn", "s1n", "wth1n", "rn", "yn", "sn", "gn", "cn", "cn1n", "src", "dst", "lv")
names(.alias_set) <- .set_al

.aliasName <- function(x) {
  if (!all(x %in% .set_al)) {
    cat("Unknown .set_al\n")
    stop("Unknown set")
  }
  .alias_set[x]
}


.fremset <- c("comm", "stg", "trade", "expp", "imp", "tech", "dem", "sup", "weather", "region", "year", "timeslice", "group", "comm", "cns", "stg", "trade", "expp", "imp", "tech", "dem", "sup", "weather", "region", "year", "timeslice", "group", "comm", "cns", "stg", "trade", "expp", "imp", "tech", "dem", "sup", "weather", "region", "year", "timeslice", "group", "comm", "cns", "stg", "trade", "expp", "imp", "tech", "dem", "sup", "weather", "region", "year", "timeslice", "group", "comm", "cns", "region", "region", "levy")
names(.fremset) <- .set_al

.removeEndSet <- function(x) {
  .fremset[x]
}
