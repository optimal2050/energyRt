#' An S4 class to specify a model variable
#'
#' @description
#' The counterpart of [class-parameter]: a `parameter` carries the model's input
#' data, a `variable` its solved output. Every variable declared by the model
#' has one of these, pre-populated from the baked specification when a `modOut`
#' is created and filled with solved values by `read_solution()` -- exactly as
#' `modInp@parameters` is populated from `data-raw/modInp.yml`.
#'
#' The specification itself is composed at build time in `data-raw/DATASET.R`:
#' dimensions, gating map and description come from the GAMS source via gams2x,
#' the emitted column names and the positive/free declaration are parsed out of
#' `glpk/energyRt.mod`, and `role` / `unit` / `origin` come from the
#' `data-raw/variables.yml` overlay.
#'
#' The class and its methods are not intended for direct use; read values with
#' [getData()].
#'
#' @slot name character, the variable's name in the model.
#' @slot desc character, one-line description.
#' @slot dimSets character, the variable's dimensions **as the model declares
#'   them**, duplicates preserved (`vTradeIr` is
#'   `trade, comm, region, region, year, timeslice`). Byte-identical to
#'   `.variable_set`, which `newCosts()` and `newConstraint()` consume
#'   positionally against the GAMS declaration.
#' @slot colNames character, the column names the variable is actually written
#'   out with. Differs from `dimSets` only where a dimension repeats and the
#'   writers disambiguate it -- `src`/`dst` for `vTradeIr`, `yearp` for
#'   `vTechRetiredNewCap`.
#' @slot gate character, the name of the map that gates the variable's domain
#'   (the `$`-condition), or `""` when it is unconditional.
#' @slot role character, what the variable is: `source`, `sink`, `flow`,
#'   `activity`, `balance`, `stock`, `capacity` or `cost`. A `stock` is a level
#'   at a point in time and is never summed over timeslices.
#' @slot unit character, the KIND of quantity (`commodity`, `activity`,
#'   `capacity`, `currency`), not a unit string -- real units are per-object
#'   here and are resolved against the model.
#' @slot positive logical, whether the model declares the variable non-negative.
#' @slot origin character, `solver` or `computed` (built in R by `read.R`, so
#'   never expected in solver output).
#' @slot data data.frame, the solved values.
#' @slot misc list, `path` / `inMemory` / `onDisk` when stored on disk.
#'
#' @name class-variable
#'
#' @include class-modelData.R
#' @family variable
#' @export
setClass(
  "variable",
  contains = "modelData",
  representation(
    name = "character",
    desc = "character",
    dimSets = "character",
    colNames = "character",
    gate = "character",
    role = "character",
    unit = "character",
    positive = "logical",
    origin = "character",
    data = "data.frame",
    misc = "list"
  ),
  prototype(
    name = NA_character_,
    desc = "",
    dimSets = character(),
    colNames = character(),
    gate = "",
    role = NA_character_,
    unit = NA_character_,
    positive = FALSE,
    origin = "solver",
    data = data.frame(),
    misc = list()
  )
)

.VARIABLE_ROLES <- c("source", "sink", "flow", "activity", "balance", "stock",
                     "capacity", "cost")
.VARIABLE_UNITS <- c("commodity", "activity", "capacity", "currency")

# The `@data` column skeleton. `parameter`'s builder cannot be reused: it
# selects columns out of a template frame keyed by dimension name, which turns
# a repeated `year` into `year.1` and can never produce `yearp`. Built from
# `colNames` instead, with each column's TYPE taken from the dimension it
# stands for -- `vTechRetiredNewCap`'s `yearp` column is integer because its
# dimension is `year`.
.var_skeleton <- function(colNames, dimSets) {
  if (length(colNames) != length(dimSets)) {
    stop("Variable skeleton: ", length(colNames), " column name(s) for ",
         length(dimSets), " dimension(s).")
  }
  if (anyDuplicated(colNames)) {
    stop("Variable skeleton: duplicated column name(s) ",
         paste(unique(colNames[duplicated(colNames)]), collapse = ", "),
         ". Column names disambiguate repeated dimensions and must be unique.")
  }
  cols <- lapply(dimSets, function(d) {
    if (d %in% c("year", "yearp")) integer() else character()
  })
  names(cols) <- colNames
  cols$value <- numeric()
  data.table::as.data.table(cols)
}

#' Create a `variable` object
#'
#' @param name character, the variable's name.
#' @param dimSets character, model dimensions, duplicates preserved.
#' @param colNames character, emitted column names; defaults to `dimSets` when
#'   there is nothing to disambiguate.
#' @param desc,gate,role,unit,positive,origin see [class-variable].
#' @param declared logical, `FALSE` for a variable the specification does not
#'   know about (accepted from solver output with a warning rather than
#'   dropped). Recorded in `@misc`.
#' @returns a `variable` object with an empty, correctly-typed `@data`.
#' @family variable
#' @noRd
newVariable <- function(name, dimSets = character(), colNames = NULL,
                        desc = "", gate = "", role = NA_character_,
                        unit = NA_character_, positive = FALSE,
                        origin = "solver", declared = TRUE) {
  if (!is.character(name) || length(name) != 1 || !check_name(name)) {
    stop('Wrong name: "', name, '"')
  }
  dimSets <- as.character(dimSets)
  unknown <- setdiff(dimSets, .dimSets)
  if (length(unknown)) {
    stop("Unknown dimSets: ", paste(unknown, collapse = ", "),
         "\nVariable: ", name)
  }
  if (is.null(colNames)) colNames <- .dedup2(dimSets)
  if (!is.na(role) && !role %in% .VARIABLE_ROLES) {
    stop("Variable ", name, ": unknown role '", role, "'.")
  }
  if (!is.na(unit) && !unit %in% .VARIABLE_UNITS) {
    stop("Variable ", name, ": unknown unit '", unit, "'.")
  }
  if (!origin %in% c("solver", "computed")) {
    stop("Variable ", name, ": origin must be 'solver' or 'computed'.")
  }
  new("variable",
      name = name, desc = as.character(desc), dimSets = dimSets,
      colNames = as.character(colNames), gate = as.character(gate),
      role = role, unit = unit, positive = isTRUE(positive), origin = origin,
      data = .var_skeleton(colNames, dimSets),
      misc = if (declared) list() else list(declared = FALSE))
}

# Disambiguate repeated dimensions by suffixing "2": c("year","year") ->
# c("year","year2"). The convention `newCosts()`, `newConstraint()` and the
# on-disk writer all use; previously open-coded in three places.
.dedup2 <- function(x) {
  i <- duplicated(x)
  x[i] <- paste0(x[i], 2)
  x
}

# Build every declared variable from the baked specification.
.variables_from_spec <- function() {
  spec <- tryCatch(.variables, error = function(e) NULL)
  if (is.null(spec)) return(list())
  out <- lapply(spec, function(s) {
    newVariable(s$name, dimSets = s$dimSets, colNames = s$colNames,
                desc = s$desc, gate = s$gate, role = s$role, unit = s$unit,
                positive = s$positive, origin = s$origin)
  })
  names(out) <- names(spec)
  out
}

# ---------------------------------------------------------------------------- #
# Filling a variable with solved values
# ---------------------------------------------------------------------------- #

#' Assign solved data to a variable
#'
#' The variable twin of `d2p()`. Deliberately NOT `d2p` itself: that method
#' hard-stops on a column mismatch, which is exactly what a solver whose output
#' header has drifted produces, and it carries `type`/`lo`/`up` handling that a
#' variable has no use for. Here a mismatch is a warning -- an otherwise good
#' solve should not be thrown away -- and the incoming columns are renamed onto
#' the declared ones.
#'
#' @param obj a `variable`.
#' @param data data.frame of solved values.
#' @param path character or NULL, on-disk location.
#' @returns the updated `variable`.
#' @noRd
d2v <- function(obj, data, path = NULL) {
  if (is.null(data)) return(obj)
  data <- as.data.frame(data)
  nm <- setdiff(colnames(data), "value")
  if (length(nm) != length(obj@colNames)) {
    warning("Variable ", obj@name, ": solver returned ", length(nm),
            " dimension column(s) (", paste(nm, collapse = ", "),
            "), the model declares ", length(obj@colNames), " (",
            paste(obj@colNames, collapse = ", "), "). Stored as returned.")
  } else if (!identical(nm, obj@colNames)) {
    colnames(data)[match(nm, colnames(data))] <- obj@colNames
  }
  # A variable file the solver wrote with a header but no rows comes back with
  # an all-NA `value` column typed `logical` (csv sniffing / an empty Arrow
  # column). That is a zero-row table, not bad data, so type it rather than let
  # `.force_value_class_df()` reject it.
  if (is.logical(data[["value"]])) data[["value"]] <- as.numeric(data[["value"]])
  data <- .force_year_class_df(data)
  data <- .force_value_class_df(data)

  if (isOnDisk(obj)) {
    if (is.null(path)) path <- getObjPath(obj)
    if (is.null(path)) {
      stop("Path to the variable ", obj@name, " is not specified.")
    }
    obj@data <- data
    obj <- obj2disk(obj)
    obj@data <- reset_slot(obj@data)
  } else {
    obj@data <- data
  }
  obj
}

#' Write solved values into a scenario's variable
#'
#' The mirror of [update_parameter()], sharing its on-disk path resolution.
#'
#' @param scen a scenario.
#' @param variable character, the variable's name.
#' @param data data.frame of solved values.
#' @param path character or NULL.
#' @returns the updated scenario.
#' @noRd
update_variable <- function(scen, variable, data, path = NULL) {
  if (is.null(data)) return(scen)
  v <- scen@modOut@variables[[variable]]
  if (is.null(v)) stop("Unknown variable: ", variable)
  if (isOnDisk(scen)) {
    if (is.null(path)) {
      path <- .resolve_slot_path(scen@modOut, v, "variables", variable)
    }
    v <- v |> setObjPath(path) |> mark_ondisk()
  }
  scen@modOut@variables[[variable]] <- d2v(v, data, path = path)
  scen
}

# Where an element of a container's list slot lives on disk. `obj2disk()` writes
# `<container path>/<slot>/<name>`, so the slot segment is not optional.
.resolve_slot_path <- function(container, element, slot, name) {
  p <- getObjPath(element)
  if (!is.null(p)) return(p)
  p <- getObjPath(container)
  if (is.null(p)) {
    stop("Cannot resolve the on-disk path for '", name, "': no path is ",
         "recorded on the ", class(container)[1], ".")
  }
  fp(p, slot, name)
}

# ---------------------------------------------------------------------------- #
# Compatibility
#
# `modOut@variables` used to be a list of data.frames, and `$` cannot be
# intercepted on a base list, so `scen@modOut@variables$vTechCap` now yields a
# `variable`. These make the common READ patterns keep working. `merge()`,
# `rbind()` and `colnames<-` are deliberately NOT covered -- those callers
# should be found and moved to `get_data_slot()`.
# ---------------------------------------------------------------------------- #

#' @export
setMethod("$", "variable", function(x, name) get_data_slot(x)[[name]])

#' @export
setMethod("[[", "variable", function(x, i, ...) get_data_slot(x)[[i]])

#' @export
setMethod("dim", "variable", function(x) dim(get_data_slot(x)))

#' @export
setMethod("names", "variable", function(x) colnames(get_data_slot(x)))

#' @export
as.data.frame.variable <- function(x, ...) as.data.frame(get_data_slot(x), ...)

#' Get one solved variable from a scenario
#'
#' @param scen a scenario.
#' @param name character, the variable's name.
#' @param data logical, return the values (default) or the `variable` object.
#' @returns a data.frame, a `variable`, or NULL if there is no such variable.
#' @family variable
#' @export
get_variable <- function(scen, name, data = TRUE) {
  v <- tryCatch(scen@modOut@variables[[name]], error = function(e) NULL)
  if (is.null(v)) return(NULL)
  if (inherits(v, "data.frame")) {
    # A scenario saved before `modOut@variables` held typed objects.
    warning("Scenario '", scen@name, "' predates the `variable` class; ",
            "re-save it with save_scenario().")
    return(if (data) v else NULL)
  }
  if (data) get_data_slot(v) else v
}
