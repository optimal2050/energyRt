.get_interpolation_rule <- function(x) {
  stopifnot(is.character(x))
  if (!exists(".defInt")) load("R/sysdata.rda")
  sapply(x, function(i) .defInt[[i]])
}

## constraint-class ####
#' An S4 class to represent a custom constraint.
#'
#' @name class-constraint
#'
#' @description
#' Class `constraint` is used to define custom constraints in the optimization problem.
#' `r lifecycle::badge("experimental")`
#' @inherit newConstraint details
#'
#' @md
#' @slot name `r get_slot_doc("constraint", "name")`
#' @slot desc `r get_slot_doc("constraint", "desc")`
#' @slot eq `r get_slot_doc("constraint", "eq")`
#' @slot for.each `r get_slot_doc("constraint", "for.each")`
#' @slot rhs `r get_slot_doc("constraint", "rhs")`
#' @slot defVal `r get_slot_doc("constraint", "defVal")`
#' @slot interpolation `r get_slot_doc("constraint", "interpolation")`
#' @slot lhs `r get_slot_doc("constraint", "lhs")`
#' @slot misc `r get_slot_doc("constraint", "misc")`
#'
#'
#' @include class-subsidy.R
#' @family class constraint policy
#' @rdname class-constraint
#' @order 1
#' @export
#'
setClass("constraint",
  representation(
    name = "character",
    desc = "character", # desc
    eq = "factor",
    for.each = "data.frame",
    rhs = "data.frame",
    defVal = "numeric",
    interpolation = "character",
    lhs = "list",
    misc = "list"
    # parameter= list() # For the future
  ),
  prototype(
    name = NULL,
    desc = "", # desc
    eq = factor("==", levels = c(">=", "<=", "==")),
    for.each = data.frame(),
    rhs = data.frame(),
    defVal = as.numeric(NA),
    interpolation = .get_interpolation_rule("rhs"),
    lhs = list(),
    # ! Misc
    misc = list()
  ),
  S3methods = FALSE
)
setMethod("initialize", "constraint", function(.Object, ...) {
  .Object
})

## summand-class #####
#' An S4 class to represent a summand in a constraint.
#'
#' @name class-summand
#'
#' @description
#' Class `summand` stores information about linear
#' terms (a multiplier and a variable)
#' in the `lhs` of the constraint class.
#' It is auto-created by `newConstraint` function
#' and is not intended to be used directly by the user.
#'
#' @md
#' @slot desc `r get_slot_doc("summand", "desc")`
#' @slot variable `r get_slot_doc("summand", "variable")`
#' @slot for.sum `r get_slot_doc("summand", "for.sum")`
#' @slot timeframe `r get_slot_doc("summand", "timeframe")`
#' @slot geoframe `r get_slot_doc("summand", "geoframe")`
#' @slot process_ `r get_slot_doc("summand", "process_")`
#' @slot classes `r get_slot_doc("summand", "classes")`
#' @slot vintage `r get_slot_doc("summand", "vintage")`
#' @slot vintage_ `r get_slot_doc("summand", "vintage_")`
#' @slot cluster `r get_slot_doc("summand", "cluster")`
#' @slot cluster_ `r get_slot_doc("summand", "cluster_")`
#' @slot mult `r get_slot_doc("summand", "mult")`
#' @slot defVal `r get_slot_doc("summand", "defVal")`
#' @slot misc `r get_slot_doc("summand", "misc")`
#'
#' @rdname class-constraint
#' @order 2
NULL

# Process-targeting slots on a summand. `classes` (not `class`) matches the
# `get_process_*(process=, classes=)` family, and `class` is a reserved S4 slot
# name. Exact names stay in `for.sum`, where a base name expands to its
# variants; these cover what `for.sum` cannot say.
.summand_target_slots <- c("process_", "classes",
                           "vintage", "vintage_", "cluster", "cluster_")

# Dimension NAMES of a variable (positions, not sets -- six variables index one
# set twice). `for.sum` is keyed by these.
#' @noRd
.summand_dims <- function(variable) {
  d <- .variable_dim[[variable]]
  s0 <- .variable_set[[variable]]
  if (is.null(d) || length(d) != length(s0)) s0 else d
}

# The one process-ish dimension of a variable, or NULL. Targeting filters act
# on this dimension.
#' @noRd
.summand_process_dim <- function(variable) {
  d <- intersect(.summand_dims(variable),
                 c("tech", "stg", "trade", "sup", "dem", "imp", "expp",
                   "process"))
  if (length(d) == 1) d else NULL
}

# `addSummand()` pre-fills every dimension the caller did not name with an
# unrestricted NA, so an entry that is NULL, empty or all-NA means "every
# member" -- not "no member". Intersecting against it without this check turns
# any filter into an empty selection.
#' @noRd
.for_sum_unrestricted <- function(v) {
  is.null(v) || length(v) == 0 || all(is.na(v))
}

# A level request that is unambiguous for some members of the variable and not
# others has no single right answer: pinning drops the members that only have
# FINER rows, summing double counts the ones that already have a row at the
# level. Refuse and name them.
#' @noRd
.stop_level_mixed <- function(res, arg, level, variable, stop_fn) {
  if (!identical(res$mode, "mixed")) return(invisible(NULL))
  stop_fn(paste0(
    arg, ' = "', level, '" is ambiguous for variable "', variable,
    '": these carry rows at MORE THAN ONE level at or under it -- ',
    paste0('"', res$missing, '"', collapse = ", "),
    '.\n  Summing would double-count them; pinning would drop the members ',
    'that have only finer rows. Name the ',
    if (identical(arg, "timeframe")) "timeslices" else "regions",
    ' explicitly in `for.sum`, split the constraint per level, or build the ',
    'level with `interpolate_model(parent_levels = ...)`.'))
}

# Summing a STATE over TIME is meaningless rather than merely absent: the sum of
# hourly storage levels is not an annual level. Pinning to the level the
# variable already occupies stays legal; summing does not.
#
# Which variables those are is declared, not listed here: `role: stock` in
# `data-raw/variables.yml` already means "a LEVEL at a point in time -- never
# summed over timeslices", and `.is_state_var()` (R/get_data.R) reads it for the
# reporting roll-up. Declaring the role covers both at once, which is the point
# of the field -- it replaced a hard-coded `.timeframe_state_vars` once already.
#
# TIME only. A stock does add across space (total energy stored in a zone, total
# reserve), and nothing declares otherwise, so `geoframe` keeps summing.
#' @noRd
.stop_level_state <- function(res, arg, level, variable, dim, stop_fn) {
  if (!identical(res$mode, "sum")) return(invisible(NULL))
  if (!identical(dim, "timeslice")) return(invisible(NULL))
  if (!isTRUE(.is_state_var(variable))) return(invisible(NULL))
  stop_fn(paste0(
    arg, ' = "', level, '" would sum variable "', variable,
    '" over timeslices, but it is declared `role: stock` -- a level at a point ',
    'in time, not a flow, so its sum has no meaning.\n  Name the ',
    'timeslices explicitly in `for.sum` and apply the statistic you mean.'))
}

# Read a targeting slot from a summand that may predate it (see
# `.upgrade_summand`); an absent slot reads as "no targeting".
#' @noRd
.summand_slot <- function(sm, name) {
  if (!methods::.hasSlot(sm, name)) return(character())
  v <- methods::slot(sm, name)
  if (is.null(v)) character() else v[!is.na(v) & nzchar(v)]
}

# Bring a `summand` created before the `timeframe` slot existed up to the
# current class definition. Old serialized objects lack the slot, so accessing
# `@timeframe` errors ("no slot of name ..."); add it (S4 slots are attributes)
# with the prototype default. Called on every summand at constraint compile time.
.upgrade_summand <- function(s) {
  if (!methods::is(s, "summand")) return(s)
  a <- names(attributes(s))
  if (!("timeframe" %in% a)) attr(s, "timeframe") <- NA_character_
  # `geoframe` is the spatial twin and post-dates `timeframe`, so an object
  # serialized between the two carries one and not the other.
  if (!("geoframe" %in% a)) attr(s, "geoframe") <- NA_character_
  # The process-targeting filters post-date both; a summand serialized before
  # them carries none, which reads as "no targeting" and behaves as before.
  for (sl in .summand_target_slots) {
    if (!(sl %in% a)) attr(s, sl) <- character()
  }
  s
}

# Upgrade every summand of every constraint in a model (in each repository) so
# legacy models serialized before the `timeframe` slot interpolate cleanly.
# Called once at the top of interp_mod(), before anything reads the constraints.
.upgrade_model_summands <- function(mod) {
  for (i in seq_along(mod@data)) {
    rp <- mod@data[[i]]
    if (!methods::is(rp, "repository")) next
    objs <- rp@data
    hit <- FALSE
    for (j in seq_along(objs)) {
      o <- objs[[j]]
      if (methods::is(o, "constraint") && length(o@lhs) > 0) {
        o@lhs <- lapply(o@lhs, .upgrade_summand)
        objs[[j]] <- o
        hit <- TRUE
      }
    }
    if (hit) mod@data[[i]]@data <- objs
  }
  mod
}

#' @rdname class-constraint
#' @export
setClass("summand",
  representation(
    desc = "character",
    variable = "character",
    for.sum = "list",
    timeframe = "character",
    geoframe = "character",
    process_ = "character",
    classes = "character",
    vintage = "character",
    vintage_ = "character",
    cluster = "character",
    cluster_ = "character",
    mult = "data.frame",
    defVal = "numeric",
    misc = "list"
    # parameter= list() # !!! consider adding custom parameters
  ),
  prototype(
    desc = NULL,
    variable = NULL,
    for.sum = list(),
    # `timeframe` pins the timeslice level the variable is taken at (e.g. "ANNUAL",
    # "SEASON"): the variable's `timeslice` dimension is restricted to that level's
    # timeslices, so summing them yields the level aggregate without double-counting
    # across levels. NA = no restriction (sum the variable's native timeslices).
    # Replaces the need for the *RY (year-resolution) aggregate variables.
    timeframe = NA_character_,
    # `geoframe` is the spatial twin: it pins the geoscale level the variable is
    # taken at (e.g. "zone", "nation"), restricting the variable's `region`
    # dimension to that level's regions. NA = no restriction (sum the
    # variable's native regions). Requires a geoscale on the model config.
    geoframe = NA_character_,
    # Process targeting, resolved against the variable's own domain by
    # `.target_filter()` (R/levy.R) -- the same resolver the levy classes use,
    # so a regex or a vintage means the same thing in both places.
    process_ = character(),
    classes = character(),
    vintage = character(),
    vintage_ = character(),
    cluster = character(),
    cluster_ = character(),
    mult = data.frame(),
    defVal = 1,
    misc = list()
  ),
  S3methods = FALSE
)


#' Create constraint object to add custom constraints to the model.
#'
#' @description
#' The function creates a new constraint object that can be used
#' to add custom constraints to the model.
#'
#' @details
#' Custom constraints extend the functionality of the model by adding
#' user-defined constraints to the optimization problem.
#' If the predefined constraints are not sufficient to describe the problem,
#' custom constraints can be used to add linear equality or inequality
#' constraints to define additional relationships between the variables.
#' In many cases this can be done without writing constraints in the GAMS,
#' Julia/JuMP, Python/Pyomo, or GLPK-MathProg languages by using the
#' `constrant` class and the `newConstraint` function.
#' To define a custom constraint with the `newConstraint` function,
#' the user needs to specify the name of the constraint, the type of the
#' relation (equality, less than or equal, greater than or equal), the
#' left-hand side (LHS) terms of the statement, and the right-hand side (RHS) value.
#' The dimension of the constraint is set by the `for.each` parameter.
#' The 'lhs' terms are defined as a list of linear terms (summands).
#' Each summand consists of a variable, a multiplier, and a set of sets
#' for which the summand is defined.
#'
#'
#' @param name `r get_slot_doc("constraint", "name")`
#' @param desc `r get_slot_doc("constraint", "desc")`
#' @param ... named or unnamed list(s) of left-hand side (LHS)
#' linear terms (summands) to define the constraint.
#' Every summand is defined as a list with the following elements:
#' - `variable` - name of the variable in the summand.
#' - `mult` - multiplier for the variable in the summand.
#' - `for.sum` - list of sets for which the summand is defined.
#' The summands can be passed as named or unnamed lists.
#' They will be added to the `lhs` slot of the constraint object
#' as linear terms of mulipliers and variables.
#' @param eq Type of the relation ('==' default, '<=', '>=')
#' @param for.each list or data.frame with sets that define the dimension of the constraint.
#' @param rhs a numeric value, list or data frame with sets and numeric values for each constraint.
#' A single value (a number, a one-element list or a one-cell data frame) is
#' the right-hand side of every row: it replaces `defVal`.
#' Note: zero values will be replaced with `replace_zerros` to avoid dropping them by the interpolation algorithms.
#' @param defVal `r get_slot_doc("constraint", "defVal")`
#' @param interpolation `r get_slot_doc("constraint", "interpolation")`
#' @param replace_zerros numeric value to replace zero values in `rhs` and `defVal`. Default is `1e-20`.
#'
#' @return Object of class `constraint`.
#' @family class constraint policy
#' @rdname newConstraint
#'
#' @examples
#' # cap total exports in 2020 at 1 unit
#' cnst <- newConstraint(
#'   name = "EXP_CAP",
#'   desc = "Total exports in 2020",
#'   eq = "<=",
#'   defVal = 1,
#'   for.each = data.frame(year = 2020L),
#'   total = list(variable = "vExportTot", for.sum = list(region = "R1"))
#' )
#' cnst
#'
#' @export
newConstraint <- function(
    name,
    desc = "",
    ...,
    eq = "==",
    for.each = NULL,
    rhs = data.frame(),
    defVal = NULL,
    interpolation = "inter",
    replace_zerros = 1e-20
    # arg = NULL
    ) {
  obj <- new("constraint")
  # stopifnot(length(eq) == 1 && eq %in% levels(obj@eq))
  if (length(eq) != 1 || !(eq %in% levels(obj@eq))) {
    stop(
      "Unrecognized 'eq' parameter. Use one of: ",
      paste0(levels(obj@eq), collapse = ", ")
    )
  }
  obj@eq[] <- eq
  # a single value as `rhs` is the right-hand side of every row, i.e. the
  # constraint's default: it replaces `defVal` (a differing `defVal` is
  # reported) and leaves no rhs table. Given as a number, a one-element list
  # or a one-cell data frame.
  rhs_one <- NULL
  if (is.numeric(rhs)) {
    if (length(rhs) != 1) {
      stop("rhs must be a single numeric value or a data.frame with sets and numeric values for each constraint.")
    }
    rhs_one <- rhs
  } else if (!is.data.frame(rhs) && is.list(rhs) && length(rhs) == 1 && length(rhs[[1]]) == 1) {
    rhs_one <- rhs[[1]]
  } else if (is.data.frame(rhs) && ncol(rhs) == 1 && nrow(rhs) == 1) {
    rhs_one <- rhs[1, 1]
  }
  if (!is.null(rhs_one)) {
    rhs_one <- as.numeric(rhs_one)
    if (!is.null(defVal) && !is.na(defVal) && !isTRUE(all.equal(as.numeric(defVal), rhs_one))) {
      warning("Constraint '", name, "': `rhs` is a single value (", rhs_one,
              ") and is the constraint's value everywhere; `defVal` (", defVal, ") is replaced.")
    }
    defVal <- rhs_one
    rhs <- data.frame()
  }
  if (is.null(defVal)) {
    warning("It is advisable to define 'defVal' parameter.")
    if (eq == "==") {
      defVal <- 0
    } else if (eq == "<=") {
      defVal <- Inf
    } else if (eq >= ">=") {
      defVal <- 0
    }
  }

  if (!is.data.frame(rhs) && is.list(rhs)) {
    tmp <- sapply(rhs, length)
    if (any(tmp[1] != tmp) || is.null(names(rhs))) {
      stop("Length of the list elements in 'rhs' must be identical.")
    }
    rhs <- as.data.frame(rhs, stringsAsFactors = FALSE)
  }
  if (!is.null(replace_zerros) && any(rhs$rhs == 0)) {
    # warning("Zero values in 'rhs' will be replaced with '", replace_zerros, "' to avoid ignoring them by the current interpolation algorithms. Use non-zero value to avoid auto-replacement and the warning. Use 'replace_zerros = NULL' to avoid replacement.")
    rhs[rhs == 0] <- replace_zerros
  }
  if (!is.null(replace_zerros) && !is.na(defVal) && defVal == 0) {
    # warning("Zero value in 'defVal' will be replaced with '", replace_zerros, "' to avoid ignoring it by the current interpolation algorithms. Use non-zero value to avoid auto-replacement and the warning. Use 'replace_zerros = NULL' to avoid replacement.")
    defVal <- replace_zerros
  }
  # TYPE vs SET
  obj@rhs <- rhs
  obj@defVal <- defVal
  obj@name <- name
  obj@desc <- desc
  obj@interpolation <- interpolation
  if (!is.null(for.each)) {
    names(for.each) <- .rename_slice_compat(names(for.each), "for.each")
    if (!is.data.frame(for.each) && is.list(for.each)) {
      tmp <- data.frame(stringsAsFactors = FALSE)
      fl_null <- sapply(for.each, is.null)
      for (i in names(for.each)[fl_null]) {
        for.each[[i]] <- NA
      }
      for (i in names(for.each)) {
        t2 <- data.frame(for.each[[i]], stringsAsFactors = FALSE)
        colnames(t2) <- i
        if (ncol(tmp) == 0) {
          tmp <- t2
        } else {
          tmp <- merge(tmp, t2)
        }
      }
      obj@for.each <- tmp
    } else if (is.data.frame(for.each)) {
      if (anyDuplicated(for.each)) {
        stop("Duplicated rows in 'for.each' parameter: ",
             for.each[duplicated(for.each), ])
      }
      obj@for.each <- for.each
    } else {
      stop("Unrecognized 'for.each' parameter. Failed to build the costraint.")
    }
  }
  # for (i in seq_along(arg)) {
  #   obj <- addSummand(obj, arg = arg[[i]])
  # }
  arg <- list(...)
  for (i in seq_along(arg)) {
    obj <- addSummand(obj, arg = arg[[i]])
  }
  obj
}

# @describeIn newConstraint
# @family constraint
# @export
addSummand <- function(
    eqt,
    variable = NULL,
    mult = data.frame(),
    for.sum = list(),
    timeframe = NA_character_,
    geoframe = NA_character_,
    process_ = character(),
    classes = character(),
    vintage = character(),
    vintage_ = character(),
    cluster = character(),
    cluster_ = character(),
    arg) {
  if (!is.null(names(arg))) {
    # An unknown field used to be dropped SILENTLY -- e.g. `tech = "X"`
    # (instead of `for.sum = list(tech = "X")`) quietly turned a per-tech
    # cap into a global one. Refuse rather than mislead.
    known <- c("variable", "mult", "for.sum", "defVal", "timeframe",
               "geoframe", "for.each", "desc", .summand_target_slots)
    bad <- setdiff(names(arg), known)
    if (length(bad) > 0) {
      stop("Unknown summand field(s) in constraint '", eqt@name, "': ",
           paste0('"', bad, '"', collapse = ", "),
           ". To restrict the variable to set members use ",
           "`for.sum = list(", bad[1], " = ...)`.", call. = FALSE)
    }
    if (any(names(arg) == "variable")) variable <- arg$variable
    if (any(names(arg) == "mult")) mult <- arg$mult
    if (any(names(arg) == "for.sum")) for.sum <- arg$for.sum
    if (any(names(arg) == "defVal")) defVal <- arg$defVal
    if (any(names(arg) == "timeframe")) timeframe <- arg$timeframe
    if (any(names(arg) == "geoframe")) geoframe <- arg$geoframe
    for (sl in .summand_target_slots) {
      if (any(names(arg) == sl)) assign(sl, arg[[sl]])
    }
    if (any(names(arg) == "for.each")) {
      stop(
        "The 'for.each' parameter is set of the entire constraint and",
        " cannot be changed in linear terms. \n",
        " Constraint: ", eqt@name, "\n Variable: ", variable, "\n for.each: ",
        lapply(arg[["for.each"]], head), "\n Use 'for.sum' parameter instead."
        )
    }
  }
  # eqt, variable, mult, for.sum, arg
  st <- new("summand")
  st@variable <- variable
  if (!is.data.frame(mult) && is.list(mult)) {
    xx <- sapply(mult, length)
    if (any(xx[1] != xx)) {
      stop(paste0("Wrong mult parameters "))
    }
    if (xx[1] >= 1) {
      xx <- data.frame(stringsAsFactors = FALSE)
      xx[seq_len(length(mult[[1]])), ] <- NA
      for (i in names(mult)) xx[[i]] <- mult[[i]]
      mult <- xx
    }
  }
  if (is.data.frame(mult)) {
    st@mult <- mult
  } else {
    if (length(mult) != 1) {
      stop(paste0("Multiplier parameter ('mult') must be a constant or a data.frame with sets."))
    }
    st@defVal <- mult
    # st@mult <- data.frame(value = mult, stringsAsFactors = FALSE)
  }
  st@for.sum <- for.sum
  st@timeframe <- if (is.null(timeframe) || length(timeframe) == 0) {
    NA_character_
  } else {
    as.character(timeframe)[1]
  }
  st@geoframe <- if (is.null(geoframe) || length(geoframe) == 0) {
    NA_character_
  } else {
    as.character(geoframe)[1]
  }
  # Targeting filters keep every value the caller gave: unlike timeframe and
  # geoframe (one level each) a filter may name several processes or vintages.
  for (sl in .summand_target_slots) {
    v <- get(sl)
    v <- if (is.null(v)) character() else as.character(v)
    methods::slot(st, sl) <- v[!is.na(v) & nzchar(v)]
  }
  if (all(names(.variable_set) != variable)) {
    stop(paste0('Unknown variable "', variable, '"in summand "', eqt@name, '"'))
  }
  # Fill in an unrestricted `NA` entry for every dimension the caller did not
  # name. Keyed by DIMENSION name, not set name: for the six variables that
  # index one set twice this is the difference between two usable slots
  # (`src` / `dst`) and one ambiguous one (`region`).
  need.set <- .variable_dim[[variable]]
  if (is.null(need.set) || length(need.set) != length(.variable_set[[variable]])) {
    need.set <- .variable_set[[variable]]
  }
  bad <- setdiff(setdiff(names(st@for.sum), c("lag.year", "lead.year")), need.set)
  if (length(intersect(bad, .variable_set[[variable]]))) {
    stop(paste0('Variable "', variable, '" in summand of constraint "',
                eqt@name, '" indexes "',
                paste(intersect(bad, .variable_set[[variable]]), collapse = '", "'),
                '" more than once, so `for.sum` must name the position rather ',
                'than the set.
  Available: ',
                paste(need.set, collapse = ", ")), call. = FALSE)
  }
  need.set <- need.set[!(need.set %in% c(names(eqt@for.each), names(st@for.sum)))]
  for (i in need.set) {
    st@for.sum[i] <- list(NA)
  }
  if (length(st@for.sum) != 0) {
    st@for.sum[sapply(st@for.sum, is.null)] <- NA
  }
  if (!all(names(st@mult) %in% c(names(eqt@for.each), names(st@for.sum), "value"))) {
    stop(paste0('Unrecognized set(s) in mult parameter: "',
                paste0(names(st@mult)[!(names(st@mult) %in% names(st@for.sum))],
                       collapse = '", "'), '"'))
  }
  names(st@defVal) <- NULL
  names(st@variable) <- NULL
  eqt@lhs[[length(eqt@lhs) + 1]] <- st
  eqt
}

# prec <- add0_message$add0_arg$obj
# stm <- add0_message$add0_arg$app
# approxim <- add0_message$add0_arg$approxim

# Report a constraint that has been dropped because the horizon filter left its
# `for.each` domain empty. Names the constraint, what it asked for, and what the
# model offers, so the user can widen the horizon or delete the constraint.
.drop_unbinding_constraint <- function(stm, for_each, prec) {
  detail <- ""
  if (!is.null(for_each$year)) {
    model_years <- sort(unique(prec@parameters[["mMidMilestone"]]@data$year))
    detail <- paste0(
      " Its year(s) ", .collapse_values(sort(unique(for_each$year))),
      " are outside the model horizon (",
      .collapse_values(model_years), ")."
    )
  }
  warning(
    "Constraint \"", stm@name, "\" cannot bind in this scenario and has been ",
    "dropped.", detail,
    call. = FALSE
  )
  invisible(NULL)
}

# Compact vector for messages: "2020", "2020, 2025", "2020, 2025, ... 2050".
.collapse_values <- function(x) {
  if (length(x) <= 4) {
    return(paste0(x, collapse = ", "))
  }
  paste0(paste0(x[1:3], collapse = ", "), ", ... ", x[length(x)])
}

# Calculate do equation need additional set, and add it
.getSetEquation <- function(prec, stm, approxim) {

  # temporary fix for constraints interpolation: expanding year set
  # works only for year set and if no NA values in the set
  if (!is.null(stm@rhs$year) && !any(is.na(stm@rhs$year))) {
    stm@rhs <- interpolate_slot(stm@rhs, val = "rhs")
  }
  if (!is.null(stm@for.each$year) && !any(is.na(stm@for.each$year))) {
    stm@for.each <- interpolate_slot(stm@for.each, val = NULL)
  }

  stop.constr <- function(x) {
    stop(paste0('Constraint "', stm@name, '" error: ', x))
  }
  # (The legacy `get.all.child` timeslice-ancestry helper was removed: variable
  # domains now carry every timeslice level, so timeslice restrictions -- including
  # those derived from a summand `timeframe` -- are taken literally. See the
  # for.sum timeslice handling below and the `timeframe` resolution loop.)
  # all.set contain all set for for.each & lhs
  # Estimate is need sum for for.each
  # set.map need special mapping or consist all set
  all.set <- data.frame(
    alias = character(), # name in equation
    set = character(), # original set
    dim = character(), # position name; differs from `set` only where a
    # variable indexes one set twice (vTradeIr's src/dst, the
    # *RetiredNewCap vintage/year). See `.variable_dim`.
    for.each = logical(), # for.each, lhs
    lhs.num = numeric(), # number for lhs
    lead.year = logical(), # use only for year & lhs (next year)
    lag.year = logical(), # use only for year & lhs (old year)
    def.lhs = logical(), # not in for.each
    new.map = numeric(), # need new sub set
    stringsAsFactors = FALSE
  )
  set.map <- list()
  set.map.name <- NULL # Temp vector with name for list set.map
  # all.set & set.map
  # for.each

  old_for_each <- lapply(stm@for.each, function(x) {
    if (any(is.na(x))) {
      return(NULL)
    }
    return(unique(x))
  })
  names(old_for_each) <- colnames(stm@for.each)
  nn <- seq_len(length(old_for_each) + sum(sapply(stm@lhs, function(x) length(.variable_set[[x@variable]]))))
  all.set[seq_along(nn), ] <- NA
  for (i in (1:ncol(all.set))[sapply(all.set, class) == "logical"]) {
    all.set[[i]] <- FALSE
  }
  nn <- 0
  if (length(old_for_each) > 0) {
    nn <- seq_along(old_for_each)
    all.set[nn, "set"] <- names(old_for_each)
    all.set[nn, "dim"] <- names(old_for_each)
    all.set[nn, "alias"] <- names(old_for_each)
    all.set[nn, "for.each"] <- TRUE
    for.each.set <- names(old_for_each)
    # Fill add.map for for.each
    for (j in for.each.set) {
      if (!is.null(old_for_each[[j]]) && !all(prec@sets[[j]] %in% old_for_each[[j]])) {
        if (any(old_for_each[[j]] %in% prec@sets[[j]])) {
          # warning(paste0('Set "'))
          old_for_each[[j]] <-
            old_for_each[[j]][old_for_each[[j]] %in% prec@sets[[j]]]
        }
        set.map.name <- c(set.map.name, j)
        set.map[[length(set.map.name)]] <- old_for_each[[j]]
        all.set[nn[names(old_for_each) == j], "new.map"] <- length(set.map.name)
      }
    }
  } else {
    for.each.set <- NULL
  }

  # Resolve per-summand `timeframe` to an explicit `timeslice` restriction. The
  # variable is taken at exactly the timeslices of that timeframe level
  # (calendar@timeframes), so summing them yields the level aggregate without
  # double-counting across levels -- replacing the *RY year-resolution
  # variables. Applied here, before the timeslice handling below picks it up as a
  # for.sum restriction.
  for (i in seq_along(stm@lhs)) {
    # Defensive: summand objects serialized before the `timeframe` slot existed
    # lack it; treat those as no timeframe (NA).
    tf <- tryCatch(stm@lhs[[i]]@timeframe, error = function(e) NA_character_)
    if (length(tf) != 1 || is.na(tf)) next
    if (!("timeslice" %in% .variable_set[[stm@lhs[[i]]@variable]])) {
      stop.constr(paste0(
        'timeframe = "', tf, '" set on variable "', stm@lhs[[i]]@variable,
        '" which has no timeslice dimension.'
      ))
    }
    tfs <- approxim$calendar@timeframes
    lev <- tfs[[tf]]
    if (is.null(lev) || length(lev) == 0) {
      stop.constr(paste0(
        'unknown timeframe "', tf, '". Available: ',
        paste(names(tfs), collapse = ", ")
      ))
    }
    # Timeslices AT that level plus everything under them, so a level the variable
    # is not indexed by aggregates rather than resolving to nothing.
    anc <- approxim$calendar@timeslice_ancestry
    desc <- unique(c(as.character(lev),
                     if (!is.null(anc) && nrow(anc) > 0) {
                       as.character(anc$child[as.character(anc$parent) %in% lev])
                     }))
    v <- stm@lhs[[i]]@variable
    dom <- .variable_domain_values(prec, v, "timeslice")
    res <- .resolve_level_indices(dom, tf, tfs, desc,
                                  rows = .variable_domain_rows(prec, v),
                                  dim = "timeslice")
    .stop_level_mixed(res, "timeframe", tf, v, stop.constr)
    .stop_level_state(res, "timeframe", tf, v, "timeslice", stop.constr)
    got <- res$values
    if (length(got) == 0) {
      stop.constr(paste0(
        'timeframe = "', tf, '" does not resolve to any timeslice of variable "',
        v, '". It exists at: ',
        paste(utils::head(sort(dom), 6), collapse = ", "),
        '.\n  Write the sum explicitly in `for.sum` instead.'
      ))
    }
    stm@lhs[[i]]@for.sum$timeslice <- as.character(got)
  }

  # Spatial twin of the loop above: resolve per-summand `geoframe` to an
  # explicit `region` restriction. The variable is taken at exactly the regions
  # of that geoscale level, so summing them yields the level aggregate without
  # double-counting across levels.
  for (i in seq_along(stm@lhs)) {
    gl <- tryCatch(stm@lhs[[i]]@geoframe, error = function(e) NA_character_)
    if (length(gl) != 1 || is.na(gl)) next
    if (!("region" %in% .variable_set[[stm@lhs[[i]]@variable]])) {
      stop.constr(paste0(
        'geoframe = "', gl, '" set on variable "', stm@lhs[[i]]@variable,
        '" which has no region dimension.'
      ))
    }
    hier <- approxim$geo_hierarchy
    if (is.null(hier)) {
      stop.constr(paste0(
        'geoframe = "', gl, '" requires a geoscale on the model config; ',
        'attach one with `setGeoscale()`.'
      ))
    }
    if (!gl %in% hier$levels) {
      stop.constr(paste0(
        'unknown geoframe "', gl, '". Available: ',
        paste(hier$levels, collapse = ", ")
      ))
    }
    # Regions AT that level plus every region beneath them, so constraining
    # "province P" works without knowing which level the commodity balances at.
    desc <- .geo_descendants(hier, gl)
    v <- stm@lhs[[i]]@variable
    dom <- .variable_domain_values(prec, v, "region")
    res <- .resolve_level_indices(dom, gl, hier$members, desc,
                                  rows = .variable_domain_rows(prec, v),
                                  dim = "region")
    .stop_level_mixed(res, "geoframe", gl, v, stop.constr)
    .stop_level_state(res, "geoframe", gl, v, "region", stop.constr)
    got <- res$values
    if (length(got) == 0) {
      stop.constr(paste0(
        'geoframe = "', gl, '" does not resolve to any region of variable "',
        v, '". It exists at: ',
        paste(utils::head(sort(dom), 6), collapse = ", "),
        '.\n  Write the sum explicitly in `for.sum` instead.'
      ))
    }
    stm@lhs[[i]]@for.sum$region <- as.character(got)
  }

  # Coarse indices named directly in `for.sum` (not via a level) expand to the
  # variable's own indices beneath them -- "emissions in province P" sums P's
  # counties. Runs after the level loops so both routes end up the same shape.
  for (i in seq_along(stm@lhs)) {
    v <- stm@lhs[[i]]@variable
    hier <- approxim$geo_hierarchy
    if (!is.null(hier) && !is.null(stm@lhs[[i]]@for.sum$region)) {
      dom <- .variable_domain_values(prec, v, "region")
      stm@lhs[[i]]@for.sum$region <- .expand_for_sum(
        stm@lhs[[i]]@for.sum$region, dom,
        function(r) .geo_descendants_of(hier, r), "region", v, stop.constr)
    }
    anc <- approxim$calendar@timeslice_ancestry
    if (!is.null(stm@lhs[[i]]@for.sum$timeslice) && !is.null(anc) && nrow(anc) > 0) {
      dom <- .variable_domain_values(prec, v, "timeslice")
      stm@lhs[[i]]@for.sum$timeslice <- .expand_for_sum(
        stm@lhs[[i]]@for.sum$timeslice, dom,
        function(s) as.character(anc$child[as.character(anc$parent) == s]),
        "timeslice", v, stop.constr)
    }
  }

  # `<dim>_` keys: a regular expression over that dimension, resolved here to
  # explicit members so everything downstream sees an ordinary `for.sum`. Must
  # run before the key filter further down, which drops any name that is not a
  # dimension of the variable -- silently, which is how a `tech_` key written
  # before this existed was ignored rather than refused.
  for (i in seq_along(stm@lhs)) {
    v <- stm@lhs[[i]]@variable
    dims <- .summand_dims(v)
    fs <- stm@lhs[[i]]@for.sum
    rx_keys <- grep("_$", names(fs), value = TRUE)
    rx_keys <- setdiff(rx_keys, c("lag.year", "lead.year"))
    for (k in rx_keys) {
      d <- sub("_$", "", k)
      if (!(d %in% dims)) {
        stop.constr(paste0(
          'the `for.sum` key "', k, '" is a regular expression over "', d,
          '", which variable "', v, '" is not indexed by.',
          "
  Available: ", paste(dims, collapse = ", ")))
      }
      pats <- as.character(fs[[k]])
      pats <- pats[!is.na(pats) & nzchar(pats)]
      fs[[k]] <- NULL
      if (length(pats) == 0) next
      dom <- .variable_domain_values(prec, v, d)
      hit <- unique(dom[Reduce(`|`, lapply(pats, function(pt) grepl(pt, dom)))])
      if (length(hit) == 0) {
        .stop_target_filter(paste0('constraint "', stm@name, '", variable "', v, '"'),
                            k, pats, dom)
      }
      fs[[d]] <- if (.for_sum_unrestricted(fs[[d]])) hit else {
        intersect(as.character(fs[[d]]), hit)
      }
      if (length(fs[[d]]) == 0) {
        stop.constr(paste0(
          'the `for.sum` key "', k, '" and "', d,
          '" together select nothing for variable "', v, '".'))
      }
    }
    stm@lhs[[i]]@for.sum <- fs
  }

  # Summand targeting filters (`process_`, `classes`, `vintage`, `cluster`, ...)
  # resolved by the SAME resolver the levy classes use (`.target_filter()` in
  # R/levy.R), against the variable's own process dimension. Exact names stay in
  # `for.sum`; these say what `for.sum` cannot.
  for (i in seq_along(stm@lhs)) {
    sm <- stm@lhs[[i]]
    f <- stats::setNames(
      lapply(.summand_target_slots, function(sl) .summand_slot(sm, sl)),
      .summand_target_slots)
    if (!any(vapply(f, function(x) length(x) > 0, logical(1)))) next
    v <- sm@variable
    pdim <- .summand_process_dim(v)
    if (is.null(pdim)) {
      stop.constr(paste0(
        'process targeting (', paste0("`", names(f)[vapply(f, length, integer(1)) > 0],
                                      "`", collapse = ", "),
        ') is set on variable "', v, '", which has no process dimension.'))
    }
    label <- paste0('constraint "', stm@name, '", variable "', v, '"')
    pool <- .variable_domain_values(prec, v, pdim)
    cls <- f$classes
    if (length(cls) > 0) {
      keep <- .class_members(approxim, cls, label = label)
      pool2 <- intersect(pool, keep)
      if (length(pool2) == 0) .stop_target_filter(label, "classes", cls, pool)
      pool <- pool2
    }
    f$classes <- NULL
    got <- .target_filter(pool, approxim$variant, f, label)
    sm@for.sum[[pdim]] <- if (.for_sum_unrestricted(sm@for.sum[[pdim]])) got else {
      both <- intersect(as.character(sm@for.sum[[pdim]]), got)
      if (length(both) == 0) {
        stop.constr(paste0(
          'the targeting filters and `for.sum = list(', pdim,
          ' = ...)` select nothing in common for variable "', v, '".'))
      }
      both
    }
    stm@lhs[[i]] <- sm
  }

  # Every remaining `for.sum` value must be a MEMBER OF ITS SET. Only region and
  # timeslice were checked before, so a name in any other dimension contributed
  # ZERO in silence -- `for.sum = list(tech = "ECOA")` against a vintaged
  # ECOA_VIN2030 dropped the term and still solved. A declared (base) name
  # expands to its variants, the same way a coarse region expands to its
  # children.
  #
  # The SET, not the variable's domain, is the reference: a declared process
  # that happens to have no rows in THIS variable is a legitimate thing to name.
  # TOPIA's RES_SHARE sums `c("ESOL", "EWIN", "EHYD")` where EHYD is a real
  # technology with no hydro resource in that layout -- it contributes nothing
  # and should. What is refused is a name the model does not have at all.
  for (i in seq_along(stm@lhs)) {
    v <- stm@lhs[[i]]@variable
    prov <- approxim$variant
    dims <- .summand_dims(v)
    sets <- .variable_set[[v]]
    for (d in intersect(names(stm@lhs[[i]]@for.sum), dims)) {
      if (d %in% c("region", "timeslice")) next   # handled above, with hierarchy
      vals <- stm@lhs[[i]]@for.sum[[d]]
      if (.for_sum_unrestricted(vals)) next
      sn <- sets[match(d, dims)]
      members <- as.character(prec@sets[[sn]])
      if (length(members) == 0) next
      vals <- .expand_for_sum(
        vals, members,
        function(x) {
          if (is.null(prov) || nrow(prov) == 0) return(character())
          as.character(prov$name[as.character(prov$base) == x])
        },
        d, v, stop.constr)
      # ... but a term in which NONE of the named members reaches the variable
      # can only ever be zero, which is the failure this check exists to stop.
      dom <- .variable_domain_values(prec, v, d)
      if (!is.null(dom) && length(dom) > 0 &&
          length(intersect(vals, dom)) == 0) {
        stop.constr(paste0(
          d, ' = "', paste(vals, collapse = '", "'),
          '" names no member that variable "', v, '" is defined for, so the ',
          'term would be zero for every row. It exists at: ',
          paste(utils::head(sort(dom), 6), collapse = ", "), "."))
      }
      stm@lhs[[i]]@for.sum[[d]] <- vals
    }
  }

  # lhs
  for (i in seq_along(stm@lhs)) {

    need.set <- .variable_set[[stm@lhs[[i]]@variable]]
    # Position NAMES. Identical to `need.set` except for the six variables that
    # index one set twice, where two dimensions cannot share an index name --
    # see `.variable_dim` in data-raw/DATASET.R. `for.sum` is keyed by these, so
    # `list(src = "R1")` addresses one endpoint of a route and `list(dst = ...)`
    # the other; the SET is still what domain lookups use.
    need.dim <- .variable_dim[[stm@lhs[[i]]@variable]]
    if (is.null(need.dim) || length(need.dim) != length(need.set)) {
      need.dim <- need.set
    }
    bad.fs <- setdiff(
      setdiff(names(stm@lhs[[i]]@for.sum), c("lag.year", "lead.year")),
      need.dim)
    # A name that is a SET of this variable but not one of its dimension names
    # is the interesting mistake: `for.sum = list(region = ...)` on vTradeIr,
    # which cannot mean anything because the variable has two of them.
    if (length(intersect(bad.fs, need.set))) {
      stop.constr(paste0(
        'variable "', stm@lhs[[i]]@variable, '" indexes "',
        paste(intersect(bad.fs, need.set), collapse = '", "'),
        '" more than once, so `for.sum` must name the position, not the set.',
        "
  Available: ", paste(need.dim, collapse = ", ")))
    }
    nn <- (nn[length(nn)] + seq_along(need.set))
    all.set[nn, "set"] <- need.set
    all.set[nn, "dim"] <- need.dim
    all.set[nn, "alias"] <- need.dim
    all.set[nn, "lhs.num"] <- i
    if (any(names(stm@lhs[[i]]@for.sum) == "lag.year")) {
      if (all(need.dim != "year")) {
        stop.constr("For lag.year have to define use variable with year")
      }
      all.set[nn[need.dim == "year"], c("lag.year", "def.lhs")] <- TRUE
    }
    if (any(names(stm@lhs[[i]]@for.sum) == "lead.year")) {
      if (all(need.dim != "year")) {
        stop.constr("For lead.year have to define use variable with year")
      }
      all.set[nn[need.dim == "year"], c("lead.year", "def.lhs")] <- TRUE
    }
    all.set[nn[need.dim %in% names(stm@lhs[[i]]@for.sum)], "def.lhs"] <- TRUE
    all.set[nn[!(need.dim %in% for.each.set)], "def.lhs"] <- TRUE
    # Add to set map
    st <- names(stm@lhs[[i]]@for.sum)[names(stm@lhs[[i]]@for.sum) %in% need.dim &
      !sapply(is.na(stm@lhs[[i]]@for.sum), all)]
    # Fill add.map for for.lhs
    for (j in st) {
      # Restrict to the EXACT for.sum values (incl. timeslice). The legacy
      # timeslice-ancestry expansion (get.all.child) is retired: variable domains
      # now carry every timeslice level, so a timeslice restriction -- e.g. one derived
      # from a summand `timeframe` -- must be taken literally to avoid
      # double-counting across levels.
      # `j` names a POSITION; its members come from the underlying set.
      j.set <- if (j %in% names(.dim_set)) unname(.dim_set[[j]]) else j
      if (!is.null(stm@lhs[[i]]@for.sum[[j]]) &&
        !all(prec@sets[[j.set]] %in% stm@lhs[[i]]@for.sum[[j]])) {
        # check if the same set in lhs exist
        fl <- FALSE
        if (all(!c(all.set[nn[need.dim == j], c("lead.year", "lag.year")],
          recursive = TRUE
        ))) {
          fl <- nn[(!all.set$for.each[nn] & all.set$dim[nn] == j &
            !is.na(all.set$new.map[nn]))]
        }
        add.new <- TRUE
        if (any(fl)) {
          for (k in all.set[fl, "new.map"]) {
            if (length(stm@lhs[[i]]@for.sum[[j]]) == length(set.map[[k]]) &&
              all(stm@lhs[[i]]@for.sum[[j]] %in% set.map[[k]]) && all(set.map[[k]] %in% stm@lhs[[i]]@for.sum[[j]])) {
              all.set[nn[need.dim == j], "new.map"] <- k
              add.new <- FALSE
            }
          }
        }
        if (add.new) {
          # The map is declared over the SET, but keyed by this position.
          set.map.name <- c(set.map.name, j.set)
          set.map[[length(set.map.name)]] <- stm@lhs[[i]]@for.sum[[j]]
          all.set[nn[need.dim == j], "new.map"] <- length(set.map.name)
        }
      }
    }
  }
  # Add alias
  # A lhs index collides with a for.each index only if it carries the same
  # NAME; `src` and `dst` never collide with a for.each `region`.
  fl <- (!all.set$for.each & all.set$def.lhs & all.set$dim %in% for.each.set)
  if (any(fl)) {
    all.set[fl, "alias"] <- paste0(all.set[fl, "set"], "p")
  }
  # Need add code to reduce additional mapping
  # Maaping
  if (length(set.map) > 0) {
    mpp <- all.set[!is.na(all.set$new.map), c("new.map", "alias")]
    mpp <- mpp[!duplicated(mpp$new.map), ]
    mpp <- mpp[sort(mpp$new.map, index.return = TRUE)$ix, , drop = FALSE]
    new.map.name <- paste0("mCns", stm@name, "_", mpp$new.map)
    new.map.name.full <- paste0(new.map.name, "(", mpp$alias, ")")
    for (i in seq_along(set.map)) {
      prec@parameters[[new.map.name[i]]] <-
        addMultipleSet(
          newParameter(new.map.name[i], set.map.name[i], "map"),
          c(set.map[[i]])
        )
    }

    # copy new.map for lhs set that define in for each
    fl <- seq_len(nrow(all.set))[all.set$for.each & !is.na(all.set$new.map)]
    for (i in fl) {
      all.set[!all.set$for.each & !all.set$def.lhs & all.set$set == all.set$set[i], "new.map"] <- all.set$new.map[i]
    }
  }
  if (nrow(all.set) > 0) {
    st <- unique(all.set$set)
    st <- st[!(st %in% names(approxim))]
    for (ss in st) approxim[[ss]] <- prec@sets[[ss]]
  }

  # Generate GAMS code with mult & rhs parameters
  res <- list()
  # Declaration equation in model
  res$equationDeclaration2Model <- paste0("eqCns", stm@name)
  # Declaration equation
  if (length(old_for_each) == 0) {
    res$equationDeclaration <- res$equationDeclaration2Model
  } else {
    res$equationDeclaration <- paste0(res$equationDeclaration2Model, "(", paste0(names(old_for_each), collapse = ", "), ")")
  }
  # Equation before ..
  res$equation <- res$equationDeclaration
  if (any(is.na(stm@for.each))) {
    tmp_fe <- stm@for.each
    fl_na <- colnames(stm@for.each)[apply(is.na(stm@for.each), 2, any)]
    for (i in fl_na) {
      tmg <- prec@parameters[[i]]@data
      if (i == "year") {
        tmg <- prec@parameters[["mMidMilestone"]]@data
        if (any(all.set$lag.year)) {
          tmg <- tmg[
            !(tmg$year %in% prec@parameters[["mMilestoneFirst"]]@data$year), ,
            drop = FALSE
          ]
        }
        if (any(all.set$lead.year)) {
          tmg <- tmg[
            tmg$year %in% prec@parameters[["mMilestoneHasNext"]]@data$year, ,
            drop = FALSE
          ]
        }
      }
      tmp_fe <- rbind(
        merge(
          select(tmp_fe, -any_of(i)),
          # tmp_fe[, colnames(tmp_fe) != i, drop = FALSE],
          tmg
        ),
        tmp_fe[!is.na(tmp_fe[[i]]), , drop = FALSE]
      )
      tmp_fe <- tmp_fe[!duplicated(tmp_fe), , drop = FALSE]
    }
    stm@for.each <- tmp_fe
  }
  # kept for the diagnostic below: the requested domain, before horizon filtering
  fe_before_horizon <- stm@for.each
  if (!is.null(stm@for.each$year)) {
    stm@for.each <- stm@for.each[stm@for.each$year %in% prec@parameters[["mMidMilestone"]]@data$year, , drop = FALSE]
    if (any(all.set$lag.year)) {
      stm@for.each <- stm@for.each[!(stm@for.each$year %in% prec@parameters[["mMilestoneFirst"]]@data$year), , drop = FALSE]
    }
    if (any(all.set$lead.year)) {
      stm@for.each <- stm@for.each[stm@for.each$year %in% prec@parameters[["mMilestoneHasNext"]]@data$year, , drop = FALSE]
    }
  }
  # An indexed equation whose `for.each` domain was emptied by the horizon
  # filter above cannot bind on anything. The `mCnsForEach` guard below is
  # written only for a non-empty domain, so keeping the constraint would leave
  # `eqCns<name>(...)` declared over a domain that is never defined, and the
  # backends emit that as a reference to a non-existent set. Drop it instead.
  if (nrow(stm@for.each) == 0 && length(old_for_each) > 0) {
    .drop_unbinding_constraint(stm, fe_before_horizon, prec)
    if (length(set.map) > 0) prec@parameters[new.map.name] <- NULL
    return(prec)
  }
  if (nrow(stm@for.each) > 0) {
    nmn <- paste0("mCnsForEach", stm@name)
    prec@parameters[[nmn]] <- .dat2par(
      newParameter(nmn, colnames(stm@for.each), "map",
        interpolation = stm@interpolation
      ),
      stm@for.each
    )
    res$equation <- paste0(res$equation, "$", nmn, "(", paste0(colnames(stm@for.each), collapse = ", "), ")")
  }
  res$equation <- paste0(res$equation, ".. ")

  # Add eq
  res$equation <- paste0(
    res$equation,
    " ### ",
    c("==" = "=e=", ">=" = "=g=", "<=" = "=l=")[as.character(stm@eq)],
    " "
  )
  # Add rhs
  # browser() # rhs value or data.frame
  if (nrow(stm@rhs) != 0 && ncol(stm@rhs) != 1) {
      # (any(stm@rhs$rhs != 0) ||
      #  (stm@defVal != 0 && nrow(stm@for.each) > nrow(stm@rhs))
      #  )
      # ) {
    # RHS is a data.frame
    approxim2 <-
      approxim[unique(
        c(
          colnames(stm@rhs)[colnames(stm@rhs) %in% names(approxim)],
          "solver", "year", "calendar"
        )
      )]
    if (any(names(approxim2) == "timeslice")) {
      approxim2$timeslice <- approxim2$calendar@timeslice_share$timeslice
    }
    fl <- (all.set$for.each & !is.na(all.set$new.map) &
      all.set$set %in% colnames(stm@rhs))
    need.set <- all.set[fl, , drop = FALSE]
    for (j in seq_len(nrow(need.set))) {
      approxim2[[need.set[j, "set"]]] <- set.map[[need.set[j, "new.map"]]]
    }
    approxim2$fullsets <- approxim$fullsets
    need.set0 <- for.each.set[for.each.set %in% colnames(stm@rhs)]
    xx <- newParameter(paste0("pCnsRhs", stm@name), need.set0, "numpar",
      defVal = stm@defVal,
      interpolation = stm@interpolation,
      colName = "rhs"
    )
    yy <- .interp_numpar(stm@rhs, "rhs", xx, approxim2)
    n1 <- colnames(yy)[colnames(yy) != "value"]
    # yy <- yy[
    #   (apply(yy[, n1, drop = FALSE], 1,
    #          paste0, collapse = "##") %in%
    #      apply(stm@for.each[, n1, drop = FALSE], 1,
    #            paste0, collapse = "##")
    #    ), , drop = FALSE]
    # check:
    # yy[apply(select(yy, all_of(n1)), 1, paste0, collapse = "##") %in%
    #     apply(select(stm@for.each, all_of(n1)), 1, paste0, collapse = "##"),]
    # same using dplyr
    if (ncol(yy) == 1) {
      stopifnot(names(yy) == "value")
      stopifnot(nrow(yy) == 1)
    } else {
    # suppressMessages({
      col_nms <- intersect(colnames(yy), n1)
      yy <- yy |> right_join(select(stm@for.each, all_of(n1)), by = col_nms)
    # })
    }
    prec@parameters[[xx@name]] <- .dat2par(xx, yy)
    # Add mult
    res$equation <- paste0(
      res$equation, xx@name, "(",
      paste0(need.set0, collapse = ", "), ")"
    )
  } else if (is.data.frame(stm@rhs) && nrow(stm@rhs) == 1 && ncol(stm@rhs) == 1) {
    # rhs is a 1x1 data.frame -> scalar literal
    res$equation <- paste0(res$equation, stm@rhs$rhs)
  } else {
    # No rhs data.frame (or a scalar): use the constraint's `defVal` as a
    # constant RHS, applied to every for.each tuple. (Empty `rhs` is the common
    # "sum(...) <op> defVal" form, e.g. newConstraint(..., defVal = 100).)
    if (length(stm@defVal) != 1 || is.na(stm@defVal)) {
      stop.constr(paste0(
        "RHS is empty and 'defVal' is not a single non-NA value; ",
        "provide `rhs` or a scalar `defVal`."
      ))
    }
    res$equation <- paste0(res$equation, stm@defVal)
  }

  # Add lhs
  lhs_equation <- ""

  # Add lhs to equation
  lhs.set <- all.set[!all.set$for.each, , drop = FALSE]
  for (i in seq_along(stm@lhs)) {
    vrb <- stm@lhs[[i]]@variable
    lhs.set2 <- lhs.set[lhs.set$lhs.num == i, ]
    vrb.lhs <- .variable_mapping[[vrb]]
    # Coefficient pieces, assembled AFTER the index substitution below.
    coef.val <- NULL        # literal factor, or the mult parameter defVal
    coef.par <- ""          # mult parameter name, "" when the factor is literal
    coef.set <- character() # the parameter OWN index list
    # Add multiple to vrb
    # Add to year multiplier if lag.year | lead.year
    if ((any(lhs.set2$lead.year) ||
      any(lhs.set2$lag.year)) &&
      (nrow(stm@lhs[[i]]@mult) == 0 ||
        all(colnames(stm@lhs[[i]]@mult) != "year"))) {
      if (nrow(stm@lhs[[i]]@mult) == 0) {
        stm@lhs[[i]]@mult <- data.frame(
          year = NA, value = stm@lhs[[i]]@defVal,
          stringsAsFactors = FALSE
        )
      } else {
        stm@lhs[[i]]@mult$year <- NA
      }
    }
    # Add multiplier
    if (nrow(stm@lhs[[i]]@mult) != 0) {
      # Complicated parameter
      # Generate approxim
      approxim2 <- approxim[unique(c(colnames(stm@lhs[[i]]@mult)[
        colnames(stm@lhs[[i]]@mult) %in% names(approxim)
      ], "solver", "year"))]
      if (any(names(approxim2) == "timeslice")) {
        approxim2$timeslice <- approxim2$calendar@timeslice_share$timeslice
      }
      need.set <- lhs.set2[lhs.set2$set %in% colnames(stm@lhs[[i]]@mult), "set"]
      need.set2 <- lhs.set2[!is.na(lhs.set2$new.map) &
        lhs.set2$set %in% colnames(stm@lhs[[i]]@mult), ]

      for (j in seq_len(nrow(need.set2))) {
        approxim2[[need.set2[j, "set"]]] <- set.map[[need.set2[j, "new.map"]]]

        if (any(colnames(stm@lhs[[i]]@mult) %in% c(need.set, "value"))) {
          if (!all(colnames(stm@lhs[[i]]@mult) %in%
            c(for.each.set, need.set, "value"))) {
            stop(paste0(
              "Unrecognized set in constraint ",
              stm@name, ", mult ", i, ': "',
              paste0(
                colnames(stm@lhs[[i]]@mult)[
                  !(colnames(stm@lhs[[i]]@mult) %in% c(for.each.set, "value"))
                ],
                collapse = '", "'
              ), '"'
            ))
          }
          # Add set that from  for.each
          nslc <- colnames(stm@lhs[[i]]@mult)[
            !(colnames(stm@lhs[[i]]@mult) %in% c(need.set, "value"))
          ]
          need.set <- c(need.set, nslc)
          if (nrow(stm@for.each) > 0) {
            for (j in nslc) {
              approxim2[[j]] <- unique(stm@for.each[[j]])
            }
          } else {
            for (j in nslc) {
              approxim2[[j]] <- approxim[[j]]
            }
            if (any(nslc == "timeslice")) {
              approxim2$timeslice <- approxim$calendar@timeslice_share$timeslice
            }
          }
        }
      }
      approxim2$fullsets <- approxim$fullsets
      xx <- newParameter(paste0("pCnsMult", stm@name, "_", i),
        need.set,
        "numpar",
        defVal = stm@lhs[[i]]@defVal, # !!! Check
        interpolation = stm@interpolation
      )
      prec@parameters[[xx@name]] <-
        .dat2par(xx, .interp_numpar(stm@lhs[[i]]@mult, "value", xx, approxim2))
      if (any(lhs.set2$lead.year) || any(lhs.set2$lag.year)) {
        yy <- .add_dropped_zeros(prec, xx@name)
        nn <- approxim$mileStoneForGrowth[as.character(yy$year)]
        if (any(lhs.set2$lag.year)) nn <- (-nn)
        yy$value <- (sign(yy$value) * abs(yy$value)^nn)
        prec@parameters[[xx@name]] <- .dat2par(xx, yy)
      }
      # Add mult. NOT prepended here: the positional substitution below rewrites
      # every parenthesised group it finds, so a `pCnsMult...(year)` sitting in
      # front of the variable would have its OWN index list rewritten to the
      # variable's full alias list -- a one-dimensional parameter referenced
      # with six indices. Held aside and prepended after the substitution.
      coef.val <- stm@lhs[[i]]@defVal
      coef.par <- xx@name
      coef.set <- need.set
    } else if (length(stm@lhs[[i]]@defVal) == 1 &&
               !is.na(stm@lhs[[i]]@defVal) &&
               stm@lhs[[i]]@defVal != 1) {
      # A SCALAR `mult` is stored in `defVal` (see addSummand), and this branch
      # used to be commented out -- so the coefficient was silently DISCARDED.
      # `mult = -1` then generated a sum where the user wrote a difference, and
      # `mult = 3` a term with no coefficient at all. A constant coefficient is
      # emitted as a literal; there is no parameter to build for it.
      coef.val <- stm@lhs[[i]]@defVal
      coef.par <- ""
      coef.set <- character()
    }
    # Replace setsname
    if (any(duplicated(lhs.set2$set))) {
      # POSITIONAL substitution, for the six variables that index one set twice.
      # A gsub on the set name rewrites BOTH occurrences to the same alias and
      # emits `sum((trade, comm, region, region, timeslice), ...)`, which GLPK
      # refuses -- "duplicate dummy index r not allowed" -- so none of them
      # could appear in a custom constraint. Every parenthesised group in the
      # template is the variable's dimensions in order (the variable itself,
      # then its gating map), so each is replaced wholesale.
      vrb.lhs <- gsub("[(][^()]*[)]",
                      paste0("( ", paste(lhs.set2$alias, collapse = " , "), " )"),
                      vrb.lhs)
    } else {
      for (j in seq_len(nrow(lhs.set2))[lhs.set2$alias != lhs.set2$set]) {
        vrb.lhs <- gsub(paste0(" ", lhs.set2$set[j], " "), lhs.set2$alias[j], vrb.lhs)
      }
    }
    if (!is.null(coef.val)) {
      # The coefficient parameter carries its OWN, much shorter index list, and
      # it is assembled HERE, after the substitution above -- which rewrites
      # every parenthesised group it finds and would otherwise hand this
      # parameter the variable's full alias list.
      #
      # The indices are deliberately NOT aliased. They never were: the old
      # `gsub(" set ", alias, ...)` could not match inside `(year)`, which has
      # no surrounding spaces. That is not an accident of the old code but the
      # required behaviour -- for a lag/lead constraint the coefficient is
      # indexed by the OUTER year while the variable takes the shifted one,
      # e.g. `pCnsMultGROW_2[y] * vOutTot[c, r, yp, s]`. Aliasing them together
      # would silently change every growth constraint in the package.
      for (sn in coef.set) {
        hit <- which(lhs.set2$set == sn)
        if (length(hit) > 1L) {
          stop.constr(paste0(
            'the `mult` of summand ', i, ' is indexed by "', sn,
            '", which variable "', vrb, '" carries more than once. Name the ',
            'position instead: ', paste(lhs.set2$dim[hit], collapse = ", "), "."))
        }
      }
      vrb.lhs <- paste0(
        coef.val, " * ",
        if (nzchar(coef.par)) {
          paste0(coef.par,
                 if (length(coef.set))
                   paste0("(", paste(coef.set, collapse = ", "), ")") else "",
                 " * ")
        } else "",
        vrb.lhs)
    }
    vrb.lhs <- gsub(
      "[ ]*[$][ ]*", "$",
      gsub(
        "[ ]*[)]", ")",
        gsub(
          "[ ]*[(][ ]*", "(",
          gsub(
            "[ ]*[,][ ]*", ", ",
            vrb.lhs
          )
        )
      )
    )
    # Generate data to equation
    if (i != 1) lhs_equation <- paste0(lhs_equation, "+")
    if (all(!lhs.set2$def.lhs)) {
      lhs_equation <- paste0(lhs_equation, vrb.lhs)
    } else {
      lhs.set3 <- lhs.set2[lhs.set2$def.lhs, , drop = FALSE]
      cnd <- NULL
      if (any(!is.na(lhs.set3$new.map))) {
        cnd <- c(cnd, new.map.name.full[lhs.set3$new.map[!is.na(lhs.set3$new.map)]])
      }
      if (any(lhs.set3$lag.year == "year")) {
        cnd <- c(cnd, "mMilestoneNext(yearp, year)")
      } else if (any(lhs.set3$lead.year)) {
        cnd <- c(cnd, "mMilestoneNext(year, yearp)")
      } else if (any(lhs.set3$set == "year")) {
        cnd <- c(cnd, "mMidMilestone(year)")
      }
      if (any(grep("[$]", vrb.lhs))) {
        tmp <- gsub(".*[$]", "", vrb.lhs)
        if (substr(tmp, 1, 1) == "(") {
          tmp <- substr(tmp, 2, nchar(tmp) - 1)
        }
        cnd <- c(cnd, tmp)
        vrb.lhs <- gsub("[$].*", "", vrb.lhs)
      }
      # Finish for sum
      if (sum(lhs.set2$def.lhs) == 1) {
        lhs_equation <- paste0(lhs_equation, " sum(", lhs.set3$alias)
      } else {
        lhs_equation <- paste0(
          lhs_equation, " sum((",
          paste0(lhs.set3$alias, collapse = ", "), ")"
        )
      }
      if (length(cnd) > 1 || any(grep("[ )]and[ (]", cnd))) {
        lhs_equation <- paste0(
          lhs_equation, "$(",
          paste0(cnd, collapse = " and "), "), ", vrb.lhs, ")"
        )
      } else if (length(cnd) == 1) {
        lhs_equation <- paste0(lhs_equation, "$", cnd, ", ", vrb.lhs, ")")
      } else {
        stop("error!")
      }
    }
  }

  res$equation <- gsub("###", lhs_equation, res$equation)

  res$equation <- gsub("[+][[:blank:]]*[-]", "-", res$equation)
  res$equation <- paste0(res$equation, ";")

  prec@user_constraints[[stm@name]] <- res
  prec
}

#  .getSetEquation(prec, stm, approxim)@user_constraints

