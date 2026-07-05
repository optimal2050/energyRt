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
#' @slot mult `r get_slot_doc("summand", "mult")`
#' @slot defVal `r get_slot_doc("summand", "defVal")`
#' @slot misc `r get_slot_doc("summand", "misc")`
#'
#' @family class constraint
#' @order 2
# Bring a `summand` created before the `timeframe` slot existed up to the
# current class definition. Old serialized objects lack the slot, so accessing
# `@timeframe` errors ("no slot of name ..."); add it (S4 slots are attributes)
# with the prototype default. Called on every summand at constraint compile time.
.upgrade_summand <- function(s) {
  if (methods::is(s, "summand") &&
      !("timeframe" %in% names(attributes(s)))) {
    attr(s, "timeframe") <- NA_character_
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
    mult = "data.frame",
    defVal = "numeric",
    misc = "list"
    # parameter= list() # !!! consider adding custom parameters
  ),
  prototype(
    desc = NULL,
    variable = NULL,
    for.sum = list(),
    # `timeframe` pins the slice level the variable is taken at (e.g. "ANNUAL",
    # "SEASON"): the variable's `slice` dimension is restricted to that level's
    # slices, so summing them yields the level aggregate without double-counting
    # across levels. NA = no restriction (sum the variable's native slices).
    # Replaces the need for the *RY (year-resolution) aggregate variables.
    timeframe = NA_character_,
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
#' Note: zero values will be replaced with `replace_zerros` to avoid dropping them by the interpolation algorithms.
#' @param defVal `r get_slot_doc("constraint", "defVal")`
#' @param interpolation `r get_slot_doc("constraint", "interpolation")`
#' @param replace_zerros numeric value to replace zero values in `rhs` and `defVal`. Default is `1e-20`.
#'
#' @return Object of class `constraint`.
#' @family class constraint policy
#' @rdname newConstraint
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
  # browser()
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

  if (is.numeric(rhs)) {
    if (length(rhs) != 1) {
      stop("rhs must be a single numeric value or a data.frame with sets and numeric values for each constraint.")
    }
    if (is.na(defVal)) defVal <- rhs
    rhs <- data.frame()
    # rhs <- data.frame(rhs = rhs, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(rhs) && is.list(rhs)) {
    tmp <- sapply(rhs, length)
    if (any(tmp[1] != tmp) || is.null(names(rhs))) {
      stop("Length of the list elements in 'rhs' must be identical.")
    }
    rhs <- as.data.frame(rhs, stringsAsFactors = FALSE)
  }
  if (!is.data.frame(rhs) && is.list(rhs) && length(rhs) == 1 && length(rhs[[1]]) == 1) {
    if (is.na(defVal)) defVal <- rhs[[1]]
    rhs <- data.frame()
    # rhs <- data.frame(rhs = rhs[[1]], stringsAsFactors = FALSE)
  }
  if (is.data.frame(rhs) && ncol(rhs) == 1 && nrow(rhs) == 1) {
    if (is.na(defVal)) defVal <- rhs[1, 1]
    rhs <- data.frame()
  }
  # if (is.numeric(rhs)) {
  #   if (length(rhs) != 1) {
  #     stop("rhs must be a single numeric value or a data.frame with sets and numeric values for each constraint.")
  #   }
  #   if (is.na(defVal)) defVal <- rhs
  #   rhs <- data.frame()
  # }
  # if (!is.data.frame(rhs) && is.list(rhs)) {
  #   xx <- sapply(rhs, length)
  #   if (any(xx[1] != xx)) {
  #     stop("Length of the list elements in 'rhs' must be identical.")
  #   }
  #   if (xx[1] >= 1) {
  #     xx <- data.frame(stringsAsFactors = FALSE)
  #     xx[seq_len(length(rhs[[1]])), ] <- NA
  #     for (i in names(rhs)) xx[[i]] <- rhs[[i]]
  #     rhs <- xx
  #   }
  # }
  # Replace zero values with 1e-20 in rhs and defVal
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

#' A function to check if an object is of class `constraint`.
#'
#' @param object any R object
#'
#' @return TRUE if the object inherits class `constraint`, FALSE otherwise.
#' @export
#'
#' @family class constraint
#' @describeIn newConstraint Check if an object is a constraint.
#'
#' @examples
#' isConstraint(1)
#' isConstraint(newConstraint(""))
isConstraint <- function(object) {
  inherits(object, "constraint")
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
    arg) {
  # browser()
  if (!is.null(names(arg))) {
    if (any(names(arg) == "variable")) variable <- arg$variable
    if (any(names(arg) == "mult")) mult <- arg$mult
    if (any(names(arg) == "for.sum")) for.sum <- arg$for.sum
    if (any(names(arg) == "defVal")) defVal <- arg$defVal
    if (any(names(arg) == "timeframe")) timeframe <- arg$timeframe
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
  # browser()
  if (all(names(.variable_set) != variable)) {
    stop(paste0('Unknown variable "', variable, '"in summand "', eqt@name, '"'))
  }
  need.set <- .variable_set[[variable]]
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

# Calculate do equation need additional set, and add it
.getSetEquation <- function(prec, stm, approxim) {
  # if (grepl("THERM", stm@name)) browser()
  # browser()
  # if (stm@name == "CO2_CAP") browser()
  # if (grepl("CESR_", stm@name)) browser()
  # if (grepl("CESR_5_2030", stm@name)) browser()
  # !!! add interpolation patch here? or in the calling function? !!!
  # if (nrow(stm@for.each) > 0) {
  # .interpolation0(stm@rhs, parameter = "rhs", defVal = stm@defVal,
  #                 arg = list(approxim = approxim)
  #                 )

  # temporary fix for constraints interpolation: expanding year set
  # works only for year set and if no NA values in the set
  if (!is.null(stm@rhs$year) && !any(is.na(stm@rhs$year))) {
    stm@rhs <- interpolate_slot(stm@rhs, val = "rhs")
  }
  if (!is.null(stm@for.each$year) && !any(is.na(stm@for.each$year))) {
    stm@for.each <- interpolate_slot(stm@for.each, val = NULL)
  }

  # !!! end
  stop.constr <- function(x) {
    stop(paste0('Constraint "', stm@name, '" error: ', x))
  }
  # (The legacy `get.all.child` slice-ancestry helper was removed: variable
  # domains now carry every slice level, so slice restrictions -- including
  # those derived from a summand `timeframe` -- are taken literally. See the
  # for.sum slice handling below and the `timeframe` resolution loop.)
  # all.set contain all set for for.each & lhs
  # Estimate is need sum for for.each
  # set.map need special mapping or consist all set
  all.set <- data.frame(
    alias = character(), # name in equation
    set = character(), # original set
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
    all.set[nn, "alias"] <- names(old_for_each)
    all.set[nn, "for.each"] <- TRUE
    for.each.set <- names(old_for_each)
    # Fill add.map for for.each
    for (j in for.each.set) {
      if (!is.null(old_for_each[[j]]) && !all(prec@set[[j]] %in% old_for_each[[j]])) {
        if (any(old_for_each[[j]] %in% prec@set[[j]])) {
          # warning(paste0('Set "'))
          old_for_each[[j]] <-
            old_for_each[[j]][old_for_each[[j]] %in% prec@set[[j]]]
        }
        set.map.name <- c(set.map.name, j)
        set.map[[length(set.map.name)]] <- old_for_each[[j]]
        all.set[nn[names(old_for_each) == j], "new.map"] <- length(set.map.name)
      }
    }
  } else {
    for.each.set <- NULL
  }

  # Resolve per-summand `timeframe` to an explicit `slice` restriction. The
  # variable is taken at exactly the slices of that timeframe level
  # (calendar@timeframes), so summing them yields the level aggregate without
  # double-counting across levels -- replacing the *RY year-resolution
  # variables. Applied here, before the slice handling below picks it up as a
  # for.sum restriction.
  for (i in seq_along(stm@lhs)) {
    # Defensive: summand objects serialized before the `timeframe` slot existed
    # lack it; treat those as no timeframe (NA).
    tf <- tryCatch(stm@lhs[[i]]@timeframe, error = function(e) NA_character_)
    if (length(tf) != 1 || is.na(tf)) next
    if (!("slice" %in% .variable_set[[stm@lhs[[i]]@variable]])) {
      stop.constr(paste0(
        'timeframe = "', tf, '" set on variable "', stm@lhs[[i]]@variable,
        '" which has no slice dimension.'
      ))
    }
    lev <- approxim$calendar@timeframes[[tf]]
    if (is.null(lev) || length(lev) == 0) {
      stop.constr(paste0(
        'unknown timeframe "', tf, '". Available: ',
        paste(names(approxim$calendar@timeframes), collapse = ", ")
      ))
    }
    stm@lhs[[i]]@for.sum$slice <- as.character(lev)
  }

  # lhs
  for (i in seq_along(stm@lhs)) {
    # browser()

    need.set <- .variable_set[[stm@lhs[[i]]@variable]]
    nn <- (nn[length(nn)] + seq_along(need.set))
    all.set[nn, "set"] <- need.set
    all.set[nn, "alias"] <- need.set
    all.set[nn, "lhs.num"] <- i
    if (any(names(stm@lhs[[i]]@for.sum) == "lag.year")) {
      if (all(need.set != "year")) {
        stop.constr("For lag.year have to define use variable with year")
      }
      all.set[nn[need.set == "year"], c("lag.year", "def.lhs")] <- TRUE
    }
    if (any(names(stm@lhs[[i]]@for.sum) == "lead.year")) {
      if (all(need.set != "year")) {
        stop.constr("For lead.year have to define use variable with year")
      }
      all.set[nn[need.set == "year"], c("lead.year", "def.lhs")] <- TRUE
    }
    all.set[nn[need.set %in% names(stm@lhs[[i]]@for.sum)], "def.lhs"] <- TRUE
    all.set[nn[!(need.set %in% for.each.set)], "def.lhs"] <- TRUE
    # Add to set map
    st <- names(stm@lhs[[i]]@for.sum)[names(stm@lhs[[i]]@for.sum) %in% need.set &
      !sapply(is.na(stm@lhs[[i]]@for.sum), all)]
    # Fill add.map for for.lhs
    for (j in st) {
      # Restrict to the EXACT for.sum values (incl. slice). The legacy
      # slice-ancestry expansion (get.all.child) is retired: variable domains
      # now carry every slice level, so a slice restriction -- e.g. one derived
      # from a summand `timeframe` -- must be taken literally to avoid
      # double-counting across levels.
      if (!is.null(stm@lhs[[i]]@for.sum[[j]]) &&
        !all(prec@set[[j]] %in% stm@lhs[[i]]@for.sum[[j]])) {
        # check if the same set in lhs exist
        fl <- FALSE
        if (all(!c(all.set[nn[need.set == j], c("lead.year", "lag.year")],
          recursive = TRUE
        ))) {
          fl <- nn[(!all.set$for.each[nn] & all.set$set[nn] == j &
            !is.na(all.set$new.map[nn]))]
        }
        add.new <- TRUE
        if (any(fl)) {
          for (k in all.set[fl, "new.map"]) {
            if (length(stm@lhs[[i]]@for.sum[[j]]) == length(set.map[[k]]) &&
              all(stm@lhs[[i]]@for.sum[[j]] %in% set.map[[k]]) && all(set.map[[k]] %in% stm@lhs[[i]]@for.sum[[j]])) {
              all.set[nn[need.set == j], "new.map"] <- k
              add.new <- FALSE
            }
          }
        }
        if (add.new) {
          set.map.name <- c(set.map.name, j)
          set.map[[length(set.map.name)]] <- stm@lhs[[i]]@for.sum[[j]]
          all.set[nn[need.set == j], "new.map"] <- length(set.map.name)
        }
      }
    }
  }
  # Add alias
  fl <- (!all.set$for.each & all.set$def.lhs & all.set$set %in% for.each.set)
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
    for (ss in st) approxim[[ss]] <- prec@set[[ss]]
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
  if (!is.null(stm@for.each$year)) {
    stm@for.each <- stm@for.each[stm@for.each$year %in% prec@parameters[["mMidMilestone"]]@data$year, , drop = FALSE]
    if (any(all.set$lag.year)) {
      stm@for.each <- stm@for.each[!(stm@for.each$year %in% prec@parameters[["mMilestoneFirst"]]@data$year), , drop = FALSE]
    }
    if (any(all.set$lead.year)) {
      stm@for.each <- stm@for.each[stm@for.each$year %in% prec@parameters[["mMilestoneHasNext"]]@data$year, , drop = FALSE]
    }
  }
  if (nrow(stm@for.each) > 0) {
    # browser()
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
    if (any(names(approxim2) == "slice")) {
      approxim2$slice <- approxim2$calendar@slice_share$slice
    }
    fl <- (all.set$for.each & !is.na(all.set$new.map) &
      all.set$set %in% colnames(stm@rhs))
    need.set <- all.set[fl, , drop = FALSE]
    for (j in seq_len(nrow(need.set))) {
      approxim2[[need.set[j, "set"]]] <- set.map[[need.set[j, "new.map"]]]
    }
    approxim2$fullsets <- approxim$fullsets
    need.set0 <- for.each.set[for.each.set %in% colnames(stm@rhs)]
    # browser()
    xx <- newParameter(paste0("pCnsRhs", stm@name), need.set0, "numpar",
      defVal = stm@defVal,
      # interpolation = "back.inter.forth",
      # interpolation = .defInt[["rhs"]], #!!! ToDO: add @defInt slot
      interpolation = stm@interpolation,
      colName = "rhs"
    )
    # !!! Similar interpolation for LHS is needed
    # browser()
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
      if (any(names(approxim2) == "slice")) {
        approxim2$slice <- approxim2$calendar@slice_share$slice
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
            if (any(nslc == "slice")) {
              approxim2$slice <- approxim$calendar@slice_share$slice
            }
          }
        }
      }
      approxim2$fullsets <- approxim$fullsets
      # browser()
      xx <- newParameter(paste0("pCnsMult", stm@name, "_", i),
        need.set,
        "numpar",
        defVal = stm@lhs[[i]]@defVal, # !!! Check
        # interpolation = "back.inter.forth"
        # interpolation = .defInt[["rhs"]] # !!! Temporary fix
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
      # Add mult
      # browser()
      vrb.lhs <- paste0(
        xx@name,
        if_else(length(need.set) > 0,
                paste0("(", paste0(need.set, collapse = ", "), ")"),
                ""),
        " * ", vrb.lhs
      )
    # } else if (stm@lhs[[i]]@defVal != 1) {
      vrb.lhs <- paste0(stm@lhs[[i]]@defVal, " * ", vrb.lhs)
    }
    # Replace setsname
    for (j in seq_len(nrow(lhs.set2))[lhs.set2$alias != lhs.set2$set]) {
      vrb.lhs <- gsub(paste0(" ", lhs.set2$set[j], " "), lhs.set2$alias[j], vrb.lhs)
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

  prec@gams.equation[[stm@name]] <- res
  prec
}

#  .getSetEquation(prec, stm, approxim)@gams.equation

#' @export
#' @family constraint policy
#' @rdname newConstraint
newConstraintS <- function(
    name,
    type,
    eq = "==",
    rhs = 0,
    for.sum = list(),
    for.each = list(),
    defVal = 0,
    rule = NULL,
    comm = NULL,
    cout = TRUE,
    cinp = TRUE,
    aout = TRUE,
    ainp = TRUE
  ) { # , emis = TRUE
  stop.newconstr <- function(x) {
    stop(paste0('Constraint "', name, '" error: ', x))
  }

  if (type == "tax") {
    return(newTax(name, tax = rhs, comm = comm, region = for.each$region, year = for.each$year, slice = for.each$slice))
  }
  if (type == "subs") {
    return(newSub(name, sub = rhs, comm = comm, region = for.each$region, year = for.each$year, slice = for.each$slice))
  }
  # if (any(grep('(share|growth)', type))) stop.newconstr(paste(type, 'have to do'))
  # For wich kind of variables (capacity, newcapacity, input or output)
  if (any(grep("inp", type))) {
    inpout <- "Inp"
  } else if (any(grep("out", type))) {
    inpout <- "Out"
  } else if (type == "capacity") {
    inpout <- "Cap"
  } else if (type == "newcapacity") {
    inpout <- "NewCap"
  } else if ("balance" == type) {
    inpout <- "Balance"
  } else {
    stop.newconstr(paste0("Unknown type: ", type))
  }
  #
  set.vec <- c(names(for.each), names(for.sum))
  psb.vec <- c("sup", "stg", "tech", "imp", "expp")
  psb.vec.tp <- c(
    sup = "Sup", stg = "Storage", tech = "Tech",
    imp = "Import", expp = "Export"
  )
  names(psb.vec) <- psb.vec
  is.set <- psb.vec[psb.vec %in% set.vec]
  if (length(is.set) > 1) {
    stop.newconstr(paste0("There are more than one subsets"))
  }
  vec.tp <- psb.vec.tp[is.set]
  if (length(is.set) == 0) {
    if (all(inpout != c("Inp", "Out", "Balance"))) {
      stop.newconstr(paste0("For ", type, " have to define some subsets"))
    }
    vrb <- paste0("v", inpout, "Tot"[inpout != "Balance"])
  } else {
    if (is.set == "tech" && inpout == "Inp") {
      vrb <- paste0("vTech", c("", "A")[c(cinp, ainp)], "Inp")
    } else if (is.set == "tech" && inpout == "Out") {
      vrb <- paste0("vTech", c("Out", "AOut")[c(cout, aout)]) # , 'EmsFuel' , emis
    } else if (any(type == c("newcapacity", "capacity"))) {
      if (all(is.set != c("stg", "tech"))) {
        stop.newconstr(paste0("For ", type, " could be define only for tech and storage"))
      }
      vrb <- paste0("v", vec.tp, c(capacity = "Cap", newcapacity = "NewCap")[type])
    } else {
      vrb <- paste0("v", vec.tp, inpout)
    }
  }
  term <- list(for.sum = for.sum, variable = vrb[1])
  arg <- list(term)
  for (i in vrb[-1]) {
    term$variable <- i
    arg[[length(arg) + 1]] <- term
  }
  # To share
  if (any(grep("share", type))) {
    if (length(c(rhs, recursive = TRUE)) == 0) {
      rhs <- (-defVal)
    } else if (is.list(rhs)) {
      rhs$value <- (-rhs$rhs)
      rhs$rhs <- NULL
    } else {
      rhs <- (-rhs)
    }
    # for (i in seq_along(arg)) {
    #   arg[[i]]$mult <- rhs
    # }
    term <- list(
      for.sum = for.sum[!(names(for.sum) %in% psb.vec)],
      variable = paste0("v", inpout, "Tot"), mult = rhs
    )
    rhs <- 0
    defVal <- 0
    arg[[length(arg) + 1]] <- term
  }
  # To growth
  if (any(grep("growth", type))) {
    if (length(c(rhs, recursive = TRUE)) == 0) {
      rhs <- (1 / defVal)
    } else {
      if (is.numeric(rhs)) {
        rhs <- (1 / rhs)
      } else {
        rhs$value <- (1 / rhs$rhs)
        rhs$rhs <- NULL
      }
    }
    nn <- seq_along(arg)
    nk <- length(arg)
    for (i in nn) {
      arg[[i + nk]] <- arg[[i]]
      arg[[i + nk]]$mult <- rhs
      arg[[i + nk]]$for.sum["lead.year"] <- list(NULL)
      arg[[i + nk]]$for.sum$year <- NULL
      arg[[i]]$mult <- (-1)
    }
    rhs <- 0
    defVal <- 0
  }
  newConstraint(name,
    eq = eq, for.each = for.each, defVal = defVal,
    rhs = rhs, arg = arg
  )
}
