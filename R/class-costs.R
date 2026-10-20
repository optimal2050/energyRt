## costs-class ####

#' @title An S4 class to add costs to objective function
#' @name class-costs
#'
#' @inherit newCosts description
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @md
#' @slot name `r get_slot_doc("costs", "name")`
#' @slot desc `r get_slot_doc("costs", "desc")`
#' @slot variable `r get_slot_doc("costs", "variable")`
#' @slot subset `r get_slot_doc("costs", "subset")`
#' @slot mult `r get_slot_doc("costs", "mult")`
#' @slot misc `r get_slot_doc("costs", "misc")`
#'
#' @include class-constraint.R
#' @family class constraint policy
#' @rdname class-costs
#' @export
setClass("costs",
  representation(
    name = "character",
    desc = "character", # desc
    variable = "character",
    subset = "data.frame",
    mult = "data.frame",
    # scalar multiplier used when `mult` carries no set columns; the compile
    # path (.getCostEquation) reads it, but the slot was never declared --
    # newCosts errored on every path that touched it
    defVal = "numeric",
    misc = "list"
    # parameter= list() # For the future
  ),
  prototype(
    name = NULL,
    desc = "", # desc
    variable = character(),
    subset = data.frame(),
    mult = data.frame(),
    defVal = 0,
    # ! Misc
    misc = list()
  ),
  S3methods = FALSE
)
setMethod("initialize", "costs", function(.Object, ...) {
  .Object
})

## constructor function ####
#' @title Create new costs object
#'
#' @description Costs object is used to define
#' additional costs to add to the model's
#' objective function.
#'
#' @md
#' @param name `get_slot_doc("costs", "name")`
#'
#' @param variable `get_slot_doc("costs", "variable")`
#' @param desc `get_slot_doc("costs", "desc")`
#' @param mult `r get_slot_doc("costs", "mult")`
#' @param subset `r get_slot_doc("costs", "subset")`
#' @param misc `r get_slot_doc("costs", "misc")`
#'
#' @return costs object with given specifications.
#' @family class constraint policy
#' @rdname newCosts
#'
#' @export
#'
#' @examples
#' # a per-unit fee on the activity of two technologies, stated as a rate table
#' newCosts("ACTFEE", variable = "vTechAct",
#'          mult = data.frame(tech = c("ECOA", "EGAS"), value = c(2, 3)),
#'          desc = "Activity fee")
#'
#' # one rate for a subset of processes: `mult` scalar, `subset` picks the members
#' newCosts("CAPFEE", variable = "vTechNewCap", mult = 1.5,
#'          subset = list(tech = "ECOA"))
newCosts <- function(
    name,
    variable,
    desc = "",
    mult = NULL,
    subset = NULL,
    misc = NULL
    ) {
  obj <- new("costs")
  obj@name <- name
  obj@desc <- desc

  # Add variable
  sets <- .variable_set[[variable]]
  if (is.null(sets)) {
    stop(paste0('There are unknown variable "', variable, '" in cost "', name,
                '".'))
  }
  # if (anyDuplicated(sets))
  #       stop(paste0('Add cost to variable with duplicated sets is not allowed now (cost "', name, '").'))
  if (anyDuplicated(sets)) {
    sets[duplicated(sets)] <- paste0(sets[duplicated(sets)], 2)
  }
  if (sum(sets %in% c("region", "year")) != 2) {
    stop(paste0(
      "The cost-object accepts variables with 'region' and 'year' dimensions.\n",
      "  the variable '", variable, "' has the following sets: ",
      paste0(sets, collapse = ", ")))
  }
  obj@variable <- variable

  # Add subset
  if (!is.null(subset)) {
    if (is.list(subset) && !is.data.frame(subset)) {
      # `[[<-` on a 0-row data.frame errors, so build directly from the list
      subset <- as.data.frame(subset[!vapply(subset, is.null, logical(1))],
                              stringsAsFactors = FALSE)
    }
    if (!all(colnames(subset) %in% sets)) {
      bug <- colnames(subset)[!(colnames(subset) %in% sets)]
      stop(paste0(
        # "There ", c("is", "are")[1 + length(bug) != 1],
        "Unrecognized column",
        "s"[length(bug) != 1], ' "', paste0(bug, collapse = '", "'),
        '" in subset (cost "', name, '").'
      ))
    }
    if (!is.data.frame(subset)) {
      stop(paste0('Subset must be a list or data.frame (cost "', name, '").'))
    }
    if (anyDuplicated(subset)) {
      stop(paste0('Duplicated row(s) in subset (cost "', name, '").'))
    }
    subset <- subset[, !apply(is.na(subset), 2, all), drop = FALSE]
    obj@subset <- subset
  }

  # Add mult
  if (!is.null(mult)) {
    if (!is.data.frame(mult) && !is.numeric(mult)) {
      stop(paste0('Mult must be numeric or data.frame (cost "', name, '").'))
    }
    if (is.numeric(mult)) {
      if (length(mult) != 1) {
        stop(paste0('A numeric mult must be a single value (cost "', name, '").'))
      }
      # scalar multiplier: same treatment as a set-less mult data.frame
      obj@defVal <- mult
    } else {
      if (!all(colnames(mult) %in% c("value", sets))) {
        bug <- colnames(mult)[!(colnames(mult) %in% c("value", sets))]
        stop(paste0(
          # "There ", c("is", "are")[1 + length(bug) != 1],
          "Unrecognized column",
          "s"[length(bug) != 1], ' "', paste0(bug, collapse = '", "'),
          '" in mult (cost "', name, '").'
        ))
      }
      mult <- mult[, !apply(is.na(mult), 2, all), drop = FALSE]
      # if (anyDuplicated(mult[, colnames(mult) != "value", drop = FALSE])) {
      if (anyDuplicated(select(mult, -value))) {
        stop(paste0('Duplicated row(s) in mult (cost "', name, '").'))
      }
      if (is.null(mult$value) || any(is.na(mult$value))) {
        stop(paste0('NAs in mult (cost "', name, '").'))
      }
      # Remove unused set values from mult (by subset)
      if (any(colnames(mult) %in% colnames(subset))) {
        for (ss in colnames(mult)[colnames(mult) %in% colnames(subset)]) {
          mult <- mult[is.na(mult[[ss]]) | mult[[ss]] %in%
            unique(subset[[ss]][!is.na(subset[[ss]])]), , drop = FALSE]
        }
      }
      if (ncol(mult) > 1) obj@mult <- mult else obj@defVal <- mult$value
    }
  }
  if (nrow(obj@mult) == 0 && obj@defVal == 0) {
    warning(paste0('The cost constraint of the "', name, '" is strictly equal to zero.'))
  }
  if (!is.null(misc)) obj@misc <- misc
  obj
}


## internal functions ####
# Check if the constraint needs additional set(s), add if needed
# A `subset`/`mult` filter that removes every row leaves an empty map, which
# compiles to a sum over an empty set -- a cost of zero, with no error. Name the
# values that matched nothing, and what was available.
.stop_cost_filter <- function(obj_name, set_name, asked, available, slot_name) {
  asked <- unique(as.character(asked[!is.na(asked)]))
  miss <- setdiff(asked, as.character(available))
  if (length(miss) == 0) miss <- asked
  avail <- unique(as.character(available))
  stop("cost object '", obj_name, "': `", slot_name, "` names ", set_name,
       " value(s) ", paste0('"', miss, '"', collapse = ", "),
       " that the model does not have, so the cost would be silently zero.",
       if (length(avail)) paste0(
         " Available ", set_name, ": ",
         paste0('"', utils::head(avail, 8), '"', collapse = ", "),
         if (length(avail) > 8) ", ..." else "") else "",
       call. = FALSE)
}

.getCostEquation <- function(prec, stm, approxim) {
  stop.constr <- function(x) {
    stop(paste0('Cost "', stm@name, '" error: ', x))
  }
  get.all.child <- function(x) {
    unique(c(x, c(approxim$calendar@timeslice_ancestry[
      approxim$calendar@timeslice_ancestry$parent %in% x, "child"])))
  }
  have.all.set <- function(x, name) {
    return(any(is.na(x)) || (name != "timeslice" && all(approxim[[name]] %in% x)))
  }
  sets <- .variable_set[[stm@variable]]
  if (anyDuplicated(sets)) {
    for (dst in sets[duplicated(sets)]) {
      approxim[[paste0(dst, 2)]] <- approxim[[dst]]
    }
    sets <- .dedup2(sets) # the shared spelling of the "2"-suffix convention
  }
  # Generate mult
  if (nrow(stm@mult) != 0) {
    approxim2 <- approxim[
      unique(c(colnames(stm@mult)[colnames(stm@mult) %in% names(approxim)],
               "fullsets", "solver", "year"))]
    # `calendar` is read from the FULL context: `approxim2` keeps only the
    # `mult` columns plus fullsets/solver/year, so `approxim2$calendar` is
    # always NULL and `NULL@timeslice_share` errored on any `mult` carrying a
    # `timeslice` column -- i.e. on every timeslice-separable cost.
    if (!is.null(approxim2$timeslice)) {
      approxim2$timeslice <- approxim$calendar@timeslice_share$timeslice
    }
    if (!is.null(approxim2$timeslice2)) {
      if (isDebug())
        cli::cli_warn("timeslice2 approximation path taken; see the ??? notes here.")
      approxim2$timeslice2 <- approxim$calendar@timeslice_share$timeslice #???
    }
    if (nrow(stm@subset) != 0) {
      same <- colnames(stm@subset)[colnames(stm@subset) %in% colnames(stm@mult)]
      same <- same[!apply(is.na(stm@subset[, same, drop = FALSE]), 2, any)]
      for (ss in same) {
        approxim2[[ss]] <- unique(stm@subset[[ss]][!is.na(stm@subset[[ss]])])
      }
    }
    mult_sets <- colnames(stm@mult)[colnames(stm@mult) != "value"]
    for (ss in mult_sets) {
      before <- stm@mult
      stm@mult <- stm@mult[stm@mult[[ss]] %in% approxim2[[ss]], , drop = FALSE]
      # An empty filter result compiles to a sum over an empty set, i.e. a
      # silent zero cost. Name what matched nothing instead.
      if (nrow(before) > 0 && nrow(stm@mult) == 0) {
        .stop_cost_filter(stm@name, ss, before[[ss]], approxim2[[ss]], "mult")
      }
    }
    xx <- newParameter(paste0("pCosts", stm@name), mult_sets, "numpar",
      defVal = 0,
      interpolation = "back.inter.forth", colName = "value"
    )
    yy <- .interp_numpar(stm@mult, "value", xx, approxim2)
    prec@parameters[[xx@name]] <- .dat2par(xx, yy)
    sss <- ""
    if (length(mult_sets) != 0) sss <- paste0("(", paste0(mult_sets,
                                                          collapse = ", "),
                                              ")")
    mult_txt <- paste0(xx@name, sss, " * ")
  } else {
    mult_txt <- paste0(stm@defVal, " * ")
  }

  # Generate subset
  if (nrow(stm@subset) != 0) {
    fl <- apply(!is.na(stm@subset), 1, all)
    subset <- stm@subset[fl, , drop = FALSE]
    approxim2 <- approxim[colnames(stm@subset)]
    if (!is.null(approxim2$timeslice)) approxim2$timeslice <- approxim2$calendar@timeslice_share$timeslice
    if (any(!fl)) {
      subset_na <- stm@subset[!fl, , drop = FALSE]
      for (i in seq_len(ncol(subset_na))[apply(is.na(subset_na), 2, any)]) {
        f2 <- is.na(subset_na[[i]])
        subset_na2 <- subset_na[fl, , drop = FALSE]
        subset_na3 <- subset_na2[0, , drop = FALSE]
        ncs <- approxim2[[colnames(subset_na)[i]]]
        subset_na3[nrow(subset_na2) * length(ncs), ] <- NA
        for (j in seq_len(ncol(subset_na))[seq_len(ncol(subset_na)) != i]) {
          subset_na3[, j] <- subset_na3[[j]]
        }
        subset_na3[, j] <- c(matrix(ncs, nrow(subset_na3), length(ncs), byrow = TRUE))
        subset_na <- unique(rbind(subset_na[!fl, , drop = FALSE], subset_na3))
      }
      subset <- rbind(subset, subset_na)
    }
    for (ss in colnames(subset)) {
      before <- subset
      subset <- subset[subset[[ss]] %in% approxim2[[ss]], , drop = FALSE]
      if (nrow(before) > 0 && nrow(subset) == 0) {
        .stop_cost_filter(stm@name, ss, before[[ss]], approxim2[[ss]], "subset")
      }
    }

    xnm <- paste0("mCosts", stm@name)
    prec@parameters[[xnm]] <- .dat2par(newParameter(xnm, colnames(subset), "map"),
                                       subset)
    subset_txt <- paste0(xnm, "(", paste0(colnames(subset), collapse = ", "), ")")
  } else {
    subset_txt <- NULL
  }

  # Generate equation text
  mps <- .variable_mapping[[stm@variable]]
  if (length(sets) == 2) {
    if (is.null(subset_txt)) {
      costs <- paste0(mult_txt, mps)
    } else if (any(grep("[$]", mps))) {
      costs <- paste0(
        "(", mult_txt, gsub("[$].*", "", mps), ")$(", gsub(".*[$]", "", mps),
        " and ", subset_txt, ")"
      )
    } else {
      # close the multiplier group BEFORE the guard, as the `$`-carrying
      # branch above does -- this previously emitted unbalanced parentheses
      costs <- paste0("(", mult_txt, mps, ")$", subset_txt)
    }
  } else {
    nset <- sets[!(sets %in% c("region", "year"))]
    if (length(nset) != 1) nset <- paste0("(", paste0(nset, collapse = ", "), ")")
    mps <- gsub("[(][^(]*[)]", paste0("( ", paste0(sets, collapse = " , "), " )"), mps)
    nkk <- c(gsub(".*[$]", "", mps), subset_txt)
    if (length(nkk) != 1) nkk <- paste0("(", paste0(nkk, collapse = " and "), ")")
    # A per-timeslice variable is a RATE; `pTimesliceWeight` scales it to the
    # year, and on a sampled calendar it is the sampling factor (up to 91 for
    # one day per season). Every native cost equation carries it -- see
    # `eqTechVarom` and `eqTaxCost` -- and omitting it here under-charged a
    # user cost by that factor on any sampled calendar.
    wgt_txt <- if ("timeslice" %in% sets) "pTimesliceWeight(year, timeslice) * " else ""
    costs <- paste0("sum(", nset, "$", nkk, ", ", mult_txt, wgt_txt,
                    gsub("[$].*", "", mps), ")")
  }
  costs <- gsub("[ ]*[*][ ]*", " * ", gsub("[)]and", ") and", gsub("[ ]*[,][ ]*", ", ", gsub(
    "[ ]*[)][ ]*", ")",
    gsub("[ ]*[(][ ]*", "(", gsub("[+][ ]*[-]", "-", gsub("[ ]*[$][ ]*", "$", costs)))
  ))))
  prec@user_costs <- c(prec@user_costs, costs)
  prec
}

#  .getSetEquation(prec, stm, approxim)@user_constraints
