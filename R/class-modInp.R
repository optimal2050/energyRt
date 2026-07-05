#' An S4 class to represent model input
#' @name class-modInp
#'
#' @description
#' `modInp` class is used to store interpolated date for the model input parameters.
#' It includes all the model sets, mappings, and parameters, interpolated to the
#' scenario's calendar and horizon. The class is automatically created during the
#' interpolation step and is not intended to be created by users.
#'
#' @slot set `r get_slot_doc("modInp", "set")`
#' @slot parameters `r get_slot_doc("modInp", "parameters")`
#' @slot gams.equation `r get_slot_doc("modInp", "gams.equation")`
#' @slot costs.equation `r get_slot_doc("modInp", "costs.equation")`
#' @slot misc `r get_slot_doc("modInp", "misc")`
#'
#' @include class-parameter.R
#'
setClass(
  "modInp",
  representation(
    set = "list", # !!! renaming - to be removed
    sets = "list", # !!! transiting from `set` to 'sets'
    parameters = "list", #
    # modelVersion = "character",  # !!! in use ???
    # solver = "character", # !!! in use ???
    gams.equation = "list", # user_constraints?
    costs.equation = "character", # list? user_costs?
    misc = "list"
  ),
  prototype(
    set = list(), # !!! to be removed
    sets = list(), # !!! transiting from `set` to 'sets'
    parameters = list(),
    # modelVersion = "",
    # solver = "",
    gams.equation = list(),
    costs.equation = character(),
    misc = list()
  )
)

#' Initialization of `modInp` class.
#'
#' @details
#' This is an internal method, it is called during creation of `scenario` objects, and it is not intended to run by users.
#' Initialization of `modInp` adds empty structures of the model sets, mappings, and parameters to the `modInp@parameters` slot. The `@defVal` and `@interpolation` are filled with default values from internal `.modInp` list object (edit `modInp.yml` and rebuild to modify). The `@data` slot is empty (added on the interpolation step).
#'
#' @param modInp an uninitialized (created by "new", empty) model input class object.
#' @noRd
setMethod("initialize", "modInp", function(.Object) {
  # browser()
  # x <- .Object@parameters
  x <- list()
  # .dimSets <- c("horizon", .dimSets) |> unique() #!!! test
  # x[["horizon"]] <- newSet("horizon")
  ob <- .modInp
  for (i in 1:length(ob)) {
    nm <- ob[[i]]$name
    # if (nm == "DEBUG") browser() # DEBUG
    # if (nm == "pTechRet") browser() # DEBUG
    if (ob[[i]]$type == "set") {
      # browser()
      x[[nm]] <- newSet(nm)
    } else if (ob[[i]]$type == "map") {
      x[[nm]] <- newParameter(
        nm,
        ob[[i]]$dimSets,
        type = ob[[i]]$type
      )
    } else {
      # if (nm == "pStorageCout") browser()
      x[[nm]] <- newParameter(
        nm,
        ob[[i]]$dimSets,
        type = ob[[i]]$type,
        defVal = ob[[i]]$defVal,
        interpolation = ob[[i]]$interpolation,
        colName = ob[[i]]$colName,
        cls = ob[[i]]$class,
        dropIfEmpty = ob[[i]]$dropIfEmpty,
        prune = ob[[i]]$prune
      )
    }
  }
  # browser()
  .Object@parameters <- x
  return(.Object)
})

# ============================================================================ #
# Internal functions ####
# ============================================================================ #
.get_default_values <- function(modInp, name, drop.unused.values) {
  # Returns data.frame with default values of parameters on
  #       expanded grid of all (or used only, like horizon-mid-period)
  #       values of the parameter dimension (e.g. sets)
  # name - "character", name of the parameter
  # drop_duplicates <- function(x) x[!duplicated(x), , drop = FALSE]
  # if (name == "pTechShare") browser() # DEBUG
  drop_duplicates <- function(x) filter(x, !duplicated(x))
  sets0 <- modInp@parameters[[name]]@dimSets
  sets <- NULL
  for (i in sets0) {
    j <- i
    if (any(i == c("src", "dst"))) j <- "region"
    tmp <- .get_data_slot(modInp@parameters[[j]])
    colnames(tmp) <- i
    if (nrow(tmp) == 0) {
      return(NULL)
    }
    if (drop.unused.values) {
      if (i == "slice" && any(colnames(sets) == "comm")) {
        tmp <- merge(.get_data_slot(modInp@parameters$mCommSlice), tmp)
      }
      if (i == "comm" && any(colnames(sets) == "sup")) {
        tmp <- merge(.get_data_slot(modInp@parameters$mSupComm), tmp)
      }
      if (i == "region" && any(colnames(sets) == "sup") &&
          all(sets0 != "year")) {
        tmp <- merge(drop_duplicates(
          .get_data_slot(modInp@parameters$mSupSpan)[, c("sup", "region")]
        ), tmp)
      }
      if (i == "year" && any(colnames(sets) == "sup") &&
          any(colnames(sets) == "region")) {
        tmp <- merge(.get_data_slot(modInp@parameters$mSupSpan), tmp)
      }
      if (i == "year") {
        tmp <- merge(.get_data_slot(modInp@parameters$mMidMilestone), tmp)
      }
      if (i == "year" && any(colnames(sets) == "tech")) {
        tmp <- merge(.get_data_slot(modInp@parameters$mTechSpan), tmp)
      }
      if (i == "region" && any(colnames(sets) == "tech") &&
          all(sets0 != "year")) {
        tmp <- merge(drop_duplicates(
          .get_data_slot(modInp@parameters$mTechSpan)[, c("tech", "region")]
        ), tmp)
      }
      if (i == "comm" && any(colnames(sets) == "tech")) {
        tmp <- merge(rbind(
          .get_data_slot(modInp@parameters$mTechInpComm),
          .get_data_slot(modInp@parameters$mTechOutComm)
        ), tmp)
      }
      if (i == "slice" && any(colnames(sets) == "tech")) {
        tmp <- merge0(.get_data_slot(modInp@parameters$mTechSlice), tmp)
      }
      if (i == "src") {
        aa <- .get_data_slot(modInp@parameters$mTradeSrc)
        colnames(aa)[2] <- "src"
        tmp <- merge(aa, tmp)
      }
      if (i == "dst") {
        aa <- .get_data_slot(modInp@parameters$mTradeDst)
        colnames(aa)[2] <- "dst"
        tmp <- merge(aa, tmp)
      }
      if (i == "comm" && any(colnames(sets) == "trade")) {
        tmp <- merge(.get_data_slot(modInp@parameters$mTradeComm), tmp)
      }
    }
    if (is.null(sets)) {
      sets <- tmp
    } else {
      # browser()
      sets <- merge0(sets, tmp)
    }
  }
  if (modInp@parameters[[name]]@type == "numpar" &&
      (is.null(sets) || nrow(sets) != 0)) {
    sets$value <- modInp@parameters[[name]]@defVal
    if (!is.data.frame(sets)) sets <- as.data.frame(sets)
  }
  if (modInp@parameters[[name]]@type == "bounds" &&
      (is.null(sets) || nrow(sets) != 0)) {
    sets$type <- "lo"
    sets$value <- modInp@parameters[[name]]@defVal[1]
    sets2 <- sets
    sets2$type <- "up"
    sets2$value <- modInp@parameters[[name]]@defVal[2]
    sets <- rbind(sets, sets2)
  }
  sets
}

.add_dropped_zeros <- function(modInp, name, drop.unused.values = TRUE,
                               use.dplyr = FALSE) {
  # Returns data.frame filled the parameter ("name") data with added, previous dropped zeros (if any)
  # rare use - currently reserved for "fix to scenario" routines (and some excessive/double-checking use)
  # if (name == "pTechShare") browser() # DEBUG
  tmp <- .get_default_values(modInp, name, drop.unused.values)
  # tmp$value <- 0
  dtt <- .get_data_slot(modInp@parameters[[name]])
  # browser()
  if (!is.null(tmp)) {
    if (use.dplyr) {
      cols <- colnames(dtt)
      # gg <- suppressMessages(dplyr::anti_join(tmp, dtt[, cols],
      #                                         by = cols[cols != "value"]))
      gg <- suppressMessages(dplyr::anti_join(tmp, dtt, by = cols[cols != "value"]))
      gg <- suppressMessages(dplyr::left_join(dtt, gg))
      return(gg)
    } else {
      if (ncol(dtt) == ncol(tmp)) {
        gg <- rbind(dtt, tmp)
      } else {
        # gg <- rbind(dtt, unique(tmp[, colnames(dtt), drop = FALSE]))
        if (anyDuplicated(colnames(dtt))) browser() # mappings check
        gg <- rbind(dtt, unique(select(tmp, all_of(colnames(dtt)))))
      }
      if (ncol(gg) == 1) {
        return(dtt)
      }
    }
  } else {
    gg <- dtt
  }
  # gg[!duplicated(gg[, colnames(gg) != "value"]), , drop = FALSE]
  ii <- gg |> select(-value) |> duplicated()
  filter(gg, !ii)
}

#### end ===================================================================####
