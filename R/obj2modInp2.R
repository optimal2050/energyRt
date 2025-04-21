setGeneric("ob2mi", function(scen, obj, extra_params) standardGeneric("ob2mi"))
setGeneric("d2p", function(obj, data, path) standardGeneric("d2p"))

get_data_slot <- function(obj) {
  # browser()
  data <- NULL
  if (isOnDisk(obj)) {
    data <- get_lazy_data(obj, "data")
  }
  if (is.null(data)) {
    data <- obj@data
  }
  return(data)
}

# d2p: data.frame ####
# The method adds data in data.frame format to the parameter object.
# If the parameter is on-disk, it will be added to the existing data on disk.
setMethod(
  "d2p",
  signature(obj = "parameter", data = "data.frame", path = "character"),
  function(obj, data, path = NULL) {
    # ondisk <- !is.null(path) && isOnDisk(obj)
    # browser()

    # args <- list(...)

    if (is.null(data)) {
      return(obj)
    }
    if (nrow(data) == 0) {
      return(obj)
    }
    if (!is.null(path)) {
      obj <- setObjPath(obj, path) |> mark_ondisk()
    }

    # check column number
    if (ncol(data) != ncol(obj@data)) {
      stop(
        "Parameter dimensions mismatch: ", obj@name,
        "\nExpected: ", paste(colnames(obj@data), collapse = ", "),
        "\nGot: ", paste(colnames(data), collapse = ", "),
        "\nParameter: ", obj@name, "\nData:\n", head(data), "\n"
      )
    }

    # check column names
    if (any(sort(colnames(data)) != sort(colnames(obj@data)))) {
      stop(
        "Parameter columns mismatch: ", obj@name,
        "\nExpected: ", paste(colnames(obj@data), collapse = ", "),
        "\nGot: ", paste(colnames(data), collapse = ", "),
        "\nParameter: ", obj@name, "\nData:\n", head(data), "\n"
      )
    }

    # check bounds values
    if (any(colnames(data) == "type")) {
      if (any(!(data$type %in% c("lo", "up")))) {
        stop(
          "Unrecognized type of bounds in parameter ", obj@name, "\n",
          "Expected: lo, up\nGot: ", paste(unique(data$type), collapse = ", "),
          "\nParameter: ", obj@name, "\nData:\n", head(data), "\n"
        )
      }
      data$type <- factor(data$type, levels = c("lo", "up"))
    }

    # order columns
    # data <- .force_year_class_df(data)
    # data <- .force_value_class_df(data)
    data <- force_cols_classes(data)
    data <- select(data, all_of(colnames(obj@data)))
    data <- as.data.table(data)

    # convert factors to characters (??? replace with a function ???)
    # for (i in colnames(data)[sapply(data, class) == "factor"]) {
    #   if (i != "type") data[[i]] <- as.character(data[[i]])
    # }

    # check for class mismatch
    if (isInMemory(obj) && any(sapply(data, class) != sapply(obj@data, class))) {
      stop(
        "Column classes mismatch: ", obj@name, "\n",
        "Expected: ", paste(sapply(obj@data, class), collapse = ", "),
        "\nGot: ", paste(sapply(data, class), collapse = ", "),
        "\nParameter: ", obj@name, "\nData:\n", head(data), "\n"
      )
    }

    # check for NA values
    if (any(is.na(data$value))) {
      stop(
        "NA values in parameter ", obj@name, "\n",
        "Parameter: ", obj@name, "\nData:\n", head(data), "\n"
      )
    }

    # combine existing data with new data
    # browser()
    data_exist <- get_data_slot(obj) |> force_cols_classes() # !!! ToDo: add filters
    data <- rbindlist(list(data_exist, data),
      use.names = TRUE,
      ignore.attr = TRUE # workaround for NA values in csv files
    )
    data <- unique(data)

    if (isOnDisk(obj)) {
      if (is.null(path)) {
        path <- getObjPath(obj)
        if (is.null(path)) {
          stop("Path to the parameter ", obj@name, " is not specified.")
        }
      }
      # write data to disk
      partitioning_dim <- NULL
      if (any(colnames(data) == "year")) {
        partitioning_dim <- "year"
      }

      # browser()
      obj@data <- data
      obj <- obj2disk(obj)
      # arrow::write_dataset(
      #   data,
      #   path = path,
      #   format = "parquet",
      #   partitioning = partitioning_dim,
      #   existing_data_behavior = "overwrite" # !!! ToDo: consider on-disk merge
      # )
      # obj@data <- obj@data[0,] # clear data in memory
      obj@data <- reset_slot(obj@data)
    } else {
      # assign data to the parameter
      obj@data <- data
    }
    # update the number of values in the parameter
    # obj@misc$nValues <- obj@misc$nValues + nrow(data)
    return(obj)
  }
)

# d2p: character ####
setMethod(
  "d2p",
  signature(obj = "parameter", data = "character", path = "character"),
  function(obj, data, path = NULL) {
    # browser()

    if (is.null(data)) {
      return(obj)
    }
    if (length(data) == 0) {
      return(obj)
    }
    if (!is.null(path)) {
      obj <- setObjPath(obj, path) |> mark_ondisk()
    }

    if (obj@type != "set") {
      message("Error: ", obj@name, " parameter:")
      print(head(data))
      stop(
        "Set type of parameter is expected for the character data. \n",
        "Parameter: ", obj@name, ", data: ", head(data), "..."
      )
    }
    if (!all(is.character(data))) {
      stop(
        "Assigning non-character (", class(data),
        ") data to the character set ", obj@name
      )
    }
    if (length(data) == 0) {
      obj@data <- as.data.table(obj@data)
      return(obj)
    }
    if (any(is.na(data))) {
      stop(
        "NA values in parameter ", obj@name, "\n",
        "Parameter: ", obj@name, "\nData:\n", head(data), "\n"
      )
    }
    if (any(data == "")) {
      stop(
        "Empty values in parameter ", obj@name, "\n",
        "Parameter: ", obj@name, "\nData:\n", head(data), "\n"
      )
    }
    if (any(data == " ")) {
      stop(
        "Empty values in parameter ", obj@name, "\n",
        "Parameter: ", obj@name, "\nData:\n", head(data), "\n"
      )
    }
    if (isOnDisk(obj)) {
      if (is.null(path)) {
        stop("Path to the parameter ", obj@name, " is not specified.")
      }

      # load data from disk
      data_exist <- get_data_slot(obj) |>
        force_cols_classes() # doesn't work for query

      # combine existing data with new data
      data <- rbindlist(
        list(as.data.table(data_exist), as.data.table(data)),
        use.names = FALSE # avoid step of assignment of column name
        # ignore.attr = TRUE # workaround for NA values in csv files
      ) |>
        unique()

      # write data to disk
      partitioning_dim <- NULL
      obj2disk(obj, path = path)
    } else {
      obj@data <- rbindlist(
        list(as.data.table(obj@data), as.data.table(data)),
        use.names = FALSE
      ) |>
        unique()
    }


    if (ncol(obj@data) != 1) browser()
    if (is.factor(obj@data[[1]])) browser()
    # obj@misc$nValues <- obj@misc$nValues + length(data)
    obj
  }
)

#' Update parameter in the scenario by adding data to it
#'
#' @param scen scenario object
#' @param param character, name of the parameter to update
#' @param data data.frame, data to update the parameter with
#' @param path character, path to the parameter on disk,
#' if NULL, the function will try to create the path from the path to
#' parameters in `scen@modInp@parameters` and the name of the parameter.
#'
#' @returns
#' @export
#'
#' @examples
update_parameter <- function(scen, param, data, path = NULL) {
  # browser()

  if (is.null(data)) {
    return(scen)
  }
  if (!inherits(data, "data.frame")) {
    stop("Data must be a data.frame, not ", class(data))
  }
  if (nrow(data) == 0) {
    return(scen)
  }

  if (isOnDisk(scen)) {
    if (is.null(path)) {
      # check if the path exists in the parameter
      path <- getObjPath(scen@modInp@parameters[[param]])
      if (is.null(path)) {
        path <- getObjPath(scen@modInp)
        if (is.null(path)) {
          stop(
            "Cannot create path to the parameter ", param, ".",
            "The path to the parameters is not specified in the scenario."
          )
        } else {
          path <- fp(path, param)
        }
      }
    }
    # set the path to the parameter and mark it as on disk
    scen@modInp@parameters[[param]] <- scen@modInp@parameters[[param]] |>
      setObjPath(path) |>
      mark_ondisk()
  }
  # update the parameter with the new data
  scen@modInp@parameters[[param]] <- scen@modInp@parameters[[param]] |>
    d2p(data, path = path)

  return(scen)
}

# ob2mi: scenario, ... ####
# =============================================================================#
## commodity ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "commodity", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    # .checkSliceLevel(obj, extra_params)
    # .check_timeframe(obj, scen)
    # browser()

    obj@name <- toString(obj@name)
    # obj <- .filter_data_in_slots(obj, extra_params$region, "region")

    ## pEmissionFactor ####
    dat <- data.table(
      comm = obj@name,
      commp = obj@emis$comm,
      value = as.numeric(obj@emis$emis)
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pEmissionFactor", dat)

    ## pAggregateFactor ####
    dat <- data.table(
      comm = obj@name,
      commp = obj@agg$comm,
      value = as.numeric(obj@agg$agg)
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pAggregateFactor", dat)

    ## mUpComm | mLoComm | mFxComm ####
    if (obj@limtype == "UP") {
      param <- "mUpComm"
    } else if (obj@limtype == "LO") {
      param <- "mLoComm"
    } else if (obj@limtype == "FX") {
      param <- "mFxComm"
    } else {
      stop("Unrecognized commodity type: ", obj@limtype, " in ", obj@name)
    }
    scen <- update_parameter(scen, param, data.table(comm = obj@name))

    ## mCommSlice ####
    comm_timeframe <- obj@timeframe
    if (is_empty(comm_timeframe)) {
      comm_timeframe <- scen@settings@calendar@default_timeframe
    }
    com_slice <- scen@settings@calendar@timeframes[[comm_timeframe]]
    dat <- data.table(
      comm = obj@name,
      slice = com_slice
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "mCommSlice", dat)

    # browser()
    ## pDummyImportCost ####
    # !!! ToDo: move to "generic parameters"
    # if (any(is.na(extra_params$debug$comm) | extra_params$debug$comm == obj@name)) {
    #   extra_params$debug$comm[is.na(extra_params$debug$comm)] <- obj@name
    #   dbg <- extra_params$debug[
    #     !is.na(extra_params$debug$comm) &
    #       extra_params$debug$comm == obj@name, ,
    #     drop = FALSE
    #   ]
    #   extra_params$comm <- obj@name
    #   scen@parameters[["pDummyImportCost"]] <- .dat2par(
    #     scen@parameters[["pDummyImportCost"]],
    #     .interp_numpar(
    #       dbg, "dummyImport",
    #       scen@parameters[["pDummyImportCost"]], extra_params
    #     )
    #   )
    #   scen@parameters[["pDummyExportCost"]] <- .dat2par(
    #     scen@parameters[["pDummyExportCost"]],
    #     .interp_numpar(
    #       dbg, "dummyExport",
    #       scen@parameters[["pDummyExportCost"]], extra_params
    #     )
    #   )
    # }
    scen
  }
)

# =============================================================================#
## demand ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "demand", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    # dem <- mod@data$utopia_repository@data$DEM_ELC
    dem <- obj
    dem@name <- toString(dem@name)

    # browser()

    # check if only one commodity is specified
    if (length(dem@commodity) != 1) {
      stop(
        "Demand must have exactly one commodity. ", dem@name, " has ",
        length(dem@commodity), " commodities: ",
        paste(dem@commodity, collapse = ", ")
      )
    }

    # check if commodity is not NA
    if (is.na(dem@commodity)) {
      stop("Demand commodity in ", dem@name, " is NA.")
    }

    # check if commodity is in the list of commodities
    if (!(dem@commodity %in% scen@modInp@sets$comm)) {
      stop(
        'Commodity "', dem@commodity, '" used in demand "', dem@name,
        '" is not declared in the model.'
      )
    }

    # dem <- .filter_data_in_slots(dem, approxim$region, "region")
    # approxim <- .fix_approximation_list(approxim, comm = dem@commodity)
    # dem <- .disaggregateSliceLevel(dem, approxim)

    ## pDemand ####
    dat <- data.table(
      dem = dem@name,
      comm = dem@commodity,
      region = dem@dem$region,
      year = dem@dem$year,
      slice = dem@dem$slice,
      value = as.numeric(dem@dem$dem)
    ) |>
      force_cols_classes()

    if (length(dem@region) != 0) {
      # filter out unused regions
      if (any(!(dem@region %in% scen@modInp@sets$region))) {
        stop(
          'Region "', dem@region, '" used in demand "', dem@name,
          '" is not declared in the model.'
        )
      }
      dat <- dat[region %in% dem@region]
    }
    scen <- update_parameter(scen, "pDemand", dat)

    scen
  }
)

# =============================================================================#
## export ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "export", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    # .checkSliceLevel(app, approxim)
    # browser()
    exp <- obj
    exp@name <- toString(exp@name)
    if (length(exp@commodity) != 1) {
      stop(
        "Export must have exactly one commodity. ", exp@name, " has ",
        length(exp@commodity), " commodities: ",
        paste(exp@commodity, collapse = ", ")
      )
    }
    if (is.na(exp@commodity)) {
      stop("Export commodity in ", exp@name, " is NA.")
    }
    if (!(exp@commodity %in% scen@modInp@sets$comm)) {
      stop(
        'Commodity "', exp@commodity, '" used in export "', exp@name,
        '" is not declared in the model.'
      )
    }
    # exp <- .filter_data_in_slots(exp, approxim$region, "region")
    # browser()
    # approxim <- .fix_approximation_list(approxim,
    #                                     comm = exp@commodity,
    #                                     lev = character(0)
    #                                     # lev = exp@timeframe
    # )
    # exp <- .disaggregateSliceLevel(exp, approxim)
    # mExpSlice <- data.table(expp = rep(exp@name, length(approxim$slice)), slice = approxim$slice)
    # obj@parameters[["mExpSlice"]] <- .dat2par(obj@parameters[["mExpSlice"]], mExpSlice)
    # mExpComm <- data.table(expp = exp@name, comm = exp@commodity)
    # obj@parameters[["mExpComm"]] <- .dat2par(obj@parameters[["mExpComm"]], mExpComm)


    ## pExportRowPrice ####
    # scen@modInp@parameters$pExportRowPrice@data
    dat <- data.table(
      expp = exp@name,
      # comm = exp@commodity,
      region = exp@exp$region,
      year = exp@exp$year,
      slice = exp@exp$slice,
      value = as.numeric(exp@exp$price)
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pExportRowPrice", dat)

    ## pExportRowRes ####
    # scen@modInp@parameters$pExportRowRes@data
    # pExportRowRes <- NULL
    # if (exp@reserve != Inf) pExportRowRes <- data.table(expp = exp@name, value = exp@reserve)
    # obj@parameters[["pExportRowRes"]] <- .dat2par(obj@parameters[["pExportRowRes"]], pExportRowRes)
    dat <- data.table(
      expp = exp@name,
      value = exp@reserve
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pExportRowRes", dat)

    ## pExportRow ####
    # scen@modInp@parameters$pExportRow@data
    # pExportRow <- .interp_bounds(exp@exp, "exp", obj@parameters[["pExportRow"]], approxim, "expp", exp@name)
    # obj@parameters[["pExportRow"]] <- .dat2par(obj@parameters[["pExportRow"]], pExportRow)
    dat <- data.table(
      expp = exp@name,
      region = exp@exp$region,
      year = exp@exp$year,
      slice = exp@exp$slice,
      type = exp@exp$type,
      value = as.numeric(exp@exp$value)
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pExportRow", dat)


    # mExportRow <- merge0(merge0(mExpSlice, list(region = approxim$region)), list(year = approxim$mileStoneYears))
    # if (!is.null(pExportRow) && nrow(pExportRow) != 0) {
    #   pExportRow2 <- pExportRow |>
    #     filter(type == "up" & value == 0) |>
    #     select(any_of(colnames(mExportRow)))
    #   # pExportRow2 <- pExportRow[pExportRow$type == "up" & pExportRow$value == 0,
    #   #                           colnames(pExportRow) %in% colnames(mExportRow),
    #   #                           drop = FALSE]
    #   if (nrow(pExportRow2) != 0) {
    #     # pExportRow2 <- mExportRow[1, 1:2, drop = FALSE]
    #     if (ncol(pExportRow2) != ncol(mExportRow)) pExportRow2 <- merge0(mExportRow, pExportRow2)
    #     mExportRow <- mExportRow[(!duplicated(rbind(mExportRow, pExportRow2), fromLast = TRUE)[1:nrow(mExportRow)]), , drop = FALSE]
    #   }
    # }
    # mExportRow$comm <- exp@commodity
    # obj@parameters[["mExportRow"]] <- .dat2par(obj@parameters[["mExportRow"]], mExportRow)
    # if (!is.null(pExportRow) && any(pExportRow$type == "up" & pExportRow$value != Inf & pExportRow$value != 0)) {
    #   mExportRowUp <- pExportRow |>
    #     filter(type == "up" & value != Inf & value != 0) |>
    #     select(any_of(obj@parameters[["mExportRowUp"]]@dimSets))
    #   # mExportRowUp <- pExportRow[
    #   #   pExportRow$type == "up" & pExportRow$value != Inf & pExportRow$value != 0,
    #   #   colnames(pExportRow) %in% obj@parameters[["mExportRowUp"]]@dimSets,
    #   #   drop = FALSE]
    #   mExportRowUp$comm <- exp@commodity
    #   if (!all(obj@parameters[["mExportRowUp"]]@dimSets %in% mExportRowUp)) {
    #     mExportRowUp <- merge0(mExportRow, mExportRowUp)
    #   }
    #   obj@parameters[["mExportRowUp"]] <-
    #     .dat2par(obj@parameters[["mExportRowUp"]], mExportRowUp)
    #   meqExportRowLo <- pExportRow |>
    #     filter(type == "lo" & value != 0) |>
    #     select(any_of(obj@parameters[["meqExportRowLo"]]@dimSets))
    #   # pExportRow[pExportRow$type == "lo" & pExportRow$value != 0,
    #   #            colnames(pExportRow) %in% obj@parameters[["meqExportRowLo"]]@dimSets,
    #   #            drop = FALSE]
    #   meqExportRowLo$comm <- exp@commodity
    #   if (!all(obj@parameters[["meqExportRowLo"]]@dimSets %in% meqExportRowLo)) {
    #     meqExportRowLo <- merge0(mExportRow, meqExportRowLo)
    #   }
    #   obj@parameters[["meqExportRowLo"]] <- .dat2par(
    #     obj@parameters[["meqExportRowLo"]],
    #     merge0(mExportRow, meqExportRowLo)
    #   )
    # }
    # if (!is.null(pExportRowRes)) {
    #   pExportRowRes$comm <- exp@commodity
    #   obj@parameters[["mExportRowCumUp"]] <- .dat2par(
    #     obj@parameters[["mExportRowCumUp"]],
    #     pExportRowRes[pExportRowRes$value != Inf, c("expp", "comm"), drop = FALSE]
    #   )
    # }
    scen
  }
)

# =============================================================================#
## import ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "import", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    # .checkSliceLevel(app, approxim)
    # imp <- .upper_case(app)
    imp <- obj
    imp@name <- toString(imp@name)
    # if (length(imp@commodity) != 1 || is.na(imp@commodity) || all(imp@commodity != approxim$all_comm)) {
    #   stop(paste0('Wrong commodity in import "', imp@name, '"'))
    # }
    if (length(imp@commodity) != 1) {
      stop(
        "Import must have exactly one commodity. ", imp@name, " has ",
        length(imp@commodity), " commodities: ",
        paste(imp@commodity, collapse = ", ")
      )
    }
    if (is.na(imp@commodity)) {
      stop("Import commodity in ", imp@name, " is NA.")
    }
    if (!(imp@commodity %in% scen@modInp@sets$comm)) {
      stop(
        'Commodity "', imp@commodity, '" used in import "', imp@name,
        '" is not declared in the model.'
      )
    }

    # imp <- .filter_data_in_slots(imp, approxim$region, "region")
    # browser()
    # approxim <- .fix_approximation_list(approxim,
    #                                     comm = imp@commodity,
    #                                     lev = character(0)
    #                                     # lev = imp@timeframe
    # )
    # imp <- .disaggregateSliceLevel(imp, approxim)
    # mImpSlice <- data.table(
    #   imp = rep(imp@name, length(approxim$slice)),
    #   slice = approxim$slice)
    # obj@parameters[["mImpSlice"]] <-
    #   .dat2par(obj@parameters[["mImpSlice"]], mImpSlice)
    # mImpComm <- data.table(imp = imp@name, comm = imp@commodity)
    # obj@parameters[["mImpComm"]] <- .dat2par(obj@parameters[["mImpComm"]], mImpComm)

    # pImportRowPrice <- .interp_numpar(
    #   imp@imp, "price",
    #   obj@parameters[["pImportRowPrice"]], approxim, "imp", imp@name
    # )
    # obj@parameters[["pImportRowPrice"]] <- .dat2par(obj@parameters[["pImportRowPrice"]], pImportRowPrice)

    ## pImportRowPrice ####
    # scen@modInp@parameters$pImportRowPrice@data
    dat <- data.table(
      imp = imp@name,
      region = imp@imp$region,
      year = imp@imp$year,
      slice = imp@imp$slice,
      value = as.numeric(imp@imp$price)
    ) |>
      .force_year_class_df()
    scen <- update_parameter(scen, "pImportRowPrice", dat)

    ## pImportRowRes ####
    # scen@modInp@parameters$pImportRowRes@data
    # pImportRowRes <- NULL
    # if (imp@reserve != Inf) pImportRowRes <- data.table(imp = imp@name, value = imp@reserve)
    # obj@parameters[["pImportRowRes"]] <- .dat2par(obj@parameters[["pImportRowRes"]], pImportRowRes)
    dat <- data.table(
      imp = imp@name,
      value = imp@reserve
    )
    scen <- update_parameter(scen, "pImportRowRes", dat)

    ## pImportRow ####
    # scen@modInp@parameters$pImportRow@data
    # pImportRow <- .interp_bounds(
    #   imp@imp, "imp",
    #   obj@parameters[["pImportRow"]], approxim, "imp", imp@name
    # )
    # obj@parameters[["pImportRow"]] <- .dat2par(obj@parameters[["pImportRow"]], pImportRow)
    dat <- data.table(
      imp = imp@name,
      region = imp@imp$region,
      year = imp@imp$year,
      slice = imp@imp$slice,
      type = imp@imp$type,
      value = as.numeric(imp@imp$value)
    )
    scen <- update_parameter(scen, "pImportRow", dat)

    # mImportRow <- merge0(merge0(mImpSlice, list(region = approxim$region)), list(year = approxim$mileStoneYears))
    # if (!is.null(pImportRow) && nrow(pImportRow) != 0) {
    #   pImportRow2 <- pImportRow |>
    #     filter(type == "up" & value == 0) |>
    #     select(any_of(colnames(mImportRow)))
    #   # pImportRow[pImportRow$type == "up" & pImportRow$value == 0,
    #   #            colnames(pImportRow) %in% colnames(mImportRow), drop = FALSE]
    #   if (nrow(pImportRow2) != 0) {
    #     pImportRow2 <- mImportRow[1, 1:2, drop = FALSE]
    #     if (ncol(pImportRow2) != ncol(mImportRow)) pImportRow2 <- merge0(mImportRow, pImportRow2)
    #     mImportRow <- mImportRow[(!duplicated(rbind(mImportRow, pImportRow2), fromLast = TRUE)[1:nrow(mImportRow)]), , drop = FALSE]
    #   }
    # }
    # mImportRow$comm <- imp@commodity
    # obj@parameters[["mImportRow"]] <- .dat2par(obj@parameters[["mImportRow"]], mImportRow)


    # if (!is.null(pImportRow)) {
    #   mImportRowUp <- pImportRow |>
    #     filter(type == "up" & value != Inf & value != 0) |>
    #     select(any_of(obj@parameters[["mImportRowUp"]]@dimSets))
    #   # pImportRow[
    #   #   pImportRow$type == "up" & pImportRow$value != Inf & pImportRow$value != 0,
    #   #   colnames(pImportRow) %in% obj@parameters[["mImportRowUp"]]@dimSets,
    #   #   drop = FALSE]
    #   mImportRowUp$comm <- imp@commodity
    #   if (!all(obj@parameters[["mImportRowUp"]]@dimSets %in% mImportRowUp)) {
    #     mImportRowUp <- merge0(mImportRow, mImportRowUp)
    #   }
    #   obj@parameters[["mImportRowUp"]] <- .dat2par(obj@parameters[["mImportRowUp"]], mImportRowUp)
    #   meqImportRowLo <- pImportRow |>
    #     filter(type == "lo" & value != 0) |>
    #     select(any_of(obj@parameters[["meqImportRowLo"]]@dimSets))
    #   # meqImportRowLo <- pImportRow[
    #   #   pImportRow$type == "lo" & pImportRow$value != 0,
    #   #   colnames(pImportRow) %in% obj@parameters[["meqImportRowLo"]]@dimSets,
    #   #   drop = FALSE]
    #   meqImportRowLo$comm <- imp@commodity
    #   if (!all(obj@parameters[["meqImportRowLo"]]@dimSets %in% meqImportRowLo)) {
    #     meqImportRowLo <- merge0(mImportRow, meqImportRowLo)
    #   }
    #   obj@parameters[["meqImportRowLo"]] <- .dat2par(
    #     obj@parameters[["meqImportRowLo"]],
    #     merge0(mImportRow, meqImportRowLo)
    #   )
    # }
    # if (!is.null(pImportRowRes)) {
    #   pImportRowRes$comm <- exp@commodity
    #   obj@parameters[["mImportRowCumUp"]] <- .dat2par(
    #     obj@parameters[["mImportRowCumUp"]],
    #     pImportRowRes[pImportRowRes$value != Inf, c("expp", "comm"),
    #                   drop = FALSE]
    #   )
    # }
    scen
  }
)

# =============================================================================#
## supply ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "supply", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    sup <- obj
    sup@name <- toString(sup@name)
    if (length(sup@commodity) != 1) {
      stop(
        "Supply must have exactly one commodity. ", sup@name, " has ",
        length(sup@commodity), " commodities: ",
        paste(sup@commodity, collapse = ", ")
      )
    }
    if (is.na(sup@commodity)) {
      stop("Supply commodity in ", sup@name, " is NA.")
    }
    if (!(sup@commodity %in% scen@modInp@sets$comm)) {
      stop(
        'Commodity "', sup@commodity, '" used in supply "', sup@name,
        '" is not declared in the model.'
      )
    }

    # browser()
    # approxim <- .fix_approximation_list(approxim, comm = sup@commodity,
    # lev = sup@timeframe) # dropped
    # approxim <- .fix_approximation_list(approxim, comm = sup@commodity)
    # sup <- .disaggregateSliceLevel(sup, approxim)
    # if (length(sup@region) != 0) {
    #   approxim$region <- approxim$region[approxim$region %in% sup@region]
    #   ss <- getSlots("supply")
    #   ss <- names(ss)[ss %in% "data.frame"]
    #   ss <- ss[sapply(ss, function(x) {
    #     (any(colnames(slot(sup, x)) == "region") &&
    #       any(!is.na(slot(sup, x)$region)))
    #   })]
    #   for (sl in ss) {
    #     if (any(!is.na(slot(sup, sl)$region) &
    #       !(slot(sup, sl)$region %in% sup@region))) {
    #       rr <- !is.na(slot(sup, sl)$region) &
    #         !(slot(sup, sl)$region %in% sup@region)
    #       warning(
    #         paste('There are data supply "', sup@name, '" for unused region: "',
    #           paste(unique(slot(sup, sl)$region[rr]), collapse = '", "'), '"',
    #           sep = ""
    #         )
    #       )
    #       slot(sup, sl) <- slot(sup, sl)[!rr, , drop = FALSE]
    #     }
    #   }
    #   mSupSpan <- data.table(
    #     sup = rep(sup@name, length(sup@region)),
    #     region = sup@region
    #   )
    #   obj@parameters[["mSupSpan"]] <- .dat2par(
    #     obj@parameters[["mSupSpan"]],
    #     mSupSpan
    #   )
    # } else {
    #   mSupSpan <- data.table(
    #     sup = rep(sup@name, length(approxim$region)),
    #     region = approxim$region
    #   )
    #   obj@parameters[["mSupSpan"]] <- .dat2par(
    #     obj@parameters[["mSupSpan"]],
    #     mSupSpan
    #   )
    # }
    # sup <- .filter_data_in_slots(sup, approxim$region, "region")
    # mSupSlice <- data.table(
    #   sup = rep(sup@name, length(approxim$slice)),
    #   slice = approxim$slice
    # )
    # obj@parameters[["mSupSlice"]] <-
    #   .dat2par(obj@parameters[["mSupSlice"]], mSupSlice)
    # browser()
    # mSupComm <- data.table(sup = sup@name, comm = sup@commodity)
    # obj@parameters[["mSupComm"]] <-
    #   .dat2par(obj@parameters[["mSupComm"]], mSupComm)
    # browser()

    ## pSupCost ####
    # scen@modInp@parameters$pSupCost@data
    # pSupCost <- .interp_numpar(
    #   sup@availability, "cost",
    #   obj@parameters[["pSupCost"]],
    #   approxim, c("sup", "comm"),
    #   c(sup@name, sup@commodity)
    # )
    # obj@parameters[["pSupCost"]] <- .dat2par(
    #   obj@parameters[["pSupCost"]],
    #   pSupCost
    # )
    # browser()
    dat <- data.table(
      sup = sup@name,
      comm = sup@commodity,
      region = sup@availability$region,
      year = sup@availability$year,
      slice = sup@availability$slice,
      value = as.numeric(sup@availability$cost)
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pSupCost", dat)

    ## pSupReserve ####
    # scen@modInp@parameters$pSupReserve@data
    # pSupReserve <- .interp_bounds(
    #   sup@reserve, "res", obj@parameters[["pSupReserve"]],
    #   approxim, c("sup", "comm"), c(sup@name, sup@commodity)
    # )
    # obj@parameters[["pSupReserve"]] <-
    #   .dat2par(obj@parameters[["pSupReserve"]], pSupReserve)
    # browser()
    dat <- data.table(
      sup = sup@name,
      comm = sup@commodity,
      region = sup@reserve$region,
      year = sup@reserve$year,
      type = sup@reserve$type,
      value = as.numeric(sup@reserve$value)
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pSupReserve", dat)

    ## pSupAva ####
    # scen@modInp@parameters$pSupAva@data
    # pSupAva <- .interp_bounds(
    #   sup@availability, "ava",
    #   obj@parameters[["pSupAva"]], approxim, c("sup", "comm"),
    #   c(sup@name, sup@commodity)
    # )
    # obj@parameters[["pSupAva"]] <- .dat2par(obj@parameters[["pSupAva"]], pSupAva)
    dat <- data.table(
      sup = sup@name,
      comm = sup@commodity,
      region = sup@availability$region,
      year = sup@availability$year,
      slice = sup@availability$slice,
      type = sup@availability$type,
      value = as.numeric(sup@availability$ava)
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pSupAva", dat)



    # zero_ava_up <- pSupAva[pSupAva$value == 0 & pSupAva$type == "up",
    #                        colnames(pSupAva) != "value", drop = FALSE]
    # browser()
    # if (is.null(pSupAva)) {
    #   zero_ava_up <- NULL
    # } else {
    #   zero_ava_up <- pSupAva |>
    #     filter(value == 0, type == "up") |>
    #     select(-any_of("value"))
    #   # browser()
    # }
    # # mSupAva <- merge0(merge0(mSupSpan, list(comm = sup@commodity, year = approxim$mileStoneYears)), mSupSlice)
    # mSupAva <- mSupSpan |>
    #   merge0(list(comm = sup@commodity, year = approxim$mileStoneYears)) |>
    #   merge0(mSupSlice)

    # if (!is.null(zero_ava_up) && nrow(zero_ava_up) != 0) {
    #   if (all(colnames(mSupAva) %in% colnames(zero_ava_up))) {
    #     # mSupAva <-
    #     #   mSupAva[(!duplicated(
    #     #     rbind(mSupAva, zero_ava_up[, colnames(mSupAva)]),
    #     #     fromLast = TRUE))[1:nrow(mSupAva)], ]
    #     ii <- mSupAva |>
    #       rbind(select(zero_ava_up, all_of(colnames(mSupAva)))) |>
    #       duplicated(fromLast = TRUE)
    #     # filter(n() <= nrow(mSupAva))
    #     ii <- ii[1:nrow(mSupAva)]
    #     mSupAva <- mSupAva[!ii, ]
    #   } else {
    #     # mSupAva <- mSupAva[(!duplicated(rbind(mSupAva, merge0(mSupAva, zero_ava_up[, colnames(zero_ava_up) %in% colnames(mSupAva), drop = FALSE])[, colnames(mSupAva)]), fromLast = TRUE))[1:nrow(mSupAva)], ]
    #     ii <- mSupAva |>
    #       rbind(
    #         merge0(mSupAva, select(zero_ava_up, any_of(colnames(mSupAva))))
    #       ) |>
    #       select(all_of(colnames(mSupAva))) |>
    #       duplicated(fromLast = TRUE)
    #     ii <- ii[1:nrow(mSupAva)] # ???
    #     mSupAva <- mSupAva[!ii, ]
    #   }
    # }
    # obj@parameters[["mSupAva"]] <- .dat2par(obj@parameters[["mSupAva"]], mSupAva)
    # mvSupReserve <- merge0(mSupComm, mSupSpan)
    # obj@parameters[["mvSupReserve"]] <-
    #   .dat2par(obj@parameters[["mvSupReserve"]], mvSupReserve)
    # if (all(c("sup", "comm", "region") %in% colnames(pSupReserve))) {
    #   obj@parameters[["mSupReserveUp"]] <-
    #     .dat2par(
    #       obj@parameters[["mSupReserveUp"]],
    #       pSupReserve[
    #         pSupReserve$type == "up" & pSupReserve$value != Inf,
    #         c("sup", "comm", "region")
    #       ]
    #     )
    #   obj@parameters[["meqSupReserveLo"]] <-
    #     .dat2par(
    #       obj@parameters[["meqSupReserveLo"]],
    #       pSupReserve[
    #         pSupReserve$type == "lo" & pSupReserve$value != 0,
    #         c("sup", "comm", "region")
    #       ]
    #     )
    # } else {
    #   # obj@parameters[["mSupReserveUp"]] <- .dat2par(
    #   #   obj@parameters[["mSupReserveUp"]],
    #   #   merge0(mvSupReserve, pSupReserve[pSupReserve$type == "up" & pSupReserve$value != Inf,
    #   #     colnames(pSupReserve) %in% c("sup", "comm", "region"),
    #   #     drop = FALSE
    #   #   ])
    #   # )
    #   .null_to_empty_param("pSupReserve", obj@parameters)
    #   # browser()
    #   obj@parameters[["mSupReserveUp"]] <-
    #     .dat2par(
    #       obj@parameters[["mSupReserveUp"]],
    #       merge0(
    #         mvSupReserve,
    #         select(
    #           filter(pSupReserve, type == "up" & value != Inf),
    #           any_of(c("sup", "comm", "region"))
    #         )
    #       )
    #     )
    #   # obj@parameters[["meqSupReserveLo"]] <- .dat2par(
    #   #   obj@parameters[["meqSupReserveLo"]],
    #   #   merge0(mvSupReserve, pSupReserve[pSupReserve$type == "lo" & pSupReserve$value != 0,
    #   #     colnames(pSupReserve) %in% c("sup", "comm", "region"),
    #   #     drop = FALSE
    #   #   ])
    #   # )
    #   obj@parameters[["meqSupReserveLo"]] <-
    #     .dat2par(
    #       obj@parameters[["meqSupReserveLo"]],
    #       merge0(
    #         mvSupReserve,
    #         select(
    #           filter(pSupReserve, type == "lo" & value != 0),
    #           any_of(c("sup", "comm", "region"))
    #         )
    #       )
    #     )
    # }
    # .null_to_empty_param("pSupAva", obj@parameters)
    # obj@parameters[["meqSupAvaLo"]] <- .dat2par(
    #   obj@parameters[["meqSupAvaLo"]],
    #   # merge0(mSupAva, pSupAva[pSupAva$type == "lo" & pSupAva$value != 0, colnames(pSupAva) %in% colnames(mSupAva)])
    #   merge0(
    #     mSupAva,
    #     select(
    #       filter(pSupAva, type == "lo" & value != 0),
    #       all_of(colnames(mSupAva))
    #     )
    #   )
    # )
    # # .null_to_empty_param("pSupAva", obj@parameters)
    # # browser()
    # obj@parameters[["mSupAvaUp"]] <- .dat2par(
    #   obj@parameters[["mSupAvaUp"]],
    #   # merge0(mSupAva, pSupAva[pSupAva$type == "up" & pSupAva$value != Inf,
    #   # colnames(pSupAva) %in% colnames(mSupAva)])
    #   merge0(
    #     mSupAva,
    #     select(
    #       filter(pSupAva, type == "up" & value != Inf),
    #       all_of(colnames(mSupAva))
    #     )
    #   )
    # )
    # For weather
    # browser()

    ## pSupReserve ####
    # scen@modInp@parameters$pSupReserve@data
    dat <- data.table(
      sup = sup@name,
      comm = sup@commodity,
      region = sup@reserve$region,
      type = sup@reserve$type,
      value = as.numeric(sup@reserve$value)
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pSupReserve", dat)


    # if (nrow(sup@weather) > 0) {
    #   tmp <- .toWeatherImply(sup@weather, "wava", "sup", sup@name)
    #   obj@parameters[["pSupWeather"]] <-
    #     .dat2par(obj@parameters[["pSupWeather"]], tmp$par)
    #   obj@parameters[["mSupWeatherUp"]] <-
    #     .dat2par(obj@parameters[["mSupWeatherUp"]], tmp$mapup)
    #   obj@parameters[["mSupWeatherLo"]] <-
    #     .dat2par(obj@parameters[["mSupWeatherLo"]], tmp$maplo)
    # }

    ## pSupWeather ####
    # scen@modInp@parameters$pSupWeather@data
    dat <- data.table(
      weather = sup@weather$weather,
      sup = sup@name,
      type = sup@weather$type,
      value = as.numeric(sup@weather$value)
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pSupWeather", dat)

    # t1 <- mSupAva[, c("sup", "region", "year")] |> unique()
    # t1 <- t1[!duplicated(t1), ]
    # t2 <- pSupCost[pSupCost$value != 0, colnames(pSupCost)[colnames(pSupCost) %in% c("sup", "region", "year")], drop = FALSE]
    # t2 <- t2[!duplicated(t2), , drop = FALSE]
    # browser()
    # .null_to_empty_param("pSupCost", obj@parameters)
    # t2 <- pSupCost |>
    #   filter(value != 0) |>
    #   select(any_of(c("sup", "region", "year"))) |>
    #   unique()
    # if (!is.null(t2) && ncol(t2) != 3) {
    #   browser()
    #   # t2 <- merge0(t2, mSupAva[!duplicated(mSupAva[, c("sup", "region", "year")]),
    #   #                          c("sup", "region", "year")])
    #   t2 <- merge0(t2, unique(mSupAva[, c("sup", "region", "year")]))
    # }
    # mvSupCost <- merge0(t1, t2)
    # mvSupCost <- mvSupCost[!duplicated(mvSupCost), ]
    # obj@parameters[["mvSupCost"]] <- .dat2par(obj@parameters[["mvSupCost"]], mvSupCost)
    scen
  }
)

# =============================================================================#
## weather ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "weather", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
  # browser()
  wth <- obj
  wth@name <- toString(wth@name)
  # if (length(wth@timeframe) == 0 && length(approxim$calendar@slices_in_frame) > 1) {
  #   stop("Slot weather@timeframe is empty, it should have information about slice level")
  # }
  if (length(wth@timeframe) == 0) {
    stop("Weather object must have a timeframe.")
  }
  #
  # if (length(wth@timeframe) == 0) {
  #   wth@timeframe <- names(approxim$calendar@slices_in_frame)[1]
  # }
  # approxim <- .fix_approximation_list(approxim, lev = wth@timeframe)
  # # region fix
  # if (length(wth@region) != 0) {
  #   approxim$region <- approxim$region[approxim$region %in% wth@region]
  # }
  # wth@region <- approxim$region
  # browser()
  # wth <- .filter_data_in_slots(wth, approxim$region, "region")
  # wth <- .disaggregateSliceLevel(wth, approxim)

  ## pWeather ####
  # scen@modInp@parameters$pWeather@data
  dat <- data.table(
    weather = wth@name,
    region = wth@weather$region,
    year = wth@weather$year,
    slice = wth@weather$slice,
    value = as.numeric(wth@weather$wval)
  ) |>
    force_cols_classes()
  scen <- update_parameter(scen, "pWeather", dat)

  # obj@parameters[["pWeather"]]@defVal <- wth@defVal
  # obj@parameters[["pWeather"]] <- .dat2par(obj@parameters[["pWeather"]], .interp_numpar(
  #   wth@weather, "wval",
  #   obj@parameters[["pWeather"]], approxim, "weather", wth@name
  # ))
  # obj@parameters[["mWeatherSlice"]] <- .dat2par(
  #   obj@parameters[["mWeatherSlice"]],
  #   data.table(weather = rep(wth@name, length(approxim$slice)), slice = approxim$slice)
  # )
  # obj@parameters[["mWeatherRegion"]] <- .dat2par(
  #   obj@parameters[["mWeatherRegion"]],
  #   data.table(weather = rep(wth@name, length(wth@region)), region = wth@region)
  # )
  scen
})

# =============================================================================#
## technology ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "technology", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    # browser()
    # .checkSliceLevel(app, approxim)
    # tech <- .upper_case(app)
    tech <- obj
    # if (length(tech@timeframe) == 0) {
    #   use_cmd <- unique(
    #     sapply(c(tech@output$comm, tech@output$comm, tech@aux$acomm),
    #            function(x) approxim$commodity_slice_map[x])
    #   )
    #   tech@timeframe <- colnames(approxim$calendar@timetable)[
    #     max(c(approxim$calendar@timeframe_rank[c(use_cmd, recursive = TRUE)],
    #           recursive = TRUE))
    #   ]
    # }

    ## pTechCap2act ####
    # scen@modInp@parameters$pTechCap2act@data
    browser()

    dat <- data.table(
      tech = tech@name,
      value = get_lazy_data(tech, "cap2act", default = NA_real_)
    ) |> force_cols_classes()
    scen <- update_parameter(scen, "pTechCap2act", dat)

    make_data_param(
      scen = scen,
      slot_data = dat,
      par_name = "pTechCap2act",
      short_name = "cap2act",
      class_col = "tech"
      # data = dat,
      # dim_sets = c("tech"),
      # value_col = "value"
    )

    # @capacity ####
    sl <- get_lazy_data(tech, "capacity")

    ## pTechStock ####
    # scen@modInp@parameters$pTechStock@data
    x <- sl |>
      select(any_of(c(scen@modInp@parameters$pTechStock@dimSets, "stock"))) |>
      filter(!is.na(stock)) |>
      unique()

    dat <- data.table(
      tech = tech@name,
      region = x$region,
      year = x$year,
      slice = x$slice,
      value = as.numeric(x$stock)
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pTechStock", dat)

    ## pTechCap ####
    # !!! ToDo: finish

    ## pTechNewCap ####

    ## pTechRet ####

    # @ceff ####
    sl <- get_lazy_data(tech, "ceff")

    ## pTechCinp2use ####
    # scen@modInp@parameters$pTechCinp2use@data
    x <- sl |>
      select(any_of(c(scen@modInp@parameters$pTechCinp2use@dimSets, "cinp2use"))) |>
      filter(!is.na(cinp2use)) |>
      unique()
    dat <- data.table(
      tech = tech@name,
      comm = x$comm,
      region = x$region,
      year = x$year,
      slice = x$slice,
      value = as.numeric(x$cinp2use)
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pTechCinp2use", dat)


    ## pTechUse2cact ####
    # scen@modInp@parameters$pTechUse2cact@data
    x <- sl |>
      select(any_of(c(scen@modInp@parameters$pTechUse2cact@dimSets, "use2cact"))) |>
      filter(!is.na(use2cact)) |>
      unique()
    dat <- data.table(
      tech = tech@name,
      comm = x$comm,
      region = x$region,
      year = x$year,
      slice = x$slice,
      value = as.numeric(x$use2cact)
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pTechUse2cact", dat)

    ## pTechCact2cout ####
    # scen@modInp@parameters$pTechCact2cout@data
    x <- sl |>
      select(any_of(c(scen@modInp@parameters$pTechCact2cout@dimSets, "cact2cout"))) |>
      filter(!is.na(cact2cout)) |>
      unique()
    dat <- data.table(
      tech = tech@name,
      comm = x$comm,
      region = x$region,
      year = x$year,
      slice = x$slice,
      value = as.numeric(x$cact2cout)
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pTechCact2cout", dat)

    ## pTechCinp2ginp ####
    # scen@modInp@parameters$pTechCinp2ginp@data
    x <- sl |>
      select(any_of(c(scen@modInp@parameters$pTechCinp2ginp@dimSets, "cinp2ginp"))) |>
      filter(!is.na(cinp2ginp)) |>
      unique()
    dat <- data.table(
      tech = tech@name,
      comm = x$comm,
      region = x$region,
      year = x$year,
      slice = x$slice,
      value = as.numeric(x$cinp2ginp)
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pTechCinp2ginp", dat)

    ## pTechShare ####
    # scen@modInp@parameters$pTechShare@data
    # !!! ToDo: finish

    ## pTechAfc ####
    # scen@modInp@parameters$pTechAfc@data
    # !!! ToDo: finish

    ## @geff ####
    sl <- get_lazy_data(tech, "geff")

    ## pTechGinp2use ####
    # scen@modInp@parameters$pTechGinp2use@data
    x <- sl |>
      select(any_of(c(scen@modInp@parameters$pTechGinp2use@dimSets, "ginp2use"))) |>
      filter(!is.na(ginp2use)) |>
      unique()
    dat <- data.table(
      tech = tech@name,
      group = x$group,
      region = x$region,
      year = x$year,
      slice = x$slice,
      value = as.numeric(x$ginp2use)
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "pTechGinp2use", dat)

    ## @aeff ####
    sl <- get_lazy_data(tech, "aeff")

    ## pTechCinp2AInp ####
    # scen@modInp@parameters$pTechCinp2AInp@data
    # x <- sl |>
    #   select(any_of(c(scen@modInp@parameters$pTechCinp2AInp@dimSets, "cinp2ainp"))) |>
    #   filter(!is.na(cinp2ainp)) |>
    #   unique()
    # dat <- data.table(
    #   tech = tech@name,
    #   acomm = x$acomm,
    #   comm = x$comm,
    #   region = x$region,
    #   year = x$year,
    #   slice = x$slice,
    #   value = as.numeric(x$cinp2ainp)
    # ) |>
    #   force_cols_classes()

    browser()

    dat <-  make_data_param(
      scen, slot_data = sl, obj_name = tech@name,
      par_name = "pTechCinp2AInp", short_name = "cinp2ainp",
      class_col = "tech")
    scen <- update_parameter(scen, "pTechCinp2AInp", dat)

    ## pTechCinp2AInp ####
    dat <-  make_data_param(
      scen, slot_data = sl, obj_name = tech@name,
      par_name = "pTechCinp2AInp", short_name = "cinp2ainp",
      class_col = "tech")
    scen <- update_parameter(scen, "pTechCinp2AInp", dat)

    ## pTechCout2AInp ####
    # scen@modInp@parameters$pTechCout2AInp@data
    dat <-  make_data_param(
      scen, slot_data = sl, obj_name = tech@name,
      par_name = "pTechCout2AInp", short_name = "cout2ainp",
      class_col = "tech")
    scen <- update_parameter(scen, "pTechCout2AInp", dat)

    ## pTechCout2AOut ####
    # scen@modInp@parameters$pTechCout2AOut@data
    dat <- make_data_param(
      scen, slot_data = sl, obj_name = tech@name,
      par_name = "pTechCout2AOut", short_name = "cout2aout",
      class_col = "tech")
    scen <- update_parameter(scen, "pTechCout2AOut", dat)

    ## pTechAct2AInp ####
    # scen@modInp@parameters$pTechAct2AInp@data
    dat <- make_data_param(
      scen, slot_data = sl, obj_name = tech@name,
      par_name = "pTechAct2AInp", short_name = "act2ainp",
      class_col = "tech")
    scen <- update_parameter(scen, "pTechAct2AInp", dat)

    ## pTechAct2AOut ####
    # scen@modInp@parameters$pTechAct2AOut@data
    dat <- make_data_param(
      scen, slot_data = sl, obj_name = tech@name,
      par_name = "pTechAct2AOut", short_name = "act2aout",
      class_col = "tech")
    scen <- update_parameter(scen, "pTechAct2AOut", dat)

    ## pTechCap2AInp ####
    # scen@modInp@parameters$pTechCap2AInp@data
    dat <- make_data_param(
      scen, slot_data = sl, obj_name = tech@name,
      par_name = "pTechCap2AInp", short_name = "cap2ainp",
      class_col = "tech")
    scen <- update_parameter(scen, "pTechCap2AInp", dat)

    ## pTechCap2AOut ####
    # scen@modInp@parameters$pTechCap2AOut@data
    dat <- make_data_param(
      scen, slot_data = sl, obj_name = tech@name,
      par_name = "pTechCap2AOut", short_name = "cap2aout",
      class_col = "tech")
    scen <- update_parameter(scen, "pTechCap2AOut", dat)

    ## pTechNCap2AInp ####
    # scen@modInp@parameters$pTechNCap2AInp@data
    dat <- make_data_param(
      scen, slot_data = sl, obj_name = tech@name,
      par_name = "pTechNCap2AInp", short_name = "ncap2ainp",
      class_col = "tech")
    scen <- update_parameter(scen, "pTechNCap2AInp", dat)

    ## pTechNCap2AOut ####
    # scen@modInp@parameters$pTechNCap2AOut@data
    dat <- make_data_param(
      scen, slot_data = sl, obj_name = tech@name,
      par_name = "pTechNCap2AOut", short_name = "ncap2aout",
      class_col = "tech")
    scen <- update_parameter(scen, "pTechNCap2AOut", dat)

    ## !!! sinp2ainp .... drop? ####


    ## @af ####
    sl <- get_lazy_data(tech, "af")
    # !!! ToDo: finish

    ## @afs ####
    sl <- get_lazy_data(tech, "afs")
    # !!! ToDo: finish

    ## @weather ####
    sl <- get_lazy_data(tech, "weather")
    # !!! ToDo: finish

    ## @start ####
    # scen@modInp@parameters$pTechStart@data

    ## @end ####

    ## @varom ####

    ## @fixom ####

    ## @invcost ####

    ##



    # Disaggregated AFS, if there is a timeframe level
    # if (nrow(tech@afs) != 0 &&
    #     any(tech@afs$slice %in% names(approxim$calendar@timeframes))) {
    #   chk <- seq_len(nrow(tech@afs))[tech@afs$slice %in%
    #                                    names(approxim$calendar@timeframes)]
    #   for (cc in chk) {
    #     slc <- approxim$calendar@timeframes[[tech@afs[cc, "slice"]]]
    #     tmp <- tech@afs[rep(cc, length(slc)), ]
    #     tmp$slice <- slc
    #     tech@afs <- rbind(tech@afs, tmp)
    #   }
    #   tech@afs <- tech@afs[-chk, ]
    # }
    # approxim <- .fix_approximation_list(approxim, lev = tech@timeframe)
    # tech <- .disaggregateSliceLevel(tech, approxim)
    # mTechSlice <- data.table(
    #   tech = rep(tech@name, length(approxim$slice)), slice = approxim$slice,
    #   stringsAsFactors = FALSE
    # )
    # obj@parameters[["mTechSlice"]] <-
    #   .dat2par(obj@parameters[["mTechSlice"]], mTechSlice)
    # if (length(tech@region) != 0) {
    #   approxim$region <- approxim$region[approxim$region %in% tech@region]
    #   ss <- getSlots("technology")
    #   ss <- names(ss)[ss %in% "data.frame"]
    #   ss <- ss[sapply(ss, function(x) {
    #     (any(colnames(slot(tech, x)) == "region") &&
    #        any(!is.na(slot(tech, x)$region)))
    #   })]
    #   for (sl in ss) {
    #     if (any(!is.na(slot(tech, sl)$region) &
    #             !(slot(tech, sl)$region %in% tech@region))) {
    #       rr <- !is.na(slot(tech, sl)$region) &
    #         !(slot(tech, sl)$region %in% tech@region)
    #       warning(
    #         paste('There are data technology "', tech@name,
    #               '"for unused region: "',
    #               paste(unique(slot(tech, sl)$region[rr]), collapse = '", "'),
    #               '"',
    #               sep = ""
    #         )
    #       )
    #       slot(tech, sl) <- slot(tech, sl)[!rr, , drop = FALSE]
    #     }
    #   }
    # }
    # tech <- .filter_data_in_slots(tech, approxim$region, "region")
    # Map
    # ctype <- checkInpOut(tech)
    # # Need choose comm more accuracy
    # approxim_comm <- approxim
    # approxim_comm[["comm"]] <- rownames(ctype$comm)
    # if (length(approxim_comm[["comm"]]) != 0) {
    #   pTechCvarom <- .interp_numpar(tech@varom, "cvarom",
    #                                 obj@parameters[["pTechCvarom"]], approxim_comm, "tech", tech@name
    #                                 # remValue = 0
    #   )
    #   obj@parameters[["pTechCvarom"]] <-
    #     .dat2par(obj@parameters[["pTechCvarom"]], pTechCvarom)
    # } else {
    #   pTechCvarom <- NULL
    # }
    approxim_acomm <- approxim
    approxim_acomm[["acomm"]] <- rownames(ctype$aux)
    if (length(approxim_acomm[["acomm"]]) != 0) {
      pTechAvarom <- .interp_numpar(tech@varom, "avarom",
                                    obj@parameters[["pTechAvarom"]], approxim_acomm, "tech", tech@name
                                    # remValue = 0
      )
      obj@parameters[["pTechAvarom"]] <-
        .dat2par(obj@parameters[["pTechAvarom"]], pTechAvarom)
    } else {
      pTechAvarom <- NULL
    }
    approxim_comm[["comm"]] <- rownames(ctype$comm)
    if (length(approxim_comm[["comm"]]) != 0) {
      pTechAfc <- .interp_bounds(tech@ceff, "afc",
                                 obj@parameters[["pTechAfc"]], approxim_comm, "tech", tech@name,
                                 remValueUp = Inf, remValueLo = 0
      )
      obj@parameters[["pTechAfc"]] <-
        .dat2par(obj@parameters[["pTechAfc"]], pTechAfc)
    } else {
      pTechAfc <- NULL
    }
    # Stock & Capacity
    stock_exist <- .interp_numpar(
      tech@capacity,
      "stock",
      obj@parameters[["pTechStock"]],
      approxim,
      "tech",
      tech@name
    )
    obj@parameters[["pTechStock"]] <-
      .dat2par(obj@parameters[["pTechStock"]], stock_exist)

    if (nrow(tech@capacity) > 0) {
      # browser()
      pTechCap <- .interp_bounds(
        tech@capacity, "cap",
        obj@parameters[["pTechCap"]], approxim, "tech", tech@name,
        remValueUp = Inf, remValueLo = 0
      )
      obj@parameters[["pTechCap"]] <-
        .dat2par(obj@parameters[["pTechCap"]], pTechCap)
      rm(pTechCap)

      pTechNewCap <- .interp_bounds(
        tech@capacity, "ncap",
        obj@parameters[["pTechNewCap"]], approxim, "tech", tech@name,
        remValueUp = Inf, remValueLo = 0
      )
      obj@parameters[["pTechNewCap"]] <-
        .dat2par(obj@parameters[["pTechNewCap"]], pTechNewCap)
      rm(pTechNewCap)

      # browser()
      pTechRet <- .interp_bounds(
        tech@capacity, "ret",
        obj@parameters[["pTechRet"]], approxim, "tech", tech@name,
        remValueUp = Inf, remValueLo = 0
      )
      obj@parameters[["pTechRet"]] <-
        .dat2par(obj@parameters[["pTechRet"]], pTechRet)
      rm(pTechRet)
    }

    olife <- .interp_numpar(
      tech@olife, "olife",
      obj@parameters[["pTechOlife"]], approxim,
      "tech", tech@name
      # , removeDefault = FALSE
    )
    obj@parameters[["pTechOlife"]] <-
      .dat2par(obj@parameters[["pTechOlife"]], olife)
    # browser() # !!!!! check warning msg
    dd0 <- .process_lifespan(approxim, tech, "tech", stock_exist)
    dd0$new <- dd0$new[dd0$new$year %in% approxim$mileStoneYears &
                         dd0$new$region %in% approxim$region, , drop = FALSE]
    dd0$span <- dd0$span[dd0$span$year %in% approxim$mileStoneYears &
                           dd0$span$region %in% approxim$region, , drop = FALSE]
    obj@parameters[["mTechNew"]] <-
      .dat2par(obj@parameters[["mTechNew"]], dd0$new)

    invcost <- .interp_numpar(tech@invcost, "invcost",
                              obj@parameters[["pTechInvcost"]], approxim,
                              "tech", tech@name)

    # !!! temporary fix (adding eac from slots) !!!
    pTechEac <- .interp_numpar(tech@invcost, "eac",
                               obj@parameters[["pTechEac"]],
                               approxim, "tech", tech@name
    )

    if (!is.null(invcost) || !is.null(pTechEac)) {
      # browser()
      minvcost <- merge0(dd0$new, invcost)
      obj@parameters[["mTechInv"]] <-
        .dat2par(obj@parameters[["mTechInv"]],
                 minvcost[, -"value"])
      obj@parameters[["pTechInvcost"]] <-
        .dat2par(obj@parameters[["pTechInvcost"]], invcost)
      obj@parameters[["mTechEac"]] <-
        .dat2par(obj@parameters[["mTechEac"]], dd0$eac)
    }

    # browser()
    retcost <- .interp_numpar(tech@invcost, "retcost",
                              obj@parameters[["pTechRetCost"]], approxim,
                              "tech", tech@name)
    if (!is.null(retcost) ) {
      obj@parameters[["pTechRetCost"]] <-
        .dat2par(obj@parameters[["pTechRetCost"]], retcost)

      mretcost <- retcost |> select(-value) |> unique()
      if (!is_null(mretcost) && approxim$optimizeRetirement) {
        # && scen@settings@optimizeRetirement
        obj@parameters[["mTechRetCost"]] <-
          .dat2par(obj@parameters[["mTechRetCost"]], mretcost)
      }
    }

    # browser()
    obj@parameters[["mTechSpan"]] <-
      .dat2par(obj@parameters[["mTechSpan"]], dd0$span)

    # Calculate/add EAC from invcost !!! needs adjustments
    # pTechEac <- NULL
    if (nrow(dd0$new) > 0 && !is.null(invcost)) {
      # browser()
      salv_data <- merge0(dd0$new, approxim$discount, all.x = TRUE)
      salv_data$value[is.na(salv_data$value)] <- 0
      salv_data$discount <- salv_data$value
      salv_data$value <- NULL
      olife$olife <- olife$value # !!! check multi-region values, add year dim
      olife$value <- NULL
      salv_data <- merge0(salv_data, olife)
      invcost$invcost <- invcost$value
      invcost$value <- NULL
      salv_data <- merge0(salv_data, invcost)
      # EAC
      salv_data$eac <- salv_data$invcost / salv_data$olife
      fl <- (salv_data$discount != 0 & salv_data$olife != Inf)
      salv_data$eac[fl] <- salv_data$invcost[fl] *
        (salv_data$discount[fl] *
           (1 + salv_data$discount[fl])^salv_data$olife[fl] /
           ((1 + salv_data$discount[fl])^salv_data$olife[fl] - 1)
        )
      fl <- (salv_data$discount != 0 & salv_data$olife == Inf)
      salv_data$eac[fl] <- salv_data$invcost[fl] * salv_data$discount[fl]

      salv_data$tech <- tech@name
      salv_data$value <- salv_data$eac
      # browser()
      # pTechEac <- salv_data[, c("tech", "region", "year", "value")]
      pTechEac <- salv_data |> select(all_of(c("tech", "region", "year", "value")))
      # co <- c(obj@parameters[["pTechEac"]]@dimSets, "value")
      # obj@parameters[["pTechEac"]] <-
      # .dat2par(obj@parameters[["pTechEac"]],
      #          unique(select(pTechEac, all_of(co))))
      # unique(pTechEac[, c(obj@parameters[["pTechEac"]]@dimSets, "value")]))
    }

    # (temporary fix) Overwrite pTechEac if eac is provided
    if (nrow(tech@invcost) > 0 && any(!is.na(tech@invcost$eac)) &&
        any(tech@invcost$eac != 0)) {
      pTechEac <- .interp_numpar(
        tech@invcost, "eac", obj@parameters[["pTechEac"]],
        approxim, "tech", tech@name)
    }
    if (!is.null(pTechEac)) {
      obj@parameters[["pTechEac"]] <-
        .dat2par(obj@parameters[["pTechEac"]], pTechEac)
    }
    browser()
    pTechAf <- .interp_bounds(tech@af, "af",
                              obj@parameters[["pTechAf"]], approxim, "tech", tech@name,
                              remValueUp = Inf, remValueLo = 0
    )
    obj@parameters[["pTechAf"]] <-
      .dat2par(obj@parameters[["pTechAf"]], pTechAf)
    if (nrow(tech@afs) > 0) {
      afs_slice <- unique(tech@afs$slice)
      afs_slice <- afs_slice[!is.na(afs_slice)]
      approxim.afs <- approxim
      approxim.afs$slice <- afs_slice
      pTechAfs <- .interp_bounds(
        tech@afs, "afs",
        obj@parameters[["pTechAfs"]],
        approxim.afs, "tech",
        tech@name,
        remValueUp = Inf,
        remValueLo = 0
      )
      obj@parameters[["pTechAfs"]] <-
        .dat2par(obj@parameters[["pTechAfs"]], pTechAfs)
    } else {
      pTechAfs <- NULL
    }

    approxim_comm[["comm"]] <-
      rownames(ctype$comm)[
        ctype$comm$type == "input" & is.na(ctype$comm[, "group"])
      ]
    if (length(approxim_comm[["comm"]]) != 0) {
      pTechCinp2use <- .interp_numpar(
        tech@ceff, "cinp2use",
        obj@parameters[["pTechCinp2use"]], approxim_comm, "tech", tech@name
      )
      obj@parameters[["pTechCinp2use"]] <-
        .dat2par(obj@parameters[["pTechCinp2use"]], pTechCinp2use)
    } else {
      pTechCinp2use <- NULL
    }
    approxim_comm[["comm"]] <- rownames(ctype$comm)[ctype$comm$type == "output"]
    if (length(approxim_comm[["comm"]]) != 0) {
      pTechUse2cact <- .interp_numpar(
        tech@ceff, "use2cact",
        obj@parameters[["pTechUse2cact"]], approxim_comm, "tech", tech@name
      )
      obj@parameters[["pTechUse2cact"]] <-
        .dat2par(obj@parameters[["pTechUse2cact"]], pTechUse2cact)
      pTechCact2cout <- .interp_numpar(
        tech@ceff, "cact2cout",
        obj@parameters[["pTechCact2cout"]], approxim_comm, "tech", tech@name
      )
      obj@parameters[["pTechCact2cout"]] <-
        .dat2par(obj@parameters[["pTechCact2cout"]], pTechCact2cout)
      if (any(!is.na(tech@ceff$cact2cout) & (tech@ceff$cact2cout == 0 | tech@ceff$cact2cout == Inf))) {
        stop("cact2cout is not correct ", tech@name)
      }
      if (any(!is.na(tech@ceff$use2cact) &
              (tech@ceff$use2cact == 0 | tech@ceff$use2cact == Inf))) {
        stop("use2cact is not correct ", tech@name)
      }
    } else {
      pTechUse2cact <- NULL
      pTechCact2cout <- NULL
    }
    approxim_comm[["comm"]] <-
      rownames(ctype$comm)[ctype$comm$type == "input" & !is.na(ctype$comm[, "group"])]
    if (length(approxim_comm[["comm"]]) != 0) {
      pTechCinp2ginp <- .interp_numpar(
        tech@ceff, "cinp2ginp",
        obj@parameters[["pTechCinp2ginp"]], approxim_comm, "tech", tech@name
      )
      obj@parameters[["pTechCinp2ginp"]] <-
        .dat2par(obj@parameters[["pTechCinp2ginp"]], pTechCinp2ginp)
    } else {
      pTechCinp2ginp <- NULL
    }
    if (tech@optimizeRetirement) {
      # browser()
      obj@parameters[["mTechRetirement"]] <-
        .dat2par(obj@parameters[["mTechRetirement"]], data.table(tech = tech@name))
    }
    # if (length(tech@upgrade.technology) != 0) {
    #   obj@parameters[["mTechUpgrade"]] <- .dat2par(
    #     obj@parameters[["mTechUpgrade"]],
    #     data.table(tech = rep(tech@name, length(tech@upgrade.technology)),
    #                techp = tech@upgrade.technology)
    #   )
    # }
    cmm <- rownames(ctype$comm)[ctype$comm$type == "input"]
    if (length(cmm) != 0) {
      mTechInpComm <- data.table(tech = rep(tech@name, length(cmm)), comm = cmm)
      obj@parameters[["mTechInpComm"]] <-
        .dat2par(obj@parameters[["mTechInpComm"]], mTechInpComm)
    } else {
      mTechInpComm <- NULL
    }
    # browser()

    cmm <- rownames(ctype$comm)[ctype$comm$type == "output"]
    if (length(cmm) != 0) {
      mTechOutComm <- data.table(tech = rep(tech@name, length(cmm)), comm = cmm)
      obj@parameters[["mTechOutComm"]] <-
        .dat2par(obj@parameters[["mTechOutComm"]], mTechOutComm)
    } else {
      mTechOutComm <- NULL
    }
    cmm <- rownames(ctype$comm)[is.na(ctype$comm$group)]
    if (length(cmm) != 0) {
      mTechOneComm <- data.table(tech = rep(tech@name, length(cmm)), comm = cmm)
      obj@parameters[["mTechOneComm"]] <-
        .dat2par(obj@parameters[["mTechOneComm"]], mTechOneComm)
    } else {
      mTechOneComm <- NULL
    }
    approxim_comm[["comm"]] <- rownames(ctype$comm)[!is.na(ctype$comm$group)]
    if (length(approxim_comm[["comm"]]) != 0) {
      pTechShare <- .interp_bounds(tech@ceff, "share",
                                   obj@parameters[["pTechShare"]], approxim_comm, "tech", tech@name,
                                   remValueUp = 1, remValueLo = 0
      )
      obj@parameters[["pTechShare"]] <-
        .dat2par(obj@parameters[["pTechShare"]], pTechShare)
    } else {
      pTechShare <- NULL
    }
    cmm <- rownames(ctype$comm)[ctype$comm$comb != 0]
    if (length(cmm) != 0) {
      obj@parameters[["pTechEmisComm"]] <- .dat2par(
        obj@parameters[["pTechEmisComm"]],
        data.table(
          tech = rep(tech@name, nrow(ctype$comm)),
          comm = rownames(ctype$comm),
          value = ctype$comm$comb
        )
      )
    }
    gpp <- rownames(ctype$group)[ctype$group$type == "input"]
    if (length(gpp) != 0) {
      mTechInpGroup <- data.table(tech = rep(tech@name, length(gpp)), group = gpp)
      obj@parameters[["mTechInpGroup"]] <-
        .dat2par(obj@parameters[["mTechInpGroup"]], mTechInpGroup)
    } else {
      mTechInpGroup <- NULL
    }
    gpp <- rownames(ctype$group)[ctype$group$type == "output"]
    if (length(gpp) != 0) {
      mTechOutGroup <- data.table(tech = rep(tech@name, length(gpp)), group = gpp)
      obj@parameters[["mTechOutGroup"]] <-
        .dat2par(obj@parameters[["mTechOutGroup"]], mTechOutGroup)
    } else {
      mTechOutGroup <- NULL
    }
    approxim_group <- approxim
    approxim_group[["group"]] <- rownames(ctype$group)[ctype$group$type == "input"]
    if (length(approxim_group[["group"]]) != 0) {
      pTechGinp2use <- .interp_numpar(
        tech@geff, "ginp2use",
        obj@parameters[["pTechGinp2use"]], approxim_group, "tech", tech@name
      )
      obj@parameters[["pTechGinp2use"]] <-
        .dat2par(obj@parameters[["pTechGinp2use"]], pTechGinp2use)
    } else {
      pTechGinp2use <- NULL
    }
    if (nrow(ctype$group) > 0) {
      obj@parameters[["group"]] <- addMultipleSet(obj@parameters[["group"]], rownames(ctype$group))
    }
    fl <- !is.na(ctype$comm$group)
    if (any(fl)) {
      mTechGroupComm <- data.table(
        tech = rep(tech@name, sum(fl)), group = ctype$comm$group[fl],
        comm = rownames(ctype$comm)[fl], stringsAsFactors = FALSE
      )
      obj@parameters[["mTechGroupComm"]] <- .dat2par(obj@parameters[["mTechGroupComm"]], mTechGroupComm)
    } else {
      mTechGroupComm <- NULL
    }
    if (any(ctype$aux$output)) {
      cmm <- rownames(ctype$aux)[ctype$aux$output]
      mTechAOut <- data.table(tech = rep(tech@name, length(cmm)), comm = cmm)
      obj@parameters[["mTechAOut"]] <- .dat2par(obj@parameters[["mTechAOut"]], mTechAOut)
    } else {
      mTechAOut <- NULL
    }
    if (any(ctype$aux$input)) {
      cmm <- rownames(ctype$aux)[ctype$aux$input]
      mTechAInp <- data.table(tech = rep(tech@name, length(cmm)), comm = cmm)
      obj@parameters[["mTechAInp"]] <- .dat2par(obj@parameters[["mTechAInp"]], mTechAInp)
    } else {
      mTechAInp <- NULL
    }

    # numpar & bounds
    # obj@parameters[["pTechCap2act"]] <- .dat2par(
    #   obj@parameters[["pTechCap2act"]],
    #   data.table(tech = tech@name, value = tech@cap2act)
    # )
    pTechFixom <- .interp_numpar(tech@fixom, "fixom",
                                 obj@parameters[["pTechFixom"]],
                                 approxim, "tech", tech@name)
    obj@parameters[["pTechFixom"]] <-
      .dat2par(obj@parameters[["pTechFixom"]], pTechFixom)
    pTechVarom <- .interp_numpar(tech@varom, "varom",
                                 obj@parameters[["pTechVarom"]],
                                 approxim, "tech", tech@name)
    obj@parameters[["pTechVarom"]] <-
      .dat2par(obj@parameters[["pTechVarom"]], pTechVarom)

    ## Move from reduce
    # browser()
    mTechNew <- dd0$new
    mTechSpan <- dd0$span
    pTechOlife <- olife
    if (tech@optimizeRetirement) {
      if (!is.null(stock_exist)) {
        stock_exists <- stock_exist |>
          filter(value != 0) |>
          select(-any_of("value"))
        # browser()
        obj@parameters[["mvTechRetiredStock"]] <- .dat2par(
          obj@parameters[["mvTechRetiredStock"]], stock_exists)
      }

      # stock_exist[stock_exist$value != 0, colnames(stock_exist) != "value"]
      # select(filter(stock_exist, value != 0), -any_of("value"))
      # stock_exist[stock_exist$value != 0, colnames(stock_exist) != "value"]
      # )
    }
    # browser()
    if (nrow(dd0$new) > 0 && tech@optimizeRetirement) {
      obj@parameters[["meqTechRetiredNewCap"]] <-
        .dat2par(obj@parameters[["meqTechRetiredNewCap"]], mTechNew)


      mvTechRetiredCap0 <- merge0(merge0(mTechNew, mTechSpan, by = c("tech", "region")),
                                  pTechOlife,
                                  by = c("tech", "region")
      )
      mvTechRetiredCap0 <- mvTechRetiredCap0[(
        mvTechRetiredCap0$year.x + mvTechRetiredCap0$olife > mvTechRetiredCap0$year.y &
          mvTechRetiredCap0$year.x <= mvTechRetiredCap0$year.y), -5]
      colnames(mvTechRetiredCap0)[3:4] <- c("year", "year.1")
      mvTechRetiredCap0 <- filter(mvTechRetiredCap0, year != year.1)
      obj@parameters[["mvTechRetiredNewCap"]] <- .dat2par(
        obj@parameters[["mvTechRetiredNewCap"]],
        mvTechRetiredCap0
      )
    }
    mvTechAct <- merge0(mTechSpan, mTechSlice, by = "tech")
    obj@parameters[["mvTechAct"]] <-
      .dat2par(obj@parameters[["mvTechAct"]], mvTechAct)
    # Stay only variable with non zero output
    # browser()
    merge_table <- function(mvTechInp, pTechCinp2use) {
      if (is.null(pTechCinp2use) || nrow(pTechCinp2use) == 0) {
        return(NULL)
      }
      return(merge0(
        mvTechInp,
        # pTechCinp2use[pTechCinp2use$value != 0 & pTechCinp2use$value != Inf,
        #               colnames(pTechCinp2use) != "value", drop = FALSE]
        select(
          filter(pTechCinp2use, value != 0 & value < Inf),
          -any_of("value")
        )
      ))
    }
    merge_table2 <- function(mvTechInp, pTechCinp2use, pTechCinp2ginp) {
      if (is.null(pTechCinp2use)) {
        return(merge_table(mvTechInp, pTechCinp2ginp))
      }
      if (is.null(pTechCinp2ginp)) {
        return(merge_table(mvTechInp, pTechCinp2use))
      }
      merge0(mvTechInp, unique(rbind(
        pTechCinp2use[pTechCinp2use$value != 0 & pTechCinp2use$value != Inf, ],
        pTechCinp2ginp[pTechCinp2ginp$value != 0 & pTechCinp2ginp$value != Inf, ]
      )))
    }
    # browser()
    if (!is.null(mTechInpComm)) {
      mvTechInp <- merge0(mvTechAct, mTechInpComm, by = "tech")
      mvTechInp <- merge_table2(mvTechInp, pTechCinp2use, pTechCinp2ginp)
      obj@parameters[["mvTechInp"]] <-
        .dat2par(obj@parameters[["mvTechInp"]], mvTechInp)
    } else {
      mvTechInp <- NULL
    }
    if (!is.null(mTechOutComm)) {
      mvTechOut <- merge0(mvTechAct, mTechOutComm, by = "tech")
      obj@parameters[["mvTechOut"]] <-
        .dat2par(obj@parameters[["mvTechOut"]], mvTechOut)
      # browser()
      # mvTechOutS <- mvTechOut |>
      #   left_join()

      mTechOutRY <- mvTechOut |> select(-slice) |> unique()
      obj@parameters[["mTechOutRY"]] <-
        .dat2par(obj@parameters[["mTechOutRY"]], mTechOutRY)

    } else {
      mvTechOut <- NULL
    }
    if (!is.null(mTechAInp)) {
      mvTechAInp <- merge0(mvTechAct, mTechAInp, by = "tech")
      obj@parameters[["mvTechAInp"]] <-
        .dat2par(obj@parameters[["mvTechAInp"]], mvTechAInp)
    } else {
      mvTechAInp <- NULL
    }
    if (!is.null(mTechAOut)) {
      # browser()
      mvTechAOut <- merge0(mvTechAct, mTechAOut, by = "tech")
      obj@parameters[["mvTechAOut"]] <-
        .dat2par(obj@parameters[["mvTechAOut"]], mvTechAOut)
    } else {
      mvTechAOut <- NULL
    }
    #### aeff ####
    if (nrow(tech@aeff) != 0) {
      if (any(is.na(tech@aeff$acomm))) {
        stop(paste0("NA values in column acomm, slot aeff ", tech@name))
      }
      if (any(is.na(tech@aeff[apply(!is.na(tech@aeff[, c("cinp2ainp", "cinp2aout", "cout2ainp", "cout2aout"), drop = FALSE]), 1, any), "comm"]))) {
        stop(paste0("NA value in column comm, slot aeff ", tech@name), "\n",
             "Parameter 'aeff' requires commodity specification.")
      }
      for (i in 1:4) {
        tech@aeff <- tech@aeff[!is.na(tech@aeff$acomm), ]
        ll <- c("cinp2ainp", "cinp2aout", "cout2ainp", "cout2aout")[i]
        tbl <- c("pTechCinp2AInp", "pTechCinp2AOut", "pTechCout2AInp",
                 "pTechCout2AOut")[i]
        tbl2 <- c("mTechCinp2AInp", "mTechCinp2AOut", "mTechCout2AInp",
                  "mTechCout2AOut")[i]
        # browser()
        # yy <- tech@aeff[!is.na(tech@aeff[, ll]), ]
        yy <- tech@aeff[!is.na(tech@aeff[[ll]]), ]
        if (nrow(yy) != 0) {
          approxim_commp <- approxim
          approxim_commp$acomm <- unique(yy$acomm)
          approxim_commp$comm <- unique(yy$comm)
          tmp <- .interp_numpar(yy, ll, obj@parameters[[tbl]],
                                approxim_commp, "tech", tech@name)
          tmp <- tmp[tmp$value != 0, ]
          if (nrow(tmp) > 0) {
            obj@parameters[[tbl]] <- .dat2par(obj@parameters[[tbl]], tmp)
            tmp$value <- NULL
            if (!all(c("tech", "acomm", "comm", "region", "year", "slice") %in%
                     colnames(tmp))) {
              if (i %in% c(1, 3)) {
                tmp <- merge0(tmp, mvTechInp)
              } else {
                tmp <- merge0(tmp, mvTechOut)
              }
            }
            tmp$comm.1 <- tmp$comm
            tmp$comm <- tmp$acomm
            tmp$acomm <- NULL
            obj@parameters[[tbl2]] <-
              .dat2par(obj@parameters[[tbl2]], tmp[!duplicated(tmp), ])
          }
        }
      }
    }
    dd <- data.frame(
      list = c(
        "pTechAct2AOut", "pTechCap2AOut", "pTechNCap2AOut",
        "pTechAct2AInp", "pTechCap2AInp", "pTechNCap2AInp"
      ),
      table = c("act2aout", "cap2aout", "ncap2aout",
                "act2ainp", "cap2ainp", "ncap2ainp"),
      tab2 = c("mTechAct2AOut", "mTechCap2AOut", "mTechNCap2AOut",
               "mTechAct2AInp", "mTechCap2AInp", "mTechNCap2AInp"),
      stringsAsFactors = FALSE
    )
    # browser()
    for (i in 1:nrow(dd)) {
      approxim_comm <- approxim_comm[names(approxim_comm) != "comm"]
      approxim_comm[["acomm"]] <-
        unique(tech@aeff[!is.na(tech@aeff[, dd$table[i]]), "acomm"])
      if (length(approxim_comm[["acomm"]]) != 0) {
        tmp <- .interp_numpar(tech@aeff, dd$table[i],
                              obj@parameters[[dd$list[i]]],
                              approxim_comm, "tech", tech@name)
        obj@parameters[[dd[i, "list"]]] <-
          .dat2par(obj@parameters[[dd[i, "list"]]], tmp)
        if (!all(c("tech", "acomm", "region", "year", "slice") %in% colnames(tmp))) {
          if (i <= 3) ll <- mvTechInp else ll <- mvTechOut
          ll$comm <- NULL
          tmp <- merge0(tmp, unique(ll))
        }
        tmp$comm <- tmp$acomm
        tmp$acomm <- NULL
        tmp$value <- NULL
        if (ncol(tmp) != ncol(mvTechAct) + 1) {
          tmp <- merge0(tmp, mvTechAct)
        }
        obj@parameters[[dd[i, "tab2"]]] <-
          .dat2par(obj@parameters[[dd[i, "tab2"]]], tmp)
      }
    }
    #### aeff end
    if (!is.null(mTechInpGroup) && !is.null(mTechOutGroup)) {
      meqTechGrp2Grp <- merge0(
        merge0(mTechInpGroup, mTechOutGroup, by = "tech", suffix = c("", ".1")),
        mvTechAct
      )[, c("tech", "region", "group", "group.1", "year", "slice")]
      obj@parameters[["meqTechGrp2Grp"]] <-
        .dat2par(obj@parameters[["meqTechGrp2Grp"]], meqTechGrp2Grp)
    } else {
      meqTechGrp2Grp <- NULL
    }
    if (!is.null(mTechInpGroup) || !is.null(mTechOutGroup)) {
      # browser()
      mpTechShareLo <- pTechShare |>
        filter(type == "lo" & value > 0) |>
        select(-any_of(c("value", "type")))
      # mpTechShareUp <- pTechShare[pTechShare$type == "up" & pTechShare$value < 1,
      #                             colnames(pTechShare) != "value"]
      mpTechShareUp <- pTechShare |>
        filter(type == "up" & value > 0) |>
        select(-any_of(c("value", "type")))

    } else {
      mpTechShareUp <- NULL
      mpTechShareLo <- NULL
    }
    if (!is.null(mvTechOut) && !is.null(mTechOutGroup) && !is.null(mTechGroupComm)) {
      techGroupOut <- merge0(merge0(mvTechOut, mTechOutGroup), mTechGroupComm)
    } else {
      techGroupOut <- NULL
    }
    if (!is.null(mvTechInp) && !is.null(mTechInpGroup) && !is.null(mTechGroupComm)) {
      techGroupInp <- merge0(merge0(mvTechInp, mTechInpGroup), mTechGroupComm)
    } else {
      techGroupInp <- NULL
    }
    if (!is.null(mvTechInp) && !is.null(mTechOneComm)) {
      # browser()
      techSingInp <- merge0(mvTechInp, mTechOneComm)
      if (!is.null(pTechCinp2use)) {
        techSingInp <- merge0(
          techSingInp,
          # pTechCinp2use[pTechCinp2use$value != 0,
          #               colnames(pTechCinp2use) %in% colnames(techSingInp),
          #               drop = FALSE]
          select(
            filter(pTechCinp2use, value != 0),
            any_of(colnames(techSingInp))
          )
        )
      }
      if (nrow(techSingInp) == 0) techSingInp <- NULL
    } else {
      techSingInp <- NULL
    }
    if (!is.null(mvTechOut) && !is.null(mTechOneComm)) {
      techSingOut <- merge0(mvTechOut, mTechOneComm)
      if (!is.null(pTechCact2cout)) {
        techSingOut <- merge0(
          techSingOut,
          # pTechCact2cout[pTechCact2cout$value != 0,
          #                colnames(pTechCact2cout) %in% colnames(techSingOut),
          #                drop = FALSE]
          select(
            filter(pTechCact2cout, value != 0),
            any_of(colnames(techSingOut))
          )
        )
      }
      if (nrow(techSingOut) == 0) techSingOut <- NULL
    } else {
      techSingOut <- NULL
    }
    if (!is.null(mTechInpGroup) && !is.null(techSingOut)) {
      meqTechGrp2Sng <- merge0(mTechInpGroup, techSingOut)
      obj@parameters[["meqTechGrp2Sng"]] <-
        .dat2par(obj@parameters[["meqTechGrp2Sng"]], meqTechGrp2Sng)
    } else {
      meqTechGrp2Sng <- NULL
    }
    if (!is.null(mTechOutGroup) && !is.null(techSingInp)) {
      meqTechSng2Grp <- merge0(mTechOutGroup, techSingInp)
      obj@parameters[["meqTechSng2Grp"]] <-
        .dat2par(obj@parameters[["meqTechSng2Grp"]], meqTechSng2Grp)
    } else {
      meqTechSng2Grp <- NULL
    }

    if (!is.null(techSingInp) && !is.null(techSingOut)) {
      # browser()
      meqTechSng2Sng <- merge0(techSingInp, techSingOut,
                               by = c("tech", "region", "year", "slice"),
                               suffixes = c("", ".1"))
      # filter out unavailable combinations
      # browser()
      obj@parameters[["meqTechSng2Sng"]] <-
        .dat2par(obj@parameters[["meqTechSng2Sng"]], meqTechSng2Sng)
    } else {
      meqTechSng2Sng <- NULL
    }
    if (!is.null(mpTechShareLo) && !is.null(techGroupOut)) {
      # browser()
      meqTechShareOutLo <- merge0(mpTechShareLo, techGroupOut)
      obj@parameters[["meqTechShareOutLo"]] <- .dat2par(
        obj@parameters[["meqTechShareOutLo"]],
        select(meqTechShareOutLo,
               all_of(obj@parameters[["meqTechShareOutLo"]]@dimSets)
        )
      )
    } else {
      meqTechShareOutLo <- NULL
    }
    if (!is.null(mpTechShareUp) && !is.null(techGroupOut)) {
      meqTechShareOutUp <- merge0(mpTechShareUp, techGroupOut)
      obj@parameters[["meqTechShareOutUp"]] <- .dat2par(
        obj@parameters[["meqTechShareOutUp"]],
        select(
          meqTechShareOutUp,
          all_of(obj@parameters[["meqTechShareOutUp"]]@dimSets)
        )
        # meqTechShareOutUp[, obj@parameters[["meqTechShareOutUp"]]@dimSets]
      )
    } else {
      meqTechShareOutUp <- NULL
    }
    if (!is.null(mpTechShareLo) && !is.null(techGroupInp)) {
      meqTechShareInpLo <- merge0(mpTechShareLo, techGroupInp)
      obj@parameters[["meqTechShareInpLo"]] <- .dat2par(
        obj@parameters[["meqTechShareInpLo"]],
        select(
          meqTechShareInpLo,
          all_of(obj@parameters[["meqTechShareInpLo"]]@dimSets)
        )
        # meqTechShareInpLo[, obj@parameters[["meqTechShareInpLo"]]@dimSets]
      )
    } else {
      meqTechShareInpLo <- NULL
    }
    if (!is.null(mpTechShareUp) && !is.null(techGroupInp)) {
      meqTechShareInpUp <- merge0(mpTechShareUp, techGroupInp)
      obj@parameters[["meqTechShareInpUp"]] <- .dat2par(
        obj@parameters[["meqTechShareInpUp"]],
        # meqTechShareInpUp[, obj@parameters[["meqTechShareInpUp"]]@dimSets]
        select(
          meqTechShareInpUp,
          all_of(obj@parameters[["meqTechShareInpUp"]]@dimSets)
        )
      )
    } else {
      meqTechShareInpUp <- NULL
    }

    ####
    outer_inf <- function(mvTechAct, pTechAf) {
      merge0(mvTechAct,
             # pTechAf[pTechAf$value != Inf & pTechAf$type == "up",
             #         colnames(pTechAf) %in% colnames(mvTechAct),
             #         drop = FALSE]
             select(
               filter(pTechAf, value != Inf & type == "up"),
               any_of(colnames(mvTechAct))
             )
      )
    }
    if (!is.null(pTechAf) && any(pTechAf$value != 0 & pTechAf$type == "lo")) {
      obj@parameters[["meqTechAfLo"]] <-
        .dat2par(
          obj@parameters[["meqTechAfLo"]],
          merge0(mvTechAct,
                 # pTechAf[pTechAf$value != 0 & pTechAf$type == "lo",
                 #         colnames(pTechAf)[colnames(pTechAf) %in% colnames(mvTechAct)],
                 #         drop = FALSE]
                 select(
                   filter(pTechAf, value != 0 & type == "lo"),
                   any_of(colnames(mvTechAct))
                 )
          )
        )
    }
    obj@parameters[["meqTechAfUp"]] <-
      .dat2par(obj@parameters[["meqTechAfUp"]], outer_inf(mvTechAct, pTechAf))
    if (!is.null(pTechAfs)) {
      obj@parameters[["meqTechAfsLo"]] <-
        .dat2par(
          obj@parameters[["meqTechAfsLo"]],
          merge0(
            mTechSpan,
            # pTechAfs[
            #   pTechAfs$value != 0 & pTechAfs$type == "lo",
            #   colnames(pTechAfs)[
            #     colnames(pTechAfs) %in%
            #       obj@parameters[["meqTechAfsLo"]]@dimSets]
            #   ]
            select(
              filter(pTechAfs, value != 0 & type == "lo"),
              any_of(obj@parameters[["meqTechAfsLo"]]@dimSets)
            )
          )
        )
      meqTechAfsUp <- merge0(
        mTechSpan,
        # pTechAfs[pTechAfs$value != Inf & pTechAfs$type == "up",
        #          colnames(pTechAfs) %in% obj@parameters[["meqTechAfsUp"]]@dimSets,
        #          drop = FALSE]
        select(
          filter(pTechAfs, value != Inf & type == "up"),
          any_of(obj@parameters[["meqTechAfsUp"]]@dimSets)
        )
      )
      obj@parameters[["meqTechAfsUp"]] <-
        .dat2par(obj@parameters[["meqTechAfsUp"]], meqTechAfsUp)
    }
    if (!is.null(techSingOut)) {
      obj@parameters[["meqTechActSng"]] <-
        .dat2par(obj@parameters[["meqTechActSng"]], techSingOut)
    } else {
      meqTechActSng <- NULL
    }
    if (!is.null(mTechOutGroup)) {
      obj@parameters[["meqTechActGrp"]] <-
        .dat2par(obj@parameters[["meqTechActGrp"]],
                 merge0(mvTechAct, mTechOutGroup))
    } else {
      meqTechActGrp <- NULL
    }
    if (!is.null(pTechAfc)) {
      merge_afc <- function(prm, mvTechOut, pTechAfc, type) {
        if (is.null(pTechAfc) || nrow(pTechAfc) == 0) {
          return(prm)
        }
        if (type == "up") {
          # pTechAfc <- pTechAfc[
          #   pTechAfc$value != Inf & pTechAfc$type == "up",
          #   colnames(pTechAfc) %in%  obj@parameters[["meqTechAfcOutLo"]]@dimSets,
          #   drop = FALSE]
          pTechAfc <- pTechAfc |>
            filter(value != Inf, type == "up") |>
            select(any_of(obj@parameters[["meqTechAfcOutLo"]]@dimSets))
        } else {
          # pTechAfc <- pTechAfc[
          #   pTechAfc$value != 0 & pTechAfc$type == "lo",
          #   colnames(pTechAfc) %in% obj@parameters[["meqTechAfcOutLo"]]@dimSets,
          #   drop = FALSE]
          pTechAfc <- pTechAfc |>
            filter(value != 0 & type == "lo") |>
            select(any_of(obj@parameters[["meqTechAfcOutLo"]]@dimSets))
        }
        if (nrow(pTechAfc) == 0) {
          return(prm)
        }
        return(.dat2par(prm, merge0(mvTechOut, pTechAfc)))
      }
      obj@parameters[["meqTechAfcOutLo"]] <-
        merge_afc(obj@parameters[["meqTechAfcOutLo"]], mvTechOut, pTechAfc, "lo")
      obj@parameters[["meqTechAfcOutUp"]] <-
        merge_afc(obj@parameters[["meqTechAfcOutUp"]], mvTechOut, pTechAfc, "up")
      obj@parameters[["meqTechAfcInpLo"]] <-
        merge_afc(obj@parameters[["meqTechAfcInpLo"]], mvTechInp, pTechAfc, "lo")
      obj@parameters[["meqTechAfcInpUp"]] <-
        merge_afc(obj@parameters[["meqTechAfcInpUp"]], mvTechInp, pTechAfc, "up")
    }

    if (nrow(tech@weather) > 0) { # !!!ToDo: dplyr
      tmp <- .toWeatherImply(tech@weather, "waf", "tech", tech@name)
      obj@parameters[["pTechWeatherAf"]] <-
        .dat2par(obj@parameters[["pTechWeatherAf"]], tmp$par)
      obj@parameters[["mTechWeatherAfUp"]] <-
        .dat2par(obj@parameters[["mTechWeatherAfUp"]], tmp$mapup)
      obj@parameters[["mTechWeatherAfLo"]] <-
        .dat2par(obj@parameters[["mTechWeatherAfLo"]], tmp$maplo)

      tmp <- .toWeatherImply(tech@weather, "wafs", "tech", tech@name)
      obj@parameters[["pTechWeatherAfs"]] <-
        .dat2par(obj@parameters[["pTechWeatherAfs"]], tmp$par)
      obj@parameters[["mTechWeatherAfsUp"]] <-
        .dat2par(obj@parameters[["mTechWeatherAfsUp"]], tmp$mapup)
      obj@parameters[["mTechWeatherAfsLo"]] <-
        .dat2par(obj@parameters[["mTechWeatherAfsLo"]], tmp$maplo)

      if (any(is.na(tech@weather$comm)[
        apply(!is.na(tech@weather[, c("wafc.lo", "wafc.up", "wafc.fx"),
                                  drop = FALSE]), 1, any)])) {
        stop("comm must be defined for wafc.* parameters")
      }
      tmp <- .toWeatherImply(tech@weather, "wafc", "tech", tech@name, "comm")
      obj@parameters[["pTechWeatherAfc"]] <-
        .dat2par(obj@parameters[["pTechWeatherAfc"]], tmp$par)
      obj@parameters[["mTechWeatherAfcUp"]] <-
        .dat2par(obj@parameters[["mTechWeatherAfcUp"]], tmp$mapup)
      obj@parameters[["mTechWeatherAfcLo"]] <-
        .dat2par(obj@parameters[["mTechWeatherAfcLo"]], tmp$maplo)
    }

    if (all(ctype$comm$type != "output")) {
      stop('Techology "', tech@name, '", there is not activity commodity')
    }
    # mTechOMCost(tech, region, year)
    # mTechOMCost <- NULL
    mTechFixom <- NULL
    add_omcost <- function(x, y) {
      if (is.null(y) || all(y$value == 0)) {
        return(x)
      }
      x <- rbind(
        x,
        merge0(
          mTechSpan,
          # pTechFixom[pTechFixom$value != 0,
          #            colnames(pTechFixom) %in% colnames(mTechSpan),
          #            drop = FALSE]
          select(
            filter(y, value != 0),
            any_of(colnames(mTechSpan))
          )
        )
      )
      return(unique(x))
    }

    # browser()
    mTechFixom <- add_omcost(mTechFixom, pTechFixom)
    if (!is.null(mTechFixom)) {
      mTechFixom <- merge0(mTechFixom[!duplicated(mTechFixom), ], mTechSpan)
      obj@parameters[["mTechFixom"]] <-
        .dat2par(obj@parameters[["mTechFixom"]], mTechFixom)
    }
    mTechVarom <- NULL
    mTechVarom <- add_omcost(mTechVarom, pTechVarom)
    mTechVarom <- add_omcost(mTechVarom, pTechCvarom)
    mTechVarom <- add_omcost(mTechVarom, pTechAvarom)
    if (!is.null(mTechVarom)) {
      mTechVarom <- merge0(mTechVarom, mTechSpan)
      obj@parameters[["mTechVarom"]] <-
        .dat2par(obj@parameters[["mTechVarom"]], mTechVarom)
    }

    ### Ramp
    if (tech@fullYear) {
      obj@parameters[["mTechFullYear"]] <- .dat2par(
        obj@parameters[["mTechFullYear"]],
        data.table(tech = tech@name)
      )
    }

    obj <- .add_ramp0(obj, "rampup", tech, mvTechAct, approxim)
    obj <- .add_ramp0(obj, "rampdown", tech, mvTechAct, approxim)

    # browser()
    # "mTechRampSliceNext" # tech, region, year, slice, slicep
    # ramp_data <- c(tech@af$rampdown, tech@af$rampup)
    # if (!is_empty(ramp_data) && any(!is.na(ramp_data))) {
    #
    #   if (tech@fullYear) {
    #     mTechRampSliceNext <- obj@parameters[["mSliceFYearNext"]]@data
    #   } else {
    #     mTechRampSliceNext <- obj@parameters[["mSliceNext"]]@data
    #   }
    #   tech_name <- tech@name
    #   mTechRampSliceNext <- mTechRampSliceNext |>
    #     mutate(tech = tech_name, .before = 1) |>
    #     merge0(mvTechAct) |>
    #     select(all_of(obj@parameters[["mTechRampSliceNext"]]@dimSets))
    #   obj@parameters[["mTechRampSliceNext"]] <-
    #     .dat2par(obj@parameters[["mTechRampSliceNext"]], mTechRampSliceNext)
    # }
    obj
  }
)



# =============================================================================#
# Add settings ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "settings", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    # browser()
    # clean_list <- c(
    #   "mSliceParentChild", "mSliceParentChildE", "mSliceNext",
    #   "mSliceFYearNext", "pDiscount", "pSliceShare", "pDummyImportCost",
    #   "pDummyExportCost",
    #   "pSliceWeight",
    #   # "mStartMilestone", "mEndMilestone",
    #   "mMilestoneLast", "mMilestoneFirst", "mMilestoneNext",
    #   "mMilestoneHasNext", "mSameSlice", "mSameRegion", "ordYear",
    #   "pYearFraction",
    #   "cardYear", "pPeriodLen", "pDiscountFactor" #, "mDiscountZero"
    # )
    # for (i in clean_list) {
    #   obj@parameters[[i]] <- .resetParameter(obj@parameters[[i]])
    # }
    # obj <- .drop_config_param(obj)
    # app <- .filter_data_in_slots(app, approxim$region, "region")
    obj@parameters[["mSliceParentChild"]] <- .dat2par(
      obj@parameters[["mSliceParentChild"]],
      data.table(
        slice = as.character(approxim$calendar@slice_ancestry$parent),
        slicep = as.character(approxim$calendar@slice_ancestry$child),
        stringsAsFactors = FALSE
      )
    )
    obj@parameters[["mSliceParentChildE"]] <- .dat2par(
      obj@parameters[["mSliceParentChildE"]],
      data.table(
        slice = as.character(c(app@calendar@slice_share$slice,
                               approxim$calendar@slice_ancestry$parent)),
        slicep = as.character(c(app@calendar@slice_share$slice,
                                approxim$calendar@slice_ancestry$child)),
        stringsAsFactors = FALSE
      )
    )
    # browser()
    if (length(approxim$calendar@next_in_timeframe) != 0) {
      obj@parameters[["mSliceNext"]] <-
        .dat2par(obj@parameters[["mSliceNext"]],
                 approxim$calendar@next_in_timeframe)
      obj@parameters[["mSliceFYearNext"]] <-
        .dat2par(obj@parameters[["mSliceFYearNext"]],
                 approxim$calendar@next_in_year)
    }
    # Discount
    # browser()
    approxim_no_mileStone_Year <- approxim
    approxim_no_mileStone_Year$mileStoneYears <- NULL
    pDiscount <- .interp_numpar(app@discount, "discount",
                                obj@parameters[["pDiscount"]], approxim_no_mileStone_Year,
                                all.val = TRUE
    )
    obj@parameters[["pDiscount"]] <-
      .dat2par(obj@parameters[["pDiscount"]],
               filter(pDiscount, year %in% obj@parameters$year@data$year)
               # pDiscount
      )
    approxim_comm <- approxim
    approxim_comm[["comm"]] <- approxim$all_comm
    obj@parameters[["pSliceShare"]] <- .dat2par(
      obj@parameters[["pSliceShare"]],
      data.table(
        slice = approxim$calendar@slice_share$slice,
        value = approxim$calendar@slice_share$share
      )
    )
    approxim_comm$slice <- approxim$calendar@slice_share$slice

    # browser()
    data.table::setNumericRounding(2) # ignore small differences in 'unique' function
    # add pSliceWeight from calendar@misc$pSliceWeight o @slice_share$weight
    if (!is_null(approxim$calendar@misc$pSliceWeight)) {

      pSliceWeight_tmp <- data.table(
        year = approxim$calendar@misc$pSliceWeight$year,
        slice = approxim$calendar@misc$pSliceWeight$slice,
        value = approxim$calendar@misc$pSliceWeight$weight
      )
      if (is_null(pSliceWeight_tmp[["value"]])) {
        if (is_null(pSliceWeight_tmp[["weight"]])) {
          stop("No slice weight in calendar@misc$pSliceWeight$value or @slice_share$weight")
        }
        pSliceWeight_tmp <- rename(pSliceWeight_tmp, value = weight)
      }
    } else {
      pSliceWeight_tmp <- lapply(approxim$year, function(x) {
        data.table(
          year = x,
          slice = approxim$calendar@slice_share$slice,
          value = approxim$calendar@slice_share$weight
        )
      }) |> rbindlist()
    }

    # calculate slice weights for "parent" timeframes
    # requirements: weights are given for the lowest level time-slices
    # approxim$calendar@slice_share |>
    #   select(-weight) |>
    #   left_join(, by = "slice")

    a <- pSliceWeight_tmp |>
      left_join(approxim$calendar@slice_ancestry, by = c("slice" = "child")) |>
      left_join(select(approxim$calendar@slice_share, -weight),
                by = c("parent" = "slice")) |>
      rename(weight = value) |>
      filter(!is.na(parent))

    b <-
      approxim$calendar@timetable |>
      select(1:slice) |>
      pivot_longer(cols = -slice, names_to = "timeframe",
                   values_to = "parent") |>
      as.data.table() |>
      select(timeframe, parent, slice) |>
      arrange(timeframe, parent, slice)

    ab <- left_join(a, b, by = c("slice", "parent")) |>
      group_by(year, parent) |>
      summarise(value = weighted.mean(weight, w = share)) |>
      rename(slice = parent) |>
      as.data.table()

    pSliceWeight_tmp <- rbind(pSliceWeight_tmp, ab) |>
      filter(year %in% approxim$mileStoneYears) |>
      unique()
    # pSliceWeight_tmp$value <- pSliceWeight_tmp$value /
    #   (24 * 7 * 52 / 24 / 365)

    obj@parameters[["pSliceWeight"]] <- .dat2par(
      obj@parameters[["pSliceWeight"]],
      # data.table(
      #   slice = approxim$calendar@slice_share$slice,
      #   value = approxim$calendar@slice_share$weight
      # )
      pSliceWeight_tmp
    )
    # browser()
    rm(a, b, ab, pSliceWeight_tmp)

    if (nrow(app@horizon@intervals) == 0) { # ???
      browser()
      app <- setHorizon(app,
                        horizon = app@horizon@period,
                        intervals = rep(1, length(app@horizon@period)))
    }
    obj@parameters[["mMilestoneLast"]] <- .dat2par(
      obj@parameters[["mMilestoneLast"]],
      data.table(year = max(app@horizon@intervals$mid))
    )
    obj@parameters[["mMilestoneFirst"]] <- .dat2par(
      obj@parameters[["mMilestoneFirst"]],
      data.table(year = min(app@horizon@intervals$mid))
    )
    # browser()
    obj@parameters[["mMilestoneNext"]] <- .dat2par(
      obj@parameters[["mMilestoneNext"]],
      data.table(year = app@horizon@intervals$mid[-nrow(app@horizon@intervals)],
                 yearp = app@horizon@intervals$mid[-1])
    )
    obj@parameters[["mMilestoneHasNext"]] <- .dat2par(
      obj@parameters[["mMilestoneHasNext"]],
      data.table(year = app@horizon@intervals$mid[-nrow(app@horizon@intervals)])
    )

    obj@parameters[["mSameSlice"]] <- .dat2par(
      obj@parameters[["mSameSlice"]],
      data.table(slice = app@calendar@slice_share$slice,
                 slicep = app@calendar@slice_share$slice)
    )
    obj@parameters[["mSameRegion"]] <- .dat2par(
      obj@parameters[["mSameRegion"]],
      data.table(region = app@region,
                 regionp = app@region)
    )
    # browser()
    # tmp <- data.frame(year = .get_data_slot(obj@parameters$year))
    tmp <- .get_data_slot(obj@parameters$year)
    # tmp$value <- seq_along(tmp$year)
    tmp$value <- tmp$year - min(tmp$year) + 1
    obj@parameters[["ordYear"]] <- .dat2par(obj@parameters[["ordYear"]], tmp)
    obj@parameters[["cardYear"]] <- .dat2par(obj@parameters[["cardYear"]],
                                             tmp[nrow(tmp), , drop = FALSE])

    obj@parameters[["pPeriodLen"]] <- .dat2par(
      obj@parameters[["pPeriodLen"]],
      data.table(year = app@horizon@intervals$mid,
                 value = (app@horizon@intervals$end -
                            app@horizon@intervals$start + 1),
                 stringsAsFactors = FALSE)
    )
    # browser()
    pDiscount <-
      pDiscount[sort(pDiscount$year, index.return = TRUE)$ix, , drop = FALSE]
    pDiscountFactor <- pDiscount[0, , drop = FALSE]
    for (l in unique(pDiscount$region)) {
      dd <- pDiscount[pDiscount$region == l, , drop = FALSE]
      if (!app@discountFirstYear) dd$value[which.min(dd$year)] <- 0
      dd$value <- cumprod(1 / (1 + dd$value))
      pDiscountFactor <- rbind(pDiscountFactor, dd)
    }
    obj@parameters[["pDiscountFactor"]] <-
      .dat2par(obj@parameters[["pDiscountFactor"]],
               filter(pDiscountFactor, year %in% obj@parameters$year@data$year)
               # pDiscountFactor
      )
    pYearFraction <- .get_data_slot(obj@parameters$year)
    pYearFraction$value <- app@yearFraction$fraction
    # browser()
    obj@parameters[["pYearFraction"]] <-
      .dat2par(obj@parameters[["pYearFraction"]], pYearFraction)
    obj@parameters[["pYearFraction"]]@defVal <- 1 # !!! temporary fix
    obj
  }
)

# =============================================================================#
# functions ####
# =============================================================================#
force_cols_classes <- function(dtf) {
  # if (!is.data.frame(dtf) & !is.list(dtf)) browser()
  # return(dtf)
  # temporary solution to avoid merging conflicts
  year_vars <- c("year", "yearp", "start", "end", "olife")
  force_class <- "integer"
  # force_class <- "numeric"
  for (y in year_vars) {
    if (!is.null(dtf[[y]]) && !inherits(dtf, force_class)) {
      dtf[[y]] <- as(dtf[[y]], force_class)
    }
  }
  # browser()
  string_vars <- c(
    "comm", "commp",
    "slice", "slicep",
    "region", "regionp",
    "exp", "expp",
    "stg", "stgp",
    "group",
    "tech", "techp",
    "dem", "comm", "sup", "trade", "weather"
  )

  for (s in string_vars) {
    if (!is.null(dtf[[s]]) && !inherits(dtf, "character")) {
      dtf[[s]] <- as.character(dtf[[s]])
    }
  }

  as.data.table(dtf)
}

# =============================================================================#
make_data_param <- function(scen, slot_data, obj_name, par_name,
                            short_name, class_col) {
  if (!inherits(slot_data, "data.frame")) {
    slot_data <- data.frame(value = slot_data)
  }
  browser()
  dat <- slot_data |>
    select(any_of(c(
      scen@modInp@parameters[[par_name]]@dimSets,
      short_name
      ))) |>
    rename(value = short_name) |>
    filter(!is.na(value)) |>
    unique()
  dat <- dat |>
    mutate({{class_col}} := obj_name) |>
    force_cols_classes()
  dat
}
# =============================================================================#
get_slot_meta <- function(class = NULL,
                          slot = NULL,
                          type = NULL,
                          # type = "bounds",
                          # type = "numpar",
                          dimSets = NULL,
                          colName = NULL,
                          return_names = NULL,
                          flat = length(return_names) == 1,
                          ...
                          ) {
  # sname = colName
  # browser()
  # ll <- lapply(.modInp, function(x) {
  #   # if (x$name == "pTechAct2AOut") browser()
  #   # if (x$name == "pTechCap2Act") browser()
  #
  #   if (!is.null(class) && !any(x$class %in% class)) {return(NULL)}
  #   if (!is.null(slot) && !any(x$slot %in% slot)) {return(NULL)}
  #   if (!is.null(type) && !any(x$type %in% type)) {return(NULL)}
  #   if (!is.null(dimSets) && !all(dimSets %in% x$dimSets)) {return(NULL)}
  #   if (!is.null(return_names)) {
  #     x <- x[names(x) %in% return_names]
  #   }
  #   x
  # })
  # ll <- ll[!sapply(ll, is_empty)]

  # browser()
  ll <- list()
  # for ( in seq_along(.modInp)) {
  for (x in .modInp) {
    # if (x$name == "pTechAct2AOut") browser()
    # if (x$name == "pTechCap2Act") browser()
    # x <- .modInp[[i]]
    # browser()
    if (!is.null(class) && !any(x$class %in% class)) {next}
    if (!is.null(slot) && !any(x$slot %in% slot)) {next}
    if (!is.null(type) && !any(x$type %in% type)) {next}
    if (!is.null(dimSets) && !all(dimSets %in% x$dimSets)) {next}
    if (!is.null(colName) && !any(x$colName %in% colName)) {next}
    x_name <- x$name
    if (!is.null(return_names)) {
      # browser()
      if (length(return_names) > 1 || isFALSE(flat)) {
        x <- x[names(x) %in% return_names]
      } else {
        if (!return_names %in% names(x)) {
          warning(
            paste("Slot", x_name, "does not have a name", return_names)
          )
        } else {
          x <- x[[return_names]]
        }
      }

    }
    ll[[x_name]] <- x
  }
  # names(ll)
  ll
}

if (FALSE) {
  # test
  ll <- get_slot_meta("technology", slot = "cap2act", return_names = "slot")
  names(ll)
  ll |> unlist()
  flatten(ll)
  as.data.frame(ll)
  ll <- get_slot_meta(class = "technology",
                      type = "numpar",
                      return_names = "slot")
  ll
  str(ll)
  names(ll)
  # ll[["pTechEac"]]
  get_slot_meta(class = "technology",
                # slot = "tech",
                type = "numpar",
                dimSets = c("region", "year", "slice")
                )
  get_slot_meta(class = "tech", slot = "tech", type = "numpar",
                dimSets = c("region", "year", "slice"), return_names = "slot")
  ll <- get_slot_meta(colName = "wval", return_names = "defVal", flat = TRUE)
  ll <- get_slot_meta(colName = "waf", return_names = "defVal")
  ll <- get_slot_meta(colName = "waf")
  class(ll)
  ll
  ll |> as.data.table()
  ll |> unlist()
  ll |> flatten()

}
# =============================================================================#



