setGeneric("ob2mi", function(scen, obj, extra_params) standardGeneric("ob2mi"))
setGeneric("d2p", function(obj, data, path) standardGeneric("d2p"))

get_data_slot <- function(obj) {
  # browser()
  data <- NULL
  if (isOnDisk(obj)) {
    data <- get_lazy_data(obj, "data")
    # The csv/parquet round-trip loses column types: an all-NA column (e.g. a
    # folded `region`/`timeslice`, or a map built on a region-wildcard parameter)
    # comes back as `logical`, which then breaks type-sensitive joins in the
    # fold / unfold passes. Restore the canonical classes on every on-disk read.
    if (!is.null(data) && nrow(data) > 0) data <- force_cols_classes(data)
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
      obj@data <- data
      obj <- obj2disk(obj, path = path)
      obj@data <- reset_slot(obj@data)
    } else {
      obj@data <- rbindlist(
        list(as.data.table(obj@data), as.data.table(data)),
        use.names = FALSE
      ) |>
        unique()
    }


    if (ncol(obj@data) != 1) invisible()  # browser() disabled
    if (is.factor(obj@data[[1]])) invisible()  # browser() disabled
    obj
  }
)

# d2p: NULL path (in-memory) ####
# Delegates to the `character`-path methods, whose bodies already handle a
# NULL path (in-memory assignment). Registering these signatures lets the
# mapping engine and interp pipeline run in memory (`path = NULL`).
setMethod(
  "d2p",
  signature(obj = "parameter", data = "data.frame", path = "NULL"),
  function(obj, data, path = NULL) {
    getMethod("d2p", c("parameter", "data.frame", "character"))(obj, data, NULL)
  }
)

setMethod(
  "d2p",
  signature(obj = "parameter", data = "character", path = "NULL"),
  function(obj, data, path = NULL) {
    getMethod("d2p", c("parameter", "character", "character"))(obj, data, NULL)
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
#' @returns the updated `scenario` object, invisibly.
#' @export
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
          # `<scen>/modInp` + the slot name + the parameter name. `obj2disk()`
          # writes each parameter to `<modInp path>/parameters/<name>`, so the
          # `parameters` segment is not optional -- without it this fallback
          # pointed at a directory that never exists, and it fires exactly when
          # the parameter was empty and so had no path of its own.
          path <- fp(path, "parameters", param)
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

# Pack the wide bounds columns (<colName>.lo / .up / .fx) of an object slot into
# the long (type, value) format expected by the interpolation engine. Only the
# `id_cols` that are present in the slot are carried; rows with NA bound values
# are dropped. Returns NULL when the slot has no finite bound, so callers can
# skip empty parameters. Mirrors the bounds branch of `make_data_param()`.
.pack_bounds_long <- function(slot_df, colName, id_cols) {
  if (is.null(slot_df) || nrow(slot_df) == 0) {
    return(NULL)
  }
  slot_df <- as.data.frame(slot_df)
  bound_cols <- paste0(colName, c(".lo", ".up", ".fx"))
  present <- intersect(bound_cols, colnames(slot_df))
  if (length(present) == 0) {
    return(NULL)
  }
  keep <- intersect(id_cols, colnames(slot_df))
  out <- slot_df |>
    select(all_of(c(keep, present))) |>
    pivot_longer(
      cols = all_of(present),
      names_to = "type",
      values_to = "value",
      names_prefix = paste0(colName, "."),
      values_drop_na = TRUE
    ) |>
    filter(!is.na(value))
  out <- .expand_fx_bounds(out)
  if (nrow(out) == 0) {
    return(NULL)
  }
  as.data.frame(out)
}

# Expand fixed bounds: a `type == "fx"` row means the bound is fixed
# (lower == upper == value). The stored `type` factor carries only levels
# lo/up, and the downstream bound maps select on lo/up (their `types` lists
# include "fx" defensively), so materialise each fx row into a `lo` and an `up`
# row here, while `type` is still a plain character column.
.expand_fx_bounds <- function(dat) {
  if (is.null(dat) || !"type" %in% colnames(dat) || nrow(dat) == 0) {
    return(dat)
  }
  fx <- as.character(dat$type) == "fx"
  fx[is.na(fx)] <- FALSE
  if (!any(fx)) {
    return(dat)
  }
  fx_rows <- dat[fx, , drop = FALSE]
  lo_rows <- fx_rows
  lo_rows$type <- "lo"
  up_rows <- fx_rows
  up_rows$type <- "up"
  dplyr::bind_rows(dat[!fx, , drop = FALSE], lo_rows, up_rows)
}


# ob2mi: scenario, ... ####
# =============================================================================#
## commodity ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "commodity", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    # .checkTimesliceLevel(obj, extra_params)
    # .check_timeframe(obj, scen)
    # browser()

    obj@name <- toString(obj@name)
    # obj <- .filter_data_in_slots(obj, extra_params$region, "region")

    ## pEmissionFactor ####
    # comm = emission commodity (e.g. CO2), commp = fuel being consumed
    # (matches legacy obj2modInp.R orientation).
    dat <- data.table(
      comm = obj@emis$comm,
      commp = obj@name,
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

    ## mCommTimeslice ####
    comm_timeframe <- obj@timeframe
    if (is_empty(comm_timeframe)) {
      comm_timeframe <- scen@settings@calendar@default_timeframe
    }
    com_timeslice <- scen@settings@calendar@timeframes[[comm_timeframe]]
    dat <- data.table(
      comm = obj@name,
      timeslice = com_timeslice
    ) |>
      force_cols_classes()
    scen <- update_parameter(scen, "mCommTimeslice", dat)

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
    # dem <- .disaggregateTimesliceLevel(dem, approxim)

    ## pDemand ####
    dat <- data.table(
      dem = dem@name,
      comm = dem@commodity,
      region = dem@demand$region,
      year = dem@demand$year,
      timeslice = dem@demand$timeslice,
      value = as.numeric(dem@demand$demand)
    ) |>
      force_cols_classes()

    if (length(dem@region) != 0) {
      # filter out unused regions
      if (any(!(dem@region %in% .known_regions(scen)))) {
        stop(
          'Region "', dem@region, '" used in demand "', dem@name,
          '" is not declared in the model.'
        )
      }
      # NB: `dat` has a column named `dem` (the demand name), which would shadow
      # the `dem` object inside data.table's NSE, so `dem@region` must be taken
      # into a local first.
      dem_regions <- dem@region
      dat <- dat[region %in% dem_regions]
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
    # .checkTimesliceLevel(app, approxim)
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
    # exp <- .disaggregateTimesliceLevel(exp, approxim)
    # mExpTimeslice <- data.table(expp = rep(exp@name, length(approxim$timeslice)), timeslice = approxim$timeslice)
    # obj@parameters[["mExpTimeslice"]] <- .dat2par(obj@parameters[["mExpTimeslice"]], mExpTimeslice)
    # mExpComm <- data.table(expp = exp@name, comm = exp@commodity)
    # obj@parameters[["mExpComm"]] <- .dat2par(obj@parameters[["mExpComm"]], mExpComm)


    ## pExportRowPrice ####
    # scen@modInp@parameters$pExportRowPrice@data
    dat <- data.table(
      expp = exp@name,
      # comm = exp@commodity,
      region = exp@export$region,
      year = exp@export$year,
      timeslice = exp@export$timeslice,
      value = as.numeric(exp@export$price)
    ) |>
      force_cols_classes()
    # A NA region means "all regions the export operates in". The export class
    # has no explicit @region slot yet, so get_region() returns nothing and we
    # fall back to all model regions.
    dat <- .expand_na_region(dat, c(get_region(exp), .model_regions(scen)))
    scen <- update_parameter(scen, "pExportRowPrice", dat)

    ## pExportRowRes ####
    # scen@modInp@parameters$pExportRowRes@data
    # pExportRowRes <- NULL
    # if (exp@reserve != Inf) pExportRowRes <- data.table(expp = exp@name, value = exp@reserve)
    # obj@parameters[["pExportRowRes"]] <- .dat2par(obj@parameters[["pExportRowRes"]], pExportRowRes)
    if (length(exp@reserve) == 1 && !is.na(exp@reserve) && is.finite(exp@reserve)) {
      dat <- data.table(expp = exp@name, value = as.numeric(exp@reserve))
      scen <- update_parameter(scen, "pExportRowRes", dat)
    }

    ## pExportRow ####
    # scen@modInp@parameters$pExportRow@data
    # pExportRow <- .interp_bounds(exp@export, "exp", obj@parameters[["pExportRow"]], approxim, "expp", exp@name)
    # obj@parameters[["pExportRow"]] <- .dat2par(obj@parameters[["pExportRow"]], pExportRow)
    dat <- .pack_bounds_long(exp@export, "exp", c("region", "year", "timeslice"))
    if (!is.null(dat)) {
      dat <- data.table(expp = exp@name, dat) |>
        .force_year_class_df()
      dat <- .expand_na_region(dat, c(get_region(exp), .model_regions(scen)))
      scen <- update_parameter(scen, "pExportRow", dat)
    }


    # mExportRow <- merge0(merge0(mExpTimeslice, list(region = approxim$region)), list(year = approxim$mileStoneYears))
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
    # .checkTimesliceLevel(app, approxim)
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
    # imp <- .disaggregateTimesliceLevel(imp, approxim)
    # mImpTimeslice <- data.table(
    #   imp = rep(imp@name, length(approxim$timeslice)),
    #   timeslice = approxim$timeslice)
    # obj@parameters[["mImpTimeslice"]] <-
    #   .dat2par(obj@parameters[["mImpTimeslice"]], mImpTimeslice)
    # mImpComm <- data.table(imp = imp@name, comm = imp@commodity)
    # obj@parameters[["mImpComm"]] <- .dat2par(obj@parameters[["mImpComm"]], mImpComm)

    # pImportRowPrice <- .interp_numpar(
    #   imp@import, "price",
    #   obj@parameters[["pImportRowPrice"]], approxim, "imp", imp@name
    # )
    # obj@parameters[["pImportRowPrice"]] <- .dat2par(obj@parameters[["pImportRowPrice"]], pImportRowPrice)

    ## pImportRowPrice ####
    # scen@modInp@parameters$pImportRowPrice@data
    dat <- data.table(
      imp = imp@name,
      region = imp@import$region,
      year = imp@import$year,
      timeslice = imp@import$timeslice,
      value = as.numeric(imp@import$price)
    ) |>
      .force_year_class_df()
    # A NA region means "all regions the import operates in". The import class
    # has no explicit @region slot yet, so get_region() returns nothing and we
    # fall back to all model regions.
    dat <- .expand_na_region(dat, c(get_region(imp), .model_regions(scen)))
    scen <- update_parameter(scen, "pImportRowPrice", dat)

    ## pImportRowRes ####
    # scen@modInp@parameters$pImportRowRes@data
    # pImportRowRes <- NULL
    # if (imp@reserve != Inf) pImportRowRes <- data.table(imp = imp@name, value = imp@reserve)
    # obj@parameters[["pImportRowRes"]] <- .dat2par(obj@parameters[["pImportRowRes"]], pImportRowRes)
    if (length(imp@reserve) == 1 && !is.na(imp@reserve) && is.finite(imp@reserve)) {
      dat <- data.table(imp = imp@name, value = as.numeric(imp@reserve))
      scen <- update_parameter(scen, "pImportRowRes", dat)
    }

    ## pImportRow ####
    # scen@modInp@parameters$pImportRow@data
    # pImportRow <- .interp_bounds(
    #   imp@import, "imp",
    #   obj@parameters[["pImportRow"]], approxim, "imp", imp@name
    # )
    # obj@parameters[["pImportRow"]] <- .dat2par(obj@parameters[["pImportRow"]], pImportRow)
    dat <- .pack_bounds_long(imp@import, "imp", c("region", "year", "timeslice"))
    if (!is.null(dat)) {
      dat <- data.table(imp = imp@name, dat) |>
        .force_year_class_df()
      dat <- .expand_na_region(dat, c(get_region(imp), .model_regions(scen)))
      scen <- update_parameter(scen, "pImportRow", dat)
    }

    # mImportRow <- merge0(merge0(mImpTimeslice, list(region = approxim$region)), list(year = approxim$mileStoneYears))
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
    # sup <- .disaggregateTimesliceLevel(sup, approxim)
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
    # mSupTimeslice <- data.table(
    #   sup = rep(sup@name, length(approxim$timeslice)),
    #   timeslice = approxim$timeslice
    # )
    # obj@parameters[["mSupTimeslice"]] <-
    #   .dat2par(obj@parameters[["mSupTimeslice"]], mSupTimeslice)
    # browser()
    # mSupComm <- data.table(sup = sup@name, comm = sup@commodity)
    # obj@parameters[["mSupComm"]] <-
    #   .dat2par(obj@parameters[["mSupComm"]], mSupComm)
    # browser()

    ## pSupCost ####
    # scen@modInp@parameters$pSupCost@data
    # pSupCost <- .interp_numpar(
    #   sup@supply, "cost",
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
      region = sup@supply$region,
      year = sup@supply$year,
      timeslice = sup@supply$timeslice,
      value = as.numeric(sup@supply$cost)
    ) |>
      force_cols_classes()
    # rows with unset cost (NA) carry only availability bounds (e.g. free
    # land/resource supplies) -- no cost data to interpolate
    dat <- dat[!is.na(dat$value), , drop = FALSE]
    if (nrow(dat) > 0) scen <- update_parameter(scen, "pSupCost", dat)

    ## pSupReserve ####
    # scen@modInp@parameters$pSupReserve@data
    # pSupReserve <- .interp_bounds(
    #   sup@reserve, "res", obj@parameters[["pSupReserve"]],
    #   approxim, c("sup", "comm"), c(sup@name, sup@commodity)
    # )
    # obj@parameters[["pSupReserve"]] <-
    #   .dat2par(obj@parameters[["pSupReserve"]], pSupReserve)
    # browser()
    dat <- .pack_bounds_long(sup@reserve, "res", c("region"))
    if (!is.null(dat)) {
      dat <- data.table(sup = sup@name, comm = sup@commodity, dat) |>
        force_cols_classes()
      scen <- update_parameter(scen, "pSupReserve", dat)
    }

    ## pSupAva ####
    # scen@modInp@parameters$pSupAva@data
    # pSupAva <- .interp_bounds(
    #   sup@supply, "ava",
    #   obj@parameters[["pSupAva"]], approxim, c("sup", "comm"),
    #   c(sup@name, sup@commodity)
    # )
    # obj@parameters[["pSupAva"]] <- .dat2par(obj@parameters[["pSupAva"]], pSupAva)
    dat <- .pack_bounds_long(sup@supply, "ava", c("region", "year", "timeslice"))
    if (!is.null(dat)) {
      dat <- data.table(sup = sup@name, comm = sup@commodity, dat) |>
        force_cols_classes()
      scen <- update_parameter(scen, "pSupAva", dat)
    }



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
    # # mSupAva <- merge0(merge0(mSupSpan, list(comm = sup@commodity, year = approxim$mileStoneYears)), mSupTimeslice)
    # mSupAva <- mSupSpan |>
    #   merge0(list(comm = sup@commodity, year = approxim$mileStoneYears)) |>
    #   merge0(mSupTimeslice)

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
    dat <- .pack_bounds_long(sup@weather, "wava", c("weather"))
    if (!is.null(dat)) {
      dat$sup <- sup@name
      dat <- force_cols_classes(dat)
      scen <- update_parameter(scen, "pSupWeather", dat)
    }

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
  # if (length(wth@timeframe) == 0 && length(approxim$calendar@timeslices_in_frame) > 1) {
  #   stop("Slot weather@timeframe is empty, it should have information about timeslice level")
  # }
  if (length(wth@timeframe) == 0) {
    stop("Weather object must have a timeframe.")
  }
  #
  # if (length(wth@timeframe) == 0) {
  #   wth@timeframe <- names(approxim$calendar@timeslices_in_frame)[1]
  # }
  # approxim <- .fix_approximation_list(approxim, lev = wth@timeframe)
  # # region fix
  # if (length(wth@region) != 0) {
  #   approxim$region <- approxim$region[approxim$region %in% wth@region]
  # }
  # wth@region <- approxim$region
  # browser()
  # wth <- .filter_data_in_slots(wth, approxim$region, "region")
  # wth <- .disaggregateTimesliceLevel(wth, approxim)

  ## pWeather ####
  # scen@modInp@parameters$pWeather@data
  dat <- data.table(
    weather = wth@name,
    region = wth@weather$region,
    year = wth@weather$year,
    timeslice = wth@weather$timeslice,
    value = as.numeric(wth@weather$wval)
  ) |>
    force_cols_classes()
  scen <- update_parameter(scen, "pWeather", dat)

  # obj@parameters[["pWeather"]]@defVal <- wth@defVal
  # obj@parameters[["pWeather"]] <- .dat2par(obj@parameters[["pWeather"]], .interp_numpar(
  #   wth@weather, "wval",
  #   obj@parameters[["pWeather"]], approxim, "weather", wth@name
  # ))
  # obj@parameters[["mWeatherTimeslice"]] <- .dat2par(
  #   obj@parameters[["mWeatherTimeslice"]],
  #   data.table(weather = rep(wth@name, length(approxim$timeslice)), timeslice = approxim$timeslice)
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
    for (s in slotNames(obj)) {
      if (s %in% c("name", "timeframe", "commodity", "region", "misc")) {next}
      slot_info <- get_slot_meta(class(obj), s)
      if (is_empty(slot_info)) {next}
      slot_data <- get_lazy_data(obj, s)
      for (p in slot_info) {
        # @vintage rows are keyed by (vintage, cluster), which are not
        # dimSets of any lifespan parameter: resolve to one row per key
        # first, so dropping the keys cannot fabricate duplicate
        # (tech, region) rows -- conflicting keys error instead
        pdat <- if (identical(s, "vintage")) {
          .lifespan_resolve_df(as.data.frame(slot_data), p$colName,
                               obj@name)
        } else slot_data
        dat <- make_data_param(
          scen = scen,
          obj_name = obj@name,
          slot_data = pdat,
          par_meta = p,
          class_col = "tech"
          # par_name = p$name,
          # short_name = p$colName
        )
        scen <- update_parameter(scen, p$name, dat)
      }
    }

    return(scen)
  }
)

# =============================================================================#
## storage ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "storage", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    # Which commodity a `comm`-dimensioned parameter belongs to. A storage now
    # has THREE roles (input / stored / output) instead of one `@commodity`, so
    # this has to pick the right one rather than inject one scalar. Keyed off the
    # parameter name, which already says which side it is about:
    #   pStorageInpEff, pStorageCostInp, pStorageCinp*, pStorageWeatherCinp*
    #   pStorageOutEff, pStorageCostOut, pStorageCout*, pStorageWeatherCout*
    #   everything else (StgEff, CostStore, StartLevel, NCap2Stg) -> the LEVEL
    # For a storage written with a bare `commodity =` all three roles hold the
    # same commodity, so this reproduces the old behaviour exactly.
    .role_comm <- function(pname) {
      pick <- if (grepl("inp", pname, ignore.case = TRUE)) {
        "input"
      } else if (grepl("out", pname, ignore.case = TRUE)) {
        "output"
      } else {
        "storage"
      }
      .get <- function(role) {
        cm <- tryCatch(as.character(methods::slot(obj, role)$comm),
                       error = function(e) character())
        unique(cm[!is.na(cm) & nzchar(cm)])
      }
      cm <- .get(pick)
      if (length(cm)) return(cm)
      # a role naming nothing falls back to one that does, so a partially
      # specified storage still resolves instead of erroring
      for (alt in c("storage", "input", "output")) {
        cm <- .get(alt)
        if (length(cm)) return(cm)
      }
      character()
    }
    for (s in slotNames(obj)) {
      if (s %in% c("name", "timeframe", "commodity", "region")) {next}
      slot_info <- get_slot_meta(class(obj), s)
      if (is_empty(slot_info)) {next}
      slot_data <- get_lazy_data(obj, s)
      for (p in slot_info) {
        # mod@data$utopia_repository@data$STGELC@seff
        #
        # Per-parameter COPY. Assigning back into `slot_data` made the first
        # parameter's commodity stick for every later one bound to the same
        # slot: `@seff` feeds pStorageInpEff, pStorageOutEff and pStorageStgEff,
        # so whichever was processed first stamped `comm` and the other two
        # silently inherited it. Harmless while all three roles held the same
        # commodity -- and wrong the moment they differ, which is the whole
        # point of the roles: an EV storing kWh and selling km had its km-per-kWh
        # `outeff` filed under electricity, where nothing reads it, so the motor
        # ran at the default efficiency of 1.
        pdat0 <- slot_data
        if ("comm" %in% p$dimSets && is.null(pdat0$comm)) {
          .cm <- .role_comm(p$name)
          if (length(.cm)) {
            pdat0 <- pdat0 |> mutate(comm = .cm, .before = 1)
          }
        }
        pdat <- if (identical(s, "vintage")) {
          .lifespan_resolve_df(as.data.frame(pdat0), p$colName,
                               obj@name)
        } else pdat0
        dat <- make_data_param(
          scen = scen,
          obj_name = obj@name,
          slot_data = pdat,
          par_meta = p,
          class_col = "stg"
        )
        scen <- update_parameter(scen, p$name, dat)
      }
    }
    return(scen)
  }
)

# =============================================================================#
## trade ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "trade", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    for (s in slotNames(obj)) {
      if (s %in% c("name", "timeframe", "commodity", "region")) {next}
      slot_info <- get_slot_meta(class(obj), s)
      if (is_empty(slot_info)) {next}
      slot_data <- get_lazy_data(obj, s)
      for (p in slot_info) {
        pdat <- if (identical(s, "vintage")) {
          rs <- .lifespan_resolve_df(as.data.frame(slot_data), p$colName,
                                     obj@name)
          # pTradeOlife has no region dimension: a per-region lifespan on
          # a trade would silently collapse -- refuse it instead
          if (!"region" %in% p$dimSets && any(!is.na(rs$region))) {
            stop("trade '", obj@name, "' declares a per-region `",
                 p$colName, "` in `@vintage`, but `", p$name,
                 "` carries no region dimension; drop the region key",
                 call. = FALSE)
          }
          rs
        } else slot_data
        dat <- make_data_param(
          scen = scen,
          obj_name = obj@name,
          slot_data = pdat,
          par_meta = p,
          class_col = "trade"
        )
        scen <- update_parameter(scen, p$name, dat)
      }
    }
    return(scen)
  }
)

# =============================================================================#
## tax ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "tax", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    for (s in slotNames(obj)) {
      if (s %in% c("name", "timeframe", "commodity", "region")) {next}
      slot_info <- get_slot_meta(class(obj), s)
      if (is_empty(slot_info)) {next}
      slot_data <- get_lazy_data(obj, s)
      # The commodity is carried on the object (@comm), not in the data slot.
      # An empty @region means the tax applies to all regions (kept as NA and
      # expanded when the mTaxCost map is built); a non-empty @region restricts
      # the tax to those regions.
      slot_data <- slot_data |> mutate(comm = obj@comm, .before = 1)
      if (length(obj@region) > 0) {
        slot_data <- slot_data |>
          select(-any_of("region")) |>
          tidyr::crossing(region = obj@region)
      }
      for (p in slot_info) {
        dat <- make_data_param(
          scen = scen,
          obj_name = obj@name,
          slot_data = slot_data,
          par_meta = p,
          class_col = "tax"
        )
        scen <- update_parameter(scen, p$name, dat)
      }
    }
    return(scen)
  }
)

# =============================================================================#
## sub ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "subsidy", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    for (s in slotNames(obj)) {
      if (s %in% c("name", "timeframe", "commodity", "region")) {next}
      slot_info <- get_slot_meta("subsidy", s)
      if (is_empty(slot_info)) {next}
      slot_data <- get_lazy_data(obj, s)
      # The commodity is carried on the object (@comm), not in the data slot.
      # An empty @region means the subsidy applies to all regions (kept as NA
      # and expanded when the mSubCost map is built); a non-empty @region
      # restricts the subsidy to those regions.
      slot_data <- slot_data |> mutate(comm = obj@comm, .before = 1)
      if (length(obj@region) > 0) {
        slot_data <- slot_data |>
          select(-any_of("region")) |>
          tidyr::crossing(region = obj@region)
      }
      for (p in slot_info) {
        dat <- make_data_param(
          scen = scen,
          obj_name = obj@name,
          slot_data = slot_data,
          par_meta = p,
          class_col = "sub"
        )
        scen <- update_parameter(scen, p$name, dat)
      }
    }
    return(scen)
  }
)

# =============================================================================#
## constraint (user-defined) ####
# =============================================================================#
# A user constraint compiles to the solver-agnostic GAMS-string IR
# (`scen@modInp@gams.equation[[name]]`) plus its supporting `pCns*`/`mCns*`
# parameters; the writers (write_glpk / write_jump / write_pyomo / write_gams)
# translate that IR per backend. Codegen reuses the proven `.getSetEquation`
# engine. Unlike the per-object methods above, this runs AFTER the mapping
# pipeline (via `.interp_user_constraints()` in interp.R), because a constraint
# references variable domain maps (e.g. mTechNew) that must already exist.
#
# `approxim` (the engine's set-value/calendar context) is taken from
# `extra_params$approxim` when supplied, else rebuilt here from `scen` -- same
# shape the settings builder uses. (Retiring `approxim` + the engine's legacy
# timeslice-ancestry/*RY timeslice handling in favour of mTimesliceFamily/pTimesliceAgg + a
# per-summand `timeframe` is deferred.)
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "constraint", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    approxim <- extra_params$approxim
    if (is.null(approxim)) approxim <- .constraint_approxim(scen)

    # Upgrade any summand serialized before the `timeframe` slot existed, so
    # `.getSetEquation` can read `@timeframe` without erroring on old models.
    obj@lhs <- lapply(obj@lhs, .upgrade_summand)

    # The engine reads model sets from the legacy `@set` slot; the new pipeline
    # populates `@sets`. Shim a working copy, generate, then drop the shim.
    mi <- scen@modInp
    mi@set <- scen@modInp@sets
    mi <- .getSetEquation(mi, obj, approxim)
    mi@set <- list()
    scen@modInp <- mi
    scen
  }
)

# Set-value / calendar context the constraint IR engine needs, derived directly
# from `scen` (mirrors the `approxim` the settings builder assembles).
.constraint_approxim <- function(scen) {
  ss <- scen@settings
  mid <- as.integer(scen@modInp@sets$year)
  growth <- if (length(mid) > 0) c(diff(mid), 1L) else integer(0)
  names(growth) <- as.character(mid)
  list(
    region = .model_regions(scen),
    # Pruned region hierarchy, so a summand's `geolevel` can be resolved to the
    # regions of that level (the spatial twin of `calendar@timeframes`).
    # NULL when no geoscale is attached.
    geo_hierarchy = .scen_geo_hierarchy(scen),
    year = ss@horizon@period,
    timeslice = scen@modInp@sets$timeslice,
    calendar = ss@calendar,
    solver = NULL,
    mileStoneYears = mid,
    mileStoneForGrowth = growth,
    fullsets = TRUE,
    optimizeRetirement = ss@optimizeRetirement
  )
}

# The unwired ob2mi("horizon") and ob2mi("calendar") methods were archived to
# depreciated/R/ob2mi-horizon-calendar.R (never dispatched; ob2mi("horizon") was
# the sole consumer of the `fn` list in the archived R/fn.R). Horizon/calendar
# parameters are built by ob2mi("settings") below and the `calendar` recipe.

# =============================================================================#
## settings ####
# =============================================================================#


# =============================================================================#
# Add settings ####
# =============================================================================#
setMethod(
  "ob2mi",
  signature(scen = "scenario", obj = "settings", extra_params = "list"),
  function(scen, obj, extra_params = list()) {
    # Settings -> modInp parameters. Relocated verbatim from the legacy
    # `.obj2modInp(modInp, settings, approxim)` method (R/obj2modInp.R) so the
    # interpolation pipeline no longer depends on it. The original variable names
    # are kept so the body stays identical to the legacy source: `app` is the
    # settings object, `obj` the modInp, `approxim` the interpolation context
    # assembled by `.interp_settings_params()` and passed via `extra_params`.
    app      <- obj
    approxim <- extra_params$approxim
    obj      <- scen@modInp

    clean_list <- c(
      "mTimesliceParentChild", "mTimesliceParentChildE", "mTimesliceNext",
      "mTimesliceFYearNext", "pWacc", "pSdr", "pTimesliceShare", "pDummyImportCost",
      "pDummyExportCost",
      "pTimesliceWeight",
      # "mStartMilestone", "mEndMilestone",
      "mMilestoneLast", "mMilestoneFirst", "mMilestoneNext",
      "mMilestoneHasNext", "mSameTimeslice", "mSameRegion", "ordYear",
      "pYearFraction",
      "cardYear", "pPeriodLen", "pDiscountFactor" #, "mDiscountZero"
    )
    for (i in clean_list) {
      obj@parameters[[i]] <- .resetParameter(obj@parameters[[i]])
    }
    obj <- .drop_config_param(obj)
    app <- .filter_data_in_slots(app, approxim$region, "region")
    obj@parameters[["mTimesliceParentChild"]] <- .dat2par(
      obj@parameters[["mTimesliceParentChild"]],
      data.table(
        timeslice = as.character(approxim$calendar@timeslice_ancestry$parent),
        timeslicep = as.character(approxim$calendar@timeslice_ancestry$child),
        stringsAsFactors = FALSE
      )
    )
    obj@parameters[["mTimesliceParentChildE"]] <- .dat2par(
      obj@parameters[["mTimesliceParentChildE"]],
      data.table(
        timeslice = as.character(c(app@calendar@timeslice_share$timeslice,
                               approxim$calendar@timeslice_ancestry$parent)),
        timeslicep = as.character(c(app@calendar@timeslice_share$timeslice,
                                approxim$calendar@timeslice_ancestry$child)),
        stringsAsFactors = FALSE
      )
    )
    # browser()
    if (length(approxim$calendar@next_in_timeframe) != 0) {
      obj@parameters[["mTimesliceNext"]] <-
        .dat2par(obj@parameters[["mTimesliceNext"]],
                 approxim$calendar@next_in_timeframe)
      obj@parameters[["mTimesliceFYearNext"]] <-
        .dat2par(obj@parameters[["mTimesliceFYearNext"]],
                 approxim$calendar@next_in_year)
    }
    # The model's two rates: `wacc` annuitises investment (R/eac.R), `sdr`
    # discounts the objective (pDiscountFactor, below). `@discount` carries both
    # columns on every row -- `.discount_arg_to_rates()` expanded the `discount`
    # shorthand and rejected partial rows -- so neither needs a fallback here.
    # Interpolated over ANNUAL years, not milestones: the pDiscountFactor
    # cumprod below compounds year by year.
    approxim_no_mileStone_Year <- approxim
    approxim_no_mileStone_Year$mileStoneYears <- NULL
    pSdr <- .interp_numpar(app@discount, "sdr",
                           obj@parameters[["pSdr"]], approxim_no_mileStone_Year,
                           all.val = TRUE
    )
    pWacc <- .interp_numpar(app@discount, "wacc",
                            obj@parameters[["pWacc"]], approxim_no_mileStone_Year,
                            all.val = TRUE
    )
    for (.r in list(list("pSdr", pSdr), list("pWacc", pWacc))) {
      obj@parameters[[.r[[1]]]] <-
        .dat2par(obj@parameters[[.r[[1]]]],
                 filter(.r[[2]], year %in% obj@parameters$year@data$year))
    }
    approxim_comm <- approxim
    approxim_comm[["comm"]] <- approxim$all_comm
    obj@parameters[["pTimesliceShare"]] <- .dat2par(
      obj@parameters[["pTimesliceShare"]],
      data.table(
        timeslice = approxim$calendar@timeslice_share$timeslice,
        value = approxim$calendar@timeslice_share$share
      )
    )
    approxim_comm$timeslice <- approxim$calendar@timeslice_share$timeslice

    # browser()
    data.table::setNumericRounding(2) # ignore small differences in 'unique' function
    # add pTimesliceWeight from calendar@misc$pTimesliceWeight o @timeslice_share$weight
    if (!is_null(approxim$calendar@misc$pTimesliceWeight)) {

      pTimesliceWeight_tmp <- data.table(
        year = approxim$calendar@misc$pTimesliceWeight$year,
        timeslice = approxim$calendar@misc$pTimesliceWeight$timeslice,
        value = approxim$calendar@misc$pTimesliceWeight$weight
      )
      if (is_null(pTimesliceWeight_tmp[["value"]])) {
        if (is_null(pTimesliceWeight_tmp[["weight"]])) {
          stop("No timeslice weight in calendar@misc$pTimesliceWeight$value or @timeslice_share$weight")
        }
        pTimesliceWeight_tmp <- rename(pTimesliceWeight_tmp, value = weight)
      }
    } else {
      pTimesliceWeight_tmp <- lapply(approxim$year, function(x) {
        data.table(
          year = x,
          timeslice = approxim$calendar@timeslice_share$timeslice,
          value = approxim$calendar@timeslice_share$weight
        )
      }) |> rbindlist()
    }

    # calculate timeslice weights for "parent" timeframes
    # requirements: weights are given for the lowest level time-timeslices
    # approxim$calendar@timeslice_share |>
    #   select(-weight) |>
    #   left_join(, by = "timeslice")

    a <- pTimesliceWeight_tmp |>
      left_join(approxim$calendar@timeslice_ancestry, by = c("timeslice" = "child")) |>
      left_join(select(approxim$calendar@timeslice_share, -weight),
                by = c("parent" = "timeslice")) |>
      rename(weight = value) |>
      filter(!is.na(parent))

    b <-
      approxim$calendar@timetable |>
      select(1:timeslice) |>
      pivot_longer(cols = -timeslice, names_to = "timeframe",
                   values_to = "parent") |>
      as.data.table() |>
      select(timeframe, parent, timeslice) |>
      arrange(timeframe, parent, timeslice)

    ab <- left_join(a, b, by = c("timeslice", "parent")) |>
      group_by(year, parent) |>
      summarise(value = weighted.mean(weight, w = share)) |>
      rename(timeslice = parent) |>
      as.data.table()

    pTimesliceWeight_tmp <- rbind(pTimesliceWeight_tmp, ab) |>
      filter(year %in% approxim$mileStoneYears) |>
      unique()
    # pTimesliceWeight_tmp$value <- pTimesliceWeight_tmp$value /
    #   (24 * 7 * 52 / 24 / 365)

    obj@parameters[["pTimesliceWeight"]] <- .dat2par(
      obj@parameters[["pTimesliceWeight"]],
      # data.table(
      #   timeslice = approxim$calendar@timeslice_share$timeslice,
      #   value = approxim$calendar@timeslice_share$weight
      # )
      pTimesliceWeight_tmp
    )
    # agg-rewrite: intensive timeslice-aggregation weight pTimesliceAgg[year, parent, child]
    # = pTimesliceWeight[year, child] / pTimesliceWeight[year, parent], over IMMEDIATE
    # parent-child pairs (@timeslice_family). Used to up-aggregate commodity totals
    # between adjacent levels (eqOutTot/eqInpTot), replacing *2Lo disaggregation.
    pTimesliceAgg_tmp <- dplyr::as_tibble(approxim$calendar@timeslice_family) |>
      dplyr::transmute(timeslice = as.character(parent),
                       timeslicep = as.character(child)) |>
      dplyr::left_join(dplyr::rename(pTimesliceWeight_tmp,
                                     timeslicep = timeslice, w_child = value),
                       by = "timeslicep") |>
      dplyr::left_join(dplyr::rename(pTimesliceWeight_tmp, w_parent = value),
                       by = c("year", "timeslice")) |>
      dplyr::filter(!is.na(w_child) & !is.na(w_parent) & w_parent != 0) |>
      dplyr::transmute(year, timeslice, timeslicep, value = w_child / w_parent) |>
      as.data.table()
    obj@parameters[["pTimesliceAgg"]] <- .dat2par(
      obj@parameters[["pTimesliceAgg"]], pTimesliceAgg_tmp)
    rm(pTimesliceAgg_tmp)
    # browser()
    rm(a, b, ab, pTimesliceWeight_tmp)

    if (nrow(app@horizon@intervals) == 0) { # ???
      invisible()  # browser() disabled
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

    obj@parameters[["mSameTimeslice"]] <- .dat2par(
      obj@parameters[["mSameTimeslice"]],
      data.table(timeslice = app@calendar@timeslice_share$timeslice,
                 timeslicep = app@calendar@timeslice_share$timeslice)
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
    # Cumulative discount factor for the objective -- built from the SOCIAL
    # discount rate. The financing rate (`wacc`) never enters here; it is spent
    # in R/eac.R annuitising investment.
    pSdr <- pSdr[sort(pSdr$year, index.return = TRUE)$ix, , drop = FALSE]
    pDiscountFactor <- pSdr[0, , drop = FALSE]
    for (l in unique(pSdr$region)) {
      dd <- pSdr[pSdr$region == l, , drop = FALSE]
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
    scen@modInp <- obj
    scen
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
    "timeslice", "timeslicep",
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

  # The `value` column is always numeric (double): source slots may declare it
  # as integer (e.g. trade `olife = integer()`), which would otherwise clash
  # with the numeric `value` column of the target parameter in `d2p`.
  if (!is.null(dtf[["value"]]) && !inherits(dtf[["value"]], "numeric")) {
    dtf[["value"]] <- as.numeric(dtf[["value"]])
  }

  # The bounds `type` column is a factor (levels lo, up) in memory but round-trips
  # through CSV as plain character; restore the factor so an on-disk parameter is
  # byte-identical to an in-memory one.
  if (!is.null(dtf[["type"]]) && !is.factor(dtf[["type"]])) {
    dtf[["type"]] <- factor(as.character(dtf[["type"]]), levels = c("lo", "up"))
  }

  as.data.table(dtf)
}

# =============================================================================#
# .expand_na_region: materialise wildcard (NA) region rows of a collected
# parameter to explicit regions.
#
# A NA in the `region` column means "applies to every operative region of the
# object". Such rows are expanded to one row per region in `regs` (the object's
# regions, or all model regions when the object declares none). Rows that
# already carry an explicit region win at the same non-region key, so a NA
# wildcard never overrides or duplicates a region the user set explicitly.
# =============================================================================#
.expand_na_region <- function(dat, regs) {
  if (is.null(dat) || !"region" %in% colnames(dat) || nrow(dat) == 0) {
    return(dat)
  }
  na_rows <- is.na(dat$region)
  if (!any(na_rows)) {
    return(dat)
  }
  regs <- unique(as.character(regs))
  regs <- regs[!is.na(regs) & nzchar(regs)]
  if (length(regs) == 0) {
    return(dat)
  }
  was_dt <- data.table::is.data.table(dat)
  dat <- as.data.frame(dat)
  key <- setdiff(colnames(dat), c("region", "value", "type"))
  explicit <- dat[!na_rows, , drop = FALSE]
  wild <- dat[na_rows, setdiff(colnames(dat), "region"), drop = FALSE]
  grid <- dplyr::cross_join(wild, data.frame(region = regs,
                                             stringsAsFactors = FALSE))
  # Explicit region rows win over a wildcard at the same full key.
  if (nrow(explicit) > 0) {
    grid <- dplyr::anti_join(grid, explicit, by = c(key, "region"))
  }
  out <- dplyr::bind_rows(explicit, grid[, colnames(dat), drop = FALSE])
  if (was_dt) data.table::as.data.table(out) else out
}

# =============================================================================#
make_data_param <- function(
    scen,
    obj_name,
    slot_data,
    par_meta,
    class_col = NULL
    # par_name,
    # short_name,
    # class_col
    # scen = scen,
    # obj_name = tech@name,
    # slot_data = slot_data,
    # par_meta = p
  ) {

  # if (par_meta$name == "pTechCap2act") browser()

  if (!inherits(slot_data, "data.frame")) {
    slot_data <- data.frame(value = slot_data)
    short_name <- "value"
  } else {
    short_name <- par_meta$colName
  }

  # A slot data.frame deserialised under an older class version can lack the
  # column this parameter reads -- `@invcost$eac` and `@invcost$payback` are
  # exactly that case for objects built before they existed. `rename()` (numpar)
  # and `pivot_longer()` (bounds) both error on a missing column, but an absent
  # column just means the user set nothing, so return the empty schema.
  .wanted <- if (par_meta$type == "bounds") {
    paste0(par_meta$colName, c(".lo", ".up", ".fx"))
  } else {
    short_name
  }
  if (!any(.wanted %in% names(slot_data))) {
    return(.empty_param_data(scen@modInp@parameters[[par_meta$name]]))
  }

  if (par_meta$type == "bounds") {
    # browser()
    bound_names <- paste0(par_meta$colName, c(".lo", ".up", ".fx"))
    # names(bound_names) <- c("lo", "up", "fx")

    dat <- slot_data |>
      select(any_of(c(
        scen@modInp@parameters[[par_meta$name]]@dimSets,
        bound_names
      )))

    if (!is.null(class_col) && is.null(dat[[class_col]])) {
      dat <- dat |> mutate({{class_col}} := obj_name, .before = 1)
    }

    # dimension columns absent from the slot data (e.g. objects saved under an
    # older class version without a `timeslice` column) are unset -> NA wildcard
    for (m in setdiff(scen@modInp@parameters[[par_meta$name]]@dimSets,
                      names(dat))) {
      dat[[m]] <- rep(NA, nrow(dat))
    }

    # Pack RAW (sparse) bounds in long format with `type` in {lo, up, fx}.
    # Interpolation is deferred to `interpolate_parameters`.
    dat <- dat |>
      pivot_longer(
        cols = any_of(bound_names),
        names_to = "type",
        values_to = "value",
        names_prefix = paste0(par_meta$colName, ".")
      )

    # fixed (fx) bounds -> explicit lo + up rows (see .expand_fx_bounds)
    dat <- .expand_fx_bounds(dat)

    dat <- dat |>
      select(all_of(c(
        scen@modInp@parameters[[par_meta$name]]@dimSets,
        "type", "value"
      ))) |>
      force_cols_classes()

  } else {
  # browser()
    # Pack RAW (sparse) numeric values. Interpolation is deferred to
    # `interpolate_parameters`.
    dat <- slot_data |>
      select(any_of(c(
        scen@modInp@parameters[[par_meta$name]]@dimSets,
        short_name
        ))) |>
      # `all_of()`: renaming from a character variable is a tidyselect
      # "external vector" selection, deprecated since tidyselect 1.1.0.
      rename(value = all_of(short_name)) |>
      filter(!is.na(value)) |>
      unique()

    if (!is.null(class_col) && is.null(dat[[class_col]])) {
      dat <- dat |> mutate({{class_col}} := obj_name, .before = 1)
    }

    # dimension columns absent from the slot data (e.g. objects saved under an
    # older class version without a `timeslice` column) are unset -> NA wildcard
    for (m in setdiff(scen@modInp@parameters[[par_meta$name]]@dimSets,
                      names(dat))) {
      dat[[m]] <- rep(NA, nrow(dat))
    }

    dat <- dat |>
      select(all_of(c(
        scen@modInp@parameters[[par_meta$name]]@dimSets,
        "value"
      ))) |>
      force_cols_classes()
  }

  # !!! add optional or essential info to metadata and filter NAs for optional
  dat <- dat |>
    filter(!is.na(value)) |>
    unique()

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
                dimSets = c("region", "year", "timeslice")
                )
  get_slot_meta(class = "tech", slot = "tech", type = "numpar",
                dimSets = c("region", "year", "timeslice"), return_names = "slot")
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



