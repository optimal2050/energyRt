# New interpolation functions for model objects

interp_mod <- function(mod, name = NULL, desc = NULL,
                       ondisk = TRUE, ..., overwrite = FALSE) {
  # mod - model
  scen <- new("scenario")
  # ... process arguments, add settings, ...
  if (F) {
    # debug
    # library(energyRt)
    devtools::load_all(".")
    (load("tmp/utopia-mod.RData"))
    utopia
    mod <- utopia@model
    scen <- new("scenario")
    scen@name <- "utopia_new_interpolation"
    scen@path <- fp(get_scenarios_path(), scen@name) |> .fix_path()
    slotNames(scen)
    solution_type <- "foresight"

    ondisk <- TRUE

    ECOA <- scen@model@data$utopia_repository@data$ECOA
  }

  # set scenario "in memory" or "on disk" status
  # (!!! model is assumed to be in memory yet -- address later)
  if (ondisk) {
    scen <- mark_ondisk(scen)
  } else {
    scen <- mark_inmemory(scen)
  }
  # isOnDisk(scen)

  # scenario directory ####
  # scen@path
  if (!dir.exists(scen@path)) {
    # add log/message
    dir.create(scen@path, recursive = TRUE)
  }

  mi_path <- file.path(scen@path, "modInp")
  fmp <- function(x) {
    fp(mi_path, x)
  } # shortcut for file path

  if (!dir.exists(mi_path)) {
    # add log/message
    dir.create(mi_path, recursive = TRUE)
  }
  sol_path <- file.path(scen@path, solution_type) # path to solver files
  if (!dir.exists(sol_path)) {
    # add log/message
    dir.create(sol_path, recursive = TRUE)
  }

  # class(mod)
  scen@model <- mod

  # import settings from mod@config
  scen@settings <- .config_to_settings(scen@model@config, scen@settings)

  # !!! ToDo: update settings from arguments
  # ... subset slices and regions
  # ...

  # process model inputs
  mi <- new("modInp")
  if (ondisk) {
    # add log/message
    mi <- mark_ondisk(mi)
    mi_path <- file.path(scen@path, "modInp")
    mi <- setObjPath(mi, mi_path)
  } else {
    # add log/message
    mi <- mark_inMemory(mi)
  }
  scen@modInp <- mi
  rm(mi)

  # set scenario directory and the type of solution (foresight or myopic)
  # scen@path
  if (!dir.exists(scen@path)) {
    # add log/message
    dir.create(scen@path, recursive = TRUE)
  }
  if (!dir.exists(mi_path) && ondisk) {
    # add log/message
    dir.create(mi_path, recursive = TRUE)
  }

  # slotNames(mi)
  # names(mi@parameters)
  # names(mi@set) # !!! rename to "sets"

  sets_from_settings <- c("region", "year", "slice") # from settings

  sets_from_model <- c(
    "comm", "sup", "dem", "tech",
    "group", "stg", "expp", "imp", "trade", "weather"
  )

  all_sets <- c(sets_from_settings, sets_from_model)

  # Sets from settings ####
  scen@modInp@sets$region <- as.character(scen@settings@region) # factors not allowed
  scen@modInp@sets$year <- as.integer(scen@settings@horizon@intervals$mid)
  scen@modInp@sets$slice <- scen@settings@calendar@slice_share$slice

  # Sets from names of declared model-objects ####
  # INFO: if a set element is not declared as individual object, error will be thrown

  ## commodity ####
  scen@modInp@sets$comm <- collect_object_names(mod, "commodity")$name
  scen@modInp@parameters$comm <- d2p(scen@modInp@parameters$comm, scen@modInp@sets$comm, fmp("comm"))

  ## supply ####
  scen@modInp@sets$sup <- collect_object_names(mod, "supply")$name
  scen@modInp@parameters$sup <- d2p(scen@modInp@parameters$sup, scen@modInp@sets$sup, fmp("sup"))

  ## demand ####
  scen@modInp@sets$dem <- collect_object_names(mod, "demand")$name
  scen@modInp@parameters$dem <- d2p(scen@modInp@parameters$dem, scen@modInp@sets$dem, fmp("dem"))

  ## technology ####
  scen@modInp@sets$tech <- collect_object_names(mod, "technology")$name
  scen@modInp@parameters$tech <- d2p(scen@modInp@parameters$tech, scen@modInp@sets$tech, fmp("tech"))

  ## storage ####
  scen@modInp@sets$stg <- collect_object_names(mod, "storage")$name
  scen@modInp@parameters$stg <- d2p(scen@modInp@parameters$stg, scen@modInp@sets$stg, fmp("stg"))

  ## export ####
  scen@modInp@sets$expp <- collect_object_names(mod, "export")$name
  scen@modInp@parameters$expp <- d2p(scen@modInp@parameters$expp, scen@modInp@sets$expp, fmp("expp"))

  ## import ####
  scen@modInp@sets$imp <- collect_object_names(mod, "import")$name
  scen@modInp@parameters$imp <- d2p(scen@modInp@parameters$imp, scen@modInp@sets$imp, fmp("imp"))

  ## trade ####
  scen@modInp@sets$trade <- collect_object_names(mod, "trade")$name
  scen@modInp@parameters$trade <- d2p(scen@modInp@parameters$trade, scen@modInp@sets$trade, fmp("trade"))

  ## weather ####
  scen@modInp@sets$weather <- collect_object_names(mod, "weather")$name
  scen@modInp@parameters$weather <-
    d2p(scen@modInp@parameters$weather, scen@modInp@sets$weather, fmp("weather"))

  # Other sets (not from model objects) ####
  ## group - groups of commodities
  scen@modInp@sets$group <- collect_set_elements(mod, "group")
  scen@modInp@parameters$group <- d2p(scen@modInp@parameters$group, scen@modInp@sets$group, fmp("group"))

  ## process ####
  scen@modInp@sets$process <- c(
    # combined from all process classes
    scen@modInp@sets$sup,
    scen@modInp@sets$dem,
    scen@modInp@sets$tech,
    scen@modInp@sets$stg,
    scen@modInp@sets$expp,
    scen@modInp@sets$imp,
    scen@modInp@sets$trade
  )

  # assemble summary sets to use in interpolation
  ## process class
  scen@modInp@sets$process_class <- get_process_class(scen)

  ## (keep for future use)
  process_class <- named_list_to_df(scen@modInp@sets$process_class,
                                    col_names = c("process", "class")) |>
    arrange(class, process) |> as.data.table()

  # !!! ToDo: adjust for subsets of regions and slices
  scen@modInp@sets[["comm_timeframe"]] <- map_comm_timeframe(scen)
  scen@modInp@sets[["process_timeframe"]] <-
    get_process_timeframe(scen, comm_timeframe = scen@modInp@sets$comm_timeframe)

  scen@modInp@sets[["process_inputs"]] <- get_process_inputs(scen)
  scen@modInp@sets[["process_outputs"]] <- get_process_outputs(scen)
  scen@modInp@sets[["process_aux"]] <- get_process_aux(scen)
  scen@modInp@sets[["process_comm"]] <- get_process_comm(scen)

  # Maping ####
  ## mSupComm ####
  # supply commodities
  scen@modInp@sets[["supply_comm"]] <- get_process_outputs(scen, classes = "supply")
  mSupComm <- named_list_to_df(scen@modInp@sets$supply_comm,
    col_names = c("sup", "comm")
  )
  scen@modInp@parameters[["mSupComm"]] <-
    d2p(scen@modInp@parameters[["mSupComm"]], mSupComm, fmp("mSupComm"))

  ## mImpComm ####
  # import commodities
  scen@modInp@sets[["import_comm"]] <-
    get_process_outputs(scen, classes = "import")
  mImpComm <- named_list_to_df(scen@modInp@sets[["import_comm"]],
                               col_names = c("imp", "comm"))
  scen@modInp@parameters[["mImpComm"]] <-
    d2p(scen@modInp@parameters[["mImpComm"]], mImpComm, fmp("mImpComm"))

  ## mDemComm ####
  # demand commodities
  scen@modInp@sets[["demand_comm"]] <-
    get_process_inputs(scen, classes = "demand")
  mDemComm <- named_list_to_df(scen@modInp@sets[["demand_comm"]],
                               col_names = c("dem", "comm"))
  scen@modInp@parameters[["mDemComm"]] <-
    d2p(scen@modInp@parameters[["mDemComm"]], mDemComm, fmp("mDemComm"))

  ## mExpComm ####
  # export commodities
  scen@modInp@sets[["export_comm"]] <-
    get_process_inputs(scen, classes = "export")
  mExpComm <- named_list_to_df(scen@modInp@sets[["export_comm"]],
                               col_names = c("expp", "comm"))
  scen@modInp@parameters[["mExpComm"]] <-
    d2p(scen@modInp@parameters[["mExpComm"]], mExpComm, fmp("mExpComm"))

  ## mTradeComm ####
  # interregional trade commodities
  scen@modInp@sets[["trade_comm"]] <- get_process_inputs(scen, classes = "trade")
  mTradeComm <- named_list_to_df(scen@modInp@sets[["trade_comm"]],
                                 col_names = c("trade", "comm"))
  scen@modInp@parameters[["mTradeComm"]] <-
    d2p(scen@modInp@parameters[["mTradeComm"]], mTradeComm, fmp("mTradeComm"))

  ## mStorageComm ####
  # storage commodities
  scen@modInp@sets$storage_comm <- get_process_inputs(scen, classes = "storage")
  mStorageComm <- named_list_to_df(scen@modInp@sets$storage_comm,
    col_names = c("stg", "comm")
  )
  scen@modInp@parameters[["mStorageComm"]] <-
    d2p(scen@modInp@parameters[["mStorageComm"]], mStorageComm, fmp("mStorageComm"))

  ## mTechInpComm ####
  # technology input commodities
  scen@modInp@sets$tech_input_comm <-
    get_process_inputs(scen, classes = "technology")
  mTechInpComm <- named_list_to_df(scen@modInp@sets$tech_input_comm,
    col_names = c("tech", "comm")
  )
  scen@modInp@parameters[["mTechInpComm"]] <-
    d2p(scen@modInp@parameters[["mTechInpComm"]], mTechInpComm, fmp("mTechInpComm"))

  ## mTechOutComm ####
  # technology output commodities
  scen@modInp@sets$tech_output_comm <- get_process_outputs(scen, classes = "technology")
  mTechOutComm <- named_list_to_df(scen@modInp@sets$tech_output_comm,
    col_names = c("tech", "comm")
  )
  scen@modInp@parameters[["mTechOutComm"]] <-
    d2p(scen@modInp@parameters[["mTechOutComm"]], mTechOutComm, fmp("mTechOutComm"))

  ## mProcessComm ####
  # map processes to commodities, both inputs and outputs
  # Note: does not include auxiliary commodities
  # Note: main activity commodities cannot be inputs and outputs at the same time.
  # !!! ToDo: add this mapping for futher use

  ## mProcessAuxComm ####
  # map auxiliary commodities to processes
  # Note: auxiliary commodities can be both inputs and outputs at the same time.
  # !!! ToDo: add this mapping for further use

  ## ToDo: mProcessRegion ####
  # map processes to regions !!! add mapping parameter?
  scen@modInp@sets$process_region <- get_process_region(scen)

  ## mCommReg ####
  # map commodities to regions

  ### Primary supply and import commodities ####
  primary_comm_region <- named_list_to_df(
    scen@modInp@sets$import_comm,
    col_names = c("process", "comm")
  ) |>
    rbind(
      named_list_to_df(scen@modInp@sets$supply_comm, col_names = c("process", "comm"))
    ) |>
    left_join(
      named_list_to_df(scen@modInp@sets$process_region,
        col_names = c("process", "region")
      ),
      by = "process"
    ) |>
    select(comm, region) |>
    unique()

  scen@modInp@sets$primary_comm_region <-
    split(primary_comm_region$region, primary_comm_region$comm)

  comm_region <- primary_comm_region

  # check if primary commodity can be traded and shipped to other regions
  traded_primary_comm_region <- primary_comm_region |>
    right_join(mTradeComm, by = "comm") |>
    filter(!is.na(region))

  if (nrow(traded_primary_comm_region) > 0) {
    # !!! finish: add regions to commodity
    browser()

    comm_region <- traded_primary_comm_region |>
      select(comm, region) |>
      rbind(comm_region) |>
      unique()
  }

  ### Secondary (processed) commodities' availability in regions (incl. trade) ####
  # (ignoring availability of inputs for now)
  secondary_comm_region <- scen@modInp@sets$process_inputs |>
    named_list_to_df(col_names = c("process", "input")) |>
    left_join(
      named_list_to_df(scen@modInp@sets$process_region,
        col_names = c("process", "region")
      ),
      by = "process"
    ) |>
    left_join(
      named_list_to_df(scen@modInp@sets$process_outputs,
        col_names = c("process", "output")
      ),
      by = "process"
    ) |>
    # left_join(primary_comm_region,
    #           by = c("input" = "comm", "region" = "region")) |>
    # select(input, output, region) |> # reserved for filtering of inputs
    select(output, region) |>
    filter(!is.na(output)) |>
    rename(comm = output) |>
    unique()

  # !!! Additional check/filter of comm-outputs availability via comm-inputs
  # availability in regions can be done here (see commented code above).
  # Caveat: chains of processes must be taken into account.

  scen@modInp@sets$secondary_comm_region <-
    split(secondary_comm_region$region, secondary_comm_region$comm)

  comm_region <- secondary_comm_region |>
    rbind(comm_region) |>
    unique()

  ### Auxiliary commodities ####
  aux_comm_region <- scen@modInp@sets$process_aux |>
    named_list_to_df(col_names = c("process", "aux")) |>
    left_join(
      named_list_to_df(scen@modInp@sets$process_region,
        col_names = c("process", "region")
      ),
      by = "process", relationship = "many-to-many"
    ) |>
    select(aux, region) |>
    rename(comm = aux) |>
    unique()

  # !!! in/out aux check can be added here for additional filtering
  comm_region <- aux_comm_region |>
    rbind(comm_region) |>
    unique()

  ### Emission commodities ####
  emiss_comm <- apply_to_scenario(
    scen = scen,
    classes = "commodity",
    func = function(x) {
      ll <- list()
      ll[[x@name]] <- x@emis$comm
      return(ll)
    }
  ) |>
    named_list_to_df(col_names = c("comm", "emission"))

  if (nrow(emiss_comm) > 0) {
    emiss_comm <- emiss_comm |>
      left_join(comm_region, by = c("comm")) |>
      select(emission, region) |>
      unique() |>
      rename(comm = emission)

    comm_region <- rbind(comm_region, emiss_comm) |> unique()
  }

  ### Demand commodities ####
  demand_comm_region <- mDemComm |>
    left_join(
      named_list_to_df(scen@modInp@sets$process_region,
        col_names = c("process", "region")
      ),
      by = c("dem" = "process")
    ) |>
    select(comm, region) |>
    unique()

  ## check if demand commodities are available in regions
  comm_region_dem_check <-
    comm_region |>
    filter(comm %in% unique(demand_comm_region$comm))
    # filter(region != "R1")

  comm_region_dem_check <- anti_join(
    demand_comm_region,
    comm_region_dem_check,
    by = c("comm", "region")
  ) |>
    unique()

  if (nrow(comm_region_dem_check) > 0) {
    # !!! finish: add dummy import
    # add log/message
    stop(
      "There is no supply, production, interregional trade, or import for demand-commodities in regions:\n   ",
      paste(capture.output(print(comm_region_dem_check)), collapse = "\n   "),
      "\nThe model will be infeasible.\n"
      # print()
    )
  }

  comm_region <- rbind(comm_region, demand_comm_region) |>
    unique() |>
    arrange(comm, region)

  ### Dummy import commodities ####

  ### Final check of comm_region ####
  # check if all commodities in comm_region are declared
  comm_region_check <- comm_region |>
    filter(!(comm %in% scen@modInp@sets$comm))

  if (nrow(comm_region_check) > 0) {
    # add log/message
    stop(
      "The following commodities are not declared in the model:\n   ",
      paste(capture.output(print(comm_region_check)), collapse = "\n   "),
      "\nUse `newCommodity()` to create commodity objects to add to the model.\n"
    )
  }

  ii <- scen@modInp@sets$comm %in% unique(comm_region$comm)
  # scen@modInp@sets$comm[!ii]
  if (any(!ii)) {
    warning(
      "The following commodities are not associated with any process:\n   ",
      paste(scen@modInp@sets$comm[!ii], collapse = ", "),
      "\nand will be ignored.\n"
    )
  }
  rm(ii)

  scen@modInp@parameters$mCommReg <-
    d2p(scen@modInp@parameters$mCommReg, comm_region, fmp("mCommReg"))
  scen@modInp@sets$comm_region <- split(comm_region$region, comm_region$comm)

  ## mWeatherRegion ####
  # map weather to regions !!! ToDo: finish
  # scen@modInp@sets$weather_region <- get_weather_region(scen)

  ## mWeatherTimeframe ####

  # Investment and stock windows ####
  scen@modInp@sets$process_invest_window <- get_process_invest_window(scen)
  scen@modInp@sets$process_invest_year <- get_process_invest_years(scen)

  scen@modInp@sets$process_stock_window <- get_process_stock_window(scen)
  scen@modInp@sets$process_stock_year <- get_process_stock_years(scen)

  ## mProcessYear (!!! ToDo: add) ####
  ### process availability over years by region
  scen@modInp@sets$process_year <- get_process_years(scen)

  # scen@modInp@sets$process_year <- rbind(
  #   scen@modInp@sets$process_invest_year,
  #   scen@modInp@sets$process_stock_year
  # ) |>
  #   unique() |>
  #   arrange(process, year) |>
  #   as.data.table()

  # Log/message meta-data of the scenario ####
  # !!! ToDo: log/message
  # number of regions, commodities, processes, years, slices

  #============================================================================#
  # Parameters from model objects ####
  #   Collect raw (uninterpolated) parameters from the model's objects
  #   by applying ob2mi method to each model object

  # classes <- "commodity"
  # classes <- "demand"
  # classes <- c("export", "import")
  # classes <- "supply"
  # classes <- "weather"
  classes <- "technology"
  for (i in seq(along = scen@model@data)) {
    for (j in seq(along = scen@model@data[[i]]@data)) {
      if (is.null(classes) || inherits(scen@model@data[[i]]@data[[j]], classes)) {
        # browser()
        cat(scen@model@data[[i]]@data[[j]]@name, "\n" )
        scen <- ob2mi(scen, scen@model@data[[i]]@data[[j]], list())
      }
    }
  }

  #============================================================================#
  # Fill NAs in sets ####
  #


  #============================================================================#
  # Interpolate parameters ####
  #

  #============================================================================#
  # Make mapping-sets ####
  #

  ## mvDemInp ####

  ## mExpSlice ####

  ## mExportRow ####

  ## mExportRowUp ####

  ## meqExportRowLo ####

  ## mExportRowCumUp ####

  ## mExportRowCumLo ####

  ## mImpSlice ####

  ## mImportRow ####

  ## mImportRowUp ####

  ## mImportRowLo ####

  ## meqImportRowLo ####

  ## mImportRowCumUp ####

  ## mSupSpan ####

  ## mSupSlice ####

  ## mSupAva ####

  ## mvSupReserve ####

  ## mSupReserveUp ####

  ## meqSupReserveLo ####

  ## pSupReserve ####

  ## mvSupCost ####

  ## mWeatherSlice ####

  ## mWeatherRegion ####


  ## mTechSlice ####





  #============================================================================#
  # Return scenario object ####


  # process objects one-by-one, applying ob2mi method
  # saving parameters to the modInp object or directory
  # cmd <- utopia@model@data$utopia_repository@data$COA
  # ondisk

  # extra_params <- list()
  # extra_params$comm_timeframe <- .get_commodity_timeframe(scen)

  # commodity_slice_map_obj <- .get_map_commodity_slice_map_obj(scen@model)
}

if (F) {
  # debug
  (utopia_on_disk <- load_scenario("utopia_on_disk",
    path = fp(get_scenarios_path(), "utopia_on_disk"),
    env = NULL, overwrite = T
  ))

  # utopia_on_disk <- utopia
  # utopia_on_disk@path <- fp(get_scenarios_path(), "utopia_on_disk")
  # utopia_on_disk@name <- "utopia_on_disk"
  # utopia_on_disk <- save_scenario(utopia_on_disk)
  utopia_on_disk@modInp@parameters$pEmissionFactor
  utopia_on_disk@modInp@parameters$pEmissionFactor@misc$onDisk
  utopia_on_disk@modInp@parameters$pEmissionFactor@data

  utopia_on_disk@modInp@parameters$comm@data
}


interp_slot <- function(obj, slot, overrides) {




}



#' Search through model data for set elements
#'
#' @param obj model or another S4 object
#' @param set_name name of the set to search for
#'
#' @returns a character vector of set elements
#' @export
#'
#' @examples
collect_set_elements <- function(obj, set_name) {
  # browser()

  set_elements <- list()
  if (isS4(obj)) {
    slots <- slotNames(obj)
    ii <- slots == set_name
    if (any(ii) &&
      inherits(
        slot(obj, set_name),
        c("character", "factor", "integer", "numeric", "logical")
      )) {
      set_elements <- c(set_elements, slot(obj, set_name))
      slots <- slots[!ii]
    }
    ll <- lapply(slots, function(x) {
      if (inherits(slot(obj, x), "data.frame")) {
        return(slot(obj, x)[[set_name]])
      } else if (inherits(slot(obj, x), "list")) {
        collect_set_elements(slot(obj, x), set_name)
      } else {
        return(NULL)
      }
    })
    set_elements <- c(set_elements, ll) |>
      unlist() |>
      unique() |>
      sort()
  } else if (inherits(obj, "data.frame")) {
    if (set_name %in% names(obj)) {
      set_elements <- unique(obj[[set_name]])
    }
  } else if (inherits(obj, "list")) {
    ll <- lapply(obj, function(x) {
      collect_set_elements(x, set_name)
    })
    set_elements <- c(set_elements, ll) |>
      unlist() |>
      unique() |>
      sort()
  }

  set_elements <- set_elements |>
    unlist() |>
    unique() |>
    sort()

  return(set_elements)
}

collect_object_names <- function(
    obj,
    classes = c(
      "process", "technology", "storage", "trade",
      "export", "import",
      "demand", "supply", "commodity", "weather"
    )) {
  # browser()
  # obj - model or another S4 object
  # classes - character vector of class names to search for
  # returns a character vector of process names

  process_names <- list(data.frame(
    name = character(),
    desc = character(),
    class = character()
  ))
  if (isS4(obj)) {
    slots <- slotNames(obj)
    if (.hasSlot(obj, "name")) {
      process_names <- c(process_names, list(data.frame(
        name = obj@name,
        desc = if_else(.hasSlot(obj, "desc"), obj@desc, NA_character_),
        class = class(obj)
      )))
    }
    ii <- sapply(slots, function(x) {
      inherits(slot(obj, x), "list")
    })
    obj <- lapply(slots[ii], function(x) {
      slot(obj, x)
    })
  }
  if (inherits(obj, "list")) {
    for (i in seq_along(obj)) {
      if (isS4(obj[[i]]) && .hasSlot(obj[[i]], "name")) {
        process_names <- c(process_names, list(data.frame(
          name = obj[[i]]@name,
          desc = if_else(.hasSlot(obj[[i]], "desc"), obj[[i]]@desc, NA_character_),
          class = class(obj[[i]])
        )))
      } else if (inherits(obj[[i]], "list")) {
        ll <- lapply(obj[[i]], function(x) {
          collect_object_names(x, classes)
        })
        process_names <- c(process_names, ll)
      }
    }
  }
  process_names <- process_names |>
    rbindlist() |>
    unique() |>
    arrange(class, name) |>
    as.data.table()

  if (!is.null(classes)) {
    process_names <- process_names[process_names$class %in% classes, ]
  }

  return(process_names)
}


if (F) {
  (load("tmp/utopia-mod.RData"))
  class(utopia)
  yr <- collect_set_elements(utopia, "year")
  rg <- collect_set_elements(utopia, "region")
  sl <- collect_set_elements(utopia, "slice")
  nm <- collect_set_elements(utopia, "name")
  wr <- collect_set_elements(utopia, "weather")

  pr <- collect_object_names(utopia)
  collect_object_names(utopia@model, classes = NULL)

  class(utopia)
  utopia@modInp@set

  scen
  obj <- utopia@model

  getObjPath(scen)
  scen@inMemory
}

#' Apply function to scenario data
#'
#' @param scen scenario object
#' @param func function to apply to every object of class `classes`
#' in the scenario's model data
#' @param ...
#' @param classes character vector of class names to apply the function to
#' @param return_list logical, if TRUE, return a list of results, otherwise
#' return a vector of results
#'
#' @returns
#' @export
apply_to_scenario <- function(scen, func, ...,
                              classes = NULL,
                              as_list = TRUE) {
  # !!! Consider renaming to apply_to_model_data and rewriting as a method
  rs <- list()
  for (i in seq(along = scen@model@data)) {
    for (j in seq(along = scen@model@data[[i]]@data)) {
      if (is.null(classes) || inherits(scen@model@data[[i]]@data[[j]], classes)) {
        # browser()
        rr <- func(scen@model@data[[i]]@data[[j]], ...)
        rs <- c(rs, rr)
      }
    }
  }

  # return as named list
  if (as_list) {
    return(rs)
  }

  # browser()
  dd <- try(rbindlist(rs, use.names = TRUE, fill = TRUE), silent = TRUE)
  if (inherits(dd, "try-error")) {
    # add log/message
    stop("Function returns inconsistent results. Cannot merge into data.frame.\n", print(func))
  }
  return(dd)
}


# en_rapply <- function(object, f, classes = "ANY", ...) {
#
# }


.interp_slot <- function(
    x,
    keys = c(
      "region", "slice", "comm", "acomm", "tech", "process",
      "vintage",
      "weather", "stg", "sub", "dst", "src"
    ),
    year_seq = NULL,
    val = "value",
    int_rule = "inter",
    yleft = 0, yright = 0) {
  # browser()
  if (is.null(year_seq)) year_seq <- full_seq(x$year, 1)
  .rule <- c(1, 1)
  if (!grepl("int", int_rule, ignore.case = T)) stop("Not implemented")
  if (grepl("for", int_rule, ignore.case = T)) .rule[2] <- 2
  if (grepl("back", int_rule, ignore.case = T)) .rule[1] <- 2

  x <- x |>
    group_by(across(any_of(keys))) |>
    complete(year = year_seq) |>
    mutate(
      # needs review
      .NNN := zoo::na.approx(.data[[val]],
        x = year, rule = .rule,
        yleft = yleft, yright = yright
      )
    ) |>
    # as.data.table() |>
    ungroup()
  x[[val]] <- x[[".NNN"]]
  x[[".NNN"]] <- NULL
  return(x)
}

# superseded
.complete_set <- function(x,
                         set_name, full_set,
                         par_name = NULL,
                         par_dims = NULL
                         ) {
  if (is.null(all_sets)) {
    # all_sets <- c("region", "year", "slice")
    all_sets <- c("region", "year", "slice", "comm", "tech", "process",
                  "vintage", "weather", "stg", "sub", "dst", "src",
                  "acomm", "sup", "dem", "expp", "imp", "trade")
  }
  browser()
  col_ord <- colnames(x)

  if (!is.null(par_name)) {
    x <- filter(x, !is.na(.data[[par_name]]))
  }

  # split set_name to NAs and non-NAs
  x_na <- x[is.na(x[[set_name]]), ]

  # complete set with NAs
  group_cols <- c(
    par_dims[!(par_dims %in% set_name)],
    par_name # make sure the value is
  )
  full_set_na <- full_set[!(full_set %in% x[[set_name]])]


  if (nrow(x_na) > 0) {
    x_na <- x_na |>
      # filter(is.na(.data[[set_name]])) |>
      group_by(across(any_of(group_cols))) |>
      rename(.NNN = !!set_name) |>
      complete(.NNN = full_set_na) |>
      rename(!!set_name := .NNN) |>
      ungroup() |>
      filter(!is.na(.data[[set_name]])) |>
      select(col_ord) |>
      as.data.table()

    x <- x |>
      filter(!is.na(.data[[set_name]])) |>
      select(col_ord) |>
      rbind(x_na) |>
      unique() |>
      arrange(across(any_of(c("region", "year", "slice")))) |>
      as.data.table()
  }

  if (set_name == "year") {
    # add missing milestone-years even if there is no NA values in year column
    x <- x |>
      group_by(across(any_of(group_cols))) |>
      complete(year = full_set) |>
      ungroup() |>
      arrange(across(any_of(c("region", "year", "slice")))) |>
      as.data.table()
  }

  # Add filtration for process_year if no NAs in region & year
  # process_year <- scen@modInp@sets$process_year |>
  #   filter(process == proc_name) |>
  #   select(region, year) |>
  #   unique()
  #
  # filter out years and regions not in process_year
  # x <- x |>
  #   dplyr::semi_join(process_year, by = c("region", "year"))

  return(x)
}

#' Complete a data frame with missing set elements
#'
#' @description
#' Replaces `NA` values in a data frame column `set_name` with missing values
#' from `full_set` for each unique combination of other columns.
#'
#' @param x data frame with columns of sets and parameters
#' @param set_name name of the set to complete
#' @param full_set character vector, named list, or data frame with all possible
#' combinations of the `set_name` elements and other columns.
#' If character vector, it is converted to a data frame with one column.
#' If names list, the `set_name` is taken from the names of the list.
#' If data frame, all columns matching `x` are considered as a complete set.
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
complete_set <- function(
    x, # data.frame
    set_name, # name of the set to complete
    full_set, # full set of elements
    ...
    ) {

  # check if set_name is in x

  # check if any NAs in set_name (return if no NAs)

  # match x-columns with full_set
  # keep {full_set} column and those in full_set that are in x
  # filter out rows in full_set where set_name is not NA

  # group by all columns in x except set_name

  # complete set_name with full_set

  # repeat for other sets in x with NA values

  # filter out rows where completed sets of x are not in full_set

  # return x

}

if (F) {
  # debug
  EHYD <- scen@model@data$utopia_repository@data$EHYD
  x <- EHYD@capacity
  x$region <- NA

  ECOA <- scen@model@data$utopia_repository@data$ECOA
  proc_name <- ECOA@name
  x <- ECOA@capacity |>
    filter(region %in% c("R1", "R2", "R3", "R7")) |>
    select(region, year, stock)

  ii <- x$region %in% c("R7")
  x$region[ii] <- NA
  x
  x$stock[1] <- NA
  x


  x
  x <- complete_set(x,
                    set_name = "region",
                    full_set = scen@modInp@sets$process_region[[proc_name]],
                    par_name = "stock",
                    par_dims = c("region", "year"))
  x |>
    complete_set("year",
                  full_set = scen@modInp@sets$year,
                  par_name = NULL,
                  par_dims = c("region", "year"))



}

# Info ####
## Interpolation of NA values in parameter values ####
# done for a particular value column between years only for each unique
# combination of other columns.
# If all values are NA, then the value is set to default
# for each parameter. If the value is not NA for every combination of
# sets (excluding year), then the value is interpolated for the missing
# years using the interpolation rule for this parameter. NA values beyond
# the interpolation interval (if any) are set to the default value for the
# parameter.

## Expansion of NA values in columns with set elements ####
# done for a set column where the value is NA. The missing values are
# replaced with the full set of elements for each unique combination of
# other columns.

expand_process_years <- function(dat, all_years, value_col = NULL) {
  browser()
  if (F) {
    dat <- ECOA@capacity |> select(region, year, stock)
    all_years <- get_process_years(scen, process = ECOA@name)

    # get_interpolation_rule(scen, sname = "stock", class = class(ECOA),
    #                        one_value = TRUE)

  }

  dat_cols <- names(dat)

  full_join(all_years, dat,
    by = intersect(names(dat), names(all_years))
  ) |>
    select(all_of(dat_cols)) |>
    unique()

}


expand_na_set <- function(
    x, # data.frame
    set_name, # name of the set to expand
    full_set # full set of elements
) {

}

get_parameter_full_sets <- function(
    scen, # scenario object
    param_name # name of the parameter
) {
  # returns data frame with all possible combinations of sets for the parameter,
  # considering the parameter's timeframe, region, years.
  #


}

"ANY"
"ANYREGION"
"ANYYEAR"
"ANYSLICE"
"ANYVINTAGE"

#' Expand sets for parameter
#'
#' @description Expand NA sets in a data frame where parameter is not NA
#' @param x data frame with columns for sets and parameters
#' @param param name of the parameter to expand sets for
#' @param full_sets list of full sets to expand
#' @param filter_sets list of sets with subset elements to filter
#' @param ... additional arguments
#' @returns data frame with expanded sets for the `param` parameter.
#' The parameter value(s) are repeated for each NA element of the
#' combination of sets.
#' @export
expand_sets <- function(x,
                        param,
                        process_name,
                        full_sets,
                        filter_sets = c("region", "year", "slice"),
                        ...) {
  if (F) {
    # debug
    ECOA <- scen@model@data$utopia_repository@data$ECOA
    # x <- ECOA@ceff; param <- "cinp2use"
    x <- ECOA@capacity; param <- "stock"
    process_name <- "ECOA"
    full_sets <- scen@modInp@sets
    # filter_sets:
    # region: create from ECOA@region,
    # year: create from lifespan of ECOA
    # slice: create from ECOA@timeframe
  }

  stopifnot(is.character(param))
  stopifnot(length(param) == 1)
  stopifnot(param %in% names(x))

  x <- x |>
    select(any_of(c(filter_sets, param))) |>
    filter(!is.na(.data[[param]])) |>
    as.data.table()

  # expand process' years
  if ("year" %in% names(x) && any(is.na(x$year))) {
    pr_years <- full_sets$process_year |>
      filter(process == process_name)
      # select(year, year_full) |>
      # unique()

    # x_na <- |>
    x |> filter(is.na(year))


  }


  # expand process' regions
  if ("region" %in% names(x) && any(is.na(x$region))) {
    # x_na <- |>
      x |>
      filter(is.na(region)) |>
      select(-region) |>
      # full_sets$process_region[[]]


      left_join(
        named_list_to_df(, col_names = c("region", "region_full")),
        by = "region"
      ) |>
      select(-region) |>
      rename(region = region_full)
  }






  # filter out

  # identify sets for the parameter

  # check if there are NA elements in the parameter
}

#' Operational timeframe of a commodities
#'
#' @param scen scenario object
#' @param comm character vector of commodity names, if not provided,
#' all commodities retrieved from the scenario object using the
#' `collect_set_names` function.
#'
#' @returns
#' @export
#'
#' @examples
map_comm_timeframe <- function(scen, comm = NULL) {
  apply_to_scenario(
    scen = scen,
    classes = "commodity",
    func = function(x) {
      # list(name = x@name, value = x@timeframe)
      ll <- list()
      ll[x@name] <- x@timeframe
      return(ll)
    }
  )
}

#' Get operational timeframe of processes
#'
#' @param scen scenario object
#' @param process character vector of process names, if not provided,
#' all processes retrieved from the scenario object
#' @param comm_timeframe character vector of commodity timeframes, if not provided,
#' will be retrieved from the scenario object
#'
#' @returns
#' @export
#'
#' @examples
get_process_timeframe <- function(scen, process = NULL,
                                  comm_timeframe = NULL) {
  # browser()
  # collect assigned timeframes for all processes in the scenario
  ll <- apply_to_scenario(
    scen = scen,
    classes = c(
      "process", "technology", "storage"
      # "trade",
      # "export", "import", "demand", "supply"
    ),
    func = function(x) {
      # ll <- list(name = x@name, value = character())
      ll <- list()
      ll[[x@name]] <- character()
      if (.hasSlot(x, "timeframe")) {
        # ll$value <- x@timeframe
        ll[[x@name]] <- x@timeframe
        # } else {
        #   ll$value <- character()
      }
      ll
    }
  )
  # ll
  # collect all commodities for each process w/o timeframe
  ii <- sapply(ll, function(x) {
    is_empty(x) || is.na(x) || x == ""
  })
  mm <- ll[ii]
  ll <- ll[!ii]
  rm(ii)

  # replace empty/na values with commodity timeframes
  if (is.null(comm_timeframe)) {
    comm_timeframe <- map_comm_timeframe(scen)
  }

  # create a table of processes, commodities, timeframes
  process_comm <- get_process_comm(scen) |>
    named_list_to_df(col_names = c("process", "comm"))

  process_comm_timeframe <- process_comm |>
    left_join(
      named_list_to_df(ll, col_names = c("process", "process_timeframe")),
      by = "process"
    ) |>
    left_join(
      named_list_to_df(comm_timeframe, col_names = c("comm", "comm_timeframe")),
      by = "comm"
    )

  # assign timeframe to processes based on commodity timeframes
  process_comm_timeframe <- process_comm_timeframe |>
    mutate(
      timeframe = if_else(
        is.na(process_timeframe), comm_timeframe, process_timeframe
      )
    )

  # for processess with several commodities (and not assigned timeframe)
  # select the first (lowest level) timeframe
  process_comm_timeframe <- process_comm_timeframe |>
    left_join(
      named_list_to_df(scen@settings@calendar@timeframe_rank,
        col_names = c("timeframe", "timeframe_rank")
      ),
      by = "timeframe"
    ) |>
    group_by(process) |>
    arrange(timeframe_rank)

  # check if directly assigned timeframes are consistent with commodity timeframes
  check_timeframe <- process_comm_timeframe |>
    rename(timeframe_rank_process = timeframe_rank) |>
    left_join(
      named_list_to_df(scen@settings@calendar@timeframe_rank,
        col_names = c("comm_timeframe", "timeframe_rank_comm")
      ),
      by = c("comm_timeframe")
    ) |>
    filter(!is.na(process_timeframe)) |>
    # select(process, process_timeframe, timeframe, timeframe_rank) |>
    unique() |>
    group_by(process) |>
    mutate(
      # timeframe_min = min(timeframe_rank)
      timeframe_max_pro = max(timeframe_rank_process),
      timeframe_max_comm = max(timeframe_rank_comm)
    ) |>
    filter(timeframe_max_pro != timeframe_max_comm) |>
    filter(timeframe_rank_process < timeframe_rank_comm) |>
    as.data.table()

  if (nrow(check_timeframe) > 0) {
    # add log/message
    warning(
      "Inconsistent timeframes of processes: ",
      paste(unique(check_timeframe$process), collapse = ", "),
      "\nwith commodities: ",
      paste(unique(check_timeframe$comm), collapse = ", "),
      "\nReplacing process timeframe with the lowest level timeframes of its commodities."
    )
  }

  process_comm_timeframe <- process_comm_timeframe |>
    group_by(process) |>
    arrange(timeframe_rank) |>
    dplyr::slice(1) |>
    ungroup() |>
    select(process, timeframe) |>
    unique() |>
    arrange(process) |>
    as.data.table()

  # coerce to list
  ll <- split(process_comm_timeframe$timeframe, process_comm_timeframe$process)

  return(ll)
}

#' Get class of processes
#'
#' @param scen scenario object
#' @param process character vector of process names, if not provided,
#' all processes retrieved from the scenario object
#' @param classes character vector of class names to search for
#'
#' @returns
#' @export
#'
#' @examples
get_process_class <- function(scen, process = NULL, classes = NULL) {
  # collect classes for processes in the scenario
  if (is.null(classes)) {
    classes <- c("process", "technology", "storage", "supply", "import",
                 "trade", "export", "demand")
  }

  ll <- apply_to_scenario(
    scen,
    classes = classes,
    func = function(x) {
      ll <- list()
      ll[[x@name]] <- class(x) |> as.character()
      return(ll)
    }
  )

  if (!is.null(process)) {
    ll <- ll[names(ll) %in% process]
  }

  return(ll)
}

#' Get inputs of processes
#'
#' @param scen scenario object
#' @param process character vector of process names, if not provided,
#' all processes retrieved from the scenario object
#' @param classes character vector of class names to search for
#'
#' @returns
#' @export
#'
#' @examples
get_process_inputs <- function(scen, process = NULL, classes = NULL) {
  if (is.null(classes)) {
    classes <- c(
      "process", "technology", "storage",
      # "supply", "import" # no inputs
      "trade", "export", "demand"
    )
  }

  # collect all inputs for each process

  ll <- apply_to_scenario(
    scen = scen,
    classes = classes,
    func = function(x) {
      # ll <- list(name = x@name, value = character())
      ll <- list()
      ll[[x@name]] <- character()
      if (.hasSlot(x, "input")) {
        # browser()
        ll[[x@name]] <- x@input$comm |>
          as.character() |>
          unique()
      } else if (.hasSlot(x, "commodity")) {
        ll[[x@name]] <- x@commodity |>
          as.character() |>
          unique()
      } else {
        warning("No inputs found for process ", x@name)
        ll[[x@name]] <- character()
      }
      ll
    }
  )
  ll
}

#' Get output commodities for each process
#'
#' @param scen
#' @param process
#' @param classes
#'
#' @returns
#' @export
#'
#' @examples
get_process_outputs <- function(scen, process = NULL, classes = NULL) {
  if (is.null(classes)) {
    classes <- c(
      "process", "technology", "storage",
      # "demand", "supply", # no outputs
      "supply", "import",
      "trade", "export"
    )
  }
  # collect all outputs for each process

  ll <- apply_to_scenario(
    scen = scen,
    classes = classes,
    func = function(x) {
      # ll <- list(name = x@name, value = character())
      ll <- list()
      ll[[x@name]] <- character()
      if (.hasSlot(x, "output")) {
        # ll$value <- x@output$comm |> as.character() |> unique()
        ll[[x@name]] <- x@output$comm |>
          as.character() |>
          unique()
      } else if (.hasSlot(x, "commodity")) {
        # ll$value <- x@commodity |> as.character() |> unique()
        ll[[x@name]] <- x@commodity |>
          as.character() |>
          unique()
      } else {
        warning("No outputs found for process ", x@name)
        # ll$value <- character()
        ll[[x@name]] <- character()
      }
      ll
    }
  )
  ll
}

#' Get auxiliary commodities for each process
#'
#' @param scen
#' @param process
#' @param classes
#'
#' @returns
#' @export
#'
#' @examples
get_process_aux <- function(scen, process = NULL, classes = NULL) {
  if (is.null(classes)) {
    classes <- c("process", "technology", "storage", "trade")
    # "demand", "supply", "import", "export") # no aux
  }
  # collect all outputs for each process

  ll <- apply_to_scenario(
    scen = scen,
    classes = classes,
    func = function(x) {
      # ll <- list(name = x@name, value = character())
      ll <- list()
      ll[[x@name]] <- character()
      if (.hasSlot(x, "aux")) {
        # ll$value <- x@aux$acomm |> as.character() |> unique()
        ll[[x@name]] <- x@aux$acomm |>
          as.character() |>
          unique()
        # } else if (.hasSlot(x, "commodity")) {
        #   ll$value <- x@commodity |> as.character() |> unique()
      } else {
        warning("No aux slot found for process ", x@name)
        # ll$value <- character()
        ll[[x@name]] <- character()
      }
      ll
    }
  )
  ll
}

#' Commodities associated with a processes
#'
#' @param scen scenario object
#' @param process character vector of process names, if not provided,
#' @param classes character vector of class names to search for
#' @param return_list logical, if TRUE, return a list of results, otherwise
#' data.frame with columns `process` and `comm`
#'
#' @returns
#' @export
get_process_comm <- function(scen, process = NULL, classes = NULL,
                             return_list = TRUE) {
  # browser()

  # collect all commodities for each process
  ll_inp <- get_process_inputs(scen, process = process, classes = classes)
  ll_out <- get_process_outputs(scen, process = process, classes = classes)
  ll_aux <- get_process_aux(scen, process = process, classes = classes)

  # combine all lists
  dd <- rbindlist(list(
    named_list_to_df(ll_inp, col_names = c("name", "value")),
    named_list_to_df(ll_out, col_names = c("name", "value")),
    named_list_to_df(ll_aux, col_names = c("name", "value"))
  )) |>
    unique() |>
    arrange(name) |>
    as.data.table()

  # coerce to list
  # unstack(dd, value ~ name)
  split(dd$value, dd$name)
}

#' Convert a named list of vectors to a data frame
#'
#' @param named_list named list of vectors, where each vector represents
#' a set of values for a given name
#' @param col_names character vector of column names for the resulting
#' data frame. Default is c("name", "value"), where "name" is the name of the
#' list element and "value" is the value of the list element.
#'
#' @returns data frame with two columns ("name" and "value" by default) where
#' each row represents a name-value pair from the input list. The function
#' ensures that the resulting data frame is unique and sorted by name.
#' @export
#' @aliases nl2df
named_list_to_df <- function(named_list, col_names = c("name", "value")) {
  # browser()
  nms <- names(named_list)
  lapply(nms, function(x) {
    # browser()
    if (length(named_list[x]) == 1) {
      data.table(name = x, value = named_list[[x]]) |> setNames(col_names)
    } else if (length(named_list[x]) > 1) {
      stop(
        "List element '", x, "' has more than one element.",
        " Cannot convert to data frame."
      )
    } else {
      NULL
    }
  }) |>
    rbindlist(use.names = TRUE, fill = TRUE) |>
    unique() |>
    # arrange(col_names[1]) |>
    as.data.table()
}
# alias
nl2df <- named_list_to_df


#' Get regions for each process
#'
#' @param scen scenario object
#' @param process character vector of process names, if not provided,
#' all processes in the scenario are returned.
#' @param classes character vector of class names to search for
#' @param return_list logical, if TRUE, return a list of results, otherwise
#' data.frame with columns `process` and `region`
#'
#' @returns a list of named vectors, where each vector contains the regions
#' associated with a process. The names of the list elements are the process
#' names, and the values are the regions associated with each process.
#' If `return_list` is FALSE, the function returns a data frame with two
#' columns: "process" and "region", where each row represents a process-region
#' pair. The data frame is unique and sorted by process and region.
#'
#' @export
get_process_region <- function(scen, process = NULL, classes = NULL,
                               return_list = TRUE) {
  # browser()
  # collect all regions for each process
  if (is.null(classes)) {
    classes <- c(
      "process", "technology", "storage",
      "trade", "import", "export",
      "demand", "supply"
    )
  }

  scen_regions <- scen@settings@region

  # collect regions from @region slot
  ll <- apply_to_scenario(
    scen = scen,
    classes = classes,
    # names = process, #!!! ToDo: add process names
    func = function(x) {
      # ll <- list(name = x@name, value = character())
      ll <- list()
      ll[[x@name]] <- character()
      if (.hasSlot(x, "region")) {
        # ll$value <- x@region |> as.character() |> unique()
        ll[[x@name]] <- x@region |>
          as.character() |>
          unique()
      } else {
        # warning("No region slot found for process ", x@name)
        # ll$value <- character()
        ll[[x@name]] <- character()
      }
      ll
    }
  )

  # collect regions from data.frame slots, region, src, dst columns
  ll_slots <- apply_to_scenario(
    scen = scen,
    classes = classes,
    # names = process, #!!! ToDo: add process names
    func = function(x) {
      # browser()
      # ll <- list(name = x@name, value = character())
      ll <- list()
      ll[[x@name]] <- character()
      slots <- slotNames(x)

      # 1. search regions in data.frame slots
      rr <- sapply(slots, function(s) {
        if (inherits(slot(x, s), "data.frame")) {
          rr <- character()
          # if (s == "olife") browser()
          for (col in c("region", "src", "dst")) {
            if (col %in% colnames(slot(x, s))) {
              rr <- c(rr, slot(x, s)[[col]])
            }
          }
          rr <- rr |>
            unique() |>
            sort(na.last = FALSE)
          if (length(rr) > 0) {
            return(rr)
          } else {
            return(character())
          }
        } else {
          return(NULL)
        }
      })
      rr <- rr[!sapply(rr, is_empty)] # remove empty slots

      # if (x@name == "TRBD_ELC_R1_R2") browser()

      # 2. check if there are any NA values in the slots
      # meaning that parameters are applied to all regions
      all_na <- sapply(rr, function(r) any(is.na(r))) |> all()
      if (all_na) {
        # if all slots have NA values, return empty character vector
        ll[[x@name]] <- NA_character_
        return(
          ll
          # list(name = x@name, value = NA_character_)
        )
      }

      # 3. check if there are slots with region names and no NA values,
      # restricting to the region set
      no_na <- sapply(rr, function(r) {
        !any(is.na(r))
      })
      rr <- rr[no_na]
      rm(no_na)

      # check if there are more than one slot with non-NA values in region columns
      if (length(rr) > 1) {
        unique_rr <- rr[[1]]
        # diff_rr <- character()
        for (i in 2:length(rr)) {
          unique_rr <- intersect(unique_rr, rr[[i]])
        }
        rr <- unique_rr
      } else if (length(rr) == 1) {
        rr <- rr[[1]]
      } else {
        rr <- character()
      }

      # ll$value <- rr
      ll[[x@name]] <- rr
      return(ll)
    }
  )

  # combine results from both queries
  # names(ll_slots)[!(names(ll_slots) %in% names(ll))]
  nms <- c(names(ll_slots), names(ll)) |>
    unique() |>
    sort()

  nn <- list()
  for (i in nms) {
    if (!is_empty(ll[[i]])) {
      # if regions declared in @region slot, it will be used
      if (any(is.na(ll[[i]]))) {
        # NA values are not allowed in @region slot
        stop("NA values are not allowed in @region slot. Check process ", i)
      }
      # check if all region-names are in the scenario region set
      if (!all(ll[[i]] %in% scen_regions)) {
        # add log/message
        reg_diff <- ll[[i]][!(ll[[i]] %in% scen_regions)]
        stop("Regions '", reg_diff, "' are not declared in the scenario region set.")
      }

      nn[[i]] <- ll[[i]]
    } else {
      # use regions from data.frame slots

      if (is_empty(ll_slots[[i]])) {
        stop("No regions info found for process ", i)
      }
      if (any(is.na(ll_slots[[i]]))) {
        if (any(!is.na(ll_slots[[i]]))) {
          # should not be here, either single NA or non-NA values
          stop("Cannot identify regions for process ", i)
        }
        # NA means all regions
        nn[[i]] <- scen_regions
        next
      }

      # Non-NA values of region names. Check if they are declared
      if (!all(ll_slots[[i]] %in% scen_regions)) {
        # add log/message
        reg_diff <- ll_slots[[i]][!(ll_slots[[i]] %in% scen_regions)]
        stop("Regions '", reg_diff, "' are not declared in the scenario region set.")
      }

      nn[[i]] <- ll_slots[[i]]
    }
  }

  if (!return_list) {
    # convert to data.frame
    nn <- named_list_to_df(nn, col_names = c("process", "region")) |>
      unique() |>
      arrange(process, region)
  }

  return(nn)
}

# get_primary_comm_region <- function(scen) {
#   # find regions for commodities from supply and import processes
# }


#' Start and end years for process by region
#'
#' @param scen scenario object
#' @param process character vector of process names, if not provided,
#' @param classes character vector of class names to search for
#'
#' @returns data frame with columns `process`, `region`, `start`, `end`
#' @export
#'
get_process_invest_window <- function(scen, process = NULL, classes = NULL) {
  # collect all lifespans for each process
  if (is.null(classes)) {
    classes <- c(
      "process", "technology", "storage",
      # "import", "export", "demand", "supply",
      "trade"
    )
  }

  # collect all lifespans for each process
  ll_start_end <- apply_to_scenario(
    scen = scen,
    classes = classes,
    func = function(x) {
      ll <- list()
      # cat("Process: ", x@name, "\n")
      # browser()
      dd <- data.table(
        process = x@name,
        region = character(),
        start = integer(),
        end = integer()
      )
      if (.hasSlot(x, "start")) {
        if ("region" %in% colnames(x@start)) {
          d <- merge(x@start, x@end, by = "region", all = TRUE) |>
            mutate(process = x@name, .before = 1) |>
            as.data.table()
        } else {
          # browser()
          regs <- scen@modInp@sets$process_region[[x@name]]
          d <- merge(
            data.table(region = regs, start = x@start$start),
            data.table(region = regs, end = x@end$end),
            by = "region", all = TRUE
          ) |>
            mutate(process = x@name, .before = 1) |>
            as.data.table()
        }

        if (nrow(d) > 0) dd <- d
      }
      ll[[x@name]] <- dd
      ll
    },
    as_list = FALSE
  )

  # fix for infinite values (transitioning from Inf to NA in slots to use integers)
  ll_start_end <- ll_start_end |>
    mutate(
      start = if_else(is.infinite(start), NA, start),
      end = if_else(is.infinite(end), NA, end),
      start = as.integer(start),
      end = as.integer(end)
    ) |>
    unique() |>
    arrange(process, region)

  # expand regions for NA values, keeping start and end years
  ll_na <- ll_start_end |>
    filter(is.na(region)) |>
    select(-region) |>
    unique() |>
    left_join(
      named_list_to_df(scen@modInp@sets$process_region,
        col_names = c("process", "region")
      ),
      by = "process"
    )

  ll_start_end <- ll_start_end |>
    filter(!is.na(region)) |>
    rbind(ll_na) |>
    unique() |>
    arrange(process, region)

  return(ll_start_end)
}

get_process_invest_years <- function(scen, process = NULL, classes = NULL) {
  # match model years with investment window
  if (is.null(scen@modInp@sets$process_invest_window)) {
    # log/message
    warning("Investment window is not defined. Running `get_process_invest_window()`.")
    scen@modInp@sets$process_invest_window <- get_process_invest_window(scen)
  }
  if (is.null(scen@modInp@sets$process)) {
    # log/message
    warning("No process set `scen@modInp@sets$process` found.",
            "Hint: either the model has not processes, or ",
            "the set was no created. Check `scen@modInp@sets$process`"
            )
  }

  process_invest_window <- scen@modInp@sets$process_invest_window

  process_invest_year <- lapply(
    scen@modInp@sets$process, function(x) {
      xx <- process_invest_window[process == x]
      if (nrow(xx) == 0) {
        return(NULL)
      }
      yy <- expand_grid(
        process = x,
        region = unique(xx$region),
        year = scen@settings@horizon@intervals$mid
      ) |>
        left_join(
          xx,
          by = c("process", "region")
        ) |>
        filter(year >= start | is.na(start)) |>
        filter(year <= end | is.na(end)) |>
        select(process, region, year) |>
        unique() |>
        as.data.table()
      return(yy)
    }
  ) |>
  rbindlist(use.names = TRUE, fill = TRUE)

  return(process_invest_year)

}

get_process_stock_window <- function(scen, process = NULL, classes = NULL) {
  # collect capacity$stock years by process and region
  if (is.null(classes)) {
    # processes with capacity
    classes <- c(
      "process", "technology", "storage",
      # "import", "export", "demand", "supply",
      "trade"
    )
  }

  mod_years <- scen@settings@horizon@intervals$mid

  ll <- apply_to_scenario(
    scen = scen,
    classes = classes,
    func = function(x) {
      ll <- list()
      # cat("Process: ", x@name, "\n")
      # browser()
      if (!.hasSlot(x, "capacity")) return(NULL)
      dd <- x@capacity |>
        select(any_of("region"), year, stock) |>
        filter(!is.na(stock)) |>
        unique() |>
        mutate(process = x@name, .before = 1) |>
        group_by(across(any_of(c("process", "region")))) |>
        mutate(
          start = min(c(year, mod_years), na.rm = TRUE),
          end = max(c(year, mod_years), na.rm = TRUE)
        ) |>
        ungroup() |>
        select(process, any_of("region"), start, end) |>
        unique() |>
        as.data.table()

      ll[[x@name]] <- dd
      ll
    },
    as_list = FALSE
  )

  # expand regions for NA values, keeping start and end years
  ll_na <- ll |>
    filter(is.na(region)) |>
    select(-region) |>
    unique() |>
    left_join(
      named_list_to_df(scen@modInp@sets$process_region,
        col_names = c("process", "region")
      ),
      by = "process"
    )

  ll <- ll |>
    filter(!is.na(region)) |>
    rbind(ll_na) |>
    unique() |>
    arrange(process, region)

  # !!!??? filter for process' regions
  return(ll)
}

get_process_stock_years <- function(scen, process = NULL, classes = NULL) {
  # match model years with stock window
  if (is.null(scen@modInp@sets$process_stock_window)) {
    # log/message
    warning("Stock window is not defined. Running `get_process_stock_window()`.")
    scen@modInp@sets$process_stock_window <- get_process_stock_window(scen)
  }

  if (is.null(scen@modInp@sets$process)) {
    # log/message
    warning("No process set `scen@modInp@sets$process` found.",
            "Hint: either the model has not processes, or ",
            "the set was no created. Check `scen@modInp@sets$process`"
            )
  }

  process_stock_window <- scen@modInp@sets$process_stock_window
  process_stock_year <- lapply(
    scen@modInp@sets$process, function(x) {
      xx <- process_stock_window[process == x]
      if (nrow(xx) == 0) {
        return(NULL)
      }
      yy <- expand_grid(
        process = x,
        region = unique(xx$region),
        year = scen@settings@horizon@intervals$mid
      ) |>
        left_join(
          xx,
          by = c("process", "region")
        ) |>
        filter(year >= start | is.na(start)) |>
        filter(year <= end | is.na(end)) |>
        select(process, region, year) |>
        unique() |>
        as.data.table()
      return(yy)
    }
  ) |>
    rbindlist(use.names = TRUE, fill = TRUE)

  return(process_stock_year)

}

get_process_years <- function(scen, process = NULL, classes = NULL) {
  # check if scen@modInp@sets$process_year exists and/or rerun

  if (is.null(scen@modInp@sets$process_year)) {
    # log/message

    if (is.null(scen@modInp@sets$process_invest_window)) {
      warning("Investment window is not defined. Running `get_process_invest_window()`.")
      scen@modInp@sets$process_invest_window <- get_process_invest_window(scen)
    }
    if (is.null(scen@modInp@sets$process_invest_year)) {
      warning("Investment years are not defined. Running `get_process_invest_years()`.")
      scen@modInp@sets$process_invest_year <- get_process_invest_years(scen)
    }
    if (is.null(scen@modInp@sets$process_stock_window)) {
      warning("Stock window is not defined. Running `get_process_stock_window()`.")
      scen@modInp@sets$process_stock_window <- get_process_stock_window(scen)
    }
    if (is.null(scen@modInp@sets$process_stock_year)) {
      warning("Stock years are not defined. Running `get_process_stock_years()`.")
      scen@modInp@sets$process_stock_year <- get_process_stock_years(scen)
    }

    # ### process availability over years by region
    scen@modInp@sets$process_year <- rbind(
      scen@modInp@sets$process_invest_year,
      scen@modInp@sets$process_stock_year
    ) |>
      unique() |>
      arrange(process, year) |>
      as.data.table()

  }

  process_year <- scen@modInp@sets$process_year

  if (!is.null(process)) {
    ii <- process_year$process %in% process
    process_year <- process_year[ii, ]
  }
  if (!is.null(classes)) {
    process_class <-
      get_process_class(scen, process = process, classes = classes) |>
      named_list_to_df(col_names = c("process", "class"))

    process_year <- process_year |>
      left_join(
        process_class,
        by = "process"
      ) |>
      filter(class %in% classes) |>
      select(-class)
  }

  return(process_year)
}

# get_comm_region <- function(scen, comm = NULL) {
#
# }

#' Return default value for one, several, or all parameters
#'
#' @param scen
#' @param pname character, parameter name, as it appears in the model
#' @param sname character, short name of the parameter, as it appears in classes
#' @param class character, class of the parameter to search for
#' @param bound character, name of the bound to retrieve, `lo` or `up`.
#' If `NULL`, the function will return the default value for both upper and
#' lower bounds.
#' @param global logical, if `FALSE`, the function will search for adjusted
#' values for the parameter in the scenario object. If `TRUE` or no adjustments
#' found, the function will return the default value stored in the package data.
#' @param one_value logical, if `TRUE`, the function will force returning
#' a single value for the parameter, and will throw an error if multiple values
#' are found. If `FALSE`, the function will return all found values.
#'
#'
#' @returns
#' A default value for one parameter, or a list of default values for several parameters.
#' @export
get_default_value <- function(
    scen = NULL,
    pname = NULL,
    sname = NULL,
    oname = NULL,
    class = NULL,
    bound = NULL,
    global = is.null(scen),
    one_value = FALSE
    ) {
  # browser()
  # global defaults from .modInp
  if (!is.null(pname) && !is.null(sname)) {
    stop("Either `pname` or `sname` must be provided, not both.")
  }
  if (!is.null(pname)) {
    p <- .modInp[names(.modInp) %in% pname]
    if (is.null(p)) {
      stop("Parameter '", pname, "' not found in the `.modInp`.")
    }
  } else if (!is.null(sname)) {
    p <- get_slot_meta(class = class, colName = sname)

    if (is.null(p)) {
      stop("Parameter '", sname, "' not found in the `.modInp`.")
    }
  } else {
    stop("Either `pname` or `sname` must be provided.")
  }

  # !!! ToDo: add
  # check if the parameter is adjusted in the scenario

  # check the bounds
  ii <- sapply(p, function(x) {x$type == "bounds"})
  p <- lapply(p, function(x) x[["defVal"]])
  if (any(ii)) {
    if (!all(ii)) {
      stop("Parameter '", pname, "' has different types: ",
           paste(p$type[!ii], collapse = ", "),
           " and ", paste(p$type[ii], collapse = ", "),
           ". Cannot retrieve default value.")
    }
    if (!is.null(bound)) {
      if (bound == "lo") {
        p <- sapply(p, function(x) x[1])
      } else if (bound == "up") {
        p <- sapply(p, function(x) x[2])
      } else {
        stop("Invalid bound name. Use 'lo' or 'up'.")
      }
    } else {
      # return both bounds
      # p <- p[p$type == "bounds", c("lo", "up")]
    }
    # p <- p[ii]
  } else {
    if (!is.null(bound)) {
      warning("Parameter '", pname, "' is not a bounds parameter.")
    }
  }

  # one_value
  if (one_value) {
    if (length(p) > 1) {
      stop("Parameter '", pname, "' has multiple values. Use `one_value = FALSE`.")
    }
    return(p[[1]])
  } else {
    return(p)
  }

}

if (FALSE) {
  # debug
  # scen <- get_scenario("ECOA")
  # pname <- "cinp2use"
  # sname <- "cinp2use"
  # class <- "capacity"
  # bound <- "lo"
  # global <- FALSE
  # one_value <- TRUE
  get_default_value(pname = "pWeather")
  get_default_value(sname = "wval")
  get_default_value(sname = "cinp2use")
  get_default_value(sname = "af", class = "technology", bound = "lo")
  get_default_value(sname = "af", class = "technology", bound = "lo", one_value = TRUE)

}

get_interpolation_rule <- function(
    scen = NULL,
    pname = NULL,
    sname = NULL,
    oname = NULL,
    class = NULL,
    bound = NULL,
    global = is.null(scen),
    one_value = FALSE
) {

  # global defaults from .modInp
  if (!is.null(pname) && !is.null(sname)) {
    stop("Either `pname` or `sname` must be provided, not both.")
  }
  if (!is.null(pname)) {
    p <- .modInp[names(.modInp) %in% pname]
    if (is.null(p)) {
      stop("Parameter '", pname, "' not found in the `.modInp`.")
    }
  } else if (!is.null(sname)) {
    p <- get_slot_meta(class = class, colName = sname)

    if (is.null(p)) {
      stop("Parameter '", sname, "' not found in the `.modInp`.")
    }
  } else {
    stop("Either `pname` or `sname` must be provided.")
  }
  # check the bounds
  ii <- sapply(p, function(x) {x$type == "bounds"})
  p <- lapply(p, function(x) x[["interpolation"]])
  if (any(ii)) {
    if (!all(ii)) {
      stop("Parameter '", pname, "' has different types: ",
           paste(p$type[!ii], collapse = ", "),
           " and ", paste(p$type[ii], collapse = ", "),
           ". Cannot retrieve default value.")
    }
    if (!is.null(bound)) {
      if (bound == "lo") {
        p <- sapply(p, function(x) x[1])
      } else if (bound == "up") {
        p <- sapply(p, function(x) x[2])
      } else {
        stop("Invalid bound name. Use 'lo' or 'up'.")
      }
    } else {
      # return both bounds
      # p <- p[p$type == "bounds", c("lo", "up")]
    }
    # p <- p[ii]
  } else {
    if (!is.null(bound)) {
      warning("Parameter '", pname, "' is not a bounds parameter.")
    }
  }
  # one_value
  if (one_value) {
    if (length(p) > 1) {
      stop("Parameter '", pname, "' has multiple values. Use `one_value = FALSE`.")
    }
    return(p[[1]])
  } else {
    return(p)
  }

}

if (FALSE) {
  # debug
  get_interpolation_rule(pname = "pWeather")
  get_interpolation_rule(sname = "wval")
  get_interpolation_rule(sname = "cinp2use")
  get_interpolation_rule(sname = "af", class = "technology", bound = "lo")
}

interpolate_numpar <- function() {}

interpolate_bounds <- function() {}



