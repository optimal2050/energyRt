# interpolate(scenario, overwrite = TRUE) - updates interpolation in scenario objects

# solve_model(scenario, interpolate = NULL) - solves scenario, interpolate if required (NULL), force (TRUE), or no interpolation (FALSE, error if not interpolated)
# returns list with solution status and folder

# read_solution(scen = NULL, folder) - reads results and updates scen, if scen = NULL, creates empty scenario with solution data


# The function creates scenario object with interpolated data, ready to pass to a solver
#' @export
#' @rdname interpolate
#' @family interpolate model scenario
interpolate_model <- function(object, ...) { #- returns class scenario
  ## arguments
  # obj - scenario or model
  # name
  # desc
  # (!!! depreciated) n.threads - number of threads use for approximation
  # (!!! depreciated) startYear && fixTo have to define both (or not define both) - run with startYear
  # (!!! depreciated) year - basic horizon year definition (not recommended for use), use only if horizon not defined
  # discount - define discount, not good practice
  # region - define region, not good practice
  # repository - class for add to model (repository or list of repository)
  # echo - print working data
  # !!! ADD: adjust_weather_sample, adjust_demand_sample
  # browser()
  obj <- object
  arg <- list(...)
  # browser()
  if (any(sapply(arg, function(x) inherits(x, "list")))) {
    # !!! temporary fix to avoid unlisting solver list
    arg_names <- names(arg)
    solv <- NULL
    if (any(arg_names %in% "solver")) {
      solv <- arg$solver
      arg$solver <- NULL
    }
    arg0 <- try(purrr::list_flatten(arg, name_spec = "{inner}"))
    if (inherits(arg, "try-error")) {
      warning("Potential error in arguments list.")
    } else {
      arg <- arg0
    }
    arg$solver <- solv
    rm(arg0, solv)
  }
  tictoc::tic()
  # browser()
  interpolation_start_time <- proc.time()[3]
  if (is.null(arg$echo)) arg$echo <- FALSE

  if (is(obj, "model")) {
    scen <- new("scenario")
    # scen <- newScenario()
    scen@model <- obj
    scen@name <- paste("scen", obj@name, sep = "_")
    scen@desc <- ""
    scen@settings <- .config_to_settings(obj@config) # import model settings
  } else if (is(obj, "scenario")) {
    scen <- obj
  } else {
    stop('Interpolation is not available for class: "', class(obj), '"')
  }

  # tictoc::toc()
  if (!is.null(arg$overwrite)) {
    overwrite <- arg$overwrite; arg$overwrite <- NULL
  } else {
    overwrite <- FALSE
  }
  if (isTRUE(arg$force)) { # force interpolation of interpolated model
    scen@status$interpolated <- FALSE
    arg$force <- NULL
  }
  if (!is.null(arg$name)) {scen@name <- arg$name; arg$name <- NULL}
  if (!is.null(arg$desc)) {scen@desc <- arg$desc; arg$desc <- NULL}
  #!!! browser()
  # if (!is.null(arg$path)) {
  #   scen@path <- arg$path; arg$path <- NULL
  # } else {
  #   scen@path <- fp(get_scenarios_path(), make_scenario_dirname(scen))
  # }
  # if (!is.null(arg$inMemory)) {
  #   scen@inMemory <- arg$inMemory;
  #   arg$inMemory <- NULL
  # }
  # cat("model: '", object@name, "'\n", sep = "")
  # cat("scenario: '", scen@name, "'\n", sep = "")
  # if (!(scen@desc == "")) cat("        '", arg$desc, "'\n", sep = "")
  # if (!is.null(scen@path) || !scen@inMemory) {
  #   cat("path: ", scen@path, "\n", sep = "")
  #   cat("inMemory: ", scen@inMemory, "\n", sep = "")
  # }

  # repository ############################################
  if (!is.null(arg$repository)) {
    if (!is.null(arg$data))
      stop("Only one of arguments 'repository' or 'data' can be used.",
           call. = FALSE)
    scen@model <- .add_repository(scen@model, arg$repository) # !!! use add?
    arg$repository <- NULL
  }
  # data ############################################
  if (!is.null(arg$data)) {
    scen@model <- .add_repository(scen@model, arg$data) # !!! use add instead?
    arg$data <- NULL
    scen@status$interpolated <- FALSE
  }

  carg <- sapply(arg, function(x) class(x)[1])
  # more repositories ############################################
  # check if there are more repositories to add
  ii <- carg == "repository"
  if (any(ii)) {
    for (ob in arg[ii]) {
      scen@model <- add(scen@model, ob, overwrite = overwrite)
    }
    arg[ii] <- NULL; carg <- carg[!ii]
    scen@status$interpolated <- FALSE
  }

  # "bricks" ############################################
  # check if there are any objects to add to a repository
  repo_objs <- newRepository("")@permit
  ii <- carg %in% repo_objs
  if (any(ii)) {
    scen_repo <- newRepository(name = paste0(scen@name, "_repo"), arg[ii])
    scen@model <- add(scen@model, scen_repo); rm(scen_repo)
    arg[ii] <- NULL; carg <- carg[!ii]
    scen@status$interpolated <- FALSE
  }

  # config ############################################
  ii <- carg %in% "config"
  if (any(ii)) {
    if (sum(ii) > 1) {
      stop("Two or more 'config' objects found in 'interpolation' arguments.",
           call. = FALSE)
    }
    if (any(carg %in% "settings") || !is.null(arg@settings)) {
      stop("Both 'config' and 'settings' objects found in 'interpolation' arguments.",
           "Only one of them can be used in 'interpolation' arguments.",
           call. = FALSE)
    }
    scen@settings <- .config_to_settings(arg[ii][[1]])
    arg[ii] <- NULL; carg <- carg[!ii]
    scen@status$interpolated <- FALSE
  }

  # settings ############################################
  ii <- carg %in% "settings"
  if (any(ii)) {
    if (sum(ii) > 1) {
      stop("Two or more 'settings' objects found in 'interpolation' arguments.",
           call. = FALSE)
    }
    scen@settings <- arg[ii][[1]]
    arg[ii] <- NULL; carg <- carg[!ii]
    scen@status$interpolated <- FALSE
  }

  # calendar ############################################
  ii <- carg %in% "calendar"
  if (any(ii)) {
    if (sum(ii) > 1) {
      stop("Two or more 'calendar' objects found in 'interpolation' arguments.",
           call. = FALSE)
    }
    # scen <- setCalendar(scen, arg[ii][[1]]) # !!! ToDo
    scen@settings@calendar <- arg[ii][[1]]
    scen@settings@yearFraction$fraction <-
      sum(scen@settings@calendar@timetable$share)
    arg[ii] <- NULL; carg <- carg[!ii]
    scen@status$interpolated <- FALSE
  }

  # horizon ############################################
  ii <- carg %in% "horizon"
  if (any(ii)) {
    if (sum(ii) > 1) {
      stop("Two or more 'horizon' objects found in 'interpolation' arguments.",
           call. = FALSE)
    }
    # browser()
    scen <- setHorizon(scen, arg[ii][[1]])
    # scen@settings <- arg[ii]
    arg[ii] <- NULL; carg <- carg[!ii]
    scen@status$interpolated <- FALSE
  }

  # SETTINGS slots ############################################
  # check if `...` has data for `settings` or `horizon` parameters
  # ToDo: rewrite with 'add' once implemented for settings/horizon
  # !!! currently one-by-one
  # discountFirstYear ############################################
  if (any(names(arg) %in% "discountFirstYear")) {
    scen@settings@discountFirstYear <- arg$discountFirstYear
    arg$discountFirstYear <- NULL
    scen@status$interpolated <- FALSE
  }
  # optimizeRetirement ############################################
  if (any(names(arg) %in% "optimizeRetirement")) {
    scen@settings@optimizeRetirement <- arg$optimizeRetirement
    arg$optimizeRetirement <- NULL
    scen@status$interpolated <- FALSE
  }
  # defValue ############################################
  if (any(names(arg) %in% "defValue")) {
    scen@settings@defValue <- arg$defValue
    arg$defValue <- NULL
    scen@status$interpolated <- FALSE
  }
  # interpolation ############################################
  if (any(names(arg) %in% "interpolation")) {
    scen@settings@interpolation <- arg$interpolation
    arg$interpolation <- NULL
    scen@status$interpolated <- FALSE
  }
  # debug ############################################
  if (any(names(arg) %in% "debug")) {
    scen@settings@debug <- arg$debug
    arg$debug <- NULL
    scen@status$interpolated <- FALSE
  }
  # discount ############################################
  if (!is.null(arg$discount)) {
    scen@settings@discount <- arg$discount
    arg$discount <- NULL
    scen@status$interpolated <- FALSE
  }
  # solver ############################################
  if (!is.null(arg$solver)) {
    if (is.character(arg$solver)) {
      if (length(arg$solver) > 1) {
        stop("Solver must be a list or a single string.")
      }
      arg$solver <- list(nane = arg$solver, lang = arg$solver)
    }
    scen@settings@solver <- arg$solver
    arg$solver <- NULL
    # scen@status$interpolated <- FALSE
  }
  # *DEPRICIATED ############################################
  # year
  if (!is.null(arg$year)) {
    stop("\nThe 'year' argument is depreciated. \nUse 'period' ",
         "to set planning (optimization) period.  \n",
         "See ?horizon and ?settings for help", call. = FALSE)
  }
  # region
  if (!is.null(arg$region)) {
    stop("Regions must be defined before the interpolation")
    scen@settings@region <- arg$region
    arg$region <- NULL
  }
  # HORIZON slots ############################################
  if (!is.null(arg$period) | !is.null(arg$intervals)) {
    if (!is.null(arg$period)) {
      upd_period <- arg$period
    } else {
      upd_period <- scen@settings@horizon@period
    }
    if (!is.null(arg$intervals)) {
      upd_intervals <- arg$intervals
    } else {
      upd_intervals <- scen@settings@horizon@intervals
    }
    scen@settings@horizon <- newHorizon(
      period = upd_period,
      intervals = upd_intervals,
      desc = scen@settings@horizon@desc
    )
    scen@status$interpolated <- FALSE
  }


  # INTERPOLATE ############################################
  # Check if the interpolation is needed
  if (scen@status$interpolated) {
    message("The scenario is already interpolated. Use 'force = TRUE' ",
            "to re-interpolate the scenario.")
    return(invisible(scen))
  }

  # tictoc::toc()
  # other interpolation parameters
  if (is.null(arg$n.threads)) arg$n.threads <- 1 #+ 0 * detectCores()
  if (is.null(arg$verbose)) arg$verbose <- 0
  if (!is.null(arg$table_format)) { # !!! draft, not actual
    scen@misc$table_format <- arg$table_format
    # set_table_format(scen@misc$table_format)
    arg$table_format <- NULL
  } else {
    scen@misc$table_format <- "data.table"
    # set_table_format(scen@misc$table_format)
  }
  # tictoc::toc(); tictoc::tic()
  ### Interpolation
  scen@modInp <- new("modInp")
  # browser()
  ## Fill basic sets
  # Fill year
  if (!is.null(arg$year)) { #
    if (nrow(scen@settings@horizon@intervals) != 0) {
      stop("argument can't be used with horizon")
    }
    scen@settings@horizon@period <- arg$year
  }
  # browser()
  if (nrow(scen@settings@horizon@intervals) == 0) {
    # if (any(sort(scen@settings@horizon@period) !=
    #         scen@settings@horizon@period) ||
    #     max(scen@settings@horizon@period) -
    #     min(scen@settings@horizon@period) + 1 !=
    #   length(scen@settings@horizon@period)) {
    if (length(scen@settings@horizon@period) == 0) {
      stop("Empty 'period' parameter. \n",
           "Add 'settings' or 'horizon' to the model or 'interpolation(...)'")
    }
    scen@settings <- setHorizon(scen@settings,
      horizon = scen@settings@horizon@period[1],
      intervals = rep(1, length(scen@settings@horizon@period))
    )
  }

  #!!!
  # browser()
  if (!is.null(arg$path)) {
    scen@path <- arg$path; arg$path <- NULL
  } else {
    scen@path <- fp(get_scenarios_path(), make_scenario_dirname(scen))
  }
  if (!is.null(arg$inMemory)) {
    scen@inMemory <- arg$inMemory;
    arg$inMemory <- NULL
  }
  cat("model: '", object@name, "'\n", sep = "")
  cat("scenario: '", scen@name, "'\n", sep = "")
  if (!(scen@desc == "")) cat("        '", arg$desc, "'\n", sep = "")
  if (!is.null(scen@path) || !scen@inMemory) {
    cat("path: ", scen@path, "\n", sep = "")
    cat("inMemory: ", scen@inMemory, "\n", sep = "")
  }

  # browser()
  #!!! Suppressed parameter
  # newParameter("horizon", dimSets = "horizon", type = "map")
  # scen@modInp@parameters[["horizon"]] <-
  #   .dat2par(scen@modInp@parameters[["horizon"]],
  #            scen@settings@horizon@period)
  # tictoc::toc(); tictoc::tic()
  scen@modInp@parameters[["year"]] <-
    .dat2par(scen@modInp@parameters[["year"]],
             scen@settings@horizon@intervals$mid)
  # .dat2par(scen@modInp@parameters[["year"]], scen@settings@horizon@period)

  # browser()
  # nYears <- length(scen@settings@horizon@intervals$mid)
  # scen@modInp@parameters[["nextYear"]] <-
  #   .dat2par(scen@modInp@parameters[["nextYear"]],
  #            data.table(
  #              year = scen@settings@horizon@intervals$mid[-nYears],
  #              value = scen@settings@horizon@intervals$mid[-1]
  #              )
  #            )

  #!!! Suppressed parameter
  # scen@modInp@parameters[["pYear"]] <-
  #   .dat2par(scen@modInp@parameters[["pYear"]],
  #            data.table(
  #              year = scen@settings@horizon@intervals$mid,
  #              value = scen@settings@horizon@intervals$mid
  #            )
  #   )

  scen@modInp@parameters[["mMidMilestone"]] <-
    .dat2par(scen@modInp@parameters[["mMidMilestone"]],
             data.table(year = scen@settings@horizon@intervals$mid)
             )
  # slices ####
  # browser()
  # scen@settings@timeframe <- .init_slice(scen@settings@timeframe)
  # browser()
  # tictoc::toc(); tictoc::tic()
  if (mean(scen@settings@yearFraction$fraction) != 1.) {
    # filter out unused slices
    # browser()
    # warning(
    #   "Solving for a fraction of a year.\n",
    #   "Variables without slice dimension scaled to the annual level using weights.",
    #   "The solution might differ from the whole-year optimization."
    # )
    cat("Subsetting time-slices\n")
    scen@model@data <- subset_slices_repo(
      repo = scen@model@data,
      yearFraction = mean(scen@settings@yearFraction$fraction),
      keep_slices = scen@settings@calendar@slice_share$slice
    )
  }
  # !!!??? add filtering slices here
  # nslices <- nrow(scen@settings@calendar@timetable)
  # scen@settings@calendar@timetable$share <- scen@settings@calendar@timetable$share * nslices/8760
  # scen@settings@calendar@slice_share$share <- scen@settings@calendar@slice_share$share * nslices/8760
  # browser()
  scen@modInp@parameters[["slice"]] <- .dat2par(
    scen@modInp@parameters[["slice"]],
    scen@settings@calendar@slice_share$slice
  )
  # region ####
  scen@modInp@parameters[["region"]] <-
    .dat2par(scen@modInp@parameters[["region"]], scen@settings@region)

  # browser()
  # List for approximation
  # Generate approxim list, that contain basic data for approximation
  xx <- c(
    scen@settings@horizon@intervals$mid[-1] -
    scen@settings@horizon@intervals$mid[-nrow(scen@settings@horizon@intervals)],
    1)
  names(xx) <- scen@settings@horizon@intervals$mid

  if (is.null(arg$fullsets)) fullsets <- TRUE else fullsets <- arg$fullsets
  scen@status$fullsets <- fullsets # !!!???

  # browser()
  approxim <- list(
    region = scen@settings@region,
    year = scen@settings@horizon@period,
    # slice = scen@settings@slice,
    calendar = scen@settings@calendar,
    solver = arg$solver,
    mileStoneYears = scen@settings@horizon@intervals$mid,
    mileStoneForGrowth = xx,
    fullsets = fullsets,
    optimizeRetirement = scen@settings@optimizeRetirement
  )
  approxim$ry <- merge0(
    data.table(region = approxim$region, stringsAsFactors = FALSE),
    data.table(year = approxim$mileStoneYears, stringsAsFactors = FALSE)
  ) |> as.data.table()
  approxim$rys <- merge0(
    approxim$ry,
    data.table(
      slice = approxim$calendar@slice_share$slice,
      stringsAsFactors = FALSE
    )
  ) |> as.data.table()
  approxim$ry <- as.data.table(approxim$ry)
  approxim$rys <- as.data.table(approxim$rys)

  # Basic interpolation parameter from config
  approxim$all_comm <-
    c(lapply(scen@model@data, function(x) {
      c(lapply(x@data, function(y) {
        if (!is(y, "commodity")) {
          return(NULL)
        }
        return(y@name)
      }), recursive = TRUE)
    }), recursive = TRUE)
  names(approxim$all_comm) <- NULL
  scen@modInp <- .read_default_data(scen@modInp, scen@settings)

  # (Optional/experimental) trimming the model == dropping unused dimensions
  if (!is.null(arg$trim) && arg$trim) {
    ## Trim before interpolation
    par_name <- grep("^p", names(scen@modInp@parameters), value = TRUE)
    par_name <- par_name[
      !(par_name %in% c("pEmissionFactor", "pTechEmisComm", "pDiscount"))
      ]
    # Get repository / class structure
    rep_class <- NULL
    for (i in seq_along(scen@model@data)) {
      rep_class <- rbind(
        rep_class,
        data.table(
          repos = rep(i, length(scen@model@data[[i]]@data)),
          class = sapply(scen@model@data[[i]]@data, class),
          name = c(sapply(scen@model@data[[i]]@data, function(x) x@name)),
          stringsAsFactors = FALSE
          )
        )
    }
    # Trim data
    for (pr in par_name) {
      tmp <- scen@modInp@parameters[[pr]]
      if (!is.null(tmp@inClass$class) &&
          (length(tmp@inClass$colName) != 1 || tmp@inClass$colName != "") &&
          length(tmp@dimSets) > 1) {
        # Get prototype
        prot <- new(tmp@inClass$class)
        psb_slot <- getSlots(tmp@inClass$class)
        psb_slot <- names(psb_slot)[psb_slot %in% "data.frame"]
        psb_slot <- psb_slot[!(psb_slot %in% c("defVal", "interpolation"))]
        fl <- sapply(psb_slot, function(x) {
          any(colnames(slot(prot, x)) %in% tmp@inClass$colName)
          })
        if (sum(fl) != 1) stop("Internal error")
        slt <- psb_slot[fl]
        need_col <- tmp@dimSets[tmp@dimSets %in% colnames(slot(prot, slt))]
        if (any(pr == c("pDummyImportCost", "pDummyExportCost")))
          need_col <- need_col[need_col != "comm"]
        if (tmp@type == "numpar") {
          val_col <- tmp@inClass$colName
        } else {
          val_col <- c(tmp@inClass$colName,
                       gsub("[.].*", ".fx", tmp@inClass$colName[1]))
        }
        # Try find reduce column
        rep_class2 <- rep_class[rep_class$class == tmp@inClass$class, ]
        i <- 0
        while (i < nrow(rep_class2) && length(need_col) != 0) {
          i <- i + 1
          tbl <- slot(
            scen@model@data[[rep_class2[i, "repos"]]]@data[[rep_class2[i, "name"]]],
            slt)
          if (nrow(tbl) > 0) {
            # need_col <- need_col[apply(is.na(tbl[apply(!is.na(tbl[, val_col, drop = FALSE]), 1, any), need_col, drop = FALSE]), 2, all)]
            # tb_nna <- tbl |> select(all_of(val_col))
            if (anyDuplicated(colnames(val_col))) browser() # mappings check
            ii <- apply(!is.na(select(all_of(val_col))), 1, any)
            ii <- apply(is.na(filter(tbl, ii)), 2, all)
            need_col <- need_col[ii]
          }
        }
        if (length(need_col) > 0) {
          scen@modInp@parameters[[pr]]@misc$rem_col <-
            seq_along(tmp@dimSets)[tmp@dimSets %in% need_col]
          scen@modInp@parameters[[pr]]@misc$not_need_interpolate <- need_col
          scen@modInp@parameters[[pr]]@misc$init_dim <- tmp@dimSets
          scen@modInp@parameters[[pr]]@dimSets <-
            tmp@dimSets[!(tmp@dimSets %in% need_col)]
          # scen@modInp@parameters[[pr]]@data <-
          #   scen@modInp@parameters[[pr]]@data[,!(colnames(
          #     scen@modInp@parameters[[pr]]@data) %in% need_col), drop = FALSE]
          ii <-
          scen@modInp@parameters[[pr]]@data <- select(
            scen@modInp@parameters[[pr]]@data,
            !(colnames(scen@modInp@parameters[[pr]]@data) %in% need_col)
          )
          if (arg$verbose >= 1) {
            scen@misc$trimDroppedDimensions <- rbind(
              scen@misc$trimDroppedDimensions,
              data.table(parameter = rep(pr, length(need_col)),
                         dimname = need_col, stringsAsFactors = FALSE)
            )
            warning(paste0('Dropping dimension "',
                           paste0(need_col, collapse = '", "'),
                           '" from parameter "', pr, '"'))
          }
        }
      }
    }
  }

  scen@modInp <- .obj2modInp(scen@modInp, scen@settings, approxim = approxim)
  # browser()
  # Add discount data to approxim
  approxim <- .add_discount_approxim(scen, approxim)

  # Update early retirement parameter
  # browser()
  if (!scen@settings@optimizeRetirement) {
    scen <- .remove_early_retirment(scen)
  }
  approxim$debug <- scen@settings@debug
  # Fill slice level for commodity if not defined
  scen <- .fill_default_slice_leve4comm(scen, def.level = approxim$calendar@default_timeframe)
  # Add commodity slice_level map to approxim
  approxim$commodity_slice_map <- .get_map_commodity_slice_map(scen)
  scen@misc$approxim <- approxim

  # Fill set list for interpolation and os one
  # browser()
  scen <- .add_name_for_basic_set(scen, approxim)
  scen@modInp@set <-
    lapply(scen@modInp@parameters[sapply(scen@modInp@parameters,
                                         function(x) x@type == "set")],
           function(x) .get_data_slot(x)[[1]])

  ## interpolate data by year, slice, ...
  # if (arg$echo) cat("Interpolation: \n")
  cat("Interpolating parameters\n")
  interpolation_count <- .get_objects_count(scen) + 46
  len_name <- .get_objects_len_name(scen)
  if (arg$n.threads == 1) {
    # scen <- .add2_nthreads_1(0, 1, scen, arg, approxim,
    #   interpolation_start_time = interpolation_start_time,
    #   interpolation_count = interpolation_count, len_name = len_name
    # )
  } else {
    warning("Multiple threads are not implemented yet,
            ignoring `n.threads` parameter")
    # use_par <- names(scen@modInp@parameters)[sapply(scen@modInp@parameters, function(x) nrow(x@data) == 0)]
    # # require(parallel)
    # cl <- makeCluster(arg$n.threads)
    # scen_pr <- parLapply(
    #   cl, 0:(arg$n.threads - 1), .add2_nthreads_1, arg$n.threads, scen, arg, approxim,
    #   interpolation_start_time, interpolation_count, len_name
    # )
    # stopCluster(cl)
    # scen <- .merge_scen(scen_pr, use_par)
  }
  # browser()
  scen <- .add2_nthreads_1(n.thread = 0, max.thread = 1,
                           scen = scen, arg = arg, approxim = approxim,
                           interpolation_start_time = interpolation_start_time,
                           interpolation_count = interpolation_count,
                           len_name = len_name
                           )

  # Remove duplicates
  scen@modInp@parameters$group <- .unique_set(scen@modInp@parameters$group)
  scen@modInp@parameters$mvDemInp <- .unique_set(scen@modInp@parameters$mvDemInp)

  # browser()
  # Check for unknown set in model and duplicated set
  .check_sets(scen)
  # Check for unknown set in constraints
  .check_constraint(scen)
  # Check for unknown weather
  .check_weather(scen)
  # # Check for unknown set in model and duplicated set
  # .check_sets(scen)

  #!!!ToDo: add NA checks for sets

  # Tune for LEC
  # if (length(scen@model@LECdata) != 0) {
  #   scen@modInp@parameters$mLECRegion <-
  #     addMultipleSet(
  #       scen@modInp@parameters$mLECRegion,
  #       scen@model@LECdata$region
  #     )
  #   if (length(obj@LECdata$pLECLoACT) == 1) {
  #     scen@modInp@parameters$pLECLoACT <- .dat2par(
  #       scen@modInp@parameters$pLECLoAC= TRUE,
  #       data.frame(
  #         region = scen@model@LECdata$region,
  #         value = scen@model@LECdata$pLECLoACT
  #       )
  #     )
  #   }
  # }
  # Reduce mapping
  # browser()
  # mCommReg ####
  # rest <- rest + 1
  # .interpolation_message("mCommReg", rest, interpolation_count,
  #                        interpolation_start_time, len_name)
  # browser()
  # scan all "^p"-parameters for (comm, region)
  # allpar <- names(scen@modInp@parameters)
  # allpar <- allpar[grepl("^p", allpar)] # parameters
  # allpar <- allpar[!grepl("Dummy", allpar)] # drop Dummy-Imp/Exp (for filtering)
  # allpar <- c(allpar, "mSupAva") # add sup with default/missing data
  # mCommReg <- lapply(scen@modInp@parameters[allpar], function(x) {
  #   if (!all(c("comm", "region") %in% x@dimSets)) return(NULL)
  #   select(x@data, comm, region) |> unique()
  # }) |>
  #   rbindlist() |>
  #   unique()
  # browser()
  mCommReg <- fmCommReg(scen@model, scen@settings@region) |>
    filter(region %in% scen@settings@region)
  scen@modInp@parameters[["mCommReg"]] <-
    .dat2par(scen@modInp@parameters[["mCommReg"]], mCommReg)
  # browser()

  # !!!patch: update parameters (drop technologies with unavailable in regs inputs)
  #  ToDo: take into account of other mapping and parameters in new interpolate
  # mCommReg - filter with comm-reg dims
  filter_comreg <- function(p, y = mCommReg) {
    p@data <-
      p@data |>
      dplyr::semi_join(y, by = intersect(colnames(p@data), colnames(y))) |>
      unique()
    p@misc$nValues <- nrow(p@data)
    p
  }
  # filter with "tech-region" dims
  y_tech_reg <- scen@modInp@parameters[["mTechInpComm"]]@data |>
    # rbind(scen@modInp@parameters[["mTechOutComm"]]@data) |>
    rbind(scen@modInp@parameters[["mTechAInp"]]@data) |>
    # rbind(scen@modInp@parameters[["mTechAOut"]]@data) |>
    unique() |>
    right_join(mCommReg, by = "comm") |>
    filter(!is.na(tech)) |>
    select(-comm) |> unique()

  scen@modInp@parameters[["mvTechInp"]] <-
    filter_comreg(scen@modInp@parameters[["mvTechInp"]])
  scen@modInp@parameters[["mvTechOut"]] <-
    filter_comreg(scen@modInp@parameters[["mvTechOut"]], y_tech_reg)
  scen@modInp@parameters[["mvTechAInp"]] <-
    filter_comreg(scen@modInp@parameters[["mvTechAInp"]], y_tech_reg)
  scen@modInp@parameters[["mvTechAOut"]] <-
    filter_comreg(scen@modInp@parameters[["mvTechAOut"]])

  # scen@modInp@parameters[["mTechInpTot"]]
  # y_out <- scen@modInp@parameters[["mTechOutComm"]]@data |>
  #   rbind(scen@modInp@parameters[["mTechAOut"]]@data) |>
  #   unique() |>
  #   right_join(mCommReg, by = "comm") |>
  #   filter(!is.na(tech))
  # browser()
  scen@modInp@parameters[["mTechNew"]] <-
    filter_comreg(scen@modInp@parameters[["mTechNew"]], y_tech_reg)
  # scen@modInp@parameters[["mTechNew"]] |> filter_comreg(y_out)
  scen@modInp@parameters[["mTechSpan"]] <-
    filter_comreg(scen@modInp@parameters[["mTechSpan"]], y_tech_reg)
  # scen@modInp@parameters[["mTechSpan"]] |> filter_comreg(y_out)
  scen@modInp@parameters[["mvTechAct"]] <-
    filter_comreg(scen@modInp@parameters[["mvTechAct"]], y_tech_reg)
  scen@modInp@parameters[["mTechInv"]] <-
    filter_comreg(scen@modInp@parameters[["mTechInv"]], y_tech_reg)
  scen@modInp@parameters[["mTechEac"]] <-
    filter_comreg(scen@modInp@parameters[["mTechEac"]], y_tech_reg)
  # scen@modInp@parameters[["mTechOMCost"]] <-
  #   filter_comreg(scen@modInp@parameters[["mTechOMCost"]], y_tech_reg)
  scen@modInp@parameters[["mTechFixom"]] <-
    filter_comreg(scen@modInp@parameters[["mTechFixom"]], y_tech_reg)
  scen@modInp@parameters[["mTechVarom"]] <-
    filter_comreg(scen@modInp@parameters[["mTechVarom"]], y_tech_reg)
  scen@modInp@parameters[["mTechCapLo"]] <-
    filter_comreg(scen@modInp@parameters[["mTechCapLo"]], y_tech_reg)
  scen@modInp@parameters[["mTechCapUp"]] <-
    filter_comreg(scen@modInp@parameters[["mTechCapUp"]], y_tech_reg)
  scen@modInp@parameters[["mTechNewCapLo"]] <-
    filter_comreg(scen@modInp@parameters[["mTechNewCapLo"]], y_tech_reg)
  scen@modInp@parameters[["mTechNewCapUp"]] <-
    filter_comreg(scen@modInp@parameters[["mTechNewCapUp"]], y_tech_reg)

  # scen@modInp@parameters[["mStorageFixom"]] <-
  #   filter_comreg(scen@modInp@parameters[["mStorageFixom"]], y_tech_reg)
  # scen@modInp@parameters[["mStorageVarom"]] <-
  #   filter_comreg(scen@modInp@parameters[["mStorageVarom"]], y_tech_reg)

  # pTechEac(tech, region, year)
  # scen@modInp@parameters[["pTechEac"]] <-
  #   filter_comreg(scen@modInp@parameters[["pTechEac"]], y_tech_reg)
  # scen@modInp@parameters[["pTechEac"]] <-
  #   filter_comreg(scen@modInp@parameters[["pTechEac"]], y_inp)

  # mTechAct2AInp(tech, comm, region, year, slice)
  scen@modInp@parameters[["mTechAct2AInp"]] <-
    filter_comreg(scen@modInp@parameters[["mTechAct2AInp"]], y_tech_reg)
  # mTechCap2AInp(tech, comm, region, year, slice)
  scen@modInp@parameters[["mTechCap2AInp"]] <-
    filter_comreg(scen@modInp@parameters[["mTechCap2AInp"]], y_tech_reg)
  # mTechNCap2AInp(tech, comm, region, year, slice)
  scen@modInp@parameters[["mTechNCap2AInp"]] <-
    filter_comreg(scen@modInp@parameters[["mTechNCap2AInp"]], y_tech_reg)
  # mTechCinp2AInp(tech, comm, comm, region, year, slice)
  scen@modInp@parameters[["mTechCinp2AInp"]] <-
    filter_comreg(scen@modInp@parameters[["mTechCinp2AInp"]], y_tech_reg)
  # mTechCout2AInp(tech, comm, comm, region, year, slice)
  scen@modInp@parameters[["mTechCout2AInp"]] <-
    filter_comreg(scen@modInp@parameters[["mTechCout2AInp"]], y_tech_reg)
  # mTechAct2AOut(tech, comm, region, year, slice)
  scen@modInp@parameters[["mTechAct2AOut"]] <-
    filter_comreg(scen@modInp@parameters[["mTechAct2AOut"]], y_tech_reg)
  # mTechCap2AOut(tech, comm, region, year, slice)
  scen@modInp@parameters[["mTechCap2AOut"]] <-
    filter_comreg(scen@modInp@parameters[["mTechCap2AOut"]], y_tech_reg)
  # mTechNCap2AOut(tech, comm, region, year, slice)
  scen@modInp@parameters[["mTechNCap2AOut"]] <-
    filter_comreg(scen@modInp@parameters[["mTechNCap2AOut"]], y_tech_reg)
  # mTechCinp2AOut(tech, comm, comm, region, year, slice)
  scen@modInp@parameters[["mTechCinp2AOut"]] <-
    filter_comreg(scen@modInp@parameters[["mTechCinp2AOut"]], y_tech_reg)
  # mTechCout2AOut(tech, comm, comm, region, year, slice)
  scen@modInp@parameters[["mTechCout2AOut"]] <-
    filter_comreg(scen@modInp@parameters[["mTechCout2AOut"]], y_tech_reg)
  # browser()
  # meqTechSng2Sng
  scen@modInp@parameters[["meqTechSng2Sng"]] <-
    filter_comreg(scen@modInp@parameters[["meqTechSng2Sng"]], y_tech_reg)
  # meqTechGrp2Sng
  scen@modInp@parameters[["meqTechGrp2Sng"]] <-
    filter_comreg(scen@modInp@parameters[["meqTechGrp2Sng"]], y_tech_reg)
  # meqTechSng2Grp
  scen@modInp@parameters[["meqTechSng2Grp"]] <-
    filter_comreg(scen@modInp@parameters[["meqTechSng2Grp"]], y_tech_reg)
  # meqTechGrp2Grp
  scen@modInp@parameters[["meqTechGrp2Grp"]] <-
    filter_comreg(scen@modInp@parameters[["meqTechGrp2Grp"]], y_tech_reg)
  # meqTechAfLo
  scen@modInp@parameters[["meqTechAfLo"]] <-
    filter_comreg(scen@modInp@parameters[["meqTechAfLo"]], y_tech_reg)
  # meqTechAfUp
  scen@modInp@parameters[["meqTechAfUp"]] <-
    filter_comreg(scen@modInp@parameters[["meqTechAfUp"]], y_tech_reg)
  # meqTechAfsLo
  scen@modInp@parameters[["meqTechAfsLo"]] <-
    filter_comreg(scen@modInp@parameters[["meqTechAfsLo"]], y_tech_reg)
  # meqTechAfsUp
  scen@modInp@parameters[["meqTechAfsUp"]] <-
    filter_comreg(scen@modInp@parameters[["meqTechAfsUp"]], y_tech_reg)
  # mTechRampUp
  scen@modInp@parameters[["mTechRampUp"]] <-
    filter_comreg(scen@modInp@parameters[["mTechRampUp"]], y_tech_reg)
  # mTechRampDown
  scen@modInp@parameters[["mTechRampDown"]] <-
    filter_comreg(scen@modInp@parameters[["mTechRampDown"]], y_tech_reg)
  # meqTechActSng
  scen@modInp@parameters[["meqTechActSng"]] <-
    filter_comreg(scen@modInp@parameters[["meqTechActSng"]], y_tech_reg)
  # meqTechActGrp
  scen@modInp@parameters[["meqTechActGrp"]] <-
    filter_comreg(scen@modInp@parameters[["meqTechActGrp"]], y_tech_reg)
  # meqTechAfcOutLo
  scen@modInp@parameters[["meqTechAfcOutLo"]] <-
    filter_comreg(scen@modInp@parameters[["meqTechAfcOutLo"]], y_tech_reg)
  # meqTechAfcOutUp
  scen@modInp@parameters[["meqTechAfcOutUp"]] <-
    filter_comreg(scen@modInp@parameters[["meqTechAfcOutUp"]], y_tech_reg)
  # meqTechAfcInpLo
  scen@modInp@parameters[["meqTechAfcInpLo"]] <-
    filter_comreg(scen@modInp@parameters[["meqTechAfcInpLo"]], y_tech_reg)
  # meqTechAfcInpUp
  scen@modInp@parameters[["meqTechAfcInpUp"]] <-
    filter_comreg(scen@modInp@parameters[["meqTechAfcInpUp"]], y_tech_reg)

  # browser()
  # !!! End of patch
  rm(mCommReg)

  # filter parameters by (comm, region)
  filter_params <- c("pDummyExportCost", "pDummyImportCost")
  for (p in filter_params) {
    d <- scen@modInp@parameters[[p]]@data |>
      inner_join(scen@modInp@parameters[["mCommReg"]]@data,
                 by = c("comm", "region"))
    scen@modInp@parameters[[p]]@data <- scen@modInp@parameters[[p]]@data[0,]
    scen@modInp@parameters[[p]] <- .dat2par(scen@modInp@parameters[[p]], d)
  }

  # scen@modInp <- .write_mapping(scen@modInp,
  #   interpolation_count = interpolation_count,
  #   interpolation_start_time = interpolation_start_time,
  #   len_name = len_name
  # )

  scen@modInp <- .make_mapping(
    scen@modInp,
    interpolation_count = interpolation_count,
    interpolation_start_time = interpolation_start_time,
    len_name = len_name,
    scen_settings = scen@settings
  )

  # Clean parameters, need when nValues != -1, and mean that add NA row for speed
  # browser()
  for (i in names(scen@modInp@parameters)) {
    if (scen@modInp@parameters[[i]]@misc$nValues != -1) {
      # scen@modInp@parameters[[i]]@data <- scen@modInp@parameters[[i]]@data[
      #   seq(length.out = scen@modInp@parameters[[i]]@misc$nValues), ,
      #   drop = FALSE
      # ]
      nrow_i <- scen@modInp@parameters[[i]]@data |> nrow()
      scen@modInp@parameters[[i]]@data <- scen@modInp@parameters[[i]]@data |>
        slice_head(n = min(nrow_i,
                           scen@modInp@parameters[[i]]@misc$nValues,
                           na.rm = TRUE)
                   )
    }
  }
  scen@settings@sourceCode <- .modelCode
  scen@status$interpolated <- TRUE
  scen@status$script <- FALSE

  # Check parameters
  scen <- .check_scen_par(scen)
  # if (arg$echo) cat(" ", round(proc.time()[3] - interpolation_start_time, 2), "s\n")
  # cat("\n")
  tictoc::toc()
  invisible(scen)
}


#' Interpolate model
#'
#' @param object model or scenario type of object.
#'
#' @return scenario object with enclosed model (slot `@model`) and interpolated parameters (slot `@modInp`).
#' @export
#' @family interpolate model
#'
#' @examples
#' \dontrun{
#' scen <- interpolate(mod)
#' }
setMethod("interpolate", signature(object = "model"),
  function(object, ...) {
    interpolate_model(object, ...)
})

#' @rdname interpolate
#' @family interpolate scenario
#' @method interpolate scenario
#' @export
setMethod("interpolate", "scenario", function(object, ...) {
  interpolate_model(object, ...)
})

subset_slices <- function(obj, yearFraction = 1, keep_slices = NULL) {
  # subset_hours <- length(SLICE_SUBSET)
  # subset_fraction <- subset_hours/8760
  if (yearFraction == 1) {
    return(obj)
  }
  keep_slices <- unique(c(NA, "ANNUAL", keep_slices))
  if (inherits(obj, "data.frame")) { # unnesecary - reserved for future
    if (any(names(obj) == "slice")) {
      obj <- filter(obj, slice %in% keep_slices)
    }
    return(obj)
  } else if (!isS4(obj)) {
    return(obj)
  }
  # S4
  slot_names <- slotNames(obj)
  ii <- sapply(slot_names, function(x) {
    inherits(slot(obj, x), "data.frame") &&
      any(colnames(slot(obj, x)) == "slice")
  })
  # summary(ii)
  for (s in slot_names[ii]) {
    # stop()
    slot(obj, s) <- slot(obj, s) |>
      filter(slice %in% keep_slices)
  }
  #
  # obj <- fract_year_adj(obj, subset_hours, check = TRUE)
  #
  return(obj)
}

subset_slices_repo <- function(repo, yearFraction = 1, keep_slices = NULL) {
  # browser()
  if (yearFraction == 1) {
    return(repo)
  }
  if (length(repo) == 0) {
    return(repo)
  }
  # browser()
  if (inherits(repo, "list")) {
    # list of repositories
    repo <- lapply(repo, function(o) {
      subset_slices_repo(o, yearFraction, keep_slices)
    })
    return(repo)
  }
  # stopifnot(inherits(repo, c("repository")))
  # if (repo)
  # subset_hours <- length(SLICE_SUBSET)
  # subset_fraction <- subset_hours/8760
  # i <- 19
  # browser()
  if (inherits(repo, c("repository"))) {
    cat(" Repository '", repo@name, "'\n", sep = "")
    x <- repo@data
  } else {
    # x <- repo
    # browser()
    stop("Unrecognized class of object in the repository.")
  }
  p <- progressr::progressor(
    along = names(x)
    # message = paste(" Repository '", repo@name, "'\n", sep = "")
  )
  for (i in 1:length(x)) {
    # print(i)
    obj <- x[[i]]
    p(obj@name)
    if (inherits(obj, c("repository"))) {
      # nested repository - should not be here, check repo@misc$permit
      stop("Nested repositories are not supported. ", obj@name,
           " is a part of another repository.")
      subset_slices_repo(obj, yearFraction, keep_slices)
    } else {
      obj <- subset_slices(obj, yearFraction, keep_slices)
    }
    x[[i]] <- obj
  }
  if (inherits(repo, c("repository"))) {
    repo@data <- x
  } else {
    repo <- x
  }
  return(repo)
}

# Check parameters
.check_scen_par <- function(scen) {
  # Check for non negative parameters, all except 'pAggregateFactor', 'pTechCvarom', 'pTechAvarom', 'pTechVarom', 'pTechInvcost'
  non_negative <- unique(c(
    "pSliceShare", "pSliceWeight", "pTechOlife", "pTechCinp2ginp",
    "pTechGinp2use", "pTechCinp2use", "pTechUse2cact", "pTechCact2cout",
    "pTechEmisComm", "pTechAct2AInp", "pTechCap2AInp", "pTechNCap2AInp",
    "pTechCinp2AInp", "pTechCout2AInp",
    "pTechAct2AOut", "pTechCap2AOut", "pTechNCap2AOut", "pTechCinp2AOut",
    "pTechCout2AOut", "pTechFixom", "pTechShare",
    "pTechShare", "pTechAf", "pTechAf", "pTechAfs", "pTechAfs", "pTechAfc",
    "pTechAfc", "pTechStock", "pTechCap2act", "pDiscount",
    "pDiscountFactor", "pSupCost", "pSupAva", "pSupAva", "pSupReserve",
    "pSupReserve", "pDemand", "pEmissionFactor", "pDummyImportCost",
    "pDummyExportCost", "pTaxCostInp", "pSubCostInp", "pTaxCostOut",
    "pSubCostOut", "pTaxCostBal", "pSubCostBal",
    "pWeather", "pSupWeather", "pSupWeather", "pTechWeatherAf",
    "pTechWeatherAf", "pTechWeatherAfs", "pTechWeatherAfs",
    "pTechWeatherAfc", "pTechWeatherAfc", "pStorageWeatherAf",
    "pStorageWeatherAf", "pStorageWeatherCinp", "pStorageWeatherCinp",
    "pStorageWeatherCout", "pStorageWeatherCout", "pStorageInpEff",
    "pStorageOutEff", "pStorageStgEff", "pStorageStock", "pStorageOlife",
    "pStorageCostStore", "pStorageCostInp",
    "pStorageCostOut", "pStorageFixom", "pStorageInvcost", "pStorageCap2stg",
    "pStorageAf", "pStorageAf", "pStorageCinp", "pStorageCinp", "pStorageCout",
    "pStorageCout", "pStorageStg2AInp", "pStorageStg2AOut", "pStorageCinp2AInp",
    "pStorageCinp2AOut", "pStorageCout2AInp", "pStorageCout2AOut",
    "pStorageCap2AInp", "pStorageCap2AOut", "pStorageNCap2AInp",
    "pStorageNCap2AOut", "pTradeIrEff", "pTradeIr", "pTradeIr",
    "pTradeIrCost", "pTradeIrMarkup", "pTradeIrCsrc2Ainp",
    "pTradeIrCsrc2Aout", "pTradeIrCdst2Ainp", "pTradeIrCdst2Aout",
    "pExportRowRes", "pExportRow",
    # "pExportRowPrice",
    "pImportRowRes", "pImportRow",
    "pImportRow",
    "pTechRet", "pTechCap", "pTechNewCap",
    "pStorageRet", "pStorageCap", "pStorageNewCap",
    "pTradeRet", "pTradeCap", "pTradeNewCap"
    # "pImportRowPrice"
  ))
  # browser()
  msg_small_err <- NULL
  for (i in non_negative) {
    if (any(scen@modInp@parameters[[i]]@data$value < 0)) {
      if (any(scen@modInp@parameters[[i]]@data$value < -1e-7)) {
        msg <- paste0('An attempt to assignin negative numbers
                      to non-negative parameter: "', i)
        tmp <- scen@modInp@parameters[[i]]@data[
          scen@modInp@parameters[[i]]@data$value < 0, ,
          drop = FALSE]
        msg <- c(msg, capture.output(print(tmp[1:min(c(10, nrow(tmp))), ,
                                               drop = FALSE])))

        if (nrow(tmp) > 10) {
          msg <- c(msg,
                   paste0("Showing only the first 10 errors in data, from ",
                          nrow(tmp), "\n")
                   )
        }
        stop(paste0(msg, collapse = "\n"))
      } else {
        msg_small_err <- c(msg_small_err, i)
        scen@modInp@parameters[[i]]@data[
          scen@modInp@parameters[[i]]@data$value > -1e-7 &
          scen@modInp@parameters[[i]]@data$value < 0, "value"] <- 0
      }
    }
  }
  if (length(msg_small_err) > 0) {
    warning(paste0(
      "There small negative value (abs(err) < 1e-7) in parameter",
      "s"[length(msg_small_err) > 1], ': "',
      paste0(msg_small_err, collapse = '", "'), '". Assigned as zerro.'
    ))
  }
  # Check share
  if (nrow(scen@modInp@parameters$pTechShare@data) > 0) {
    # Share check is not working, probably unfinished migration to dplyr & data.table
    # !!! ToDo: rewrite this function.
    # browser()
    # mTechGroupComm <- .get_data_slot(scen@modInp@parameters$mTechGroupComm)
    # # scen@modInp@parameters$pTechShare@data <- merge(scen@modInp@parameters$pTechShare@data, mTechGroupComm)
    # # if (scen@modInp@parameters$pTechShare@misc$nValues != - 1)
    # # 		scen@modInp@parameters$pTechShare@misc$nValues <- nrow(scen@modInp@parameters$pTechShare@data)
    # tmp <- .add_dropped_zeros(scen@modInp, "pTechShare")
    # mTechSpan <- .get_data_slot(scen@modInp@parameters$mTechSpan)
    # browser()
    # tmp <- merge(tmp, mTechSpan)
    # tmp_lo <- merge0(tmp[tmp$type == "lo", , drop = FALSE], mTechGroupComm)
    # tmp_up <- merge0(tmp[tmp$type == "up", , drop = FALSE], mTechGroupComm)
    # tmp_lo <- aggregate(
    #   tmp_lo[, "value", drop = FALSE],
    #   select(tmp_lo, -any_of(c("type", "comm", "value"))),
    #   # tmp_lo[, !(colnames(tmp_lo) %in% c("type", "comm", "value")),
    #   #        drop = FALSE],
    #   sum)
    #   tmp_up <- aggregate(
    #   tmp_up[, "value", drop = FALSE],
    #   tmp_up[, !(colnames(tmp_up) %in% c("type", "comm", "value")),
    #          drop = FALSE],
    #   sum)
    # if (any(tmp_lo$value > 1) || any(tmp_up$value < 1)) {
    #   tech_wrong_lo <- tmp_lo[tmp_lo$value > 1, , drop = FALSE]
    #   tech_wrong_up <- tmp_up[tmp_up$value < 1, , drop = FALSE]
    #   tech_wrong <- unique(c(tech_wrong_up$tech, tech_wrong_lo$tech))
    #   assign("tech_wrong_lo", tech_wrong_lo, globalenv())
    #   assign("tech_wrong_up", tech_wrong_up, globalenv())
    #   stop(paste0(
    #     "Error in share (sum of up < 1 or sum of lo > 1)",
    #     "(see `tech_wrong_lo` and `tech_wrong_up`)",
    #     ' for technology "', paste0(tech_wrong, collapse = '", "'), '"'
    #   ))
    # }
    # fl <- colnames(tmp)[!(colnames(tmp) %in% c("type"))]
    # tmp_cmd <- merge(tmp[tmp$type == "lo", fl, drop = FALSE],
    #                  tmp[tmp$type == "up", fl, drop = FALSE],
    #                  by = fl[fl != "value"])
    # if (any(tmp_cmd$value.x > tmp_cmd$value.y)) {
    #   tech_wrong <- tmp_cmd[tmp_cmd$value.x > tmp_cmd$value.y, , drop = FALSE]
    #   assign("tech_wrong", tech_wrong, globalenv())
    #   stop(paste0(
    #     'Error in share data (tuple (tech, comm, region, year, slice) lo",
    #     " share > up), see `tech_wrong`"',
    #     paste0(unique(tech_wrong$tech), collapse = '", "'), '"'
    #   ))
    # }
  }
  scen
}

.unique_set <- function(obj) {
  if (obj@misc$nValues != -1) {
    # browser()
    obj@data <- obj@data[seq(length.out = obj@misc$nValues), , drop = FALSE]
    obj@data <- obj@data[!duplicated(obj@data), , drop = FALSE]
  }
  obj@data <- obj@data[!duplicated(obj@data), , drop = FALSE]
  if (obj@misc$nValues != -1) {
    obj@misc$nValues <- nrow(obj@data)
  }
  return(obj)
}

# Read default parameter from config
.read_default_data <- function(prec, ss) {
  for (i in seq(along = prec@parameters)) {
    # assign('test', prec@parameters[[i]], globalenv())
    # if (i == 342) browser()
    # if (prec@parameters[[i]]@name == "pTechRet") browser() # DEBUG
    cn <- prec@parameters[[i]]@inClass$colName
    if (!any(!is.na(cn) & cn != "")) next
    # The configuration tables (`ss@defVal` / `ss@interpolation`) store the
    # lower/upper bounds of `bounds` parameters in the `<colName>.lo` /
    # `<colName>.up` columns, whereas `numpar` parameters use `<colName>`
    # directly. Look up the matching column(s) accordingly and only override
    # the registry (modInp) defaults when the configuration actually defines
    # them, otherwise keep the registry values.
    if (as.character(prec@parameters[[i]]@type) == "bounds") {
      lookup <- c(paste0(cn[[1]], ".lo"), paste0(cn[[1]], ".up"))
    } else {
      lookup <- cn
    }
    if (all(lookup %in% colnames(ss@defVal))) {
      prec@parameters[[i]]@defVal <- as.numeric(ss@defVal[1, lookup])
    }
    if (all(lookup %in% colnames(ss@interpolation))) {
      prec@parameters[[i]]@interpolation <-
        as.character(ss@interpolation[1, lookup])
    }
  }
  prec
}

.add_repository <- function(mdl, x) {
  if (is(x, "list")) {
    for (i in seq_along(x)) {
      mdl <- .add.repository(mdl, x[[i]])
    }
  } else {
    mdl <- add(mdl, x)
  }
  mdl
}

.add_discount_approxim <- function(scen, approxim) {
  # browser()
  approxim$discountFactor <- .add_dropped_zeros(scen@modInp,
                                                "pDiscountFactor", FALSE)
  approxim$discount <- .add_dropped_zeros(scen@modInp, "pDiscount", FALSE)
  yy <- approxim$discountFactor
  # ll <- NULL
  # for (rg in unique(yy$region)) {
  # 	l1 <- yy[yy$region == rg, ]
  # 	l1$value <- cumsum(l1$value)
  # 	if (is.null(ll)) ll <- l1 else ll <- rbind(ll, l1)
  # }
  # approxim$discountCum <- ll
  approxim
}
# Get commodity slice map for interpolate
.get_map_commodity_slice_map_obj <- function(obj) {
  xx <- list()
  for (i in seq(along = obj@data)) {
    for (j in seq(along = obj@data[[i]]@data)) { #
      prec <- .add2set(
        prec,
        obj@data[[i]]@data[[j]],
        approxim = approxim)
      if (is(obj@data[[i]]@data[[j]], "commodity")) {
        if (length(obj@data[[i]]@data[[j]]@timeframe) == 0) {
          obj@data[[i]]@data[[j]]@timeframe <-
            approxim$calendar@default_timeframe
        }
        commodity_slice_map[[obj@data[[i]]@data[[j]]@name]] <-
          obj@data[[i]]@data[[j]]@timeframe
      }
    }
  }
}

# Apply func to data in scenario by class and return scenario
.apply_to_code_ret_scen <- function(scen, func, ..., clss = NULL) {
  for (i in seq(along = scen@model@data)) {
    for (j in seq(along = scen@model@data[[i]]@data)) {
      if (is.null(clss) || any(class(scen@model@data[[i]]@data[[j]]) == clss)) {
        scen@model@data[[i]]@data[[j]] <- func(scen@model@data[[i]]@data[[j]],
                                               ...)
      }
    }
  }
  scen
}

# Apply func to data in scenario by class and return list
.apply_to_code_ret_list <- function(scen, func, ..., clss = NULL,
                                    need.name = TRUE) {
  rs <- list()
  for (i in seq(along = scen@model@data)) {
    for (j in seq(along = scen@model@data[[i]]@data)) {
      if (is.null(clss) || any(class(scen@model@data[[i]]@data[[j]]) == clss)) {
        if (need.name) {
          rr <- func(scen@model@data[[i]]@data[[j]], ...)
          rs[[rr$name]] <- rr$val
        } else {
          rs[[length(rs) + 1]] <- func(scen@model@data[[i]]@data[[j]], ...)
        }
      }
    }
  }
  rs
}

# Fill slice level for commodity if not defined
.fill_default_slice_leve4comm <- function(scen, def.level) {
  .apply_to_code_ret_scen(
    scen = scen, clss = "commodity", def.level,
    func = function(x, def.level) {
      if (length(x@timeframe) == 0) x@timeframe <- def.level
      x
    }
  )
}

# Add name for basic set
.add_name_for_basic_set <- function(scen, approxim) {
  # browser()
  for (i in seq(along = scen@model@data)) {
    for (j in seq(along = scen@model@data[[i]]@data)) {
      inRange <- withinHorizon(scen@model@data[[i]]@data[[j]], scen@settings)
      if (!isFALSE(inRange)) { # NULL is allowed
        scen@modInp <- .add2set(scen@modInp,
                                scen@model@data[[i]]@data[[j]],
                                approxim)
      }
    }
  }
  scen
}

.remove_early_retirment <- function(scen) {
  scen <- .apply_to_code_ret_scen(
    scen = scen, clss = "technology",
    func = function(x) {
      x@optimizeRetirement <- FALSE
      x
    }
  )
  scen
}

# Add commodity slice_level map to approxim
.get_map_commodity_slice_map <- function(scen) {
  .apply_to_code_ret_list(
    scen = scen,
    clss = "commodity",
    func = function(x) {
      list(name = x@name, val = x@timeframe)
    }
  )
}



# Implement add0 for all parameters
.add2_nthreads_1 <- function(n.thread, max.thread, scen, arg, approxim,
                             interpolation_start_time, interpolation_count,
                             len_name) {
  # browser()
  # A couple of string for progress bar
  # !!! ToDo: rewrite selection of columns to be compatible with data.table
  num_classes_for_progrees_bar <- sum(c(sapply(scen@model@data,
                                               function(x) length(x@data)),
                                        recursive = TRUE))
  # if (num_classes_for_progrees_bar < 50) {
  #   need.tick <- rep(TRUE, num_classes_for_progrees_bar)
  # } else {
  #   need.tick <- rep(FALSE, num_classes_for_progrees_bar)
  #   need.tick[trunc(seq(1, num_classes_for_progrees_bar, length.out = 50))] <- TRUE
  # }
  # Fill DB main data
  tmlg <- 0
  mnch <- 0
  # cat(rep(" ", len_name), sep = "")
  k <- 0
  time.log.nm <- rep(NA, num_classes_for_progrees_bar)
  time.log.tm <- rep(NA, num_classes_for_progrees_bar)
  mdinp <- list()
  for (i in seq(along = scen@model@data)) {
    cat(" Repository '", names(scen@model@data)[i], "'\n", sep = "")
    nm <- names(scen@model@data[[i]]@data)
    p <- progressr::progressor(along = nm)
    for (j in seq(along = scen@model@data[[i]]@data)) {
      p(nm[j])
      k <- k + 1
      # if (inherits(scen@model@data[[i]]@data[[j]], "costs")) browser()
      if (k %% max.thread == n.thread) {
        tmlg <- tmlg + 1
        if (arg$echo) {
          .interpolation_message(
            scen@model@data[[i]]@data[[j]]@name, k, interpolation_count,
            interpolation_start_time = interpolation_start_time, len_name
          )
        }
        p1 <- proc.time()[3]
        # tryCatch({
        inRange <- withinHorizon(scen@model@data[[i]]@data[[j]], scen@settings)
        if (!isFALSE(inRange)) { # NULL is allowed
          if (inherits(scen@model@data[[i]]@data[[j]], c("constraint", "costs"))) { #isConstraint
            scen@modInp <- .obj2modInp(
              scen@modInp,
              scen@model@data[[i]]@data[[j]],
              approxim = approxim
            )
          } else {
            mdinp[[length(mdinp) + 1]] <- .obj2modInp(
              scen@modInp,
              scen@model@data[[i]]@data[[j]],
              approxim = approxim
            )@parameters
          }
        } else {
          # ignore this obj
        }
        # }, error = function(e) {
        #   assign('add0_message', list(tracedata = sys.calls(),
        #     add0_arg = list(obj = scen@modInp, app = scen@model@data[[i]]@data[[j]], approxim = approxim)),
        #     globalenv())
        #   message('\nError in .obj2modInp function, additional desc in "add0_message" object\n')
        #   stop(e)
        # })
        time.log.nm[tmlg] <- scen@model@data[[i]]@data[[j]]@name
        time.log.tm[tmlg] <- proc.time()[3] - p1
        # if (need.tick[k] && arg$echo) {
        #   cat('.')
        #   flush.console()
        # }
      }
    }
  }
  # browser()
  scen <- .filter_sets(scen)
  # require(data.table)
  nval <- rep(NA, length(mdinp))
  for (pr in names(mdinp[[1]])) { # !!! Rewrite this part with rbindlist
    if (scen@modInp@parameters[[pr]]@misc$nValues <= 0) {
      if (mdinp[[1]][[pr]]@misc$nValues != -1) {
        for (i in seq_along(mdinp)) {
          nval[i] <- mdinp[[i]][[pr]]@misc$nValues
        }
        if (any(nval != 0)) {
          scen@modInp@parameters[[pr]]@data <-
            # as.data.frame(
              rbindlist(
                lapply(
                  mdinp[nval != 0], function(x) {
                    x[[pr]]@data[1:x[[pr]]@misc$nValues, , drop = FALSE]
                  })
                )
          # )
          scen@modInp@parameters[[pr]]@misc$nValues <- sum(nval)
        }
      } else {
        stop("should not be here - debug is required")
      }
    }
  }
  # browser()
  scen@misc$time.log <- data.table(
    name = time.log.nm[seq_len(tmlg)],
    time = time.log.tm[seq_len(tmlg)], stringsAsFactors = FALSE
  )
  # if (arg$echo) cat(' ')
  # if (arg$echo) {
  #   .remove_char(len_name)
  # }
  scen
}

.merge_scen <- function(scen_pr, use_par) {
  if (scen_pr[[1]]@modInp@parameters$mCommSlice@misc$nValues == -1) {
    stop("have to do")
  }
  scen <- scen_pr[[1]]
  scen_pr <- scen_pr[-1]
  for (nm in use_par) {
    hh <- sapply(scen_pr, function(x) x@modInp@parameters[[nm]]@misc$nValues)
    if (sum(hh) != 0) {
      scen@modInp@parameters[[nm]]@data[scen@modInp@parameters[[nm]]@misc$nValues +
                                          sum(hh), ] <- NA
    }
    for (i in seq_along(hh)[hh != 0]) {
      scen@modInp@parameters[[nm]]@data[scen@modInp@parameters[[nm]]@misc$nValues +
                                          1:hh[i], ] <-
        scen_pr[[i]]@modInp@parameters[[nm]]@data[1:hh[i], ]
      scen@modInp@parameters[[nm]]@misc$nValues <-
        scen@modInp@parameters[[nm]]@misc$nValues + hh[i]
    }
  }
  for (i in seq_along(scen_pr)) {
    scen@misc$time.log <- rbind(scen@misc$time.log, scen_pr[[i]]@misc$time.log)
  }

  scen
}

# Implement add0 for all parameters
.get_objects_count <- function(scen) {
  sum(c(sapply(scen@model@data, function(x) length(x@data)), recursive = TRUE))
}
.get_objects_len_name <- function(scen) {
  (25 + max(c(30, max(c(sapply(scen@model@data, function(x) {
    max(if (length(x@data) > 0) sapply(x@data, function(y) nchar(y@name)) else 0)
  }), recursive = TRUE)))))
}

.interpolation_message <- function(name, num, interpolation_count,
                                   interpolation_start_time, len_name) {
  jj <- paste0(
    num, " (", interpolation_count, "),",
    paste0(rep(" ", max(c(1, 15 - (nchar(name) %% 15)))), collapse = ""),
    name, ", time: ", round(proc.time()[3] - interpolation_start_time, 2), "s"
  )
  # bug "invalid langth.out element - workaround
  length_out <- len_name - nchar(jj)
  if (length_out < 0) {
    len_name <- len_name + abs(length_out)
    length_out <- 0
  }
  jj <- paste0(jj, paste0(rep(" ", length_out), collapse = ""))
  # cat(rep_len("\b", len_name), jj, sep = "") # , rep(' ', 100), rep('\b', 100)
}

.remove_char <- function(x) {
  if (is.character(x)) x <- nchar(x)
  if (x == 0) {
    return()
  }
  n1 <- paste0(rep("\b", x), collapse = "")
  cat(n1, paste0(rep(" ", x), collapse = ""), n1, sep = "")
}


.check_constraint <- function(scen) {
  cat("Validating constraints\n")
  # browser()
  # Collect sets data
  sets <- list()
  for (ss in c(
    "tech", "sup", "dem", "stg", "expp", "imp", "trade",
    "group", "comm", "region", "year", "slice"
  )) {
    sets[[ss]] <- .get_data_slot(scen@modInp@parameters[[ss]])[[ss]]
  }
  add_to_err <- function(err_msg, cns, slt, have, psb) {
    if (!all(have %in% psb)) {
      have <- unique(have[!(have %in% psb)])
      have <- have[!is.na(have)]
      tmp <- data.table(value = have, stringsAsFactors = FALSE)
      tmp$slot <- slt
      tmp$constraint <- cns
      return(rbind(err_msg,
                   tmp[, c("constraint", "slot", "value"), drop = FALSE]
                   )
             )
    }
    return(err_msg)
  }
  sets$lag.year <- sets$year
  sets$lead.year <- sets$year
  err_msg <- NULL
  # Check sets in constraints
  # cat(" Repository ", names(scen@model@data)[i], "\n")
  # nm <- names(scen@model@data)
  # p <- progressr::progressor(along = nm)
  for (i in seq_along(scen@model@data)) {
    cat(" Repository '", names(scen@model@data)[i], "'\n", sep = "")
    ii <- sapply(scen@model@data[[i]]@data, class) == "constraint"
    nn <- seq_along(scen@model@data[[i]]@data)[ii]
    if (length(nn) > 0) {
      # cat(" Repository ", names(scen@model@data)[i], "\n")
      # browser()
      p <- progressr::progressor(along = nn)
    }
    # nm <- names(scen@model@data[[i]]@data)
    for (j in nn) {
    # for (j in seq_along(scen@model@data[[i]]@data)[
    #   sapply(scen@model@data[[i]]@data, class) == "constraint"]) {
      # browser()
      tmp <- scen@model@data[[i]]@data[[j]]
      p(tmp@name)
      for (k in colnames(tmp@rhs)) {
        if (k != "value" && k != "year") {
          err_msg <- add_to_err(err_msg, cns = tmp@name, slt = "rhs",
                                have = tmp@for.each[[k]], psb = sets[[k]])
        }
      }
      for (u in seq_along(tmp@lhs)) {
        for (k in colnames(tmp@lhs[[u]]@mult)) {
          if (k != "value" && k != "year") {
            err_msg <- add_to_err(err_msg, cns = tmp@name,
                                  slt = paste0("lhs (", u, ") mult"),
                                  have = tmp@lhs[[u]]@mult[[k]], psb = sets[[k]])
          }
        }
        for (k in names(tmp@lhs[[u]]@for.sum)) {
          if (k != "value" && k != "year" && !all(is.na(tmp@lhs[[u]]@for.sum[[k]]))) {
            err_msg <- add_to_err(err_msg, cns = tmp@name,
                                  slt = paste0("lhs (", u, ") for.sum"),
                                  have = tmp@lhs[[u]]@for.sum[[k]], psb = sets[[k]])
          }
        }
      }
    }
  }
  if (!is.null(err_msg) && nrow(err_msg) > 0) {
    nn <- capture.output(err_msg)
    # print(err_msg); stop("Unknow sets in constrint(s)")
    try({
      err_msg0 <- err_msg |>
        lapply(function(x) {head(unique(x), 3)}) |>
        paste(collapse = "\n   ")
      err_msg0 <- c("   ", err_msg0, "\n...\n",
                    "See 'scen@misc$dropped_data' for details."
                    )
    })
    # browser()
    warning("\nUnused (ignored) sets in constraints: \n", head(err_msg0))
  }
}

.check_weather <- function(scen) {
  cat("Validating weather-sets\n")
  # browser()
  weather <- scen@modInp@parameters$weather@data$weather
  err_msg <- list()
  pars <- names(scen@modInp@parameters)[sapply(scen@modInp@parameters, function(x) {
    !is.null(x@data$weather) &&
      nrow(x@data) != 0
  })]
  # browser()
  p <- progressr::progressor(along = pars)
  for (pr in pars) {
    p(pr)
    tmp <- scen@modInp@parameters[[pr]]@data
    tmp <- tmp[!is.na(tmp$weather) & !(tmp$weather %in% weather), ,
               drop = FALSE]
    if (nrow(tmp) != 0) err_msg[[pr]] <- tmp
  }
  if (length(err_msg) != 0) {
    nn <- capture.output(err_msg)
    stop(paste0("Unknow 'weather' set in parameters\n", paste0(nn, collapse = "\n")))
  }
}

# unrecognized sets ####
.check_sets <- function(scen) {
  cat("Validating sets\n")
  # browser()
  lsets <- lapply(scen@modInp@parameters, function(x) {
    if (x@type == "set") .get_data_slot(x)[[1]]
  })
  lsets <- lsets[!sapply(lsets, is.null)]
  # Add alias for set
  lsets$src <- lsets$region
  lsets$dst <- lsets$region
  dset <- unique(c(lapply(scen@modInp@parameters,
                          function(x) x@dimSets), recursive = TRUE))
  dset <- dset[!(dset %in% names(lsets))]
  for (ss in dset) {
    i <- 1
    while (i <= length(lsets) && length(grep(names(lsets)[i], ss)) != 1) i <- i + 1
    if (i > length(lsets)) stop("Internal error. Alias problem")
    lsets[[ss]] <- lsets[[i]]
  }

  error_duplicated_value <- NULL
  err_dtf <- NULL
  int_err <- NULL

  nm <- names(scen@modInp@parameters)
  p <- progressr::progressor(along = nm)
  for (prm in scen@modInp@parameters) {
    p(prm@name)
    # if (grepl("CESR_.+_4", prm@name)) browser()
    if (!all(prm@dimSets %in% names(lsets))) {
      int_err <- unique(c(int_err, prm@dimSets[!(prm@dimSets %in% names(lsets))]))
    } else {
      # tmp <- .get_data_slot(prm)[, prm@dimSets, drop = FALSE]
      tmp <- .get_data_slot(prm) |> select(prm@dimSets)
      for (ss in prm@dimSets) {
        unq <- unique(tmp[[ss]])
        fl <- !(unq %in% lsets[[ss]])
        if (any(fl)) {
          # isTRUE(options("en_debug")) browser()
          err_dtf <- rbind(err_dtf,
                           data.table(name = prm@name, set = ss, value = unq[fl]
                                      )
                           )
        }
      }
      # tmp <- .get_data_slot(prm)[, colnames(prm@data) != "value", drop = FALSE]
      tmp <- .get_data_slot(prm) |> select(-any_of("value"))
      tmp <- tmp[duplicated(tmp), , drop = FALSE]
      # tmp <- filter(duplicated(tmp)) # need if_empty check
      if (nrow(tmp) != 0) {
        error_duplicated_value <- rbind(
          error_duplicated_value,
          data.table(name = prm@name,
                     value = apply(tmp, 1, paste0, collapse = "."))
        )
      }
    }
  }
  if (length(int_err) != 0) {
    err_msg0 <- paste0('Internal error. Unknown set "',
                  paste0(int_err, collapse = '", "'), '"')
    if (isFALSE(getOption("en.debug"))) {
      message("Use 'option(en.debug = TRUE)' to ignore this error")
      stop(err_msg0)
    } else {
      message(err_msg0)
      browser()
    }
  }
  if (!is.null(err_dtf)) {
    assign("unknown_sets", err_dtf, globalenv())
    # isTRUE(options("en_debug")) browser()
    err_msg <- c(
      "Unknown sets (see unknown_sets in .globalenv)\n",
      paste0(capture.output(print(head(err_dtf))), collapse = "\n")
    )
    if (nrow(head(err_dtf)) != nrow(err_dtf)) {
      err_msg <- c(err_msg, paste0("\n", nrow(err_dtf) - nrow(head(err_dtf)),
                                   " row(s) dropped"))
    }

    if (!isTRUE(getOption("en.debug"))) {
      message("Use 'option(en.debug = TRUE)' to ignore this error")
      stop(err_msg)
    } else {
      message(err_msg)
    }
  }
  if (!is.null(error_duplicated_value)) {
    assign("error_duplicated_value", error_duplicated_value, globalenv())
    err_msg <- c(
      "Duplicated sets/values (see error_duplicated_value in globalenv)\n",
      paste0(capture.output(print(head(error_duplicated_value))),
             collapse = "\n")
    )
    if (nrow(head(error_duplicated_value)) != nrow(error_duplicated_value)) {
      err_msg <- c(err_msg, paste0("\n",
                                   nrow(error_duplicated_value) -
                                     nrow(head(error_duplicated_value)),
                                   " row(s) dropped"))
    }
    # stop(err_msg)
    if (isFALSE(getOption("en.debug"))) {
      message("Use 'option(en.debug = TRUE)' to ignore this error")
      stop(err_msg)
    } else {
      message(err_msg)
    }
  }
}

# the function checks if the object (from repository)
# is within the settings@horizon@period, returns TRUE if it is
# FALSE, if beyond the period, and NULL if the object cannot be checked
withinHorizon <- function(obj, settings) {
  # return(T)
  # browser()
  # if (inherits(obj, "trade")) browser()
  if (inherits(obj, "constraint")) return(NULL)
  # if (T) { ## Debug
  #   if (grepl("", obj@name)) browser()
  # }
  # yrs <- range()
  yrs <- settings@horizon@period
  ret <- NULL # return NULL if not applicable to the object
  # check stock
  sn <- slotNames(obj)
  if (any(sn == "capacity")) {
    stock <- obj@capacity |>
      select(any_of(c("region", "year")), stock) |>
      filter(!is.na(stock)) |> unique()
    # stock <- obj@stock # !!! add check for interpolation rule or interpolate first
    if (nrow(stock) > 0) {
      if (all(is.na(stock$year))) return(TRUE) # years are not defined
      if (any(stock$year >= min(yrs)) && any(stock$stock[!is.na(stock$stock)] > 0)) {
        return(TRUE) # capacity exists within the period
      } else {
        ret <- FALSE
      }
    }
  }
  if (any(sn == "end")) {
    if (is.data.frame(obj@end)) {
      end <- obj@end$end
    } else {
      end <- obj@end
    }

    if (is.null(end) || is_empty(end)) {
      end <- TRUE
    } else if (any(is.na(end))) { # at least in one region
      end <- TRUE
    } else if (!all(end < min(yrs))) {
      end <- TRUE
    } else {
      end <- FALSE
      return(FALSE) # not available for investment
    }

    # if (end == TRUE) { supposed to be true
    if (is.data.frame(obj@start)) {
      start <- obj@start$start
    } else {
      start <- obj@start
    }
    if (is.null(start) || is_empty(start)) {
      start <- TRUE
    } else if (any(is.na(start))) { # at least in one region
      start <- TRUE
    } else if (!all(start > max(yrs))) {
      start <- TRUE
    } else {
      start <- FALSE
      return(FALSE) # not available for investment
    }

    if (end & start) return(TRUE)
    ret <- FALSE
    # }
  }
  return(ret)
}

# filter data for non-valid sets
.filter_sets <- function(scen) {
  # browser()
  dropped <- list() # log removed data
  set_names <- names(scen@modInp@set)
  for (i in seq_along(scen@modInp@parameters)) {
    x <- scen@modInp@parameters[[i]]
    ii <- x@dimSets %in% set_names
    for (s in x@dimSets[ii]) {
      kk <- x@data[[s]] %in% scen@modInp@set[[s]]
      if (!all(kk)) {
        dropped[[x@name]] <- x@data[!kk,]
        x@data <- x@data[kk,]
        x@misc$nValues <- x@misc$nValues - length(sum(!kk))
        scen@modInp@parameters[[i]] <- x
      }
    }
  }
  scen@misc$dropped_data <- dropped
  scen
}

interpolate_slot <- function(
    x,
    keys = c("region", "slice", "comm", "acomm", "tech", "process",
             "weather", "stg", "sub", "dst", "src"),
    year_seq = NULL,
    val = "value"
) {
  # browser()
  if (!is.null(x$year)) {
    # year_seq = full_seq(c(x$year, year_seq), 1)
    if (is.null(year_seq)) year_seq = full_seq(x$year, 1)
    x <- x |>
      group_by(
        across(any_of(keys))
      ) |>
      complete(year = year_seq) |>
      ungroup()
    if (!is.null(val) && !is.na(val)) {
      x <- x |>
        mutate(
          {{val}} := zoo::na.approx(.data[[val]], x = year)
        )
    }
  }
  # if (is.null(year_seq)) year_seq = full_seq(x$year, 1)
    #
    # mutate(
    #   {{val}} := zoo::na.approx(.data[[val]], x = year)
    # ) |>
    # as.data.table() |>
    # ungroup()
  x
}
