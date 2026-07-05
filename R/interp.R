#' Interpolate a model into a solver-ready scenario
#'
#' @description
#' Builds an interpolated [scenario] from a [model] via the mapping pipeline:
#' collects sets from the model objects, builds the membership / calendar /
#' lifespan / value / constraint / cost mappings, extracts and interpolates the
#' numeric parameters over the milestone years, and (optionally) folds, prunes
#' and validates the result. The returned scenario is ready for [solve_model()] /
#' [solve_scenario()].
#'
#' @param mod a [model] object, or a [scenario] (its `@model` is re-interpolated).
#' @param name character scenario name. If `NULL`, a default `scen_<model>` is
#'   used (with a warning).
#' @param ... additional energyRt objects folded into the model BEFORE the
#'   pipeline runs: `settings`, `config`, `calendar`, `horizon`, a whole
#'   `repository`, or individual model "bricks" (`technology`, `commodity`,
#'   `storage`, ...). This is how a scenario overrides or extends the model (e.g.
#'   pass a sampled `calendar` to interpolate on a reduced time resolution).
#' @param desc character scenario description.
#' @param ondisk logical; store each parameter's data in the on-disk parameter
#'   store rather than the in-memory `@data` slot. `FALSE` (default) keeps data in
#'   memory, which the solver writers read directly; `TRUE` suits very large
#'   models (data is materialised back to memory at solve time).
#' @param overwrite logical; overwrite an existing on-disk scenario of the same
#'   name.
#' @param fold logical or character; whole-column "fold" of trimmable dimensions
#'   to NA wildcards to shrink the data. `TRUE` folds `region` + `slice`; `FALSE`
#'   (default) folds nothing; a character vector selects dims among
#'   `region`, `slice`, `year`, `comm`, `tech`, `stg`, `trade`. A folded scenario
#'   is expanded to solver-ready form at solve time.
#' @param sparse logical; the storage knob. `TRUE` drops `value == defVal` rows
#'   (and folds); `FALSE` materialises the default over each parameter's full
#'   domain (and unfolds).
#' @param prune logical; drop interpolated rows that fall outside the
#'   equation-domain maps (no effect on the solution, smaller data).
#' @param validate logical; run post-interpolation consistency checks (schema,
#'   duplicate keys, map/parameter coverage).
#' @param code optional named list overriding solver source-code blocks
#'   (`GLPK`, `GAMS`, `JuMP`, `PYOMOConcrete`, ...), each either a script-file
#'   path or a character vector of lines. Lets a model-script version be supplied
#'   at interpolation time without rebuilding `sysdata` (handy to A/B templates).
#' @param verbose logical; print per-step progress.
#'
#' @return an interpolated [scenario] object.
#' @seealso [solve_model()], [solve_scenario()], the `interpolate` S4 method.
#' @family interpolation
#' @export
interpolate_model <- function(mod, name = NULL, ...,
                       desc = NULL, ondisk = FALSE, overwrite = FALSE,
                       fold = FALSE, sparse = TRUE, prune = TRUE,
                       validate = TRUE, code = NULL,
                       verbose = getOption("energyRt.verbose", FALSE)) {
  # Accept a scenario (re-interpolate its model), matching the legacy interface.
  if (inherits(mod, "scenario")) mod <- mod@model
  # `...` (after `mod`/`name`) accepts ANY energyRt objects -- settings, config,
  # calendar, horizon, whole repositories, or individual model "bricks"
  # (technology/commodity/...) -- and folds them into the model BEFORE the
  # mapping pipeline runs, reproducing the legacy `interpolate_model(object,...)`
  # interface. All controls (desc, ondisk, overwrite, fold, sparse, prune,
  # validate, code, verbose) live AFTER `...` and must therefore be named.
  # `code`: optional override of solver source-code blocks, so a model-script
  # version can be supplied at interpolation time WITHOUT rebuilding sysdata
  # (handy to A/B test template versions). A named list mapping a block name
  # (GLPK, GAMS, JuMP, PYOMOConcrete, ...) to either a script-file path or a
  # character vector of lines. See the override applied after `.modelCode` below.
  # mod - model
  # `sparse` is the single storage knob: TRUE drops `value==defVal` rows (and
  # folds); FALSE materialises defVal over each parameter's domain (and unfolds).
  # `drop_default` is the internal strip-component of `sparse`, not user-facing.
  # Upgrade any constraint summands serialized before the `timeframe` slot so
  # legacy models interpolate without a "no slot of name timeframe" error.
  mod <- .upgrade_model_summands(mod)

  drop_default <- isTRUE(sparse)
  # `fold` selects which dimensions to whole-column fold: TRUE -> region + slice
  # (default), FALSE -> none, or a character vector of foldable dims (region,
  # slice, year, comm, tech, stg, trade).
  fold_dims <- if (isTRUE(fold)) c("region", "slice")
    else if (is.null(fold) || isFALSE(fold)) character(0)
    else intersect(as.character(fold), .foldable_dims)
  scen <- new("scenario")

  # scenario name
  if (is.null(name)) {
    name <- paste0("scen_", mod@name)
    warning("Scenario name is not provided. Using default: ", name)
  }
  scen@name <- name

  if (!is.null(desc)) {
    # scen@description <- desc
    # scen@desc <- desc
  }

  # !!! ... process arguments, add settings, ...
  args <- list(...)

  # Scenario folder: an explicit `path` wins; otherwise a "smart" folder name
  # {scenario}_{model}_{calendar}_{horizon} (from the model's own
  # calendar/horizon here; recomputed from the final settings at the end to
  # reflect any `...` overrides).
  explicit_path <- !is.null(args$path)
  if (explicit_path) {
    scen@path <- args$path |> .fix_path()
    args$path <- NULL
  } else {
    scen@path <- fp(get_scenarios_path(), .scenario_dir_name(
      scen@name, mod@name, mod@config@calendar@name, mod@config@horizon@name
    )) |> .fix_path()
  }

  # scenario directory ####
  if (ondisk) {
    scen <- mark_ondisk(scen)
    # (!!! model is assumed to be in memory yet -- address later)
    if (dir.exists(scen@path)) {
      if (!overwrite) {
        stop(
          "Scenario directory already exists. ",
          "Use `overwrite = TRUE` to overwrite it, ",
          "or `ondisk = FALSE` to create a scenario in memory, ",
          "or provide a different path.\n"
        )
      } else {
        # add log/message
        # remove existing scenario directory
        unlink(scen@path, recursive = TRUE)
      }

      dir.create(scen@path, recursive = TRUE)

    } else {
      # add log/message
      dir.create(scen@path, recursive = TRUE)
    }
  } else {
    scen <- mark_inMemory(scen)
    mi_path <- NULL
  }
  .interp_banner(scen, sparse, prune, fold_dims, validate, ondisk, verbose)
  # isOnDisk(scen)
  # Parameter path resolver. In-memory scenarios (`ondisk = FALSE`, `mi_path`
  # NULL) must return NULL so `d2p` keeps parameters in memory rather than
  # marking them on-disk with a bare (non-existent) path.
  fmp <- function(x) {
    if (is.null(mi_path)) return(NULL)
    fp(mi_path, x)
  }

  if (F) {
    # debug
    # library(energyRt)
    devtools::load_all(".")
    (load("tmp/utopia-mod.RData"))
    # fix trade objects
    repo <- utopia@model@data$utopia_repository
    repo
    new_varom <- newTrade("")@varom
    for (o in repo@data) {
      if (inherits(o, "trade")) {
        # browser()
        # !!! ToDo: modify update function to check columns
        # varom <- newTrade("")@varom |> full_join(o@varom)
        # o <- update(o, varom = varom)
        o@varom <- full_join(new_varom, o@varom)
        repo@data[[o@name]] <- o
      }
    }
    repo@data$TRBD_ELC_R1_R2@varom
    utopia@model@data$utopia_repository <- repo
    # save(utopia, file = "tmp/utopia-mod.RData")
    # end of fix
    mod <- utopia@model
    scen <- new("scenario")
    scen@name <- "utopia_new_interpolation"
    scen@path <- fp(get_scenarios_path(), scen@name) |> .fix_path()
    slotNames(scen)
    solution_type <- "foresight"

    ondisk <- TRUE

    ECOA <- mod@data$utopia_repository@data$ECOA
  }

  # class(mod)
  scen@model <- mod

  # import settings from mod@config
  scen@settings <- .config_to_settings(scen@model@config, scen@settings)

  # config object -> settings (legacy parity)
  ii <- vapply(args, function(x) inherits(x, "config"), logical(1))
  if (sum(ii) > 1) {
    stop("Only one config object is allowed in the arguments")
  } else if (sum(ii) == 1) {
    scen@settings <- .config_to_settings(args[[which(ii)]], scen@settings)
    args <- args[!ii]
  }

  # settings object (overrides config-derived settings)
  ii <- vapply(args, function(x) inherits(x, "settings"), logical(1))
  if (sum(ii) > 1) {
    stop("Only one settings object is allowed in the arguments")
  } else if (sum(ii) == 1) {
    scen@settings <- args[[which(ii)]]
    args <- args[!ii]
  }

  # calendar object -> settings@calendar (correct slot; the pipeline reads
  # scen@settings@calendar@slice_share below)
  ii <- vapply(args, function(x) inherits(x, "calendar"), logical(1))
  if (sum(ii) > 1) {
    stop("Only one calendar object is allowed in the arguments")
  } else if (sum(ii) == 1) {
    scen@settings@calendar <- args[[which(ii)]]
    args <- args[!ii]
  }

  # horizon object -> settings@horizon (via setHorizon; the pipeline reads
  # scen@settings@horizon@intervals below)
  ii <- vapply(args, function(x) inherits(x, "horizon"), logical(1))
  if (sum(ii) > 1) {
    stop("Only one horizon object is allowed in the arguments")
  } else if (sum(ii) == 1) {
    scen <- setHorizon(scen, args[[which(ii)]])
    args <- args[!ii]
  }

  # whole repositories -> added to the model
  ii <- vapply(args, function(x) inherits(x, "repository"), logical(1))
  if (any(ii)) {
    for (repo in args[ii]) mod <- add(mod, repo, overwrite = overwrite)
    args <- args[!ii]
  }

  # model "bricks" (technology/commodity/...) -> bundled into a repository and
  # added to the model so the set/parameter collection below picks them up
  ii <- vapply(args, function(x) inherits(x, newRepository("")@permit), logical(1))
  if (any(ii)) {
    scen_specific_repo <- newRepository(name = paste0(scen@name, "_repo")) |>
      add(args[ii], overwrite = overwrite)
    mod <- add(mod, scen_specific_repo, overwrite = overwrite)
    args <- args[!ii]
  }
  scen@model <- mod # keep scen@model in sync with the (possibly extended) model

  # update individual settings slots from named args -- !!! ToDo (legacy parity:
  # discount, region, discountFirstYear, optimizeRetirement, defValue,
  # interpolation, debug). Pass a full settings/config object for now.

  # warn on any unrecognized leftover `...` argument (typo / misrouted control)
  if (length(args) > 0L) {
    nm <- names(args)
    if (is.null(nm)) nm <- rep("", length(args))
    nm[!nzchar(nm)] <- "<unnamed>"
    warning("interp_mod(): ignoring unrecognized argument(s): ",
            paste(nm, collapse = ", "), call. = FALSE)
  }

  # guard: the mapping pipeline needs a non-empty horizon
  if (nrow(scen@settings@horizon@intervals) == 0L) {
    stop("The model has no horizon. Set one before interpolating, e.g. ",
         "`mod <- setHorizon(mod, 2020:2050)`, or pass a horizon object via ",
         "`...`: `interp_mod(mod, name, newHorizon(period = ...))`.",
         call. = FALSE)
  }

  # guard: every region referenced by a model object must be declared. Catches
  # typos / stray regions (e.g. an offshore "ES_off" in object data that is not
  # a declared region) up front, instead of surfacing as an obscure
  # "<param> ... out of domain" error deep in the solver writer.
  .check_declared_regions(scen@model, scen@settings@region)

  # !!! ToDo:
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
  # if (!dir.exists(scen@path)) {
  #   # add log/message
  #   dir.create(scen@path, recursive = TRUE)
  # }
  # if (!dir.exists(mi_path) && ondisk) {
  #   # add log/message
  #   dir.create(mi_path, recursive = TRUE)
  # }

  # slotNames(mi)
  # names(mi@parameters)
  # names(mi@set) # !!! rename to "sets"

  sets_from_settings <- c("region", "year", "slice") # from settings

  sets_from_model <- c(
    "comm", "sup", "dem", "tech",
    "group", "stg", "expp", "imp", "trade", "weather"
  )

  all_sets <- c(sets_from_settings, sets_from_model)
  scen@modInp@sets$set_names <- all_sets

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

  # Mapping sets ####
  # The membership MAPS (mSupComm, mImpComm, mDemComm, mExpComm, mTradeComm,
  # mStorageComm, mTechInpComm, mTechOutComm and the aux maps mTech/StorageAInp/
  # AOut) are built from these sets by R/map_membership.R via
  # build_mappings(recipes = "membership") below. Here we only populate the
  # `*_comm` sets that those map builders read.
  scen@modInp@sets[["supply_comm"]] <- get_process_outputs(scen, classes = "supply")
  scen@modInp@sets[["import_comm"]] <- get_process_outputs(scen, classes = "import")
  scen@modInp@sets[["demand_comm"]] <- get_process_inputs(scen, classes = "demand")
  scen@modInp@sets[["export_comm"]] <- get_process_inputs(scen, classes = "export")
  scen@modInp@sets[["trade_comm"]] <- get_process_inputs(scen, classes = "trade")
  scen@modInp@sets$storage_comm <- get_process_inputs(scen, classes = "storage")
  scen@modInp@sets$tech_input_comm <- get_process_inputs(scen, classes = "technology")
  scen@modInp@sets$tech_output_comm <- get_process_outputs(scen, classes = "technology")

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

  # Build the membership maps then the mCommReg closure from the `*_comm` /
  # process sets above (R/map_membership.R, R/map_closure.R). The closure also
  # populates the primary/secondary/comm_region sets and validates commodity
  # reachability. Formerly built inline here (archived: drafts/legacy-mapping/).
  .interp_step(verbose, "building maps: membership + closure")
  scen <- build_mappings(scen, fmp = fmp, recipes = c("membership", "closure"))

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
  scen@modInp@sets$process_years <- get_process_years(scen)

  # scen@modInp@sets$process_years <- rbind(
  #   scen@modInp@sets$process_invest_year,
  #   scen@modInp@sets$process_stock_year
  # ) |>
  #   unique() |>
  #   arrange(process, year) |>
  #   as.data.table()

  #============================================================================#
  # Settings-derived parameters ####
  #   Set members (year / region / slice) and the calendar / horizon / discount
  #   numerics (pPeriodLen, pDiscount, pDiscountFactor, pSliceShare,
  #   pSliceWeight, pYearFraction, ordYear, cardYear, ...). Built before the
  #   calendar recipe (so any overlapping calendar maps are subsequently
  #   refreshed by the recipe) and before `interpolate_parameters()` (discount
  #   factors feed value interpolation).
  .interp_step(verbose, "settings parameters (discount, period length, slice share)")
  scen <- .interp_settings_params(scen)

  #============================================================================#
  # Structural mappings (calendar + lifespan) ####
  #   Built by the spec-driven recipe engine. These are interpolation-
  #   independent: calendar maps derive from settings, lifespan maps from the
  #   investment/stock windows computed above. (Membership and closure maps are
  #   still built inline above; they will be migrated to the engine later.)
  .interp_step(verbose, "building maps: calendar + lifespan")
  scen <- build_mappings(scen, fmp = fmp,
                         recipes = c("calendar", "lifespan"))

  # Log/message meta-data of the scenario ####
  # !!! ToDo: log/message
  # number of regions, commodities, processes, years, slices

  #============================================================================#
  # Parameters from model objects ####
  #   Collect raw (uninterpolated) parameters from the model's objects
  #   by applying ob2mi method to each model object.
  #   `classes <- NULL` processes every object in `scen@model@data`. Objects
  #   that live in `scen@settings` (horizon, calendar, settings) are not part
  #   of `scen@model@data`, so they are not dispatched here.
  classes <- NULL

  .n_obj <- sum(vapply(scen@model@data, function(r) length(r@data), integer(1)))
  .interp_step(verbose, paste0("ob2mi: interpolating ", .n_obj, " model objects"),
               oneline = FALSE)
  .prg <- if (.n_obj > 0) progressr::progressor(steps = .n_obj) else NULL
  for (i in seq(along = scen@model@data)) {
    for (j in seq(along = scen@model@data[[i]]@data)) {
      if (is.null(classes) || inherits(scen@model@data[[i]]@data[[j]], classes)) {
        # Advance the bar for EVERY object (incl. skipped ones) so it reaches
        # 100%; otherwise it freezes short of the end and looks stuck.
        if (!is.null(.prg)) .prg(message = scen@model@data[[i]]@data[[j]]@name)
        # User constraints are compiled later (.interp_user_constraints), after
        # the variable domain maps they reference (e.g. mTechNew) are built.
        if (inherits(scen@model@data[[i]]@data[[j]], "constraint")) next
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
  .interp_step(verbose, "interpolating parameters over milestone years")
  scen <- interpolate_parameters(scen, drop_default = drop_default)

  # Restrict interpolated value parameters to the scenario's declared calendar
  # slices (drops excess data for slices a sampled calendar does not declare;
  # required for GAMS correctness). Legacy interpolate_model did this by default.
  .interp_step(verbose, "calendar-filter: restricting parameters to declared slices")
  scen <- .filter_params_by_declared_slices(scen, verbose)

  #============================================================================#
  # Equivalent annual cost (EAC) ####
  #   Annuitise investment cost into pTechEac / pStorageEac / pTradeEac. Must run
  #   after interpolation (reads pXInvcost / pDiscount / pXOlife) and before the
  #   value recipe (mTradeEac reads pTradeEac's value domain). The generic ob2mi
  #   slot loop leaves these equal to the raw invcost.
  .interp_step(verbose, "computing equivalent annual costs (EAC)")
  scen <- compute_eac_parameters(scen)

  #============================================================================#
  # Value-derived mapping parameters ####
  #   Built after interpolation because they project the (interpolated) cost /
  #   value parameters onto their lifespan windows.
  .interp_step(verbose, "building maps: value, filter, constraint, cost")
  scen <- build_mappings(scen, fmp = fmp, recipes = "value")

  #============================================================================#
  # Filter / activity-domain mapping parameters ####
  #   Built after value maps because they join the operation windows, the
  #   membership commodity maps, the per-object slice maps and the commodity-
  #   region closure (and reuse mSupSpan from the value recipe).
  scen <- build_mappings(scen, fmp = fmp, recipes = "filter")

  #============================================================================#
  # Constraint / equation-domain mapping parameters ####
  #   Built after the filter recipe because they restrict the activity / flow
  #   domains to the equations that reference them. Several constraint maps that
  #   share a derivation with a filter map are built during the filter recipe;
  #   the rest are reported pending until implemented.
  scen <- build_mappings(scen, fmp = fmp, recipes = "constraint")

  #============================================================================#
  # Cost-aggregation mapping parameters ####
  #   Top-level cost domains (mvTotalCost, mvTotalUserCosts) projected onto the
  #   full region x year grid (or the union of user-cost footprints). Built last
  #   because mvTotalUserCosts reads the interpolated user-cost (mCosts*) maps.
  scen <- build_mappings(scen, fmp = fmp, recipes = "cost_agg")

  #============================================================================#
  # User-defined constraints ####
  #   Compile each `constraint` object to the GAMS-string IR (+ pCns/mCns
  #   params) now that all variable domain maps exist. See ob2mi("constraint").
  scen <- .interp_user_constraints(scen, verbose)

  #============================================================================#
  # Prune parameters ####
  #   Drop `value == prune$value` rows of parameters flagged `prune` in
  #   modInp.yml (e.g. pWeather: the 0 night-slice rows). Lossless: an absent
  #   tuple reads as the default, which equals the pruned value. The dependent
  #   variable-column removal is multimod `trim`'s cascade, not done here. The
  #   `prune` argument is a global on/off over the per-parameter flags.
  if (isTRUE(prune)) {
    .interp_step(verbose, "prune: dropping flagged default-valued rows")
    scen <- prune_parameters(scen)
  }

  #============================================================================#
  # Fold parameters ####
  #   Collapse trimmable dimensions (region, slice) of interpolated numpar /
  #   bounds parameters to wildcard (NA) rows wherever the value does not vary
  #   across the entity's full membership of that dimension. Runs after the
  #   filter recipe so the per-object slice/region membership maps exist. The
  #   reverse operation (`unfold`) is applied at read time in `getData()`.
  if (length(fold_dims) > 0 && isTRUE(sparse)) {
    # Fold value parameters (numpar/bounds) to wildcards, but MATERIALISE the maps
    # so every variable / equation domain stays over explicit members. Folded
    # value parameters carry their single value at the artificial set member
    # (ANYREGION / ANYSLICE / 0 for year / ...), substituted into the model code at
    # write time (apply_fold_artificial); the maps must never reference that member.
    # Record the pre-fold value-parameter total so `model_size` can report the
    # exact rows folding saved (membership re-expansion under-counts entity dims).
    scen@misc$fold_dims <- fold_dims
    scen@misc$fold_rows_before <- .value_param_rows(scen)
    .interp_step(verbose, paste0("fold: folding ", paste(fold_dims, collapse = ", ")))
    scen <- fold_scenario_parameters(scen, dims = fold_dims)
    # Maps must ALWAYS materialise to explicit members on every foldable dim --
    # some maps carry NA wildcards (built mid-pipeline on region-folded params)
    # independently of which value-parameter dims are being folded now.
    scen <- unfold_scenario_parameters(scen, dims = .foldable_dims, types = "map")
  } else {
    # Materialise any source / interpolated wildcard (NA) rows in the trimmable
    # dimensions to explicit members, so the written model carries no NAs.
    scen <- unfold_scenario_parameters(scen, dims = .foldable_dims)
    # Densify (sparse = FALSE): now that wildcards are explicit, materialise each
    # parameter's finite non-zero defVal over its domain for backends without a
    # native default (GAMS). Runs before the clip so over-covered rows are pruned.
    if (!isTRUE(sparse)) {
      .interp_step(verbose, "densify: materialising defaults over the domain")
      scen <- densify_parameters(scen)
    }
    # Drop value-parameter rows outside the equation-domain maps (lifespan
    # window x membership). The maps are the minimal authority on the domain, so
    # any remaining row no map indexes is dead data (and may still carry a stale
    # wildcard NA that is out-of-domain in the solver).
    scen <- trim_parameters_by_maps(scen)
  }

  # Materialise wildcard (NA) trade route endpoints (`src` / `dst`) to the
  # explicit route pairs of each trade. Route endpoints are not foldable, so
  # this runs in both fold modes: the equations look trade parameters up over
  # maps that carry the explicit endpoints, and an unmaterialised wildcard would
  # silently resolve to the solver default.
  scen <- unfold_trade_routes(scen)

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

  # Finalise the total user-cost equation. Accumulated per-cost contributions
  # (if any) are wrapped into the `eqTotalUserCosts` definition; with no user
  # cost objects the default zero form is used. Mirrors write.R.
  if (length(scen@modInp@costs.equation) == 0) {
    scen@modInp@costs.equation <- paste0(
      "eqTotalUserCosts(region, year)$mvTotalUserCosts(region, year).. ",
      "vTotalUserCosts(region, year) =e= 0;"
    )
  } else {
    scen@modInp@costs.equation <- paste0(
      "eqTotalUserCosts(region, year)$mvTotalUserCosts(region, year)..",
      "   vTotalUserCosts(region, year) =e= ",
      gsub("[+][ ]*[-]", "-",
           paste0(scen@modInp@costs.equation, collapse = " + ")), ";"
    )
  }

  # Attach the solver source-code templates and mark the scenario interpolated
  # so it can be written / solved directly (parallel to the tail of
  # `interpolate()`).
  scen@settings@sourceCode <- .modelCode
  # Optional per-block override (see `code` arg): swap in a model script from a
  # file (or character vector) instead of the sysdata-baked `.modelCode`.
  if (!is.null(code)) {
    if (!is.list(code) || is.null(names(code)) || any(!nzchar(names(code))))
      stop("`code` must be a named list mapping a model block (e.g. 'GLPK') ",
           "to a script-file path or a character vector of lines.")
    for (.blk in names(code)) {
      .src <- code[[.blk]]
      if (is.character(.src) && length(.src) == 1L && file.exists(.src))
        .src <- readLines(.src)
      if (!.blk %in% names(scen@settings@sourceCode))
        warning("`code`: unknown model block '", .blk, "' (added anyway)")
      scen@settings@sourceCode[[.blk]] <- as.character(.src)
    }
  }
  scen@status$interpolated <- TRUE
  scen@status$script <- FALSE
  scen@status$solved <- FALSE
  # Refresh the smart scenario-folder name from the FINAL settings (so a
  # calendar/horizon passed via `...` is reflected). Only for in-memory
  # scenarios and default (non-explicit) paths -- an on-disk directory has
  # already been created at the early path.
  if (!explicit_path && !ondisk) {
    scen@path <- fp(get_scenarios_path(), .scenario_dir_name(
      scen@name, scen@model@name,
      scen@settings@calendar@name, scen@settings@horizon@name
    )) |> .fix_path()
  }
  # Storage-state flags mirroring the build knobs (self-describing for the
  # writers, model_size(), and save/reload). `sparse`: parameters omit
  # value==defVal rows -- a native-default backend (MathProg/JuMP/Pyomo) reads an
  # absent tuple as its defVal, but GAMS reads 0, so a sparse scenario must be
  # densified before a GAMS write. `folded`: the dims actually collapsed to
  # wildcards (folding only runs on the sparse path; empty = none). `pruned`:
  # default-valued flagged rows were dropped. (Replaces the legacy
  # `status$fullsets`, which was `!sparse`.)
  scen@status$sparse <- isTRUE(sparse)
  scen@status$folded <- if (isTRUE(sparse)) fold_dims else character(0)
  scen@status$pruned <- isTRUE(prune)

  # Post-interpolation consistency checks (NA index columns, schema, duplicate
  # keys, and bidirectional map <-> parameter coverage). Reports issues without
  # aborting by default; the folded pipeline permits wildcard NAs in trimmable
  # dimensions.
  if (isTRUE(validate)) {
    .interp_step(verbose, "validating parameters", oneline = FALSE)
    # Permit fold wildcards (NA) only in the dimensions actually folded this run.
    validate_scenario_parameters(
      scen, fold = if (isTRUE(sparse)) fold_dims else character(0),
      action = "warn")
  }

  .interp_footer(scen, verbose)
  scen
}

# Build the settings-derived parameters (set members + calendar / horizon /
# discount numerics) on the new-pipeline scenario. These are computed directly
# from `scen@settings` (not interpolated from model objects) and were previously
# missing from the new pipeline, leaving `pPeriodLen`, `pDiscount`,
# `pDiscountFactor`, `pSliceShare`, `pSliceWeight`, `pYearFraction`, `ordYear`,
# `cardYear` and the `year` / `region` / `slice` set parameters empty. Reuses the
# proven legacy settings builder `.obj2modInp(modInp, settings, approxim)`; the
# `approxim` list mirrors the one assembled in `interpolate()`.
#
# The legacy builder writes the in-memory `@data` slots. When the scenario is
# on-disk (`ondisk = TRUE`) those rows would not be persisted to the parameter
# store, so `save_scenario()` / the writers would see them empty. After building,
# the populated parameters are therefore flushed to disk through the canonical
# `update_parameter()` path (same as every `ob2mi` method) when `isOnDisk(scen)`.
.interp_user_constraints <- function(scen, verbose = FALSE) {
  cns <- list()
  for (i in seq_along(scen@model@data)) {
    for (j in seq_along(scen@model@data[[i]]@data)) {
      o <- scen@model@data[[i]]@data[[j]]
      if (inherits(o, "constraint")) cns[[length(cns) + 1L]] <- o
    }
  }
  if (length(cns) == 0L) return(scen)
  .interp_step(verbose, paste0("compiling ", length(cns), " user constraint(s)"))
  # Build the engine's set-value/calendar context once and reuse it.
  approxim <- .constraint_approxim(scen)
  for (o in cns) scen <- ob2mi(scen, o, list(approxim = approxim))
  scen
}

# Populate pDummyImportCost / pDummyExportCost from the config `@debug` table.
# `dbg` columns: comm, region, year, slice, dummyImport, dummyExport. Rows with a
# finite cost enable the slack; NA in comm / region / year / slice is a wildcard
# expanded to all commodities / regions / milestone years / slices.
.interp_dummy_slack <- function(scen, ss, mid) {
  dbg <- ss@debug
  if (!is.data.frame(dbg) || nrow(dbg) == 0) return(scen)
  all_comm   <- unique(unlist(lapply(scen@model@data, function(x)
    unlist(lapply(x@data, function(y) if (is(y, "commodity")) y@name else NULL)))))
  all_region <- ss@region
  all_year   <- as.integer(mid)
  all_slice  <- ss@calendar@slice_share$slice
  for (spec in list(c(col = "dummyImport", par = "pDummyImportCost"),
                    c(col = "dummyExport", par = "pDummyExportCost"))) {
    col <- spec[["col"]]; par <- spec[["par"]]
    if (!col %in% names(dbg)) next
    rows <- dbg[is.finite(suppressWarnings(as.numeric(dbg[[col]]))), , drop = FALSE]
    if (nrow(rows) == 0) next
    exp <- do.call(rbind, lapply(seq_len(nrow(rows)), function(i) {
      r  <- rows[i, ]
      cc <- if (is.na(r$comm))   all_comm   else as.character(r$comm)
      rr <- if (is.na(r$region)) all_region else as.character(r$region)
      yy <- if (is.na(r$year))   all_year   else as.integer(r$year)
      sl <- if (is.na(r$slice))  all_slice  else as.character(r$slice)
      e  <- expand.grid(comm = cc, region = rr, year = yy, slice = sl,
                        stringsAsFactors = FALSE)
      e$value <- as.numeric(r[[col]]); e
    }))
    if (is.null(exp) || nrow(exp) == 0) next
    exp <- exp[!duplicated(exp[c("comm", "region", "year", "slice")]), , drop = FALSE]
    scen@modInp@parameters[[par]] <-
      .dat2par(scen@modInp@parameters[[par]], as.data.table(exp))
  }
  scen
}

.interp_settings_params <- function(scen) {
  ss <- scen@settings
  mid <- ss@horizon@intervals$mid

  # Set parameters (legacy interpolate.R: year / slice / region).
  scen@modInp@parameters[["year"]] <-
    .dat2par(scen@modInp@parameters[["year"]], mid)
  scen@modInp@parameters[["slice"]] <-
    .dat2par(scen@modInp@parameters[["slice"]], ss@calendar@slice_share$slice)
  scen@modInp@parameters[["region"]] <-
    .dat2par(scen@modInp@parameters[["region"]], ss@region)
  scen@modInp@parameters[["mMidMilestone"]] <-
    .dat2par(scen@modInp@parameters[["mMidMilestone"]],
             data.table(year = mid))

  # Approximation list (subset used by the settings builder), mirroring
  # `interpolate()`.
  xx <- c(mid[-1] - mid[-length(mid)], 1)
  names(xx) <- mid
  approxim <- list(
    region = ss@region,
    year = ss@horizon@period,
    calendar = ss@calendar,
    solver = NULL,
    mileStoneYears = mid,
    mileStoneForGrowth = xx,
    fullsets = TRUE,
    optimizeRetirement = ss@optimizeRetirement
  )
  approxim$ry <- merge0(
    data.table(region = approxim$region, stringsAsFactors = FALSE),
    data.table(year = approxim$mileStoneYears, stringsAsFactors = FALSE)
  ) |> as.data.table()
  approxim$rys <- merge0(
    approxim$ry,
    data.table(slice = approxim$calendar@slice_share$slice,
               stringsAsFactors = FALSE)
  ) |> as.data.table()
  approxim$all_comm <- unlist(lapply(scen@model@data, function(x)
    unlist(lapply(x@data, function(y)
      if (is(y, "commodity")) y@name else NULL))))
  names(approxim$all_comm) <- NULL

  # Build the calendar / horizon / discount parameters via the `ob2mi`
  # "settings" method (R/obj2modInp.R), which writes the in-memory `@data`
  # slots of `scen@modInp@parameters`. Ported from the legacy `.obj2modInp`
  # settings method; the overlapping calendar maps it also builds are refreshed
  # by the calendar recipe that runs next.
  scen <- ob2mi(scen, ss, list(approxim = approxim))

  # Dummy import / export slack costs from `config@debug`. A finite cost enables
  # a slack term in the corresponding commodity balance (the model may import /
  # export the commodity at that penalty), guaranteeing feasibility. The default
  # cost is Inf (no slack). NA in a dimension is a wildcard, expanded here to all
  # commodities / regions / milestone years / slices (no interpolation needed).
  scen <- .interp_dummy_slack(scen, ss, mid)

  # Persist the freshly-built parameters to the on-disk store so they survive
  # `save_scenario()` / reload and are seen by the writers. No-op in memory.
  if (isOnDisk(scen)) {
    built <- c(
      "year", "slice", "region", "mMidMilestone",
      "mSliceParentChild", "mSliceParentChildE", "mSliceNext",
      "mSliceFYearNext", "pDiscount", "pSliceShare", "pSliceWeight",
      "mMilestoneLast", "mMilestoneFirst", "mMilestoneNext",
      "mMilestoneHasNext", "mSameSlice", "mSameRegion", "ordYear",
      "pYearFraction", "cardYear", "pPeriodLen", "pDiscountFactor",
      "pDummyImportCost", "pDummyExportCost"
    )
    for (nm in built) {
      p <- scen@modInp@parameters[[nm]]
      if (is.null(p)) next
      d <- .get_data_slot(p)
      if (is.null(d) || nrow(d) == 0) next
      # Reset to an empty (in-memory) template, then re-add the rows through
      # `update_parameter()` so they are written to the parameter store on disk.
      scen@modInp@parameters[[nm]] <- .resetParameter(p)
      scen <- update_parameter(scen, nm, as.data.frame(d))
    }
  }

  scen
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
      obj_desc <- if (.hasSlot(obj, "desc") && length(obj@desc) == 1) {
        obj@desc
      } else {
        NA_character_
      }
      process_names <- c(process_names, list(data.frame(
        name = obj@name,
        desc = obj_desc,
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
#' @param ... additional arguments passed to `func`.
#' @param classes character vector of class names to apply the function to
#' @param return_list logical, if TRUE, return a list of results, otherwise
#' return a vector of results
#'
#' @returns a list or vector of `func` results, one per matching object.
#' @export
apply_to_scenario_data <- function(
    scen,
    func,
    ...,
    classes = NULL,
    as_list = TRUE
  ) {

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


apply_to_parameters <- function(
    scen,
    func,
    ...,
    as_list = TRUE
) {
  # browser()
  rs <- list()
  for (i in seq(along = scen@modInp@parameters)) {
    if (inherits(scen@modInp@parameters[[i]], "parameter")) {
      # browser()
      rr <- func(scen@modInp@parameters[[i]], ...)
      rs <- c(rs, rr)
    }
  }

  # return as named list
  if (as_list) {
    return(rs)
  }
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
  invisible()  # browser() disabled
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

  # Add filtration for process_years if no NAs in region & year
  # process_years <- scen@modInp@sets$process_years |>
  #   filter(process == proc_name) |>
  #   select(region, year) |>
  #   unique()
  #
  # filter out years and regions not in process_years
  # x <- x |>
  #   dplyr::semi_join(process_years, by = c("region", "year"))

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
#' @param ... additional arguments (currently unused).
#'
#' @returns a data frame with the completed set combinations.
#' @export
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

# identify sets by name or by class
is_set <- function(x, scen = NULL) {
  # check if x is a set
  # x - character vector or list
  # scen - scenario object

  if (is.null(scen)) {
    # !!! ToDo: finish
    stop("Scenario object is required")
  }

  if (is.null(scen@modInp@sets$set_names)) {
    stop("No set names found in the scenario object.\n",
         "Not interpolated scenario object?")
  }

  if (inherits(x, "data.frame")) {
    x <- names(x)
  } else if (inherits(x, "list")) {
    x <- names(x)
  } else if (!inherits(x, "character")) {
    stop("x must be a character vector, list, or data frame")
  }

  sn <- x %in% scen@modInp@sets$set_names
  names(sn) <- x

  return(sn)
}


guess_sets <- function(x) {}


# expand_dim <- function(
#     dat, # data.frame
#     ...
#     # dim_name, # name of the dimension to expand
#     # full_set, # full set of elements
#     # par_name = NULL # name of the parameter to expand
# ) {
#   # browser()
#   # dat - data frame with columns of sets and parameters
#   # dim_name - name of the dimension to expand
#   # full_set - full set of elements
#   # par_name - name of the parameter to expand
#
#   if (is.null(full_set)) {
#     stop("full_set is required")
#   }
#   if (is.null(dim_name)) {
#     stop("dim_name is required")
#   }
#   if (is.null(dat)) {
#     stop("dat is required")
#   }
#
#   # check if dim_name is in dat
#   if (!dim_name %in% names(dat)) {
#     stop("dim_name not found in dat")
#   }
#
#   # check if any NAs in dim_name (return if no NAs)
#   if (!any(is.na(dat[[dim_name]]))) {
#     return(dat)
#   }
#
#   # match dat-columns with full_set
#   complete(dat, ...)
#
#
#
# }

#' Expand rows with NA values in set columns
#'
#' @description Replaces `NA` values in a data frame column with all possible
#' values for each unique combination of other columns.
#'
#' @param data data frame with columns of sets and parameters
#' @param column name of the column to expand, can be a symbol or string
#' @param all_values vector of all possible values for the column
#' @param all_regions vector of all possible regions
#' @param all_years vector of all possible years
#'
#' @returns data frame with expanded rows
#' @export
expand_na_rows <- function(data, column, all_values, group_cols = NULL) {
  # browser()
  # checks
  if (is_empty(data) || is_empty(all_values)) {
    return(data)
  }

  if (!is_vector(all_values)) {
    stop("all_values must be a vector")
  }

  col <- tryCatch(
    rlang::as_string(column),
    error = function(e) {
      # if column is a symbol, convert to string
      rlang::ensym(column) |> rlang::as_string()
    }
  )

  if (!is_empty(group_cols)) {
    dd <- data |>
      group_by(across(all_of(group_cols))) |>
      group_split()
  } else {
    dd <- list(data)
  }

  # Separate NA and non-NA rows
  dd <- lapply(dd, function(d) {
    na_rows <- d |>
      dplyr::filter(is.na(.data[[col]])) |> select(-!!col)

    non_na_rows <- d |> filter(!is.na(.data[[col]])) |> as.data.table()

    all_values_d <- all_values[!(all_values %in% non_na_rows[[col]])]

    # Create all combinations: NA rows × all_values
    expanded <- tidyr::crossing(
      na_rows,
      as.data.table(set_names(list(all_values_d), col))
    )

    rbindlist(list(non_na_rows, expanded), use.names = TRUE)
  })
  # na_rows <- data |>
  #   dplyr::filter(is.na(.data[[col]])) |> select(-!!col)
  # non_na_rows <- data |> filter(!is.na(.data[[col]])) |> as.data.table()
  #
  # # Create all combinations: NA rows × all_values
  # expanded <- tidyr::crossing(
  #   na_rows,
  #   as.data.table(set_names(list(all_values), col)))
  #
  # rbindlist(list(non_na_rows, expanded), use.names = TRUE)
  dd <- rbindlist(dd, use.names = TRUE) |>
    arrange(across(any_of(c("region", "vintage", "year"))))

  stopifnot(!anyDuplicated(dd))

  return(dd)

}

#' @export
#' @rdname expand_na_rows
expand_na_regions <- function(data, all_regions, group_cols = NULL) {
  expand_na_rows(data, "region", all_regions, group_cols)
}

#' @export
#' @rdname expand_na_rows
expand_na_years <- function(data, all_years, group_cols = NULL) {
  expand_na_rows(data, "year", all_years, group_cols)
}

#' @param full_sets data frame with all possible combinations of process
#' years, and other sets for the process (e.g. region, vintage, etc.). The
#' data frame is considered as a full set of elements for the process.
#' @param skip_na_dims logical, if TRUE, do not expand dimensions with all NA values.
#' @param add_missing_dims logical, if TRUE, add missing dimensions to the data
#' from the full_sets data frame.
#' @param unmatched_action action to take if no matching process years are found
#' in the data frame. Possible values are "warning", "drop", "error", and "ignore".
#' Default is a combination of "warning" and "drop".
#' @export
#' @rdname expand_na_rows
expand_sets <- function(
    data,
    full_sets, # full_sets
    name_col = NULL, # name_col / index_col
    value_col = NULL, # param_col
    skip_na_dims = FALSE,
    add_missing_dims = !skip_na_dims,
    unmatched_action = c("warning", "drop")
    ) {
  # browser()

  # check if data is empty
  if (is_empty(data)) {
    return(data)
  }
  # check if full_sets is empty
  if (is_empty(full_sets)) {
    warning("full_sets is empty")
    return(data)
  }
  # check if data is a data frame
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  # check if full_sets is a data frame
  if (!is.data.frame(full_sets)) {
    stop("full_sets must be a data frame")
  }
  # check if full_sets has 'year' column
  # if (!("year" %in% names(full_sets))) {
  #   stop("full_sets does not have 'year' column")
  # }
  # # check if data has 'year' column
  # if (!("year" %in% names(data))) {
  #   stop("data does not have 'year' column")
  # }
  # check if full_sets has any NAs
  if (anyNA(full_sets)) {
    stop("full_sets cannot have NAs. It should be a full set of elements.")
  }
  data_cols <- names(data)
  # check if data has 'name_col' column
  if (!is.null(name_col)) {
    if (!(name_col %in% names(data))) {
      stop("data does not have '", name_col, "' column")
    }
    # check if full_sets has 'name_col' column
    if (!(name_col %in% names(full_sets))) {
      if (!("process" %in% names(full_sets))) {
        stop("full_sets does not have '",
             paste(unique(c("process", name_col)), collapse = " or "),
             "' column")
      } else {
        # rename "process" column to `name_col`
        full_sets <- full_sets |>
          rename(!!name_col := process)
      }
    }
  }

  key_cols <- names(data)[names(data) %in% names(full_sets)]
  # key_cols_no_year <- key_cols[key_cols != "year"]
  if (length(key_cols) == 0) {
    stop("No matching columns found in data and full_sets")
  }

  #!!! check if there are duplicated values for the same key_cols with NAs
  # browser()
  amb_check <- data |> select(all_of(key_cols))
  ii <- duplicated(amb_check) | duplicated(amb_check, fromLast = TRUE)
  duplicated_rows <- data[ii, ]
  if (any(ii)) {
    # browser()
    stop("Ambiguous assignment for parameter '", value_col, "':\n",
         paste0(
           apply(duplicated_rows, 1, function(x) {
             paste0(x, collapse = ", ")
           }),
           collapse = "\n"
         ),
         "\nPlease check the data for duplicates."
    )
  }

  if (add_missing_dims) {
    # if (skip_na_dims) {
    #   stop("skip_na_dims and add_missing_dims cannot be both TRUE")
    # }
    # browser()
    missing_cols <- setdiff(names(full_sets), key_cols)
    # add missing dimensions
    for (col in missing_cols) {
      # add column to data with NA values
      data <- data |>
        mutate(!!col := as(NA, class(full_sets[[col]])[1]), .before = 1)
      key_cols <- c(col, key_cols)
      data_cols <- c(col, data_cols)
    }
  } else {
    # drop unused dimensions
    full_sets <- full_sets |>
      select(all_of(key_cols)) |>
      unique()
  }

  if (skip_na_dims) {
    # browser()
    na_cols <- data |>
      select(all_of(key_cols)) |>
      sapply(function(x) all(is_any(x)))
    na_cols <- names(na_cols[na_cols])
    if (add_missing_dims) {
      # exclude missing dimensions from na_cols
      na_cols <- na_cols[!(na_cols %in% missing_cols)]
    }
    # drop unused dimensions from full_sets
    full_sets <- full_sets |>
      select(-all_of(na_cols)) |>
      unique()
    # drop unused dimensions from key_cols
    key_cols <- key_cols[!(key_cols %in% na_cols)]
  }

  # check if full_sets has any data
  if (nrow(full_sets) == 0) {
    return(data)
  }


  # Rows with complete values
  complete_rows <- data |>
    filter(if_all(all_of(key_cols), ~ !is.na(.))) |>
    force_cols_classes()

  # exclude combinations of full_sets that are in complete_rows
  full_sets <- full_sets |>
    anti_join(complete_rows, by = key_cols)

  # browser()
  # Rows with NA in any of the specified columns
  na_rows <- data |> filter(if_any(all_of(key_cols), is.na))
  if (nrow(na_rows) == 0) {
    # add non-existing combinations of key_cols from full_sets
    data <- anti_join(full_sets, data, by = key_cols) |>
      full_join(data, complete_rows, by = key_cols) |>
      unique() |>
      arrange(across(any_of(c("region", "vintage", "year")))) |>
      select(all_of(data_cols)) |>
      force_cols_classes()

    return(data)
  }

  na_rows <- split(na_rows, 1:nrow(na_rows))

  # For each NA row, expand using full_sets
  # ll <- lapply(na_rows, function(row) {
  ll <- list()
  for (row in na_rows) {

    # find non-NA columns
    jj <- map_lgl(key_cols, ~ !is.na(row[[.x]]))
    non_na_keys <- key_cols[jj]
    na_keys <- key_cols[!jj]

    if (!is.null(row[["year"]]) && !is.na(row[["year"]])) {
      # !!! ToDo: add other sets?
      proc_year <- full_sets |> filter(year == row[["year"]])
    } else {
      proc_year <- full_sets
    }

    if (length(non_na_keys) == 0) {
      # no non-NA columns, expand all rows
      return(
        cross_join(
          proc_year,
          select(row, -all_of(key_cols))
        )
      )
    }

    # continue with at least one non-NA column in the row
    # select rows in proc_year with matching to non-NA columns in the row

    # browser()

    fill_rows <- semi_join(
      proc_year,
      select(row, all_of(non_na_keys)),
      by = non_na_keys
    )

    expanded_rows <- full_join(
      fill_rows,
      select(row, -all_of(na_keys)),
      by = intersect(names(row), non_na_keys)
    )

    # missing_rows <- anti_join(
    #   proc_year,
    #   fill_rows,
    #   by = names(proc_year)
    # )
    #
    # expanded_rows <- rbindlist(
    #   list(expanded_rows, missing_rows),
    #   use.names = TRUE, fill = TRUE)

    ll[[length(ll) + 1]] <- expanded_rows

  }
    # }) |> rbindlist()
  expanded_rows <- rbindlist(ll, use.names = TRUE, fill = TRUE)
  # browser()

  d <- rbindlist(list(complete_rows, expanded_rows), use.names = TRUE) |>
    unique() |>
    arrange(across(any_of(c("region", "vintage", "year"))))

  # complete
  d <- complete(d, nesting(full_sets)) |>
    arrange(across(any_of(c("region", "vintage", "year")))) |>
    select(all_of(data_cols)) |>
    force_cols_classes()

  return(d)
}

if (F) {
  dt <- ECOA@capacity |>
    filter(region %in% c("R1", "R2", "R3")) |>
    # mutate(region = NA)
    select(region, year, stock)

  # dt[dt$region == "R1" & dt$year == 2015, c("region", "year")] <- NA
  dt$region[dt$region == "R1"] <- NA
  dt

  expand_na_rows(
    data = dt,
    column = "region",
    all_values = scen@modInp@sets$process_region[[ECOA@name]],
    group_cols = c("year")
  )

  expand_sets(
    # data = ECOA@capacity,
    data = dt,
    # process = ECOA@name,
    # region = c("R1", "R2", "R3"),
    full_sets = filter(scen@modInp@sets$process_years, process == ECOA@name),
    value_col = "stock"
  ) |>
    # pivot_wider(
    #   names_from = year,
    #   values_from = stock
    # ) |>
    as.data.frame()


  # check 'skip_na_dims' option
  dt <- ECOA@capacity |>
    filter(region %in% c("R1")) |>
    mutate(region = NA) |>
    select(region, year, stock)
  dt
  expand_sets(
    data = dt,
    full_sets = filter(scen@modInp@sets$process_years, process == ECOA@name),
    value_col = "stock",
    skip_na_dims = TRUE
  )

  # check 'add_missing_dims' option
  expand_sets(
    data = dt,
    full_sets = filter(scen@modInp@sets$process_years, process == ECOA@name),
    value_col = "stock",
    add_missing_dims = TRUE
  )

  # check 'skip_na_dims' and 'add_missing_dims' options
  expand_sets(
    data = dt,
    full_sets = filter(scen@modInp@sets$process_years, process == ECOA@name),
    value_col = "stock",
    add_missing_dims = TRUE,
    skip_na_dims = TRUE
  )
  dt <- ECOA@capacity |>
    filter(region %in% c("R1"), year == 2015) |>
    mutate(region = NA_character_, year = NA_integer_) |>
    mutate(stock = 10) |>
    select(region, year, stock)
  dt
  expand_sets(
    data = dt,
    full_sets = filter(scen@modInp@sets$process_years, process == ECOA@name),
    value_col = "stock",
    add_missing_dims = TRUE,
    skip_na_dims = TRUE
  )

  # check umbiguous assignment
  dt <- ECOA@capacity |>
    filter(region %in% c("R1", "R2")) |>
    mutate(region = NA) |>
    select(region, year, stock)
  dt
  expand_sets(
    data = dt,
    full_sets = filter(scen@modInp@sets$process_years, process == ECOA@name),
    value_col = "stock",
    add_missing_dims = TRUE,
    skip_na_dims = TRUE
  )

}


#' Interpolate numerical parameter for missing years
#'
#' @param data data frame with columns of sets and the parameter to interpolate
#' @param value_col name of the column with the parameter to interpolate
#' @param set_cols optional character vector of set columns to group by, if NULL,
#' all columns except `value_col` are used
#' @param int_rule interpolation rule, default is "inter" (linear interpolation
#' between years). Other options are "forw" (forward fill) and "back"
#' (backward fill) of the last or first value respectively.
#' @param def_val default value for the parameter, if not provided, NA values
#' will be returned for missing years not covered by the interpolation rule.
#'
#' @returns data frame with interpolated values for the parameter
#' @export
interpolate_numpar <- function(
    data,
    value_col,
    set_cols = NULL,
    int_rule = "inter",
    def_val = NULL
  ) {
  # browser()
  if (F) {
    # data
    set_cols <- scen@modInp@sets$set_names
    dt <- ECOA@capacity |>
      filter(region %in% c("R1", "R2", "R3")) |>
      select(region, year, stock) |>
      expand_sets(
        full_sets = filter(scen@modInp@sets$process_years, process == ECOA@name),
        value_col = "stock"
      )
    dt
    # test
    interpolate_numpar(
      data = dt,
      value_col = "stock",
      set_cols = c("region"),
      # int_rule = "inter"
      # int_rule = "forw"
      # int_rule = "back"
      int_rule = "bwd.mid.fwd"
      # def_val = 100
    )

    # one value
    dt <- data.frame(
      region = c("R1", "R2", "R3"),
      year = c(2015, 2020, 2025),
      stock = c(NA, 10, NA)
    ) |>
      expand_sets(
        full_sets = filter(scen@modInp@sets$process_years, process == ECOA@name),
        value_col = "stock"
      )
    dt

    interpolate_numpar(
      data = dt,
      value_col = "stock",
      set_cols = c("region"),
      # int_rule = "inter"
      int_rule = "forw"
    )

  }

  if (nrow(data) == 0) {
    return(data)
  }

  if (is.null(data[["year"]])) {
    # not a time series
    return(data)
  }

  if ("year" %in% set_cols) {
    # warning("set_cols should not include 'year' - dropping it")
    set_cols <- set_cols[set_cols != "year"]
  }

  if (is.null(set_cols)) {
    set_cols <- names(data)[!(names(data) %in% c(value_col, "year"))]
    # drop numeric columns except 'vintage'
    # jj <- sapply(select(data, all_of(set_cols)), is.numeric)
    # jj <- jj & !(set_cols %in% c("vintage", "year"))
    # set_cols <- set_cols[!jj]
  }

  # Fast path: nothing missing => nothing to interpolate. Skips the whole
  # group machinery, which otherwise dominates for high-resolution parameters
  # (e.g. an 8760-slice pDemand whose source years already cover the milestones).
  if (!anyNA(data[[value_col]])) {
    return(as.data.table(data))
  }

  approx_rule <- c(1, 1)
  if (grepl("for|fwd|frw", int_rule, ignore.case = T)) {
    approx_rule[2] <- 2
  }
  if (grepl("back|bwd", int_rule, ignore.case = T)) {
    approx_rule[1] <- 2
  }
  if (grepl("int|mid", int_rule, ignore.case = T)) {
    interp_within <- TRUE
  } else {
    interp_within <- FALSE
  }

  # Per-group (set_cols) interpolation over `year`. Same logic as before, but
  # driven by data.table's C-level grouping instead of group_split() + lapply,
  # which allocated one tibble per group and was pathologically slow at tens of
  # thousands of groups (e.g. 8760 slices x regions x ...).
  .interp_grp <- function(year, val) {
    not_na <- !is.na(val)
    nval <- sum(not_na)
    # all-NA (nothing to drive interpolation) or no-NA (nothing missing): keep.
    if (nval == 0L || nval == length(val)) {
      return(val)
    }
    if (interp_within && nval > 1L) {
      return(approx(val, x = year, xout = year, rule = approx_rule)$y)
    }
    # forward and/or backward fill only
    nval_first <- which(year == min(year[not_na]))[1]
    nval_last <- which(year == max(year[not_na]))[1]
    if (approx_rule[1] == 2 && nval_first > 1L) {
      val[1:nval_first] <- val[nval_first]
    }
    if (approx_rule[2] == 2 && nval_last < length(val)) {
      val[nval_last:length(val)] <- val[nval_last]
    }
    val
  }

  dt <- as.data.table(data)
  by_cols <- intersect(set_cols, names(dt))
  data.table::setorderv(dt, c(by_cols, "year"))
  if (length(by_cols) > 0L) {
    dt[, (value_col) := .interp_grp(year, get(value_col)), by = by_cols]
  } else {
    dt[, (value_col) := .interp_grp(year, get(value_col))]
  }

  ll <- dt |>
    arrange(across(any_of(c("region", "vintage", "year")))) |>
    as.data.table()

  if (!is.null(def_val)) {
    # replace NA values with default value
    ll[[value_col]][is.na(ll[[value_col]])] <- def_val
  }

  return(ll)

}

#' Interpolate lower and upper bounds
#'
#' @param data data frame with columns of sets and the parameter to interpolate
#' @param value_col name of the column with the parameter to interpolate
#' @param set_cols optional character vector of set columns to group by, if NULL,
#' @param int_rule interpolation rule, default is "inter" (linear interpolation
#' @param def_val default value for the parameter, if not provided, NA values
#' will be returned for missing years not covered by the interpolation rule.
#' @param value_sfx suffix for the value column, default is c(".lo", ".up", ".fx")
#'
#' @returns data frame with interpolated values for the parameter
#' @export
interpolate_bounds <- function(
    data,
    value_col,
    set_cols,
    int_rule = "mid", # !!! ToDo: add separate rules for lo, up, fx
    def_val = NULL,
    value_sfx = c(".lo", ".up", ".fx")
) {
  # browser()
  if (nrow(data) == 0) {
    return(data)
  }
  if (is.null(data[["year"]])) {
    # not a time series
    return(data)
  }
  bound_cols <- paste0(value_col, value_sfx)
  # check if bound_cols are in data
  ii <- bound_cols %in% names(data)
  if (!any(ii)) {
    stop("Parameter columns not found in data:\n",
         paste(bound_cols[!ii], collapse = ", "))
  } else if (sum(ii) < length(bound_cols)) {
    warning("Some parameter columns not found in data:\n",
            paste(bound_cols[!ii], collapse = ", "))
  }
  bound_cols <- bound_cols[ii]

  d <- data |>
    select(all_of(c(set_cols, bound_cols))) |>
    unique() |>
    force_cols_classes()

  # browser()

  # check for conflicts in data
  lo <- !is.na(d[[bound_cols[1]]])
  up <- !is.na(d[[bound_cols[2]]])
  fx <- !is.na(d[[bound_cols[3]]])

  # fx vs lo/up
  if (any(fx)) {
    if (any(!is.na(d[[bound_cols[1]]][fx]))) {
      stop("Both lo and fx values are set for the same parameter:\n",
           paste(bound_cols[1], bound_cols[3], collapse = ", "))
    }
    if (any(!is.na(data[[bound_cols[2]]][fx]))) {
      stop("Both up and fx values are set for the same parameter:\n",
           paste(bound_cols[2], bound_cols[3], collapse = ", "))
    }
  }

  # lo vs up
  if (any(lo) && any(up)) {
    if (isTRUE(any(d[[bound_cols[1]]][lo] > d[[bound_cols[2]]][lo]) ||
        any(d[[bound_cols[1]]][up] > d[[bound_cols[2]]][up]))) {
      stop("lo and up values are not consistent for the same parameter:\n",
           paste(bound_cols[1], bound_cols[2], collapse = ", "))
    }
  }

  # interpolate every bound separately
  # for (i in seq_along(bound_cols)) {
  #   # browser()
  #   # interpolate bounds
  #   d <- interpolate_numpar(
  #     data = d,
  #     value_col = bound_cols[i],
  #     set_cols = set_cols,
  #     int_rule = int_rule,
  #     def_val = def_val
  #   )
  # }

  # interpolate every bound separately
  d <- purrr::reduce(bound_cols, function(data, col) {
    interpolate_numpar(
      data = data,
      value_col = col,
      set_cols = set_cols,
      int_rule = int_rule,
      def_val = def_val
    )
  }, .init = d)

  # combine interpolated values from bounds
  # Fold fixed bounds into equal lower/upper bounds (fx => lo == up) so the
  # fixed value survives the downstream drop of the `.fx` column, and enforce
  # lo <= up at the interpolated points.
  lo_col <- paste0(value_col, ".lo")
  up_col <- paste0(value_col, ".up")
  fx_col <- paste0(value_col, ".fx")
  if (fx_col %in% names(d)) {
    isfx <- !is.na(d[[fx_col]])
    if (any(isfx)) {
      d[[lo_col]][isfx] <- d[[fx_col]][isfx]
      d[[up_col]][isfx] <- d[[fx_col]][isfx]
    }
  }
  if (all(c(lo_col, up_col) %in% names(d))) {
    both <- !is.na(d[[lo_col]]) & !is.na(d[[up_col]])
    if (any(both)) {
      lo_v <- d[[lo_col]][both]
      up_v <- d[[up_col]][both]
      d[[lo_col]][both] <- pmin(lo_v, up_v)
      d[[up_col]][both] <- pmax(lo_v, up_v)
    }
  }
  d <- force_cols_classes(d)

  return(d)

}

if (F) {
  data <- ECOA@afs |>
    expand_sets(
      full_sets = filter(scen@modInp@sets$process_years, process == ECOA@name),
      skip_na_dims = TRUE
      # value_col = "stock"
    )
  ECOA@af
  dt <- data.table(
    region = NA_character_,
    year = c(2015, 2020, 2025, 2030, 2050),
    slice = NA_character_,
    af.lo = c(.75, NA, NA, .25, NA),
    af.up = c(.8, NA, .2, NA, .8),
    af.fx = c(NA, .5, NA, NA, NA)
  ) |>
    force_cols_classes()
  dt

  interpolate_bounds(
    data = dt,
    value_col = "af",
    set_cols = c("region", "year", "slice"),
    int_rule = "mid"
    # def_val = 0.5
  )

  # conflicts
  dt <- data.table(
    region = NA_character_,
    year = c(2015, 2020, 2025, 2030, 2050),
    slice = NA_character_,
    af.lo = c(.75, NA, NA, .25, NA),
    af.up = c(.8, NA, .2, NA, .8),
    af.fx = c(NA, .5, NA, NA, .8)
  ) |>
    force_cols_classes()
  interpolate_bounds(
    data = dt,
    value_col = "af",
    set_cols = c("region", "year", "slice"),
    int_rule = "mid"
  )

  dt <- data.table(
    region = NA_character_,
    year = c(2015, 2020, 2025, 2030, 2050),
    slice = NA_character_,
    af.lo = c(.75, NA, NA, .25, .85),
    af.up = c(.8, NA, .2, NA, .8),
    af.fx = c(NA, .5, NA, NA, NA)
  ) |>
    force_cols_classes()

  interpolate_bounds(
    data = dt,
    value_col = "af",
    set_cols = c("region", "year", "slice"),
    int_rule = "mid"
  )

}

# fill_defaults <- function(
#     x, # data frame
#     param_name, # name of the parameter
#     def_val = NULL # default value for the parameter
# ) {
#   # browser()
#   # check if param_name is in x
#   if (!param_name %in% names(x)) {
#     stop("Parameter '", param_name, "' not found in data frame")
#   }
#   # check if def_val is NULL
#   if (is.null(def_val)) {
#     return(x)
#   }
#   # assign default value to NA elements of the parameter
#   x[[param_name]][is.na(x[[param_name]])] <- def_val
#   return(x)
# }


get_parameter_full_sets <- function(
    scen, # scenario object
    param_name # name of the parameter
) {
  # returns data frame with all possible combinations of sets for the parameter,
  # considering the parameter's timeframe, region, years.
  #


}

# =========================================================================== #
# Per-parameter interpolation pass ####
# =========================================================================== #
# One interpolation pass per `p*` parameter, operating on the full table of
# raw (sparse) values collected by `ob2mi` across all processes at once. Each
# value series (one per combination of the parameter's non-year dimensions) is
# interpolated across years using the parameter's own interpolation rule and
# default value, then filtered to the model's milestone years.

#' Normalise a parameter interpolation rule for `interpolate_numpar()`
#'
#' `interpolate_numpar()` recognises the tokens `back`/`inter`/`forth` (and the
#' aliases `bwd`/`mid`/`fwd`). The legacy `parameter@interpolation` strings use
#' the `back.inter.forth` form, which already matches. An empty/`NA` rule falls
#' back to linear interpolation only.
#' @noRd
.interp_rule_token <- function(rule) {
  if (length(rule) == 0 || is.na(rule[1]) || rule[1] == "") {
    return("inter")
  }
  rule[1]
}

#' Interpolate a single value column across years for each set combination
#'
#' Builds the grid `distinct(set_cols) x year_seq` (where `year_seq` spans the
#' milestone years extended by any anchor years present in `raw`, so out-of-
#' horizon anchors can still drive back/forward extrapolation), joins the raw
#' values, interpolates with `interpolate_numpar()`, then keeps milestone years.
#' @noRd
.interp_one_series <- function(raw, set_cols, milestones, int_rule, def_val) {
  year_seq <- sort(unique(c(milestones, as.integer(raw$year))))

  # Values to drive interpolation. Rows with an explicit year are used as-is.
  # Rows with NA year represent a value held constant over the horizon, so they
  # are broadcast to every year in `year_seq`; an explicit-year value always
  # takes precedence over a broadcast value at the same (set_cols, year) key.
  raw_yr <- raw |> dplyr::filter(!is.na(year))
  raw_na <- raw |> dplyr::filter(is.na(year))
  vals <- raw_yr |> dplyr::select(dplyr::all_of(c(set_cols, "year", "value")))
  if (nrow(raw_na) > 0) {
    bc <- raw_na |>
      dplyr::select(dplyr::all_of(c(set_cols, "value"))) |>
      dplyr::distinct() |>
      dplyr::cross_join(dplyr::tibble(year = year_seq))
    if (nrow(vals) > 0) {
      bc <- dplyr::anti_join(bc, vals, by = c(set_cols, "year"))
    }
    vals <- dplyr::bind_rows(vals, bc)
  }

  grid <- raw |>
    dplyr::distinct(dplyr::across(dplyr::all_of(set_cols))) |>
    dplyr::cross_join(dplyr::tibble(year = year_seq)) |>
    dplyr::left_join(vals, by = c(set_cols, "year"))
  out <- interpolate_numpar(
    data = grid, value_col = "value", set_cols = set_cols,
    int_rule = int_rule, def_val = def_val
  )
  out |> dplyr::filter(year %in% milestones, !is.na(value))
}

#' Build an empty (0-row) data table matching a parameter's schema
#'
#' Returns a 0-row copy of `param@data`, preserving its column names and
#' classes. Used to clear a parameter's data slot when interpolation leaves no
#' surviving rows, so the stale (sparse, possibly NA-indexed) raw data is not
#' written to the solver.
#' @param param a `parameter` object.
#' @returns a 0-row data frame / data.table with the parameter's schema.
#' @noRd
.empty_param_data <- function(param) {
  param@data[0, , drop = FALSE]
}

#' Write a parameter's data slot, honouring on-disk vs in-memory storage
#'
#' Shared write-back used by interpolation and trimming. For on-disk parameters
#' the existing on-disk `data` directory is replaced and the in-memory `@data`
#' is reset to the empty schema; for in-memory parameters `@data` is set
#' directly. `new_data` must already match the parameter's column schema.
#' @param scen scenario.
#' @param pn parameter name.
#' @param new_data data frame matching `param@data` columns.
#' @returns the scenario with the parameter slot updated.
#' @noRd
.interp_write_param <- function(scen, pn, new_data) {
  param <- scen@modInp@parameters[[pn]]
  if (isOnDisk(param)) {
    ppath <- getObjPath(param)
    if (is.null(ppath)) {
      stop("On-disk parameter '", pn, "' has no path for write-back.")
    }
    data_dir <- file.path(ppath, "data")
    existing <- list.files(data_dir, recursive = TRUE)
    fmt <- if (any(grepl("\\.parquet$", existing))) "parquet" else "csv"
    unlink(data_dir, recursive = TRUE)
    data2disk(data.table::as.data.table(new_data), path = data_dir,
              format = fmt)
    scen@modInp@parameters[[pn]]@data <-
      reset_slot(data.table::as.data.table(new_data))
  } else {
    scen@modInp@parameters[[pn]]@data <- new_data
  }
  # Keep the row-count cache in sync: writers (Pyomo / JuMP / GLPK v1) truncate
  # `@data` to `@misc$nValues`, and a stale count pads with NA rows.
  scen@modInp@parameters[[pn]]@misc$nValues <- nrow(new_data)
  scen
}

#' Inverted parameter -> value-map registry
#'
#' Inverts `.value_map_def` (map -> source parameters) into a
#' parameter -> maps lookup, so a value parameter can be trimmed / validated
#' against the equation-domain maps that index it.
#' @returns named list: parameter name -> character vector of map names.
#' @noRd
.param_value_maps <- function() {
  reg <- list()
  for (mp in names(.value_map_def)) {
    for (sp in .value_map_def[[mp]]$source) {
      reg[[sp]] <- union(reg[[sp]], mp)
    }
  }
  reg
}

#' Trim numeric/bounds parameters to the domain of the maps that index them
#'
#' Drops rows of a value parameter that lie outside the union of the equation-
#' domain maps referencing it (lifespan window x membership). The maps are the
#' authority on the minimal domain, so any parameter row no map indexes is dead
#' data: it bloats the written model and (for unfolded scenarios) may carry a
#' stale wildcard NA that is out-of-domain in the solver.
#'
#' Intended for the unfolded (`fold = FALSE`) pipeline, where wildcard (NA)
#' dimensions have already been materialised. A parameter is left untouched when
#' none of its registered maps is available, to avoid clearing data whose map
#' was not built.
#' @param scen scenario.
#' @param verbose logical; report per-parameter trim counts.
#' @returns scenario with trimmed parameter slots.
#' @export
trim_parameters_by_maps <- function(scen, verbose = FALSE) {
  reg <- .param_value_maps()
  for (pn in names(reg)) {
    param <- scen@modInp@parameters[[pn]]
    if (is.null(param)) next
    pdata <- get_data_slot(param)
    if (is.null(pdata) || nrow(pdata) == 0) next
    pdata <- as.data.frame(pdata)

    kept <- list()
    used_any <- FALSE
    for (mp in reg[[pn]]) {
      mpar <- scen@modInp@parameters[[mp]]
      if (is.null(mpar)) next
      md <- get_data_slot(mpar)
      if (is.null(md)) next
      md <- as.data.frame(md)
      # Key on the map's declared dimensions (robust to an empty map whose data
      # slot may carry no columns), restricted to the parameter's own id cols.
      key <- intersect(intersect(colnames(pdata), mpar@dimSets), param@dimSets)
      if (length(key) == 0) next
      used_any <- TRUE
      if (nrow(md) == 0) {
        kept[[mp]] <- pdata[0, , drop = FALSE]
      } else {
        kept[[mp]] <- dplyr::semi_join(
          pdata, dplyr::distinct(md[, key, drop = FALSE]), by = key
        )
      }
    }
    if (!used_any) next

    trimmed <- dplyr::distinct(dplyr::bind_rows(kept))
    if (nrow(trimmed) == nrow(pdata)) next

    new_data <- trimmed |>
      force_cols_classes() |>
      as.data.frame() |>
      (\(d) d[, colnames(param@data), drop = FALSE])()

    if (isTRUE(verbose)) {
      message("trim '", pn, "': ", nrow(pdata), " -> ", nrow(new_data),
              " rows")
    }
    scen <- .interp_write_param(scen, pn, new_data)
  }
  scen
}

# Filter interpolated VALUE parameters (numpar/bounds) to the scenario's DECLARED
# calendar slices (scen@modInp@sets$slice, sourced from scen@settings@calendar).
# A sampled calendar declares fewer slices (that is the point of sampling); any
# interpolated row for a slice the calendar does not declare is excessive data.
# GAMS errors on data indexed by an undeclared slice; other backends tolerate it
# via gating maps but carry needless bulk (e.g. pWeather at full 8760h). Legacy
# interpolate_model filtered by default; this restores it. Runs BEFORE fold/prune
# so both the sparse (fold) and dense paths benefit. Maps are built later and are
# already calendar-consistent, so only value parameters need this.
.filter_params_by_declared_slices <- function(scen, verbose = FALSE) {
  sl <- scen@modInp@sets$slice
  if (is.null(sl) || length(sl) == 0L) return(scen)
  sl <- as.character(sl)
  for (pn in names(scen@modInp@parameters)) {
    param <- scen@modInp@parameters[[pn]]
    if (is.null(param) || !(param@type %in% c("numpar", "bounds"))) next
    pdata <- get_data_slot(param)
    if (is.null(pdata) || nrow(pdata) == 0L) next
    scols <- intersect(c("slice", "slicep", "slice.1"), colnames(pdata))
    if (length(scols) == 0L) next
    # NA in a slice column is a wildcard ("applies to all slices", the sparse/fold
    # representation of a slice-independent value) -> always keep it. Drop a row only
    # when it names a CONCRETE slice the calendar does not declare.
    keep <- Reduce(`&`, lapply(scols, function(cc) {
      v <- as.character(pdata[[cc]]); is.na(v) | v %in% sl
    }))
    if (all(keep)) next
    new_data <- as.data.frame(pdata)[keep, , drop = FALSE]
    if (isTRUE(verbose)) {
      message("cal-filter '", pn, "': ", nrow(pdata), " -> ", nrow(new_data), " rows")
    }
    scen <- .interp_write_param(scen, pn, new_data)
  }
  scen
}

#' Validate interpolated scenario parameters
#'
#' Runs a set of post-interpolation consistency checks over the numeric / bounds
#' / map parameters of a scenario and reports any issues. Checks:
#' \itemize{
#'   \item \strong{NA index columns}: no NA in a parameter's `dimSets` id
#'     columns. When `fold = TRUE`, NA is permitted only in the trimmable
#'     dimensions (region / slice / vintage, which fold encodes as wildcards);
#'     when `fold = FALSE`, no NA is permitted in any id column.
#'   \item \strong{Schema}: data columns match the declared `dimSets`
#'     (plus `value`, and `type` for bounds).
#'   \item \strong{Duplicate keys}: no duplicate id tuples.
#'   \item \strong{Map vs parameter (correctness)}: every tuple of a value map is
#'     covered by its source parameter; a missing value would otherwise be
#'     silently replaced by the solver default.
#'   \item \strong{Parameter vs map (efficiency)}: value parameter rows lie
#'     within the union of their maps (no orphan / out-of-domain rows). After
#'     trimming this must hold exactly.
#' }
#' @param scen scenario.
#' @param fold logical; whether the scenario is folded (NA wildcards allowed in
#'   trimmable dimensions).
#' @param action one of `"warn"` (default), `"stop"`, `"silent"`: how to report
#'   issues.
#' @returns (invisibly) a data frame of issues with columns
#'   `parameter`, `check`, `detail`.
#' @export
validate_scenario_parameters <- function(scen, fold = TRUE,
                                          action = c("warn", "stop", "silent")) {
  action <- match.arg(action)
  # Dimensions in which a fold wildcard (NA) is legitimate. `fold` may be the
  # legacy logical (TRUE -> the original trimmable dims) or the character vector of
  # dims actually folded (region / slice / year / comm / tech / stg / trade).
  trim_dims <- if (isTRUE(fold)) c("region", "slice", "vintage")
    else if (is.character(fold)) fold else character(0)
  issues <- list()
  add <- function(parameter, check, detail) {
    issues[[length(issues) + 1L]] <<-
      data.frame(parameter = parameter, check = check, detail = detail,
                 stringsAsFactors = FALSE)
  }

  # Progress bar: this pass scans every parameter's rows (and the value-map
  # coverage joins below), the slowest stage on large models. Mirror ob2mi's
  # progressor so a registered handler shows movement instead of a frozen line.
  # Silent when no handler is set. Steps: all parameters + all value maps.
  pnames <- names(scen@modInp@parameters)
  .n_val <- length(pnames) + length(.value_map_def)
  .prg <- if (.n_val > 0) progressr::progressor(steps = .n_val) else NULL

  for (pn in pnames) {
    # Advance for EVERY parameter (incl. skipped types/empties) so the bar
    # reaches 100% instead of freezing short of the end.
    if (!is.null(.prg)) .prg(message = pn)
    param <- scen@modInp@parameters[[pn]]
    ptype <- as.character(param@type)
    if (!(ptype %in% c("numpar", "bounds", "map"))) next
    d <- get_data_slot(param)
    if (is.null(d) || nrow(d) == 0) next
    d <- as.data.frame(d)
    # Id columns are the actual data columns minus the `value` column. Using the
    # data column names (rather than `intersect(dimSets, ...)`) preserves
    # repeated dimensions, which data frames disambiguate with a `.N` suffix
    # (e.g. a (slice, slice) map becomes columns `slice`, `slice.1`); collapsing
    # them would mis-key duplicate and NA checks. For bounds parameters the
    # bound `type` (lo / up / fx) is kept as an id column so that a `lo` and an
    # `up` row on the same dimensions are NOT counted as a duplicate key; `type`
    # is absent from `dimSets`, so the schema check below ignores it, and it is
    # never NA, so the NA check is unaffected.
    val_cols <- "value"
    id_cols <- setdiff(colnames(d), val_cols)
    # Base dimension name (drops the `.N` disambiguation suffix) for matching a
    # column against the trimmable-dimension set.
    base_dim <- sub("\\.[0-9]+$", "", id_cols)

    # NA index columns
    na_allowed <- id_cols[base_dim %in% trim_dims]
    for (cc in setdiff(id_cols, na_allowed)) {
      n_na <- sum(is.na(d[[cc]]))
      if (n_na > 0) {
        add(pn, "na_index", paste0(n_na, " NA in id column '", cc, "'"))
      }
    }

    # Schema: declared dimSets present (compare multiset counts so a repeated
    # dimension must appear the right number of times in the data).
    want <- table(param@dimSets)
    have <- table(base_dim)
    missing_cols <- character(0)
    for (dn in names(want)) {
      have_n <- if (dn %in% names(have)) have[[dn]] else 0L
      if (have_n < want[[dn]]) {
        missing_cols <- c(missing_cols,
                          paste0(dn, " (need ", want[[dn]], ", have ", have_n, ")"))
      }
    }
    if (length(missing_cols) > 0) {
      add(pn, "schema", paste0("missing id column(s): ",
                               paste(missing_cols, collapse = ", ")))
    }

    # Duplicate id tuples
    if (length(id_cols) > 0) {
      key <- d[, id_cols, drop = FALSE]
      n_dup <- sum(duplicated(key))
      if (n_dup > 0) {
        add(pn, "duplicate_key", paste0(n_dup, " duplicate id tuple(s)"))
      }
    }
  }

  # Map <-> parameter coverage checks.
  #
  # A value map is built from the UNION of its source parameters' value domains
  # (`.value_map_def`): a map tuple is covered if ANY source supplies a defined
  # value at it. The check is therefore MAP-centric and union-aware -- it pools
  # all sources before testing coverage, so an individually empty source (e.g.
  # `pStorageCostInp`) that is covered by a sibling (`pStorageCostStore`) is not
  # reported.
  for (mp in names(.value_map_def)) {
    if (!is.null(.prg)) .prg(message = mp)
    mpar <- scen@modInp@parameters[[mp]]
    if (is.null(mpar)) next
    md <- get_data_slot(mpar)
    if (is.null(md) || nrow(md) == 0) next
    md <- as.data.frame(md)
    map_dims <- colnames(md)
    sources <- .value_map_def[[mp]]$source

    # Union of the source value domains, projected onto the map's columns and
    # restricted to defined (non-NA) values -- mirrors `.build_value_map_std`.
    union_src <- NULL
    for (sp in sources) {
      sp_par <- scen@modInp@parameters[[sp]]
      if (is.null(sp_par)) next
      sd <- get_data_slot(sp_par)
      if (is.null(sd) || nrow(sd) == 0) next
      sd <- as.data.frame(sd)
      if (!is.null(sd$value)) sd <- sd[!is.na(sd$value), , drop = FALSE]
      if (nrow(sd) == 0) next
      sd <- sd[, intersect(map_dims, colnames(sd)), drop = FALSE]
      union_src <- dplyr::bind_rows(union_src, dplyr::distinct(sd))
    }

    src_label <- paste(sources, collapse = " + ")
    if (is.null(union_src) || nrow(union_src) == 0) {
      add(mp, "map_not_in_param",
          paste0("map has ", nrow(md),
                 " tuple(s) but source(s) '", src_label, "' are empty"))
      next
    }
    union_src <- dplyr::distinct(union_src)

    key <- intersect(map_dims, colnames(union_src))
    if (length(key) == 0) next
    # A folded source carries wildcard (NA) values in trimmable dimensions that
    # cover every member of that dimension. Such columns must be dropped from
    # the coverage key, otherwise a literal join would falsely report the
    # explicit map tuples as uncovered.
    wild_cols <- key[vapply(key, function(cc) any(is.na(union_src[[cc]])),
                            logical(1))]
    jkey <- setdiff(key, wild_cols)
    if (length(jkey) == 0) next
    mk <- dplyr::distinct(md[, jkey, drop = FALSE])
    uncovered <- dplyr::anti_join(
      mk, dplyr::distinct(union_src[, jkey, drop = FALSE]), by = jkey
    )
    if (nrow(uncovered) > 0) {
      add(mp, "map_not_in_param",
          paste0(nrow(uncovered), " map tuple(s) not covered by source(s) '",
                 src_label, "' (would use solver default)"))
    }
  }

  res <- if (length(issues) == 0) {
    data.frame(parameter = character(0), check = character(0),
               detail = character(0), stringsAsFactors = FALSE)
  } else {
    do.call(rbind, issues)
  }

  if (nrow(res) > 0 && action != "silent") {
    msg <- paste0(
      "validate_scenario_parameters: ", nrow(res), " issue(s):\n",
      paste0("  [", res$check, "] ", res$parameter, ": ", res$detail,
             collapse = "\n")
    )
    if (action == "stop") stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }

  invisible(res)
}

#' Interpolate all year-indexed numeric/bounds parameters of a scenario
#'
#' @param scen scenario object whose `modInp@parameters` already hold the raw
#'   (sparse) values collected by `ob2mi`.
#' @param drop_default logical; when `TRUE` rows whose interpolated value equals
#'   the parameter default are dropped (the solver substitutes the declared
#'   `default`), yielding a smaller data slot. When `FALSE` (the default, and
#'   the legacy behaviour) default-valued rows are kept and written explicitly.
#'   Dropping defaults requires the solver/writer to honour declared parameter
#'   defaults (GLPK / JuMP / Pyomo do; GAMS support is unverified).
#' @returns the scenario with interpolated parameter data.
#' @export
interpolate_parameters <- function(scen, drop_default = FALSE) {
  milestones <- as.integer(scen@modInp@sets$year)
  if (length(milestones) == 0) {
    stop("Milestone years (scen@modInp@sets$year) are not defined.")
  }

  for (pn in names(scen@modInp@parameters)) {
    param <- scen@modInp@parameters[[pn]]
    ptype <- as.character(param@type)
    if (!(ptype %in% c("numpar", "bounds"))) next
    if (!("year" %in% param@dimSets)) next

    raw <- get_data_slot(param)
    if (is.null(raw) || nrow(raw) == 0) next
    raw <- as.data.frame(raw)
    non_year <- setdiff(param@dimSets, "year")

    # Per-parameter override of the global `drop_default`. A parameter may set
    # `@misc$dropIfEmpty` (from the `dropIfEmpty` field in modInp.yml) to force
    # its own policy regardless of the global flag: `TRUE` drops default-valued
    # rows (so an all-default parameter collapses to empty and its constraint is
    # dropped), `FALSE` keeps them (so a structurally required parameter such as
    # `pTechCap2act` is always emitted).
    eff_drop <- if (!is.null(param@misc$dropIfEmpty)) {
      isTRUE(param@misc$dropIfEmpty)
    } else {
      isTRUE(drop_default)
    }

    if (ptype == "numpar") {
      out <- .interp_one_series(
        raw, set_cols = non_year, milestones = milestones,
        int_rule = .interp_rule_token(param@interpolation),
        def_val = param@defVal[1]
      )
      if (isTRUE(eff_drop) && length(param@defVal) >= 1) {
        out <- out |> dplyr::filter(value != param@defVal[1])
      }
      if (nrow(out) == 0) {
        # All rows collapsed to the default (or none survived). Clear the slot
        # to an empty (0-row) table with the correct schema rather than leaving
        # the stale sparse raw data in place, which would otherwise be written
        # with NA index columns and crash the solver (out-of-domain).
        out <- .empty_param_data(param)
      } else {
        out <- out |> dplyr::select(dplyr::all_of(c(param@dimSets, "value")))
      }
    } else {
      # bounds: long format with `type` in {lo, up, fx}.
      rule <- param@interpolation
      defv <- param@defVal

      # Fold fixed bounds (type == "fx") into equal lower and upper bounds so
      # the fixed value is interpolated and survives as lo == up. Done before
      # the conflict check and the lo/up interpolation loop.
      if ("fx" %in% as.character(raw$type)) {
        fx_rows <- raw |> dplyr::filter(as.character(type) == "fx")
        raw <- raw |> dplyr::filter(as.character(type) != "fx")
        raw <- dplyr::bind_rows(
          raw,
          fx_rows |> dplyr::mutate(type = "lo"),
          fx_rows |> dplyr::mutate(type = "up")
        )
      }

      # Hard stop if the GIVEN data points already conflict (lo > up at the
      # same key and year). Such conflicts are input errors, not artefacts of
      # interpolation, so they must be fixed in the model data.
      given <- raw |>
        dplyr::select(dplyr::all_of(c(non_year, "year", "type", "value"))) |>
        dplyr::mutate(type = as.character(type)) |>
        tidyr::pivot_wider(names_from = "type", values_from = "value")
      if (all(c("lo", "up") %in% names(given))) {
        bad <- given |> dplyr::filter(!is.na(lo), !is.na(up), lo > up)
        if (nrow(bad) > 0) {
          stop(
            "Conflicting bounds in the input data for parameter '", pn,
            "' (lower > upper at given data points):\n   ",
            paste(utils::capture.output(print(as.data.frame(bad))),
                  collapse = "\n   "),
            "\nPlease fix the conflicting lower/upper bounds in the model data.\n"
          )
        }
      }

      # Interpolate each bound separately.
      parts <- list()
      for (tp in c("lo", "up")) {
        idx <- if (tp == "lo") 1L else min(2L, length(rule))
        didx <- if (tp == "lo") 1L else min(2L, length(defv))
        sub <- raw |> dplyr::filter(as.character(type) == tp)
        if (nrow(sub) == 0) next
        o <- .interp_one_series(
          sub, set_cols = non_year, milestones = milestones,
          int_rule = .interp_rule_token(rule[idx]), def_val = defv[didx]
        )
        # Drop default-valued rows only when explicitly requested; by default
        # (legacy) bounds equal to their default are kept and written.
        if (isTRUE(eff_drop)) o <- o |> dplyr::filter(value != defv[didx])
        if (nrow(o) > 0) parts[[tp]] <- o |> dplyr::mutate(type = tp)
      }
      out <- dplyr::bind_rows(parts)
      if (nrow(out) == 0) {
        out <- .empty_param_data(param)
      } else {

      # Separate interpolation of lo/up can produce lo > up at interpolated
      # years even when the given anchors are consistent. Input-level conflicts
      # already errored above, so any remaining conflict is an interpolation
      # artefact: warn and clamp (lo <- min, up <- max).
      if (length(parts) == 2L) {
        wide <- out |>
          tidyr::pivot_wider(names_from = "type", values_from = "value")
        confl <- wide |> dplyr::filter(!is.na(lo), !is.na(up), lo > up)
        if (nrow(confl) > 0) {
          warning(
            "Interpolation produced lower > upper bounds for parameter '", pn,
            "' at ", nrow(confl), " point(s); clamping to lo = min, up = max.\n   ",
            paste(utils::capture.output(print(as.data.frame(utils::head(confl, 10)))),
                  collapse = "\n   ")
          )
          wide <- wide |>
            dplyr::mutate(
              .lo = dplyr::if_else(!is.na(lo) & !is.na(up), pmin(lo, up), lo),
              .up = dplyr::if_else(!is.na(lo) & !is.na(up), pmax(lo, up), up),
              lo = .lo, up = .up
            ) |>
            dplyr::select(-.lo, -.up)
        }
        out <- wide |>
          tidyr::pivot_longer(
            cols = dplyr::any_of(c("lo", "up")),
            names_to = "type", values_to = "value", values_drop_na = TRUE
          )
      }
      out <- out |> dplyr::select(dplyr::all_of(c(param@dimSets, "type", "value")))
      }
    }

    new_data <- as.data.frame(out)
    if ("type" %in% colnames(new_data)) {
      new_data$type <- factor(as.character(new_data$type), levels = c("lo", "up"))
    }
    new_data <- new_data |>
      force_cols_classes() |>
      as.data.frame() |>
      (\(d) d[, colnames(param@data), drop = FALSE])()

    # The raw (sparse) values were written to disk by `ob2mi`. Interpolation
    # supersedes them, so the on-disk `data` slot is replaced with the
    # interpolated result (the in-memory `@data` is kept as the empty schema).
    scen <- .interp_write_param(scen, pn, new_data)
  }

  scen
}

"ANY"
"ANYREGION"
"ANYYEAR"
"ANYSLICE"
"ANYVINTAGE"

# Expand sets for parameter
#
# Expand NA sets in a data frame where parameter is not NA
# @param x data frame with columns for sets and parameters
# @param param name of the parameter to expand sets for
# @param full_sets list of full sets to expand
# @param filter_sets list of sets with subset elements to filter
# @param ... additional arguments
# @returns data frame with expanded sets for the `param` parameter.
# The parameter value(s) are repeated for each NA element of the
# combination of sets.
# @export
# .expand_sets <- function(x,
#                         param,
#                         process_name,
#                         full_sets,
#                         filter_sets = c("region", "year", "slice"),
#                         ...) {
#   browser() # !!! ToDo: finish
#   if (F) {
#     # debug
#     ECOA <- scen@model@data$utopia_repository@data$ECOA
#     # x <- ECOA@ceff; param <- "cinp2use"
#     x <- ECOA@capacity; param <- "stock"
#     process_name <- "ECOA"
#     full_sets <- scen@modInp@sets
#     # filter_sets:
#     # region: create from ECOA@region,
#     # year: create from lifespan of ECOA
#     # slice: create from ECOA@timeframe
#   }
#
#   stopifnot(is.character(param))
#   stopifnot(length(param) == 1)
#   stopifnot(param %in% names(x))
#
#   x <- x |>
#     select(any_of(c(filter_sets, param))) |>
#     filter(!is.na(.data[[param]])) |>
#     as.data.table()
#
#   # expand process' years
#   if ("year" %in% names(x) && any(is.na(x$year))) {
#     pr_years <- full_sets$process_years |>
#       filter(process == process_name)
#       # select(year, year_full) |>
#       # unique()
#
#     # x_na <- |>
#     x |> filter(is.na(year))
#
#
#   }
#
#
#   # expand process' regions
#   if ("region" %in% names(x) && any(is.na(x$region))) {
#     # x_na <- |>
#       x |>
#       filter(is.na(region)) |>
#       select(-region) |>
#       # full_sets$process_region[[]]
#
#
#       left_join(
#         named_list_to_df(, col_names = c("region", "region_full")),
#         by = "region"
#       ) |>
#       select(-region) |>
#       rename(region = region_full)
#   }
#
#
#
#
#
#
#   # filter out
#
#   # identify sets for the parameter
#
#   # check if there are NA elements in the parameter
# }

#' Operational timeframe of a commodities
#'
#' @param scen scenario object
#' @param comm character vector of commodity names, if not provided,
#' all commodities retrieved from the scenario object using the
#' `collect_set_names` function.
#'
#' @returns a named list mapping each commodity to its timeframe.
#' @export
map_comm_timeframe <- function(scen, comm = NULL) {
  apply_to_scenario_data(
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
#' @returns a named list mapping each process to its operational timeframe.
#' @export
get_process_timeframe <- function(scen, process = NULL,
                                  comm_timeframe = NULL) {
  # browser()
  # collect assigned timeframes for all processes in the scenario
  ll <- apply_to_scenario_data(
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
  # select the FINEST (highest-resolution) timeframe -- the process operates at
  # the resolution of its finest commodity (e.g. a generator producing hourly
  # ELC, or an electrolyzer consuming hourly ELC, must run hourly even if other
  # commodities are annual; finer flows up-aggregate to the coarser balances).
  # This restores the legacy rule; the new multi-level up-aggregation
  # (mSliceFamily/pSliceAgg, eqOutTot/eqInpTot) only flows fine->coarse, so a
  # process pinned to a coarser timeframe than its commodities cannot meet the
  # finer balance and demand leaks to imports.
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
    unique()

  # Only run the per-process consistency check when at least one process carries
  # a directly-assigned timeframe. Otherwise `check_timeframe` is empty and the
  # grouped `max()` calls below would warn on a zero-length vector ("no
  # non-missing arguments to max; returning -Inf").
  if (nrow(check_timeframe) > 0) {
    check_timeframe <- check_timeframe |>
      group_by(process) |>
      mutate(
        # timeframe_min = min(timeframe_rank)
        timeframe_max_pro = max(timeframe_rank_process),
        timeframe_max_comm = max(timeframe_rank_comm)
      ) |>
      filter(timeframe_max_pro != timeframe_max_comm) |>
      filter(timeframe_rank_process < timeframe_rank_comm) |>
      ungroup()
  }
  check_timeframe <- as.data.table(check_timeframe)

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
    arrange(dplyr::desc(timeframe_rank)) |> # finest (highest-rank) timeframe
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
#' @returns a named list mapping each process to its class.
#' @export
get_process_class <- function(scen, process = NULL, classes = NULL) {
  # collect classes for processes in the scenario
  if (is.null(classes)) {
    classes <- c("process", "technology", "storage", "supply", "import",
                 "trade", "export", "demand")
  }

  ll <- apply_to_scenario_data(
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
#' @returns a named list mapping each process to its input commodities.
#' @export
get_process_inputs <- function(scen, process = NULL, classes = NULL) {
  if (is.null(classes)) {
    classes <- c(
      "process", "technology", "storage",
      # "supply", "import" # no inputs
      "trade", "export", "demand"
    )
  }

  # collect all inputs for each process

  ll <- apply_to_scenario_data(
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
#' @param scen scenario object
#' @param process character vector of process names, if not provided,
#' all processes retrieved from the scenario object
#' @param classes character vector of class names to search for
#'
#' @returns a named list mapping each process to its output commodities.
#' @export
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

  ll <- apply_to_scenario_data(
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

#' Build the auxiliary-commodity membership maps (input / output direction)
#'
#' For each object of `classes`, splits its declared auxiliary commodities
#' (`aeff$acomm`) into the input-direction map (`inp_name`) and the
#' output-direction map (`out_name`) according to whether any `*2ainp` /
#' `*2aout` conversion factor is supplied for that commodity. Mirrors the
#' direction split in the legacy technology / storage `.obj2modInp` methods
#' (obj2modInp.R L840-856 / L2084-2096).
#'
#' @param scen scenario object.
#' @param classes object class(es) carrying an `aeff` slot.
#' @param key key column name (e.g. "tech", "stg").
#' @param fmp function mapping a parameter name to its on-disk path.
#' @param inp_name input-direction membership map name.
#' @param out_name output-direction membership map name.
#' @returns updated scenario object.
#' @keywords internal
.build_aux_membership <- function(scen, classes, key, fmp,
                                  inp_name, out_name) {
  inp_rows <- list()
  out_rows <- list()
  for (i in seq_along(scen@model@data)) {
    for (j in seq_along(scen@model@data[[i]]@data)) {
      x <- scen@model@data[[i]]@data[[j]]
      if (!inherits(x, classes)) next
      if (!.hasSlot(x, "aeff")) next
      ae <- x@aeff
      if (is.null(ae) || nrow(ae) == 0) next
      ae <- ae[!is.na(ae$acomm), , drop = FALSE]
      if (nrow(ae) == 0) next
      inp_cols <- grep("2ainp$", colnames(ae), value = TRUE)
      out_cols <- grep("2aout$", colnames(ae), value = TRUE)
      if (length(inp_cols) > 0) {
        hi <- apply(!is.na(ae[, inp_cols, drop = FALSE]), 1, any)
        cmm <- unique(as.character(ae$acomm[hi]))
        if (length(cmm) > 0) {
          inp_rows[[length(inp_rows) + 1L]] <-
            data.frame(k = x@name, comm = cmm, stringsAsFactors = FALSE)
        }
      }
      if (length(out_cols) > 0) {
        ho <- apply(!is.na(ae[, out_cols, drop = FALSE]), 1, any)
        cmm <- unique(as.character(ae$acomm[ho]))
        if (length(cmm) > 0) {
          out_rows[[length(out_rows) + 1L]] <-
            data.frame(k = x@name, comm = cmm, stringsAsFactors = FALSE)
        }
      }
    }
  }
  set_one <- function(scen, nm, rows) {
    df <- if (length(rows) > 0) {
      do.call(rbind, rows)
    } else {
      data.frame(k = character(), comm = character(), stringsAsFactors = FALSE)
    }
    names(df)[names(df) == "k"] <- key
    scen@modInp@parameters[[nm]] <-
      d2p(scen@modInp@parameters[[nm]], df, fmp(nm))
    scen
  }
  scen <- set_one(scen, inp_name, inp_rows)
  scen <- set_one(scen, out_name, out_rows)
  scen
}

#' Get auxiliary commodities for each process
#'
#' @param scen scenario object
#' @param process character vector of process names, if not provided,
#' all processes retrieved from the scenario object
#' @param classes character vector of class names to search for
#'
#' @returns a named list mapping each process to its auxiliary commodities.
#' @export
get_process_aux <- function(scen, process = NULL, classes = NULL) {
  if (is.null(classes)) {
    classes <- c("process", "technology", "storage", "trade")
    # "demand", "supply", "import", "export") # no aux
  }
  # collect all outputs for each process

  ll <- apply_to_scenario_data(
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
#' @returns a named list, or a data.frame with columns `process` and `comm`.
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
  # Empty input: return an empty table that still carries `col_names`, so that
  # downstream joins (`by = col_names[1]`) find the expected columns instead of
  # failing with "Join columns in `y` must be present".
  if (length(nms) == 0) {
    out <- data.table()
    for (cn in col_names) out[[cn]] <- character(0)
    return(out)
  }
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

  # Region scope of each object (uniform rule for all classes):
  #   * `@region` is AUTHORITATIVE. Populated with region names -> the object
  #     exists only there; empty or NA -> it exists in ALL regions.
  #   * `region` columns in OTHER (parameter) slots only localize a value
  #     (NA = all regions / overrides default; a name = that region only, then
  #     interpolated). They NEVER restrict the object's scope. A parameter-slot
  #     region that is not within a populated `@region` is an error.
  #   * Trade has no `@region`; its scope is the structural route endpoints
  #     (`src`/`dst`).
  info <- apply_to_scenario_data(
    scen = scen,
    classes = classes,
    func = function(x) {
      reg <- if (.hasSlot(x, "region")) unique(as.character(x@region)) else character()
      reg <- reg[!is.na(reg)]            # NA in @region == all regions (no restriction)
      struct <- character()              # route endpoints (trade): structural scope
      param  <- character()              # region values in parameter slots
      for (s in slotNames(x)) {
        v <- slot(x, s)
        if (!inherits(v, "data.frame")) next
        for (col in c("src", "dst")) {
          if (col %in% colnames(v)) struct <- c(struct, v[[col]])
        }
        if ("region" %in% colnames(v)) param <- c(param, v[["region"]])
      }
      o <- list()
      o[[x@name]] <- list(
        reg    = reg,
        struct = unique(struct[!is.na(struct)]),
        param  = unique(param[!is.na(param)])
      )
      o
    }
  )

  nn <- list()
  for (i in names(info)) {
    reg <- info[[i]]$reg; struct <- info[[i]]$struct; param <- info[[i]]$param
    if (length(reg) > 0) {
      # @region populated: authoritative. Any region used in a slot must be in it.
      bad <- setdiff(c(param, struct), reg)
      if (length(bad) > 0) {
        stop("Process '", i, "': region(s) '", paste(bad, collapse = "', '"),
             "' appear in a slot but are not in its @region scope.")
      }
      nn[[i]] <- reg
    } else if (length(struct) > 0) {
      # no @region: structural route endpoints define the scope (trade).
      nn[[i]] <- struct
    } else {
      # no @region, no routes: the object exists in ALL regions. Parameter-slot
      # regions only localize values; they do not restrict the scope.
      nn[[i]] <- scen_regions
    }
    bad2 <- setdiff(nn[[i]], scen_regions)
    if (length(bad2) > 0) {
      stop("Process '", i, "': region(s) '", paste(bad2, collapse = "', '"),
           "' are not declared in the scenario region set.")
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
  ll_start_end <- apply_to_scenario_data(
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

        if (nrow(d) > 0) {
          dd <- d
        } else {
          # Object has a `start` slot but no start/end data: it is available in
          # all of its regions across the whole horizon. Emit an NA window so
          # the NA-region expansion below fills every region (and all years).
          dd <- data.table(
            process = x@name,
            region = NA_character_,
            start = NA_integer_,
            end = NA_integer_
          )
        }
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

  ll <- apply_to_scenario_data(
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
  # check if scen@modInp@sets$process_years exists and/or rerun

  if (is.null(scen@modInp@sets$process_years)) {
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
    scen@modInp@sets$process_years <- rbind(
      scen@modInp@sets$process_invest_year,
      scen@modInp@sets$process_stock_year
    ) |>
      unique() |>
      arrange(process, year) |>
      as.data.table()

  }

  process_years <- scen@modInp@sets$process_years

  if (!is.null(process)) {
    ii <- process_years$process %in% process
    process_years <- process_years[ii, ]
  }
  if (!is.null(classes)) {
    process_class <-
      get_process_class(scen, process = process, classes = classes) |>
      named_list_to_df(col_names = c("process", "class"))

    process_years <- process_years |>
      left_join(
        process_class,
        by = "process"
      ) |>
      filter(class %in% classes) |>
      select(-class)
  }

  return(process_years)
}

# get_comm_region <- function(scen, comm = NULL) {
#
# }

#' Return default value for one, several, or all parameters
#'
#' @param scen scenario object
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




