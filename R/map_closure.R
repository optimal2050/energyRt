# =========================================================================== #
# map_closure.R  —  commodity<->region reachability map (family "closure")
#
# map_mCommReg(scen, fmp): the (comm, region) set where each commodity is
# available (primary supply/import, secondary process outputs, aux, emissions,
# traded, demand). Faithful port of the inline block formerly in interp_mod
# (archived to drafts/legacy-mapping/closure-inline.R); it also populates the
# helper sets primary_comm_region / secondary_comm_region / comm_region and runs
# the declared-commodity / served-demand validation. Built via
# build_mappings(recipes = "closure") in interp_mod, after the membership maps.
#
# NOTE: the traded-primary-commodity branch (a primary supply/import commodity
# that is also traded across regions) carried a pre-existing `browser()` debug
# stop from the legacy inline block. The test models never reach it; an older
# model with a traded primary commodity does. The `browser()` was removed so such
# models run -- the branch already adds the shipped-to regions to `comm_region`.
# =========================================================================== #

map_mCommReg <- function(scen, fmp) {
  # Membership maps built just before by build_mappings(recipes = "membership").
  mTradeComm <- as.data.frame(get_data_slot(scen@modInp@parameters[["mTradeComm"]]))
  mDemComm   <- as.data.frame(get_data_slot(scen@modInp@parameters[["mDemComm"]]))

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
    # A primary commodity (supply/import) that is also traded becomes available in
    # the regions it can be shipped to. (Was a pre-existing `browser()` debug stop
    # ported from the legacy inline block; removed so models with traded primary
    # commodities run. !!! revisit: confirm this covers multi-hop / aux cases.)
    comm_region <- traded_primary_comm_region |>
      select(comm, region) |>
      rbind(comm_region) |>
      unique()
  }

  ### Secondary (processed) commodities' availability in regions (incl. trade) ####
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
    select(output, region) |>
    filter(!is.na(output)) |>
    rename(comm = output) |>
    unique()

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

  comm_region <- aux_comm_region |>
    rbind(comm_region) |>
    unique()

  ### Emission commodities ####
  emiss_comm <- apply_to_scenario_data(
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
  # [nested-regions] judge reachability at the level the commodity is BALANCED
  # at. Steel made in R1 and demanded nationally is served, because the national
  # balance pools the regions; comparing per fine region would report a spurious
  # infeasibility. A no-op for commodities at the finest level.
  comm_region_dem_check <-
    comm_region |>
    filter(comm %in% unique(demand_comm_region$comm)) |>
    .lift_to_comm_level(scen)

  comm_region_dem_check <- anti_join(
    .lift_to_comm_level(demand_comm_region, scen),
    comm_region_dem_check,
    by = c("comm", "region")
  ) |>
    unique()

  # Advisory, not a guard: `comm_region_dem_check` is never read again and the
  # `rbind()` below adds these demand pairs regardless, so the mapping is the
  # same either way. Warn and let the solver return the verdict -- an
  # unservable demand is a normal modelling state worth inspecting, not a
  # reason to refuse to build. `options(en.model_checks_stop = TRUE)` restores
  # the previous fail-fast behaviour.
  if (nrow(comm_region_dem_check) > 0) {
    msg <- paste0(
      "There is no supply, production, interregional trade, or import for demand-commodities in regions:\n   ",
      paste(capture.output(print(comm_region_dem_check)), collapse = "\n   "),
      "\nThe model will be infeasible unless these commodities are supplied.\n"
    )
    if (isTRUE(getOption("en.model_checks_stop", FALSE))) {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }

  comm_region <- rbind(comm_region, demand_comm_region) |>
    unique() |>
    arrange(comm, region)

  ### Final check of comm_region ####
  comm_region_check <- comm_region |>
    filter(!(comm %in% scen@modInp@sets$comm))

  if (nrow(comm_region_check) > 0) {
    stop(
      "The following commodities are not declared in the model:\n   ",
      paste(capture.output(print(comm_region_check)), collapse = "\n   "),
      "\nUse `newCommodity()` to create commodity objects to add to the model.\n"
    )
  }

  ii <- scen@modInp@sets$comm %in% unique(comm_region$comm)
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

  # [nested-regions] the process/commodity level rule. Checked here because this
  # is the first point where process_region and process inputs/outputs are all
  # populated.
  .assert_process_geolevel(scen)

  # Declared timeslices/regions must match the commodity's own level for every
  # class that has no aggregation path (see check_levels.R). Same reason for
  # checking here: it needs the collected process/commodity relations.
  .check_process_levels(scen)

  scen
}

# -- registry for the closure family --------------------------------------- #
.closure_builders <- list(
  mCommReg = map_mCommReg
)
