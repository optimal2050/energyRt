# =========================================================================== #
# map_calendar.R  —  calendar / horizon mapping builders (family "calendar")
#
# One `map_<Name>(scen, fmp) -> scen` per calendar map. These are model-object
# independent: they derive from the calendar (timeslices, ancestry, next-in-timeslice) and
# the horizon milestones. Registered in `.calendar_builders`. Reuses the live
# helpers `.set_calendar_map`, `.comm_timeslice_df`, `.process_timeslice_df`,
# `.proc_timeslice_for`, `merge0` from mapping_engine.R.
#
# mStorageFullYear / mTechFullYear (object `@fullYear` flag) are built here; the
# remaining calendar-tagged map mWeatherTimeslice is emitted with the weather object.
# =========================================================================== #

# write only when the builder yields a non-empty frame (matches recipe_calendar).
.set_cal <- function(scen, name, df, fmp) {
  if (is.null(df) || nrow(df) == 0) return(scen)
  .set_calendar_map(scen, name, df, fmp)
}

# -- shared calendar accessors --------------------------------------------- #
.cal_timeslices <- function(scen) as.character(scen@settings@calendar@timeslice_share$timeslice)
.cal_anc    <- function(scen) dplyr::as_tibble(scen@settings@calendar@timeslice_ancestry)
.cal_mid    <- function(scen) as.integer(scen@settings@horizon@intervals$mid)

# timeslice ancestry-expansion (parent -> self + all descendants), shared by
# mTimesliceParentChildE and mCommTimesliceOrParent.
.cal_spce <- function(scen) {
  timeslices <- .cal_timeslices(scen)
  dplyr::bind_rows(
    dplyr::tibble(timeslice = timeslices, timeslicep = timeslices),
    .cal_anc(scen) |> dplyr::transmute(timeslice  = as.character(.data$parent),
                                       timeslicep = as.character(.data$child))
  ) |> dplyr::distinct()
}

# -- per-object / per-commodity timeslice maps --------------------------------- #
# The table is written for the builders that read it; the rule (R/map-rules.R)
# is recorded beside it and defines the map once the tables are stripped.
.proc_timeslice_map <- function(scen, name, cls, key, fmp) {
  scen <- .set_cal(scen, name, .proc_timeslice_for(.process_timeslice_df(scen), get_process_class(scen), cls, key), fmp)
  .set_map_rule(scen, name, .proc_timeslice_rule(scen, cls, key))
}
map_mCommTimeslice  <- function(scen, fmp) {
  scen <- .set_cal(scen, "mCommTimeslice", .comm_timeslice_df(scen), fmp)
  .set_map_rule(scen, "mCommTimeslice", .comm_timeslice_rule(scen))
}
map_mTechTimeslice  <- function(scen, fmp) .proc_timeslice_map(scen, "mTechTimeslice",  "technology", "tech",  fmp)
map_mSupTimeslice   <- function(scen, fmp) .proc_timeslice_map(scen, "mSupTimeslice",   "supply",     "sup",   fmp)
map_mTradeTimeslice <- function(scen, fmp) .proc_timeslice_map(scen, "mTradeTimeslice", "trade",      "trade", fmp)
map_mImpTimeslice   <- function(scen, fmp) .proc_timeslice_map(scen, "mImpTimeslice",   "import",     "imp",   fmp)
map_mExpTimeslice   <- function(scen, fmp) .proc_timeslice_map(scen, "mExpTimeslice",   "export",     "expp",  fmp)

# -- timeslice ancestry / next maps -------------------------------------------- #
map_mTimesliceParentChild <- function(scen, fmp) {
  df <- .cal_anc(scen) |>
    dplyr::transmute(timeslice = as.character(.data$parent),
                     timeslicep = as.character(.data$child))
  .set_cal(scen, "mTimesliceParentChild", df, fmp)
}

map_mTimesliceParentChildE <- function(scen, fmp) .set_cal(scen, "mTimesliceParentChildE", .cal_spce(scen), fmp)

# Immediate parent->child (one level), from @timeslice_family (not the transitive
# @timeslice_ancestry). Drives the up-aggregation of commodity totals between adjacent
# levels (agg-rewrite: replaces *2Lo down-disaggregation).
map_mTimesliceFamily <- function(scen, fmp) {
  df <- dplyr::as_tibble(scen@settings@calendar@timeslice_family) |>
    dplyr::transmute(timeslice = as.character(.data$parent),
                     timeslicep = as.character(.data$child))
  .set_cal(scen, "mTimesliceFamily", df, fmp)
}

# Commodity timeslice-or-parent aggregation map: for each commodity, maps any
# finer-or-equal timeslice (`timeslicep`) up to the commodity's own timeslice level (`timeslice`).
map_mCommTimesliceOrParent <- function(scen, fmp) {
  cs <- .comm_timeslice_df(scen)
  if (is.null(cs) || nrow(cs) == 0) return(scen)
  cs   <- as.data.frame(cs)
  spce <- as.data.frame(.cal_spce(scen))
  l1 <- merge0(
    data.frame(comm = unique(cs$comm), stringsAsFactors = FALSE),
    data.frame(timeslice = as.character(.cal_timeslices(scen)), stringsAsFactors = FALSE)
  )
  l2 <- as.data.frame(merge0(cs, spce)) |>
    dplyr::select(dplyr::all_of(c("comm", "timeslice", "timeslicep")))
  l3 <- l2 |>
    dplyr::select(dplyr::all_of(c("comm", "timeslicep"))) |>
    dplyr::distinct() |>
    dplyr::rename(timeslice = "timeslicep")
  l3 <- rbind(l1, l3)
  l3 <- l3[!duplicated(l3) & !duplicated(l3, fromLast = TRUE), , drop = FALSE]
  l3$timeslicep <- l3$timeslice
  .set_cal(scen, "mCommTimesliceOrParent", rbind(l2, l3), fmp)
}

map_mTimesliceNext <- function(scen, fmp) {
  nxt <- dplyr::as_tibble(scen@settings@calendar@next_in_timeframe)
  if (nrow(nxt) == 0) return(scen)
  .set_cal(scen, "mTimesliceNext",
           nxt |> dplyr::transmute(timeslice = as.character(.data$timeslice),
                                   timeslicep = as.character(.data$timeslicep)), fmp)
}

map_mTimesliceFYearNext <- function(scen, fmp) {
  nxt <- dplyr::as_tibble(scen@settings@calendar@next_in_year)
  if (nrow(nxt) == 0) return(scen)
  .set_cal(scen, "mTimesliceFYearNext",
           nxt |> dplyr::transmute(timeslice = as.character(.data$timeslice),
                                   timeslicep = as.character(.data$timeslicep)), fmp)
}

# -- milestone (horizon) maps ---------------------------------------------- #
map_mMilestoneFirst   <- function(scen, fmp) .set_cal(scen, "mMilestoneFirst", dplyr::tibble(year = min(.cal_mid(scen))), fmp)
map_mMilestoneLast    <- function(scen, fmp) .set_cal(scen, "mMilestoneLast",  dplyr::tibble(year = max(.cal_mid(scen))), fmp)
map_mMidMilestone     <- function(scen, fmp) .set_cal(scen, "mMidMilestone",   dplyr::tibble(year = .cal_mid(scen)), fmp)
map_mMilestoneNext    <- function(scen, fmp) {
  mid <- .cal_mid(scen)
  .set_cal(scen, "mMilestoneNext",
           dplyr::tibble(year = mid[-length(mid)], yearp = mid[-1]), fmp)
}
map_mMilestoneHasNext <- function(scen, fmp) {
  mid <- .cal_mid(scen)
  .set_cal(scen, "mMilestoneHasNext", dplyr::tibble(year = mid[-length(mid)]), fmp)
}

# -- identity maps ---------------------------------------------------------- #
map_mSameTimeslice  <- function(scen, fmp) {
  s <- .cal_timeslices(scen)
  .set_cal(scen, "mSameTimeslice", dplyr::tibble(timeslice = s, timeslicep = s), fmp)
}
map_mSameRegion <- function(scen, fmp) {
  r <- as.character(scen@settings@region)
  .set_cal(scen, "mSameRegion", dplyr::tibble(region = r, regionp = r), fmp)
}

# mTechFullYear / mStorageFullYear: the (tech) / (stg) of objects flagged
# `@fullYear` (operate over the whole year rather than per-timeslice). Faithful port
# of the legacy .obj2modInp blocks (obj2modInp.R:2645 / :750).
.full_year_map <- function(scen, cls, key, name, fmp) {
  res <- apply_to_scenario_data(
    scen = scen, classes = cls, as_list = TRUE,
    func = function(x) {
      if (!isTRUE(x@fullYear)) return(NULL)
      o <- list(); o[[x@name]] <- stats::setNames(
        data.frame(x@name, stringsAsFactors = FALSE), key)
      o
    })
  df <- dplyr::distinct(dplyr::bind_rows(res))
  if (is.null(df) || nrow(df) == 0) return(scen)
  scen@modInp@parameters[[name]] <-
    d2p(scen@modInp@parameters[[name]], df, fmp(name))
  scen
}
map_mTechFullYear    <- function(scen, fmp) .full_year_map(scen, "technology", "tech", "mTechFullYear", fmp)
map_mStorageFullYear <- function(scen, fmp) .full_year_map(scen, "storage",    "stg",  "mStorageFullYear", fmp)

# mWeatherTimeslice: (weather, timeslice) over every LEAF timeslice (finest resolution; the
# timeslices that are never a parent in mTimesliceParentChild), for each weather object.
# Faithful port of the legacy weather .obj2modInp block (obj2modInp.R:166), whose
# `approxim$timeslice` is the leaf set. Registered LAST in the calendar family so
# mTimesliceParentChild is already built.
map_mWeatherTimeslice <- function(scen, fmp) {
  spc <- .gds(scen, "mTimesliceParentChild")
  if (is.null(spc)) return(scen)
  parents <- unique(spc$timeslice[spc$timeslice != spc$timeslicep])
  leaves  <- setdiff(unique(c(spc$timeslice, spc$timeslicep)), parents)
  if (length(leaves) == 0) return(scen)
  res <- apply_to_scenario_data(
    scen = scen, classes = "weather", as_list = TRUE,
    func = function(x) {
      o <- list(); o[[x@name]] <- data.frame(weather = x@name, timeslice = leaves,
                                             stringsAsFactors = FALSE)
      o
    })
  df <- dplyr::distinct(dplyr::bind_rows(res))
  if (is.null(df) || nrow(df) == 0) return(scen)
  scen@modInp@parameters[["mWeatherTimeslice"]] <-
    d2p(scen@modInp@parameters[["mWeatherTimeslice"]], df, fmp("mWeatherTimeslice"))
  # the rule: every weather series with every leaf timeslice
  .set_map_rule(scen, "mWeatherTimeslice", .map_rule(
    of = c("weather", "leaf"),
    tables = list(weather = data.frame(weather = unique(df$weather), stringsAsFactors = FALSE),
                  leaf = data.frame(timeslice = leaves, stringsAsFactors = FALSE)),
    cols = c("weather", "timeslice")))
}

# -- registry for the calendar family -------------------------------------- #
.calendar_builders <- list(
  mTechFullYear      = map_mTechFullYear,
  mStorageFullYear   = map_mStorageFullYear,
  mCommTimeslice         = map_mCommTimeslice,
  mTechTimeslice         = map_mTechTimeslice,
  mSupTimeslice          = map_mSupTimeslice,
  mTradeTimeslice        = map_mTradeTimeslice,
  mImpTimeslice          = map_mImpTimeslice,
  mExpTimeslice          = map_mExpTimeslice,
  mTimesliceParentChild  = map_mTimesliceParentChild,
  mTimesliceParentChildE = map_mTimesliceParentChildE,
  mTimesliceFamily       = map_mTimesliceFamily,
  mCommTimesliceOrParent = map_mCommTimesliceOrParent,
  mTimesliceNext         = map_mTimesliceNext,
  mTimesliceFYearNext    = map_mTimesliceFYearNext,
  mMilestoneFirst    = map_mMilestoneFirst,
  mMilestoneLast     = map_mMilestoneLast,
  mMidMilestone      = map_mMidMilestone,
  mMilestoneNext     = map_mMilestoneNext,
  mMilestoneHasNext  = map_mMilestoneHasNext,
  mSameTimeslice         = map_mSameTimeslice,
  mSameRegion        = map_mSameRegion,
  mWeatherTimeslice      = map_mWeatherTimeslice   # last: needs mTimesliceParentChild above
)


# ---------------------------------------------------------------------------
# (was R/map_closure.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

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

  # A process with NO inputs (run-of-river, rooftop PV: weather only) is absent
  # from process_inputs, so the join above never sees its outputs. Where it is
  # the only producer in a region, the output commodity was missing there,
  # `.filt_cr()` emptied its output rows, and the plant ran producing nothing.
  free_out <- get_process_outputs(scen, classes = c("technology", "storage",
                                                    "process"))
  has_inp <- names(Filter(length, scen@modInp@sets$process_inputs))
  free_out <- free_out[!names(free_out) %in% has_inp]
  if (length(free_out) > 0) {
    free_comm_region <- named_list_to_df(free_out,
                                         col_names = c("process", "comm")) |>
      left_join(
        named_list_to_df(scen@modInp@sets$process_region,
          col_names = c("process", "region")
        ),
        by = "process", relationship = "many-to-many"
      ) |>
      select(comm, region) |>
      filter(!is.na(comm), !is.na(region)) |>
      unique()
    secondary_comm_region <- rbind(secondary_comm_region, free_comm_region) |>
      unique()
  }

  scen@modInp@sets$secondary_comm_region <-
    split(secondary_comm_region$region, secondary_comm_region$comm)

  comm_region <- secondary_comm_region |>
    rbind(comm_region) |>
    unique()

  ### Stored commodities ####
  # A storage's STORED commodity is produced by the storage itself, out of its
  # own inputs, so it is available wherever the storage operates. Nothing else
  # states this: `secondary_comm_region` reads process_outputs, and a store that
  # holds something other than what it exchanges (ELC in, H2 held, ELC out) has
  # the stored commodity in NEITHER its inputs nor its outputs. Without this the
  # commodity never reaches `comm_region`, `.filt_cr()` empties `mvStorageLevel`,
  # and the storage silently loses its level variable -- it is built, solved and
  # reported, and simply never stores anything.
  #
  # A no-op for a single-commodity storage, whose stored commodity is already its
  # input and output. `storage_stg_comm` defaults from `newStorage(commodity=)`,
  # so legacy objects land in exactly the same place.
  stored_comm_region <- named_list_to_df(
    scen@modInp@sets$storage_stg_comm, col_names = c("process", "comm")
  ) |>
    left_join(
      named_list_to_df(scen@modInp@sets$process_region,
        col_names = c("process", "region")
      ),
      by = "process", relationship = "many-to-many"
    ) |>
    select(comm, region) |>
    filter(!is.na(comm), !is.na(region)) |>
    unique()

  comm_region <- stored_comm_region |>
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
    # where the parent commodity is available -- and where a process burns it:
    # a fuel supplied at the nation and burnt in a sub-region emits there
    burnt_region <- scen@modInp@sets$process_inputs |>
      named_list_to_df(col_names = c("process", "comm")) |>
      left_join(
        named_list_to_df(scen@modInp@sets$process_region,
          col_names = c("process", "region")
        ),
        by = "process", relationship = "many-to-many"
      ) |>
      select(comm, region) |>
      filter(!is.na(comm), !is.na(region))
    emiss_comm <- emiss_comm |>
      left_join(rbind(comm_region, burnt_region) |> unique(), by = c("comm"),
                relationship = "many-to-many") |>
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
  .assert_process_geoframe(scen)

  # Every weather link must resolve to a region the weather actually carries a
  # series at. Checked here for the same reason: it needs process_region.
  .assert_weather_reachable(scen)

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


# ---------------------------------------------------------------------------
# (was R/map_constraint.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =========================================================================== #
# map_constraint.R  —  equation index-domain maps (family "constraint")
#
# One `map_<Name>(scen, fmp) -> scen` per regular constraint map. A regular
# constraint map = a base activity/flow domain intersected with a bound parameter
# (filtered by type lo/up/fx and a value predicate) projected onto its dims — the
# "domain x filtered-source" shape implemented by the shared
# `.build_constraint_join_map` (mapping_engine.R), driven by `.constraint_map_def`.
#
# This migrates the ~39 def-table maps. The bespoke maps (meqStorageStore,
# meqTradeCapFlow), the tech-group/share and ramp maps,
# and the cross-stage maps (built in filter / elsewhere / empty-legacy /
# deprecated) remain on the recipe_constraint fallback. Reuses the engine's
# `.constraint_map_def` + `.build_constraint_join_map` in place (archiving deferred
# to the Phase 4 sweep).
#
# These maps depend only on earlier-recipe domains (mv*, m*Span/New) and
# interpolated bound parameters, so they are mutually independent (no intra-family
# build order needed).
# =========================================================================== #

.cjoin <- function(scen, name, fmp)
  .build_constraint_join_map(scen, name, .constraint_map_def[[name]], fmp)

# A weather bound (`waf.lo` / `waf.fx`, and the storage, supply and `wafc`
# equivalents) is a factor INSIDE its family's bound equation, e.g.
#   af.lo * cap2act * vTechCap * share * prod(waf.lo * weather) <= vTechAct,
# whose domain holds only the rows where the bound itself is set and which is
# multiplied by that bound (`.lo` default 0, `afc.up` default Inf). A process
# with a weather bound and no bound of its own on that side therefore had no
# bound at all, and `.fx` fixed nothing. Here such a process gets the bound = 1
# on its whole domain, so the weather binds; a declared bound is kept (the two
# multiply).
#   wmap  the weather membership of that side (mTechWeatherAfLo, ...)
#   par   the family's bound parameter (pTechAf, ...), `type`/`value` columns
#   dom   the variable domain(s) the rows are taken from (mvTechAct, ...)
#   key   the column(s) naming the bounded entity (tech; tech + comm for afc)
#   side  "lo" or "up"
.weather_bound_default <- function(scen, wmap, par, dom, key, side = "lo") {
  w <- .gds(scen, wmap); pp <- scen@modInp@parameters[[par]]
  d <- lapply(dom, function(x) .gds(scen, x))
  d <- d[!vapply(d, is.null, logical(1))]
  if (is.null(w) || !length(d) || is.null(pp)) return(scen)
  dims <- pp@dimSets
  d <- d[vapply(d, function(x) all(dims %in% colnames(x)), logical(1))]
  if (!length(d) || !all(key %in% colnames(w))) return(scen)
  d <- .udf(do.call(rbind, lapply(d, function(x) x[, dims, drop = FALSE])))
  id <- function(x) do.call(paste, c(lapply(x[key], as.character), sep = "\r"))
  ents <- unique(id(w))
  p <- .gds(scen, par)
  if (!is.null(p) && nrow(p) > 0) {
    ents <- setdiff(ents, unique(id(p[as.character(p$type) == side, , drop = FALSE])))
  }
  if (!length(ents)) return(scen)
  rows <- .udf(d[id(d) %in% ents, dims, drop = FALSE])
  if (!nrow(rows)) return(scen)
  # the parameter may still be folded here: a dimension NA in every row is a
  # wildcard. The new rows take the same wildcard, otherwise the column stops
  # being all-NA and the folded rows of other processes no longer match their
  # domain (their own bounds would drop out of the map). The value 1 is uniform
  # over the domain, so the wildcard is exact.
  if (!is.null(p) && nrow(p) > 0) {
    for (dc in setdiff(dims, key)) if (all(is.na(p[[dc]]))) rows[[dc]] <- NA
    rows <- .udf(rows)
  }
  rows$type <- side
  rows$value <- 1
  # `d2p()` appends and, unlike `.dat2par()`, keeps NA wildcards
  scen@modInp@parameters[[par]] <- d2p(pp, rows, NULL)
  scen
}
.weather_lo_default <- function(scen, wmap, par, dom, key)
  .weather_bound_default(scen, wmap, par, dom, key, "lo")

# A weather factor is read at the timeslices of the bound it multiplies: `wafs`
# at those of `afs`, `wafc` at the commodity's. A weather series with no value
# at one of them would read as 0 there and silently zero the bound, so it is an
# error.
#   meq   the bound's equation map (meqTechAfsLo, ...)
#   wmap  the weather membership of that side
#   key   the columns joining the two (tech; tech + comm for afc)
.weather_timeframe_check <- function(scen, meq, wmap, key, what) {
  m <- .gds(scen, meq); w <- .gds(scen, wmap); wt <- .gds(scen, "pWeather")
  if (is.null(m) || is.null(w) || is.null(wt)) return(invisible())
  x <- dplyr::inner_join(dplyr::distinct(m[, c(key, "timeslice")]),
                         dplyr::distinct(w[, c("weather", key)]), by = key)
  if (!nrow(x) || all(is.na(wt$timeslice))) return(invisible())
  miss <- dplyr::anti_join(x, dplyr::distinct(wt[, c("weather", "timeslice")]),
                           by = c("weather", "timeslice"))
  if (nrow(miss)) {
    miss <- dplyr::distinct(miss[, c(key, "weather")])
    stop(what, ": the weather series has no value at the timeslices of the bound ",
         "it multiplies (", paste(utils::head(do.call(paste, c(miss, sep = "/")), 5),
                                  collapse = ", "), "). ",
         "Give the weather the bound's timeframe.", call. = FALSE)
  }
  invisible()
}

# `wafs` multiplies `afs`: a weather side with no `afs` on that side is an error
.wafs_needs_afs <- function(scen, side) {
  w <- .gds(scen, paste0("mTechWeatherAfs", side))
  if (is.null(w)) return(invisible())
  m <- .gds(scen, paste0("meqTechAfs", side))
  bad <- setdiff(unique(as.character(w$tech)),
                 if (is.null(m)) character(0) else unique(as.character(m$tech)))
  if (length(bad)) {
    stop("wafs.", tolower(side), " is set without afs.", tolower(side), " on ",
         paste(utils::head(bad, 5), collapse = ", "),
         ": wafs multiplies afs and has no effect on its own.", call. = FALSE)
  }
  invisible()
}

# C1 commodity balance
map_meqBalLo <- function(scen, fmp) .cjoin(scen, "meqBalLo", fmp)
map_meqBalUp <- function(scen, fmp) .cjoin(scen, "meqBalUp", fmp)
map_meqBalFx <- function(scen, fmp) .cjoin(scen, "meqBalFx", fmp)
# C2 technology availability factors
map_meqTechAfLo     <- function(scen, fmp) {
  scen <- .weather_lo_default(scen, "mTechWeatherAfLo", "pTechAf", "mvTechAct", "tech")
  if (isTRUE(getOption("energyRt.fx_equ", TRUE))) return(.tech_af_fx_maps(scen, fmp))
  .set_cells_rules(.cjoin(scen, "meqTechAfLo", fmp), "meqTechAfLo")
}
map_meqTechAfUp     <- function(scen, fmp) {
  if (isTRUE(getOption("energyRt.fx_equ", TRUE))) return(scen)
  .set_cells_rules(.cjoin(scen, "meqTechAfUp", fmp), "meqTechAfUp")
}
# built together with meqTechAfLo / meqTechAfUp by .tech_af_fx_maps()
map_meqTechAfFx     <- function(scen, fmp) scen

# `fx_equ`: a cell carried by both meqTechAfLo and meqTechAfUp whose two sides
# are the same number -- af.lo == af.up and the same weather factors -- is the
# pair  x <= v  and  x >= v.  It goes to meqTechAfFx (one equality row) and
# leaves the Lo and Up maps. The three maps are written once each, since
# `d2p()` appends.
#
# The three maps are decided per (tech, region, year) cell of the activity
# domain wherever that is exact (`.tech_af_cells()`): a map holding whole
# cells only is recorded as the activity rule gated by its cells and never
# built tuple by tuple. Elsewhere, and with `energyRt.map_rules_verify`
# (which then checks the cell decision against the hourly build), the maps
# are built per timeslice.
.tech_af_fx_maps <- function(scen, fmp) {
  if (isTRUE(scen@misc$cells_maps) && isTRUE(getOption("energyRt.map_rules", TRUE))) {
    cm <- .tech_af_cells(scen)
    if (!is.null(cm)) {
      if (isTRUE(getOption("energyRt.map_rules_verify", FALSE))) {
        out <- .tech_af_fx_maps_hourly(scen, fmp)
        .tech_af_cells_verify(out, cm)
        return(out)
      }
      return(.tech_af_cells_set(scen, cm, fmp))
    }
  }
  .tech_af_fx_maps_hourly(scen, fmp)
}

# The availability-factor maps per cell of the activity domain.
#   The activity domain is the span's (tech, region, year) cells, less the
#   minus tables of its rule, times the technology's timeslices; the maps are
#   counts per cell of the timeslices where
#     Lo: pTechAf lo is set and not 0;
#     Up: pTechAf up is not Inf (the map is structural);
#     Fx: both, with af.lo == af.up (af.up's default where no row) and the
#         same weather factors;
#   Lo and Up then lose the Fx timeslices. A cell whose count is all of its
#   timeslices or none is whole; any other is partial.
#   NULL (the hourly build decides) when pTechAf has wildcard (NA) rows or
#   repeated keys, or a minus table of the activity rule is not per cell.
# Returns list(cells, ts, base, counts, L, UO, F).
.tech_af_cells <- function(scen) {
  base <- .mvTechAct_rule(scen)
  pp <- scen@modInp@parameters[["pTechAf"]]
  if (is.null(base) || is.null(pp)) return(NULL)
  dt <- function(x) data.table::as.data.table(as.data.frame(x))
  cell <- c("tech", "region", "year")
  key <- c(cell, "timeslice")
  tb <- base$tables
  cells <- unique(dt(tb$span)[, cell, with = FALSE])
  for (nm in base$minus) {
    m <- unique(dt(tb[[nm]]))
    sh <- intersect(names(cells), names(m))
    if (!length(sh) || !all(names(m) %in% cell)) return(NULL)
    cells <- cells[!m, on = sh]
  }
  ts <- unique(dt(tb$proc_frame)[dt(tb$frame_slice), on = "timeframe", nomatch = NULL,
                                 allow.cartesian = TRUE][, c("tech", "timeslice"), with = FALSE])
  nts <- ts[, list(n = .N), by = "tech"]
  cells <- nts[cells, on = "tech", nomatch = NULL]
  P <- get_data_slot(pp)
  if (is.null(P) || !nrow(P)) return(NULL)
  P <- dt(P)
  if (!all(c(key, "type", "value") %in% names(P))) return(NULL)
  for (cl in c(key, "type")) data.table::set(P, j = cl, value = as.character(P[[cl]]))
  if (anyNA(P[, key, with = FALSE])) return(NULL)
  # the rows inside the activity domain
  P <- P[cells[, cell, with = FALSE], on = cell, nomatch = NULL]
  P <- P[ts, on = c("tech", "timeslice"), nomatch = NULL]
  if (anyDuplicated(P, by = c(key, "type"))) return(NULL)
  dv <- pp@defVal
  L <- P[type == "lo" & value != 0, c(key, "value"), with = FALSE]
  data.table::setnames(L, "value", "vlo")
  UO <- P[type == "up" & value == Inf, key, with = FALSE]
  PU <- P[type == "up", c(key, "value"), with = FALSE]
  data.table::setnames(PU, "value", "vup")
  LU <- L[!UO, on = key]
  LU <- PU[LU, on = key]
  LU[is.na(vup), vup := dv[2]]
  same <- is.finite(LU$vlo) & is.finite(LU$vup) & abs(LU$vlo - LU$vup) <= 1e-12 * pmax(1, abs(LU$vlo))
  same <- same & LU$tech %in% .tech_af_weather_same(scen, unique(LU$tech))
  F <- LU[same, key, with = FALSE]
  cnt <- function(x) if (nrow(x)) x[, list(k = .N), by = cell] else data.table::data.table(tech = character(), region = character(), year = character(), k = integer())
  counts <- cells[, c(cell, "n"), with = FALSE]
  add <- function(d, x, nm) {
    d <- cnt(x)[d, on = cell]
    d[is.na(k), k := 0L]
    data.table::setnames(d, "k", nm)
    d
  }
  counts <- add(counts, L, "nL")
  counts <- add(counts, UO, "nUO")
  counts <- add(counts, F, "nF")
  counts[, `:=`(lo = nL - nF, up = n - nUO - nF, fx = nF)]
  list(cells = cells, ts = ts, base = base, counts = counts, L = L[, key, with = FALSE], UO = UO, F = F)
}

# The tuples of one map from the cell decision: its whole cells times their
# timeslices, and the timeslices of its partial cells
.tech_af_cells_tuples <- function(cm, map) {
  cell <- c("tech", "region", "year")
  key <- c(cell, "timeslice")
  cn <- cm$counts
  whole <- cn[get(map) == n & n > 0, cell, with = FALSE]
  part <- cn[get(map) > 0 & get(map) < n, cell, with = FALSE]
  out <- list(cm$ts[whole, on = "tech", allow.cartesian = TRUE][, key, with = FALSE])
  if (nrow(part)) {
    in_part <- function(x) x[part, on = cell, nomatch = NULL][, key, with = FALSE]
    pt <- switch(map,
      lo = in_part(cm$L)[!cm$F, on = key],
      up = cm$ts[part, on = "tech", allow.cartesian = TRUE][, key, with = FALSE][!cm$UO, on = key][!cm$F, on = key],
      fx = in_part(cm$F))
    out[[2L]] <- pt
  }
  unique(data.table::rbindlist(out))
}

# Record the three maps from the cell decision: a map of whole cells only is
# the activity rule gated by its cells, with no table; any other is a table
.tech_af_cells_set <- function(scen, cm, fmp) {
  cell <- c("tech", "region", "year")
  maps <- c(lo = "meqTechAfLo", up = "meqTechAfUp", fx = "meqTechAfFx")
  for (m in names(maps)) {
    cn <- cm$counts
    k <- cn[[m]]
    if (!any(k > 0)) next
    if (any(k > 0 & k < cn$n)) {
      scen <- .set_map(scen, maps[[m]], as.data.frame(.tech_af_cells_tuples(cm, m)), fmp)
      next
    }
    whole <- cn[k == cn$n & cn$n > 0, cell, with = FALSE]
    data.table::setorderv(whole, cell)
    tables <- cm$base$tables
    tables$cells <- as.data.frame(whole)
    rule <- .map_rule(of = cm$base$of, gate = "cells", minus = cm$base$minus, tables = tables,
                      cols = c("tech", "region", "year", "timeslice"))
    scen <- .set_map_rule(scen, maps[[m]], rule, empty_ok = TRUE)
  }
  scen
}

# `energyRt.map_rules_verify`: the cell decision must give the tuples the
# hourly build wrote, map by map
.tech_af_cells_verify <- function(scen, cm) {
  key <- c("tech", "region", "year", "timeslice")
  maps <- c(lo = "meqTechAfLo", up = "meqTechAfUp", fx = "meqTechAfFx")
  for (m in names(maps)) {
    p <- scen@modInp@parameters[[maps[[m]]]]
    a <- if (is.null(p)) NULL else get_data_slot(p)
    a <- if (is.null(a) || !nrow(a)) data.table::data.table(tech = character(), region = character(),
                                                            year = character(), timeslice = character())
         else data.table::as.data.table(lapply(as.data.frame(a)[, key, drop = FALSE], as.character))
    b <- .tech_af_cells_tuples(cm, m)
    same <- nrow(a) == nrow(b) && nrow(data.table::fsetdiff(a, b)) == 0L && nrow(data.table::fsetdiff(b, a)) == 0L
    if (!same) {
      stop("availability-factor map '", maps[[m]], "': the cell decision gives ", nrow(b),
           " tuples where the hourly build gives ", nrow(a), ".", call. = FALSE)
    }
  }
  invisible(TRUE)
}

utils::globalVariables(c("vup", "vlo", "nL", "nUO", "nF", "k", "n", "type", "lo", "up", "fx", "tech"))

.tech_af_fx_maps_hourly <- function(scen, fmp) {
  mem <- function(x) NULL
  lo <- .gds(.cjoin(scen, "meqTechAfLo", mem), "meqTechAfLo")
  up <- .gds(.cjoin(scen, "meqTechAfUp", mem), "meqTechAfUp")
  key <- c("tech", "region", "year", "timeslice")
  fx <- NULL
  if (!is.null(lo) && !is.null(up)) {
    both <- dplyr::inner_join(dplyr::distinct(lo[, key]),
                              dplyr::distinct(up[, key]), by = key)
    if (nrow(both) > 0) {
      dv <- scen@modInp@parameters[["pTechAf"]]@defVal
      src <- .gds(scen, "pTechAf")
      vlo <- .bound_at(src, both, "lo", dv[1])
      vup <- .bound_at(src, both, "up", dv[2])
      same <- is.finite(vlo) & is.finite(vup) &
        abs(vlo - vup) <= 1e-12 * pmax(1, abs(vlo))
      same <- same & both$tech %in% .tech_af_weather_same(scen, unique(both$tech))
      fx <- both[same, , drop = FALSE]
    }
  }
  if (!is.null(fx) && nrow(fx) > 0) {
    lo <- dplyr::anti_join(lo, fx, by = key)
    up <- dplyr::anti_join(up, fx, by = key)
  }
  scen <- .set_map(scen, "meqTechAfLo", lo, fmp)
  scen <- .set_map(scen, "meqTechAfUp", up, fmp)
  scen <- .set_map(scen, "meqTechAfFx", fx, fmp)
  # each map is the activity domain at its cells when the bound is one number
  # per (tech, region, year); the rule then replaces the table (R/map-rules.R)
  .set_cells_rules(scen, c("meqTechAfLo", "meqTechAfUp", "meqTechAfFx"))
}

# value of a `bounds` parameter (`type`/`value` columns) of side `type` at each
# row of `cells`. The parameter may still be folded: an NA in a key column of a
# source row is a wildcard for that row (all members), whether the whole column
# is NA or only some rows. The most specific matching row wins (fewest
# wildcards), as when the writers expand them; a cell no row matches reads as
# the default `def`.
.bound_at <- function(src, cells, type, def) {
  n <- nrow(cells)
  v <- rep(NA_real_, n)
  if (!is.null(src) && nrow(src) > 0) {
    s <- src[as.character(src$type) == type, , drop = FALSE]
    by <- intersect(colnames(cells), setdiff(colnames(s), c("type", "value")))
    if (nrow(s) > 0 && length(by) == 0) v[] <- s$value[1]
    if (nrow(s) > 0 && length(by) > 0) {
      # per-row wildcard pattern ("0" explicit, "1" NA), built column-wise
      wild <- do.call(cbind, lapply(by, function(b) is.na(s[[b]])))
      pat <- do.call(paste0, lapply(seq_along(by), function(j) as.integer(wild[, j])))
      # patterns in order of specificity: fewest wildcard columns first
      ord <- unique(pat[order(rowSums(wild))])
      for (p in ord) {
        if (!anyNA(v) ) break
        k <- by[strsplit(p, "")[[1]] == "0"]
        sp <- s[pat == p, , drop = FALSE]
        todo <- which(is.na(v))
        if (length(k) == 0) { v[todo] <- sp$value[1]; next }
        # first matching row, as match() on pasted keys would give
        spd <- data.table::as.data.table(lapply(sp[k], as.character))
        data.table::set(spd, j = "value", value = sp$value)
        cd <- data.table::as.data.table(lapply(cells[todo, k, drop = FALSE], as.character))
        v[todo] <- spd[cd, on = k, mult = "first"]$value
      }
    }
  }
  v[is.na(v)] <- def
  v
}

# technologies whose af.lo weather product equals the af.up one: the same
# weather members (mTechWeatherAfLo vs mTechWeatherAfUp) with the same
# coefficients (pTechWeatherAf lo vs up)
.tech_af_weather_same <- function(scen, techs) {
  wl <- .gds(scen, "mTechWeatherAfLo"); wu <- .gds(scen, "mTechWeatherAfUp")
  pw <- .gds(scen, "pTechWeatherAf")
  dv <- scen@modInp@parameters[["pTechWeatherAf"]]@defVal
  sig <- function(w, type, def) {
    if (is.null(w) || nrow(w) == 0) return(setNames(character(0), character(0)))
    w <- dplyr::distinct(w[, c("weather", "tech")])
    v <- .bound_at(pw, w, type, def)
    w$s <- paste0(w$weather, "=", sprintf("%.15g", v))
    vapply(split(w$s, as.character(w$tech)),
           function(x) paste(sort(x), collapse = "|"), character(1))
  }
  sl <- sig(wl, "lo", dv[1]); su <- sig(wu, "up", dv[2])
  a <- unname(sl[techs]); b <- unname(su[techs])
  a[is.na(a)] <- ""; b[is.na(b)] <- ""
  techs[a == b]
}
map_meqTechAfsLo    <- function(scen, fmp) {
  scen <- .cjoin(scen, "meqTechAfsLo", fmp)
  .wafs_needs_afs(scen, "Lo")
  .weather_timeframe_check(scen, "meqTechAfsLo", "mTechWeatherAfsLo", "tech", "wafs.lo")
  scen
}
map_meqTechAfsUp    <- function(scen, fmp) {
  scen <- .cjoin(scen, "meqTechAfsUp", fmp)
  .wafs_needs_afs(scen, "Up")
  .weather_timeframe_check(scen, "meqTechAfsUp", "mTechWeatherAfsUp", "tech", "wafs.up")
  scen
}
# `wafc` acts like `waf` at the commodity's timeframe: with no `afc` of its own
# on a side, the commodity gets `afc = 1` there (both defaults are needed, since
# `afc.up` defaults to Inf); the four maps then read the completed parameter
map_meqTechAfcInpLo <- function(scen, fmp) {
  for (sd in c("lo", "up")) {
    scen <- .weather_bound_default(scen, paste0("mTechWeatherAfc", .tc(sd)), "pTechAfc",
                                   c("mvTechInp", "mvTechOut"), c("tech", "comm"), sd)
  }
  .cjoin(scen, "meqTechAfcInpLo", fmp)
}
map_meqTechAfcInpUp <- function(scen, fmp) .cjoin(scen, "meqTechAfcInpUp", fmp)
map_meqTechAfcOutLo <- function(scen, fmp) .cjoin(scen, "meqTechAfcOutLo", fmp)
map_meqTechAfcOutUp <- function(scen, fmp) {
  scen <- .cjoin(scen, "meqTechAfcOutUp", fmp)
  for (io in c("Inp", "Out")) for (sd in c("Lo", "Up")) {
    .weather_timeframe_check(scen, paste0("meqTechAfc", io, sd), paste0("mTechWeatherAfc", sd),
                             c("tech", "comm"), paste0("wafc.", tolower(sd)))
  }
  scen
}
.tc <- function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
# C4 technology capacity / retirement
map_mTechCapLo    <- function(scen, fmp) .cjoin(scen, "mTechCapLo", fmp)
map_mTechCapUp    <- function(scen, fmp) .cjoin(scen, "mTechCapUp", fmp)
map_mTechNewCapLo <- function(scen, fmp) .cjoin(scen, "mTechNewCapLo", fmp)
map_mTechNewCapUp <- function(scen, fmp) .cjoin(scen, "mTechNewCapUp", fmp)
map_mTechRetLo    <- function(scen, fmp) .cjoin(scen, "mTechRetLo", fmp)
map_mTechRetUp    <- function(scen, fmp) .cjoin(scen, "mTechRetUp", fmp)
# C5 storage activity bounds
map_meqStorageAfLo  <- function(scen, fmp) {
  scen <- .weather_lo_default(scen, "mStorageWeatherAfLo", "pStorageAf", "mvStorageLevel", "stg")
  .cjoin(scen, "meqStorageAfLo", fmp)
}
map_meqStorageAfUp  <- function(scen, fmp) .cjoin(scen, "meqStorageAfUp", fmp)
map_meqStorageInpLo <- function(scen, fmp) {
  scen <- .weather_lo_default(scen, "mStorageWeatherInpAfLo", "pStorageInpAf", "mvStorageInp", "stg")
  .cjoin(scen, "meqStorageInpLo", fmp)
}
map_meqStorageInpUp <- function(scen, fmp) .cjoin(scen, "meqStorageInpUp", fmp)
map_meqStorageOutLo <- function(scen, fmp) {
  scen <- .weather_lo_default(scen, "mStorageWeatherOutAfLo", "pStorageOutAf", "mvStorageOut", "stg")
  .cjoin(scen, "meqStorageOutLo", fmp)
}
map_meqStorageOutUp <- function(scen, fmp) .cjoin(scen, "meqStorageOutUp", fmp)
# C6 storage capacity / retirement
map_mStorageCapLo    <- function(scen, fmp) .cjoin(scen, "mStorageOutCapLo", fmp)
map_mStorageCapUp    <- function(scen, fmp) .cjoin(scen, "mStorageOutCapUp", fmp)
map_mStorageNewCapLo <- function(scen, fmp) .cjoin(scen, "mStorageOutNewCapLo", fmp)
map_mStorageNewCapUp <- function(scen, fmp) .cjoin(scen, "mStorageOutNewCapUp", fmp)
map_mStorageInpCapLo <- function(scen, fmp) .cjoin(scen, "mStorageInpCapLo", fmp)
map_mStorageInpCapUp <- function(scen, fmp) .cjoin(scen, "mStorageInpCapUp", fmp)
map_mStorageInpNewCapLo <- function(scen, fmp) .cjoin(scen, "mStorageInpNewCapLo", fmp)
map_mStorageInpNewCapUp <- function(scen, fmp) .cjoin(scen, "mStorageInpNewCapUp", fmp)
map_mStorageInp2outLo <- function(scen, fmp) .cjoin(scen, "mStorageInp2outLo", fmp)
map_mStorageInp2outUp <- function(scen, fmp) .cjoin(scen, "mStorageInp2outUp", fmp)
map_mStorageInp2stgLo <- function(scen, fmp) .cjoin(scen, "mStorageInp2stgLo", fmp)
map_mStorageInp2stgUp <- function(scen, fmp) .cjoin(scen, "mStorageInp2stgUp", fmp)
map_mStorageStgCapLo    <- function(scen, fmp) .cjoin(scen, "mStorageStgCapLo", fmp)
map_mStorageStgCapUp    <- function(scen, fmp) .cjoin(scen, "mStorageStgCapUp", fmp)
map_mStorageStgNewCapLo <- function(scen, fmp) .cjoin(scen, "mStorageStgNewCapLo", fmp)
map_mStorageStgNewCapUp <- function(scen, fmp) .cjoin(scen, "mStorageStgNewCapUp", fmp)
map_mStorageDurationLo  <- function(scen, fmp) .cjoin(scen, "mStorageDurationLo", fmp)
map_mStorageDurationUp  <- function(scen, fmp) .cjoin(scen, "mStorageDurationUp", fmp)
map_mStorageRetLo    <- function(scen, fmp) .cjoin(scen, "mStorageOutRetLo", fmp)
map_mStorageRetUp    <- function(scen, fmp) .cjoin(scen, "mStorageOutRetUp", fmp)
map_mStorageInpRetLo <- function(scen, fmp) .cjoin(scen, "mStorageInpRetLo", fmp)
map_mStorageInpRetUp <- function(scen, fmp) .cjoin(scen, "mStorageInpRetUp", fmp)
map_mStorageStgRetLo <- function(scen, fmp) .cjoin(scen, "mStorageStgRetLo", fmp)
map_mStorageStgRetUp <- function(scen, fmp) .cjoin(scen, "mStorageStgRetUp", fmp)
# C7 trade capacity / retirement
map_mTradeCapLo    <- function(scen, fmp) .cjoin(scen, "mTradeCapLo", fmp)
map_mTradeCapUp    <- function(scen, fmp) .cjoin(scen, "mTradeCapUp", fmp)
map_mTradeNewCapLo <- function(scen, fmp) .cjoin(scen, "mTradeNewCapLo", fmp)
map_mTradeNewCapUp <- function(scen, fmp) .cjoin(scen, "mTradeNewCapUp", fmp)
map_mTradeRetLo    <- function(scen, fmp) .cjoin(scen, "mTradeRetLo", fmp)
map_mTradeRetUp    <- function(scen, fmp) .cjoin(scen, "mTradeRetUp", fmp)
# C7 trade inter-regional flow bounds
map_meqTradeFlowLo <- function(scen, fmp) .cjoin(scen, "meqTradeFlowLo", fmp)
map_meqTradeFlowUp <- function(scen, fmp) .cjoin(scen, "meqTradeFlowUp", fmp)
# a route with a weather factor and no `af` of its own takes the bound 1, so
# the weather binds
.trade_route <- c("trade", "src", "dst")
map_meqTradeIrAfLo <- function(scen, fmp) {
  scen <- .weather_bound_default(scen, "mTradeWeatherAfLo", "pTradeIrAf",
                                 "mvTradeIr", .trade_route, "lo")
  .cjoin(scen, "meqTradeIrAfLo", fmp)
}
map_meqTradeIrAfUp <- function(scen, fmp) {
  scen <- .weather_bound_default(scen, "mTradeWeatherAfUp", "pTradeIrAf",
                                 "mvTradeIr", .trade_route, "up")
  .cjoin(scen, "meqTradeIrAfUp", fmp)
}
# A route's flow over a parent timeslice: the routes that carry flow, at the
# bound's own timeslices.
.trade_afs_map <- function(scen, name, type, fmp) {
  src <- .gds(scen, "pTradeIrAfs")
  dom <- .gds(scen, "mvTradeIr")
  if (is.null(dom) || !nrow(dom)) return(scen)
  src <- if (is.null(src)) data.frame(trade = character(), src = character(),
                                      dst = character(), type = character(),
                                      value = numeric()) else as.data.frame(src)
  src <- src[src$type == type, , drop = FALSE]
  src <- src[if (type == "lo") src$value != 0 else src$value != Inf, , drop = FALSE]
  # `wafs` multiplies `afs`: a route weathered on a side with no `afs` there
  w <- .gds(scen, paste0("mTradeWeatherAfs", if (type == "lo") "Lo" else "Up"))
  if (!is.null(w) && nrow(w)) {
    id <- function(x) do.call(paste, c(lapply(x[.trade_route], as.character), sep = "/"))
    miss <- setdiff(id(w), id(src))
    if (length(miss)) {
      stop("wafs.", type, " on route(s) ", paste(utils::head(miss, 5), collapse = ", "),
           " without afs.", type, ": the weather factor multiplies the afs bound. ",
           "Give the route an afs.", type, ".", call. = FALSE)
    }
  }
  if (!nrow(src)) return(scen)
  keys <- c("trade", "src", "dst", "year", "timeslice")
  src <- dplyr::distinct(src[, keys, drop = FALSE])
  dom <- dplyr::distinct(as.data.frame(dom)[, c("trade", "src", "dst", "year"),
                                            drop = FALSE])
  .set_map(scen, name, .wild_join(dom, src), fmp)
}
map_meqTradeIrAfsLo <- function(scen, fmp) .trade_afs_map(scen, "meqTradeIrAfsLo", "lo", fmp)
map_meqTradeIrAfsUp <- function(scen, fmp) .trade_afs_map(scen, "meqTradeIrAfsUp", "up", fmp)
# C8 supply reserve margins
map_meqSupReserveLo <- function(scen, fmp) .cjoin(scen, "meqSupReserveLo", fmp)
map_mSupReserveUp   <- function(scen, fmp) .cjoin(scen, "mSupReserveUp", fmp)

# -- bespoke builders (each already a per-mapping function) ---------------- #
map_meqStorageStore       <- function(scen, fmp) .build_meqStorageStore(scen, fmp)
map_meqTradeCapFlow       <- function(scen, fmp) .build_meqTradeCapFlow(scen, fmp)

# -- technology group / share maps ----------------------------------------- #
# Built together from shared intermediates by .build_tech_group_maps; the wrapper
# passes the single requested name (the builder gates output by `names`).
.tgroup <- function(scen, name, fmp) .build_tech_group_maps(scen, name, fmp)
map_meqTechActSng     <- function(scen, fmp) .tgroup(scen, "meqTechActSng", fmp)
map_meqTechActGrp     <- function(scen, fmp) .tgroup(scen, "meqTechActGrp", fmp)
map_meqTechGrp2Sng    <- function(scen, fmp) .tgroup(scen, "meqTechGrp2Sng", fmp)
map_meqTechSng2Grp    <- function(scen, fmp) .tgroup(scen, "meqTechSng2Grp", fmp)
map_meqTechSng2Sng    <- function(scen, fmp) .tgroup(scen, "meqTechSng2Sng", fmp)
map_meqTechGrp2Grp    <- function(scen, fmp) .tgroup(scen, "meqTechGrp2Grp", fmp)
map_meqTechShareInpLo <- function(scen, fmp) .tgroup(scen, "meqTechShareInpLo", fmp)
map_meqTechShareInpUp <- function(scen, fmp) .tgroup(scen, "meqTechShareInpUp", fmp)
map_meqTechShareOutLo <- function(scen, fmp) .tgroup(scen, "meqTechShareOutLo", fmp)
map_meqTechShareOutUp <- function(scen, fmp) .tgroup(scen, "meqTechShareOutUp", fmp)

# -- ramping maps ---------------------------------------------------------- #
map_mTechRampUp   <- function(scen, fmp) .build_ramp_maps(scen, "mTechRampUp", fmp)
map_mTechRampDown <- function(scen, fmp) .build_ramp_maps(scen, "mTechRampDown", fmp)

# -- intentionally-empty maps ---------------------------------------------- #
# Declared as solver index sets but never populated by the legacy pipeline.
# NB these are NOT dead: `af.up` still binds, through `meqTechAfUp` + `pTechAf`.
# They are redundant domain maps, so they stay empty (faithful to legacy).
map_mTechAfUp      <- function(scen, fmp) scen
map_mTechAfcUp     <- function(scen, fmp) scen

# -- registry for the constraint family (def-table maps) ------------------- #
.constraint_builders <- list(
  meqBalLo = map_meqBalLo, meqBalUp = map_meqBalUp, meqBalFx = map_meqBalFx,
  meqTechAfLo = map_meqTechAfLo, meqTechAfUp = map_meqTechAfUp,
  meqTechAfFx = map_meqTechAfFx,
  meqTechAfsLo = map_meqTechAfsLo, meqTechAfsUp = map_meqTechAfsUp,
  meqTechAfcInpLo = map_meqTechAfcInpLo, meqTechAfcInpUp = map_meqTechAfcInpUp,
  meqTechAfcOutLo = map_meqTechAfcOutLo, meqTechAfcOutUp = map_meqTechAfcOutUp,
  mTechCapLo = map_mTechCapLo, mTechCapUp = map_mTechCapUp,
  mTechNewCapLo = map_mTechNewCapLo, mTechNewCapUp = map_mTechNewCapUp,
  mTechRetLo = map_mTechRetLo, mTechRetUp = map_mTechRetUp,
  meqStorageAfLo = map_meqStorageAfLo, meqStorageAfUp = map_meqStorageAfUp,
  meqStorageInpLo = map_meqStorageInpLo, meqStorageInpUp = map_meqStorageInpUp,
  meqStorageOutLo = map_meqStorageOutLo, meqStorageOutUp = map_meqStorageOutUp,
  mStorageOutCapLo = map_mStorageCapLo, mStorageOutCapUp = map_mStorageCapUp,
  mStorageOutNewCapLo = map_mStorageNewCapLo, mStorageOutNewCapUp = map_mStorageNewCapUp,
  mStorageInpCapLo = map_mStorageInpCapLo, mStorageInpCapUp = map_mStorageInpCapUp,
  mStorageInpNewCapLo = map_mStorageInpNewCapLo,
  mStorageInpNewCapUp = map_mStorageInpNewCapUp,
  mStorageInp2outLo = map_mStorageInp2outLo,
  mStorageInp2outUp = map_mStorageInp2outUp,
  mStorageInp2stgLo = map_mStorageInp2stgLo,
  mStorageInp2stgUp = map_mStorageInp2stgUp,
  mStorageStgCapLo = map_mStorageStgCapLo, mStorageStgCapUp = map_mStorageStgCapUp,
  mStorageStgNewCapLo = map_mStorageStgNewCapLo,
  mStorageStgNewCapUp = map_mStorageStgNewCapUp,
  mStorageDurationLo = map_mStorageDurationLo,
  mStorageDurationUp = map_mStorageDurationUp,
  mStorageOutRetLo = map_mStorageRetLo, mStorageOutRetUp = map_mStorageRetUp,
  mStorageInpRetLo = map_mStorageInpRetLo, mStorageInpRetUp = map_mStorageInpRetUp,
  mStorageStgRetLo = map_mStorageStgRetLo, mStorageStgRetUp = map_mStorageStgRetUp,
  mTradeCapLo = map_mTradeCapLo, mTradeCapUp = map_mTradeCapUp,
  mTradeNewCapLo = map_mTradeNewCapLo, mTradeNewCapUp = map_mTradeNewCapUp,
  mTradeRetLo = map_mTradeRetLo, mTradeRetUp = map_mTradeRetUp,
  meqTradeFlowLo = map_meqTradeFlowLo, meqTradeFlowUp = map_meqTradeFlowUp,
  meqTradeIrAfLo = map_meqTradeIrAfLo, meqTradeIrAfUp = map_meqTradeIrAfUp,
  meqTradeIrAfsLo = map_meqTradeIrAfsLo, meqTradeIrAfsUp = map_meqTradeIrAfsUp,
  meqSupReserveLo = map_meqSupReserveLo, mSupReserveUp = map_mSupReserveUp,
  # bespoke
  meqStorageStore = map_meqStorageStore, meqTradeCapFlow = map_meqTradeCapFlow,
  # tech-group / share
  meqTechActSng = map_meqTechActSng, meqTechActGrp = map_meqTechActGrp,
  meqTechGrp2Sng = map_meqTechGrp2Sng, meqTechSng2Grp = map_meqTechSng2Grp,
  meqTechSng2Sng = map_meqTechSng2Sng, meqTechGrp2Grp = map_meqTechGrp2Grp,
  meqTechShareInpLo = map_meqTechShareInpLo, meqTechShareInpUp = map_meqTechShareInpUp,
  meqTechShareOutLo = map_meqTechShareOutLo, meqTechShareOutUp = map_meqTechShareOutUp,
  # ramp
  mTechRampUp = map_mTechRampUp, mTechRampDown = map_mTechRampDown,
  # intentionally empty (empty-legacy)
  mTechAfUp = map_mTechAfUp, mTechAfcUp = map_mTechAfcUp
)

# -- fixed bounds as one equality row (`fx_equ`) ---------------------------- #
# A cell carried by both the Lo and the Up map of a family, whose two sides are
# the same number, is the pair  x >= v  and  x <= v. With `fx_equ` (default) it
# goes to the family's Fx map, written as one equality row (eq<Fam>Fx, the Lo
# equation with `=`), and leaves the Lo and Up maps. The two sides are equal
# when every factor that differs between eq<Fam>Lo and eq<Fam>Up does:
#   par    the bound parameter (`type` lo / up)
#   extra  other bounds parameters the two equations read on their own side
#          (storage duration, inp2out)
#   w*     the weather product: same members and coefficients on both sides
# TechAf is built with its Lo/Up maps (.tech_af_fx_maps); the rest here, after
# every Lo/Up map exists. An on-disk map is not rewritten (the pair is kept).
.fx_families <- list(
  TechAfs = list(lo = "meqTechAfsLo", up = "meqTechAfsUp", fx = "meqTechAfsFx", par = "pTechAfs",
                 wlo = "mTechWeatherAfsLo", wup = "mTechWeatherAfsUp", wcoef = "pTechWeatherAfs", went = "tech"),
  TechAfcInp = list(lo = "meqTechAfcInpLo", up = "meqTechAfcInpUp", fx = "meqTechAfcInpFx", par = "pTechAfc",
                    wlo = "mTechWeatherAfcLo", wup = "mTechWeatherAfcUp", wcoef = "pTechWeatherAfc",
                    went = c("tech", "comm")),
  TechAfcOut = list(lo = "meqTechAfcOutLo", up = "meqTechAfcOutUp", fx = "meqTechAfcOutFx", par = "pTechAfc",
                    wlo = "mTechWeatherAfcLo", wup = "mTechWeatherAfcUp", wcoef = "pTechWeatherAfc",
                    went = c("tech", "comm")),
  TechShareInp = list(lo = "meqTechShareInpLo", up = "meqTechShareInpUp", fx = "meqTechShareInpFx",
                      par = "pTechShare"),
  TechShareOut = list(lo = "meqTechShareOutLo", up = "meqTechShareOutUp", fx = "meqTechShareOutFx",
                      par = "pTechShare"),
  TechCap = list(lo = "mTechCapLo", up = "mTechCapUp", fx = "mTechCapFx", par = "pTechCap"),
  TechNewCap = list(lo = "mTechNewCapLo", up = "mTechNewCapUp", fx = "mTechNewCapFx", par = "pTechNewCap"),
  TechRet = list(lo = "mTechRetLo", up = "mTechRetUp", fx = "mTechRetFx", par = "pTechRet"),
  StorageAf = list(lo = "meqStorageAfLo", up = "meqStorageAfUp", fx = "meqStorageAfFx", par = "pStorageAf",
                   extra = "pStorageDuration",
                   wlo = "mStorageWeatherAfLo", wup = "mStorageWeatherAfUp", wcoef = "pStorageWeatherAf",
                   went = "stg"),
  StorageInp = list(lo = "meqStorageInpLo", up = "meqStorageInpUp", fx = "meqStorageInpFx", par = "pStorageInpAf",
                    extra = "pStorageInp2out",
                    wlo = "mStorageWeatherInpAfLo", wup = "mStorageWeatherInpAfUp",
                    wcoef = "pStorageWeatherInpAf", went = "stg"),
  StorageOut = list(lo = "meqStorageOutLo", up = "meqStorageOutUp", fx = "meqStorageOutFx", par = "pStorageOutAf",
                    wlo = "mStorageWeatherOutAfLo", wup = "mStorageWeatherOutAfUp",
                    wcoef = "pStorageWeatherOutAf", went = "stg"),
  StorageDuration = list(lo = "mStorageDurationLo", up = "mStorageDurationUp", fx = "mStorageDurationFx",
                         par = "pStorageDuration"),
  StorageInp2out = list(lo = "mStorageInp2outLo", up = "mStorageInp2outUp", fx = "mStorageInp2outFx",
                        par = "pStorageInp2out"),
  StorageInp2stg = list(lo = "mStorageInp2stgLo", up = "mStorageInp2stgUp", fx = "mStorageInp2stgFx",
                        par = "pStorageInp2stg"),
  StorageInpCap = list(lo = "mStorageInpCapLo", up = "mStorageInpCapUp", fx = "mStorageInpCapFx",
                       par = "pStorageInpCap"),
  StorageInpNewCap = list(lo = "mStorageInpNewCapLo", up = "mStorageInpNewCapUp", fx = "mStorageInpNewCapFx",
                          par = "pStorageInpNewCap"),
  StorageInpRet = list(lo = "mStorageInpRetLo", up = "mStorageInpRetUp", fx = "mStorageInpRetFx",
                       par = "pStorageInpRet"),
  StorageOutCap = list(lo = "mStorageOutCapLo", up = "mStorageOutCapUp", fx = "mStorageOutCapFx",
                       par = "pStorageOutCap"),
  StorageOutNewCap = list(lo = "mStorageOutNewCapLo", up = "mStorageOutNewCapUp", fx = "mStorageOutNewCapFx",
                          par = "pStorageOutNewCap"),
  StorageOutRet = list(lo = "mStorageOutRetLo", up = "mStorageOutRetUp", fx = "mStorageOutRetFx",
                       par = "pStorageOutRet"),
  StorageStgCap = list(lo = "mStorageStgCapLo", up = "mStorageStgCapUp", fx = "mStorageStgCapFx",
                       par = "pStorageStgCap"),
  StorageStgNewCap = list(lo = "mStorageStgNewCapLo", up = "mStorageStgNewCapUp", fx = "mStorageStgNewCapFx",
                          par = "pStorageStgNewCap"),
  StorageStgRet = list(lo = "mStorageStgRetLo", up = "mStorageStgRetUp", fx = "mStorageStgRetFx",
                       par = "pStorageStgRet"),
  SupAva = list(lo = "meqSupAvaLo", up = "mSupAvaUp", fx = "meqSupAvaFx", par = "pSupAva",
                wlo = "mSupWeatherLo", wup = "mSupWeatherUp", wcoef = "pSupWeather", went = "sup"),
  SupReserve = list(lo = "meqSupReserveLo", up = "mSupReserveUp", fx = "meqSupReserveFx", par = "pSupReserve"),
  TradeFlow = list(lo = "meqTradeFlowLo", up = "meqTradeFlowUp", fx = "meqTradeFlowFx", par = "pTradeIr"),
  TradeIrAf = list(lo = "meqTradeIrAfLo", up = "meqTradeIrAfUp", fx = "meqTradeIrAfFx", par = "pTradeIrAf",
                   wlo = "mTradeWeatherAfLo", wup = "mTradeWeatherAfUp", wcoef = "pTradeWeatherAf",
                   went = c("trade", "src", "dst")),
  TradeIrAfs = list(lo = "meqTradeIrAfsLo", up = "meqTradeIrAfsUp", fx = "meqTradeIrAfsFx", par = "pTradeIrAfs",
                    wlo = "mTradeWeatherAfsLo", wup = "mTradeWeatherAfsUp", wcoef = "pTradeWeatherAfs",
                    went = c("trade", "src", "dst")),
  TradeCap = list(lo = "mTradeCapLo", up = "mTradeCapUp", fx = "mTradeCapFx", par = "pTradeCap"),
  TradeNewCap = list(lo = "mTradeNewCapLo", up = "mTradeNewCapUp", fx = "mTradeNewCapFx", par = "pTradeNewCap"),
  TradeRet = list(lo = "mTradeRetLo", up = "mTradeRetUp", fx = "mTradeRetFx", par = "pTradeRet"),
  ImportRow = list(lo = "meqImportRowLo", up = "mImportRowUp", fx = "meqImportRowFx", par = "pImportRow"),
  ExportRow = list(lo = "meqExportRowLo", up = "mExportRowUp", fx = "meqExportRowFx", par = "pExportRow")
)

# lo == up of a bounds parameter at each row of `cells`
.bound_same <- function(scen, par, cells) {
  pp <- scen@modInp@parameters[[par]]
  if (is.null(pp)) return(rep(FALSE, nrow(cells)))
  src <- .gds(scen, par); dv <- pp@defVal
  vlo <- .bound_at(src, cells, "lo", dv[1]); vup <- .bound_at(src, cells, "up", dv[2])
  is.finite(vlo) & is.finite(vup) & abs(vlo - vup) <= 1e-12 * pmax(1, abs(vlo))
}

# per row of `cells`: does the Lo weather product equal the Up one (same
# members, same coefficients) for the row's entity (`ent` columns)?
.weather_same_ent <- function(scen, wlo, wup, wcoef, ent, cells) {
  pw <- .gds(scen, wcoef)
  pp <- scen@modInp@parameters[[wcoef]]
  dv <- if (is.null(pp)) c(1, 1) else pp@defVal
  id <- function(x) do.call(paste, c(lapply(x[ent], as.character), sep = "\r"))
  sig <- function(w, type, def) {
    if (is.null(w) || nrow(w) == 0) return(setNames(character(0), character(0)))
    w <- dplyr::distinct(w[, c("weather", ent), drop = FALSE])
    v <- .bound_at(pw, w, type, def)
    w$s <- paste0(w$weather, "=", sprintf("%.15g", v))
    vapply(split(w$s, id(w)), function(x) paste(sort(x), collapse = "|"), character(1))
  }
  sl <- sig(.gds(scen, wlo), "lo", dv[1]); su <- sig(.gds(scen, wup), "up", dv[2])
  k <- id(cells)
  a <- unname(sl[k]); b <- unname(su[k])
  a[is.na(a)] <- ""; b[is.na(b)] <- ""
  a == b
}

# overwrite a map that is already built (`d2p()` appends)
.map_replace <- function(scen, name, df, fmp) {
  p <- .resetParameter(scen@modInp@parameters[[name]])
  if (isOnDisk(p)) {
    # d2p() appends to what is stored: drop the stored copy and rebuild the
    # map in memory; saving the scenario writes it back
    unlink(fp(fmp(name), "data"), recursive = TRUE)
    p <- mark_inMemory(p)
    fmp <- function(x) NULL
  }
  scen@modInp@parameters[[name]] <- p
  .set_map(scen, name, df, fmp)
}

.fx_split <- function(scen, f, fmp) {
  if (!isTRUE(getOption("energyRt.fx_equ", TRUE))) return(scen)
  pl <- scen@modInp@parameters[[f$lo]]; pu <- scen@modInp@parameters[[f$up]]
  if (is.null(pl) || is.null(pu) || is.null(scen@modInp@parameters[[f$fx]])) return(scen)
  lo <- .gds(scen, f$lo); up <- .gds(scen, f$up)
  if (is.null(lo) || is.null(up)) return(scen)
  key <- intersect(colnames(lo), colnames(up))
  both <- dplyr::inner_join(dplyr::distinct(lo[, key, drop = FALSE]),
                            dplyr::distinct(up[, key, drop = FALSE]), by = key)
  if (nrow(both) == 0) return(scen)
  same <- .bound_same(scen, f$par, both)
  for (x in f$extra) same <- same & .bound_same(scen, x, both)
  if (!is.null(f$wlo)) {
    same <- same & .weather_same_ent(scen, f$wlo, f$wup, f$wcoef, f$went, both)
  }
  fx <- both[same, , drop = FALSE]
  if (nrow(fx) == 0) return(scen)
  scen <- .map_replace(scen, f$lo, dplyr::anti_join(lo, fx, by = key), fmp)
  scen <- .map_replace(scen, f$up, dplyr::anti_join(up, fx, by = key), fmp)
  .set_map(scen, f$fx, fx, fmp)
}

.fx_builders <- lapply(.fx_families, function(f) {
  force(f)
  function(scen, fmp) .fx_split(scen, f, fmp)
})
names(.fx_builders) <- vapply(.fx_families, `[[`, character(1), "fx")
.constraint_builders <- c(.constraint_builders, .fx_builders)


# ---------------------------------------------------------------------------
# (was R/map_costagg.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =========================================================================== #
# map_costagg.R  —  top-level cost-aggregation mapping builders (family "cost_agg")
#
# One `map_<Name>(scen, fmp) -> scen` per cost-aggregation map. Registered in
# `.cost_agg_builders`. Reuses `.set_map` / get_data_slot from mapping_engine.R.
# =========================================================================== #

# Every map whose variable `eqCost` sums into `vTotalCost`. Read off the GLPK
# equation, so a new cost stream has to be added here too -- which is the point:
# the coarse-cell rule below must see every place a cost can land.
.COST_DOMAIN_MAPS <- c(
  "mDummyExportCost", "mDummyImportCost", "mExportIrCost", "mExportRowCost",
  "mImportIrCost", "mImportRowCost", "mStorageEac", "mStorageFixom",
  "mStorageRetCost", "mStorageVarom", "mSubCost", "mTaxCost", "mTechEac",
  "mTechFixom", "mTechRetCost", "mTechVarom", "mTradeEac", "mTradeFixom",
  "mTradeRetCost", "mvSupCost"
)

# Coarse (non-atomic) regions that a cost actually lands in, with the years they
# land in. Empty in a flat model, and empty in a model whose geoscale nobody uses
# for a cost -- so both stay bit-for-bit unchanged.
.coarse_cost_cells <- function(scen) {
  atomic <- .model_regions(scen)
  known  <- .known_regions(scen)
  coarse <- setdiff(known, atomic)
  empty  <- data.frame(region = character(), year = integer())
  if (length(coarse) == 0L) return(empty)

  out <- list()
  for (nm in intersect(.COST_DOMAIN_MAPS, names(scen@modInp@parameters))) {
    d <- get_data_slot(scen@modInp@parameters[[nm]])
    if (is.null(d) || NROW(d) == 0L) next
    d <- as.data.frame(d)
    if (!all(c("region", "year") %in% names(d))) next
    d <- d[as.character(d$region) %in% coarse, c("region", "year"), drop = FALSE]
    if (NROW(d)) out[[length(out) + 1L]] <- d
  }
  if (!length(out)) return(empty)
  d <- unique(do.call(rbind, out))
  d$region <- as.character(d$region)
  d$year <- as.integer(d$year)
  d[order(d$region, d$year), , drop = FALSE]
}

# Region x year grid: the domain of the system-cost variable.
#
# The atomic regions always -- costs accrue where processes are. PLUS any coarse
# geoscale cell a cost was actually declared at: `sets$region` carries those
# levels, so `eqTradeFixom` and friends will happily compute a cost there, and
# without a matching `mvTotalCost` row `eqCost` never visits it and the cost is
# silently dropped. A cost lands in exactly one cell, coarse or atomic, so adding
# the coarse ones cannot double-count.
.cost_region_year <- function(scen) {
  grid <- tidyr::expand_grid(
    region = .model_regions(scen),
    year   = as.integer(scen@modInp@sets[["year"]])
  ) |> as.data.frame()
  coarse <- .coarse_cost_cells(scen)
  if (NROW(coarse) == 0L) return(grid)
  unique(rbind(grid, coarse))
}

# mvTotalCost: total system cost domain = full region x year grid.
map_mvTotalCost <- function(scen, fmp) {
  .set_map(scen, "mvTotalCost", .cost_region_year(scen), fmp)
}

# mvTotalUserCosts: domain of user-defined cost constraints. For each user cost
# map (`mCosts*`), take its region/year footprint; collapse to the full grid when
# any cost spans it, otherwise union the per-cost footprints. Stays empty when the
# model declares no user costs (matching legacy).
map_mvTotalUserCosts <- function(scen, fmp) {
  dregionyear <- .cost_region_year(scen)
  cost_nms <- grep("^mCosts", names(scen@modInp@parameters), value = TRUE)
  footprints <- lapply(cost_nms, function(x) {
    xx <- get_data_slot(scen@modInp@parameters[[x]])
    if (is.null(xx) || nrow(xx) == 0) return(NULL)
    xx <- as.data.frame(xx) |>
      dplyr::select(dplyr::any_of(c("region", "year"))) |>
      dplyr::distinct()
    if (nrow(xx) == nrow(dregionyear) || ncol(xx) == 0) return(dregionyear)
    if (is.null(xx$region)) {
      return(dplyr::filter(dregionyear, .data$year %in% unique(xx$year)))
    } else if (is.null(xx$year)) {
      return(dplyr::filter(dregionyear, .data$region %in% unique(xx$region)))
    }
    xx
  })
  footprints <- Filter(Negate(is.null), footprints)
  df <- NULL
  if (any(vapply(footprints, nrow, integer(1)) == nrow(dregionyear))) {
    df <- dregionyear
  } else if (length(footprints) > 0) {
    df <- dplyr::distinct(dplyr::bind_rows(footprints))
  } else if (length(scen@modInp@user_costs) > 0) {
    # a cost term with no `subset` builds no mCosts* map but still spans the
    # whole grid -- without this the eqTotalUserCosts domain stayed empty and
    # the term silently never reached the objective
    df <- dregionyear
  }
  if (!is.null(df) && nrow(df) > 0) {
    scen <- .set_map(scen, "mvTotalUserCosts", df, fmp)
  }
  scen
}

# -- registry for the cost_agg family -------------------------------------- #
.cost_agg_builders <- list(
  mvTotalCost      = map_mvTotalCost,
  mvTotalUserCosts = map_mvTotalUserCosts
)


# A per-region rate (`pWacc`, `pSdr`) for the coarse geoscale cells.
#
# A coarse cell uses, in order: the rate DECLARED for it in the model's
# `discount` table (interpolated with the model regions, `.inherit_rates()`);
# else the rate its children share, walking up one level at a time. Where the
# children differ and nothing is declared the cell gets no rate, and that is an
# error only where a process actually needs one -- `.eac_one()` names it.
# (Before any of this, a process at a coarse cell found no rate and was
# annuitised at 0: a 60-year asset at 1/60 instead of 2.88% at SDR 2%.)
#' @noRd
.extend_rate_to_coarse <- function(scen, pname) {
  p <- scen@modInp@parameters[[pname]]
  if (is.null(p)) return(scen)
  df <- as.data.frame(get_data_slot(p))
  if (is.null(df) || NROW(df) == 0L || !"region" %in% names(df)) return(scen)
  h <- tryCatch(.scen_geo_hierarchy(scen), error = function(e) NULL)
  if (is.null(h) || is.null(h$family) || NROW(h$family) == 0L) return(scen)
  fam <- as.data.frame(h$family)   # region = parent, regionp = child
  have <- df
  new_rows <- list()
  repeat {
    todo <- setdiff(unique(as.character(fam$region)), unique(as.character(have$region)))
    added <- FALSE
    for (r in todo) {
      kids <- as.character(fam$regionp[as.character(fam$region) == r])
      sub <- have[as.character(have$region) %in% kids, , drop = FALSE]
      if (!NROW(sub) || !all(kids %in% sub$region)) next   # a child not yet resolved
      by_year <- split(sub, sub$year)
      bad <- names(by_year)[vapply(by_year, function(d) length(unique(round(d$value, 12))) > 1L, logical(1))]
      if (length(bad)) next   # no common rate: left unset, checked where used
      rows <- do.call(rbind, lapply(by_year, function(d) data.frame(region = r, year = d$year[1], value = d$value[1])))
      have <- rbind(have[, c("region", "year", "value")], rows)
      new_rows[[length(new_rows) + 1L]] <- rows
      added <- TRUE
    }
    if (!added) break
  }
  if (!length(new_rows)) return(scen)
  # `.dat2par()` appends: pass only the new rows
  scen@modInp@parameters[[pname]] <- .dat2par(p, unique(do.call(rbind, new_rows)))
  scen
}

# Discount factor for a coarse cost cell.
#
# `eqObjective` weights every `mvTotalCost` row by `pDiscountFactor[r,y]`, which
# is built per region from the social discount rate (`R/obj2modInp.R`). A coarse
# cell has no rate of its own, so it inherits its children's -- but only when
# they agree. Where they differ there is no defensible answer, and averaging one
# would silently pick a number nobody declared, so name the regions and stop.
#
# Runs after the cost_agg recipe, once `mvTotalCost` knows which coarse cells
# exist. A no-op when there are none.
.extend_discount_to_coarse <- function(scen, skip = NULL) {
  p <- scen@modInp@parameters[["pDiscountFactor"]]
  if (is.null(p)) return(scen)
  df <- as.data.frame(get_data_slot(p))
  if (is.null(df) || NROW(df) == 0L) return(scen)

  cells <- .coarse_cost_cells(scen)
  # `skip`: cells already extended (a derived scenario's base); one pass
  # reaches one level up, so a second pass over them would go further
  if (NROW(skip)) {
    cells <- cells[!paste(cells$region, cells$year) %in%
                     paste(skip$region, skip$year), , drop = FALSE]
  }
  if (NROW(cells) == 0L) return(scen)

  h <- .scen_geo_hierarchy(scen)
  if (is.null(h) || is.null(h$family) || NROW(h$family) == 0L) return(scen)
  fam <- as.data.frame(h$family)   # region = parent, regionp = child

  add <- list()
  for (r in unique(cells$region)) {
    # a coarse cell that declares its own `sdr` already has its factor
    if (any(as.character(df$region) == r)) next
    kids <- fam$regionp[as.character(fam$region) == r]
    if (!length(kids)) next
    yrs <- unique(cells$year[cells$region == r])
    sub <- df[as.character(df$region) %in% kids & df$year %in% yrs, , drop = FALSE]
    if (!NROW(sub)) next
    bad <- character()
    keep <- lapply(split(sub, sub$year), function(d) {
      v <- unique(round(d$value, 12))
      if (length(v) > 1L) {
        bad <<- c(bad, paste0(d$year[1], " (",
                              paste(sort(unique(as.character(d$region))),
                                    collapse = ", "), ")"))
        return(NULL)
      }
      data.frame(region = r, year = d$year[1], value = v[1])
    })
    if (length(bad)) {
      stop("A cost is declared at the coarse region '", r, "', but its child ",
           "regions do not share one discount factor in: ",
           paste(bad, collapse = "; "), ".
",
           "  A coarse cell has no discount rate of its own and energyRt will ",
           "not average one. Declare the cost at the child regions instead, or ",
           "give the children a common `sdr`.", call. = FALSE)
    }
    keep <- keep[!vapply(keep, is.null, logical(1))]
    if (length(keep)) add[[length(add) + 1L]] <- do.call(rbind, keep)
  }
  if (!length(add)) return(scen)
  # `.dat2par()` APPENDS to the parameter's data (`rbindlist(obj@data, data)`),
  # so only the NEW rows go in. Passing the existing frame back duplicates every
  # row, which GLPK rejects outright ("pDiscountFactor[R1,2020] already defined")
  # while the sparse path quietly folds it away.
  new_rows <- unique(do.call(rbind, add))
  key <- paste(df$region, df$year)
  new_rows <- new_rows[!paste(new_rows$region, new_rows$year) %in% key, , drop = FALSE]
  if (!NROW(new_rows)) return(scen)
  scen@modInp@parameters[["pDiscountFactor"]] <- .dat2par(p, new_rows)
  scen
}


# ---------------------------------------------------------------------------
# (was R/map_lifespan.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =========================================================================== #
# map_lifespan.R  —  lifespan-window mapping builders (family "lifespan")
#
# One `map_<Name>(scen, fmp) -> scen` per lifespan map. New = investment-year
# window; Span = invest UNION stock years; Eac = Span; OlifeInf = (obj[,region])
# whose operational life is infinite. Plus the technology capacity-retirement maps
# (gated on settings@optimizeRetirement). Registered in `.lifespan_builders`.
#
# Reuses the shared helpers / family table still defined in mapping_engine.R
# (`.lifespan_family_def`, `.set_lifespan_map`, `.lifespan_olife_inf`,
# `.lifespan_retirement_tech`) and the window accessors get_process_invest_years()
# / get_process_years() (interp.R). Those are archived/relocated in the Phase 4
# sweep; here they are simply called.
# =========================================================================== #

# Family table: object family -> key column, whether region-indexed, and the
# new / span / eac / inf map names. (Relocated from mapping_engine.R.)
.lifespan_family_def <- list(
  technology = list(key = "tech",  region = TRUE,
                    new = "mTechNew",    span = "mTechSpan",
                    eac = "mTechEac",    inf  = "mTechOlifeInf",
                    ret_eqnew = "meqTechRetiredNewCap",
                    ret_stock = "mvTechRetiredStock",
                    ret_pairs = "mvTechRetiredNewCap",
                    phaseout = "mvTechPhaseOut"),
  storage    = list(key = "stg",   region = TRUE,
                    new = "mStorageNew", span = "mStorageSpan",
                    eac = "mStorageEac", inf  = "mStorageOlifeInf",
                    # One set for all three parts: olife is storage-wide.
                    ret_eqnew = "meqStorageRetiredNewCap",
                    ret_stock = "mvStorageRetiredStock",
                    ret_pairs = "mvStorageRetiredNewCap",
                    phaseout = "mvStoragePhaseOut"),
  trade      = list(key = "trade", region = FALSE,
                    new = "mTradeNew",   span = "mTradeSpan",
                    eac = NA_character_, inf  = "mTradeOlifeInf",
                    ret_eqnew = "meqTradeRetiredNewCap",
                    ret_stock = "mvTradeRetiredStock",
                    ret_pairs = "mvTradeRetiredNewCap",
                    phaseout  = "mvTradePhaseOut")
)

# process -> class tibble (column needed to route windows to a family).
.lifespan_cls_map <- function(scen) {
  get_process_class(scen) |>
    named_list_to_df(col_names = c("process", "class")) |>
    dplyr::as_tibble()
}

# New / Span / Eac window map for one object family (kind = "new" | "span").
.lifespan_window_map <- function(scen, name, cls, kind, fmp) {
  f   <- .lifespan_family_def[[cls]]
  win <- if (kind == "new") get_process_invest_years(scen) else get_process_years(scen)
  win <- win |>
    dplyr::as_tibble() |>
    dplyr::left_join(.lifespan_cls_map(scen), by = "process") |>
    dplyr::filter(.data$class == cls) |>
    dplyr::select(-"class")
  .set_lifespan_map(scen, name, win, f$key, fmp)
}

# Infinite-operational-life membership map for one object family.
.lifespan_inf_map <- function(scen, name, cls, fmp) {
  f  <- .lifespan_family_def[[cls]]
  df <- .lifespan_olife_inf(scen, cls, f$key, f$region,
                            as.character(scen@settings@region))
  .set_lifespan_map(scen, name, df, f$key, fmp)
}

# mStorageOlifeInf: (stg, region) for storages with a FINITE olife, over their
# operating regions (mStorageSpan). NOTE: storage uses the OPPOSITE convention to
# technology (which lists INFINITE-olife). It is redundant with the
# `ordYear < pStorageOlife + ordYear[yp]` clause in eqStorageOutCap, so it is
# behaviour-neutral; ported for v0.51 parity. Faithful port of obj2modInp.R:1055.
map_mStorageOlifeInf <- function(scen, fmp) {
  span <- .gds(scen, "mStorageSpan")
  if (is.null(span)) return(scen)
  fin <- apply_to_scenario_data(
    scen = scen, classes = "storage", as_list = TRUE,
    func = function(x) {
      ol <- .lifespan_resolve(x, "olife")
      if (nrow(ol) == 0 || all(is.infinite(ol$olife))) return(NULL)
      o <- list(); o[[x@name]] <- data.frame(stg = x@name, stringsAsFactors = FALSE)
      o
    })
  fdf <- dplyr::bind_rows(fin)
  if (is.null(fdf) || nrow(fdf) == 0) return(scen)
  sr <- dplyr::distinct(dplyr::select(span, dplyr::any_of(c("stg", "region"))))
  df <- dplyr::distinct(dplyr::inner_join(sr, fdf, by = "stg"))
  .set_map(scen, "mStorageOlifeInf", df, fmp)
}

# One technology capacity-retirement map (delegates to the shared builder, which
# self-gates on settings@optimizeRetirement and builds only the requested name).
.lifespan_retire_one <- function(scen, name, fmp, cls = "technology") {
  clsm <- .lifespan_cls_map(scen)
  win <- function(accessor) {
    accessor(scen) |>
      dplyr::as_tibble() |>
      dplyr::left_join(clsm, by = "process") |>
      dplyr::filter(.data$class == cls) |>
      dplyr::select(-"class")
  }
  .lifespan_retirement(scen, name, fmp,
                       win(get_process_invest_years),
                       win(get_process_years),
                       as.character(scen@settings@region),
                       cls)
}

# -- per-mapping entry points ---------------------------------------------- #
map_mTechNew         <- function(scen, fmp) .lifespan_window_map(scen, "mTechNew",      "technology", "new",  fmp)
map_mTechSpan        <- function(scen, fmp) .lifespan_window_map(scen, "mTechSpan",     "technology", "span", fmp)
map_mTechEac         <- function(scen, fmp) .lifespan_window_map(scen, "mTechEac",      "technology", "span", fmp)
map_mTechOlifeInf    <- function(scen, fmp) .lifespan_inf_map(scen,    "mTechOlifeInf", "technology",         fmp)
map_mStorageNew      <- function(scen, fmp) .lifespan_window_map(scen, "mStorageNew",      "storage", "new",  fmp)
map_mStorageSpan     <- function(scen, fmp) .lifespan_window_map(scen, "mStorageSpan",     "storage", "span", fmp)
map_mStorageEac      <- function(scen, fmp) .lifespan_window_map(scen, "mStorageEac",      "storage", "span", fmp)
# map_mStorageOlifeInf defined above (storage uses the finite-olife convention).
map_mTradeNew        <- function(scen, fmp) .lifespan_window_map(scen, "mTradeNew",      "trade", "new",  fmp)
map_mTradeSpan       <- function(scen, fmp) .lifespan_window_map(scen, "mTradeSpan",     "trade", "span", fmp)
map_mTradeOlifeInf   <- function(scen, fmp) .lifespan_inf_map(scen,    "mTradeOlifeInf", "trade",        fmp)
map_meqTechRetiredNewCap <- function(scen, fmp) .lifespan_retire_one(scen, "meqTechRetiredNewCap", fmp)
map_mvTechRetiredStock   <- function(scen, fmp) .lifespan_retire_one(scen, "mvTechRetiredStock",   fmp)
map_mvTechRetiredNewCap  <- function(scen, fmp) .lifespan_retire_one(scen, "mvTechRetiredNewCap",  fmp)
map_meqStorageRetiredNewCap <- function(scen, fmp) .lifespan_retire_one(scen, "meqStorageRetiredNewCap", fmp, "storage")
map_mvStorageRetiredStock   <- function(scen, fmp) .lifespan_retire_one(scen, "mvStorageRetiredStock",   fmp, "storage")
map_mvStorageRetiredNewCap  <- function(scen, fmp) .lifespan_retire_one(scen, "mvStorageRetiredNewCap",  fmp, "storage")
map_meqTradeRetiredNewCap   <- function(scen, fmp) .lifespan_retire_one(scen, "meqTradeRetiredNewCap",   fmp, "trade")
map_mvTradeRetiredStock     <- function(scen, fmp) .lifespan_retire_one(scen, "mvTradeRetiredStock",     fmp, "trade")
map_mvTradeRetiredNewCap    <- function(scen, fmp) .lifespan_retire_one(scen, "mvTradeRetiredNewCap",    fmp, "trade")

# Phase-out domain: a process's span years MINUS the earliest one. Phase-out is
# a transition between milestones, so the first year of a span has nothing
# behind it to have aged out. `eqXPhaseOut` reads `vXCap` at the previous
# milestone, which does not exist there either.
.lifespan_phaseout_one <- function(scen, name, fmp, cls) {
  f <- .lifespan_family_def[[cls]]
  if (is.null(f$phaseout) || is.na(f$phaseout) || !identical(name, f$phaseout))
    return(scen)
  clsm <- .lifespan_cls_map(scen)
  win <- get_process_years(scen) |>
    dplyr::as_tibble() |>
    dplyr::left_join(clsm, by = "process") |>
    dplyr::filter(.data$class == cls) |>
    dplyr::select(-"class")
  if (!NROW(win)) return(scen)
  win <- win |>
    dplyr::group_by(dplyr::across(dplyr::any_of(c("process", "region")))) |>
    dplyr::filter(.data$year > min(.data$year)) |>
    dplyr::ungroup()
  .set_lifespan_map(scen, name, win, f$key, fmp)
}

map_mvTechPhaseOut    <- function(scen, fmp) .lifespan_phaseout_one(scen, "mvTechPhaseOut",    fmp, "technology")
map_mvStoragePhaseOut <- function(scen, fmp) .lifespan_phaseout_one(scen, "mvStoragePhaseOut", fmp, "storage")
map_mvTradePhaseOut   <- function(scen, fmp) .lifespan_phaseout_one(scen, "mvTradePhaseOut",   fmp, "trade")

# -- registry for the lifespan family -------------------------------------- #
.lifespan_builders <- list(
  mTechNew         = map_mTechNew,
  mTechSpan        = map_mTechSpan,
  mTechEac         = map_mTechEac,
  mTechOlifeInf    = map_mTechOlifeInf,
  mStorageNew      = map_mStorageNew,
  mStorageSpan     = map_mStorageSpan,
  mStorageEac      = map_mStorageEac,
  mStorageOlifeInf = map_mStorageOlifeInf,
  mTradeNew        = map_mTradeNew,
  mTradeSpan       = map_mTradeSpan,
  mTradeOlifeInf   = map_mTradeOlifeInf,
  meqTechRetiredNewCap = map_meqTechRetiredNewCap,
  mvTechRetiredStock   = map_mvTechRetiredStock,
  mvTechRetiredNewCap  = map_mvTechRetiredNewCap,
  meqStorageRetiredNewCap = map_meqStorageRetiredNewCap,
  mvStorageRetiredStock   = map_mvStorageRetiredStock,
  mvStorageRetiredNewCap  = map_mvStorageRetiredNewCap,
  meqTradeRetiredNewCap   = map_meqTradeRetiredNewCap,
  mvTradeRetiredStock     = map_mvTradeRetiredStock,
  mvTradeRetiredNewCap    = map_mvTradeRetiredNewCap,
  mvTechPhaseOut          = map_mvTechPhaseOut,
  mvStoragePhaseOut       = map_mvStoragePhaseOut,
  mvTradePhaseOut         = map_mvTradePhaseOut
)


# ---------------------------------------------------------------------------
# (was R/map_membership.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =========================================================================== #
# map_membership.R  —  process<->commodity membership maps (family "membership")
#
# One `map_<Name>(scen, fmp) -> scen` per core membership map. Each is the set of
# (object, commodity) pairs where the object consumes/produces the commodity,
# read from the corresponding `*_comm` set (populated in interp_mod from the model
# objects). Registered in `.membership_builders` and built via
# `build_mappings(recipes = "membership")` in interp_mod.
#
# These replace the inline membership block formerly in interp.R (archived to
# drafts/legacy-mapping/membership.R). The aux maps reuse the live
# `.build_aux_membership` helper (interp.R). The other ~18 membership-tagged maps
# (mTech*CommAgg / *SameTimeslice / *Group / mTechEmsFuel / mWeatherRegion / ...) are
# built later in the filter recipe, not here.
# =========================================================================== #

# named-list set (object -> commodities) -> 2-column map parameter.
.membership_map <- function(scen, name, named_list, key, fmp) {
  df <- named_list_to_df(named_list, col_names = c(key, "comm"))
  scen@modInp@parameters[[name]] <-
    d2p(scen@modInp@parameters[[name]], df, fmp(name))
  scen
}

map_mSupComm     <- function(scen, fmp) .membership_map(scen, "mSupComm",     scen@modInp@sets[["supply_comm"]],      "sup",   fmp)
map_mImpComm     <- function(scen, fmp) .membership_map(scen, "mImpComm",     scen@modInp@sets[["import_comm"]],      "imp",   fmp)
map_mDemComm     <- function(scen, fmp) .membership_map(scen, "mDemComm",     scen@modInp@sets[["demand_comm"]],      "dem",   fmp)
map_mExpComm     <- function(scen, fmp) .membership_map(scen, "mExpComm",     scen@modInp@sets[["export_comm"]],      "expp",  fmp)
map_mTradeComm   <- function(scen, fmp) .membership_map(scen, "mTradeComm",   scen@modInp@sets[["trade_comm"]],       "trade", fmp)
map_mStorageComm <- function(scen, fmp) .membership_map(scen, "mStorageComm", scen@modInp@sets[["storage_comm"]],     "stg",   fmp)
# The three storage commodity roles. All default from `@commodity`, so for a
# storage written the old way these three are identical to mStorageComm.
map_mStorageInpComm <- function(scen, fmp) .membership_map(scen, "mStorageInpComm", scen@modInp@sets[["storage_inp_comm"]], "stg", fmp)
map_mStorageOutComm <- function(scen, fmp) .membership_map(scen, "mStorageOutComm", scen@modInp@sets[["storage_out_comm"]], "stg", fmp)
map_mStorageStgComm <- function(scen, fmp) .membership_map(scen, "mStorageStgComm", scen@modInp@sets[["storage_stg_comm"]], "stg", fmp)
map_mTechInpComm <- function(scen, fmp) .membership_map(scen, "mTechInpComm", scen@modInp@sets[["tech_input_comm"]],  "tech",  fmp)
map_mTechOutComm <- function(scen, fmp) .membership_map(scen, "mTechOutComm", scen@modInp@sets[["tech_output_comm"]], "tech",  fmp)

# mTechOneComm: (tech, comm) for each technology's NON-grouped commodities — those
# whose `group` is NA in checkInpOut()'s per-commodity table (input/output/aux).
# Consumed by the single-tech constraint maps (meqTechActSng / meqTechSng2Sng via
# .build_tech_group_maps). Faithful port of the legacy .obj2modInp(technology)
# derivation (obj2modInp.R:2013); reads the tech objects directly like the other
# membership maps read their `*_comm` sets.
# checkInpOut() of every technology, computed once per interpolation: the
# one-commodity and the three group maps read it for each technology.
.tech_inpout <- function(scen) {
  .interp_memo("tech_inpout", .interp_model_fp(scen), function() {
    apply_to_scenario_data(
      scen = scen, classes = "technology", as_list = TRUE,
      func = function(tech) {
        out <- list()
        out[[tech@name]] <- checkInpOut(tech)
        out
      })
  })
}
.tech_inpout_of <- function(io, tech) {
  r <- io[[tech@name]]
  if (is.null(r)) checkInpOut(tech) else r
}

map_mTechOneComm <- function(scen, fmp) {
  io <- .tech_inpout(scen)
  res <- apply_to_scenario_data(
    scen = scen, classes = "technology", as_list = TRUE,
    func = function(tech) {
      ct  <- .tech_inpout_of(io, tech)$comm
      cmm <- rownames(ct)[is.na(ct$group)]
      if (length(cmm) == 0) return(NULL)
      out <- list()
      out[[tech@name]] <- data.frame(tech = tech@name, comm = cmm,
                                     stringsAsFactors = FALSE)
      out
    })
  df <- dplyr::distinct(dplyr::bind_rows(res))
  if (is.null(df) || nrow(df) == 0) return(scen)
  scen@modInp@parameters[["mTechOneComm"]] <-
    d2p(scen@modInp@parameters[["mTechOneComm"]], df, fmp("mTechOneComm"))
  scen
}

# Group membership maps (mTechGroupComm / mTechInpGroup / mTechOutGroup) — the
# missing half of the input/output-group formulation. `.build_tech_group_maps`
# (mapping_engine.R, constraint recipe) READS these to build the group coupling and
# share equations (meqTechGrp2Sng etc.), but no recipe populated them: the spec tags
# them `recipe: membership`, and recipe_membership() is a no-op whose comment wrongly
# claims they are built later in the filter recipe. Without them a grouped-input
# technology gets NO input<->output coupling equation and produces output from zero
# fuel. Derived here from checkInpOut(), like map_mTechOneComm reads its NA-group
# commodities — same object-slot source, run in the same (pre-constraint) phase.

# mTechGroupComm: (tech, group, comm) for each technology's GROUPED commodities —
# those whose `group` is non-NA in checkInpOut()'s per-commodity table.
map_mTechGroupComm <- function(scen, fmp) {
  io <- .tech_inpout(scen)
  res <- apply_to_scenario_data(
    scen = scen, classes = "technology", as_list = TRUE,
    func = function(tech) {
      ct  <- .tech_inpout_of(io, tech)$comm
      idx <- !is.na(ct$group)
      if (!any(idx)) return(NULL)
      out <- list()
      out[[tech@name]] <- data.frame(tech = tech@name, group = ct$group[idx],
                                     comm = rownames(ct)[idx],
                                     stringsAsFactors = FALSE)
      out
    })
  df <- dplyr::distinct(dplyr::bind_rows(res))
  if (is.null(df) || nrow(df) == 0) return(scen)
  scen@modInp@parameters[["mTechGroupComm"]] <-
    d2p(scen@modInp@parameters[["mTechGroupComm"]], df, fmp("mTechGroupComm"))
  scen
}

# mTechInpGroup / mTechOutGroup: (tech, group) for groups typed input / output in
# checkInpOut()'s per-group table (gtype). A tech with no groups yields an empty
# gtype and is skipped.
.build_tech_group_dir <- function(scen, fmp, want_type, param_name) {
  io <- .tech_inpout(scen)
  res <- apply_to_scenario_data(
    scen = scen, classes = "technology", as_list = TRUE,
    func = function(tech) {
      gt <- .tech_inpout_of(io, tech)$group
      if (is.null(gt) || nrow(gt) == 0) return(NULL)
      grp <- rownames(gt)[!is.na(gt$type) & gt$type == want_type]
      if (length(grp) == 0) return(NULL)
      out <- list()
      out[[tech@name]] <- data.frame(tech = tech@name, group = grp,
                                     stringsAsFactors = FALSE)
      out
    })
  df <- dplyr::distinct(dplyr::bind_rows(res))
  if (is.null(df) || nrow(df) == 0) return(scen)
  scen@modInp@parameters[[param_name]] <-
    d2p(scen@modInp@parameters[[param_name]], df, fmp(param_name))
  scen
}

map_mTechInpGroup <- function(scen, fmp)
  .build_tech_group_dir(scen, fmp, "input", "mTechInpGroup")
map_mTechOutGroup <- function(scen, fmp)
  .build_tech_group_dir(scen, fmp, "output", "mTechOutGroup")

# Auxiliary-commodity membership maps, split by input / output direction. The
# helper builds BOTH directions per family in one call (idempotent); registering
# each name to it keeps the registry interface per-mapping.
map_mTechAInp    <- function(scen, fmp) .build_aux_membership(scen, "technology", "tech", fmp, "mTechAInp",    "mTechAOut")
map_mTechAOut    <- function(scen, fmp) .build_aux_membership(scen, "technology", "tech", fmp, "mTechAInp",    "mTechAOut")
map_mStorageAInp <- function(scen, fmp) .build_aux_membership(scen, "storage",    "stg",  fmp, "mStorageAInp", "mStorageAOut")
map_mStorageAOut <- function(scen, fmp) .build_aux_membership(scen, "storage",    "stg",  fmp, "mStorageAInp", "mStorageAOut")

# (weather, region) for each weather object's regions: its `@region`, else the
# `region` column of its own data, else every scenario region. Shared by
# `map_mWeatherRegion()` and `map_mWeatherRegionAt()` so the two can never
# disagree about where a series lives.
# The region universe a weather may be declared in: the WIDENED set, because a
# weather may legitimately sit at a coarser geoscale level than the model's
# atoms (one profile per adm1 serving its adm2 children). Filtering against
# `settings@region` dropped such an object silently and left both weather maps
# empty.
#' @noRd
.weather_region_span <- function(scen) {
  regs <- as.character(.known_regions(scen))
  if (length(regs) == 0) regs <- as.character(scen@settings@region)
  regs
}

# The regions ONE weather object serves: its `@region` slot, else the regions
# named in its own data, else every region of the span. Reading the slot alone
# claimed every region, and `pWeather` defaults to 0, so the phantom cells
# silently zeroed availability wherever the object was never declared.
#
# Shared by `map_mWeatherRegion()`, `map_mWeatherRegionAt()` AND
# `ob2mi(weather)`: the maps read the slot while `pWeather` copied the data
# column, so a series whose data left `region` NA was stored at every region of
# every level while the maps correctly pointed at one.
#' @noRd
.weather_scope <- function(obj, regs, hier = NULL) {
  r <- as.character(obj@region)
  r <- r[!is.na(r)]
  if (length(r) == 0) r <- .obj_data_regions(obj, "weather")
  # naming no region at all means every region -- of the DECLARED level when
  # `@geoframe` names one, which is the whole point of declaring it
  if (length(r) == 0) r <- .obj_level_members(obj, hier, regs)
  r[r %in% regs]
}

#' @noRd
.weather_regions_served <- function(scen) {
  regs <- .weather_region_span(scen)
  hier <- .scen_geo_hierarchy(scen)
  res <- apply_to_scenario_data(
    scen = scen, classes = "weather", as_list = TRUE,
    func = function(x) {
      r <- .weather_scope(x, regs, hier)
      if (length(r) == 0) return(NULL)
      o <- list(); o[[x@name]] <- data.frame(weather = x@name, region = r,
                                             stringsAsFactors = FALSE)
      o
    })
  df <- dplyr::distinct(dplyr::bind_rows(res))
  if (is.null(df) || nrow(df) == 0) return(NULL)
  df
}

# mWeatherRegion: (weather, region) for each weather object's regions (its
# `@region`, or all scenario regions when unset). Faithful port of the legacy
# weather .obj2modInp block (obj2modInp.R:170) which ob2mi(weather) leaves out.
map_mWeatherRegion <- function(scen, fmp) {
  df <- .weather_regions_served(scen)
  if (is.null(df)) return(scen)
  scen@modInp@parameters[["mWeatherRegion"]] <-
    d2p(scen@modInp@parameters[["mWeatherRegion"]], df, fmp("mWeatherRegion"))
  scen
}

# mWeatherRegionAt: (weather, region, regionp) -- the REDIRECT. For a process
# operating in `region`, `regionp` is the region whose `pWeather` series it
# reads. The equations index `pWeather` through this map instead of at their
# own region, which is what lets ONE series declared at a parent level serve
# every child region instead of being copied per child.
#
# The rule: `regionp` is `region` itself when the weather serves it, else the
# NEAREST ancestor of `region` that the weather serves. "Nearest" = finest
# level, so a profile at adm1 loses to one at adm2 if both exist.
#
# Identity rows `(w, r, r)` are emitted for every region a weather already
# serves, so a flat model (or any model whose weather sits where it is used)
# resolves to exactly the pre-redirect lookup and is value-identical.
#
# EXACTLY ONE `regionp` per (weather, region) is required: the equation sums
# over the map, so a duplicate would multiply two profiles into one factor.
# Ties are impossible once the geoframe chain nests (one ancestor per level),
# which `.geo_hierarchy()` now enforces -- the guard below is belt-and-braces.
map_mWeatherRegionAt <- function(scen, fmp) {
  served <- .weather_regions_served(scen)
  if (is.null(served)) return(scen)

  # identity: read the series where it is declared
  cand <- data.frame(weather = served$weather, region = served$region,
                     regionp = served$region, stringsAsFactors = FALSE)

  h <- .scen_geo_hierarchy(scen)
  anc <- .region_ancestry(scen)            # (regionp = node, region = ancestor)
  if (!is.null(h) && !is.null(anc) && nrow(anc) > 0) {
    # a node may read any ancestor the weather serves
    up <- merge(anc, served, by = "region")          # region = the ancestor
    if (nrow(up) > 0) {
      cand <- rbind(cand, data.frame(
        weather = up$weather, region = up$regionp, regionp = up$region,
        stringsAsFactors = FALSE))
    }
  }
  cand <- dplyr::distinct(cand)

  # keep the FINEST candidate per (weather, region). `h$levels` is coarsest
  # first, so a larger rank is finer; identity always wins because a region's
  # own level is finer than any ancestor's.
  if (!is.null(h) && nrow(cand) > 0) {
    rk <- .geo_region_rank(h)
    cand$.rk <- unname(rk[cand$regionp])
    cand$.rk[is.na(cand$.rk)] <- 0L
    cand <- cand[order(cand$weather, cand$region, -cand$.rk), , drop = FALSE]
    cand <- cand[!duplicated(cand[, c("weather", "region")]), , drop = FALSE]
    cand$.rk <- NULL
  }

  dup <- duplicated(cand[, c("weather", "region")])
  if (any(dup)) {
    bad <- unique(cand[dup, c("weather", "region")])
    stop("mWeatherRegionAt: ", nrow(bad), " (weather, region) pair(s) resolve ",
         "to more than one source region, which would multiply two profiles ",
         "into one factor:
   ",
         paste(utils::capture.output(print(utils::head(bad, 5))),
               collapse = "
   "), call. = FALSE)
  }
  if (nrow(cand) == 0) return(scen)
  scen@modInp@parameters[["mWeatherRegionAt"]] <-
    d2p(scen@modInp@parameters[["mWeatherRegionAt"]], cand,
        fmp("mWeatherRegionAt"))
  scen
}

# -- registry for the membership family ------------------------------------ #
.membership_builders <- list(
  mWeatherRegion = map_mWeatherRegion,
  mWeatherRegionAt = map_mWeatherRegionAt,
  mSupComm     = map_mSupComm,
  mImpComm     = map_mImpComm,
  mDemComm     = map_mDemComm,
  mExpComm     = map_mExpComm,
  mTradeComm   = map_mTradeComm,
  mStorageComm = map_mStorageComm,
  mStorageInpComm = map_mStorageInpComm,
  mStorageOutComm = map_mStorageOutComm,
  mStorageStgComm = map_mStorageStgComm,
  mTechInpComm = map_mTechInpComm,
  mTechOutComm = map_mTechOutComm,
  mTechOneComm = map_mTechOneComm,
  mTechGroupComm = map_mTechGroupComm,
  mTechInpGroup  = map_mTechInpGroup,
  mTechOutGroup  = map_mTechOutGroup,
  mTechAInp    = map_mTechAInp,
  mTechAOut    = map_mTechAOut,
  mStorageAInp = map_mStorageAInp,
  mStorageAOut = map_mStorageAOut
)


# ---------------------------------------------------------------------------
# (was R/map_region.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =========================================================================== #
# map_region.R  —  region-hierarchy mapping builders
#
# The spatial twin of the timeslice maps in map_calendar.R. Both are built in the
# `calendar` recipe tier (see `mapping_spec.yml`), which runs before `filter`,
# so `mvBalance` can be restricted with `mCommRegion` once it exists.
#
#   mRegionFamily(region, regionp)  immediate parent -> child, one level only.
#                                   Mirrors mTimesliceFamily. Drives the
#                                   up-aggregation term in eqOutTot/eqInpTot.
#   mCommRegion(comm, region)       the regions each commodity is BALANCED at,
#                                   i.e. the members of its `@geoframe`.
#                                   Mirrors mCommTimeslice.
#
# Both are empty without a geoscale, which is what keeps a flat model bit-for-bit
# unchanged. `.set_cal()` skips empty frames, so nothing is emitted at all.
#
# Unlike the timeslice side there is no `pRegionAgg`: timeslice values are intensive
# rates and need renormalising by duration, whereas regional quantities are
# extensive and simply add up.
# =========================================================================== #

#' @include geoscale.R
NULL

# The pruned region hierarchy for a scenario, or NULL when no geoscale is
# attached. Cheap enough to recompute at each call site.
#' @noRd
.scen_geo_hierarchy <- function(scen) {
  gs <- tryCatch(getGeoscale(scen@settings), error = function(e) NULL)
  .geo_hierarchy(gs, .model_regions(scen))
}

# Regions of each commodity = the members of the commodity's own geo-level.
# A commodity with no `@geoframe` sits at the finest level, which reproduces
# the flat behaviour exactly.
#' @noRd
.comm_region_df <- function(scen) {
  h <- .scen_geo_hierarchy(scen)
  cgl <- map_comm_geoframe(scen)
  if (is.null(h)) {
    # No hierarchy: a `@geoframe` cannot be honoured, and silently balancing
    # the commodity at the finest level would solve a different problem.
    named <- names(cgl)[vapply(cgl, function(v) {
      length(v) > 0 && !is.na(v[1]) && nzchar(v[1])
    }, logical(1))]
    if (length(named) > 0) {
      stop("Commodity ", paste0("'", utils::head(named, 5), "'",
                                collapse = ", "),
           if (length(named) > 5) ", ..." else "",
           " declare(s) `@geoframe`, but the model has no geoscale with ",
           "coarser levels. Attach one with `setGeoscale()`.", call. = FALSE)
    }
    return(NULL)
  }
  rows <- lapply(names(cgl), function(cm) {
    regs <- .geo_level_regions(h, cgl[[cm]])
    if (length(regs) == 0) return(NULL)
    data.frame(comm = cm, region = as.character(regs),
               stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows)
}

# (comm, regionp, region): for each commodity, every ancestor `region` of a
# declared region `regionp`, walking up ONE LEVEL AT A TIME and stopping at the
# commodity's own `@geoframe`.
#
# Two things need this. `mvOutTot`/`mvInpTot` must be EXTENDED with the coarse
# cells -- `vOutTot[STEEL, IND, ...]` has to exist before eqOutTot can write the
# equation that sums the states into it -- and every INTERMEDIATE level must be
# present too, because mRegionFamily is adjacent-only and the accumulation
# chains level by level.
#
# Returns NULL when no commodity names a coarser level, which is what leaves a
# flat model untouched.
#' @noRd
.comm_region_chain <- function(scen) {
  h <- .scen_geo_hierarchy(scen)
  if (is.null(h) || nrow(h$family) == 0) return(NULL)

  levels <- h$levels
  rank <- .geo_level_rank(h)
  finest <- h$finest

  cgl <- map_comm_geoframe(scen)
  target <- vapply(cgl, function(v) {
    if (length(v) == 0 || is.na(v[1]) || !nzchar(v[1])) finest else as.character(v[1])
  }, character(1))
  bad <- setdiff(unique(target), levels)
  if (length(bad) > 0) {
    stop("Unknown `@geoframe` ", paste0("'", bad, "'", collapse = ", "),
         ". The model's geoscale has: ", paste(levels, collapse = ", "), ".",
         call. = FALSE)
  }
  target <- target[target != finest]
  if (length(target) == 0) return(NULL)

  # Which level each region belongs to, so a walk can be cut at the right depth.
  lvl_of <- .geo_level_of(h)

  # Transitive ancestry, accumulated one adjacent step at a time. A join rather
  # than a parent lookup, so a region with more than one parent (levels that
  # cross-cut, which geoscales permits) is handled without special-casing.
  fam <- data.frame(child = as.character(h$family$regionp),
                    parent = as.character(h$family$region),
                    stringsAsFactors = FALSE)
  base <- h$members[[finest]]
  reach <- data.frame(base = base, node = base, stringsAsFactors = FALSE)
  acc <- list()
  for (i in seq_along(levels)) {          # bounded: at most one step per level
    step <- merge(reach, fam, by.x = "node", by.y = "child")
    if (nrow(step) == 0) break
    step <- unique(data.frame(base = step$base, node = step$parent,
                              stringsAsFactors = FALSE))
    acc[[length(acc) + 1L]] <- step
    reach <- step
  }
  if (length(acc) == 0) return(NULL)
  anc <- unique(do.call(rbind, acc))
  anc$lvl <- unname(lvl_of[anc$node])
  anc <- anc[!is.na(anc$lvl), , drop = FALSE]

  rows <- lapply(names(target), function(cm) {
    keep <- anc[rank[anc$lvl] >= rank[[target[[cm]]]], , drop = FALSE]
    if (nrow(keep) == 0) return(NULL)
    data.frame(comm = cm, regionp = keep$base, region = keep$node,
               stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(NULL)
  unique(do.call(rbind, rows))
}

# Add the coarse cells a commodity's own level needs, keeping the fine ones.
#' @noRd
.extend_comm_region <- function(df, chain) {
  if (is.null(df) || nrow(df) == 0 || is.null(chain)) return(df)
  df <- as.data.frame(df)
  if (!all(c("comm", "region") %in% names(df))) return(df)
  add <- merge(df, chain, by.x = c("comm", "region"),
               by.y = c("comm", "regionp"))
  if (nrow(add) == 0) return(df)
  add$region <- add$region.y
  add$region.y <- NULL
  .udf(rbind(df, add[, names(df), drop = FALSE]))
}

# Comm-free transitive region ancestry: (regionp = node, region = ancestor)
# pairs from EVERY region at any level to each of its ancestors, on the
# pruned model hierarchy. The spatial twin of `@timeslice_ancestry`: it
# feeds the always-on totals chain in mvOutTot/mvInpTot, so every commodity
# has total cells at every coarser region level (`@geoframe` keeps setting
# only the BALANCE level). NULL without a geoscale.
#' @noRd
.region_ancestry <- function(scen) {
  h <- .scen_geo_hierarchy(scen)
  if (is.null(h) || nrow(h$family) == 0) return(NULL)
  fam <- data.frame(child = as.character(h$family$regionp),
                    parent = as.character(h$family$region),
                    stringsAsFactors = FALSE)
  base <- unique(unlist(h$members, use.names = FALSE))
  reach <- data.frame(base = base, node = base, stringsAsFactors = FALSE)
  acc <- list()
  for (i in seq_along(h$levels)) {        # bounded: at most one step per level
    step <- merge(reach, fam, by.x = "node", by.y = "child")
    if (nrow(step) == 0) break
    step <- unique(data.frame(base = step$base, node = step$parent,
                              stringsAsFactors = FALSE))
    acc[[length(acc) + 1L]] <- step
    reach <- step
  }
  if (length(acc) == 0) return(NULL)
  anc <- unique(do.call(rbind, acc))
  data.frame(regionp = anc$base, region = anc$node, stringsAsFactors = FALSE)
}

# The region twin of `.extend_comm_timeslice()`: add each row's ancestor-
# region cells (all commodities alike), keeping the fine ones.
#' @noRd
.extend_region_chain <- function(df, anc) {
  if (is.null(df) || nrow(df) == 0 || is.null(anc) || nrow(anc) == 0) {
    return(df)
  }
  df <- as.data.frame(df)
  if (!"region" %in% names(df)) return(df)
  add <- merge(df, anc, by.x = "region", by.y = "regionp")
  if (nrow(add) == 0) return(df)
  add$region <- add$region.y
  add$region.y <- NULL
  .udf(rbind(df, add[, names(df), drop = FALSE]))
}

# Rewrite each (comm, region) pair to the commodity's OWN balancing level,
# leaving pairs that are already there untouched.
#
# Reachability has to be judged at the level the commodity is balanced at: steel
# produced in R1 and demanded nationally IS served, because the national balance
# pools both. Comparing per fine region would wrongly report infeasibility.
#' @noRd
.lift_to_comm_level <- function(df, scen) {
  if (is.null(df) || nrow(df) == 0) return(df)
  chain <- .comm_region_chain(scen)
  cr <- .comm_region_df(scen)
  if (is.null(chain) || is.null(cr)) return(df)
  # keep only the step that lands exactly on the commodity's own level
  cr$.at <- TRUE
  top <- merge(chain, cr, by = c("comm", "region"))
  if (nrow(top) == 0) return(df)
  top <- unique(top[, c("comm", "regionp", "region")])

  df <- as.data.frame(df)
  m <- merge(df, top, by.x = c("comm", "region"), by.y = c("comm", "regionp"),
             all.x = TRUE)
  m$region <- ifelse(is.na(m$region.y), m$region, m$region.y)
  m$region.y <- NULL
  unique(m[, names(df), drop = FALSE])
}

# Pin a domain to each commodity's own region level. The spatial twin of
# `.restrict_comm_timeslice()`; applied to mvBalance ONLY, never to the totals.
#' @noRd
.restrict_comm_region <- function(df, comm_region) {
  if (is.null(df) || nrow(df) == 0 || is.null(comm_region)) return(df)
  dplyr::distinct(as.data.frame(merge0(df, comm_region)))
}

# -- builders --------------------------------------------------------------- #

# Immediate parent->child region pairs (one level), from the geoscale's own
# adjacent-level family table. Transitive ancestry is deliberately NOT used:
# accumulation walks the levels one step at a time, exactly as mTimesliceFamily does.
map_mRegionFamily <- function(scen, fmp) {
  h <- .scen_geo_hierarchy(scen)
  if (is.null(h)) return(scen)
  .set_cal(scen, "mRegionFamily", h$family, fmp)
}

map_mCommRegion <- function(scen, fmp) {
  .set_cal(scen, "mCommRegion", .comm_region_df(scen), fmp)
}

# -- validation ------------------------------------------------------------- #

# A process must sit at a region level at least as FINE as every commodity it
# touches, because collection only ever flows fine -> coarse.
#
# The temporal rule is the same one `get_process_timeframe()` enforces (a
# process runs at the finest timeframe among its commodities). Spatially the
# failure is silent rather than noisy: a plant placed at the nation producing a
# state-balanced commodity would have its output stranded at a cell no balance
# equation reads, so it simply vanishes from the model.
#' @noRd
# A process links a weather that has no series it can read.
#
# `pWeather` defaults to 0 and the factor is MULTIPLICATIVE, so an unresolved
# link silently multiplies the availability bound by zero: the process is shut
# down and the model still solves. Measured on a two-plant model, declaring the
# weather one level up turned a 0.33 objective into 16.67 -- the cheap plant
# was simply never built, with no warning. That is why this is an error.
#
# Resolution is `mWeatherRegionAt`: the process's own region, or the nearest
# ancestor of it the weather serves.
#' @noRd
.assert_weather_reachable <- function(scen) {
  at <- .read_map(scen, "mWeatherRegionAt")
  preg <- named_list_to_df(scen@modInp@sets$process_region,
                           col_names = c("process", "region"))
  if (is.null(preg) || nrow(preg) == 0) return(invisible(NULL))

  # (process, weather) links, read off the objects so this does not depend on
  # which weather maps have been built yet.
  res <- apply_to_scenario_data(
    scen = scen, classes = c("technology", "storage", "supply"),
    as_list = TRUE,
    func = function(x) {
      w <- get_weather(x)
      if (length(w) == 0) return(NULL)
      o <- list(); o[[x@name]] <- data.frame(process = x@name, weather = w,
                                             stringsAsFactors = FALSE)
      o
    })
  links <- dplyr::distinct(dplyr::bind_rows(res))
  if (is.null(links) || nrow(links) == 0) return(invisible(NULL))

  need <- dplyr::distinct(merge(links, preg, by = "process"))
  if (nrow(need) == 0) return(invisible(NULL))
  have <- if (is.null(at) || nrow(at) == 0) {
    need[0, c("weather", "region"), drop = FALSE]
  } else {
    dplyr::distinct(as.data.frame(at)[, c("weather", "region"), drop = FALSE])
  }
  bad <- dplyr::anti_join(need, have, by = c("weather", "region"))
  if (nrow(bad) == 0) return(invisible(NULL))

  srv <- .weather_regions_served(scen)
  where <- function(w) {
    r <- if (is.null(srv)) character(0) else srv$region[srv$weather == w]
    if (length(r) == 0) "nowhere" else paste(utils::head(sort(r), 4),
                                             collapse = ", ")
  }
  bad <- unique(bad[, c("process", "region", "weather"), drop = FALSE])
  det <- vapply(seq_len(min(nrow(bad), 5L)), function(i) sprintf(
    "%s in %s -> weather '%s' (declared at: %s)",
    bad$process[i], bad$region[i], bad$weather[i], where(bad$weather[i])),
    character(1))
  stop("Weather link(s) that cannot be resolved: ", nrow(bad),
       " (process, region) cell(s) reference a weather object with no series ",
       "there and no ancestor to read.
   ",
       paste(det, collapse = "
   "),
       if (nrow(bad) > 5) "
   ..." else "",
       "
Declare the weather at that region, or at a COARSER region of the ",
       "geoscale that contains it. Left unresolved the factor would be 0 and ",
       "the process would be shut down silently.", call. = FALSE)
}

.assert_process_geoframe <- function(scen) {
  h <- .scen_geo_hierarchy(scen)
  if (is.null(h)) return(invisible(NULL))
  cr <- .comm_region_df(scen)
  if (is.null(cr)) return(invisible(NULL))

  reg_rank <- .geo_region_rank(h)
  comm_rank <- vapply(split(cr$region, cr$comm), function(rg) {
    min(reg_rank[unique(rg)], na.rm = TRUE)
  }, numeric(1))

  preg <- named_list_to_df(scen@modInp@sets$process_region,
                           col_names = c("process", "region"))
  pio <- rbind(
    named_list_to_df(scen@modInp@sets$process_inputs,
                     col_names = c("process", "comm")),
    named_list_to_df(scen@modInp@sets$process_outputs,
                     col_names = c("process", "comm"))
  )
  pio <- pio[!is.na(pio$comm) & pio$comm %in% names(comm_rank), , drop = FALSE]
  if (nrow(pio) == 0 || nrow(preg) == 0) return(invisible(NULL))

  # One row per (process, region, comm): with an object declared for many
  # regions and carrying several commodities this is legitimately larger than
  # nrow(preg) + nrow(pio), which is exactly the case data.table refuses by
  # default. The cross is the point of the check.
  d <- merge(preg, pio, by = "process", allow.cartesian = TRUE)
  d$preg_rank <- unname(reg_rank[d$region])
  d$comm_rank <- unname(comm_rank[d$comm])
  bad <- d[!is.na(d$preg_rank) & !is.na(d$comm_rank) &
             d$preg_rank < d$comm_rank, , drop = FALSE]
  if (nrow(bad) == 0) return(invisible(NULL))

  bad <- unique(bad[, c("process", "region", "comm")])

  # A coarse TECHNOLOGY or STORAGE is supported in space: its INPUT flows sit
  # at the commodity's finer regions and its own equations sum them
  # (mTechProcAggRegion / mStorageProcAggRegion). Every other class, and any
  # object declaring a feature whose coarse form is not written yet, is still
  # refused -- accepted but unsummed, the flow is read at a cell that holds no
  # variable and the model either strands it or leaves it free.
  objs <- .level_check_objects(scen)
  cls <- get_process_class(scen)
  ready <- .coarse_axis_ready("region")
  out_blk <- if (ready) .coarse_output_blockers(scen) else character()
  allowed <- vapply(unique(bad$process), function(p) {
    if (!ready) return(FALSE)
    if (!unname(cls[[p]]) %in% .coarse_classes_supported) return(FALSE)
    if (any(startsWith(out_blk, paste0(p, " ")))) return(FALSE)
    length(.coarse_blockers(objs[[p]])) == 0L
  }, logical(1))
  if (all(allowed)) return(invisible(NULL))

  bad <- bad[bad$process %in% names(allowed)[!allowed], , drop = FALSE]
  reasons <- vapply(unique(bad$process), function(p) {
    k <- unname(cls[[p]])
    if (!ready) {
      paste0(p, ": a process coarser than its commodities is not implemented ",
             "on the region axis yet (the equations that would sum its finer ",
             "flows are not written)")
    } else if (!k %in% .coarse_classes_supported) {
      paste0(p, " (", if (is.null(k)) "unknown class" else k,
             "): only ", paste0("`", .coarse_classes_supported, "`",
                                collapse = ", "),
             " may be coarser than its commodities, and only in space")
    } else {
      ob <- out_blk[startsWith(out_blk, paste0(p, " "))]
      if (length(ob) > 0) {
        paste0(ob, " -- only INPUTS may be finer than the process")
      } else {
        paste0(p, ": ", paste(.coarse_blockers(objs[[p]]), collapse = ", "),
               " -- not supported on a coarse process yet")
      }
    }
  }, character(1))

  stop("Process(es) placed at a region level COARSER than a commodity they ",
       "use, whose flows could never reach that commodity's balance:\n   ",
       paste(utils::capture.output(print(bad)), collapse = "\n   "),
       "\n\n   ", paste(reasons, collapse = "\n   "),
       "\n\nMove the process to a finer region, or declare the commodity at a ",
       "coarser `@geoframe`.", call. = FALSE)
}

# -- engine support --------------------------------------------------------- #


# -- registry --------------------------------------------------------------- #

.region_builders <- list(
  mRegionFamily = map_mRegionFamily,
  mCommRegion   = map_mCommRegion
)


# ---------------------------------------------------------------------------
# (was R/map_value.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# =========================================================================== #
# map_value.R  —  value-derived mapping builders (family "value")
#
# One `map_<Name>(scen, fmp) -> scen` per value mapping. A value map's domain is
# the set of points where its source p* parameter(s) carry a defined value (see
# value_on_window / weather_map in mapping_helpers.R), optionally intersected with
# a lifespan window. Registered in `.value_builders` and dispatched by
# build_mappings() ahead of the legacy recipe_value().
#
# The regular maps are table-backed: `.value_map_def` / `.weather_map_def` are the
# single source of truth for each map's source parameter(s) + window / bound
# types. `.value_map_def` is ALSO consumed by interp.R `.param_value_maps()` to
# trim value parameters to the domain of the maps that index them, so it is real
# shared metadata (build + trim), kept co-located with this family.
#
# To add a value mapping: add its row to the table (or write a bespoke builder),
# a thin `map_<Name>`, and an entry to `.value_builders`; declare it in
# modInp.yml (type: map, dimSets) + maps.R.
# =========================================================================== #

# Regular value maps: source = interpolated p* parameter(s) supplying the value
# domain; window = lifespan window map to intersect (NULL = none); gate = optional
# scenario settings flag that must be TRUE.
.value_map_def <- list(
  # technology
  mTechInv      = list(source = "pTechInvcost", window = "mTechNew"),
  mTechFixom    = list(source = "pTechFixom",   window = "mTechSpan"),
  mTechVarom    = list(source = "pTechVarom",   window = "mTechSpan"),
  mTechRetCost  = list(source = "pTechRetCost", window = NULL,
                       gate = "optimizeRetirement"),
  # One retirement-cost domain per storage, spanning the three parts -- the
  # cost equation sums them into a single `vStorageRetCost`, as eqStorageEac
  # does for the annuity.
  mStorageRetCost = list(source = c("pStorageOutRetCost", "pStorageInpRetCost",
                                    "pStorageStgRetCost"),
                         window = NULL, gate = "optimizeRetirement"),
  mTradeRetCost   = list(source = "pTradeRetCost", window = NULL,
                         gate = "optimizeRetirement"),
  # storage
  # Spans the three parts, like `mStorageRetCost` above and for the same reason:
  # `eqStorageFixom` sums all three into a single `vStorageFixom`, with the
  # storing and charging terms as guarded sums INSIDE the equation. Sourced from
  # the out part alone, a storage priced only on its reservoir got no equation at
  # all and its fixed O&M silently left the objective.
  mStorageFixom = list(source = c("pStorageOutFixom", "pStorageInpFixom",
                                  "pStorageStgFixom"),
                       window = "mStorageSpan"),
  mStorageVarom = list(source = c("pStorageCostInp", "pStorageCostOut",
                                  "pStorageCostStore"),
                       window = "mStorageSpan"),
  # STRUCTURE FOLLOWS DATA. `mStorageStgCap` is the set of (stg, region, year)
  # where the storing part carries data of its own -- a stock, a bound, or a
  # price. ONLY there does `vStorageStgCap` exist; elsewhere the af equations
  # fall back to `duration * vStorageOutCap`, which is the previous model.
  #
  # `@storage$comm` is deliberately NOT a source: naming a commodity is
  # metadata, not data. Were it included, every storage would acquire an energy
  # capacity variable and `inp.af.up`/`out.af.up` defaulting to .inf would let the
  # LP drive it to zero -- a priced-but-unbounded capacity vanishing silently.
  #
  # `pStorage*Eac` IS a source, alongside its `pStorage*Invcost` sibling. Both
  # are a price on the part, so both are data by the same rule. Omitting the eac
  # produced exactly the failure the line above warns about, only from the other
  # direction: `mStorage*Eac` still built an objective term, so the LP carried a
  # capacity variable that was priced but appeared in no constraint, and drove it
  # to zero. An `eac`-financed charging or storing part was silently free.
  # THE RESERVOIR IS THE ANCHOR: `vStorageStgCap` exists for every storage, so
  # its maps are the storage's own span and new-capacity window (bespoke
  # builders below). The data rule survives as `.storage_stg_data`, which the
  # check that a reservoir is determined reads.
  # The discharging part, same structure-follows-data gate as the charging part.
  mStorageOutCap = list(source = c("pStorageOutStock", "pStorageOutCap",
                                   "pStorageOutNewCap", "pStorageOutInvcost",
                                   "pStorageOutEac", "pStorageOutFixom"),
                        window = "mStorageSpan"),
  mStorageOutNew = list(source = c("pStorageOutStock", "pStorageOutCap",
                                   "pStorageOutNewCap", "pStorageOutInvcost",
                                   "pStorageOutEac", "pStorageOutFixom"),
                        window = "mStorageNew"),
  # The charging part, same structure-follows-data gate as the storing part.
  mStorageInpCap = list(source = c("pStorageInpStock", "pStorageInpCap",
                                   "pStorageInpNewCap", "pStorageInpInvcost",
                                   "pStorageInpEac", "pStorageInpFixom"),
                        window = "mStorageSpan"),
  mStorageInpNew = list(source = c("pStorageInpStock", "pStorageInpCap",
                                   "pStorageInpNewCap", "pStorageInpInvcost",
                                   "pStorageInpEac", "pStorageInpFixom"),
                        window = "mStorageNew"),
  mStorageInpFixom = list(source = "pStorageInpFixom", window = "mStorageSpan"),
  mStorageInpEac   = list(source = "pStorageInpEac",   window = "mStorageNew"),
  mStorageStgFixom = list(source = "pStorageStgFixom", window = "mStorageSpan"),
  mStorageStgEac   = list(source = "pStorageStgEac",   window = "mStorageNew"),
  # trade
  mTradeInv     = list(source = "pTradeInvcost", window = "mTradeNew"),
  mTradeEac     = list(source = "pTradeEac",     window = "mTradeNew"),
  mTradeFixom   = list(source = "pTradeFixom",   window = "mTradeSpan"),
  # supply
  mvSupCost     = list(source = "pSupCost",      window = NULL),
  mvSupReserve  = list(source = "pSupReserve",   window = NULL)
)

# Weather-availability membership maps: select bound `types` from the source
# bounds parameter ("Up" -> up/fx, "Lo" -> lo/fx) and project onto the map dims.
.weather_map_def <- list(
  mTechWeatherAfUp      = list(source = "pTechWeatherAf",      types = c("up", "fx")),
  mTechWeatherAfLo      = list(source = "pTechWeatherAf",      types = c("lo", "fx")),
  mTechWeatherAfsUp     = list(source = "pTechWeatherAfs",     types = c("up", "fx")),
  mTechWeatherAfsLo     = list(source = "pTechWeatherAfs",     types = c("lo", "fx")),
  mTechWeatherAfcUp     = list(source = "pTechWeatherAfc",     types = c("up", "fx")),
  mTechWeatherAfcLo     = list(source = "pTechWeatherAfc",     types = c("lo", "fx")),
  mStorageWeatherAfUp   = list(source = "pStorageWeatherAf",   types = c("up", "fx")),
  mStorageWeatherAfLo   = list(source = "pStorageWeatherAf",   types = c("lo", "fx")),
  mStorageWeatherInpAfUp = list(source = "pStorageWeatherInpAf", types = c("up", "fx")),
  mStorageWeatherInpAfLo = list(source = "pStorageWeatherInpAf", types = c("lo", "fx")),
  mStorageWeatherOutAfUp = list(source = "pStorageWeatherOutAf", types = c("up", "fx")),
  mStorageWeatherOutAfLo = list(source = "pStorageWeatherOutAf", types = c("lo", "fx")),
  mTradeWeatherAfUp     = list(source = "pTradeWeatherAf",     types = c("up", "fx")),
  mTradeWeatherAfLo     = list(source = "pTradeWeatherAf",     types = c("lo", "fx")),
  mTradeWeatherAfsUp    = list(source = "pTradeWeatherAfs",    types = c("up", "fx")),
  mTradeWeatherAfsLo    = list(source = "pTradeWeatherAfs",    types = c("lo", "fx")),
  mSupWeatherUp         = list(source = "pSupWeather",         types = c("up", "fx")),
  mSupWeatherLo         = list(source = "pSupWeather",         types = c("lo", "fx"))
)

.value_std     <- function(scen, name, fmp) {
  d <- .value_map_def[[name]]
  value_on_window(scen, name, source = d$source, window = d$window,
                  gate = d$gate, fmp = fmp)
}
.value_weather <- function(scen, name, fmp) {
  d <- .weather_map_def[[name]]
  weather_map(scen, name, source = d$source, types = d$types, fmp = fmp)
}

# -- per-mapping entry points (thin, table-backed) ------------------------- #
map_mTechInv      <- function(scen, fmp) .value_std(scen, "mTechInv", fmp)
map_mTechFixom    <- function(scen, fmp) .value_std(scen, "mTechFixom", fmp)
map_mTechVarom    <- function(scen, fmp) .value_std(scen, "mTechVarom", fmp)
map_mTechRetCost  <- function(scen, fmp) .value_std(scen, "mTechRetCost", fmp)
map_mStorageRetCost <- function(scen, fmp) .value_std(scen, "mStorageRetCost", fmp)
map_mTradeRetCost   <- function(scen, fmp) .value_std(scen, "mTradeRetCost", fmp)
map_mStorageFixom <- function(scen, fmp) .value_std(scen, "mStorageFixom", fmp)
# the reservoir is the anchor: its capacity exists wherever the storage operates
.map_copy <- function(scen, name, from, fmp) {
  d <- .gds(scen, from)
  if (is.null(d) || !nrow(d)) return(scen)
  .set_map(scen, name, as.data.frame(d), fmp)
}
# Which storages carry reservoir data of their own (a stock, a bound or a price,
# a declared zero included) is still needed: `.check_storage_parts()` stops on a
# reservoir nothing determines. The data rule is the one this map used before the
# anchor move, evaluated on a scratch copy and kept in `@misc`.
map_mStorageStgCap   <- function(scen, fmp) {
  tmp <- value_on_window(scen, "mStorageStgCap", source = .storage_stg_data,
                         window = "mStorageSpan", fmp = function(x) NULL)
  d <- .gds(tmp, "mStorageStgCap")
  scen@misc$.storage_stg_data <- if (is.null(d)) character(0) else unique(as.character(d$stg))
  .map_copy(scen, "mStorageStgCap", "mStorageSpan", fmp)
}
map_mStorageStgNew   <- function(scen, fmp) .map_copy(scen, "mStorageStgNew", "mStorageNew", fmp)
map_mStorageOutCap   <- function(scen, fmp) .value_std(scen, "mStorageOutCap", fmp)
map_mStorageOutNew   <- function(scen, fmp) .value_std(scen, "mStorageOutNew", fmp)
map_mStorageInpCap   <- function(scen, fmp) .value_std(scen, "mStorageInpCap", fmp)
map_mStorageInpNew   <- function(scen, fmp) .value_std(scen, "mStorageInpNew", fmp)
map_mStorageInpFixom <- function(scen, fmp) .value_std(scen, "mStorageInpFixom", fmp)
map_mStorageInpEac   <- function(scen, fmp) .value_std(scen, "mStorageInpEac", fmp)
map_mStorageStgFixom <- function(scen, fmp) .value_std(scen, "mStorageStgFixom", fmp)
map_mStorageStgEac   <- function(scen, fmp) .value_std(scen, "mStorageStgEac", fmp)

# The COMPLEMENT of mStorageStgCap within the storage's operating span: where the
# storing part has no data and therefore no capacity variable, so the af bounds
# use `duration * vStorageOutCap` instead. Built as a set difference rather than
# a value domain, hence the bespoke builder. Must run AFTER map_mStorageStgCap
# (see the .value_builders order below).
# `have` is the part's capacity domain; the complement within the operating span
# is where that part has no variable and the linking ratio is inlined onto
# vStorageOutCap instead. Must run AFTER the map it complements.
.map_no_part_cap <- function(scen, name, have, fmp) {
  span <- .gds(scen, "mStorageSpan")
  if (is.null(span) || !nrow(span)) return(scen)
  span <- as.data.frame(span)
  h <- .gds(scen, have)
  out <- if (is.null(h) || !nrow(h)) span else
    dplyr::anti_join(span, as.data.frame(h), by = c("stg", "region", "year"))
  .set_map(scen, name, as.data.frame(out), fmp)
}
map_mStorageNoStgCap <- function(scen, fmp)
  .map_no_part_cap(scen, "mStorageNoStgCap", "mStorageStgCap", fmp)
map_mStorageNoInpCap <- function(scen, fmp)
  .map_no_part_cap(scen, "mStorageNoInpCap", "mStorageInpCap", fmp)

# The INTERSECTION of the charging and storing capacity domains: the inp2stg
# ratio links vStorageInpCap to vStorageStgCap, so its constraint can only
# exist where BOTH variables do. There is no inlining fallback (nothing to
# inline the ratio onto), so an unpriced part simply drops the link. Must run
# AFTER both parents (see the .value_builders order below).
map_mStorageInpStgCap <- function(scen, fmp) {
  inp <- .gds(scen, "mStorageInpCap")
  stg <- .gds(scen, "mStorageStgCap")
  if (is.null(inp) || !nrow(inp) || is.null(stg) || !nrow(stg)) return(scen)
  out <- dplyr::inner_join(as.data.frame(inp), as.data.frame(stg),
                           by = c("stg", "region", "year"))
  .set_map(scen, "mStorageInpStgCap", as.data.frame(out), fmp)
}
# The INTERSECTION of the charging and discharging capacity domains: the
# inp2out ratio links the two, so it can only exist where both do.
map_mStorageInpOutCap <- function(scen, fmp) {
  inp <- .gds(scen, "mStorageInpCap")
  out <- .gds(scen, "mStorageOutCap")
  if (is.null(inp) || !nrow(inp) || is.null(out) || !nrow(out)) return(scen)
  d <- dplyr::inner_join(as.data.frame(inp), as.data.frame(out),
                         by = c("stg", "region", "year"))
  .set_map(scen, "mStorageInpOutCap", as.data.frame(d), fmp)
}
map_mStorageVarom <- function(scen, fmp) .value_std(scen, "mStorageVarom", fmp)

# -- storage parts: the checks the anchor needs ------------------------------ #
# The reservoir (`vStorageStgCap`) exists for every storage; the charger and
# discharger only with data. Three things the maps cannot say on their own, so
# interpolation stops on them (`.check_storage_parts()`, after the constraint
# maps are built):
#   1. a reservoir nothing determines -- no reservoir data (stock, bound, cost)
#      and no link to a sized part (`duration`, `inp2stg`) -- is a free,
#      unbounded energy capacity;
#   2. a flow bound (`inp.af`, `out.af`) on a part that does not exist binds a
#      capacity variable that is not there;
#   3. a link (`duration`, `inp2out`, `inp2stg`) declared on a part that does
#      not exist links nothing.
.storage_stg_data <- c("pStorageStgStock", "pStorageStgCap", "pStorageStgNewCap",
                       "pStorageStgInvcost", "pStorageStgEac", "pStorageStgFixom")

# storages (stg names) with any row in `pars`; for a bounds parameter only rows
# that bound (lo > 0 or up < Inf) count
.storages_in <- function(scen, pars) {
  out <- character(0)
  for (p in pars) {
    d <- .gds(scen, p)
    if (is.null(d) || !nrow(d) || !"stg" %in% colnames(d)) next
    if ("type" %in% colnames(d)) {
      d <- d[(as.character(d$type) == "lo" & d$value > 0) |
               (as.character(d$type) == "up" & is.finite(d$value)), , drop = FALSE]
    }
    out <- union(out, unique(as.character(d$stg)))
  }
  out
}
.stg_of <- function(scen, map) {
  d <- .gds(scen, map)
  if (is.null(d) || !nrow(d)) character(0) else unique(as.character(d$stg))
}

# storages whose reservoir is DECLARED on the object -- any stg.* stock, bound,
# price or fixed cost, a deliberate zero included. Zero rows are dropped before
# the maps are built, so an explicit `stg.invcost = 0` (a free reservoir, chosen)
# is read from the objects; variant names map back to their base object.
.storage_stg_declared <- function(scen) {
  objs <- tryCatch(getObjects(scen@model, class = "storage"), error = function(e) NULL)
  if (!length(objs)) return(character(0))
  has <- function(df, cols) {
    cols <- intersect(cols, colnames(df))
    length(cols) > 0 && any(!is.na(as.matrix(df[, cols, drop = FALSE])))
  }
  base <- unique(unlist(lapply(objs, function(o) {
    stg_cols <- function(df) grep("^stg[.]", colnames(df), value = TRUE)
    if (has(o@capacity, stg_cols(o@capacity)) || has(o@invcost, stg_cols(o@invcost)) ||
        has(o@fixom, stg_cols(o@fixom))) o@name else NULL
  })))
  if (!length(base)) return(character(0))
  v <- tryCatch(as.data.frame(scen@modInp@sets$variant), error = function(e) NULL)
  if (is.null(v) || !nrow(v) || !all(c("name", "base") %in% colnames(v))) return(base)
  union(base, as.character(v$name[as.character(v$base) %in% base]))
}

.check_storage_parts <- function(scen) {
  all_stg <- .stg_of(scen, "mStorageSpan")
  if (!length(all_stg)) return(invisible())
  inp <- .stg_of(scen, "mStorageInpCap"); out <- .stg_of(scen, "mStorageOutCap")
  msg <- character(0)
  # 1. the reservoir is determined
  linked <- union(.stg_of(scen, "mStorageDurationLo"),
                  c(.stg_of(scen, "mStorageDurationUp"), .stg_of(scen, "mStorageDurationFx"),
                    .stg_of(scen, "mStorageInp2stgLo"), .stg_of(scen, "mStorageInp2stgUp"),
                    .stg_of(scen, "mStorageInp2stgFx")))
  free <- setdiff(all_stg, Reduce(union, list(scen@misc$.storage_stg_data, linked,
                                               .storage_stg_declared(scen))))
  if (length(free)) {
    msg <- c(msg, paste0(
      "the reservoir of ", paste(utils::head(free, 5), collapse = ", "),
      " is not determined: give it a stock, a capacity bound or a price (stg.*; ",
      "stg.invcost = 0 for a deliberately free reservoir), or link it to a sized ",
      "discharger (duration) or charger (inp2stg)"))
  }
  # 2. flow bounds on a part that does not exist
  for (pt in list(list("Inp", inp, "inp.af", "charger"), list("Out", out, "out.af", "discharger"))) {
    b <- unique(unlist(lapply(c("Lo", "Up", "Fx"), function(s)
      .stg_of(scen, paste0("meqStorage", pt[[1]], s)))))
    bad <- setdiff(b, pt[[2]])
    if (length(bad)) {
      msg <- c(msg, paste0(pt[[3]], " is set on ", paste(utils::head(bad, 5), collapse = ", "),
                           ", which has no ", pt[[4]], " (no ", tolower(pt[[1]]),
                           ".* capacity or cost data)"))
    }
  }
  # 3. links on a part that does not exist
  for (lk in list(list("pStorageDuration", out, "duration", "discharger"),
                  list("pStorageInp2out", intersect(inp, out), "inp2out", "charger and a discharger"),
                  list("pStorageInp2stg", inp, "inp2stg", "charger"))) {
    bad <- intersect(setdiff(.storages_in(scen, lk[[1]]), lk[[2]]), all_stg)
    if (length(bad)) {
      msg <- c(msg, paste0(lk[[3]], " is declared on ", paste(utils::head(bad, 5), collapse = ", "),
                           ", which has no ", lk[[4]], " to link"))
    }
  }
  if (length(msg)) stop(paste0("Storage: ", msg, collapse = "\n"), call. = FALSE)
  invisible()
}
map_mTradeInv     <- function(scen, fmp) .value_std(scen, "mTradeInv", fmp)
map_mTradeEac     <- function(scen, fmp) .value_std(scen, "mTradeEac", fmp)
map_mTradeFixom   <- function(scen, fmp) .value_std(scen, "mTradeFixom", fmp)
map_mvSupCost     <- function(scen, fmp) .value_std(scen, "mvSupCost", fmp)
map_mvSupReserve  <- function(scen, fmp) .value_std(scen, "mvSupReserve", fmp)

map_mTechWeatherAfUp      <- function(scen, fmp) .value_weather(scen, "mTechWeatherAfUp", fmp)
map_mTradeWeatherAfUp     <- function(scen, fmp) .value_weather(scen, "mTradeWeatherAfUp", fmp)
map_mTradeWeatherAfLo     <- function(scen, fmp) .value_weather(scen, "mTradeWeatherAfLo", fmp)
map_mTradeWeatherAfsUp    <- function(scen, fmp) .value_weather(scen, "mTradeWeatherAfsUp", fmp)
map_mTradeWeatherAfsLo    <- function(scen, fmp) .value_weather(scen, "mTradeWeatherAfsLo", fmp)
map_mTechWeatherAfLo      <- function(scen, fmp) .value_weather(scen, "mTechWeatherAfLo", fmp)
map_mTechWeatherAfsUp     <- function(scen, fmp) .value_weather(scen, "mTechWeatherAfsUp", fmp)
map_mTechWeatherAfsLo     <- function(scen, fmp) .value_weather(scen, "mTechWeatherAfsLo", fmp)
map_mTechWeatherAfcUp     <- function(scen, fmp) .value_weather(scen, "mTechWeatherAfcUp", fmp)
map_mTechWeatherAfcLo     <- function(scen, fmp) .value_weather(scen, "mTechWeatherAfcLo", fmp)
map_mStorageWeatherAfUp   <- function(scen, fmp) .value_weather(scen, "mStorageWeatherAfUp", fmp)
map_mStorageWeatherAfLo   <- function(scen, fmp) .value_weather(scen, "mStorageWeatherAfLo", fmp)
map_mStorageWeatherInpAfUp <- function(scen, fmp) .value_weather(scen, "mStorageWeatherInpAfUp", fmp)
map_mStorageWeatherInpAfLo <- function(scen, fmp) .value_weather(scen, "mStorageWeatherInpAfLo", fmp)
map_mStorageWeatherOutAfUp <- function(scen, fmp) .value_weather(scen, "mStorageWeatherOutAfUp", fmp)
map_mStorageWeatherOutAfLo <- function(scen, fmp) .value_weather(scen, "mStorageWeatherOutAfLo", fmp)
map_mSupWeatherUp         <- function(scen, fmp) .value_weather(scen, "mSupWeatherUp", fmp)
map_mSupWeatherLo         <- function(scen, fmp) .value_weather(scen, "mSupWeatherLo", fmp)

# Regions named in an object's own data slots. Empty when the object names
# none, and empty when any row leaves `region` NA: an NA row is a wildcard
# meaning every region, so the caller falls back to the full span.
.obj_data_regions <- function(obj, slots) {
  out <- character(0)
  for (sl in slots) {
    if (!.hasSlot(obj, sl)) next
    d <- methods::slot(obj, sl)
    if (!is.data.frame(d) || nrow(d) == 0 || !"region" %in% names(d)) next
    r <- as.character(d$region)
    if (any(is.na(r))) return(character(0))
    out <- c(out, r)
  }
  unique(out[nzchar(out)])
}

# -- bespoke value maps ----------------------------------------------------- #
# mSupSpan: (sup, region) operational span of each supply object (its own regions,
# defaulting to all model regions when unspecified).
map_mSupSpan <- function(scen, fmp) {
  # A supply spans the regions its COMMODITY is balanced at: the atoms for a
  # finest-level commodity (the flat case, unchanged), the level's members
  # for a commodity with a coarse `@geoframe`. Supply is a STRICT-level class
  # (check_levels.R): a nation-balanced commodity MUST be supplied at the
  # nation -- and the old blanket atom intersection silently DELETED exactly
  # that shape, leaving the balance with a free costless vOutTot cell
  # (energy from nowhere, objective 0, model feasible).
  atoms <- .model_regions(scen)
  comm_reg <- .comm_region_df(scen)   # NULL when no geoscale is attached
  res <- apply_to_scenario_data(
    scen = scen, classes = "supply", as_list = TRUE,
    func = function(obj) {
      allowed <- atoms
      if (!is.null(comm_reg)) {
        m <- comm_reg$region[comm_reg$comm == obj@commodity]
        if (length(m) > 0) allowed <- m
      }
      # The span must follow the DECLARATION, and a supply may be scoped
      # either by the `@region` slot or by the `region` column of its own
      # data. Reading the slot alone spanned a data-scoped supply across
      # every region, and those cells carry no `pSupCost` / `pSupAva` row --
      # so they were free and unbounded, and the solver drew from them in
      # preference to every priced cell (objective 0, model feasible).
      regs <- as.character(obj@region)
      regs <- regs[!is.na(regs)]
      if (length(regs) == 0) regs <- .obj_data_regions(obj, c("supply",
                                                              "reserve"))
      if (length(regs) == 0) regs <- allowed
      regs <- regs[regs %in% allowed]
      if (length(regs) == 0) return(NULL)
      out <- list()
      out[[obj@name]] <- data.frame(sup = obj@name, region = regs,
                                    stringsAsFactors = FALSE)
      out
    }
  )
  if (length(res) == 0) return(scen)
  .set_map(scen, "mSupSpan", dplyr::bind_rows(res), fmp)
}

# mTechRetirement: technologies with retirement optimisation enabled.
map_mTechRetirement <- function(scen, fmp) {
  if (!isTRUE(scen@settings@optimizeRetirement)) return(scen)
  techs <- .retirement_objects(scen, "technology")
  if (length(techs) == 0) return(scen)
  .set_map(scen, "mTechRetirement",
           data.frame(tech = techs, stringsAsFactors = FALSE), fmp)
}

# mTaxCost / mSubCost: (levy, region, year) domains where a tax / subsidy applies,
# projected from the three rate tables (inp/out/bal) -- the commodity and the
# timeslice are summed over inside eqTaxCost/eqSubsCost, so neither is a domain
# of the map. The levy dimension is what keeps two instruments on one commodity
# from overwriting each other. NA region in the source means "all regions" and is
# expanded to every model region.
# The dims come from the parameter's own @dimSets, so re-dimensioning the map in
# data-raw/modInp.yml is enough; nothing here is positional.
.policy_cost_map <- function(scen, name, sources, fmp) {
  p <- scen@modInp@parameters[[name]]
  if (is.null(p)) return(scen)
  set <- p@dimSets
  tx <- lapply(sources, function(sp) {
    sp_par <- scen@modInp@parameters[[sp]]
    if (is.null(sp_par)) return(NULL)
    sd <- get_data_slot(sp_par)
    if (is.null(sd) || nrow(sd) == 0) return(NULL)
    as.data.frame(sd)
  })
  df <- .reduce_sect_merge_unique(tx, set)
  if (is.null(df) || nrow(df) == 0) return(scen)
  regions <- scen@modInp@sets[["region"]]
  if ("region" %in% set && length(regions) > 0 && anyNA(df$region)) {
    na_rows <- df[is.na(df$region), setdiff(colnames(df), "region"), drop = FALSE]
    expanded <- merge0(na_rows,
                       data.frame(region = regions, stringsAsFactors = FALSE))
    df <- dplyr::distinct(dplyr::bind_rows(df[!is.na(df$region), , drop = FALSE],
                                           expanded))
  }
  .set_map(scen, name, df, fmp)
}
# Every rate table the levy can be written in, commodity and process alike: a
# process-only levy has no pTaxCost* rows at all, and leaving its parameters out
# here would give it an empty (levy, region, year) domain -- eqTaxCost would not
# be generated for it and the rate would be silently ignored.
map_mTaxCost <- function(scen, fmp)
  .policy_cost_map(scen, "mTaxCost",
                   c("pTaxCostInp", "pTaxCostOut", "pTaxCostBal",
                     "pTaxProcAct", "pTaxProcInp", "pTaxProcOut",
                     "pTaxProcCap", "pTaxProcNcap", "pTaxProcInpCap",
                     "pTaxProcInpNcap", "pTaxProcOutCap", "pTaxProcOutNcap"), fmp)
map_mSubCost <- function(scen, fmp)
  .policy_cost_map(scen, "mSubCost",
                   c("pSubCostInp", "pSubCostOut", "pSubCostBal",
                     "pSubProcAct", "pSubProcInp", "pSubProcOut",
                     "pSubProcCap", "pSubProcNcap", "pSubProcInpCap",
                     "pSubProcInpNcap", "pSubProcOutCap", "pSubProcOutNcap"), fmp)

# -- registry for the value family ----------------------------------------- #
.value_builders <- list(
  mTechInv      = map_mTechInv,
  mTechFixom    = map_mTechFixom,
  mTechVarom    = map_mTechVarom,
  mTechRetCost  = map_mTechRetCost,
  mStorageRetCost = map_mStorageRetCost,
  mTradeRetCost = map_mTradeRetCost,
  mStorageFixom = map_mStorageFixom,
  mStorageStgCap = map_mStorageStgCap,
  mStorageOutCap = map_mStorageOutCap,
  mStorageOutNew = map_mStorageOutNew,
  mStorageInpCap = map_mStorageInpCap,
  mStorageNoInpCap = map_mStorageNoInpCap,   # AFTER mStorageInpCap
  mStorageInpNew = map_mStorageInpNew,
  mStorageInpFixom = map_mStorageInpFixom,
  mStorageInpEac = map_mStorageInpEac,
  mStorageNoStgCap = map_mStorageNoStgCap,   # AFTER mStorageStgCap: complement
  mStorageInpStgCap = map_mStorageInpStgCap, # AFTER both parents: intersection
  mStorageInpOutCap = map_mStorageInpOutCap, # AFTER both parents: intersection
  mStorageStgNew = map_mStorageStgNew,
  mStorageStgFixom = map_mStorageStgFixom,
  mStorageStgEac = map_mStorageStgEac,
  mStorageVarom = map_mStorageVarom,
  mTradeInv     = map_mTradeInv,
  mTradeEac     = map_mTradeEac,
  mTradeFixom   = map_mTradeFixom,
  mvSupCost     = map_mvSupCost,
  mvSupReserve  = map_mvSupReserve,
  mTechWeatherAfUp      = map_mTechWeatherAfUp,
  mTradeWeatherAfUp     = map_mTradeWeatherAfUp,
  mTradeWeatherAfLo     = map_mTradeWeatherAfLo,
  mTradeWeatherAfsUp    = map_mTradeWeatherAfsUp,
  mTradeWeatherAfsLo    = map_mTradeWeatherAfsLo,
  mTechWeatherAfLo      = map_mTechWeatherAfLo,
  mTechWeatherAfsUp     = map_mTechWeatherAfsUp,
  mTechWeatherAfsLo     = map_mTechWeatherAfsLo,
  mTechWeatherAfcUp     = map_mTechWeatherAfcUp,
  mTechWeatherAfcLo     = map_mTechWeatherAfcLo,
  mStorageWeatherAfUp   = map_mStorageWeatherAfUp,
  mStorageWeatherAfLo   = map_mStorageWeatherAfLo,
  mStorageWeatherInpAfUp = map_mStorageWeatherInpAfUp,
  mStorageWeatherInpAfLo = map_mStorageWeatherInpAfLo,
  mStorageWeatherOutAfUp = map_mStorageWeatherOutAfUp,
  mStorageWeatherOutAfLo = map_mStorageWeatherOutAfLo,
  mSupWeatherUp         = map_mSupWeatherUp,
  mSupWeatherLo         = map_mSupWeatherLo,
  mSupSpan        = map_mSupSpan,
  mTechRetirement = map_mTechRetirement,
  mTaxCost        = map_mTaxCost,
  mSubCost        = map_mSubCost
)
