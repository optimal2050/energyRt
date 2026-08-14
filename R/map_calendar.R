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
map_mCommTimeslice  <- function(scen, fmp) .set_cal(scen, "mCommTimeslice", .comm_timeslice_df(scen), fmp)
map_mTechTimeslice  <- function(scen, fmp) .set_cal(scen, "mTechTimeslice",  .proc_timeslice_for(.process_timeslice_df(scen), get_process_class(scen), "technology", "tech"),  fmp)
map_mSupTimeslice   <- function(scen, fmp) .set_cal(scen, "mSupTimeslice",   .proc_timeslice_for(.process_timeslice_df(scen), get_process_class(scen), "supply",     "sup"),   fmp)
map_mTradeTimeslice <- function(scen, fmp) .set_cal(scen, "mTradeTimeslice", .proc_timeslice_for(.process_timeslice_df(scen), get_process_class(scen), "trade",      "trade"), fmp)
map_mImpTimeslice   <- function(scen, fmp) .set_cal(scen, "mImpTimeslice",   .proc_timeslice_for(.process_timeslice_df(scen), get_process_class(scen), "import",     "imp"),   fmp)
map_mExpTimeslice   <- function(scen, fmp) .set_cal(scen, "mExpTimeslice",   .proc_timeslice_for(.process_timeslice_df(scen), get_process_class(scen), "export",     "expp"),  fmp)

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
  scen
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
