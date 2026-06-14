# =========================================================================== #
# map_calendar.R  —  calendar / horizon mapping builders (family "calendar")
#
# One `map_<Name>(scen, fmp) -> scen` per calendar map. These are model-object
# independent: they derive from the calendar (slices, ancestry, next-in-slice) and
# the horizon milestones. Registered in `.calendar_builders`. Reuses the live
# helpers `.set_calendar_map`, `.comm_slice_df`, `.process_slice_df`,
# `.proc_slice_for`, `merge0` from mapping_engine.R.
#
# Three calendar-tagged maps (mWeatherSlice, mStorageFullYear, mTechFullYear) are
# built in later recipes (filter/constraint), not here.
# =========================================================================== #

# write only when the builder yields a non-empty frame (matches recipe_calendar).
.set_cal <- function(scen, name, df, fmp) {
  if (is.null(df) || nrow(df) == 0) return(scen)
  .set_calendar_map(scen, name, df, fmp)
}

# -- shared calendar accessors --------------------------------------------- #
.cal_slices <- function(scen) as.character(scen@settings@calendar@slice_share$slice)
.cal_anc    <- function(scen) dplyr::as_tibble(scen@settings@calendar@slice_ancestry)
.cal_mid    <- function(scen) as.integer(scen@settings@horizon@intervals$mid)

# slice ancestry-expansion (parent -> self + all descendants), shared by
# mSliceParentChildE and mCommSliceOrParent.
.cal_spce <- function(scen) {
  slices <- .cal_slices(scen)
  dplyr::bind_rows(
    dplyr::tibble(slice = slices, slicep = slices),
    .cal_anc(scen) |> dplyr::transmute(slice  = as.character(.data$parent),
                                       slicep = as.character(.data$child))
  ) |> dplyr::distinct()
}

# -- per-object / per-commodity slice maps --------------------------------- #
map_mCommSlice  <- function(scen, fmp) .set_cal(scen, "mCommSlice", .comm_slice_df(scen), fmp)
map_mTechSlice  <- function(scen, fmp) .set_cal(scen, "mTechSlice",  .proc_slice_for(.process_slice_df(scen), get_process_class(scen), "technology", "tech"),  fmp)
map_mSupSlice   <- function(scen, fmp) .set_cal(scen, "mSupSlice",   .proc_slice_for(.process_slice_df(scen), get_process_class(scen), "supply",     "sup"),   fmp)
map_mTradeSlice <- function(scen, fmp) .set_cal(scen, "mTradeSlice", .proc_slice_for(.process_slice_df(scen), get_process_class(scen), "trade",      "trade"), fmp)
map_mImpSlice   <- function(scen, fmp) .set_cal(scen, "mImpSlice",   .proc_slice_for(.process_slice_df(scen), get_process_class(scen), "import",     "imp"),   fmp)
map_mExpSlice   <- function(scen, fmp) .set_cal(scen, "mExpSlice",   .proc_slice_for(.process_slice_df(scen), get_process_class(scen), "export",     "expp"),  fmp)

# -- slice ancestry / next maps -------------------------------------------- #
map_mSliceParentChild <- function(scen, fmp) {
  df <- .cal_anc(scen) |>
    dplyr::transmute(slice = as.character(.data$parent),
                     slicep = as.character(.data$child))
  .set_cal(scen, "mSliceParentChild", df, fmp)
}

map_mSliceParentChildE <- function(scen, fmp) .set_cal(scen, "mSliceParentChildE", .cal_spce(scen), fmp)

# Commodity slice-or-parent aggregation map: for each commodity, maps any
# finer-or-equal slice (`slicep`) up to the commodity's own slice level (`slice`).
map_mCommSliceOrParent <- function(scen, fmp) {
  cs <- .comm_slice_df(scen)
  if (is.null(cs) || nrow(cs) == 0) return(scen)
  cs   <- as.data.frame(cs)
  spce <- as.data.frame(.cal_spce(scen))
  l1 <- merge0(
    data.frame(comm = unique(cs$comm), stringsAsFactors = FALSE),
    data.frame(slice = as.character(.cal_slices(scen)), stringsAsFactors = FALSE)
  )
  l2 <- as.data.frame(merge0(cs, spce)) |>
    dplyr::select(dplyr::all_of(c("comm", "slice", "slicep")))
  l3 <- l2 |>
    dplyr::select(dplyr::all_of(c("comm", "slicep"))) |>
    dplyr::distinct() |>
    dplyr::rename(slice = "slicep")
  l3 <- rbind(l1, l3)
  l3 <- l3[!duplicated(l3) & !duplicated(l3, fromLast = TRUE), , drop = FALSE]
  l3$slicep <- l3$slice
  .set_cal(scen, "mCommSliceOrParent", rbind(l2, l3), fmp)
}

map_mSliceNext <- function(scen, fmp) {
  nxt <- dplyr::as_tibble(scen@settings@calendar@next_in_timeframe)
  if (nrow(nxt) == 0) return(scen)
  .set_cal(scen, "mSliceNext",
           nxt |> dplyr::transmute(slice = as.character(.data$slice),
                                   slicep = as.character(.data$slicep)), fmp)
}

map_mSliceFYearNext <- function(scen, fmp) {
  nxt <- dplyr::as_tibble(scen@settings@calendar@next_in_year)
  if (nrow(nxt) == 0) return(scen)
  .set_cal(scen, "mSliceFYearNext",
           nxt |> dplyr::transmute(slice = as.character(.data$slice),
                                   slicep = as.character(.data$slicep)), fmp)
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
map_mSameSlice  <- function(scen, fmp) {
  s <- .cal_slices(scen)
  .set_cal(scen, "mSameSlice", dplyr::tibble(slice = s, slicep = s), fmp)
}
map_mSameRegion <- function(scen, fmp) {
  r <- as.character(scen@settings@region)
  .set_cal(scen, "mSameRegion", dplyr::tibble(region = r, regionp = r), fmp)
}

# -- registry for the calendar family -------------------------------------- #
.calendar_builders <- list(
  mCommSlice         = map_mCommSlice,
  mTechSlice         = map_mTechSlice,
  mSupSlice          = map_mSupSlice,
  mTradeSlice        = map_mTradeSlice,
  mImpSlice          = map_mImpSlice,
  mExpSlice          = map_mExpSlice,
  mSliceParentChild  = map_mSliceParentChild,
  mSliceParentChildE = map_mSliceParentChildE,
  mCommSliceOrParent = map_mCommSliceOrParent,
  mSliceNext         = map_mSliceNext,
  mSliceFYearNext    = map_mSliceFYearNext,
  mMilestoneFirst    = map_mMilestoneFirst,
  mMilestoneLast     = map_mMilestoneLast,
  mMidMilestone      = map_mMidMilestone,
  mMilestoneNext     = map_mMilestoneNext,
  mMilestoneHasNext  = map_mMilestoneHasNext,
  mSameSlice         = map_mSameSlice,
  mSameRegion        = map_mSameRegion
)
