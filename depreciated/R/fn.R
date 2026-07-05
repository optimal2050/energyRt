#' @include defaults.R
#'

# a list of functions to create data for each parameter from an object
# every function must be applied to
fn <- list()

#==========================================================================#
# generics ####
# functions to use in different classes

fn$self <- function(x) {
  x
}

fn$slot_self <- function(object, slot_name) {
  slot(object, slot_name)
}


#=========================================================================#
# horizon ####

fn$year <- function(horizon) {
  data.table(year = horizon@intervals$mid)
}

fn$pYear <- function(horizon) {
  data.table(
    year = horizon@intervals$mid,
    value = as.integer(horizon@intervals$mid)
  )
}

fn$ordYear <- function(horizon) {
  data.table(
    year = horizon@intervals$mid,
    value = as.integer(
      max(horizon@intervals$mid) - min(horizon@intervals$mid) + 1
    )
  )
}

fn$cardYear <- function(horizon) {
  data.table(
    year = horizon@intervals$mid,
    value = as.integer(horizon@intervals$mid - min(horizon@intervals$mid) + 1)
  )
}

fn$pPeriodLen <- function(horizon) {
  data.table(
    year = horizon@intervals$mid,
    value = (horizon@intervals$end - horizon@intervals$start + 1)
  )
}

fn$mMilestoneLast <- function(horizon) {
  data.table(year = max(horizon@intervals$mid))
}

fn$mMilestoneFirst <- function(horizon) {
  data.table(year = min(horizon@intervals$mid))
}

fn$mMilestoneNext <- function(horizon) {
  data.table(
    year = horizon@intervals$mid[-nrow(horizon@intervals)],
    yearp = horizon@intervals$mid[-1])
}

fn$mMilestoneHasNext <- function(horizon) {
  data.table(year = horizon@intervals$mid[-nrow(horizon@intervals)])
}



#==========================================================================#
# calendar ####

fn$pSliceShare <- function(calendar) {
  data.table(
    slice = calendar@slice_share$slice,
    value = calendar@slice_share$share
  )
}

fn$mSliceParentChild <- function(calendar) {
  data.table(
    slice = as.character(calendar@slice_ancestry$parent),
    slicep = as.character(calendar@slice_ancestry$child),
    stringsAsFactors = FALSE
  )
}

fn$mSliceParentChildE <- function(calendar) {
  data.table(
    slice = as.character(c(
      calendar@slice_share$slice,
      calendar@slice_ancestry$parent
      )),
    slicep = as.character(c(
      calendar@slice_share$slice,
      calendar@slice_ancestry$child
      )),
    stringsAsFactors = FALSE
  )
}

fn$mSliceNext <- function(calendar) {
  calendar@next_in_timeframe
}

fn$mSliceFYearNext <- function(calendar) {
  calendar@next_in_year
}

fn$pSliceWeight <- function(calendar) {

  if (!is_null(calendar@misc$pSliceWeight)) {
    # !!! temporary solution for multi-year slice weight - take from misc
    pSliceWeight_tmp <- data.table(
      year = calendar@misc$pSliceWeight$year,
      slice = calendar@misc$pSliceWeight$slice,
      value = calendar@misc$pSliceWeight$weight
    )
    if (is_null(pSliceWeight_tmp[["value"]])) {
      if (is_null(pSliceWeight_tmp[["weight"]])) {
        stop("No slice weight in calendar@misc$pSliceWeight$value or @slice_share$weight")
      }
      pSliceWeight_tmp <- rename(pSliceWeight_tmp, value = weight)
    }
  } else {
    # !!! temporary solution for multi-year slice weight - repeat for each year
    pSliceWeight_tmp <- lapply(year, function(x) {
      data.table(
        year = x,
        slice = calendar@slice_share$slice,
        value = calendar@slice_share$weight
      )
    }) |> rbindlist()
  }

  # calculate slice weights for "parent" timeframes
  # !!! review after introducing multi-year weights
  a <- pSliceWeight_tmp |>
    left_join(calendar@slice_ancestry, by = c("slice" = "child")) |>
    left_join(select(calendar@slice_share, -weight),
              by = c("parent" = "slice")) |>
    rename(weight = value) |>
    filter(!is.na(parent))

  b <- calendar@timetable |>
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
    filter(year %in% mileStoneYears) |>
    unique()

  pSliceWeight_tmp
}

fn$mSameSlice <- function(calendar) {
  data.table(
    slice = calendar@slice_share$slice,
    slicep = calendar@slice_share$slice
  )
}

#==========================================================================#
# settings ####

# pYearFraction - slot_self

mSameRegion <- function(settings) {
  data.table(
    region = settings@region,
    regionp = settings@region
  )
}


#==========================================================================#
# commodity ####
# fn$mUpComm <- function(commodity) {
#   if (commodity@limtype == "UP") {
#     obj@parameters[["mUpComm"]] <-
#       .dat2par(obj@parameters[["mUpComm"]], data.table(comm = cmd@name))
#   } else if (cmd@limtype == "LO") {
#     obj@parameters[["mLoComm"]] <-
#       .dat2par(obj@parameters[["mLoComm"]], data.table(comm = cmd@name))
#   } else if (cmd@limtype == "FX") {
#     obj@parameters[["mFxComm"]] <- .dat2par(obj@parameters[["mFxComm"]],
#                                             data.table(comm = cmd@name))
#   } else {
#     stop("Unknown commodity type: ", cmd@limtype, " in ", cmd@name)
#   }
# }

fn$mCommSlice <- function(commodity, scen) {
  if (is_null(commodity@timeframe)) {
    timeframe <- scen@modInp@sets$comm_timeframe[[commodity@name]]
  } else {
    timeframe <- commodity@timeframe
  }
  data.table(
    comm = commodity@name,
    slice = scen@settings@calendar@timeframes[[timeframe]],
  )
}

#==========================================================================#
# demand ####

fn$mDemComm <- function(demand, ...) {
  data.table(
    dem = demand@name,
    comm = demand@commodity
  )
}

# fn$pDemand <- function(demand, ...) {
#   data.table(
#     dem = demand@name,
#     comm = demand@commodity,
#     region = demand@dem$region,
#     year = demand@dem$year,
#     value = demand@dem$dem
#   )
# }

# fn$mvDemInput <- function(demand, ...) {}


#==========================================================================#
# supply ####


#==========================================================================#
# weather ####



