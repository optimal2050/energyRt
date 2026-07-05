# utopia_profiles.R -- deterministic input profiles for the UTOPIA teaching
# model. Replaces the vignette's old random generators. The saved region-
# agnostic profiles (`utopia_weather`, `utopia_demand`, `utopia_stock`, built by
# data-raw/utopia_data.R) are expanded to a model's regions here; the weather
# capacity factors can also be re-sourced at run time from IDEEA.
#
# Three target resolutions are supported, matching the saved calendars:
#   "utopia_s4h24"  -- 4 seasons x 24 hours (96 slices, the DEFAULT base case;
#                      full diurnal detail so storage cycles), "WIN_h00".
#   "utopia_m12h24" -- 12 months x 24 hours (288 slices, higher resolution),
#                      slices like "m01_h00".
#   "utopia_seasons" -- 4 seasons x 3 dayparts (12 slices), slices like "WIN_DAY".

.utopia_calendars <- c("utopia_s4h24", "utopia_m12h24", "utopia_seasons")

# ---- internal: map an IDEEA d365_h24 slice ("d001_h00") to a target slice -----
.utopia_season_of_month <- function(m) {
  c("WIN", "WIN", "SPR", "SPR", "SPR", "SUM",
    "SUM", "SUM", "AUT", "AUT", "AUT", "WIN")[m]
}
# dayparts: DAY 07-17 (11h), PK 18-20 (evening peak, 3h), NGT 21-06 (10h)
.utopia_daypart_of_hour <- function(h) {
  ifelse(h >= 7 & h <= 17, "DAY", ifelse(h >= 18 & h <= 20, "PK", "NGT"))
}
# Vectorised (yday, hour) -> target slice name for a given calendar.
.utopia_slice_key <- function(yday, hour, calendar) {
  month <- as.integer(format(as.Date(yday - 1, origin = "2019-01-01"), "%m"))
  if (calendar == "utopia_s4h24") {
    sprintf("%s_h%02d", .utopia_season_of_month(month), hour)
  } else if (calendar == "utopia_m12h24") {
    sprintf("m%02d_h%02d", month, hour)
  } else if (calendar == "utopia_seasons") {
    paste(.utopia_season_of_month(month), .utopia_daypart_of_hour(hour), sep = "_")
  } else {
    stop("unsupported calendar '", calendar, "'")
  }
}

# Aggregate an IDEEA d365_h24 weather frame (region, year, slice, wval) to a
# target calendar's slices by averaging the capacity factor.
.utopia_aggregate_cf <- function(w, calendar) {
  w <- as.data.frame(w)
  sl <- as.character(w$slice)                # "d001_h00"
  yday <- as.integer(substr(sl, 2, 4))
  hour <- as.integer(substr(sl, 7, 8))       # after the "h"
  key  <- .utopia_slice_key(yday, hour, calendar)
  agg  <- stats::aggregate(w$wval, by = list(slice = key), FUN = mean, na.rm = TRUE)
  data.frame(slice = agg$slice, wval = agg$x, stringsAsFactors = FALSE)
}

# Pull a representative CF frame for a resource from an IDEEA reg5 element
# (`WSOL`/`WWIN`/`WWIF` are repositories of per-cluster weather objects; `WHYD`
# is a plain weather).
.utopia_get_ideea_cf <- function(x, cluster = 1L) {
  if (methods::is(x, "weather")) {
    return(as.data.frame(x@weather))
  }
  if (methods::is(x, "repository")) {
    ws <- Filter(function(o) methods::is(o, "weather"), x@data)
    if (length(ws) == 0) stop("no weather objects in repository")
    return(as.data.frame(ws[[min(cluster, length(ws))]]@weather))
  }
  stop("unsupported object of class ", class(x)[1])
}

# Re-source the calendar-aggregated weather CFs from IDEEA (used by
# data-raw/utopia_data.R and by `utopia_profiles(source = "ideea")`).
.utopia_weather_from_ideea <- function(
    calendar = "utopia_m12h24",
    resources = c(WSOL = "WSOL", WWIN = "WWIN", WHYD = "WHYD"),
    cluster = 1L) {
  if (!requireNamespace("IDEEA", quietly = TRUE)) {
    stop("Package 'IDEEA' is not installed; use source = 'saved'.")
  }
  reg5 <- IDEEA::ideea_modules$electricity$reg5
  out <- lapply(names(resources), function(res) {
    cf <- .utopia_aggregate_cf(
      .utopia_get_ideea_cf(reg5[[resources[[res]]]], cluster), calendar)
    data.frame(resource = res, cf, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)[, c("resource", "slice", "wval")]
}

#' UTOPIA input profiles (deterministic)
#'
#' Expand the saved, region-agnostic UTOPIA profiles ([utopia_weather],
#' [utopia_demand], [utopia_stock]) to a set of regions for a chosen calendar.
#' Replaces the vignette's former random generators. The weather capacity
#' factors can be re-sourced at run time from IDEEA (`source = "ideea"`);
#' `"saved"` (default) uses the packaged data and never needs an external
#' dataset.
#'
#' @param regions character vector of region names.
#' @param calendar target resolution: `"utopia_s4h24"` (4 seasons x 24 hours, 96
#'   slices, the default base case), `"utopia_m12h24"` (12 months x 24 hours,
#'   288) or `"utopia_seasons"` (4 seasons x 3 dayparts, 12).
#' @param source `"saved"` (packaged data, default) or `"ideea"` (re-aggregate
#'   from `IDEEA::ideea_modules` if installed).
#' @param resources named character vector mapping resource keys (`WSOL`,
#'   `WWIN`, `WHYD`) to IDEEA element names, used when `source = "ideea"`.
#' @param cluster integer, which IDEEA resource cluster to use (`source =
#'   "ideea"`).
#' @param diversify logical (default `TRUE`): scale the solar and wind capacity
#'   factors by deterministic per-region factors (defined for the UTOPIA map
#'   regions `R1`--`R11`; other names get factor 1), so regions have different
#'   renewable endowments -- sunnier south, windier coast. `FALSE` replicates
#'   identical profiles to every region.
#'
#' @return a list of tidy data.frames, each replicated across `regions`:
#'   `weather` (`resource`, `region`, `slice`, `wval`), `demand` (`region`,
#'   `slice`, `load` -- a relative load shape) and `stock` (`region`, `tech`,
#'   `gw` -- base-year capacity).
#' @seealso [utopia_weather], [utopia_demand], [utopia_stock], [calendars]
#' @export
utopia_profiles <- function(regions,
                            calendar = c("utopia_s4h24", "utopia_m12h24",
                                         "utopia_seasons"),
                            source = c("saved", "ideea"),
                            resources = c(WSOL = "WSOL", WWIN = "WWIN",
                                          WHYD = "WHYD"),
                            cluster = 1L,
                            diversify = TRUE) {
  calendar <- match.arg(calendar)
  source <- match.arg(source)
  stopifnot(is.character(regions), length(regions) > 0)

  wx <- if (source == "ideea") {
    .utopia_weather_from_ideea(calendar, resources, cluster)
  } else {
    w <- as.data.frame(utopia_weather)
    w[w$calendar == calendar, c("resource", "slice", "wval")]
  }
  d <- as.data.frame(utopia_demand)
  dx <- d[d$calendar == calendar, c("slice", "load")]
  sx <- as.data.frame(utopia_stock)

  # replicate each region-agnostic profile across the requested regions
  rep_reg <- function(df) {
    do.call(rbind, lapply(regions, function(r) {
      cbind(region = r, df, stringsAsFactors = FALSE, row.names = NULL)
    }))
  }
  weather <- rep_reg(wx)[, c("resource", "region", "slice", "wval")]

  # deterministic regional endowments: sunnier south, windier coast (UTOPIA map
  # regions R1-R11; unknown region names keep factor 1)
  if (isTRUE(diversify)) {
    sol_f <- c(R1 = 1.15, R2 = 1.00, R3 = 0.90, R4 = 1.10, R5 = 0.95,
               R6 = 0.85, R7 = 1.05, R8 = 0.90, R9 = 0.80, R10 = 1.20,
               R11 = 1.00)
    win_f <- c(R1 = 0.85, R2 = 1.15, R3 = 1.05, R4 = 0.90, R5 = 1.10,
               R6 = 1.20, R7 = 0.95, R8 = 1.05, R9 = 1.15, R10 = 0.80,
               R11 = 1.00)
    f <- rep(1, nrow(weather))
    i_sol <- weather$resource == "WSOL"
    i_win <- weather$resource == "WWIN"
    f[i_sol] <- ifelse(is.na(sol_f[weather$region[i_sol]]), 1,
                       sol_f[weather$region[i_sol]])
    f[i_win] <- ifelse(is.na(win_f[weather$region[i_win]]), 1,
                       win_f[weather$region[i_win]])
    weather$wval <- pmin(1, weather$wval * f)
  }

  list(
    weather = weather,
    demand  = rep_reg(dx)[, c("region", "slice", "load")],
    stock   = rep_reg(sx)[, c("region", "tech", "gw")]
  )
}
