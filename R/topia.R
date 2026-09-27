# Dataset documentation for the TOPIA teaching model.

#' The TOPIA reference dataset
#'
#' Everything the TOPIA teaching model needs, in one list: the maps of the
#' imaginary country "Topia", the deterministic weather / demand / stock
#' profiles, and the kit of ready energyRt building blocks and scenario levers.
#' Built by `data-raw/topia_maps.R`, `topia_data.R` and `topia_modules.R`,
#' folded together by `data-raw/topia_assemble.R`.
#'
#' The profile tables are region-agnostic; expand them to a model's regions
#' with [topia_profiles()]. For synthetic shapes on any calendar (rather than
#' TOPIA's saved curves) use [topia_profile()].
#'
#' @format A named list of five elements:
#' \describe{
#'   \item{geoscales}{named list of four [geoscales::Geoscale] objects
#'     (`squares`, `honeycomb`, `island`, `continent`). Each carries the same
#'     hierarchy -- `nation TOPIA -> zone WEST/CENTRAL/EAST -> region` over
#'     `W1`, `W2`, `C1`..`C6`, `E1`..`E3` -- and that layout's polygons, plus a
#'     planar `area` weight. This is the dataset's ONLY copy of the geometry:
#'     the plain `topia$map` and the `topia$geo` hierarchy table were removed in
#'     0.91. Get either back with `geoscales::geoscale_geometry(gs, "region")`
#'     and `geoscales::geoscale_leaftable(gs)`. Every layout draws the same
#'     eleven regions in different places, and each assigns the codes so that a
#'     zone is one contiguous block on that picture.}
#'   \item{weather}{deterministic solar / wind / hydro capacity factors for the
#'     three teaching calendars. Columns `calendar`
#'     (`s4_h24`/`m12_h24`/`topia_seasons`), `resource`
#'     (`WSOL`/`WWIN`/`WHYD`), `timeslice` (e.g. `SUM_h12`, `m06_h12` or
#'     `SUM_DAY`) and `wval` (capacity factor, 0-1). Attribute `source` records
#'     whether it came from IDEEA reanalysis or the curated fallback.}
#'   \item{demand}{a relative electricity-load shape by timeslice, for the same
#'     three calendars. Columns `calendar`, `timeslice` and `load` (relative,
#'     mean ~1); scale it by a region's annual demand and the timeslice shares
#'     to get energy per timeslice.}
#'   \item{stock}{deterministic base-year installed capacity per technology.
#'     Columns `tech` and `gw`.}
#'   \item{modules}{the kit -- see below.}
#' }
#'
#' @section The `modules` kit:
#' Mirrors the structure of `IDEEA::ideea_modules`. Assemble a model from a
#' chosen region layout, solve it, and layer the levers to run scenarios (see
#' the `topia-use` vignette).
#' \describe{
#'   \item{info}{a description string.}
#'   \item{calendars}{the TOPIA calendars (`annual`/`topia_seasons`/
#'     `s4_h24`/`m12_h24`) plus the symmetric unit calendars
#'     (`unit_s4`, 4 equal seasons; `unit_s4h4`, 4x4). Note `annual`
#'     cannot drive the shipped kits (`ELC` is balanced at the `HOUR` level
#'     and [topia_profiles()] has no annual shapes); it ships as the
#'     coarsest example calendar.}
#'   \item{horizons}{planning horizons (`base` = 2020/2030/2040/2050;
#'     `unit` = the single year 2025 for the unit kits).}
#'   \item{electricity}{per region layout (`R1`, `R3`, `R7`, `R11` -- an
#'     n-region layout is keyed `R<n>` and takes the first n regions of the
#'     canonical west-to-east order `W1`, `W2`, `C1`..`C6`, `E1`..`E3`),
#'     each a "kit":
#'     `repo` (the base repository), the individual blocks (`repo_comm`,
#'     `repo_supply`, `DEM_ELC`, `WSOL`/`WWIN`/`WHYD`, the technologies,
#'     `STG_ELC`), the scenario levers `CO2_CAP`, `CT_CO2`, `RES_SHARE`,
#'     `NO_NEW_NUC`, `EARLY_RET`, and the add-on modules -- re-declared
#'     objects that REPLACE their base counterpart when added with
#'     `add(mod, ., overwrite = TRUE)`: `GAS_CURVE` (domestic gas as a
#'     3-step [asSupplyCurve()], absent in the `R1` kit, whose single region
#'     has no gas),
#'     `EWIN_SITES` (wind in two site-grade clusters, finite GOOD +
#'     down-rated POOR), `ENUC_VINT` (nuclear in two build vintages).}
#'   \item{unit}{the "unit model" kits (`U1`, `U3`): every input is 1 on the
#'     symmetric `unit_s4` calendar, single-year `unit` horizon,
#'     `discount = 0`, so each variant's objective is a small hand-checkable
#'     integer -- base `U1` solves to exactly 8, `U3` (trade chain from the
#'     `W1` endowment) to 36, `SUP_CURVE` (2-step unit supply curve
#'     replacing the flat supply) to 10, the self-contained `SOLAR`
#'     repository (on/off resource from [topia_profile()] + storage
#'     bridging) to 10. The arithmetic is written out in
#'     `data-raw/topia_modules.R` and pinned by `test-unit-model.R`.}
#' }
#'
#' @seealso [topia_profiles()], [topia_profile()],
#'   [calendars], [horizons], [asSupplyCurve()], the TOPIA vignettes
#' @family topia
#' @examples
#' names(topia)
#' names(topia$geoscales)
#' head(topia$weather)
#' topia$stock
#'
#' \dontrun{
#' gs <- topia$geoscales$honeycomb          # hierarchy + that layout's polygons
#' geoscales::geoscale_leaftable(gs)        # the region table
#' plot_geoscale(gs, type = "map")
#'
#' um <- topia$modules$electricity$R3
#' mod <- newModel("TOPIA", data = um$repo,
#'                 calendar = topia$modules$calendars$s4_h24,
#'                 region = um$regions,
#'                 horizon = topia$modules$horizons$base,
#'                 discount = 0.05)
#' scen <- solve_scenario(interpolate_model(mod, "BASE"),
#'                        solver = solver_options$glpk)
#'
#' # the unit model: hand-checkable integer objective (8)
#' uk <- topia$modules$unit$U1
#' umod <- newModel("UNIT", data = uk$repo,
#'                  calendar = topia$modules$unit$calendars$unit_s4,
#'                  region = uk$regions,
#'                  horizon = topia$modules$horizons$unit, discount = 0)
#' }
"topia"


# ---------------------------------------------------------------------------
# (was R/topia-geoscale.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# (was R/topia_profiles.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# topia_profiles.R -- deterministic input profiles for the TOPIA teaching
# model. Replaces the vignette's old random generators. The saved region-
# agnostic profiles (`topia_weather`, `topia_demand`, `topia_stock`, built by
# data-raw/topia_data.R) are expanded to a model's regions here; the weather
# capacity factors can also be re-sourced at run time from IDEEA.
#
# Three target resolutions are supported, matching the saved calendars:
#   "s4_h24"  -- 4 seasons x 24 hours (96 timeslices, the DEFAULT base case;
#                      full diurnal detail so storage cycles), "WIN_h00".
#   "m12_h24" -- 12 months x 24 hours (288 timeslices, higher resolution),
#                      timeslices like "m01_h00".
#   "topia_seasons" -- 4 seasons x 3 dayparts (12 timeslices), timeslices like "WIN_DAY".

.topia_calendars <- c("s4_h24", "m12_h24", "topia_seasons")

# ---- internal: map an IDEEA d365_h24 timeslice ("d001_h00") to a target timeslice -----
.topia_season_of_month <- function(m) {
  c("WIN", "WIN", "SPR", "SPR", "SPR", "SUM",
    "SUM", "SUM", "FAL", "FAL", "FAL", "WIN")[m]
}
# dayparts: DAY 07-17 (11h), PK 18-20 (evening peak, 3h), NGT 21-06 (10h)
.topia_daypart_of_hour <- function(h) {
  ifelse(h >= 7 & h <= 17, "DAY", ifelse(h >= 18 & h <= 20, "PK", "NGT"))
}
# Vectorised (yday, hour) -> target timeslice name for a given calendar.
.topia_timeslice_key <- function(yday, hour, calendar) {
  month <- as.integer(format(as.Date(yday - 1, origin = "2019-01-01"), "%m"))
  if (calendar == "s4_h24") {
    sprintf("%s_h%02d", .topia_season_of_month(month), hour)
  } else if (calendar == "m12_h24") {
    sprintf("m%02d_h%02d", month, hour)
  } else if (calendar == "topia_seasons") {
    paste(.topia_season_of_month(month), .topia_daypart_of_hour(hour), sep = "_")
  } else {
    stop("unsupported calendar '", calendar, "'")
  }
}

# Aggregate an IDEEA d365_h24 weather frame (region, year, timeslice, wval) to a
# target calendar's timeslices by averaging the capacity factor.
.topia_aggregate_cf <- function(w, calendar) {
  w <- as.data.frame(w)
  # IDEEA (external) data may still carry the pre-v0.80 `slice` column
  names(w) <- .rename_slice_compat(names(w), "IDEEA weather data")
  sl <- as.character(w$timeslice)                # "d001_h00"
  yday <- as.integer(substr(sl, 2, 4))
  hour <- as.integer(substr(sl, 7, 8))       # after the "h"
  key  <- .topia_timeslice_key(yday, hour, calendar)
  agg  <- stats::aggregate(w$wval, by = list(timeslice = key), FUN = mean, na.rm = TRUE)
  data.frame(timeslice = agg$timeslice, wval = agg$x, stringsAsFactors = FALSE)
}

# Pull a representative CF frame for a resource from an IDEEA reg5 element
# (`WSOL`/`WWIN`/`WWIF` are repositories of per-cluster weather objects; `WHYD`
# is a plain weather).
.topia_get_ideea_cf <- function(x, cluster = 1L) {
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
# data-raw/topia_data.R and by `topia_profiles(source = "ideea")`).
.topia_weather_from_ideea <- function(
    calendar = "m12_h24",
    resources = c(WSOL = "WSOL", WWIN = "WWIN", WHYD = "WHYD"),
    cluster = 1L) {
  if (!requireNamespace("IDEEA", quietly = TRUE)) {
    stop("Package 'IDEEA' is not installed; use source = 'saved'.")
  }
  reg5 <- IDEEA::ideea_modules$electricity$reg5
  out <- lapply(names(resources), function(res) {
    cf <- .topia_aggregate_cf(
      .topia_get_ideea_cf(reg5[[resources[[res]]]], cluster), calendar)
    data.frame(resource = res, cf, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)[, c("resource", "timeslice", "wval")]
}

#' TOPIA input profiles (deterministic)
#'
#' Expand TOPIA's saved, region-agnostic profiles (`topia$weather`,
#' `topia$demand`, `topia$stock`) to a set of regions for a chosen calendar,
#' returning a list of three tidy data.frames. The weather capacity factors can
#' be re-sourced at run time from IDEEA (`source = "ideea"`); `"saved"`
#' (default) uses the packaged data and never needs an external dataset.
#'
#' Not to be confused with the near-namesake [topia_profile()], which takes no
#' TOPIA data at all: it generates a synthetic step / sine / cosine / hex shape
#' over any calendar and returns a single data.frame.
#'
#' @param regions character vector of region names.
#' @param calendar target resolution: `"s4_h24"` (4 seasons x 24 hours, 96
#'   timeslices, the default base case), `"m12_h24"` (12 months x 24 hours,
#'   288) or `"topia_seasons"` (4 seasons x 3 dayparts, 12).
#' @param source `"saved"` (packaged data, default) or `"ideea"` (re-aggregate
#'   from `IDEEA::ideea_modules` if installed).
#' @param resources named character vector mapping resource keys (`WSOL`,
#'   `WWIN`, `WHYD`) to IDEEA element names, used when `source = "ideea"`.
#' @param cluster integer, which IDEEA resource cluster to use (`source =
#'   "ideea"`).
#' @param diversify logical (default `TRUE`): scale the solar and wind capacity
#'   factors by deterministic per-region factors (defined for the TOPIA map
#'   regions `W1`, `W2`, `C1`--`C6`, `E1`--`E3`; other names get factor 1), so regions have different
#'   renewable endowments -- sunnier south, windier coast. `FALSE` replicates
#'   identical profiles to every region.
#'
#' @return a list of tidy data.frames, each replicated across `regions`:
#'   `weather` (`resource`, `region`, `timeslice`, `wval`), `demand` (`region`,
#'   `timeslice`, `load` -- a relative load shape) and `stock` (`region`, `tech`,
#'   `gw` -- base-year capacity).
#' @seealso [topia], [topia_profile()], [calendars]
#' @family topia
#' @export
topia_profiles <- function(regions,
                            calendar = c("s4_h24", "m12_h24",
                                         "topia_seasons"),
                            source = c("saved", "ideea"),
                            resources = c(WSOL = "WSOL", WWIN = "WWIN",
                                          WHYD = "WHYD"),
                            cluster = 1L,
                            diversify = TRUE) {
  calendar <- match.arg(calendar)
  source <- match.arg(source)
  stopifnot(is.character(regions), length(regions) > 0)

  wx <- if (source == "ideea") {
    .topia_weather_from_ideea(calendar, resources, cluster)
  } else {
    w <- as.data.frame(topia$weather)
    w[w$calendar == calendar, c("resource", "timeslice", "wval")]
  }
  d <- as.data.frame(topia$demand)
  dx <- d[d$calendar == calendar, c("timeslice", "load")]
  sx <- as.data.frame(topia$stock)

  # replicate each region-agnostic profile across the requested regions
  rep_reg <- function(df) {
    do.call(rbind, lapply(regions, function(r) {
      cbind(region = r, df, stringsAsFactors = FALSE, row.names = NULL)
    }))
  }
  weather <- rep_reg(wx)[, c("resource", "region", "timeslice", "wval")]

  # deterministic regional endowments: sunnier south, windier coast. Keyed by
  # the TOPIA region codes in their canonical west-to-east order (W1, W2,
  # C1..C6, E1..E3); each factor kept the position it had before the regions
  # were renamed. Unknown region names keep factor 1.
  if (isTRUE(diversify)) {
    sol_f <- c(W1 = 1.15, W2 = 1.00, C1 = 0.90, C2 = 1.10, C3 = 0.95,
               C4 = 0.85, C5 = 1.05, C6 = 0.90, E1 = 0.80, E2 = 1.20,
               E3 = 1.00)
    win_f <- c(W1 = 0.85, W2 = 1.15, C1 = 1.05, C2 = 0.90, C3 = 1.10,
               C4 = 1.20, C5 = 0.95, C6 = 1.05, E1 = 1.15, E2 = 0.80,
               E3 = 1.00)
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
    demand  = rep_reg(dx)[, c("region", "timeslice", "load")],
    stock   = rep_reg(sx)[, c("region", "tech", "gw")]
  )
}


# ---------------------------------------------------------------------------
# (was R/topia_shapes.R, merged 2026-09-20)
# ---------------------------------------------------------------------------

# topia_shapes.R -- deterministic SYNTHETIC input shapes for teaching and
# testing, the geometric counterpart of the realistic curves in
# topia_profiles.R. One generator, four shapes:
#
#   step    -- ascending staircase 0, 1/(n-1), ..., 1 (n = `levels`): the
#              piecewise-constant family (levels = 2 is on/off -- the canonical
#              storage case: the resource is free in half the slices and absent
#              in the other half);
#   sine    -- smooth wave, sampled at each timeslice's midpoint;
#   cosine  -- the same wave shifted a quarter period;
#   hex     -- trapezoid ramp / plateau / ramp in equal thirds (the honeycomb
#              silhouette): piecewise-LINEAR between the constant and smooth
#              families.
#
# Unlike topia_profiles() (locked to the three shipped realistic calendars),
# these work on ANY calendar: slice positions come from the calendar's own
# chronological chain (`@next_in_year` / `@next_in_timeframe`) and widths from
# `@timeslice_share`, so unequal slices land where they belong.

# Resolve a calendar argument: a `calendar` object, a shipped calendar's name,
# or a timetable data.frame (built into a calendar on the fly).
.shape_calendar <- function(calendar) {
  if (methods::is(calendar, "calendar")) return(calendar)
  if (is.character(calendar) && length(calendar) == 1) {
    cal <- energyRt::calendars[[calendar]]
    if (is.null(cal)) {
      stop("unknown calendar '", calendar, "'; shipped names: ",
           paste(names(energyRt::calendars), collapse = ", "),
           ". Pass a calendar object or a timetable data.frame instead.",
           call. = FALSE)
    }
    return(cal)
  }
  if (is.data.frame(calendar)) {
    return(newCalendar(timetable = data.table::as.data.table(calendar)))
  }
  stop("`calendar` must be a calendar object, a shipped calendar name, ",
       "or a timetable data.frame.", call. = FALSE)
}

# Chronologically ordered finest-level timeslices of a calendar. The cycle has
# no canonical start (storage wraps around), so the walk deterministically
# starts at the alphabetically first slice; `phase` rotates the shape when a
# different anchor is wanted.
.shape_slices <- function(cal) {
  lvl <- cal@default_timeframe
  sl <- as.character(cal@timeframes[[lvl]])
  if (length(sl) <= 1L) return(sl)
  nxt <- as.data.frame(cal@next_in_year)
  chain <- stats::setNames(as.character(nxt$timeslicep),
                           as.character(nxt$timeslice))
  chain <- chain[names(chain) %in% sl]
  out <- character(length(sl))
  out[1] <- sort(sl)[1]
  for (i in seq_len(length(sl) - 1L)) {
    out[i + 1L] <- chain[[out[i]]]
    if (is.na(out[i + 1L])) {
      stop("calendar's next_in_year chain is broken at '", out[i], "'",
           call. = FALSE)
    }
  }
  out
}

# Midpoint positions in [0, 1) for ordered slices with (possibly unequal)
# shares: x_i = (share before i + share_i / 2) / total.
.shape_positions <- function(shares) {
  tot <- sum(shares)
  (cumsum(shares) - shares / 2) / tot
}

# Evaluate one shape at positions x in [0, 1). Returns list(value in [0, 1],
# step = integer index or NA).
.shape_eval <- function(shape, x, levels) {
  x <- x %% 1
  switch(shape,
    step = {
      k <- pmin(levels - 1L, as.integer(floor(x * levels)))
      list(value = if (levels > 1L) k / (levels - 1L) else rep(1, length(x)),
           step = k)
    },
    sine   = list(value = (sin(2 * pi * x) + 1) / 2, step = rep(NA_integer_, length(x))),
    cosine = list(value = (cos(2 * pi * x) + 1) / 2, step = rep(NA_integer_, length(x))),
    hex = {
      v <- ifelse(x < 1 / 3, 3 * x, ifelse(x < 2 / 3, 1, 3 * (1 - x)))
      list(value = v, step = rep(NA_integer_, length(x)))
    },
    stop("unknown shape '", shape, "'", call. = FALSE)
  )
}

#' Synthetic input shapes on a calendar (deterministic)
#'
#' Generate a simple geometric profile -- a step-wise staircase, a sine/cosine
#' wave, or a hexagonal trapezoid -- over the timeslices of a calendar, ready
#' to use as weather (`wval`), availability multipliers, or demand shapes.
#' The synthetic counterpart of the realistic [topia_profiles()]: where those
#' answer "what does a year look like", these answer "what is the simplest
#' input that isolates one mechanism" (storage bridging an on/off resource,
#' techs following a ramp, trade smoothing opposite phases).
#'
#' Despite the shared prefix, this function reads no TOPIA data and is not
#' limited to TOPIA: it works on any calendar and returns a single data.frame.
#' [topia_profiles()] (plural) is the other one -- it expands TOPIA's saved
#' weather / demand / stock to regions and returns a list of three.
#'
#' The shape spans `period` once: `"year"` stretches it over the whole
#' calendar; `"frame"` repeats it inside each parent timeframe (e.g. each
#' season's hours get the same diurnal shape). Slice positions are the
#' midpoints of each timeslice's `share` along the calendar's own
#' chronological chain, so unequal slices land where they belong; the cycle's
#' anchor is the alphabetically first timeslice (rotate with `phase`).
#'
#' @param shape `"step"` (ascending staircase with `levels` plateaus:
#'   0, 1/(levels-1), ..., 1), `"sine"`, `"cosine"`, or `"hex"` (trapezoid
#'   ramp/plateau/ramp in equal thirds).
#' @param levels integer >= 2, number of plateaus for `shape = "step"`
#'   (ignored otherwise). `levels = 2` is the on/off case.
#' @param calendar a [calendar] object, the name of a shipped calendar (see
#'   [calendars]), or a timetable `data.frame`.
#' @param regions optional character vector. `NULL` returns the bare shape
#'   (`timeslice`, `step`, `value`); otherwise the shape is replicated per
#'   region with a leading `region` column.
#' @param vary per-region variation (needs `regions`): `"none"` replicates
#'   identically; `"phase"` shifts region i by `(i-1)/n` of the period (a
#'   gradient across the map -- neighbouring regions peak in sequence);
#'   `"amplitude"` scales region i's amplitude by `1 - (i-1)/(2(n-1))`
#'   (from 1 down to 1/2 -- good sites to poor sites).
#' @param min,max numeric range the unit shape is scaled to
#'   (`value = min + shape * (max - min)`).
#' @param period `"year"` (one cycle over the calendar) or `"frame"` (one
#'   cycle inside each parent timeframe of the finest level).
#' @param phase numeric, fraction of the period the shape is shifted by
#'   (applied to all regions, on top of `vary = "phase"` offsets).
#'
#' @return A tidy `data.frame`, chronologically ordered: `region` (only when
#'   `regions` is given), `timeslice` (the calendar's finest level), `step`
#'   (integer plateau index `0..levels-1` for `"step"`, `NA` otherwise -- the
#'   pretty-number handle for hand-checkable inputs), and `value` in
#'   `[min, max]`.
#'
#' @examples
#' # on/off resource on the 4-slice test calendar: free half the year
#' topia_profile("step", levels = 2, calendar = "topia_seasons")
#'
#' # a diurnal sine for every season, phased across three regions
#' head(topia_profile("sine", calendar = "s4_h24",
#'                     regions = c("W1", "W2", "C1"), vary = "phase",
#'                     period = "frame"))
#' @seealso [topia_profiles()] for the realistic TOPIA curves, [topia] for
#'   the reference dataset and the teaching kits.
#' @family topia
#' @export
topia_profile <- function(shape = c("step", "sine", "cosine", "hex"),
                           levels = 4L,
                           calendar = "s4_h24",
                           regions = NULL,
                           vary = c("none", "phase", "amplitude"),
                           min = 0, max = 1,
                           period = c("year", "frame"),
                           phase = 0) {
  shape <- match.arg(shape)
  vary <- match.arg(vary)
  period <- match.arg(period)
  levels <- as.integer(levels)
  stopifnot(length(levels) == 1L, levels >= 2L || shape != "step",
            is.numeric(min), is.numeric(max), max >= min,
            is.numeric(phase), length(phase) == 1L)
  if (!is.null(regions)) stopifnot(is.character(regions), length(regions) > 0)
  if (vary != "none" && is.null(regions)) {
    stop("`vary = \"", vary, "\"` needs `regions`.", call. = FALSE)
  }

  cal <- .shape_calendar(calendar)
  sl <- .shape_slices(cal)
  shr <- as.data.frame(cal@timeslice_share)
  shares <- shr$share[match(sl, as.character(shr$timeslice))]

  if (period == "year" || length(cal@timeframes) < 3L) {
    x <- .shape_positions(shares)
  } else {
    # one cycle per parent frame: positions restart inside each parent,
    # in chain order (sl is already chronological, so parents are contiguous)
    anc <- as.data.frame(cal@timeslice_ancestry)
    parent <- as.character(anc$parent)[match(sl, as.character(anc$child))]
    x <- numeric(length(sl))
    for (p in unique(parent)) {
      i <- which(parent == p)
      x[i] <- .shape_positions(shares[i])
    }
  }

  one <- function(region, extra_phase, amp) {
    ev <- .shape_eval(shape, x - phase - extra_phase, levels)
    df <- data.frame(timeslice = sl, step = ev$step,
                     value = min + ev$value * amp * (max - min),
                     stringsAsFactors = FALSE)
    if (!is.null(region)) df <- cbind(region = region, df, stringsAsFactors = FALSE)
    df
  }

  if (is.null(regions)) return(one(NULL, 0, 1))
  n <- length(regions)
  out <- lapply(seq_len(n), function(i) {
    one(regions[i],
        extra_phase = if (vary == "phase") (i - 1) / n else 0,
        amp = if (vary == "amplitude" && n > 1) 1 - (i - 1) / (2 * (n - 1)) else 1)
  })
  do.call(rbind, out)
}
