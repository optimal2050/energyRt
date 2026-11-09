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
#'   \item{geoscales}{named list of four [geoscales::GeoScale] objects
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
#'   \item{horizons}{planning horizons, named for their MILESTONES rather
#'     than the period end, with `BY` marking a one-year base year at the
#'     front. `BY2025_50_by10` = milestones 2025/2030/2040/2050, a base year
#'     then 10-year intervals (its periods run to 2055). Each milestone is the
#'     MID-point of its period, so that is where its costs are discounted
#'     from. `BY2025` = the base year alone, for the unit kits.}
#'   \item{electricity}{THE model, over all eleven regions `W1`, `W2`,
#'     `C1`..`C6`, `E1`..`E3`: `repo` (the repository), `regions`, `calendar`
#'     (the name of the calendar it was built for), `geoscale` (its
#'     `nation -> zone -> region` hierarchy, without geometry -- a model MUST
#'     carry it, see the examples), and the scenario levers `CO2_CAP`,
#'     `CT_CO2`, `EARLY_RET` and `GREEN_H2`, which are NOT in `repo` and
#'     are added to a scenario on top of it. Built by
#'     `data-raw/topia_model.R`, which
#'     tangles `vignettes/topia-build.Rmd` -- the vignette is the source, so
#'     what it documents and what ships cannot drift apart. Smaller cases come
#'     from [subset_model_regions()], not from a saved layout.
#'
#'   The resources are CONCENTRATED, which is what drives trade: `COA` and
#'   `GAS` are balanced per ZONE (`commodity@geoframe`) and traded between
#'   zones on `TBD_COA_*` / `TBD_GAS_*` corridors, with finite `ava.up`
#'   endowments -- coal cheap and plentiful in `WEST`, gas in `EAST` --
#'   above which `IMP_COA` / `IMP_GAS` are the unbounded backstop.
#'   Electricity is balanced and traded at the region level (`TBD_ELC_*`).
#'   Reservoir hydro sits in `C4` (12 GW), `E1` (6) and `C1` (3), and the
#'   single nuclear station in `C1`. Costs are USD (MUSD/GW, MUSD/PJ).}
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
#' um <- topia$modules$electricity
#' mod <- newModel("TOPIA", data = um$repo,
#'                 calendar = topia$modules$calendars$s4_h24,
#'                 region = um$regions,
#'                 horizon = topia$modules$horizons$BY2025_50_by10,
#'                 discount = 0.05)
#' mod <- setGeoscale(mod, um$geoscale)
#' scen <- solve_scenario(interpolate_model(mod, "BASE"),
#'                        solver = solver_options$glpk)
#'
#' # the unit model: hand-checkable integer objective (8)
#' uk <- topia$modules$unit$U1
#' umod <- newModel("UNIT", data = uk$repo,
#'                  calendar = topia$modules$unit$calendars$unit_s4,
#'                  region = uk$regions,
#'                  horizon = topia$modules$horizons$BY2025, discount = 0)
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

.topia_calendars <- c("s4_h24", "m12_h24", "topia_seasons", "d365_h24")

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

.TOPIA_DATASETS <- c("simulated", "sample")

# The kit's data, or the tables a rebuild is in the middle of making.
# `data-raw/topia_modules.R` builds its repositories THROUGH topia_profiles(),
# so without this seam regenerating `data/topia.rda` would read the previously
# shipped copy of the data it is replacing. The option is set by
# `data-raw/topia_data.R` and cleared by `data-raw/topia_assemble.R`.
.topia_ds <- function() {
  ov <- getOption("energyRt.topia_data", NULL)
  if (is.null(ov)) energyRt::topia else ov
}

# region -> zone -> nation, from the kit's own geoscale table
.topia_geo_map <- function() {
  g <- as.data.frame(.topia_ds()$geoscales)
  nm <- c("region", "zone", "nation")
  pre <- sub("\\.region$", "", grep("\\.region$", names(g), value = TRUE)[1])
  out <- g[, paste(pre, nm, sep = "."), drop = FALSE]
  names(out) <- nm
  out <- out[!is.na(out$region) & !duplicated(out$region), ]
  out[] <- lapply(out, as.character)
  out
}

# Which base regions does a requested code stand for?
.topia_members <- function(code, gm) {
  if (code %in% gm$region) return(code)
  if (code %in% gm$zone) return(gm$region[gm$zone == code])
  if (code %in% gm$nation) return(gm$region[gm$nation == code])
  NULL
}

# Average a per-region table over the members of each requested code.
.topia_lookup <- function(x, regions, value, gm) {
  parts <- lapply(regions, function(r) {
    mem <- .topia_members(r, gm)
    if (is.null(mem)) {
      stop("no TOPIA profile for region '", r, "'. Known codes: ",
           paste(c(unique(gm$region), unique(gm$zone), unique(gm$nation)),
                 collapse = ", "), call. = FALSE)
    }
    v <- x[x$region %in% mem, ]
    # A region carries a resource GRADE only where its own ground reaches it,
    # so a (region, cluster) pair with no series is absent rather than zero:
    # returning nothing keeps it out of the model, while aggregating an empty
    # frame would stop the build.
    if (nrow(v) == 0L) return(NULL)
    agg <- stats::aggregate(v[[value]], by = list(timeslice = v$timeslice),
                            FUN = mean)
    d <- data.frame(region = r, timeslice = as.character(agg$timeslice),
                    v = agg$x, stringsAsFactors = FALSE)
    names(d)[names(d) == "v"] <- value
    d
  })
  do.call(rbind, parts)
}

# Which dataset serves a calendar when the caller does not say?
.topia_default_dataset <- function(calendar) {
  if (identical(calendar, "d365_h24")) "sample" else "simulated"
}

.topia_pick <- function(x, calendar, dataset, what) {
  v <- x[x$calendar == calendar & x$dataset == dataset, ]
  if (nrow(v) == 0) {
    have <- unique(as.character(x$calendar[x$dataset == dataset]))
    stop("TOPIA's '", dataset, "' ", what, " is not built for calendar '",
         calendar, "'. It covers: ", paste(have, collapse = ", "),
         ". Pass `dataset =` to choose the other set.", call. = FALSE)
  }
  v
}

#' TOPIA input profiles
#'
#' Expand TOPIA's per-region profiles (`topia$weather`, `topia$demand`,
#' `topia$stock`) to a set of regions for a chosen calendar, returning a list
#' of three tidy data.frames.
#'
#' Two input datasets ship and the caller chooses between them:
#'
#' \describe{
#'   \item{`"simulated"`}{closed-form shapes on the small calendars
#'     (`s4_h24`, `m12_h24`, `topia_seasons`). Demonstration data: it carries a
#'     spatial gradient and a diurnal structure, and is not a resource estimate
#'     for anywhere.}
#'   \item{`"sample"`}{a measured hourly set on `d365_h24`. Multi-day calm
#'     spells survive only at that resolution, so it is where flexibility has
#'     something to earn -- reducing the hourly series to a handful of typical
#'     days keeps about a quarter of the wind's variance and none of its calm
#'     spells, which is why the small calendars are served by the simulated set
#'     instead.}
#' }
#'
#' A request for a coarser geoscale code -- a zone, or the nation -- averages
#' its member regions, which is what a copperplate zone sees.
#'
#' Not to be confused with the near-namesake [topia_profile()], which takes no
#' TOPIA data at all: it generates a synthetic step / sine / cosine / hex shape
#' over any calendar and returns a single data.frame.
#'
#' @param regions character vector of region codes: a region of the TOPIA map
#'   (`W1`, `W2`, `C1`--`C6`, `E1`--`E3`), a zone (`WEST`, `CENTRAL`, `EAST`)
#'   or the nation (`TOPIA`). An unknown code is an error that lists what is
#'   known.
#' @param calendar target resolution: `"s4_h24"` (4 seasons x 24 hours, 96
#'   timeslices, the default base case), `"m12_h24"` (12 months x 24 hours,
#'   288), `"topia_seasons"` (4 seasons x 3 dayparts, 12) or `"d365_h24"`
#'   (8760).
#' @param dataset `"simulated"` or `"sample"`. `NULL` (the default) picks the
#'   set built for `calendar` -- the sample for `"d365_h24"`, the simulated set
#'   otherwise. Naming a pair that was not built is an error saying what each
#'   set covers.
#' @param source `"saved"` (packaged data, default) or `"ideea"` (re-aggregate
#'   from `IDEEA::ideea_modules` if installed). The IDEEA route yields one
#'   region-agnostic shape per resource and replicates it to every region.
#' @param resources named character vector mapping resource keys (`WSOL`,
#'   `WWIN`, `WHYD`) to IDEEA element names, used when `source = "ideea"`.
#' @param cluster integer, which IDEEA resource cluster to use (`source =
#'   "ideea"`).
#' @param diversify defunct. TOPIA's profiles are per region and are not scaled
#'   copies of one shape, so there is nothing to diversify; passing it warns
#'   and changes nothing.
#'
#' @return a list of tidy data.frames: `weather` (`resource`, `region`,
#'   `timeslice`, `wval`), `demand` (`region`, `timeslice`, `load` -- a relative
#'   load shape) and `stock` (`region`, `tech`, `gw` -- base-year capacity,
#'   replicated across `regions`).
#' @seealso [topia], [topia_profile()], [calendars]
#' @family topia
#' @export
#' @examples
#' # weather, demand and stock for two regions of the TOPIA map
#' prof <- topia_profiles(c("W1", "W2"), calendar = "s4_h24")
#' head(prof$weather, 3)
topia_profiles <- function(regions,
                           calendar = c("s4_h24", "m12_h24", "topia_seasons",
                                        "d365_h24"),
                           dataset = NULL,
                           source = c("saved", "ideea"),
                           resources = c(WSOL = "WSOL", WWIN = "WWIN",
                                         WHYD = "WHYD"),
                           cluster = 1L,
                           diversify = NULL) {
  calendar <- match.arg(calendar)
  source <- match.arg(source)
  dataset <- if (is.null(dataset)) {
    .topia_default_dataset(calendar)
  } else {
    match.arg(dataset, .TOPIA_DATASETS)
  }
  stopifnot(is.character(regions), length(regions) > 0)
  if (!is.null(diversify)) {
    warning("`diversify` is defunct: TOPIA's profiles are per region and are ",
            "not scaled copies of one shape. The argument is ignored.",
            call. = FALSE)
  }
  gm <- .topia_geo_map()

  if (source == "ideea") {
    wx <- .topia_weather_from_ideea(calendar, resources, cluster)
    weather <- do.call(rbind, lapply(regions, function(r)
      cbind(region = r, wx, stringsAsFactors = FALSE, row.names = NULL)))
    weather$cluster <- NA_character_
    weather <- weather[, c("resource", "cluster", "region", "timeslice",
                           "wval")]
  } else {
    w <- .topia_pick(as.data.frame(.topia_ds()$weather), calendar, dataset,
                     "weather")
    w$resource <- as.character(w$resource)
    w$region <- as.character(w$region)
    w$timeslice <- as.character(w$timeslice)
    w$cluster <- if (is.null(w$cluster)) NA_character_ else as.character(w$cluster)
    # one series per (resource, cluster): a wind grade or a solar mounting.
    # `NA` is the single series of a set that has no clusters.
    keys <- unique(w[, c("resource", "cluster")])
    keys <- keys[order(keys$resource, keys$cluster), , drop = FALSE]
    weather <- do.call(rbind, lapply(seq_len(nrow(keys)), function(i) {
      cl <- keys$cluster[i]
      sel <- w$resource == keys$resource[i] &
        (if (is.na(cl)) is.na(w$cluster) else !is.na(w$cluster) & w$cluster == cl)
      v <- .topia_lookup(w[sel, ], regions, "wval", gm)
      cbind(resource = keys$resource[i], cluster = cl, v,
            stringsAsFactors = FALSE, row.names = NULL)
    }))
    weather <- weather[, c("resource", "cluster", "region", "timeslice",
                           "wval")]
  }

  d <- .topia_pick(as.data.frame(.topia_ds()$demand), calendar, dataset,
                   "demand")
  d$region <- as.character(d$region)
  d$timeslice <- as.character(d$timeslice)
  demand <- .topia_lookup(d, regions, "load", gm)

  sx <- as.data.frame(.topia_ds()$stock)
  stock <- do.call(rbind, lapply(regions, function(r)
    cbind(region = r, sx, stringsAsFactors = FALSE, row.names = NULL)))

  list(
    weather = weather,
    demand = demand[, c("region", "timeslice", "load")],
    stock = stock[, c("region", "tech", "gw")])
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
#' topia_profile("step", levels = 2, calendar = "s4")
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
