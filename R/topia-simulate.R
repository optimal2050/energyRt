# ============================================================================ #
# topia_simulate() -- synthetic TOPIA weather and demand profiles.
#
# Deterministic and closed form. The shipped "simulated" dataset is one cached
# run of this function; every other parameter set is reachable by argument.
#
# Three mechanisms carry the spatial structure, none of them a scaled copy of
# another, because scaling a shared shape does not decorrelate it:
#   solar  one bell per province, centred on that province's own peak hour
#   wind   a blend of two endpoint day shapes, calm sun-following to nocturnal
#   load   a residential/industrial mix by zone, shifted by position
#
# LEVELS ARE CALIBRATED against the shipped measured set: each (resource,
# cluster, province) field is scaled to the mean capacity factor measured
# there, and the load shape to the measured peak-to-mean. Clusters differ in
# SHAPE through one parameter each -- an offset along the calm-to-windy axis
# for wind, the half-length of the solar day for the mounting -- so a grade is
# a different resource rather than another ceiling on one series.
#
# Every diurnal curve is an explicit 24-value vector with distinct values:
# ties make the LP degenerate and GLPK cycles on them.
# ============================================================================ #

# seasonal multipliers, normalised to mean 1 inside the generator
.SIM_M_SOL <- c(0.42, 0.62, 0.95, 1.28, 1.52, 1.62,
                1.58, 1.38, 1.05, 0.72, 0.48, 0.38)
.SIM_M_WIN <- c(1.32, 1.26, 1.14, 0.96, 0.84, 0.70,
                0.64, 0.69, 0.87, 1.04, 1.21, 1.33)
# inflow peaks in spring at the calm end (snowmelt) and in winter at the windy
# end (rainfall); the generator rotates between the two
.SIM_M_HYD_A <- c(0.55, 0.62, 1.15, 1.85, 1.68, 1.22,
                  0.88, 0.72, 0.64, 0.58, 0.54, 0.57)
.SIM_M_HYD_B <- c(1.72, 1.58, 1.34, 0.92, 0.70, 0.52,
                  0.44, 0.42, 0.58, 0.88, 1.32, 1.58)

# day shapes: wind at the two ends of the calm-to-windy axis
.SIM_H_WIN_CALM <- c(0.62, 0.60, 0.58, 0.565, 0.57, 0.59,
                     0.63, 0.68, 0.74, 0.81, 0.88, 0.94,
                     0.99, 1.02, 1.04, 1.05, 1.03, 0.99,
                     0.93, 0.86, 0.79, 0.73, 0.685, 0.65)
.SIM_H_WIN_WINDY <- c(1.28, 1.31, 1.33, 1.34, 1.32, 1.27,
                      1.19, 1.09, 0.98, 0.87, 0.78, 0.71,
                      0.66, 0.63, 0.62, 0.645, 0.69, 0.76,
                      0.85, 0.95, 1.06, 1.15, 1.22, 1.26)
# load shapes: residential (evening peak) and industrial (nearly flat)
.SIM_H_RES <- c(0.58, 0.555, 0.54, 0.55, 0.60, 0.70,
                0.85, 1.00, 1.05, 1.02, 0.98, 0.95,
                0.93, 0.925, 0.935, 0.98, 1.12, 1.35,
                1.52, 1.55, 1.42, 1.18, 0.92, 0.70)
.SIM_H_IND <- c(0.92, 0.905, 0.90, 0.895, 0.915, 0.96,
                1.02, 1.08, 1.10, 1.10, 1.09, 1.08,
                1.07, 1.075, 1.085, 1.09, 1.082, 1.06,
                1.04, 1.02, 1.00, 0.98, 0.96, 0.94)
.SIM_M_RES <- c(1.22, 1.16, 1.02, 0.90, 0.86, 0.92,
                1.02, 1.02, 0.92, 0.94, 1.06, 1.24)
.SIM_M_IND <- c(1.04, 1.03, 1.00, 0.98, 0.97, 1.00,
                1.01, 1.01, 0.99, 0.98, 1.00, 1.05)

# Residential share of load by ZONE: the island is residential and has no
# industry, the gas-and-solar provinces are mixed, the firm and industrial
# zone is not residential, the resource belt is mixed and the last zone holds
# the load centre.
.SIM_W_RES <- c(Z1 = 0.85, Z2 = 0.70, Z3 = 0.25, Z4 = 0.45, Z5 = 0.60)

# Cluster SHAPE parameters. Levels come from the measured calibration, so
# these set only the shape.
#   wind   an offset along the calm-to-windy axis: an exposed ridge is less
#          tied to daytime heating than inland terrain
#   solar  the half-length of the solar day: tracking holds output near the
#          peak for longer than a fixed tilt
.SIM_WIN_GRADE <- c(HI = 0.25, MD = 0, LO = -0.25)
.SIM_SOL_HALFLEN <- c(FL = 6.1, TH = 7.3)

.SIM_HYD_CF <- 0.354        # fallback annual mean inflow factor
.SIM_SOL_HALFLEN_DEF <- 6.1 # a mounting the table does not name
.SIM_WVAL_MAX <- 1          # a capacity factor cannot exceed nameplate
.SIM_DPM <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

#' @noRd
.sim_lin <- function(ends, x) ends[1] + (ends[2] - ends[1]) * x

# Resample a 24-hour cycle shifted `by` hours later, wrapping at midnight.
# Mixing two basis day shapes cannot decorrelate them -- every mix lies on the
# line between the two -- so the load shapes are shifted as well as mixed, by
# the same clock that moves the solar peak.
#' @noRd
.sim_shift_day <- function(v, by) {
  h <- 0:23
  s <- (h - by) %% 24
  i <- floor(s)
  f <- s - i
  v[i + 1L] * (1 - f) + v[(i + 1L) %% 24 + 1L] * f
}

# Shrink a field's deviation from its mean until its maximum reaches `ceiling`.
# The mean is preserved exactly and no two values are made equal.
#' @noRd
.sim_squash <- function(z, ceiling) {
  m <- mean(z)
  if (max(z) <= ceiling || max(z) <= m) return(z)
  m + (z - m) * ((ceiling - m) / (max(z) - m))
}

# A solar day shape peaking at `peak`, zero at night, asymmetric about noon.
#' @noRd
.sim_sol_day <- function(peak, halflen = .SIM_SOL_HALFLEN_DEF) {
  h <- 0:23
  u <- (h - peak) / halflen
  v <- ifelse(abs(u) >= 1, 0, cos(u * pi / 2)^2.1)
  # a slightly fatter morning limb than afternoon, so no two hours tie
  v <- v * (1 + 0.06 * tanh((peak - h) / 3))
  pmax(v, 0)
}

# ---- calibration, read from the shipped measured set ----------------------
#
# The measured hourly set carries the cluster dimension, so the targets are
# per (resource, cluster, province) rather than per province. Reading them
# here is what frees this function from `data-raw/`.
#' @noRd
.sim_targets <- function(regions, measured = NULL) {
  if (is.null(measured)) {
    ds <- .topia_ds()
    w <- as.data.frame(ds$weather)
    d <- as.data.frame(ds$demand)
    w <- w[as.character(w$dataset) == "sample", , drop = FALSE]
    d <- d[as.character(d$dataset) == "sample", , drop = FALSE]
  } else {
    # the dataset build calibrates against the measured frames directly: at
    # that point the shipped dataset is what is being written
    w <- as.data.frame(measured$weather)
    d <- as.data.frame(measured$demand)
  }
  if (!nrow(w) || !nrow(d)) {
    stop("topia_simulate(): the measured set the levels are calibrated ",
         "against is not in this dataset.", call. = FALSE)
  }
  w$region <- as.character(w$region)
  w$resource <- as.character(w$resource)
  w$cluster <- as.character(w$cluster)
  d$region <- as.character(d$region)
  key <- paste(w$resource, ifelse(is.na(w$cluster), "", w$cluster), w$region)
  cf <- tapply(w$wval, key, mean)
  # an ungraded request takes the resource's level in that province, pooled
  # over whatever grades the measured set happens to carry there
  cf_r <- tapply(w$wval, paste(w$resource, w$region), mean)

  sol <- w[w$resource == "WSOL", , drop = FALSE]
  sol$h <- as.integer(sub("^.*_h", "", as.character(sol$timeslice)))
  peak <- vapply(regions, function(r) {
    z <- sol[sol$region == r, , drop = FALSE]
    if (!nrow(z)) return(NA_real_)
    as.numeric(names(which.max(tapply(z$wval, z$h, mean)))) + 0.5
  }, numeric(1))

  pk <- vapply(regions, function(r) {
    v <- d$load[d$region == r]
    if (!length(v)) return(NA_real_)
    max(v) / mean(v)
  }, numeric(1))

  # position on the calm-to-windy axis, from the measured wind level of each
  # province pooled over its grades
  win <- w[w$resource == "WWIN", , drop = FALSE]
  wl <- vapply(regions, function(r) mean(win$wval[win$region == r]),
               numeric(1))
  x <- if (diff(range(wl, na.rm = TRUE)) > 0) {
    (wl - min(wl, na.rm = TRUE)) / diff(range(wl, na.rm = TRUE))
  } else {
    stats::setNames(rep(0.5, length(wl)), regions)
  }
  list(cf = cf, cf_r = cf_r, peak = peak, load_peak = pk,
       x = stats::setNames(x, regions))
}

# The zone each province belongs to, and the within-zone offset that keeps two
# provinces of one zone from sharing a load shape at any zone size.
#' @noRd
.sim_zones <- function(regions, layout = "honeycomb") {
  gs <- .topia_ds()$geoscales[[layout]]
  df <- as.data.frame(gs)
  fine <- intersect(c("province", "region"), names(df))[1]
  if (is.na(fine) || !"zone" %in% names(df)) {
    stop("topia_simulate(): this geoscale declares no zone over its ",
         "provinces.", call. = FALSE)
  }
  z <- stats::setNames(as.character(df$zone), as.character(df[[fine]]))[regions]
  off <- stats::setNames(rep(0, length(regions)), regions)
  for (zz in unique(z)) {
    i <- which(z == zz)
    off[i] <- if (length(i) == 1L) 0 else
      seq(0.06, -0.06, length.out = length(i))
  }
  list(zone = z, offset = off)
}

# ---- reduce a (hour x month) field to a calendar's timeslices --------------
#
# The target calendar's own timetable drives this. A YDAY frame names one
# represented day per slice, which fixes its month; a SEASON frame covers
# several months and is averaged over them, weighted by their length.
#' @noRd
.sim_reduce <- function(z, cal) {
  tt <- as.data.frame(cal@timetable)
  if (!"HOUR" %in% names(tt)) {
    stop("topia_simulate(): calendar \"", cal@name, "\" has no HOUR frame, ",
         "so an hourly shape cannot be placed on it.", call. = FALSE)
  }
  hour <- as.integer(sub("^h", "", as.character(tt$HOUR)))
  ts <- as.character(tt$timeslice)
  if ("YDAY" %in% names(tt)) {
    yday <- as.integer(sub("^d", "", as.character(tt$YDAY)))
    month <- as.integer(format(as.Date(yday - 1, origin = "2019-01-01"), "%m"))
    val <- z[cbind(hour + 1L, month)]
    return(data.frame(timeslice = ts, value = as.numeric(val),
                      stringsAsFactors = FALSE))
  }
  if ("SEASON" %in% names(tt)) {
    seas <- as.character(tt$SEASON)
    msea <- .topia_season_of_month(1:12)
    val <- vapply(seq_along(ts), function(i) {
      m <- which(msea == seas[i])
      stats::weighted.mean(z[hour[i] + 1L, m], .SIM_DPM[m])
    }, numeric(1))
    return(data.frame(timeslice = ts, value = val, stringsAsFactors = FALSE))
  }
  stop("topia_simulate(): calendar \"", cal@name, "\" has neither a YDAY nor ",
       "a SEASON frame between ANNUAL and HOUR.", call. = FALSE)
}

#' Synthetic TOPIA profiles
#'
#' @description
#' Generate TOPIA weather and demand profiles from a closed-form model of the
#' map, at any resolution and for any cluster roster. The shipped `"simulated"`
#' dataset ([topia]) is one cached run of this function, so
#' `topia_simulate()` at its defaults reproduces it and every other parameter
#' set is an alternative.
#'
#' The output is shaped like [topia_profiles()]'s, so either can be passed to
#' `build_topia(weather = , demand = )`.
#'
#' @details
#' Levels are calibrated against the shipped measured set: each
#' `(resource, cluster, province)` field is scaled to the mean capacity factor
#' measured there, and the load shape to the measured peak-to-mean. What the
#' arguments change is the SHAPE.
#'
#' Clusters differ through one parameter each -- `wind_grade` offsets a grade
#' along the calm-to-windy axis, `solar_halflen` sets the half-length of the
#' solar day per mounting -- so a grade is a different resource rather than
#' another ceiling on one series. A roster with no clusters yields one series
#' per resource with `cluster` `NA`, which no graded technology can use; see
#' `build_topia(clusters = )`.
#'
#' @param regions provinces to generate, defaulting to all of TOPIA's.
#' @param calendar a calendar name, a calendar object, or a timetable
#'   `data.frame`. It must carry an `HOUR` frame and either `YDAY` or
#'   `SEASON` above it.
#' @param clusters the roster to generate: a `data.frame` of `resource`,
#'   `cluster` and `region`, or `NULL` to take the rosters the shipped weather
#'   objects declare. `FALSE` generates one ungraded series per resource.
#' @param solar_season,wind_season,hydro_season_calm,hydro_season_windy twelve
#'   monthly multipliers each, normalised to mean 1 internally.
#' @param wind_day_calm,wind_day_windy,load_day_res,load_day_ind twenty-four
#'   hourly values each: the endpoints the shapes are blended between.
#' @param load_season_res,load_season_ind twelve monthly multipliers each.
#' @param residential residential share of load by zone, named.
#' @param wind_grade,solar_halflen the cluster shape parameters, named by
#'   cluster.
#' @param hydro_cf annual mean inflow factor, used where the measured set has
#'   no hydro series for a province.
#' @param jitter relative amplitude of a deterministic perturbation applied to
#'   each field, `0` for none. The mean is preserved and no values are made
#'   equal, so the energy is unbiased and the LP stays non-degenerate.
#' @param seed integer seed for `jitter`.
#' @param layout the geoscale whose zones the load mix is keyed on.
#' @param measured the measured frames the levels are calibrated against, as
#'   `list(weather =, demand =)`. `NULL` reads the shipped set, which is what
#'   a user wants; the dataset build passes its own frames, since at that
#'   point the shipped set is what is being written.
#'
#' @return a list of two tidy `data.frame`s: `weather`
#'   (`resource`, `cluster`, `region`, `timeslice`, `wval`) and `demand`
#'   (`region`, `timeslice`, `load`), both carrying a `dataset` attribute
#'   naming the generator and the parameters that differ from the defaults.
#' @seealso [topia_profiles()] for the shipped sets, [build_topia()],
#'   [topia_profile()] for geometric shapes
#' @family topia
#' @export
#' @examples
#' p <- topia_simulate(regions = c("R01", "R10"), calendar = "s4_h24")
#' head(p$weather, 3)
#'
#' # an alternative: no seasonality in the wind
#' p2 <- topia_simulate(regions = c("R01", "R10"), wind_season = rep(1, 12))
topia_simulate <- function(regions = NULL,
                           calendar = "s4_h24",
                           clusters = NULL,
                           solar_season = .SIM_M_SOL,
                           wind_season = .SIM_M_WIN,
                           hydro_season_calm = .SIM_M_HYD_A,
                           hydro_season_windy = .SIM_M_HYD_B,
                           wind_day_calm = .SIM_H_WIN_CALM,
                           wind_day_windy = .SIM_H_WIN_WINDY,
                           load_day_res = .SIM_H_RES,
                           load_day_ind = .SIM_H_IND,
                           load_season_res = .SIM_M_RES,
                           load_season_ind = .SIM_M_IND,
                           residential = .SIM_W_RES,
                           wind_grade = .SIM_WIN_GRADE,
                           solar_halflen = .SIM_SOL_HALFLEN,
                           hydro_cf = .SIM_HYD_CF,
                           jitter = 0,
                           seed = NULL,
                           layout = "honeycomb",
                           measured = NULL) {
  ds <- .topia_ds()
  kit <- ds$modules$electricity
  if (is.null(regions)) regions <- as.character(kit$regions)
  if (is.null(regions)) {
    stop("topia_simulate(regions = ): this dataset carries no model to take ",
         "the provinces from; name them.", call. = FALSE)
  }
  regions <- as.character(regions)
  stopifnot(length(regions) > 0)
  for (v in list(solar_season, wind_season, hydro_season_calm,
                 hydro_season_windy, load_season_res, load_season_ind)) {
    stopifnot(is.numeric(v), length(v) == 12L)
  }
  for (v in list(wind_day_calm, wind_day_windy, load_day_res, load_day_ind)) {
    stopifnot(is.numeric(v), length(v) == 24L)
  }
  stopifnot(is.numeric(jitter), length(jitter) == 1L, jitter >= 0)
  cal <- .shape_calendar(calendar)

  # -- the roster ------------------------------------------------------------
  ros <- if (identical(clusters, FALSE)) {
    data.frame(resource = rep(c("WSOL", "WWIN", "WHYD"), each = length(regions)),
               cluster = NA_character_, region = rep(regions, times = 3L),
               stringsAsFactors = FALSE)
  } else if (is.null(clusters)) {
    ob <- getObject(kit$repo)
    cls <- vapply(ob, function(x) class(x)[1], character(1))
    rs <- lapply(names(ob)[cls == "weather"], function(nm) {
      r <- ob[[nm]]@cluster
      if (!nrow(r)) {
        data.frame(resource = nm, cluster = NA_character_, region = regions,
                   stringsAsFactors = FALSE)
      } else {
        r <- r[as.character(r$region) %in% regions, , drop = FALSE]
        data.frame(resource = nm, cluster = as.character(r$cluster),
                   region = as.character(r$region), stringsAsFactors = FALSE)
      }
    })
    do.call(rbind, rs)
  } else {
    if (!is.data.frame(clusters) ||
        !all(c("resource", "cluster", "region") %in% names(clusters))) {
      stop("topia_simulate(clusters = ): give a data.frame of resource, ",
           "cluster and region, or NULL for the shipped rosters, or FALSE ",
           "for none.", call. = FALSE)
    }
    cl <- clusters
    cl$resource <- as.character(cl$resource)
    cl$cluster <- as.character(cl$cluster)
    cl$region <- as.character(cl$region)
    cl[cl$region %in% regions, , drop = FALSE]
  }
  if (!nrow(ros)) {
    stop("topia_simulate(): the roster covers none of `regions`.",
         call. = FALSE)
  }

  tg <- .sim_targets(regions, measured)
  zn <- .sim_zones(regions, layout)
  miss <- setdiff(unique(zn$zone), names(residential))
  if (length(miss)) {
    stop("topia_simulate(residential = ): no share for zone(s) ",
         paste(miss, collapse = ", "), ".", call. = FALSE)
  }

  sol_m <- solar_season / mean(solar_season)
  win_m <- wind_season / mean(wind_season)
  if (!is.null(seed)) set.seed(as.integer(seed))

  # a deterministic perturbation of a field, mean-preserving and tie-free
  jit <- function(z) {
    if (jitter <= 0) return(z)
    e <- stats::runif(length(z), -1, 1)
    e <- e - mean(e)
    z * (1 + jitter * e)
  }

  # -- the fields ------------------------------------------------------------
  cfk <- function(res, cl, r) {
    v <- unname(tg$cf[paste(res, if (is.na(cl)) "" else cl, r)])
    if (is.na(v)) v <- unname(tg$cf_r[paste(res, r)])
    if (length(v) != 1L || !is.finite(v)) NA_real_ else v
  }
  scale_to <- function(hv, mv, target) {
    z <- outer(hv, mv)
    z * (target / mean(z))
  }

  wx <- vector("list", nrow(ros))
  for (i in seq_len(nrow(ros))) {
    res <- ros$resource[i]
    cl <- ros$cluster[i]
    r <- ros$region[i]
    x0 <- tg$x[[r]]
    if (!is.finite(x0)) x0 <- 0.5
    target <- cfk(res, cl, r)
    if (res == "WSOL") {
      hl <- if (!is.na(cl) && cl %in% names(solar_halflen)) {
        solar_halflen[[cl]]
      } else {
        .SIM_SOL_HALFLEN_DEF
      }
      pk <- tg$peak[[r]]
      if (!is.finite(pk)) pk <- 12.5
      hv <- .sim_sol_day(pk, hl)
      mv <- sol_m
    } else if (res == "WWIN") {
      off <- if (!is.na(cl) && cl %in% names(wind_grade)) wind_grade[[cl]] else 0
      x <- min(1, max(0, x0 + off))
      hv <- .sim_lin(c(1, 0), x) * wind_day_calm +
        .sim_lin(c(0, 1), x) * wind_day_windy
      mv <- win_m
    } else {
      hm <- .sim_lin(c(1, 0), x0) * hydro_season_calm +
        .sim_lin(c(0, 1), x0) * hydro_season_windy
      hv <- rep(1, 24)
      mv <- hm / mean(hm)
      if (is.na(target)) target <- hydro_cf
    }
    if (is.na(target)) {
      stop("topia_simulate(): the measured set has no ", res,
           if (is.na(cl)) "" else paste0(" ", cl), " series for ", r,
           ", so its level cannot be calibrated.", call. = FALSE)
    }
    z <- .sim_squash(jit(scale_to(hv, mv, target)), .SIM_WVAL_MAX)
    v <- .sim_reduce(z, cal)
    wx[[i]] <- data.frame(resource = res, cluster = cl, region = r,
                          timeslice = v$timeslice, wval = v$value,
                          stringsAsFactors = FALSE)
  }
  weather <- do.call(rbind, wx)

  # -- the load curve --------------------------------------------------------
  dx <- vector("list", length(regions))
  for (i in seq_along(regions)) {
    r <- regions[i]
    wr <- min(0.95, max(0.05, residential[[zn$zone[[r]]]] + zn$offset[[r]]))
    pk <- tg$peak[[r]]
    if (!is.finite(pk)) pk <- 12.5
    hv <- .sim_shift_day(wr * load_day_res + (1 - wr) * load_day_ind,
                         pk - min(tg$peak, na.rm = TRUE))
    mv <- wr * load_season_res + (1 - wr) * load_season_ind
    z <- jit(scale_to(hv, mv / mean(mv), 1))
    v <- .sim_reduce(z, cal)
    cap <- tg$load_peak[[r]]
    dx[[i]] <- data.frame(region = r, timeslice = v$timeslice,
                          load = if (is.finite(cap)) .sim_squash(v$value, cap)
                                 else v$value,
                          stringsAsFactors = FALSE)
  }
  demand <- do.call(rbind, dx)

  changed <- .sim_changed(
    list(solar_season = solar_season, wind_season = wind_season,
         hydro_season_calm = hydro_season_calm,
         hydro_season_windy = hydro_season_windy,
         wind_day_calm = wind_day_calm, wind_day_windy = wind_day_windy,
         load_day_res = load_day_res, load_day_ind = load_day_ind,
         load_season_res = load_season_res, load_season_ind = load_season_ind,
         residential = residential, wind_grade = wind_grade,
         solar_halflen = solar_halflen, hydro_cf = hydro_cf),
    list(solar_season = .SIM_M_SOL, wind_season = .SIM_M_WIN,
         hydro_season_calm = .SIM_M_HYD_A,
         hydro_season_windy = .SIM_M_HYD_B,
         wind_day_calm = .SIM_H_WIN_CALM, wind_day_windy = .SIM_H_WIN_WINDY,
         load_day_res = .SIM_H_RES, load_day_ind = .SIM_H_IND,
         load_season_res = .SIM_M_RES, load_season_ind = .SIM_M_IND,
         residential = .SIM_W_RES, wind_grade = .SIM_WIN_GRADE,
         solar_halflen = .SIM_SOL_HALFLEN, hydro_cf = .SIM_HYD_CF))
  tag <- paste0("simulated", if (length(changed)) {
    paste0(" (", paste(changed, collapse = ", "), " changed)")
  } else "",
  if (jitter > 0) paste0(", jitter ", jitter,
                         if (is.null(seed)) "" else paste0(" seed ", seed))
  else "")
  attr(weather, "dataset") <- tag
  attr(demand, "dataset") <- tag
  list(weather = weather, demand = demand)
}

# Which parameters a call changed, for the provenance tag.
#' @noRd
.sim_changed <- function(got, def) {
  nm <- names(def)
  nm[!vapply(nm, function(n) isTRUE(all.equal(got[[n]], def[[n]])),
             logical(1))]
}
