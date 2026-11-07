# Simulated TOPIA profiles: the demonstration dataset for the small calendars.
#
# Deterministic, closed form, and documented as synthetic. Levels are set to
# the same order as the sample's, so a model built on one behaves like a model
# built on the other, but the gradients are monotone and the shapes clean:
# this dataset is for teaching a small case, not for a resource claim.
#
# Three mechanisms carry the spatial structure, each of which genuinely
# decorrelates:
#   solar   one bell per region, shifted to that region's own measured peak
#           hour (the continental clock spans ~2.3 h of solar time)
#   wind    a blend of two endpoint day shapes, calm sun-following at the calm
#           end, nocturnal at the windy one
#   load    a residential/industrial mix by zone, tilted by position
#
# LEVELS ARE CALIBRATED PER REGION against the committed sample: each region's
# simulated annual mean, solar peak hour and load peak-to-mean are read from
# `data-raw/topia_profiles_sample.rds` rather than interpolated along the code
# order. The codes are not ordered by resource -- a zone is a compact group on
# the map, not a step along the gradient -- so a linear chain over `R01`-`R12`
# would put the windy end in the middle of the list.
# Scaling a shared shape does not decorrelate it, which is why none of the
# three is a scaled copy.
#
# Every diurnal curve is an explicit 24-value vector with distinct values: ties
# make the LP degenerate and GLPK cycles on them.

REG_SIM <- sprintf("R%02d", 1:12)

HYD_CF <- 0.354             # annual mean inflow factor
SOL_PEAK <- c(9.8, 12.1)    # clock hours the solar maximum spans

# Per-region calibration from the measured sample: annual mean solar and wind
# capacity factor, the clock hour of the solar maximum, and the peak-to-mean of
# the reduced load shape.
.sim_calibration <- function(
    rds = "data-raw/topia_profiles_sample.rds") {
  if (!file.exists(rds)) {
    stop("missing ", rds, ": the simulated profiles are calibrated against ",
         "the sample, so run data-raw/topia_profiles_sample.R with ",
         "ENERGYRT_TOPIA_SAMPLE=refresh first.", call. = FALSE)
  }
  smp <- readRDS(rds)
  w <- as.data.frame(smp$weather)
  d <- as.data.frame(smp$demand)
  w$region <- as.character(w$region)
  d$region <- as.character(d$region)
  stopifnot(setequal(w$region, REG_SIM), setequal(d$region, REG_SIM))
  hour <- function(ts) as.integer(sub("^d[0-9]+_h", "", as.character(ts)))
  cf <- function(res) {
    v <- w[w$resource == res, ]
    vapply(REG_SIM, function(r) mean(v$wval[v$region == r]), numeric(1))
  }
  sol <- w[w$resource == "WSOL", ]
  sol$h <- hour(sol$timeslice)
  peak <- vapply(REG_SIM, function(r) {
    z <- sol[sol$region == r, ]
    as.numeric(names(which.max(tapply(z$wval, z$h, mean)))) + 0.5
  }, numeric(1))
  pk <- vapply(REG_SIM, function(r) {
    v <- d$load[d$region == r]
    max(v) / mean(v)
  }, numeric(1))
  list(sol_cf = cf("WSOL"), win_cf = cf("WWIN"), sol_peak = peak,
       load_peak = pk)
}

# seasonal multipliers, normalised to mean 1 inside the generator
M_SOL <- c(0.42, 0.62, 0.95, 1.28, 1.52, 1.62,
           1.58, 1.38, 1.05, 0.72, 0.48, 0.38)
M_WIN <- c(1.32, 1.26, 1.14, 0.96, 0.84, 0.70,
           0.64, 0.69, 0.87, 1.04, 1.21, 1.33)
# inflow peaks in spring at the chain's start (snowmelt) and in winter at its
# end (rainfall); the generator rotates between the two
M_HYD_A <- c(0.55, 0.62, 1.15, 1.85, 1.68, 1.22,
             0.88, 0.72, 0.64, 0.58, 0.54, 0.57)
M_HYD_B <- c(1.72, 1.58, 1.34, 0.92, 0.70, 0.52,
             0.44, 0.42, 0.58, 0.88, 1.32, 1.58)

# day shapes: wind at the two ends of the chain
H_WIN_A <- c(0.62, 0.60, 0.58, 0.565, 0.57, 0.59,
             0.63, 0.68, 0.74, 0.81, 0.88, 0.94,
             0.99, 1.02, 1.04, 1.05, 1.03, 0.99,
             0.93, 0.86, 0.79, 0.73, 0.685, 0.65)
H_WIN_B <- c(1.28, 1.31, 1.33, 1.34, 1.32, 1.27,
             1.19, 1.09, 0.98, 0.87, 0.78, 0.71,
             0.66, 0.63, 0.62, 0.645, 0.69, 0.76,
             0.85, 0.95, 1.06, 1.15, 1.22, 1.26)
# load shapes: residential (evening peak) and industrial (nearly flat)
H_RES <- c(0.58, 0.555, 0.54, 0.55, 0.60, 0.70,
           0.85, 1.00, 1.05, 1.02, 0.98, 0.95,
           0.93, 0.925, 0.935, 0.98, 1.12, 1.35,
           1.52, 1.55, 1.42, 1.18, 0.92, 0.70)
H_IND <- c(0.92, 0.905, 0.90, 0.895, 0.915, 0.96,
           1.02, 1.08, 1.10, 1.10, 1.09, 1.08,
           1.07, 1.075, 1.085, 1.09, 1.082, 1.06,
           1.04, 1.02, 1.00, 0.98, 0.96, 0.94)
M_RES <- c(1.22, 1.16, 1.02, 0.90, 0.86, 0.92,
           1.02, 1.02, 0.92, 0.94, 1.06, 1.24)
M_IND <- c(1.04, 1.03, 1.00, 0.98, 0.97, 1.00,
           1.01, 1.01, 0.99, 0.98, 1.00, 1.05)
# Residential share of load by zone: the arid west is residential, the firm
# and industrial north-east is not, the resource belt is mixed, and the windy
# end holds the load centre.
W_RES <- c(NW = 0.80, NE = 0.25, SW = 0.45, SE = 0.60)
# an offset on the zone's share, so no two regions of one zone share a shape
W_RES_OFF <- stats::setNames(rep(c(0.06, 0, -0.06), times = 4L), REG_SIM)
WVAL_MAX <- 1          # a capacity factor cannot exceed nameplate

.lin <- function(ends, x) ends[1] + (ends[2] - ends[1]) * x

# Resample a 24-hour cycle shifted `by` hours later, wrapping at midnight.
# Mixing two basis day shapes cannot decorrelate them -- every mix lies on the
# line between the two -- so the load shapes are shifted as well as mixed, by
# the same continental clock that moves the solar peak.
.shift_day <- function(v, by) {
  h <- 0:23
  s <- (h - by) %% 24
  i <- floor(s)
  f <- s - i
  v[i + 1L] * (1 - f) + v[(i + 1L) %% 24 + 1L] * f
}

# Shrink a field's deviation from its mean until its maximum reaches `ceiling`.
# The mean is preserved exactly and no two values are made equal, so this
# neither biases the energy nor creates ties.
.squash <- function(z, ceiling) {
  m <- mean(z)
  if (max(z) <= ceiling || max(z) <= m) return(z)
  m + (z - m) * ((ceiling - m) / (max(z) - m))
}

# A solar day shape peaking at `peak`, zero at night, asymmetric about noon.
.sol_day <- function(peak, halflen = 6.1) {
  h <- 0:23
  u <- (h - peak) / halflen
  v <- ifelse(abs(u) >= 1, 0, cos(u * pi / 2)^2.1)
  # a slightly fatter morning limb than afternoon, so no two hours tie
  v <- v * (1 + 0.06 * tanh((peak - h) / 3))
  pmax(v, 0)
}

# One region's (month, hour) field for every resource and for load. `x` places
# the region on the calm-to-windy axis (shapes); the annual means and the solar
# peak hour come from that region's own calibration.
.sim_fields <- function(x, zone, region, sol_cf, win_cf, sol_peak) {
  sol_m <- M_SOL / mean(M_SOL)
  win_m <- M_WIN / mean(M_WIN)
  hyd_m <- .lin(c(1, 0), x) * M_HYD_A + .lin(c(0, 1), x) * M_HYD_B
  hyd_m <- hyd_m / mean(hyd_m)
  sol_h <- .sol_day(sol_peak)
  win_h <- .lin(c(1, 0), x) * H_WIN_A + .lin(c(0, 1), x) * H_WIN_B
  wr <- min(0.95, max(0.05, W_RES[[zone]] + W_RES_OFF[[region]]))
  load_h <- .shift_day(wr * H_RES + (1 - wr) * H_IND,
                       sol_peak - SOL_PEAK[1])
  load_m <- wr * M_RES + (1 - wr) * M_IND
  # outer(hour, month): scale each field to its target annual mean
  f <- function(hv, mv, target) {
    z <- outer(hv, mv)
    z * (target / mean(z))
  }
  list(
    WSOL = .squash(f(sol_h, sol_m, sol_cf), WVAL_MAX),
    WWIN = .squash(f(win_h, win_m, win_cf), WVAL_MAX),
    WHYD = .squash(f(rep(1, 24), hyd_m, HYD_CF), WVAL_MAX),
    load = f(load_h, load_m / mean(load_m), 1))
}

# Reduce a (24 x 12) field to a calendar's timeslices, weighting months by
# their length so the annual mean is preserved.
DPM <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

.sim_reduce <- function(z, calendar) {
  g <- expand.grid(hour = 0:23, month = 1:12)
  v <- as.vector(z)
  wt <- DPM[g$month]
  key <- energyRt:::.topia_timeslice_key(
    yday = as.integer(format(as.Date(paste0("2001-", g$month, "-15")), "%j")),
    hour = g$hour, calendar = calendar)
  num <- tapply(v * wt, key, sum)
  den <- tapply(wt, key, sum)
  data.frame(timeslice = names(num), value = as.numeric(num / den),
             stringsAsFactors = FALSE)
}

topia_sim_profiles <- function(
    zones = stats::setNames(rep(c("NW", "NE", "SW", "SE"), each = 3L),
                            REG_SIM),
    calendars = "s4_h24",
    calibration = .sim_calibration()) {
  cal_ <- calibration
  stopifnot(all(REG_SIM %in% names(cal_$win_cf)))
  # position on the calm-to-windy axis, from the measured wind level
  wr_ <- cal_$win_cf[REG_SIM]
  xpos <- stats::setNames(
    (wr_ - min(wr_)) / (max(wr_) - min(wr_)), REG_SIM)
  wx <- list()
  dx <- list()
  for (i in seq_along(REG_SIM)) {
    r <- REG_SIM[i]
    fl <- .sim_fields(xpos[[r]], zones[[r]], r,
                      sol_cf = cal_$sol_cf[[r]], win_cf = cal_$win_cf[[r]],
                      sol_peak = cal_$sol_peak[[r]])
    for (cal in calendars) {
      for (res in c("WSOL", "WWIN", "WHYD")) {
        v <- .sim_reduce(fl[[res]], cal)
        wx[[length(wx) + 1L]] <- data.frame(calendar = cal, resource = res,
          region = r, timeslice = v$timeslice, wval = v$value,
          stringsAsFactors = FALSE)
      }
      # peak-to-mean is a property of the reduced shape, so it is set here
      v <- .sim_reduce(fl$load, cal)
      dx[[length(dx) + 1L]] <- data.frame(calendar = cal, region = r,
        timeslice = v$timeslice, load = .squash(v$value, cal_$load_peak[[r]]),
        stringsAsFactors = FALSE)
    }
  }
  list(weather = do.call(rbind, wx), demand = do.call(rbind, dx))
}

# ── the builder's product ────────────────────────────────────────────────────
# data-raw/topia_data.R consumes this; nothing is written here.
topia_sim <- topia_sim_profiles()
stopifnot(nrow(topia_sim$weather) > 0, nrow(topia_sim$demand) > 0,
          max(topia_sim$weather$wval) <= 1, min(topia_sim$weather$wval) >= 0)
