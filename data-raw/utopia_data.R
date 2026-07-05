## data-raw/utopia_data.R
## Deterministic, sourced input data for the UTOPIA vignette (replaces the old
## random generators: fLoadCurve / rwind / rclouds / runif). Builds, for BOTH
## teaching calendars (utopia_m12h24 = default, 288 slices; utopia_seasons = 12):
##   utopia_weather - representative solar/wind/hydro capacity factors by slice
##   utopia_demand  - a deterministic electricity load shape by slice
##   utopia_stock   - deterministic base-year capacity per technology (calendar-agnostic)
## Region-agnostic; utopia_profiles() (R/utopia_profiles.R) expands them to a
## model's regions and can re-source the weather CFs from IDEEA / merra2ools.
## Run: pkgload::load_all(".") ; source("data-raw/utopia_data.R") ; devtools::document()

if (!isNamespaceLoaded("energyRt")) library(energyRt)
library(usethis)

CALS <- c("utopia_s4h24", "utopia_m12h24", "utopia_seasons")

# ── curated fallback CF (used only when IDEEA is not installed) ────────────────
# Physically motivated: solar = daylight bell curve, wind ~ flat w/ night boost,
# hydro seasonal. Built per (month, hour) then reduced to the target calendar.
.curated_cf <- function(calendar) {
  grid <- expand.grid(month = 1:12, hour = 0:23)
  bell <- pmax(0, sin(pi * (grid$hour - 6) / 12)) * (grid$hour >= 6 & grid$hour <= 18)
  sol_s <- c(1.0, 1.05, 1.1, 1.15, 1.2, 1.1, 1.05, 1.05, 1.0, 0.95, 0.9, 0.95)
  wnd_s <- c(1.2, 1.15, 1.1, 1.0, 0.9, 0.8, 0.75, 0.8, 0.9, 1.0, 1.1, 1.2)
  hyd_s <- c(0.25, 0.25, 0.35, 0.5, 0.6, 0.55, 0.5, 0.45, 0.4, 0.35, 0.3, 0.25)
  grid$WSOL <- pmin(1, bell * sol_s[grid$month])
  grid$WWIN <- pmin(1, (0.25 + 0.10 * (grid$hour < 7 | grid$hour > 20)) * wnd_s[grid$month])
  grid$WHYD <- pmin(1, hyd_s[grid$month])
  key <- energyRt:::.utopia_slice_key(
    yday = as.integer(format(as.Date(paste0("2019-", grid$month, "-15")), "%j")),
    hour = grid$hour, calendar = calendar)
  out <- lapply(c("WSOL", "WWIN", "WHYD"), function(res) {
    a <- stats::aggregate(grid[[res]], by = list(slice = key), FUN = mean)
    data.frame(resource = res, slice = a$slice, wval = a$x, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# ── 1. Weather (CF) for both calendars ────────────────────────────────────────
have_ideea <- requireNamespace("IDEEA", quietly = TRUE)
utopia_weather <- do.call(rbind, lapply(CALS, function(cal) {
  cf <- if (have_ideea) energyRt:::.utopia_weather_from_ideea(cal) else .curated_cf(cal)
  cbind(calendar = cal, cf, stringsAsFactors = FALSE, row.names = NULL)
}))
attr(utopia_weather, "source") <- if (have_ideea) {
  "IDEEA::ideea_modules$electricity$reg5 (d365_h24 CL01, calendar-aggregated)"
} else "curated fallback (IDEEA not installed)"
message("utopia_weather source: ", attr(utopia_weather, "source"),
        "  rows: ", nrow(utopia_weather))

# ── 2. Deterministic electricity load shape for both calendars ────────────────
# m12h24: a 24-hour diurnal curve x a 12-month seasonal factor.
.diurnal24 <- c(0.70, 0.65, 0.62, 0.60, 0.62, 0.68,   # 00-05 night
                0.80, 0.95, 1.05, 1.08, 1.07, 1.05,   # 06-11 morning/day
                1.05, 1.04, 1.03, 1.05, 1.10, 1.20,   # 12-17
                1.30, 1.32, 1.25, 1.10, 0.95, 0.80)   # 18-23 evening peak
.monthly12 <- c(1.20, 1.15, 1.00, 0.90, 0.90, 1.00,
                1.10, 1.10, 0.95, 0.90, 1.00, 1.20)
.season_factor <- c(WIN = 1.2, SPR = 0.9, SUM = 1.1, AUT = 0.9)
.demand_shape <- function(calendar) {
  if (calendar == "utopia_s4h24") {
    g <- expand.grid(season = c("WIN", "SPR", "SUM", "AUT"), hour = 0:23,
                     stringsAsFactors = FALSE)
    data.frame(slice = sprintf("%s_h%02d", g$season, g$hour),
               load = .season_factor[g$season] * .diurnal24[g$hour + 1],
               stringsAsFactors = FALSE)
  } else if (calendar == "utopia_m12h24") {
    g <- expand.grid(month = 1:12, hour = 0:23)
    data.frame(slice = sprintf("m%02d_h%02d", g$month, g$hour),
               load = .monthly12[g$month] * .diurnal24[g$hour + 1],
               stringsAsFactors = FALSE)
  } else { # utopia_seasons
    sl <- expand.grid(season = c("WIN", "SPR", "SUM", "AUT"),
                      daypart = c("DAY", "NGT", "PK"), stringsAsFactors = FALSE)
    dp <- c(DAY = 1.0, NGT = 0.6, PK = 1.35)
    se <- c(WIN = 1.2, SPR = 0.9, SUM = 1.1, AUT = 0.9)
    data.frame(slice = paste(sl$season, sl$daypart, sep = "_"),
               load = dp[sl$daypart] * se[sl$season], stringsAsFactors = FALSE)
  }
}
utopia_demand <- do.call(rbind, lapply(CALS, function(cal) {
  cbind(calendar = cal, .demand_shape(cal), stringsAsFactors = FALSE, row.names = NULL)
}))

# ── 3. Deterministic base-year capacity per technology (GW, per region) ───────
utopia_stock <- data.frame(
  tech = c("ECOA", "EGAS", "ENUC", "EHYD", "ESOL", "EWIN"),
  gw   = c(6.0,    3.0,    2.0,    5.0,    1.0,    1.0),
  stringsAsFactors = FALSE
)

# ── 4. Store ──────────────────────────────────────────────────────────────────
usethis::use_data(utopia_weather, utopia_demand, utopia_stock, overwrite = TRUE)
message("saved: utopia_weather (", nrow(utopia_weather), "), utopia_demand (",
        nrow(utopia_demand), "), utopia_stock (", nrow(utopia_stock), ")")
