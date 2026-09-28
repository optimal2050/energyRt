## data-raw/topia_data.R
## Deterministic, sourced input data for the TOPIA vignette (replaces the old
## random generators: fLoadCurve / rwind / rclouds / runif). Builds, for BOTH
## teaching calendars (m12_h24 = default, 288 timeslices; topia_seasons = 12):
##   topia_weather - representative solar/wind/hydro capacity factors by timeslice
##   topia_demand  - a deterministic electricity load shape by timeslice
##   topia_stock   - deterministic base-year capacity per technology (calendar-agnostic)
## Region-agnostic; topia_profiles() (R/topia_profiles.R) expands them to a
## model's regions and can re-source the weather CFs from IDEEA / merra2ools.
## Run: pkgload::load_all(".") ; source("data-raw/topia_data.R") ; devtools::document()

if (!isNamespaceLoaded("energyRt")) library(energyRt)
library(usethis)

CALS <- c("s4_h24", "m12_h24", "topia_seasons")

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
  key <- energyRt:::.topia_timeslice_key(
    yday = as.integer(format(as.Date(paste0("2019-", grid$month, "-15")), "%j")),
    hour = grid$hour, calendar = calendar)
  out <- lapply(c("WSOL", "WWIN", "WHYD"), function(res) {
    a <- stats::aggregate(grid[[res]], by = list(timeslice = key), FUN = mean)
    data.frame(resource = res, timeslice = a$timeslice, wval = a$x, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# ── 1. Weather (CF) for both calendars ────────────────────────────────────────
# "installed but no data" (IDEEA >= 0.80 ships no datasets) counts as not
# installed -- same treatment as data-raw/calendars.R
have_ideea <- FALSE
if (requireNamespace("IDEEA", quietly = TRUE)) {
  .ide <- new.env()
  suppressWarnings(utils::data("ideea_modules", package = "IDEEA",
                               envir = .ide))
  have_ideea <- !is.null(.ide$ideea_modules)
}
# NON-DESTRUCTIVE second choice: without IDEEA, carry the previously
# shipped weather forward (re-keyed to the current calendar names/labels)
# rather than regenerating from the curated toy CFs. The toy profiles have
# almost no unique values (flat steps), which makes TOPIA LPs massively
# DEGENERATE -- GLPK cycles for hours on ties the real profiles never
# produce. The curated fallback remains the last resort only.
.prev_weather <- NULL
if (!have_ideea && file.exists("data/topia.rda")) {
  .pw <- new.env()
  load("data/topia.rda", envir = .pw)
  # v0.90: the standalone `topia_weather` dataset is gone; the previously
  # shipped weather now lives in the combined list.
  .prev_weather <- .pw$topia$weather
}

if (have_ideea) {
  topia_weather <- do.call(rbind, lapply(CALS, function(cal) {
    cf <- energyRt:::.topia_weather_from_ideea(cal)
    cbind(calendar = cal, cf, stringsAsFactors = FALSE, row.names = NULL)
  }))
  attr(topia_weather, "source") <-
    "IDEEA::ideea_modules$electricity$reg5 (d365_h24 CL01, calendar-aggregated)"
} else if (!is.null(.prev_weather)) {
  message("IDEEA data not available: carrying previously shipped ",
          "topia_weather over (re-keyed to current calendar labels).")
  topia_weather <- as.data.frame(.prev_weather)
  # 2026-08 unification re-keys: retired calendar names and the AUT season
  .cal_map <- c(topia_annual = "annual", topia_s4h24 = "s4_h24",
                topia_m12h24 = "m12_h24")
  hit <- topia_weather$calendar %in% names(.cal_map)
  topia_weather$calendar[hit] <- .cal_map[topia_weather$calendar[hit]]
  topia_weather$timeslice <- sub("^AUT_", "FAL_", topia_weather$timeslice)
  stopifnot(sort(unique(topia_weather$calendar)) %in%
              sort(unique(c(CALS, "annual"))))
  # carry the tag EXACTLY once: a rebuild reads back the previously shipped
  # weather, whose source note already ends in the tag, so appending blindly
  # accumulated one copy per regeneration
  .tag <- " [carried over; re-keyed 2026-08]"
  .src <- attr(.prev_weather, "source") %||% "previously shipped"
  attr(topia_weather, "source") <-
    paste0(gsub(.tag, "", .src, fixed = TRUE), .tag)
} else {
  topia_weather <- do.call(rbind, lapply(CALS, function(cal) {
    cf <- .curated_cf(cal)
    cbind(calendar = cal, cf, stringsAsFactors = FALSE, row.names = NULL)
  }))
  attr(topia_weather, "source") <- "curated fallback (IDEEA not installed)"
}
message("topia_weather source: ", attr(topia_weather, "source"),
        "  rows: ", nrow(topia_weather))

# ── 2. Deterministic electricity load shape for both calendars ────────────────
# m12h24: a 24-hour diurnal curve x a 12-month seasonal factor.
.diurnal24 <- c(0.70, 0.65, 0.62, 0.60, 0.62, 0.68,   # 00-05 night
                0.80, 0.95, 1.05, 1.08, 1.07, 1.05,   # 06-11 morning/day
                1.05, 1.04, 1.03, 1.05, 1.10, 1.20,   # 12-17
                1.30, 1.32, 1.25, 1.10, 0.95, 0.80)   # 18-23 evening peak
.monthly12 <- c(1.20, 1.15, 1.00, 0.90, 0.90, 1.00,
                1.10, 1.10, 0.95, 0.90, 1.00, 1.20)
.season_factor <- c(WIN = 1.2, SPR = 0.9, SUM = 1.1, FAL = 0.9)
.demand_shape <- function(calendar) {
  if (calendar == "s4_h24") {
    g <- expand.grid(season = c("WIN", "SPR", "SUM", "FAL"), hour = 0:23,
                     stringsAsFactors = FALSE)
    data.frame(timeslice = sprintf("%s_h%02d", g$season, g$hour),
               load = .season_factor[g$season] * .diurnal24[g$hour + 1],
               stringsAsFactors = FALSE)
  } else if (calendar == "m12_h24") {
    g <- expand.grid(month = 1:12, hour = 0:23)
    data.frame(timeslice = sprintf("m%02d_h%02d", g$month, g$hour),
               load = .monthly12[g$month] * .diurnal24[g$hour + 1],
               stringsAsFactors = FALSE)
  } else { # topia_seasons
    sl <- expand.grid(season = c("WIN", "SPR", "SUM", "FAL"),
                      daypart = c("DAY", "NGT", "PK"), stringsAsFactors = FALSE)
    dp <- c(DAY = 1.0, NGT = 0.6, PK = 1.35)
    se <- c(WIN = 1.2, SPR = 0.9, SUM = 1.1, FAL = 0.9)
    data.frame(timeslice = paste(sl$season, sl$daypart, sep = "_"),
               load = dp[sl$daypart] * se[sl$season], stringsAsFactors = FALSE)
  }
}
topia_demand <- do.call(rbind, lapply(CALS, function(cal) {
  cbind(calendar = cal, .demand_shape(cal), stringsAsFactors = FALSE, row.names = NULL)
}))

# ── 3. Deterministic base-year capacity per technology (GW, per region) ───────
topia_stock <- data.frame(
  tech = c("ECOA", "EGAS", "ENUC", "EHYD", "ESOL", "EWIN"),
  gw   = c(6.0,    3.0,    2.0,    5.0,    1.0,    1.0),
  stringsAsFactors = FALSE
)

# ── 4. Store ──────────────────────────────────────────────────────────────────
# (no use_data here: data-raw/topia_assemble.R writes the single dataset)
message("saved: topia_weather (", nrow(topia_weather), "), topia_demand (",
        nrow(topia_demand), "), topia_stock (", nrow(topia_stock), ")")
