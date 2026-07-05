## data-raw/calendars.R
## Recreate the package's example time-structure data:
##   data/calendars.rda  — named list of `calendar` objects
##   data/horizons.rda   — named list of `horizon` objects
##
## Run with the *development* version of energyRt loaded so the stored S4
## objects match the current class definitions, then re-document:
##   pkgload::load_all(".")
##   source("data-raw/calendars.R")
##   devtools::document()
##
## Detailed hourly calendars and milestone horizons are imported from the IDEEA
## package (https://github.com/optimal2050/IDEEA) when it is installed; a few
## small, self-contained calendars/horizons are always built from energyRt's own
## constructors so the datasets never depend on IDEEA being available.

if (!isNamespaceLoaded("energyRt")) library(energyRt)
library(usethis)

# ── helpers ──────────────────────────────────────────────────────────────────
# Rebuild calendar/horizon objects through the *current* energyRt constructors,
# so the stored objects are guaranteed valid under the current class
# definitions regardless of which energyRt version produced the source (the
# imported IDEEA objects may have been built against an older version).

rebuild_calendar <- function(cal) {
  newCalendar(
    # `.complete_calendar()` uses data.table semantics on @timetable
    timetable     = data.table::as.data.table(cal@timetable),
    year_fraction = cal@year_fraction,
    name          = if (length(cal@name)) cal@name else "",
    desc          = if (length(cal@desc)) cal@desc else ""
  )
}

rebuild_horizon <- function(hor) {
  newHorizon(
    period    = hor@period,
    intervals = as.data.frame(hor@intervals),
    name      = if (length(hor@name) && nzchar(hor@name)) hor@name else NULL,
    desc      = if (length(hor@desc) && nzchar(hor@desc)) hor@desc else NULL
  )
}

# name a list element by the object's own @name, else the cleaned source key
.nm <- function(obj, key) {
  if (length(obj@name) == 1 && nzchar(obj@name)) {
    obj@name
  } else {
    sub("^(calendar|horizon)_", "", key)
  }
}

# ── 1. Small, self-contained calendars (always available) ────────────────────
calendars <- list()

calendars[["season_dn"]] <- newCalendar(
  make_timetable(list(
    SEASON = c("WINTER", "SPRING", "SUMMER", "AUTUMN"),
    DAY    = c("DAY", "NIGHT")
  )),
  name = "season_dn",
  desc = "Four seasons, day/night (8 slices)"
)

calendars[["d365"]] <- newCalendar(
  make_timetable(list(YDAY = yday2YDAY(1:365))),
  name = "d365",
  desc = "Daily resolution, 365 days"
)

# ── 1b. UTOPIA teaching calendars ────────────────────────────────────────────
# Used by the UTOPIA vignette (previously built inline). Their construction is
# demonstrated in the "time-resolution" article.

# Annual (single slice) — the coarsest resolution.
calendars[["utopia_annual"]] <- newCalendar(
  make_timetable(list(ANNUAL = "ANNUAL")),
  name = "utopia_annual",
  desc = "UTOPIA: annual resolution (1 slice)"
)

# Four seasons x three dayparts (DAY/NIGHT/PEAK), with representative shares
# (peak hours are short; night is longer in winter, day longer in summer).
# 12 slices — the default UTOPIA resolution (tractable on GLPK).
calendars[["utopia_seasons"]] <- newCalendar(
  make_timetable(list(
    SEASON = list(
      WIN = list(1 / 4, HOUR = list(DAY =  9 / 24, NGT = 12 / 24, PK = 3 / 24)),
      SPR = list(1 / 4, HOUR = list(DAY = 11 / 24, NGT = 11 / 24, PK = 2 / 24)),
      SUM = list(1 / 4, HOUR = list(DAY = 12 / 24, NGT =  9 / 24, PK = 3 / 24)),
      AUT = list(1 / 4, HOUR = list(DAY = 11 / 24, NGT = 11 / 24, PK = 2 / 24))
    )
  )),
  name = "utopia_seasons",
  desc = "UTOPIA: 4 seasons x 3 dayparts (DAY/NIGHT/PEAK), 12 slices"
)

# Four seasons x 24 hours (equal shares), 96 slices — the DEFAULT UTOPIA base
# case: full diurnal detail (24 h, so storage cycles) at a tractable size.
calendars[["utopia_s4h24"]] <- newCalendar(
  make_timetable(list(
    SEASON = c("WIN", "SPR", "SUM", "AUT"),
    HOUR   = paste0("h", formatC(0:23, width = 2, flag = "0"))
  )),
  name = "utopia_s4h24",
  desc = "UTOPIA: 4 seasons x 24 hours, 96 slices (default base case)"
)

# Twelve months x 24 hours (equal shares), 288 slices — the higher-resolution
# UTOPIA option for the load curve / renewable-profile detail.
calendars[["utopia_m12h24"]] <- newCalendar(
  make_timetable(list(
    MONTH = paste0("m", formatC(1:12, width = 2, flag = "0")),
    HOUR  = paste0("h", formatC(0:23, width = 2, flag = "0"))
  )),
  name = "utopia_m12h24",
  desc = "UTOPIA: 12 months x 24 hours, 288 slices"
)

# ── 2. Import detailed calendars & horizons from IDEEA (optional) ─────────────
horizons <- list()

if (requireNamespace("IDEEA", quietly = TRUE)) {
  ideea <- IDEEA::ideea_modules

  for (key in names(ideea$calendars)) {
    src <- ideea$calendars[[key]]
    calendars[[.nm(src, key)]] <- rebuild_calendar(src)
  }
  for (key in names(ideea$horizons)) {
    src <- ideea$horizons[[key]]
    horizons[[.nm(src, key)]] <- rebuild_horizon(src)
  }
} else {
  message(
    "Package 'IDEEA' is not installed: skipping IDEEA calendars/horizons.\n",
    "Install it with pak::pak('optimal2050/IDEEA') to include them."
  )
  # Minimal self-contained horizons so `horizons` is never empty.
  horizons[["Y2020_2050_by_5"]] <- newHorizon(
    2020:2050, c(1, rep(5, 6)),
    name = "Y2020_2050_by_5", desc = "2020-2050 by 5 years"
  )
  horizons[["Y2020"]] <- newHorizon(
    2020, name = "Y2020", desc = "one year horizon: 2020"
  )
}

# ── 3. Validate & store ──────────────────────────────────────────────────────
stopifnot(
  length(calendars) > 0,
  all(vapply(calendars, methods::is, logical(1), class2 = "calendar")),
  all(vapply(horizons,  methods::is, logical(1), class2 = "horizon"))
)
invisible(lapply(calendars, validObject))
invisible(lapply(horizons,  validObject))

message("calendars: ", paste(names(calendars), collapse = ", "))
message("horizons:  ", paste(names(horizons),  collapse = ", "))

usethis::use_data(calendars, horizons, overwrite = TRUE)
