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
  desc = "Four seasons, day/night (8 timeslices)"
)

calendars[["d365"]] <- newCalendar(
  make_timetable(list(YDAY = yday2YDAY(1:365))),
  name = "d365",
  desc = "Daily resolution, 365 days"
)

# ── 1b. Generic and UTOPIA teaching calendars ────────────────────────────────
# UTOPIA reuses the mainstream calendars (Section 2b): `annual`, `s4_h24` and
# `m12_h24` replaced the former utopia_annual/utopia_s4h24/utopia_m12h24
# (2026-08 unification; seasons are WIN/SPR/SUM/FAL, day-proportional
# shares). Only `utopia_seasons` remains UTOPIA-own -- its DAY/NGT/PK
# daypart shares have no catalog twin.

# Annual (single timeslice) — the coarsest resolution.
calendars[["annual"]] <- newCalendar(
  make_timetable(list(ANNUAL = "ANNUAL")),
  name = "annual",
  desc = "Annual resolution (1 timeslice)"
)

# Four seasons x three dayparts (DAY/NIGHT/PEAK), with representative shares
# (peak hours are short; night is longer in winter, day longer in summer).
# 12 timeslices — the default UTOPIA resolution (tractable on GLPK).
calendars[["utopia_seasons"]] <- newCalendar(
  make_timetable(list(
    SEASON = list(
      WIN = list(1 / 4, HOUR = list(DAY =  9 / 24, NGT = 12 / 24, PK = 3 / 24)),
      SPR = list(1 / 4, HOUR = list(DAY = 11 / 24, NGT = 11 / 24, PK = 2 / 24)),
      SUM = list(1 / 4, HOUR = list(DAY = 12 / 24, NGT =  9 / 24, PK = 3 / 24)),
      FAL = list(1 / 4, HOUR = list(DAY = 11 / 24, NGT = 11 / 24, PK = 2 / 24))
    )
  )),
  name = "utopia_seasons",
  desc = "UTOPIA: 4 seasons x 3 dayparts (DAY/NIGHT/PEAK), 12 timeslices"
)

# ── 1c. Unit calendars ───────────────────────────────────────────────────────
# Perfectly symmetric time structures for the `utopia_modules$unit` kits: every
# share is a power of 1/4, so weights are exactly 4 (and 16) and hand-computed
# objectives come out as small integers. See "unit kit" in ?utopia_modules.

calendars[["unit_s4"]] <- newCalendar(
  make_timetable(list(SEASON = paste0("S", 1:4))),
  name = "unit_s4",
  desc = "Unit calendar: 4 equal seasons (share 1/4 each, weight 4)"
)

calendars[["unit_s4h4"]] <- newCalendar(
  make_timetable(list(
    SEASON = paste0("S", 1:4),
    HOUR   = paste0("H", 1:4)
  )),
  name = "unit_s4h4",
  desc = "Unit calendar: 4 seasons x 4 hours, 16 timeslices (share 1/16 each)"
)

# ── 2. Import detailed calendars & horizons from IDEEA (optional) ─────────────
horizons <- list()

# `ideea_modules` is lazy data, not an exported symbol in every IDEEA version
# -- and the rebuilt IDEEA (>= 0.80) ships no datasets at all. Treat "installed
# but no data" the same as "not installed".
ideea <- NULL
if (requireNamespace("IDEEA", quietly = TRUE)) {
  .ide <- new.env()
  suppressWarnings(utils::data("ideea_modules", package = "IDEEA",
                               envir = .ide))
  ideea <- .ide$ideea_modules
}

if (!is.null(ideea)) {
  for (key in names(ideea$calendars)) {
    src <- ideea$calendars[[key]]
    calendars[[.nm(src, key)]] <- rebuild_calendar(src)
  }
  for (key in names(ideea$horizons)) {
    src <- ideea$horizons[[key]]
    horizons[[.nm(src, key)]] <- rebuild_horizon(src)
  }
} else {
  # NON-DESTRUCTIVE fallback: carry the previously shipped (IDEEA-derived)
  # calendars/horizons over from the package's current data files, so
  # regenerating without the IDEEA source never silently drops them.
  message(
    "IDEEA data not available: carrying previously shipped calendars/",
    "horizons over from data/*.rda."
  )
  .prev <- new.env()
  if (file.exists("data/calendars.rda")) load("data/calendars.rda", envir = .prev)
  if (file.exists("data/horizons.rda")) load("data/horizons.rda", envir = .prev)
  # RETIRED entries never come back through the carry-over (2026-08
  # unification: UTOPIA reuses annual / s4_h24 / m12_h24)
  .retired <- c("utopia_annual", "utopia_s4h24", "utopia_m12h24")
  for (key in setdiff(names(.prev$calendars),
                      c(names(calendars), .retired))) {
    calendars[[key]] <- .prev$calendars[[key]]
  }
  for (key in setdiff(names(.prev$horizons), names(horizons))) {
    horizons[[key]] <- .prev$horizons[[key]]
  }
  # Minimal self-contained horizons if there was nothing to carry over.
  if (length(horizons) == 0) {
    horizons[["Y2020_2050_by_5"]] <- newHorizon(
      2020:2050, c(1, rep(5, 6)),
      name = "Y2020_2050_by_5", desc = "2020-2050 by 5 years"
    )
    horizons[["Y2020"]] <- newHorizon(
      2020, name = "Y2020", desc = "one year horizon: 2020"
    )
  }
}

# ── 2b. Mainstream calendars imported from the timescales catalog ────────────
# Built AT DATA-BUILD TIME only (data-raw/ is Rbuildignored), so timescales is
# never a runtime dependency; it sits in Suggests purely to document the
# relationship. The bridge follows the canonical contract (timescales
# tests/testthat/test-regressions.R): leaftable + ANNUAL root, timeframes
# coarsest-first, then `timeslice`, then `share` LAST; year_fraction = the
# leaftable's surviving sum(share). Rows are reordered CHRONOLOGICALLY
# (member order per timeframe, coarsest-major): the timescales leaftable is
# vocabulary-ordered -- hour-major for two-level designs -- while energyRt's
# `next_in_year` follows row order.
#
# Vocabulary note: seasons are WIN/SPR/SUM/FAL in calendar order, with
# DAY-PROPORTIONAL shares (90/92/92/91 days) -- the UTOPIA world was
# unified onto this vocabulary in 2026-08 (the old equal-share
# utopia_s4h24/utopia_m12h24/utopia_annual entries are retired). The
# catalog's d365/d365_h24 are NOT imported: their label sets and shares
# are identical to the entries already shipped above (pinned by the
# stopifnot at the end of this section).

.ts_full <- c("m12", "m12a", "q4", "s4", "s4_h24", "m12_h24",
              "wd7_h24", "w52_h24")
.ts_samples <- c("s4_h24_subset_2seasons", "m12_h24_subset_4months",
                 "m12_subset_q1")

if (requireNamespace("timescales", quietly = TRUE)) {
  .ts_ver <- as.character(utils::packageVersion("timescales"))

  ts_bridge <- function(cal, name, desc = "") {
    lv  <- timescales::calendar_leaftable(cal)
    tfs <- timescales::calendar_timeframes(cal)
    ord <- do.call(order, lapply(tfs, function(tf) {
      match(as.character(lv[[tf]]),
            timescales::calendar_timeslices(cal, tf))
    }))
    lv <- lv[ord, , drop = FALSE]
    lv$ANNUAL <- "ANNUAL"
    tt <- data.table::as.data.table(
      lv[, c("ANNUAL", tfs, "timeslice", "share")]
    )
    newCalendar(name = name, desc = desc, timetable = tt,
                year_fraction = sum(lv$share))
  }

  .ts_cat <- timescales::calendar_catalog()
  ts_desc <- function(id) {
    paste0(.ts_cat$desc[match(id, .ts_cat$id)],
           " (timescales ", .ts_ver, ")")
  }

  for (id in .ts_full) {
    calendars[[id]] <- ts_bridge(timescales::calendar(id),
                                 name = id, desc = ts_desc(id))
  }

  # Sampled designs: timescales::filter_calendar keeps shares UNnormalized,
  # so year_fraction = sum(share) carries over -- the documented sampled-
  # calendar contract (never rebuild a subset with make_timetable()).
  calendars[["s4_h24_subset_2seasons"]] <- ts_bridge(
    timescales::filter_calendar(timescales::calendar("s4_h24"),
                                "SEASON", c("WIN", "SUM")),
    name = "s4_h24_subset_2seasons",
    desc = paste0("s4_h24 sampled to WIN+SUM; year_fraction = the two ",
                  "seasons' share (timescales ", .ts_ver, ")")
  )
  calendars[["m12_h24_subset_4months"]] <- ts_bridge(
    timescales::filter_calendar(timescales::calendar("m12_h24"),
                                "MONTH", c("m01", "m04", "m07", "m10")),
    name = "m12_h24_subset_4months",
    desc = paste0("m12_h24 sampled to m01/m04/m07/m10; year_fraction = ",
                  "the four months' share (timescales ", .ts_ver, ")")
  )
  calendars[["m12_subset_q1"]] <- ts_bridge(
    timescales::filter_calendar(timescales::calendar("m12"),
                                "MONTH", c("m01", "m02", "m03")),
    name = "m12_subset_q1",
    desc = paste0("m12 sampled to Jan-Mar; year_fraction = 90/365 ",
                  "(timescales ", .ts_ver, ")")
  )

  # Non-collision pins for the SKIPPED catalog ids: the shipped d365 /
  # d365_h24 entries are label-identical to the timescales designs, so
  # importing them would change nothing but the desc.
  stopifnot(
    setequal(calendars$d365@timetable$timeslice,
             timescales::calendar_leaftable(
               timescales::calendar("d365"))$timeslice),
    setequal(calendars$d365_h24@timetable$timeslice,
             timescales::calendar_leaftable(
               timescales::calendar("d365_h24"))$timeslice)
  )
} else {
  # NON-DESTRUCTIVE fallback, mirroring the IDEEA block: carry the
  # previously shipped timescales imports over so a timescales-less rebuild
  # never shrinks the dataset.
  message("timescales not installed: carrying previously shipped catalog ",
          "imports over from data/calendars.rda.")
  .prev_ts <- new.env()
  if (file.exists("data/calendars.rda")) {
    load("data/calendars.rda", envir = .prev_ts)
  }
  for (key in setdiff(intersect(c(.ts_full, .ts_samples),
                                names(.prev_ts$calendars)),
                      names(calendars))) {
    calendars[[key]] <- .prev_ts$calendars[[key]]
  }
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
