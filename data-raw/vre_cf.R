# Hourly wind and solar capacity factors for one anonymous site ##############
#
# Source: NASA MERRA-2 reanalysis for one location, one calendar year, as
# shipped in `timescales::merra2_cities` (hourly `W50M` wind speed at 50 m,
# `SWGDN` downward shortwave irradiance, `T10M` air temperature).
#
# The site is DELIBERATELY UNLABELLED. The example needs one pair of strongly
# seasonal, mutually complementary profiles, not a claim about a particular
# place, so the shipped table records no name, coordinates or country. The site
# chosen is high-latitude: its solar resource swings by more than an order of
# magnitude between midwinter and midsummer while its wind resource peaks in
# winter. That mismatch is the whole point -- it is what forces a store to hold
# energy for months rather than hours, which is what `storage_duration()` is
# for. A site with flat solar and no wind produces a chart with nothing in it.
#
# Conversions:
#   wind  -- `merra2ools::fWPC()`, a standard turbine power curve on the 50 m
#            wind speed (cut-in 3 m/s, cut-out 25 m/s).
#   solar -- global horizontal irradiance relative to the 1000 W/m2 standard
#            test condition. A first-order capacity factor for a horizontal
#            panel: it needs no coordinates, and it preserves the diurnal and
#            seasonal shape, which is all the storage example depends on. Use
#            `merra2ools::fPVCF()` on plane-of-array irradiance for tilt,
#            tracking and temperature effects.
#
# Run from the package root (needs `timescales` and `merra2ools`):
#   source("data-raw/vre_cf.R")
##############################################################################

SRC <- "C:/Users/admin/Documents/R/timescales/data/merra2_cities.rda"
SRC_KEY <- "Helsinki"   # build-time only; not carried into the result

.src <- new.env()
load(SRC, envir = .src)
raw <- as.data.frame(.src$merra2_cities)
raw <- raw[raw$city == SRC_KEY, , drop = FALSE]

# MERRA-2 stamps each hour at its midpoint (00:30, 01:30, ...).
yday <- as.integer(format(raw$datetime, "%j"))
hour <- as.integer(format(raw$datetime, "%H"))
stopifnot(nrow(raw) == 8760L, max(yday) == 365L,
          setequal(hour, 0:23))

vre_cf <- data.frame(
  slice = sprintf("d%03d_h%02d", yday, hour),
  solar = pmin(pmax(raw$SWGDN / 1000, 0), 1),
  wind  = pmin(pmax(merra2ools::fWPC(raw$W50M), 0), 1),
  stringsAsFactors = FALSE
)
vre_cf <- vre_cf[order(vre_cf$slice), , drop = FALSE]
rownames(vre_cf) <- NULL

stopifnot(
  nrow(vre_cf) == 8760L,
  identical(names(vre_cf), c("slice", "solar", "wind")),
  !anyNA(vre_cf$solar), !anyNA(vre_cf$wind),
  all(vre_cf$solar >= 0), all(vre_cf$solar <= 1),
  all(vre_cf$wind >= 0), all(vre_cf$wind <= 1),
  !anyDuplicated(vre_cf$slice),
  # nothing identifying survives
  !any(grepl(SRC_KEY, unlist(vre_cf), fixed = TRUE))
)

mon <- as.integer(substr(vre_cf$slice, 2, 4)) %/% 31 + 1L
message("vre_cf: ", nrow(vre_cf), " rows; mean solar ",
        round(mean(vre_cf$solar), 3), ", mean wind ",
        round(mean(vre_cf$wind), 3))
message("solar min/max monthly mean: ",
        paste(round(range(tapply(vre_cf$solar, mon, mean)), 3), collapse = " .. "))

usethis::use_data(vre_cf, overwrite = TRUE, compress = "xz")
