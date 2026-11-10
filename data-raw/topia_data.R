## data-raw/topia_data.R
## TOPIA's input profiles, as the two datasets `topia_profiles(dataset =)`
## serves, plus the calendar-agnostic base-year capacity:
##   topia_weather - dataset/calendar/resource/region/timeslice/wval
##   topia_demand  - dataset/calendar/region/timeslice/load
##   topia_stock   - tech/gw
##
##   "simulated"  closed-form shapes on the coarse calendar (s4_h24), built
##                here by topia_profiles_sim.R. A
##                demonstration set: it carries a spatial gradient and a
##                diurnal structure, and is not a resource estimate.
##   "sample"     a measured hourly set on d365_h24, read from
##                topia_profiles_sample.rds. topia_profiles_sample.R writes
##                that file from sources held by the maintainer. The dataset
##                can be rebuilt anywhere, only REFRESHING the sample needs
##                them.
##
## The shipped tables carry region codes and timeslice labels and nothing else:
## no coordinates, no country, no calendar year, no source path. The assertions
## at the end of section 3 enforce that.
##
## Run: pkgload::load_all(".") ; source("data-raw/topia_data.R")

if (!isNamespaceLoaded("energyRt")) library(energyRt)

# ── 1. simulated: the small calendars ────────────────────────────────────────
source("data-raw/topia_profiles_sim.R", local = TRUE)

# ── 2. sample: the measured hourly set ───────────────────────────────────────
.sample_rds <- "data-raw/topia_profiles_sample.rds"
if (!file.exists(.sample_rds)) {
  stop("missing ", .sample_rds, ": run data-raw/topia_profiles_sample.R with ",
       "ENERGYRT_TOPIA_SAMPLE=refresh on a machine that holds the sources.",
       call. = FALSE)
}
topia_sample <- readRDS(.sample_rds)

# ── 3. both datasets, in one table each ──────────────────────────────────────
# `cluster` names the resource grade (wind) or the mounting (solar) a series
# belongs to. The simulated set has one series per resource and so carries NA,
# which reads as "the only one".
# The solar mountings carry the merra2ools array-type codes: `FL` a fixed
# tilted array, `TH` a horizontal single-axis tracker (merra2ools writes them
# lower case, `fl` / `th`; upper case here so they read like the wind grades
# beside them). The committed sample was written with the older `FIX` / `TRK`,
# so it is relabelled as it is read rather than regenerated -- refreshing it
# needs sources only the maintainer holds, and the series are unchanged.
.SOL_MOUNTING <- c(FIX = "FL", TRK = "TH")

.keep_w <- c("dataset", "calendar", "resource", "region", "cluster",
             "timeslice", "wval")
.keep_d <- c("dataset", "calendar", "region", "timeslice", "load")
.tag <- function(x, tag) {
  x <- as.data.frame(x)
  x$dataset <- tag
  if (!"cluster" %in% names(x)) x$cluster <- NA_character_
  cl <- as.character(x$cluster)
  hit <- !is.na(cl) & cl %in% names(.SOL_MOUNTING)
  if (any(hit)) x$cluster[hit] <- unname(.SOL_MOUNTING[cl[hit]])
  x
}

# The resource grades behind the sample's series: one row per (region,
# resource, cluster) with its mean capacity factor, the cells the series is
# built from, and the cells AVAILABLE in that grade -- which is what the
# model's buildable potential is sized from.
topia_grades <- as.data.frame(topia_sample$grades)
# the grades table names the same mountings and is relabelled with them, or
# the model's cluster roster joins against labels the weather no longer uses
# and comes out empty
.gcl <- as.character(topia_grades$cluster)
.ghit <- !is.na(.gcl) & .gcl %in% names(.SOL_MOUNTING)
if (any(.ghit)) topia_grades$cluster[.ghit] <- unname(.SOL_MOUNTING[.gcl[.ghit]])

topia_weather <- rbind(.tag(topia_sim$weather, "simulated")[, .keep_w],
                       .tag(topia_sample$weather, "sample")[, .keep_w])
topia_demand <- rbind(.tag(topia_sim$demand, "simulated")[, .keep_d],
                      .tag(topia_sample$demand, "sample")[, .keep_d])

# 3 decimals, not 4: rounding to 4 put a floor of exactly 1e-4 on the
# capacity factors, and that floor became the smallest coefficient in the LP
# (in `eqTechAfUp` against `vTechCap`) -- four orders below the median entry,
# carrying no physical content, and widening the matrix range an interior
# point method has to work across. At 3 decimals the floor is 1e-3 and the
# ~1.5% of values below it round to zero, where they are dropped as sparse.
topia_weather$wval <- round(as.numeric(topia_weather$wval), 3)
topia_demand$load <- round(as.numeric(topia_demand$load), 4)
for (.k in setdiff(.keep_w, "wval")) {
  topia_weather[[.k]] <- factor(as.character(topia_weather[[.k]]))
}
for (.k in setdiff(.keep_d, "load")) {
  topia_demand[[.k]] <- factor(as.character(topia_demand[[.k]]))
}
rownames(topia_weather) <- NULL
rownames(topia_demand) <- NULL

# nothing in the shipped tables may identify a place or a date
.banned <- c("lon", "lat", "cc", "country", "date", "time", "utc", "year",
             "cell_id", "uparea_km2")
stopifnot(
  !any(.banned %in% names(topia_weather)),
  !any(.banned %in% names(topia_demand)),
  !anyNA(topia_weather$wval), !anyNA(topia_demand$load),
  max(topia_weather$wval) <= 1, min(topia_weather$wval) >= 0,
  min(topia_demand$load) > 0,
  setequal(levels(topia_weather$dataset), c("sample", "simulated")),
  setequal(levels(topia_demand$dataset), c("sample", "simulated")),
  setequal(levels(topia_weather$resource), c("WHYD", "WSOL", "WWIN"))
)
message("topia_weather: ", nrow(topia_weather), " rows (",
        paste(levels(topia_weather$calendar), collapse = ", "), ")")
message("topia_demand:  ", nrow(topia_demand), " rows")

# ── 4. base-year capacity per technology (GW, per region) ────────────────────
topia_stock <- data.frame(
  tech = c("ECOA", "EGAS", "ENUC", "EHYD", "ESOL", "EWIN"),
  gw   = c(6.0,    3.0,    2.0,    5.0,    1.0,    1.0),
  stringsAsFactors = FALSE
)

# ── 5. the build seam ────────────────────────────────────────────────────────
# data-raw/topia_modules.R builds its repositories THROUGH topia_profiles(),
# which reads the shipped dataset. Publish the tables just built so the modules
# are built from THEM and not from the copy being replaced.
# data-raw/topia_assemble.R clears this.
options(energyRt.topia_data = list(geoscales = topia_geoscales,
                                   weather = topia_weather,
                                   demand = topia_demand,
                                   stock = topia_stock))

# ── 6. Store ─────────────────────────────────────────────────────────────────
# (no use_data here: data-raw/topia_assemble.R writes the single dataset)
rm(.sample_rds, .keep_w, .keep_d, .tag, .k, .banned)
