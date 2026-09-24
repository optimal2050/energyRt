# topia$geo -- the TOPIA region hierarchy ####################################
#
# A plain data.frame, NOT a `geoscales::Geoscale`. `geoscales` is a Suggests
# dependency, so shipping one of its objects inside a lazy-loaded dataset would
# put a class that may not be installed into `data/`. `topia_geoscale()`
# (R/topia-geoscale.R) builds the object on demand instead.
#
# The hierarchy is keyed by region NAME, never by geometry. The four layouts in
# `topia$map` place R1..R11 differently on purpose -- "a model built for one
# layout draws over any of them" -- so any coordinate-derived grouping would be
# valid for one layout and wrong for the other three.
#
# Run with:  source("data-raw/topia_geoscale.R")

library(sf)

load("data/topia.rda")

geo <- data.frame(
  nation = "TOPIA",
  zone   = c("WEST", "WEST", "WEST",
             "CENTRAL", "CENTRAL", "CENTRAL", "CENTRAL",
             "EAST", "EAST", "EAST", "EAST"),
  region = paste0("R", 1:11),
  stringsAsFactors = FALSE
)

# Human-readable names, taken from the `states` column that the island and
# continent layouts carry (R1 is Oswestia in both).
isl <- as.data.frame(topia$map$island)
geo$name <- isl$states[match(geo$region, isl$region)]

stopifnot(
  setequal(geo$region, topia$map$honeycomb$region),
  !anyDuplicated(geo$region),
  nrow(geo) == 11L,
  !anyNA(geo$name)
)

topia$geo <- geo
save(topia, file = "data/topia.rda", compress = "xz")
