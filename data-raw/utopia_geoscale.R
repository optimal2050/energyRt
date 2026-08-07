# utopia$geo -- the UTOPIA region hierarchy ####################################
#
# A plain data.frame, NOT a `geoscales::Geoscale`. `geoscales` is a Suggests
# dependency, so shipping one of its objects inside a lazy-loaded dataset would
# put a class that may not be installed into `data/`. `utopia_geoscale()`
# (R/utopia-geoscale.R) builds the object on demand instead.
#
# The hierarchy is keyed by region NAME, never by geometry. The four layouts in
# `utopia$map` place R1..R11 differently on purpose -- "a model built for one
# layout draws over any of them" -- so any coordinate-derived grouping would be
# valid for one layout and wrong for the other three.
#
# Run with:  source("data-raw/utopia_geoscale.R")

library(sf)

load("data/utopia.rda")

geo <- data.frame(
  nation = "UTOPIA",
  zone   = c("WEST", "WEST", "WEST",
             "CENTRAL", "CENTRAL", "CENTRAL", "CENTRAL",
             "EAST", "EAST", "EAST", "EAST"),
  region = paste0("R", 1:11),
  stringsAsFactors = FALSE
)

# Human-readable names, taken from the `states` column that the island and
# continent layouts carry (R1 is Oswestia in both).
isl <- as.data.frame(utopia$map$island)
geo$name <- isl$states[match(geo$region, isl$region)]

stopifnot(
  setequal(geo$region, utopia$map$honeycomb$region),
  !anyDuplicated(geo$region),
  nrow(geo) == 11L,
  !anyNA(geo$name)
)

utopia$geo <- geo
save(utopia, file = "data/utopia.rda", compress = "xz")
