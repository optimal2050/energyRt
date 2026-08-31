## data-raw/utopia_assemble.R
## Folds the UTOPIA satellite datasets into the single `utopia` list.
##
## Run order (each script writes its own .rda first):
##   1. data-raw/utopia_maps.R      -> utopia (map, geo)
##   2. data-raw/utopia_data.R      -> utopia_weather, utopia_demand, utopia_stock
##   3. data-raw/utopia_modules.R   -> utopia_modules
##   4. data-raw/utopia_assemble.R  -> utopia (map, geo, weather, demand, stock,
##                                             modules)   <- this file
##
## The four satellite datasets stay shipped until v0.90 so existing code keeps
## working; they are documented under one internal topic and deleted with the
## rest of the deprecation layer.
## Run: pkgload::load_all(".") ; source("data-raw/utopia_assemble.R")

if (!isNamespaceLoaded("energyRt")) library(energyRt)

utopia <- energyRt::utopia
utopia <- utopia[c("map", "geo")]          # drop any previous fold, stay idempotent
utopia$weather <- energyRt::utopia_weather
utopia$demand  <- energyRt::utopia_demand
utopia$stock   <- energyRt::utopia_stock
utopia$modules <- energyRt::utopia_modules

stopifnot(
  identical(names(utopia),
            c("map", "geo", "weather", "demand", "stock", "modules")),
  is.list(utopia$map), is.data.frame(utopia$geo),
  is.data.frame(utopia$weather), is.data.frame(utopia$demand),
  is.data.frame(utopia$stock), is.list(utopia$modules)
)

usethis::use_data(utopia, overwrite = TRUE)
cat("utopia.rda folded:", paste(names(utopia), collapse = ", "), "\n")
