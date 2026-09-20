## data-raw/utopia_assemble.R
## Builds the single shipped `utopia` dataset. THE entry point: run this, not
## the builders below it.
##
##   data-raw/utopia_maps.R      -> utopia (map, geo)
##   data-raw/utopia_data.R      -> utopia_weather, utopia_demand, utopia_stock
##   data-raw/utopia_modules.R   -> utopia_modules
##
## The builders define their objects and write nothing; this script sources
## them into one environment and writes `data/utopia.rda` once. Before v0.90
## each builder shipped its own dataset and this script read them back through
## `energyRt::` -- so regenerating the data depended on the previously SHIPPED
## copy of that same data. The satellite datasets were removed in v0.90
## (their content lives on as `utopia$weather` etc.), which is what let the
## cycle be cut.
##
## Run: pkgload::load_all(".") ; source("data-raw/utopia_assemble.R")

if (!isNamespaceLoaded("energyRt")) library(energyRt)

.builders <- c("data-raw/utopia_maps.R",
               "data-raw/utopia_data.R",
               "data-raw/utopia_modules.R")
stopifnot(all(file.exists(.builders)))

.e <- new.env(parent = globalenv())
for (.f in .builders) {
  message("-- building: ", .f)
  sys.source(.f, envir = .e)
}

# `utopia_maps.R` leaves the map/geo pair in `utopia`; the others leave one
# object each under its pre-0.90 dataset name.
stopifnot(
  is.list(.e$utopia), all(c("map", "geo") %in% names(.e$utopia)),
  !is.null(.e$utopia_weather), !is.null(.e$utopia_demand),
  !is.null(.e$utopia_stock),   !is.null(.e$utopia_modules)
)

utopia <- .e$utopia[c("map", "geo")]
utopia$weather <- .e$utopia_weather
utopia$demand  <- .e$utopia_demand
utopia$stock   <- .e$utopia_stock
utopia$modules <- .e$utopia_modules

stopifnot(
  identical(names(utopia),
            c("map", "geo", "weather", "demand", "stock", "modules")),
  is.list(utopia$map), is.data.frame(utopia$geo),
  is.data.frame(utopia$weather), is.data.frame(utopia$demand),
  is.data.frame(utopia$stock), is.list(utopia$modules)
)

usethis::use_data(utopia, overwrite = TRUE)
cat("utopia.rda written:", paste(names(utopia), collapse = ", "), "\n")
rm(.e, .f, .builders)
