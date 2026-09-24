## data-raw/topia_assemble.R
## Builds the single shipped `topia` dataset. THE entry point: run this, not
## the builders below it.
##
##   data-raw/topia_maps.R      -> topia (map, geo)
##   data-raw/topia_data.R      -> topia_weather, topia_demand, topia_stock
##   data-raw/topia_modules.R   -> topia_modules
##
## The builders define their objects and write nothing; this script sources
## them into one environment and writes `data/topia.rda` once. Before v0.90
## each builder shipped its own dataset and this script read them back through
## `energyRt::` -- so regenerating the data depended on the previously SHIPPED
## copy of that same data. The satellite datasets were removed in v0.90
## (their content lives on as `topia$weather` etc.), which is what let the
## cycle be cut.
##
## Run: pkgload::load_all(".") ; source("data-raw/topia_assemble.R")

if (!isNamespaceLoaded("energyRt")) library(energyRt)

.builders <- c("data-raw/topia_maps.R",
               "data-raw/topia_data.R",
               "data-raw/topia_modules.R")
stopifnot(all(file.exists(.builders)))

.e <- new.env(parent = globalenv())
for (.f in .builders) {
  message("-- building: ", .f)
  sys.source(.f, envir = .e)
}

# `topia_maps.R` leaves the map/geo pair in `topia`; the others leave one
# object each under its pre-0.90 dataset name.
stopifnot(
  is.list(.e$topia), all(c("map", "geo") %in% names(.e$topia)),
  !is.null(.e$topia_weather), !is.null(.e$topia_demand),
  !is.null(.e$topia_stock),   !is.null(.e$topia_modules)
)

topia <- .e$topia[c("map", "geo")]
topia$weather <- .e$topia_weather
topia$demand  <- .e$topia_demand
topia$stock   <- .e$topia_stock
topia$modules <- .e$topia_modules

stopifnot(
  identical(names(topia),
            c("map", "geo", "weather", "demand", "stock", "modules")),
  is.list(topia$map), is.data.frame(topia$geo),
  is.data.frame(topia$weather), is.data.frame(topia$demand),
  is.data.frame(topia$stock), is.list(topia$modules)
)

usethis::use_data(topia, overwrite = TRUE)
cat("topia.rda written:", paste(names(topia), collapse = ", "), "\n")
rm(.e, .f, .builders)
