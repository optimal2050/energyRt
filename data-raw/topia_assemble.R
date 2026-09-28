## data-raw/topia_assemble.R
## Builds the single shipped `topia` dataset. THE entry point: run this, not
## the builders below it.
##
##   data-raw/topia_maps.R      -> `map`, the four layout geometries (local)
##   data-raw/topia_geoscale.R  -> topia$geoscales (hierarchy + those polygons)
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

# topia_geoscale.R must follow topia_maps.R: it recodes the regions ON the
# layouts maps.R has just repaired, and builds the matching hierarchy.
.builders <- c("data-raw/topia_maps.R",
               "data-raw/topia_geoscale.R",
               "data-raw/topia_data.R",
               "data-raw/topia_modules.R")
stopifnot(all(file.exists(.builders)))

.e <- new.env(parent = globalenv())
for (.f in .builders) {
  message("-- building: ", .f)
  sys.source(.f, envir = .e)
}

# Each builder leaves one object behind: `topia_maps.R` a plain `map` list that
# `topia_geoscale.R` consumes, `topia_geoscale.R` the Geoscales, the rest one
# table each under its pre-0.90 dataset name.
stopifnot(
  is.list(.e$topia_geoscales), length(.e$topia_geoscales) == 4L,
  !is.null(.e$topia_weather), !is.null(.e$topia_demand),
  !is.null(.e$topia_stock),   !is.null(.e$topia_modules)
)

topia <- list(geoscales = .e$topia_geoscales)
topia$weather <- .e$topia_weather
topia$demand  <- .e$topia_demand
topia$stock   <- .e$topia_stock
topia$modules <- .e$topia_modules

stopifnot(
  identical(names(topia),
            c("geoscales", "weather", "demand", "stock", "modules")),
  all(vapply(topia$geoscales, inherits, logical(1), "geoscales::Geoscale")),
  is.data.frame(topia$weather), is.data.frame(topia$demand),
  is.data.frame(topia$stock), is.list(topia$modules)
)

# `topia$geoscales` is the dataset's ONLY copy of the geometry: the plain
# `topia$map`, the `topia$geo` table and the `topia$modules$maps` duplicate were
# all removed in 0.91. The hierarchy table is still one call away --
# `geoscales::geoscale_leaftable(topia$geoscales$honeycomb)`.
stopifnot(!any(c("map", "geo") %in% names(topia)),
          is.null(topia$modules$maps))

usethis::use_data(topia, overwrite = TRUE)
cat("topia.rda written:", paste(names(topia), collapse = ", "), "|",
    length(topia$geoscales), "layouts:",
    paste(names(topia$geoscales), collapse = ", "), "\n")

rm(.e, .f, .builders)
