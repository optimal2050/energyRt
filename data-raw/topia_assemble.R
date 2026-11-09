## data-raw/topia_assemble.R
## Builds the single shipped `topia` dataset. THE entry point: run this, not
## the builders below it.
##
##   data-raw/topia_maps.R      -> `map`, the four layout geometries (local)
##   data-raw/topia_geoscale.R  -> topia$geoscales (hierarchy + those polygons)
##   data-raw/topia_links.R     -> topia$links, the transmission network read
##                                 off those polygons
##   data-raw/topia_data.R      -> topia_weather, topia_demand, topia_stock,
##                                 topia_grades
##   data-raw/topia_modules.R   -> topia_modules (calendars, horizons, the
##                                 unit kits, and the pre-0.92 region kits)
##   data-raw/topia_model.R     -> topia_model, tangled from
##                                 vignettes/topia-build.Rmd
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
               "data-raw/topia_links.R",
               "data-raw/topia_data.R",
               "data-raw/topia_modules.R",
               "data-raw/topia_model.R")
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
  is.matrix(.e$topia_links), isSymmetric(.e$topia_links),
  !is.null(.e$topia_weather), !is.null(.e$topia_demand),
  !is.null(.e$topia_stock),   !is.null(.e$topia_grades),
  !is.null(.e$topia_modules),
  !is.null(.e$topia_model)
)

topia <- list(geoscales = .e$topia_geoscales)
topia$links   <- .e$topia_links
topia$weather <- .e$topia_weather
topia$demand  <- .e$topia_demand
topia$stock   <- .e$topia_stock
topia$grades  <- .e$topia_grades
# `modules$electricity` IS the vignette's model. The hand-written kits that
# used to live there still back the golden regressions, but they are retiring
# and are not something to point a reader at, so they are written out as a
# TEST FIXTURE rather than shipped in the dataset. When the goldens are
# re-pinned against the current model, this block and the fixture go together.
.golden <- .e$topia_modules$electricity
topia$modules <- .e$topia_modules
topia$modules$electricity <- .e$topia_model

.fixture <- "tests/testthat/fixtures/topia_golden.rds"
stopifnot(setequal(names(.golden), c("R1", "R3", "R7", "R12")))
dir.create(dirname(.fixture), recursive = TRUE, showWarnings = FALSE)
saveRDS(.golden, .fixture, compress = "xz")
cat("golden kits written to", .fixture, "-",
    round(file.size(.fixture) / 1024), "KB (test-only, not shipped)
")

stopifnot(
  identical(names(topia),
            c("geoscales", "links", "weather", "demand", "stock", "grades",
              "modules")),
  # one model, not a list of layouts
  methods::is(topia$modules$electricity$repo, "repository"),
  length(topia$modules$electricity$regions) == 12L,
  # the retiring kits are a test fixture now, not part of the dataset
  is.null(topia$modules$golden),
  all(vapply(topia$geoscales, inherits, logical(1), "geoscales::GeoScale")),
  is.data.frame(topia$weather), is.data.frame(topia$demand),
  is.data.frame(topia$stock), is.list(topia$modules)
)

# `topia$geoscales` is the dataset's ONLY copy of the geometry: the plain
# `topia$map`, the `topia$geo` table and the `topia$modules$maps` duplicate were
# all removed in 0.91. The hierarchy table is still one call away --
# `geoscales::geoscale_leaftable(topia$geoscales$honeycomb)`.
stopifnot(!any(c("map", "geo") %in% names(topia)),
          is.null(topia$modules$maps))

# xz, as the other data-raw drivers use: on this object it is half the
# size of usethis's bzip2 default and faster to write. `LazyData` is on,
# so DESCRIPTION also sets `LazyDataCompression: xz` -- otherwise the
# INSTALLED database is rebuilt with gzip and the saving is lost.
usethis::use_data(topia, overwrite = TRUE, compress = "xz")
cat("topia.rda written:", paste(names(topia), collapse = ", "), "|",
    length(topia$geoscales), "layouts:",
    paste(names(topia$geoscales), collapse = ", "), "\n")

options(energyRt.topia_data = NULL)   # the build seam, set by topia_data.R
rm(.e, .f, .builders, .golden, .fixture)
