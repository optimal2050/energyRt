## data-raw/topia_maps.R
## The four reference layout GEOMETRIES -> a plain `map` list.
##
## Since v0.93 the geometry is not derived here: it is read from
## `data-raw/topia_geom12.rds`, the frozen twelve-region layouts. The freeze
## was settled over a long visual iteration and its record, including how each
## layout was obtained, is `drafts/topia-v1/FROZEN.md` with `frozen_regions.csv`
## and `frozen_links.csv` beside it. The rds carries `region`, `cell` and
## `zone` per layout, so the code assignment travels WITH the polygons instead
## of being re-chosen here.
##
##   squares    the shipped lattice plus one cell at (1,1); `R01` keeps (0,2)
##              and now touches its neighbour at a corner only
##   honeycomb  the shipped lattice plus one cell west of the old `C3`
##   island     the shipped polygons, the old `C1` cut in two
##   continent  replaced by `Rififia`, twelve states natively, cleaned so each
##              state is one piece
##
## `R01` is the remote region: north-west on all four layouts, geometrically
## detached on `squares` and `island`, and it carries no transmission at all.
##
## Coordinates arrive rounded to a 1e-9 grid so that shared vertices are
## byte-identical and GEOS can merge zones and the nation into single shapes;
## the earlier per-hex sin/cos construction left shared edges jittered at
## ~1e-15 and `st_union()` then refused to dissolve them.
##
## BORDERS ARE READ WITH `F***1****`, NOT `F***T****`. The `T` pattern admits
## contact of any dimension, so a corner counts as a border -- which would make
## `R01`'s corner contact on `squares` a border and stop the island being an
## island. Only a shared EDGE is a border here.
##
## Leaves `map` (one `sf` per layout: `region`, `cell`, `zone`, geometry) in the
## builder environment. Nothing is shipped from here: `topia_geoscale.R` folds
## these polygons into the `Geoscale` objects that `topia$geoscales` carries,
## and that is the dataset's ONLY copy of the geometry.
##
## Run: pkgload::load_all(".") ; source("data-raw/topia_assemble.R")

library(sf)

.GEOM <- "data-raw/topia_geom12.rds"
stopifnot(file.exists(.GEOM))
map <- readRDS(.GEOM)

.topia_layouts <- c("squares", "honeycomb", "island", "continent")
stopifnot(identical(sort(names(map)), sort(.topia_layouts)))

# shared EDGE, never a shared corner -- see the note in the header
.edge_borders <- function(x) {
  nb <- sf::st_relate(x, x, pattern = "F***1****")
  reg <- as.character(x$region)
  m <- matrix(FALSE, length(reg), length(reg), dimnames = list(reg, reg))
  for (i in seq_along(nb)) if (length(nb[[i]])) m[i, nb[[i]]] <- TRUE
  diag(m) <- FALSE
  m
}

# `R01` is detached on two layouts and corner-touching on a third, so a
# layout's polygons need not form one piece; every OTHER region does.
.mainland_parts <- function(x) {
  g <- sf::st_geometry(x[as.character(x$region) != "R01", ])
  length(sf::st_cast(sf::st_union(g), "POLYGON"))
}

for (nm in .topia_layouts) {
  x <- map[[nm]]
  stopifnot(
    nrow(x) == 12L,
    identical(as.character(x$region), sprintf("R%02d", 1:12)),
    !anyNA(x$cell), !anyDuplicated(x$cell),
    all(sf::st_geometry_type(sf::st_geometry(x)) == "POLYGON")
  )
  b <- suppressWarnings(.edge_borders(x))
  parts <- suppressMessages(suppressWarnings(.mainland_parts(x)))
  cat(sprintf("%-10s regions %2d  edge borders %2d  R01 degree %d  mainland parts %d\n",
              nm, nrow(x), sum(b) / 2, sum(b["R01", ]), parts))
  stopifnot(parts == 1L)
}

# `R01` carries no transmission, so whatever endowment it holds has to balance
# locally; the geometry states that on the two layouts where it is drawn apart.
.r01_detached <- vapply(map[c("squares", "island")], function(x)
  sum(suppressWarnings(.edge_borders(x))["R01", ]) == 0L, logical(1))
stopifnot(all(.r01_detached))

rm(.GEOM, .r01_detached)
cat("layout geometry read:", paste(.topia_layouts, collapse = ", "), "\n")
