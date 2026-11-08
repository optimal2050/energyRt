# topia$geoscales + per-layout region codes ##################################
#
# TOPIA's hierarchy is `nation TOPIA -> zone {NW, NE, SW, SE} -> region`, with
# twelve regions coded `R01`-`R12`, three to a zone.
#
# THE POINT OF THE CODES. Every zone is a compact group of touching
# neighbours on every layout, never a slice of the map. The four layouts are
# different pictures of the same model and they draw the regions in different
# places, so each one carries its own assignment of the twelve codes to its
# twelve polygons -- chosen so the zones come out contiguous there. The model
# never sees the difference: it knows only the codes, and `R01` is in `NW`
# whichever layout you draw it on.
#
# `R01` is the remote region. It is north-west on all four layouts, detached on
# `squares` and `island`, and it carries no transmission at all, so its
# endowment has to balance locally in every hour.
#
# The assignment is NOT chosen here. It arrives with the polygons in
# `data-raw/topia_geom12.rds` (`region`, `cell`, `zone`), which is the frozen
# twelve-region geometry. This replaces the eleven-region arrangement with
# zone-prefixed codes
# (`W1`-`W2`, `C1`-`C6`, `E1`-`E3`) and, before that, zones that were slices
# of the number line and contiguous on no layout at all.
#
# NAMES ARE DEFERRED. The labels are placeholders: naming the regions is a
# separate decision.
#
# WHAT THIS FILE SHIPS. One `geoscales::GeoScale` per layout, in
# `topia$geoscales`. Each carries the hierarchy AND that layout's polygons, so
# the dataset holds exactly one copy of the geometry -- there is no separate
# `topia$map` or `topia$geo`.
#
# Run: pkgload::load_all(".") ; source("data-raw/topia_assemble.R")
#      (sourced by the assembler, after topia_maps.R, and uses the `map` list
#       that file leaves behind)

library(sf)

# -- the hierarchy ------------------------------------------------------------
# Canonical region ORDER. It is load-bearing twice over: the model kits are
# PREFIXES of it (`R3` = the first three, which is now one whole zone), and
# the reference transmission chain links consecutive pairs.
.topia_regions <- sprintf("R%02d", 1:12)

.topia_zones <- c("NW", "NE", "SW", "SE")

# Zone membership is a property of the CODE, identical on every layout; the
# geometry file states it per layout and the agreement is checked below.
.topia_zone_of <- rep(.topia_zones, each = 3L)
names(.topia_zone_of) <- .topia_regions

# Placeholder labels -- see "NAMES ARE DEFERRED" above.
.topia_names <- stats::setNames(
  paste("Region", substr(.topia_regions, 2, 3)), .topia_regions)

geo <- data.frame(
  nation = "TOPIA",
  zone   = unname(.topia_zone_of[.topia_regions]),
  region = .topia_regions,
  name   = unname(.topia_names[.topia_regions]),
  stringsAsFactors = FALSE
)

stopifnot(
  nrow(geo) == 12L, !anyDuplicated(geo$region), !anyNA(geo$name),
  identical(geo$region, .topia_regions),
  identical(sort(unique(geo$zone)), sort(.topia_zones)),
  all(table(geo$zone) == 3L)
)

# -- the geometry agrees with the hierarchy ----------------------------------
for (nm in names(map)) {
  m <- map[[nm]]
  stopifnot(
    setequal(as.character(m$region), .topia_regions),
    identical(unname(.topia_zone_of[as.character(m$region)]),
              as.character(m$zone))
  )
}

# -- verification: every zone is ONE block on EVERY layout -------------------
# `R01` is alone in no zone, so a detached `R01` would split `NW`: it is
# joined to the nearest region for THIS check only. The check is about the
# picture being readable, not about what the model may ship, and the model
# ships no link to `R01` at all.
.sea_link <- function(x, nbl) {
  reg <- as.character(x$region)
  d <- st_distance(x); units(d) <- NULL
  for (i in which(lengths(nbl) == 0)) {
    v <- d[i, ]; v[i] <- Inf; j <- which.min(v)
    nbl[[reg[i]]] <- c(nbl[[reg[i]]], reg[j])
    nbl[[reg[j]]] <- c(nbl[[reg[j]]], reg[i])
  }
  nbl
}
.connected <- function(m, nbl) {
  if (length(m) <= 1) return(TRUE)
  seen <- m[1]; fr <- m[1]
  while (length(fr)) {
    nx <- setdiff(intersect(unlist(nbl[fr]), m), seen); seen <- c(seen, nx); fr <- nx
  }
  setequal(seen, m)
}
for (nm in names(map)) {
  x <- map[[nm]]; reg <- as.character(x$region)
  nb <- suppressMessages(suppressWarnings(
    st_relate(x, x, pattern = "F***1****")))
  nbl <- stats::setNames(lapply(nb, function(i) reg[i]), reg)
  nbl <- suppressMessages(.sea_link(x, nbl))
  split_zones <- names(which(!vapply(
    split(reg, geo$zone[match(reg, geo$region)]),
    .connected, logical(1), nbl = nbl)))
  cat(sprintf("%-10s zones: %s\n", nm,
      if (length(split_zones)) paste("SPLIT:", paste(split_zones, collapse = ", "))
      else "all contiguous"))
  stopifnot(length(split_zones) == 0L)
}

# -- the shipped objects: one Geoscale per layout ----------------------------
# The hierarchy is identical in all four; only the attached polygons differ.
# `area` is planar (the layouts carry no CRS), which is the honest measure for
# a synthetic map, so the warning is muffled rather than left to fire.
topia_geoscales <- lapply(stats::setNames(nm = names(map)), function(nm) {
  gs <- geoscales::geoscale_from_leaftable(
    geo,
    geoframes = c("nation", "zone", "region"),
    key = "region",
    weights = character(),
    name = paste0("topia_", nm),
    desc = paste0("TOPIA reference regions (", nm,
                  " layout), nested nation -> zone -> region"),
    labels = "name"
  )
  gs <- geoscales::attach_geometry_geoscale(gs, map[[nm]][, "region"],
                                            by = "region",
                                            geoframe = "region")
  withCallingHandlers(
    gs <- geoscales::add_area_geoscale(gs, name = "area"),
    warning = function(w) {
      if (grepl("no CRS", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
  gs
})

for (nm in names(topia_geoscales)) {
  gs <- topia_geoscales[[nm]]
  stopifnot(
    inherits(gs, "geoscales::GeoScale"),
    identical(geoscales::geoscale_geoframes(gs),
              c("nation", "zone", "region")),
    setequal(geoscales::geoscale_regions(gs, "region"), .topia_regions),
    nrow(geoscales::geoscale_geometry(gs, "zone")) == 4L
  )
}

# -- the hierarchy a MODEL is solved on --------------------------------------
# The same nation -> zone -> region table with NO geometry: `setGeoscale()`
# only needs the membership, and the kits narrow this to their own regions.
# Keeping it separate means a model carries no polygons, and the four layouts'
# maps stay the dataset's single copy of the geometry.
topia_model_geoscale <- geoscales::geoscale_from_leaftable(
  geo,
  geoframes = c("nation", "zone", "region"),
  key = "region",
  weights = character(),
  name = "topia",
  desc = "TOPIA hierarchy (nation -> zone -> region), no geometry",
  labels = "name"
)
stopifnot(
  inherits(topia_model_geoscale, "geoscales::GeoScale"),
  identical(geoscales::geoscale_geoframes(topia_model_geoscale),
            c("nation", "zone", "region"))
)

cat("topia$geoscales: ", length(topia_geoscales), " layouts x ", nrow(geo),
    " regions in ", length(unique(geo$zone)), " zones\n", sep = "")
