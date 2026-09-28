# topia$geoscales + per-layout region codes ##################################
#
# TOPIA's hierarchy is `nation TOPIA -> zone {WEST, CENTRAL, EAST} -> region`,
# with zone-prefixed region codes: W1-W2, C1-C6, E1-E3.
#
# THE POINT OF THE CODES. Every zone is a single CONTIGUOUS block on every
# layout. The four layouts are different pictures of the same
# model, and they draw the regions in different places, so each one carries its
# OWN assignment of the eleven codes to its eleven polygons -- chosen so the
# zones come out contiguous there. The model never sees the difference: it
# knows only the codes, and `W1` is in WEST whichever layout you draw it on.
#
# This replaces the pre-0.91 arrangement, where the codes were `R1`..`R11`
# placed arbitrarily per layout and the zones were slices of the number line
# (WEST = R1-R3, ...). Those zones were contiguous on NO layout -- 7 of the 12
# zone x layout cells dissolved into multipolygons, which is why zone shapes
# and `plot_trade_map(level = "zone")` label placement were fragile.
#
# HOW THE ASSIGNMENT WAS CHOSEN (frozen below; see data-raw/topia_zones.R for
# the search that produced it):
#   * each zone is one connected block under shared-boundary adjacency, plus
#     one explicit sea link per layout for a region drawn offshore;
#   * WEST sits in the western third of the map, EAST in the eastern third,
#     and neither interleaves across CENTRAL;
#   * blocks are as compact as possible;
#   * CENTRAL is numbered in rows from the SOUTH, west to east inside a row,
#     so C1 is the south-west region on every layout. A tall narrow zone
#     (continent's EAST) is numbered north to south instead.
#
# WHAT THIS FILE SHIPS. One `geoscales::Geoscale` per layout, in
# `topia$geoscales`. Each carries the hierarchy AND that layout's polygons, so
# the dataset holds exactly one copy of the geometry -- there is no separate
# `topia$map` or `topia$geo` any more (0.91 hard break).
#
# Run: pkgload::load_all(".") ; source("data-raw/topia_assemble.R")
#      (this file is sourced by the assembler, after topia_maps.R, and uses
#       the `map` list that file leaves behind)

library(sf)

# -- the hierarchy ------------------------------------------------------------
# Canonical region ORDER, west to east. It is load-bearing twice over: the
# model kits are PREFIXES of it (`R3` = the first three), and the reference
# transmission chain links consecutive pairs.
.topia_regions <- c("W1", "W2", "C1", "C2", "C3", "C4", "C5", "C6",
                    "E1", "E2", "E3")

# Human labels. Assigned to the CODES, not to a layout: the layouts disagree
# about which polygon is which code, so a name lifted from one layout's
# `states` column would contradict the others.
.topia_names <- c(
  W1 = "Oswestia", W2 = "Antidia",
  C1 = "Kopalia",  C2 = "Kalgia", C3 = "Robatia", C4 = "Ramia",
  C5 = "Nishi",    C6 = "Masland",
  E1 = "Sind",     E2 = "Kumalia", E3 = "Neutrals")

geo <- data.frame(
  nation = "TOPIA",
  zone   = c("WEST", "CENTRAL", "EAST")[
              match(substr(.topia_regions, 1, 1), c("W", "C", "E"))],
  region = .topia_regions,
  name   = unname(.topia_names[.topia_regions]),
  stringsAsFactors = FALSE
)

# -- per-layout code assignment ----------------------------------------------
# old `R<n>` code -> new zone-prefixed code, one column per layout.
.topia_recode <- list(
  squares   = c(R1 = "W2", R2 = "C1", R3 = "C2", R4 = "E1", R5 = "E2",
                R6 = "E3", R7 = "C3", R8 = "C4", R9 = "C5", R10 = "W1",
                R11 = "C6"),
  honeycomb = c(R1 = "C1", R2 = "C2", R3 = "C3", R4 = "C4", R5 = "E2",
                R6 = "W1", R7 = "W2", R8 = "C5", R9 = "E1", R10 = "E3",
                R11 = "C6"),
  island    = c(R1 = "C3", R2 = "C4", R3 = "C5", R4 = "E1", R5 = "W2",
                R6 = "C1", R7 = "C2", R8 = "E2", R9 = "C6", R10 = "W1",
                R11 = "E3"),
  continent = c(R1 = "C1", R2 = "C2", R3 = "C3", R4 = "C5", R5 = "E2",
                R6 = "E1", R7 = "C4", R8 = "C6", R9 = "W2", R10 = "W1",
                R11 = "E3")
)

stopifnot(identical(sort(names(.topia_recode)), sort(names(map))))

for (nm in names(map)) {
  m <- map[[nm]]
  cur <- as.character(m$region)
  key <- .topia_recode[[nm]]
  # already recoded (a re-run on a rebuilt dataset) -- leave it alone
  if (all(cur %in% .topia_regions)) next
  stopifnot(setequal(cur, names(key)))
  m$region <- unname(key[cur])
  # the `states` column, where a layout has one, must follow the code
  if ("states" %in% names(m)) m$states <- unname(.topia_names[m$region])
  map[[nm]] <- m
}

# -- verification: every zone is ONE block on EVERY layout --------------------
.sea_link <- function(x, nbl) {           # an offshore region joins its nearest
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
  stopifnot(setequal(reg, .topia_regions))
  nbl <- .sea_link(x, setNames(lapply(st_touches(x), function(i) reg[i]), reg))
  split_zones <- names(which(!vapply(
    split(reg, geo$zone[match(reg, geo$region)]),
    .connected, logical(1), nbl = nbl)))
  cat(sprintf("%-10s zones: %s\n", nm,
      if (length(split_zones)) paste("SPLIT:", paste(split_zones, collapse = ", "))
      else "all contiguous"))
  stopifnot(length(split_zones) == 0L)
}

stopifnot(
  nrow(geo) == 11L, !anyDuplicated(geo$region), !anyNA(geo$name),
  identical(geo$region, .topia_regions)
)

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
  gs <- geoscales::attach_geometry_geoscale(gs, map[[nm]], by = "region",
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
    inherits(gs, "geoscales::Geoscale"),
    identical(geoscales::geoscale_geoframes(gs),
              c("nation", "zone", "region")),
    setequal(geoscales::geoscale_regions(gs, "region"), .topia_regions),
    nrow(geoscales::geoscale_geometry(gs, "zone")) == 3L
  )
}
cat("topia$geoscales: ", length(topia_geoscales), " layouts x ", nrow(geo),
    " regions in ", length(unique(geo$zone)), " zones\n", sep = "")
