## data-raw/utopia_maps.R
## Repair / regenerate the `utopia$map` reference layouts.
##
## The original generation script did not survive; this one rebuilds the
## honeycomb layout from the SHIPPED hex centroids (so the layout and the
## region assignment stay exactly as published) and snaps every layout's
## coordinates to a fixed grid.
##
## Why: the honeycomb hexes were originally built with per-hex sin/cos
## vertices, leaving shared edges jittered at ~1e-15. GEOS then treats
## adjacent hexes as NOT touching, so `st_union()` (geoscales dissolves,
## `geoscale_geometry()`) cannot merge zones/nation into single shapes.
## Rebuilding every hexagon from one global vertex formula and rounding
## coordinates to a 1e-9 grid makes shared vertices byte-identical.
##
## The hierarchy is keyed by region NAME, never by geometry — coordinates
## are per-layout and carry no meaning beyond the picture.
##
## Run: pkgload::load_all(".") ; source("data-raw/utopia_maps.R")

library(sf)

grid_round <- function(g, digits = 9) {
  st_sfc(lapply(g, function(gm) {
    st_polygon(lapply(unclass(gm), function(ring) round(ring, digits)))
  }), crs = st_crs(g))
}

utopia <- energyRt::utopia
map <- utopia$map

# -- honeycomb: rebuild each hex from its centroid ---------------------------
hc <- map$honeycomb
ctr <- suppressWarnings(st_coordinates(st_centroid(st_geometry(hc))))

# Lattice geometry from the shipped centers: neighbor spacing s is the
# smallest center-to-center distance; touching regular hexagons then have
# circumradius r = s / sqrt(3), with an edge FACING each neighbor -- so
# vertex angles sit at theta0 + 30 + 60k degrees, where theta0 is the
# nearest-neighbor direction.
cd <- as.matrix(dist(ctr[, 1:2]))
diag(cd) <- NA
s <- min(cd, na.rm = TRUE)
nn <- which(cd == s, arr.ind = TRUE)[1, ]
theta0 <- atan2(ctr[nn[2], 2] - ctr[nn[1], 2],
                ctr[nn[2], 1] - ctr[nn[1], 1])
r <- s / sqrt(3)

# One global vertex formula, rounded to the grid -- every shared vertex
# computes to the identical number from both neighboring hexes
ang <- theta0 + pi / 6 + pi / 3 * (0:5)
hex_at <- function(cx, cy) {
  v <- cbind(round(cx + r * cos(ang), 9), round(cy + r * sin(ang), 9))
  st_polygon(list(rbind(v, v[1, , drop = FALSE])))
}

# Snap centers to the EXACT integer lattice: origin + i*a1 + j*a2. The
# shipped centroids inherit the original jitter, so shared-vertex
# arithmetic from two raw centers can never coincide; integer lattice
# coordinates make it exact (then the 1e-9 vertex rounding heals fp-eps).
a1 <- s * c(cos(theta0), sin(theta0))
a2 <- s * c(cos(theta0 + pi / 3), sin(theta0 + pi / 3))
B <- cbind(a1, a2)
origin <- ctr[1, 1:2]
ij <- t(solve(B) %*% t(sweep(ctr[, 1:2], 2, origin)))
stopifnot(max(abs(ij - round(ij))) < 0.2)   # centers really sit on a lattice
ij <- round(ij)
cxy <- sweep(ij %*% t(B), 2, origin, `+`)

st_geometry(hc) <- st_sfc(lapply(seq_len(nrow(hc)), function(i) {
  hex_at(cxy[i, 1], cxy[i, 2])
}), crs = st_crs(st_geometry(map$honeycomb)))
map$honeycomb <- hc

# Expected connectivity from the integer lattice (neighbors = distance s)
adj <- as.matrix(dist(cxy)) < s * 1.01 & as.matrix(dist(cxy)) > 0
expected_pairs <- sum(adj) / 2
comp <- local({                              # connected components
  n <- nrow(adj); grp <- seq_len(n)
  repeat {
    new <- vapply(seq_len(n), function(i) min(grp[c(i, which(adj[i, ]))]),
                  integer(1) + 0)
    if (identical(new, grp)) break
    grp <- new
  }
  length(unique(grp))
})

# -- all layouts: snap coordinates to the grid -------------------------------
for (nm in names(map)) {
  st_geometry(map[[nm]]) <- grid_round(st_geometry(map[[nm]]))
}

utopia$map <- map

# -- verification -------------------------------------------------------------
g <- st_geometry(map$honeycomb)
touch_pairs <- sum(lengths(st_touches(g))) / 2
union_parts <- length(st_cast(st_union(g), "POLYGON"))
cat("honeycomb: touching pairs", touch_pairs, "(lattice expects",
    expected_pairs, ") | union parts", union_parts,
    "(lattice components:", comp, ")\n")
stopifnot(touch_pairs == expected_pairs,
          union_parts == comp)

for (nm in names(map)) {
  gg <- st_geometry(map[[nm]])
  cat(sprintf("%-10s touching pairs %2d | union parts %d\n", nm,
              sum(lengths(st_touches(gg))) / 2,
              length(st_cast(st_union(gg), "POLYGON"))))
}

usethis::use_data(utopia, overwrite = TRUE)
cat("utopia.rda updated\n")
