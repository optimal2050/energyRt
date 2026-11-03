## data-raw/topia_links.R
## TOPIA's transmission network, derived from the geometry rather than listed
## by hand -> `topia_links`, a symmetric logical region-by-region matrix.
##
## A link between two regions that share a BORDER cannot cross another link,
## so adjacency is what keeps the drawn network planar. TOPIA has four map
## layouts of the same eleven regions and they do not agree on which regions
## touch, so there is no set of borders that is a border in all four:
##
##   layout     borders  W1 degree
##   squares         18          0
##   honeycomb       18          1
##   island          16          0
##   continent       16          1
##
## `W1` is a detached island in `squares` and `island`. Since the network has
## to reach it, the rule is a MAJORITY one: a link is kept when the two
## regions share a border in at least `.MIN_LAYOUTS` of the four layouts, so
## every link is a real border on most maps and crosses on at most one. `W1`
## is then joined to `W2` -- its only neighbour anywhere.
##
## Geometry proposes; the modeller disposes. `.ISLAND_LINK` is a link the
## borders do not give and the model needs, `.DROP_LINKS` are links the
## borders do give and the model does not want -- a corridor is a decision
## about the system, not only about which hexagons touch. Both are named here
## rather than derived, so the departures from the rule are visible in one
## place and the connectivity check below sees the network as it ships.
##
## Run from `data-raw/topia_assemble.R`, after `data-raw/topia_geoscale.R`.
## Needs `sf`; the result is shipped so that building the model does not.

if (!isNamespaceLoaded("energyRt")) library(energyRt)
stopifnot(requireNamespace("sf", quietly = TRUE))

.MIN_LAYOUTS <- 3L
.ISLAND_LINK <- c("W1", "W2")
# pairs the majority rule admits but the network does without: they
# triangulate the central cluster rather than extend it
.DROP_LINKS <- list(c("C2", "C4"), c("C4", "C6"), c("C1", "C3"))

# the canonical west-to-east order, which the matrix is keyed on
.link_regions <- as.character(
  geoscales::geoscale_regions(topia_geoscales[[1]], geoframe = "region"))

# Border matrix of one layout: TRUE where two regions touch along a line.
# "F***T****" is the DE-9IM pattern for interiors disjoint but boundaries
# meeting -- a shared edge, not a shared corner.
.borders <- function(gs) {
  g <- sf::st_as_sf(geoscales::geoscale_geometry(gs, geoframe = "region"))
  key <- intersect(c("region", "name", "id"), names(g))[1]
  reg <- as.character(g[[key]])
  nb <- sf::st_relate(g, g, pattern = "F***T****")
  m <- matrix(FALSE, length(reg), length(reg), dimnames = list(reg, reg))
  for (i in seq_along(nb)) if (length(nb[[i]])) m[i, nb[[i]]] <- TRUE
  diag(m) <- FALSE
  m[.link_regions, .link_regions]
}

.counts <- Reduce(`+`, lapply(topia_geoscales, function(gs) .borders(gs) * 1L))
topia_links <- .counts >= .MIN_LAYOUTS
topia_links[.ISLAND_LINK[1], .ISLAND_LINK[2]] <- TRUE
topia_links[.ISLAND_LINK[2], .ISLAND_LINK[1]] <- TRUE
for (.d in .DROP_LINKS) {
  if (!all(.d %in% .link_regions)) {
    stop("`.DROP_LINKS` names a region that is not on the map: ",
         paste(setdiff(.d, .link_regions), collapse = ", "), call. = FALSE)
  }
  topia_links[.d[1], .d[2]] <- FALSE
  topia_links[.d[2], .d[1]] <- FALSE
}

# A network that leaves a region stranded is a modelling error, not a layout
# quirk: that region could neither import nor export and would have to meet
# its own demand in isolation.
.reach <- function(m) {
  seen <- 1L
  repeat {
    nxt <- setdiff(which(apply(m[seen, , drop = FALSE], 2, any)), seen)
    if (!length(nxt)) break
    seen <- c(seen, nxt)
  }
  seen
}
.unreached <- setdiff(seq_along(.link_regions), .reach(topia_links))
if (length(.unreached)) {
  stop("the transmission network leaves region(s) unreachable: ",
       paste(.link_regions[.unreached], collapse = ", "),
       ". Lower `.MIN_LAYOUTS` or name another exception.", call. = FALSE)
}

stopifnot(
  isSymmetric(topia_links),
  !any(diag(topia_links)),
  identical(dimnames(topia_links), list(.link_regions, .link_regions))
)
message("topia_links: ", sum(topia_links) / 2, " links over ",
        length(.link_regions), " regions (>= ", .MIN_LAYOUTS,
        " of ", length(topia_geoscales), " layouts, plus ",
        paste(.ISLAND_LINK, collapse = "-"), ")")

rm(.MIN_LAYOUTS, .ISLAND_LINK, .DROP_LINKS, .d, .link_regions, .borders,
   .counts, .reach, .unreached)
