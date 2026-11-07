## data-raw/topia_links.R
## TOPIA's transmission network -> `topia_links`, a symmetric logical
## region-by-region matrix.
##
## The network is LISTED here rather than derived. It was settled with the
## geometry (`drafts/topia-v1/FROZEN.md`, `frozen_links.csv`) in three steps:
## a majority rule over the four layouts' borders, then a greedy pruning of
## every link that crosses another when the network is drawn on the continent,
## then one corridor dropped by hand. Re-running that derivation here would
## re-decide a frozen question, so the result is written out and the geometry
## is used to CHECK it: every corridor must be a real border on at least
## `.MIN_LAYOUTS` of the four maps.
##
## `R01` carries NO transmission. It is the remote region, detached on two
## layouts, and it meets its own demand in every hour from local resources and
## storage -- so the usual "a stranded region is a modelling error" check is
## inverted for it: `R01` must have no links, and every OTHER region must be
## reachable from the mainland.
##
## Borders are read with `F***1****`, a shared EDGE. The earlier `F***T****`
## admitted contact of any dimension, so a corner counted as a border; on the
## frozen `squares` layout that would turn `R01`'s corner contact into a
## corridor and stop the island being an island.
##
## Run from `data-raw/topia_assemble.R`, after `data-raw/topia_geoscale.R`.
## Needs `sf`; the result is shipped so that building the model does not.

if (!isNamespaceLoaded("energyRt")) library(energyRt)
stopifnot(requireNamespace("sf", quietly = TRUE))

.MIN_LAYOUTS <- 3L

# Departures from the majority rule, named so that every one is visible here.
# `R03`-`R07` is a border on `honeycomb` and `continent` only, and is kept
# because the freeze kept it; `R03`-`R08` and `R07`-`R09` are borders on three
# layouts each and are NOT corridors -- the first crosses another link when the
# network is drawn on the continent, the second was dropped by hand.
.WEAK_OK <- list(c("R03", "R07"))
.MAJORITY_NOT_SHIPPED <- list(c("R03", "R08"), c("R07", "R09"))

# The fourteen frozen corridors.
.LINKS <- rbind(
  c("R02", "R03"), c("R02", "R05"), c("R04", "R05"), c("R04", "R06"),
  c("R05", "R06"), c("R02", "R07"), c("R03", "R07"), c("R05", "R07"),
  c("R07", "R08"), c("R08", "R09"), c("R06", "R11"), c("R09", "R11"),
  c("R10", "R12"), c("R11", "R12")
)

# the canonical order, which the matrix is keyed on
.link_regions <- as.character(
  geoscales::geoscale_regions(topia_geoscales[[1]], geoframe = "region"))

.ISOLATED <- "R01"

# Border matrix of one layout: TRUE where two regions share an EDGE.
.borders <- function(gs) {
  g <- sf::st_as_sf(geoscales::geoscale_geometry(gs, geoframe = "region"))
  key <- intersect(c("region", "name", "id"), names(g))[1]
  reg <- as.character(g[[key]])
  nb <- suppressMessages(suppressWarnings(
    sf::st_relate(g, g, pattern = "F***1****")))
  m <- matrix(FALSE, length(reg), length(reg), dimnames = list(reg, reg))
  for (i in seq_along(nb)) if (length(nb[[i]])) m[i, nb[[i]]] <- TRUE
  diag(m) <- FALSE
  m[.link_regions, .link_regions]
}

.counts <- Reduce(`+`, lapply(topia_geoscales, function(gs) .borders(gs) * 1L))

topia_links <- matrix(FALSE, length(.link_regions), length(.link_regions),
                      dimnames = list(.link_regions, .link_regions))
for (.i in seq_len(nrow(.LINKS))) {
  .a <- .LINKS[.i, 1]; .b <- .LINKS[.i, 2]
  if (!all(c(.a, .b) %in% .link_regions)) {
    stop("`.LINKS` names a region that is not on the map: ",
         paste(setdiff(c(.a, .b), .link_regions), collapse = ", "),
         call. = FALSE)
  }
  topia_links[.a, .b] <- TRUE
  topia_links[.b, .a] <- TRUE
}

# every shipped corridor is a real border on most of the maps, or is named
# above as a departure
.allowed_weak <- matrix(FALSE, length(.link_regions), length(.link_regions),
                        dimnames = list(.link_regions, .link_regions))
for (.p in .WEAK_OK) {
  .allowed_weak[.p[1], .p[2]] <- TRUE
  .allowed_weak[.p[2], .p[1]] <- TRUE
}
.weak <- which(topia_links & .counts < .MIN_LAYOUTS & !.allowed_weak,
               arr.ind = TRUE)
if (nrow(.weak)) {
  stop("corridor(s) that are a border on fewer than ", .MIN_LAYOUTS,
       " layouts and are not named in `.WEAK_OK`: ",
       paste(sprintf("%s-%s (%d)", .link_regions[.weak[, 1]],
                     .link_regions[.weak[, 2]],
                     .counts[.weak]), collapse = ", "), call. = FALSE)
}

# and the converse: a pair the majority rule admits is either a corridor or
# named as deliberately dropped, so the network cannot gain a link by a change
# of geometry without someone noticing
.dropped <- matrix(FALSE, length(.link_regions), length(.link_regions),
                   dimnames = list(.link_regions, .link_regions))
for (.p in .MAJORITY_NOT_SHIPPED) {
  .dropped[.p[1], .p[2]] <- TRUE
  .dropped[.p[2], .p[1]] <- TRUE
}
.surprise <- which(.counts >= .MIN_LAYOUTS & !topia_links & !.dropped &
                     upper.tri(.counts), arr.ind = TRUE)
if (nrow(.surprise)) {
  stop("border pair(s) on >= ", .MIN_LAYOUTS,
       " layouts that are neither a corridor nor named as dropped: ",
       paste(sprintf("%s-%s (%d)", .link_regions[.surprise[, 1]],
                     .link_regions[.surprise[, 2]],
                     .counts[.surprise]), collapse = ", "), call. = FALSE)
}

# `R01` is the remote region: no corridor touches it, by design
stopifnot(!any(topia_links[.ISOLATED, ]), !any(topia_links[, .ISOLATED]))

# the mainland is one network: every other region can trade with every other
.reach <- function(m, from) {
  seen <- from
  repeat {
    nxt <- setdiff(which(apply(m[seen, , drop = FALSE], 2, any)), seen)
    if (!length(nxt)) break
    seen <- c(seen, nxt)
  }
  seen
}
.mainland <- setdiff(seq_along(.link_regions), match(.ISOLATED, .link_regions))
.unreached <- setdiff(.mainland, .reach(topia_links, .mainland[1]))
if (length(.unreached)) {
  stop("the transmission network leaves region(s) unreachable: ",
       paste(.link_regions[.unreached], collapse = ", "),
       ". Name another corridor in `.LINKS`.", call. = FALSE)
}

stopifnot(
  isSymmetric(topia_links),
  !any(diag(topia_links)),
  identical(dimnames(topia_links), list(.link_regions, .link_regions)),
  sum(topia_links) / 2 == nrow(.LINKS)
)
message("topia_links: ", sum(topia_links) / 2, " corridors over ",
        length(.link_regions), " regions (", .ISOLATED, " isolated)")

rm(.MIN_LAYOUTS, .LINKS, .WEAK_OK, .MAJORITY_NOT_SHIPPED, .ISOLATED, .i, .a,
   .b, .p, .link_regions, .borders, .counts, .allowed_weak, .weak, .dropped,
   .surprise, .reach, .mainland, .unreached)
