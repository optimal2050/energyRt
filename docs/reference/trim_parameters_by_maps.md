# Trim numeric/bounds parameters to the domain of the maps that index them

Drops rows of a value parameter that lie outside the union of the
equation- domain maps referencing it (lifespan window x membership). The
maps are the authority on the minimal domain, so any parameter row no
map indexes is dead data: it bloats the written model and (for unfolded
scenarios) may carry a stale wildcard NA that is out-of-domain in the
solver.

## Usage

``` r
trim_parameters_by_maps(scen, verbose = FALSE)
```

## Arguments

- scen:

  scenario.

- verbose:

  logical; report per-parameter trim counts.

## Value

scenario with trimmed parameter slots.

## Details

Intended for the unfolded (`fold = FALSE`) pipeline, where wildcard (NA)
dimensions have already been materialised. A parameter is left untouched
when none of its registered maps is available, to avoid clearing data
whose map was not built.
