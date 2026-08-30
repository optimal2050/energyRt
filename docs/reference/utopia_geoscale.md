# A geoscale for the UTOPIA reference model

Builds a
[geoscales::Geoscale](https://optimal2050.github.io/geoscales/r/reference/Geoscale.html)
over UTOPIA's eleven regions, nested `nation -> zone -> region`, and
attaches one of the reference map layouts.

## Usage

``` r
utopia_geoscale(layout = "honeycomb", region = NULL, area = TRUE)
```

## Arguments

- layout:

  Which layout in `utopia$map` to take geometry from: `"honeycomb"`
  (default, the one the vignettes draw), `"squares"`, `"island"` or
  `"continent"`. `NULL` builds the hierarchy with no geometry, which
  needs neither `sf` nor a map.

- region:

  Optional subset of regions to keep, e.g. `c("R1","R2","R3")` to match
  the three-region UTOPIA model.

- area:

  Add an `area` weight measured from the geometry. The layouts carry no
  CRS, so this is planar area in the coordinates' own units.

## Value

A
[`geoscales::Geoscale`](https://optimal2050.github.io/geoscales/r/reference/Geoscale.html).

## Details

The hierarchy comes from `utopia$geo` and is keyed by region name, so it
is valid for every layout in `utopia$map` — the layouts place `R1`…`R11`
differently but share their names.

## See also

Other geoscale:
[`check_geoscale_regions()`](https://energyRt.org/reference/check_geoscale_regions.md),
[`plot_geoscale()`](https://energyRt.org/reference/plot_geoscale.md),
[`plot_map()`](https://energyRt.org/reference/plot_map.md),
[`setGeoscale,config-method`](https://energyRt.org/reference/setGeoscale.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gs <- utopia_geoscale()
geoscales::geoscale_children(gs, "zone", "WEST")

# the three-region model used in vignette("utopia-build")
gs3 <- utopia_geoscale(region = c("R1", "R2", "R3"))
mod <- newModel("UTOPIA", region = c("R1", "R2", "R3"), geoscale = gs3)
} # }
```
