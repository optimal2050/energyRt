# Map model results by region

Draws a choropleth of solved results over the model's geography,
optionally aggregated to a coarser level of the geoscale.

## Usage

``` r
plot_map(
  object,
  type = c("generation", "capacity", "new_capacity", "fuel"),
  level = NULL,
  comm = "ELC",
  year = NULL,
  process = NULL,
  geoscale = NULL,
  palette = "D",
  name = NULL,
  facet = c("none", "year"),
  ...
)
```

## Arguments

- object:

  a solved `scenario`.

- type:

  what to map, passed to
  [`getMix()`](https://energyRt.org/reference/getMix.md):
  `"generation"`, `"capacity"`, `"new_capacity"` or `"fuel"`. Ignored
  when `name` is given.

- level:

  level of the geoscale to draw. Defaults to the finest, i.e. the
  model's own regions. A coarser level aggregates first.

- comm:

  balanced commodity for `"generation"`. On the `name =` path a `comm`
  filter applies only when passed explicitly.

- year:

  integer year, or `NULL` for the sum over all milestone years.

- process:

  optional regular expression selecting processes to include.

- geoscale:

  a
  [`geoscales::Geoscale`](https://optimal2050.github.io/geoscales/r/reference/Geoscale.html),
  or `NULL` to use the scenario's.

- palette:

  viridis palette option.

- name:

  optional name of any solved variable carrying a `region` dimension
  (e.g. `"vTechNewCap"`, `"vTechRetiredStock"`): the map is drawn from
  [`getData()`](https://energyRt.org/reference/getData.md) instead of
  [`getMix()`](https://energyRt.org/reference/getMix.md). Variables
  without a region dimension (the `vTrade*` capacity/retirement family)
  cannot be mapped.

- facet:

  `"none"` (default; one map, values summed over years unless `year =`
  selects one) or `"year"` (one panel per milestone year).

- ...:

  passed to
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).

## Value

A `ggplot` object.

## Details

Requires a geoscale with geometry attached (see
[`setGeoscale()`](https://energyRt.org/reference/setGeoscale.md) and
[`geoscales::attach_geometry_geoscale()`](https://optimal2050.github.io/geoscales/r/reference/attach_geometry_geoscale.html)),
plus `sf` and `ggplot2`.

## See also

Other geoscale:
[`check_geoscale_regions()`](https://energyRt.org/reference/check_geoscale_regions.md),
[`plot_geoscale()`](https://energyRt.org/reference/plot_geoscale.md),
[`setGeoscale,config-method`](https://energyRt.org/reference/setGeoscale.md),
[`utopia_geoscale()`](https://energyRt.org/reference/utopia_geoscale.md)

## Examples

``` r
if (FALSE) { # \dontrun{
scen <- solve(interpolate(mod, "BASE"))
plot_map(scen, "capacity")
plot_map(scen, "capacity", level = "zone")
plot_map(scen, name = "vTechNewCap", facet = "year")
plot_map(scen, name = "vTechRetiredStock")
} # }
```
