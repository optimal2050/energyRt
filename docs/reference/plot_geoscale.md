# Draw a model's geoscale

Visualises the geography attached to a model, scenario, config, or a
bare
[`geoscales::GeoScale`](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html):
a membership map of one geoframe (`"map"`), a layered view of ALL
geoframes stacked in a cabinet/perspective projection with the coarsest
level at the bottom (`"stack"`), or the geometry-free `"icicle"`
diagram.

## Usage

``` r
plot_geoscale(
  object,
  type = c("map", "stack", "icicle"),
  geoframe = NULL,
  view = "cabinet",
  direction = "down",
  geoscale = NULL,
  ...
)
```

## Arguments

- object:

  a `model`, `scenario`, `config`, or
  [`geoscales::GeoScale`](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html).

- type:

  `"map"`, `"stack"`, or `"icicle"`.

- geoframe:

  for `"map"`: the geoframe whose membership colours the atoms; default
  = the coarsest level above the atoms.

- view:

  stack projection (`"cabinet"`, `"perspective"`, ... – see
  [`geoscales::geoscale_autoplot()`](https://optimal2050.github.io/geoscales/r/reference/geoscale_autoplot.html)).

- direction:

  `"down"` (default) draws the coarsest geoframe at the bottom of the
  stack; `"up"` at the top.

- geoscale:

  a
  [`geoscales::GeoScale`](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html)
  override, or `NULL` to resolve from `object`.

- ...:

  passed to the geoscales plotting functions.

## Value

A `ggplot` object.

## Details

`"map"` and `"stack"` need attached geometry (see
[`geoscales::attach_geometry_geoscale()`](https://optimal2050.github.io/geoscales/r/reference/attach_geometry_geoscale.html))
and the `sf` package; `"icicle"` works without either and is the
fallback the report templates degrade to.

## See also

Other geoscale:
[`check_geoscale_regions()`](https://energyRt.org/reference/check_geoscale_regions.md),
[`get_process_groups()`](https://energyRt.org/reference/get_process_groups.md),
[`model_clusters()`](https://energyRt.org/reference/model_clusters.md),
[`plot_map()`](https://energyRt.org/reference/plot_map.md),
[`process_cluster_sweep()`](https://energyRt.org/reference/process_cluster_sweep.md),
[`setCalendar,config-method`](https://energyRt.org/reference/setGeoscale.md)

## Examples

``` r
gs <- topia$geoscales$squares

# the nesting of a geoscale, without any geometry
plot_geoscale(gs, type = "icicle")

plot_geoscale(gs, type = "stack")


# the map itself
plot_geoscale(gs, type = "map")
```
