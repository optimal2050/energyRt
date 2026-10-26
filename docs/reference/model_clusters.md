# The clustering behind an aggregated model

Reports what
[`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md)
did when it was given `clusters`: which regions were merged into which
cluster, the `k` it used, the sweep it chose from when `k` was `"auto"`,
and a geoscale carrying a `cluster` geoframe so the grouping can be
plotted with
[`plot_geoscale()`](https://energyRt.org/reference/plot_geoscale.md).

## Usage

``` r
model_clusters(object)
```

## Arguments

- object:

  a `model` returned by
  [`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md).

## Value

A named list, one element per process group, each with `k`, `crosswalk`
(`region`, `cluster`, `medoid`, `target`), `sweep` and `geoscale`. An
empty list when the model was aggregated plainly.

## See also

[`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md),
[`process_cluster_sweep()`](https://energyRt.org/reference/process_cluster_sweep.md)

Other geoscale:
[`check_geoscale_regions()`](https://energyRt.org/reference/check_geoscale_regions.md),
[`get_process_groups()`](https://energyRt.org/reference/get_process_groups.md),
[`plot_geoscale()`](https://energyRt.org/reference/plot_geoscale.md),
[`plot_map()`](https://energyRt.org/reference/plot_map.md),
[`process_cluster_sweep()`](https://energyRt.org/reference/process_cluster_sweep.md),
[`setCalendar,config-method`](https://energyRt.org/reference/setGeoscale.md)

## Examples

``` r
if (FALSE) {
cl <- model_clusters(m)
plot_geoscale(cl$ECOA$geoscale, type = "map", geoframe = "cluster")
plot_geoscale(cl$ECOA$geoscale, type = "icicle")
}
```
