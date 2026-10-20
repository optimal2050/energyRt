# Check a geoscale against the model's declared regions

Warns when the two disagree. This is deliberately a warning, not an
error: `config@region` remains authoritative, and a geoscale covering
more ground than the model uses (a world map for a two-country model) is
normal and useful.

## Usage

``` r
check_geoscale_regions(geoscale, region, level = NULL)
```

## Arguments

- geoscale:

  A
  [`geoscales::GeoScale`](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html).

- region:

  Character vector of declared model regions.

- level:

  Level of the geoscale to match against. Defaults to the finest.

## Value

Invisibly, the regions that are declared but absent from `geoscale`.

## See also

Other geoscale:
[`get_process_groups()`](https://energyRt.org/reference/get_process_groups.md),
[`model_clusters()`](https://energyRt.org/reference/model_clusters.md),
[`plot_geoscale()`](https://energyRt.org/reference/plot_geoscale.md),
[`plot_map()`](https://energyRt.org/reference/plot_map.md),
[`process_cluster_sweep()`](https://energyRt.org/reference/process_cluster_sweep.md),
[`setCalendar,config-method`](https://energyRt.org/reference/setGeoscale.md)

## Examples

``` r
# returns the declared regions the geoscale cannot place, and warns about them
gs <- topia$geoscales$squares
check_geoscale_regions(gs, c("W1", "W2", "C1"))        # all placed: character(0)
suppressWarnings(check_geoscale_regions(gs, c("W1", "NOWHERE")))
```
