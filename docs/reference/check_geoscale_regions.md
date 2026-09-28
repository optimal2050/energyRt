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
  [`geoscales::Geoscale`](https://optimal2050.github.io/geoscales/r/reference/Geoscale.html).

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
[`setGeoscale,config-method`](https://energyRt.org/reference/setGeoscale.md)
