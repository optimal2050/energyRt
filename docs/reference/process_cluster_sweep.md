# Sweep a technology family's clusterings, to choose `k` by looking

Clusters one family over a range of `k` and reports what each one buys:
within-cluster dispersion, average silhouette and the cluster sizes.
Nothing is changed – the point is to see where the structure is before
naming a `k` in
[`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md)`(clusters = )`.

## Usage

``` r
process_cluster_sweep(
  object,
  group,
  ks = NULL,
  geoscale = NULL,
  level,
  features = NULL
)
```

## Arguments

- object:

  a `model`.

- group:

  a process group name, as
  [`get_process_groups()`](https://energyRt.org/reference/get_process_groups.md)
  reports it.

- ks:

  integer vector of cluster counts. `NULL` (default) sweeps the whole
  admissible range: from one cluster per `level` region to one per
  region.

- geoscale:

  a
  [geoscales::GeoScale](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html);
  defaults to the model's own.

- level:

  the target geoframe, e.g. `"zone"`.

- features:

  feature spec, as in
  [`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md).

## Value

A data.frame, one row per `k`, with the dispersion and silhouette
columns
[`nestedscales::cluster_sweep()`](https://rdrr.io/pkg/nestedscales/man/cluster_sweep.html)
returns plus `sizes`, the cluster sizes as a comma-separated string.

## Details

The sweep uses the same features and the same contiguity graph the
aggregation would, so what you inspect is what you would get. Two
families will generally disagree about where their structure is, which
is why `k` is set per family and not once for the model.

Read it as a scree plot: dispersion falls with `k` and always will, so
take the knee rather than the minimum, and prefer a `k` whose silhouette
is high and whose sizes are not one big cluster and a row of singletons.
There is deliberately no "best k" here, and none in `nestedscales`
either – the number depends on what the model is for.

## See also

[`get_process_groups()`](https://energyRt.org/reference/get_process_groups.md),
[`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md)

Other geoscale:
[`check_geoscale_regions()`](https://energyRt.org/reference/check_geoscale_regions.md),
[`get_process_groups()`](https://energyRt.org/reference/get_process_groups.md),
[`model_clusters()`](https://energyRt.org/reference/model_clusters.md),
[`plot_geoscale()`](https://energyRt.org/reference/plot_geoscale.md),
[`plot_map()`](https://energyRt.org/reference/plot_map.md),
[`setCalendar,config-method`](https://energyRt.org/reference/setGeoscale.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ek  <- topia$modules$electricity$R11
mod <- newModel("TOPIA", data = ek$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = ek$regions, horizon = topia$modules$horizons$base)
mod <- setGeoscale(mod, ek$geoscale)

# how a family's regions group at each k, and the spread left inside the
# clusters -- the input to choosing `clusters` for
# aggregate_model_regions(). The family needs parameters that actually
# differ by region (site yields, site costs): one value for every region
# leaves nothing to cluster on.
process_cluster_sweep(mod, group = "EWIN", ks = 3:6,
                      geoscale = topia$geoscales$honeycomb, level = "zone")
} # }
```
