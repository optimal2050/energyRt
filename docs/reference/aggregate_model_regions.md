# Aggregate a model to a coarser set of regions

Build a smaller model from a finer one by collapsing its regions onto a
coarser geoframe of a
[geoscales::GeoScale](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html).
The result is a model in its own right: it can be interpolated, solved
and reported like any other.

## Usage

``` r
aggregate_model_regions(
  object,
  geoscale = NULL,
  level,
  clusters = NULL,
  as = c("clusters", "objects"),
  name = NULL,
  verbose = isVerbose(),
  .trade_parts = NULL
)
```

## Arguments

- object:

  A `model`.

- geoscale:

  A
  [geoscales::GeoScale](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html)
  whose finest geoframe contains the model's regions. `NULL` (default)
  uses the model's own.

- level:

  Name of the target geoframe, coarser than the model's regions.

- clusters:

  `NULL` (default) collapses each coarse region to one value. Otherwise
  a list keyed by process group (see
  [`get_process_groups()`](https://energyRt.org/reference/get_process_groups.md))
  that keeps some of the spread instead – see the section below. A group
  that is not named is aggregated plainly, so clustering is opted into
  one family at a time.

- as:

  With `clusters`, the shape of the result: `"clusters"` (default) gives
  one object per family, region variants becoming `technology@cluster`
  entries; `"objects"` gives one object per (family x cluster), named
  with the cluster's label (`ECOA_W1`), each keeping its own clusters.
  Same LP either way – what differs is how results are keyed.

- name:

  Name for the aggregated model. `NULL` (default) appends `level` to the
  source model's name.

- verbose:

  Report what was aggregated, merged and dropped.

- .trade_parts:

  internal; a named vector mapping trade names to a part label, appended
  to the merge key so corridors in different parts stay separate instead
  of collapsing into one merged object.

## Value

A `model` declared over the coarser regions, carrying the pruned
geoscale. With `clusters`, the grouping is attached and readable with
[`model_clusters()`](https://energyRt.org/reference/model_clusters.md).

## Details

Every region-keyed slot is recast. Extensive quantities – capacities,
demand, supply availability, import and export limits – are summed over
the regions of each parent. Intensive ones – efficiencies, availability
factors, costs, weather values – take a mean weighted by the object's
own size in each region, so an aggregated value stays within the range
of the values it came from. When the weights of a group are all zero the
mean is unweighted, which keeps a group of empty regions at their common
value rather than undefined.

Trade is different: a trade object is one region pair, so corridors that
land inside a single parent region become internal and are dropped, and
the rest merge – their capacities add, and their loss factors take the
capacity-weighted mean. Corridors merge only when they agree on
commodity and on cluster structure, so tranched and untranched routes
stay apart.

## Keeping the spread as clusters

Collapsing a zone's regions to one average technology throws away the
difference between its cheap sites and its dear ones, and a model that
can no longer see that difference builds the average everywhere.
`clusters` keeps some of it: the fine regions are grouped into `k`
connected clusters and carried into `technology@cluster`, one parallel
sub-process each, with its own capacity, availability and costs.

Settings are **per technology family**, because one `k` for the model is
meaningless – a coal fleet groups on cost and efficiency, a wind fleet
on resource quality and profile shape:

    get_process_groups(mod)                                   # what can be merged
    process_cluster_sweep(mod, "ECOA", level = "zone")     # where its structure is
    aggregate_model_regions(
      mod, level = "zone",
      clusters = list(ECOA = list(k = 3, features = c("invcost", "eff")),
                      EWIN = list(k = 8, features = wind_shape)))

Each element is a list with `k` and optionally `features`; a bare number
is shorthand for `list(k = n)`. A bare number for the whole argument is
accepted only when the model has ONE group. `k = "auto"` picks the best
average silhouette, which is a rule rather than an answer – sweep and
look instead.

`k` runs from the number of coarse regions (one cluster each – exactly
what `clusters = NULL` gives) to the number of fine regions (1:1,
nothing averaged, which on a model whose fine regions already pool
inside each coarse region reproduces the fine objective exactly).
Clusters cannot straddle a coarse region.

A family that ALREADY has clusters – wind by site grade – keeps them:
the two groupings compose, `GOOD` in `W1` becoming `GOOD_W1`.

## See also

[`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md),
which takes a sub-territory at the model's own resolution instead of
coarsening the whole of it;
[`get_process_groups()`](https://energyRt.org/reference/get_process_groups.md),
[`process_cluster_sweep()`](https://energyRt.org/reference/process_cluster_sweep.md),
[`model_clusters()`](https://energyRt.org/reference/model_clusters.md).

## Examples

``` r
repo <- newRepository(
  "mini",
  newCommodity("COA", unit = "MMBtu"),
  newCommodity("ELC", unit = "MWh", timeframe = "ANNUAL"),
  newSupply("SUP_COA", commodity = "COA", supply = data.frame(cost = 1)),
  newTechnology("ECOA", input = list(comm = "COA"), output = list(comm = "ELC"),
                cap2act = 1, invcost = list(invcost = 100),
                olife = list(olife = 30)),
  newDemand("DEM", commodity = "ELC",
            demand = data.frame(region = c("W1", "W2"), year = 2020, demand = 10))
)
mod <- newModel("MINI", data = repo, calendar = calendars$annual,
                region = c("W1", "W2"), horizon = horizons$Y2020_2060_by_5)

# roll the two regions up into the zone they share
agg <- aggregate_model_regions(mod, geoscale = topia$geoscales$squares,
                               level = "zone")
get_region(agg)
#> [1] "WEST"
```
