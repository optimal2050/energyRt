# Technologies that may be merged into one clustered object

Groups a model's single-region technologies by STRUCTURE – input, output
and aux commodities, and input groups. Only technologies with the same
structure can become clusters of one object, because a cluster is a
variant of the same process, not a different one: a coal plant and a gas
plant have different inputs and stay separate however alike their costs
look.

## Usage

``` r
get_process_groups(x, regions = NULL)
```

## Arguments

- x:

  a `model`, `repository`, or a list of objects.

- regions:

  optional character vector; keep only technologies living in these
  regions.

## Value

A data.frame, one row per group, with `group` (the common name stem, and
the name the merged object will take), `signature`, `n`, `members` and
`regions` (comma-separated). Technologies that span several regions are
not grouped and do not appear.

## Details

This is the first step of a clustered aggregation: it names the families
[`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md)
can turn into clusters, and those names are the keys of its `clusters`
argument.

## See also

[`process_cluster_sweep()`](https://energyRt.org/reference/process_cluster_sweep.md)
to choose each family's `k`,
[`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md)
to apply it.

Other geoscale:
[`check_geoscale_regions()`](https://energyRt.org/reference/check_geoscale_regions.md),
[`model_clusters()`](https://energyRt.org/reference/model_clusters.md),
[`plot_geoscale()`](https://energyRt.org/reference/plot_geoscale.md),
[`plot_map()`](https://energyRt.org/reference/plot_map.md),
[`process_cluster_sweep()`](https://energyRt.org/reference/process_cluster_sweep.md),
[`setCalendar,config-method`](https://energyRt.org/reference/setGeoscale.md)

## Examples

``` r
topia_elc <- topia$modules$electricity
mod <- newModel("TOPIA", data = topia_elc$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = topia_elc$regions,
                horizon = topia$modules$horizons$BY2025_50_by10,
                discount = 0.05)
mod <- setGeoscale(mod, topia_elc$geoscale)

# families of processes that share a commodity signature, with their members
head(get_process_groups(mod), 4)
#>   group      class                  signature  n members
#> 1  ECOA technology   in:COA|out:ELC|aux:|grp: 11    ECOA
#> 2  EGAS technology   in:GAS|out:ELC|aux:|grp: 11    EGAS
#> 4  ESOL technology in:|out:ELC|aux:LANDS|grp: 11    ESOL
#> 5  EWIN technology in:|out:ELC|aux:LANDW|grp: 11    EWIN
#>                            regions
#> 1 W1,W2,C1,C2,C3,C4,C5,C6,E1,E2,E3
#> 2 W1,W2,C1,C2,C3,C4,C5,C6,E1,E2,E3
#> 4 W1,W2,C1,C2,C3,C4,C5,C6,E1,E2,E3
#> 5 W1,W2,C1,C2,C3,C4,C5,C6,E1,E2,E3
```
