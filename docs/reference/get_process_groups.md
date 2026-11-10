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
[`gg_map()`](https://energyRt.org/reference/gg_map.md),
[`model_clusters()`](https://energyRt.org/reference/model_clusters.md),
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
#>    group      class                  signature  n
#> 19   TRD      trade                 TRD::ELC:: 14
#> 1   ECOA technology   in:COA|out:ELC|aux:|grp: 12
#> 4   ESOL technology in:|out:ELC|aux:LANDS|grp: 12
#> 5   EWIN technology in:|out:ELC|aux:LANDW|grp: 12
#>                                                                                                                                                                                                                            members
#> 19 TBD_ELC_R02_R03,TBD_ELC_R02_R05,TBD_ELC_R04_R05,TBD_ELC_R04_R06,TBD_ELC_R05_R06,TBD_ELC_R02_R07,TBD_ELC_R03_R07,TBD_ELC_R05_R07,TBD_ELC_R07_R08,TBD_ELC_R08_R09,TBD_ELC_R06_R10,TBD_ELC_R09_R10,TBD_ELC_R10_R11,TBD_ELC_R11_R12
#> 1                                                                                                                                                                                                                             ECOA
#> 4                                                                                                                                                                                                                             ESOL
#> 5                                                                                                                                                                                                                             EWIN
#>                                            regions
#> 19     R02,R03,R05,R04,R06,R07,R08,R09,R10,R11,R12
#> 1  R01,R02,R03,R04,R05,R06,R07,R08,R09,R10,R11,R12
#> 4  R01,R02,R03,R04,R05,R06,R07,R08,R09,R10,R11,R12
#> 5  R01,R09,R10,R11,R12,R02,R03,R04,R05,R06,R07,R08
```
