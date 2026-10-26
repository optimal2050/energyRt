# Report and levcost output locations

Derived artifacts live with the object they describe when that object is
saved: a scenario's reports render into `<scenario>/reports/` and its
levcost results cache into `<scenario>/levcost/`; models and
repositories use their store entries the same way. For IN-MEMORY objects
the output is considered temporary and lands in these project-level
folders instead: `get_reports_path()` (default `reports/`) and
`get_levcost_cache_path()` (default `levcosts/`).

## Usage

``` r
set_reports_path(path = NULL)

get_reports_path()

set_levcost_cache_path(path = NULL)

get_levcost_cache_path()
```

## Arguments

- path:

  character, new location.

## Value

getters return the path; setters return the previous value, invisibly.

## See also

Other options:
[`en_config_path()`](https://energyRt.org/reference/en_config.md),
[`en_config_show()`](https://energyRt.org/reference/en_config_show.md),
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`get_exchange_format()`](https://energyRt.org/reference/exchange_format.md),
[`get_storage_format()`](https://energyRt.org/reference/storage_format.md),
[`isVerbose()`](https://energyRt.org/reference/isVerbose.md),
[`set_log_file()`](https://energyRt.org/reference/log.md),
[`set_option()`](https://energyRt.org/reference/en_option.md),
[`set_path_builder()`](https://energyRt.org/reference/path_builders.md),
[`set_registry_file()`](https://energyRt.org/reference/registry_file.md),
[`set_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)

## Examples

``` r
get_reports_path()
#> [1] "reports/"
get_levcost_cache_path()
#> [1] "levcosts/"
```
