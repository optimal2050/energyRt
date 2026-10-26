# Registry file and model store locations

`get_registry_file()` / `set_registry_file()` locate the project
registry CSV (see
[`load_registry()`](https://energyRt.org/reference/registry.md)).
`get_models_path()` / `set_models_path()` locate the model store root
used by [`save_model()`](https://energyRt.org/reference/model_store.md).
Both default to the project root / `models/`, siblings of
[`get_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md).

## Usage

``` r
set_registry_file(path = NULL)

get_registry_file()

set_models_path(path = NULL)

get_models_path()

set_repositories_path(path = NULL)

get_repositories_path()

set_datasets_path(path = NULL)

get_datasets_path()
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
[`set_reports_path()`](https://energyRt.org/reference/reports_path.md),
[`set_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)

## Examples

``` r
get_registry_file()
#> [1] "energyRt_registry.csv"
get_models_path()
#> [1] "models/"
```
