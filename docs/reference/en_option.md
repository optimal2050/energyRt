# Get or set an energyRt option

Generic accessors for any option in the energyRt registry. See
`?energyRt-options` for the full list, or
[`en_config_show()`](https://energyRt.org/reference/en_config_show.md)
to print every option together with the source its current value came
from.

Each option can be set three ways, in decreasing priority: the R option
(`options(en.<name> = )`), the environment variable (`ENERGYRT_<NAME>`),
or the configuration file (see
[`en_config_write()`](https://energyRt.org/reference/en_config.md)).

## Usage

``` r
set_option(name, value)

get_option(name, default = NULL)
```

## Arguments

- name:

  character, the option name *without* the `en.` prefix, e.g.
  `"gams_path"`.

- value:

  the new value.

- default:

  value returned if the option is not defined.

## Value

`get_option()` the option value; `set_option()` the previous value,
invisibly.

## See also

Other options:
[`en_config_path()`](https://energyRt.org/reference/en_config.md),
[`en_config_show()`](https://energyRt.org/reference/en_config_show.md),
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`get_exchange_format()`](https://energyRt.org/reference/exchange_format.md),
[`get_storage_format()`](https://energyRt.org/reference/storage_format.md),
[`isVerbose()`](https://energyRt.org/reference/isVerbose.md),
[`set_log_file()`](https://energyRt.org/reference/log.md),
[`set_path_builder()`](https://energyRt.org/reference/path_builders.md),
[`set_registry_file()`](https://energyRt.org/reference/registry_file.md),
[`set_reports_path()`](https://energyRt.org/reference/reports_path.md),
[`set_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)

## Examples

``` r
get_option("storage_format")
#> [1] "feather"
```
