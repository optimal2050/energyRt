# Verbosity and debug level

Test the current reporting level. `isVerbose()` gates user-facing
progress messages, `isDebug()` gates internal consistency warnings that
are silent in normal use. Set the levels with `options(en.verbose = )` /
`options(en.debug = )`, or `set_option("verbose", )`.

Both accept a level (`0`, `1`, `2`, ...) or a logical, which is read as
`1`/`0`.

## Usage

``` r
isVerbose(level = 1)

isDebug(level = 1)
```

## Arguments

- level:

  numeric, the level to test against.

## Value

`TRUE` if the configured level is at or above `level`.

## See also

Other options:
[`en_config_path()`](https://energyRt.org/reference/en_config.md),
[`en_config_show()`](https://energyRt.org/reference/en_config_show.md),
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`get_exchange_format()`](https://energyRt.org/reference/exchange_format.md),
[`get_storage_format()`](https://energyRt.org/reference/storage_format.md),
[`set_log_file()`](https://energyRt.org/reference/log.md),
[`set_option()`](https://energyRt.org/reference/en_option.md),
[`set_path_builder()`](https://energyRt.org/reference/path_builders.md),
[`set_registry_file()`](https://energyRt.org/reference/registry_file.md),
[`set_reports_path()`](https://energyRt.org/reference/reports_path.md),
[`set_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)

## Examples

``` r
isVerbose()
#> [1] FALSE
isDebug(2)
#> [1] FALSE
```
