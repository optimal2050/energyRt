# Set or get the directory with scenarios

Set or get the directory with scenarios

## Usage

``` r
set_scenarios_path(path = NULL)

get_scenarios_path()
```

## Arguments

- path:

  character, path to the directory holding scenario folders.

## Value

`get_scenarios_path()` the path; `set_scenarios_path()` the previous
value, invisibly.

## See also

Other options:
[`en_config_path()`](https://energyRt.org/reference/en_config.md),
[`en_config_show()`](https://energyRt.org/reference/en_config_show.md),
[`get_arrow_format()`](https://energyRt.org/reference/arrow_format.md),
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`isVerbose()`](https://energyRt.org/reference/isVerbose.md),
[`set_default_registry()`](https://energyRt.org/reference/default_registry.md),
[`set_option()`](https://energyRt.org/reference/en_option.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)

## Examples

``` r
get_scenarios_path()
#> [1] "scenarios/"
```
