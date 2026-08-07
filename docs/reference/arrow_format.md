# Arrow exchange-format options

Getters / setters for the Arrow format used to exchange data with the
JuMP / Pyomo solvers (and the default on-disk storage codec).

## Usage

``` r
get_arrow_format()

set_arrow_format(format = c("feather", "parquet", "csv"))

get_arrow_compression()

set_arrow_compression(codec = c("zstd", "lz4", "uncompressed"))

get_arrow_compression_level()

set_arrow_compression_level(level = 15L)
```

## Arguments

- format:

  one of `"feather"`, `"parquet"`, `"csv"`.

- codec:

  compression codec, e.g. `"zstd"`, `"lz4"`, `"uncompressed"`.

- level:

  integer compression level (ZSTD: 1-22).

## Value

the option value (getters) or the previous value, invisibly (setters).

## See also

Other options:
[`en_config_path()`](https://energyRt.org/reference/en_config.md),
[`en_config_show()`](https://energyRt.org/reference/en_config_show.md),
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`isVerbose()`](https://energyRt.org/reference/isVerbose.md),
[`set_default_registry()`](https://energyRt.org/reference/default_registry.md),
[`set_option()`](https://energyRt.org/reference/en_option.md),
[`set_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)

## Examples

``` r
get_arrow_format()
#> [1] "feather"
```
