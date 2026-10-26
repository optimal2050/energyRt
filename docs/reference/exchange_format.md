# Solver data-exchange options

Getters / setters for the codec of the files exchanged with the JuMP and
Pyomo backends: the model data written into the run's `solver/input/`,
and the solution written back into `solver/output/`. Julia's `Arrow.jl`
reads IPC only, so `"parquet"` is available on the Pyomo path and
refused on the Julia one.

These files live for the duration of one solve, which is why the
compression default is cheaper here than for storage
([`get_storage_compression()`](https://energyRt.org/reference/storage_format.md)).

A solver preset's `export_format` / `import_format` overrides these
options for that solve, and the two directions stay independent – Arrow
input with CSV output is a legal combination. Setting a preset's
`export_format` to `"SQLite"` (Pyomo) or `"RData"` (Julia) selects the
legacy exchange.

## Usage

``` r
get_exchange_format()

set_exchange_format(format = c("feather", "parquet", "csv"))

get_exchange_compression()

set_exchange_compression(codec = c("zstd", "lz4", "uncompressed"))

get_exchange_compression_level()

set_exchange_compression_level(level = 15L)
```

## Arguments

- format:

  one of `"feather"`, `"parquet"`, `"csv"`.

- codec:

  compression codec, e.g. `"zstd"`, `"lz4"`, `"uncompressed"`.

- level:

  integer compression level (ZSTD: 1-22; ignored for lz4).

## Value

the option value (getters) or the previous value, invisibly (setters).

## See also

Other options:
[`en_config_path()`](https://energyRt.org/reference/en_config.md),
[`en_config_show()`](https://energyRt.org/reference/en_config_show.md),
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`get_storage_format()`](https://energyRt.org/reference/storage_format.md),
[`isVerbose()`](https://energyRt.org/reference/isVerbose.md),
[`set_log_file()`](https://energyRt.org/reference/log.md),
[`set_option()`](https://energyRt.org/reference/en_option.md),
[`set_path_builder()`](https://energyRt.org/reference/path_builders.md),
[`set_registry_file()`](https://energyRt.org/reference/registry_file.md),
[`set_reports_path()`](https://energyRt.org/reference/reports_path.md),
[`set_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)

## Examples

``` r
get_exchange_format()
#> [1] "feather"
```
