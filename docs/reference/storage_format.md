# On-disk table storage options

Getters / setters for the codec of the tables energyRt writes to disk: a
scenario's parameter and variable stores, and the model / repository /
dataset stores. `"feather"` (Arrow IPC) and `"parquet"` both compress
and both support lazy, filtered reads through `open_dataset()`; parquet
is smaller, feather reads faster. `"csv"` is uncompressed and loses
column types on the round trip.

The format is recorded in the store's manifest and is authoritative when
an existing store is rewritten, so changing this option never mixes
codecs inside one store. The content-addressed stores
([`save_model()`](https://energyRt.org/reference/model_store.md),
[`save_repository()`](https://energyRt.org/reference/repo_store.md),
[`save_dataset()`](https://energyRt.org/reference/dataset_store.md)) pin
parquet regardless, because their identity is a content hash that must
be reproducible.

For the files handed to a solver, see
[`get_exchange_format()`](https://energyRt.org/reference/exchange_format.md).

## Usage

``` r
get_storage_format()

set_storage_format(format = c("feather", "parquet", "csv"))

get_storage_compression()

set_storage_compression(codec = c("zstd", "lz4", "uncompressed"))

get_storage_compression_level()

set_storage_compression_level(level = 15L)
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
[`get_exchange_format()`](https://energyRt.org/reference/exchange_format.md),
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
get_storage_format()
#> [1] "feather"
```
