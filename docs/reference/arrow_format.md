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
