# Rows an energyRt parameter holds, attached or detached

A detached (on-disk) parameter keeps its row count in the `misc$onDisk`
summary; its `@data` slot is empty by design.

## Usage

``` r
.en_n_rows(en_param)
```
