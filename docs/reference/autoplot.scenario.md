# Plot mixes from a solved scenario

`autoplot()` on a solved `scenario` draws the mixes extracted by
[`getMix()`](https://energyRt.org/reference/getMix.md): annual stacked
bars by milestone year, or – when `slice` selects a sample (e.g. one
representative day) – an hourly dispatch profile. Storage charging and
exports plot below zero; demand is overlaid as a line.

## Usage

``` r
# S3 method for class 'scenario'
autoplot(
  object,
  type = c("generation", "capacity", "new_capacity", "fuel", "storage"),
  comm = "ELC",
  region = NULL,
  year = NULL,
  slice = NULL,
  drop_small = 0,
  ...
)
```

## Arguments

- object:

  a solved `scenario` object.

- type:

  `"generation"` (default), `"capacity"`, `"new_capacity"`, `"fuel"`, or
  `"storage"` (the storage-in/out flows only).

- comm, region, year, slice, drop_small:

  passed to [`getMix()`](https://energyRt.org/reference/getMix.md). For
  a dispatch profile (`slice` given) with `year = NULL`, the last
  milestone year is used.

- ...:

  ignored.

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
autoplot(scen)                                    # annual generation mix
autoplot(scen, "generation", slice = "^SUM_")     # summer-day dispatch
autoplot(scen, "capacity")
} # }
```
