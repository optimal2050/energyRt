# Plot mixes from a solved scenario

`autoplot()` on a solved `scenario` draws the mixes extracted by
[`getMix()`](https://energyRt.org/reference/getMix.md): annual stacked
bars by milestone year, or – when `timeslice` selects a sample (e.g. one
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
  timeslice = NULL,
  drop_small = 0,
  by = NULL,
  fill = NULL,
  facet = NULL,
  ...
)
```

## Arguments

- object:

  a solved `scenario` object.

- type:

  `"generation"` (default), `"capacity"`, `"new_capacity"`, `"fuel"`, or
  `"storage"` (the storage-in/out flows only).

- comm, region, year, timeslice, drop_small:

  passed to [`getMix()`](https://energyRt.org/reference/getMix.md). For
  a dispatch profile (`timeslice` given) with `year = NULL`, the last
  milestone year is used.

- by:

  character vector passed to
  [`getMix()`](https://energyRt.org/reference/getMix.md), any of
  `"vintage"` and `"cluster"`, to keep those technology-variant
  dimensions as grouping columns. Implied automatically by
  `fill`/`facet`.

- fill:

  name of the column to colour the bars by (default `"process"`);
  `"vintage"` or `"cluster"` shows the composition of a variant group.

- facet:

  name of the column to facet by. Defaults to `"region"` when the
  scenario has more than one region (the historical behaviour).

- ...:

  ignored.

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
autoplot(scen)                                    # annual generation mix
autoplot(scen, "generation", timeslice = "^SUM_")     # summer-day dispatch
autoplot(scen, "capacity")
autoplot(scen, "capacity", fill = "cluster")      # capacity by cluster
autoplot(scen, "capacity", fill = "vintage", facet = "process")
} # }
```
