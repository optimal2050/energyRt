# Per-variant levelized cost tables

Stack the per-variant tables of a
[`levcost()`](https://energyRt.org/reference/levcost.md) result computed
for a vintaged or clustered technology, keyed by variant.

## Usage

``` r
levcost_by_variant(x, what = c("levcost", "npv", "components"))
```

## Arguments

- x:

  A `levcost_variants` object, as returned by
  [`levcost()`](https://energyRt.org/reference/levcost.md) for a
  technology declaring vintages and/or clusters.

- what:

  Which table to stack: `"levcost"` (levelized cost by year), `"npv"`
  (one row per variant), or `"components"` (cost breakdown by year).

## Value

A data.frame with `tech`, `variant`, `vintage` and `cluster` key columns
prepended to the requested table.
