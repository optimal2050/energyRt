# Check what an aggregation conserved

Compares a model with a coarser version of itself, column by column, and
says which quantities survived. The comparison is by class, slot and
column, not by object: aggregation merges corridors and drops the ones
internal to a coarse region, so a per-object diff would read that as
loss.

## Usage

``` r
verify_aggregation(fine, coarse, tol = 1e-06)
```

## Arguments

- fine:

  the model before aggregation.

- coarse:

  the model
  [`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md)
  returned.

- tol:

  relative tolerance for an extensive total (default `1e-6`).

## Value

A `data.frame` with one row per class, slot and column: the rule, the
fine and coarse totals, the absolute and relative difference, the two
ranges and a verdict. Printed as a short summary with the exceptions
listed.

## Details

Each column is judged by its own aggregation rule, read from the same
table the aggregation used:

- `sum` – an extensive quantity (a stock, a capacity, a demand, an
  availability): its total must be unchanged, and any difference is
  reported with its relative size.

- `weighted_mean`, `mean`, `copy` – a rate, a price or an invariant: a
  weighted mean cannot leave the range of the values it came from, so
  the verdict is whether the coarse range still sits inside the fine
  one.

A `trade` row whose object count fell is reported as `by construction`
rather than as a difference: a corridor between two regions of one
coarse region is internal to it and is dropped, so its capacity is not
expected to survive. That is the aggregation's meaning, not an error –
which is exactly why it is named rather than hidden.

## See also

[`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md),
[`check_model_levels()`](https://energyRt.org/reference/check_model_levels.md),
[`verify_solution()`](https://energyRt.org/reference/verify_solution.md)

Other validation:
[`check_model_levels()`](https://energyRt.org/reference/check_model_levels.md)
