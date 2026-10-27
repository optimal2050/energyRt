# What a solve sequence covered

One row per unit of work and element covered, for any sequence from
[`solve_by_sample()`](https://energyRt.org/reference/solve_by_sample.md),
[`solve_by_region()`](https://energyRt.org/reference/solve_by_region.md),
[`solve_recursive()`](https://energyRt.org/reference/solve_recursive.md)
or [`solve_guided()`](https://energyRt.org/reference/solve_guided.md).

## Usage

``` r
sequence_coverage(x, ...)

# Default S3 method
sequence_coverage(x, ...)

# S3 method for class 'by_sample'
sequence_coverage(x, ...)

# S3 method for class 'by_region'
sequence_coverage(x, ...)

# S3 method for class 'recursive'
sequence_coverage(x, ...)

# S3 method for class 'guided'
sequence_coverage(x, ...)
```

## Arguments

- x:

  a `by_sample`, `by_region`, `recursive` or `guided` sequence.

- ...:

  unused.

## Value

a tibble with `unit`, `dim`, `key`, `role` and `count`.

## Details

The `dim` column names the scale the units are drawn on — `"timeslice"`
for a calendar sample, `"region"` for a spatial one, `"year"` for a
horizon walk — so a caller can join the result onto the matching
[`timescales::Calendar`](https://optimal2050.github.io/timescales/r/reference/Calendar.html)
or
[`geoscales::GeoScale`](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html).

`role` separates what a unit **decided** from what it merely **saw**: a
recursive step's look-ahead milestones are solved for guidance and their
decisions discarded, so they are coverage of a different kind and are
labelled `"lookahead"`. Everything else is `"covered"`.

`count` is the multiplicity of an element within its unit. It is 1
except for a bootstrap calendar sample, where a timeslice drawn twice
carries a doubled share — overlap *within* a unit rather than between
units.

## Reading the result

Two questions are worth asking of it directly. Whether the units
partition the scale: `any(duplicated(res$key[res$role == "covered"]))`
is TRUE when two units claim the same element. And what was missed:
compare `res$key` against the scale's own members — the elements absent
from the frame were never solved, which is what a gap in an ensemble
looks like.

## See also

[`solve_by_sample()`](https://energyRt.org/reference/solve_by_sample.md),
[`solve_by_region()`](https://energyRt.org/reference/solve_by_region.md),
[`solve_recursive()`](https://energyRt.org/reference/solve_recursive.md)

## Examples

``` r
if (FALSE) { # \dontrun{
cv <- sequence_coverage(bs)
# did the samples tile the calendar, or overlap?
any(duplicated(cv$key[cv$role == "covered"]))
} # }
```
