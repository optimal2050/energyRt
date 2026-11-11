# Visualize a Horizon object

`autoplot()` is the ggplot2-idiomatic entry point and returns the same
`ggplot` object as [`plot()`](https://energyRt.org/reference/draw.md),
so the result can be further customised with `+ ...` layers.

When the horizon carries a base year, the milestones that are GIVEN
rather than optimised are washed out and a rule is drawn at the last
given year: milestones before the base year are history and enter no LP
at all, so they are washed more heavily than the base year, which is
solved on its own by
[`solve_base_year()`](https://energyRt.org/reference/solve_base_year.md).
The caption says which is which. A horizon with no base year is drawn
unchanged.

## Usage

``` r
# S3 method for class 'horizon'
autoplot(object, ...)
```

## Arguments

- object:

  An object of class `horizon`.

- ...:

  Additional optional arguments: `hjust` (numeric) to adjust the
  horizontal position of the intervals, accepts values between 0 and 1.

## Value

A `ggplot` object.
