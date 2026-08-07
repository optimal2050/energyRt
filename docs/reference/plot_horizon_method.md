# Visualize a Horizon object

`autoplot()` is the ggplot2-idiomatic entry point and returns the same
`ggplot` object as [`plot()`](https://energyRt.org/reference/draw.md),
so the result can be further customised with `+ ...` layers.

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
