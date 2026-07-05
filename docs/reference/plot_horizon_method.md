# Visualize a Horizon object

`autoplot()` is the ggplot2-idiomatic entry point and returns the same
`ggplot` object as
[`plot()`](https://rdrr.io/r/graphics/plot.default.html), so the result
can be further customised with `+ ...` layers.

## Usage

``` r
# S4 method for class 'horizon'
plot(x, y, ...)

# S3 method for class 'horizon'
autoplot(object, ...)
```

## Arguments

- x:

  An object of class `horizon`

- ...:

  Additional optional arguments: `hjust` (numeric) to adjust the
  horizontal position of the intervals, accepts values between 0 and 1.

- object:

  An object of class `horizon`.

## Value

A `ggplot` object.

## Examples

``` r
NULL
#> NULL
```
