# Plot a tax, subsidy or user constraint over years

A `tax`, `sub`(sidy) or `constraint` imposes a value that varies by year
— a tax/subsidy rate (`bal`) or a constraint bound (`rhs`). `autoplot()`
draws that control path: the **points** are the given years and the
**line** is the linear interpolation used between them. These are policy
*levers*, not processes.

## Usage

``` r
# S3 method for class 'tax'
autoplot(object, years = NULL, ...)

# S3 method for class 'sub'
autoplot(object, years = NULL, ...)

# S3 method for class 'constraint'
autoplot(object, years = NULL, ...)
```

## Arguments

- object:

  A `tax`, `sub` or `constraint` object.

- years:

  Optional integer vector of years to draw the interpolated line over
  (defaults to the range of the given years).

- ...:

  Unused.

## Value

A `ggplot` object (or `NULL`, invisibly, when there is nothing
year-indexed to plot).
