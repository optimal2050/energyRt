# Visualize a process object over years

Plots each year-indexed level parameter of the object against year,
using [`getData()`](https://energyRt.org/reference/getData.md) both for
the given data (points) and its interpolation (lines): supply
`ava.lo/up/fx` (+`cost`), demand `dem`, import `imp.lo/up/fx`
(+`price`), export `exp.lo/up/fx` (+`price`), and for
`technology`/`storage` their economics and capacity — base-year `stock`,
the filled `cap`/`ncap`/`ret` bounds, `invcost`, `fixom` and `varom`
(efficiency coefficients are structural and shown by
[`draw()`](https://energyRt.org/reference/draw.md) instead). Only
populated parameters appear; each is faceted by its base name so bounds
and costs keep separate y-scales. A constant parameter (a single or
unset year) is drawn as a flat dashed line showing the interpolation
direction.

## Usage

``` r
# S3 method for class 'supply'
autoplot(object, years = NULL, ...)

# S3 method for class 'import'
autoplot(object, years = NULL, ...)

# S3 method for class 'export'
autoplot(object, years = NULL, ...)

# S3 method for class 'technology'
autoplot(object, years = NULL, ...)

# S3 method for class 'storage'
autoplot(object, years = NULL, ...)
```

## Arguments

- object:

  A `supply`, `demand`, `import`, `export`, `technology`, or `storage`
  object.

- years:

  Optional integer vector of target years to interpolate over. Defaults
  to the range of years present in the object's data.

- ...:

  Passed to [`getData()`](https://energyRt.org/reference/getData.md)
  (e.g. `region=`, `slice=` filters).

## Value

A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).
