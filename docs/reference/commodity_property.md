# Physical property of a commodity

Read a single property out of `commodity@property`, or the whole table.
Safe on commodity objects created before the slot existed.

## Usage

``` r
commodity_property(
  x,
  property = NULL,
  stat = c("value", "range", "min", "max", "sd", "unit", "dist", "comment", "row")
)
```

## Arguments

- x:

  a `commodity` object.

- property:

  character, name of the property, e.g. `"lhv"`. If `NULL` (the
  default), the whole property table is returned.

- stat:

  character, which part of the row to return. `"value"` (the default)
  gives the deterministic point estimate; `"range"` gives `c(min, max)`;
  `"row"` gives the whole row as a one-row data.frame.

## Value

For `stat = "value"`, `"min"`, `"max"` or `"sd"` a numeric of length
one; for `"range"` a numeric of length two; for `"unit"`, `"dist"` or
`"comment"` a character of length one; for `"row"` a one-row data.frame.
Missing properties give `NA` (or a zero-row data.frame for `"row"`).

## See also

[`commodity_properties()`](https://energyRt.org/reference/commodity_properties.md),
[`newCommodity()`](https://energyRt.org/reference/newCommodity.md)

Other commodity:
[`commodity_properties()`](https://energyRt.org/reference/commodity_properties.md),
[`newCommodity()`](https://energyRt.org/reference/newCommodity.md),
[`object_image()`](https://energyRt.org/reference/object_image.md)

## Examples

``` r
COA <- newCommodity("COA",
  unit = "PJ",
  property = data.frame(
    property = c("lhv", "density"),
    value = c(25.8, 0.85),
    min = c(24.1, 0.75),
    max = c(27.2, 0.95),
    dist = c("uniform", "triangular"),
    unit = c("GJ/t", "t/m3")
  )
)
commodity_property(COA, "lhv")
#> [1] 25.8
commodity_property(COA, "lhv", "range")
#> [1] 24.1 27.2
commodity_property(COA)
#>   property value   min   max sd       dist unit comment
#> 1      lhv 25.80 24.10 27.20 NA    uniform GJ/t    <NA>
#> 2  density  0.85  0.75  0.95 NA triangular t/m3    <NA>
```
