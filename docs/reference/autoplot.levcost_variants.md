# Compare levelized cost across a technology's variant grid

`autoplot()` for a variant fan-out – the result of
[`levcost()`](https://energyRt.org/reference/levcost.md) on a technology
declaring `@vintage` or `@cluster`. The fan-out is a grid, so `x` and
`facet` pick which dimension goes where; any dimension left out is
averaged, because a levelized cost is a rate and cells cannot be summed.

## Usage

``` r
# S3 method for class 'levcost_variants'
autoplot(
  object,
  type = c("npv", "components", "totals", "frontier", "input_frontier"),
  x = c("auto", "variant", "vintage", "cluster", "region"),
  facet = NULL,
  cost_unit = NULL,
  angle = NULL,
  ...
)
```

## Arguments

- object:

  A `levcost_variants` object.

- type:

  `"npv"` (default) draws the variant comparison; any other value is
  passed to the `levcost_list` method.

- x:

  Dimension on the x axis: `"auto"` (the full cell label, same as
  `"variant"`), `"variant"`, `"vintage"`, `"cluster"` or `"region"`.

- facet:

  Dimensions to facet by, or `NULL`. The panel count is capped and the
  caption reports the cap.

- cost_unit:

  Cost unit for the y axis; defaults to the units carried by the result.

- angle:

  x-label angle. By default labels rotate once they would collide.

- ...:

  Passed to the `levcost_list` method for other `type`s.

## Value

A `ggplot` object.
