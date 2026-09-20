# Visualize a process object over years

Plots each year-indexed level parameter of the object against year,
using [`getData()`](https://energyRt.org/reference/getData.md) both for
the given data (points) and its interpolation (lines): supply
`ava.lo/up/fx` (+`cost`), demand `demand`, import `imp.lo/up/fx`
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
autoplot(
  object,
  year = NULL,
  interpolate = TRUE,
  show_defaults = FALSE,
  units = NULL,
  style = c("profile", "bar", "regions"),
  type = c("availability", "cost"),
  ...
)

# S3 method for class 'import'
autoplot(
  object,
  year = NULL,
  interpolate = TRUE,
  show_defaults = FALSE,
  units = NULL,
  ...
)

# S3 method for class 'export'
autoplot(
  object,
  year = NULL,
  interpolate = TRUE,
  show_defaults = FALSE,
  units = NULL,
  ...
)

# S3 method for class 'technology'
autoplot(
  object,
  year = NULL,
  interpolate = TRUE,
  show_defaults = FALSE,
  units = NULL,
  ...
)

# S3 method for class 'storage'
autoplot(
  object,
  year = NULL,
  interpolate = TRUE,
  show_defaults = FALSE,
  units = NULL,
  ...
)
```

## Arguments

- object:

  A `supply`, `demand`, `import`, `export`, `technology`, or `storage`
  object.

- year:

  Optional integer vector of target years to interpolate over. Defaults
  to the range of years present in the object's data; a wider vector
  (e.g. `2020:2050`) extends the lines beyond the given years (constant
  extrapolation, as the model interpolates).

- interpolate:

  Logical, default `TRUE`: draw the interpolated series (lines)
  alongside the given data (points), using each parameter's own
  interpolation rule via
  [`getData()`](https://energyRt.org/reference/getData.md). `FALSE`
  shows the given data only.

- show_defaults:

  Logical, default `FALSE`. When `TRUE`, parameters of the plotted
  slot(s) that are mapped to the model but NOT set in the object are
  drawn as dotted lines at their default values (e.g. a supply without
  `ava.lo` shows its default of 0). Non-finite defaults (e.g.
  `ava.up = Inf`) are listed in the caption instead of drawn.

- style:

  For `supply` only: `"profile"` (default) draws the per-parameter year
  profile shared by all process classes; `"bar"` draws quantity or price
  by region over the years (stacked availability bars, per-region cost
  lines; `type =` picks the quantity); `"regions"` draws the
  across-region comparison — availability and cost bars per region, with
  an unlimited (`Inf`) availability shown as a translucent full-height
  bar labelled "uncapped".

- type:

  For `style = "bar"`: `"availability"` (default) or `"cost"`.

- ...:

  Passed to [`getData()`](https://energyRt.org/reference/getData.md)
  (e.g. `region=`, `timeslice=` filters).

## Value

A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).
