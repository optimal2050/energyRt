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
  style = c("profile", "bar", "regions", "map"),
  param = NULL,
  map = NULL,
  type = NULL,
  ...
)

# S3 method for class 'import'
autoplot(
  object,
  year = NULL,
  interpolate = TRUE,
  show_defaults = FALSE,
  units = NULL,
  style = c("profile", "map"),
  param = NULL,
  map = NULL,
  ...
)

# S3 method for class 'export'
autoplot(
  object,
  year = NULL,
  interpolate = TRUE,
  show_defaults = FALSE,
  units = NULL,
  style = c("profile", "map"),
  param = NULL,
  map = NULL,
  ...
)

# S3 method for class 'technology'
autoplot(
  object,
  year = NULL,
  interpolate = TRUE,
  show_defaults = FALSE,
  units = NULL,
  style = c("profile", "map"),
  param = NULL,
  map = NULL,
  ...
)

# S3 method for class 'storage'
autoplot(
  object,
  year = NULL,
  interpolate = TRUE,
  show_defaults = FALSE,
  units = NULL,
  style = c("profile", "map"),
  param = NULL,
  map = NULL,
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

- units:

  named list of base units passed to the unit resolver for the axis
  label; `NULL` (default) resolves them from the object.

- style:

  For `supply` only, WHICH CHART: `"profile"` (default) draws the
  per-parameter year profile shared by all process classes; `"bar"`
  draws the quantity by region over the years; `"regions"` draws the
  across-region comparison — availability and cost bars per region, with
  an unlimited (`Inf`) availability shown as a translucent full-height
  bar; `"map"` draws it on a `map =` geoscale, at whatever level the
  object's own regions are declared at.

- param:

  For `supply` only, WHICH QUANTITY: `"availability"` (default, the
  bound that binds — `ava.fx` over `ava.up`), one of `"ava.up"`,
  `"ava.lo"`, `"ava.fx"` by name, or `"cost"`. The names are the columns
  of `@supply`, so the argument, the legend title and the slot agree.

- map:

  For `style = "map"`: a
  [`geoscales::GeoScale`](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html).

- type:

  deprecated, use `param`.

- ...:

  Passed to [`getData()`](https://energyRt.org/reference/getData.md)
  (e.g. `region=`, `timeslice=` filters).

## Value

A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).
