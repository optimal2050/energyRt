# Visualize a demand object

Two views of a `demand` object:

- `style = "area"` (default):

  **aggregated** demand – timeslice values are summed to annual totals
  and drawn as an area over the years (stacked by region), with the
  given data years marked as points.

- `style = "line"`:

  **profiles** – the within-year demand shape, drawn by the same engine
  as [`plot_weather()`](https://energyRt.org/reference/plot_weather.md):
  the finest time level on `x`, one line per coarser level, faceted by
  region and year.

- `style = "heatmap"`:

  a calendar heatmap of the demand shape – the same layout as the
  [`plot_weather()`](https://energyRt.org/reference/plot_weather.md)
  heatmap (finest timeframe on `y`, next on `x`), faceted by region and
  year.

## Usage

``` r
plot_demand(
  object,
  style = c("area", "line", "heatmap"),
  year = NULL,
  interpolate = TRUE,
  palette = "D",
  calendar = NULL,
  ...
)

# S3 method for class 'demand'
autoplot(
  object,
  style = c("area", "line", "heatmap"),
  year = NULL,
  interpolate = TRUE,
  palette = "D",
  calendar = NULL,
  ...
)
```

## Arguments

- object:

  A `demand` object.

- style:

  `"area"` (annual totals), `"line"` (timeslice profiles) or `"heatmap"`
  (calendar heatmap by region).

- year:

  Optional integer vector of years. For `"area"` these are the
  interpolation targets (default: range of the given years); for
  `"line"` and `"heatmap"` they filter which given years are shown.

- interpolate:

  Logical, default `TRUE`: for `"area"`, interpolate the annual totals
  over the target years; `FALSE` aggregates only the given data years.

- palette:

  viridis palette option, as in
  [`ggplot2::scale_fill_viridis_d()`](https://ggplot2.tidyverse.org/reference/scale_viridis.html).

- calendar:

  Optional `calendar` object ordering the heatmap's timeslice axis.

- ...:

  Passed to [`getData()`](https://energyRt.org/reference/getData.md)
  (e.g. `region =` filter).

## Value

A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).
