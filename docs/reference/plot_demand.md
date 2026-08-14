# Visualize a demand object

Two views of a `demand` object:

- `style = "area"` (default):

  **aggregated** demand – timeslice values are summed to annual totals
  and drawn as an area over the years (stacked by region), with the
  given data years marked as points.

- `style = "line"`:

  **profiles** – the within-year demand shape by region and year.
  Timeslices with an hour tag (`"..._h07"`) are drawn against the hour
  of day (faceted season x region when a season prefix is present);
  other calendars fall back to the timeslice sequence.

## Usage

``` r
plot_demand(
  object,
  style = c("area", "line"),
  year = NULL,
  interpolate = TRUE,
  palette = "D",
  ...
)

# S3 method for class 'demand'
autoplot(
  object,
  style = c("area", "line"),
  year = NULL,
  interpolate = TRUE,
  palette = "D",
  ...
)
```

## Arguments

- object:

  A `demand` object.

- style:

  `"area"` (annual totals) or `"line"` (timeslice profiles).

- year:

  Optional integer vector of years. For `"area"` these are the
  interpolation targets (default: range of the given years); for
  `"line"` they filter which given years are shown.

- interpolate:

  Logical, default `TRUE`: for `"area"`, interpolate the annual totals
  over the target years; `FALSE` aggregates only the given data years.

- palette:

  viridis palette option, as in
  [`ggplot2::scale_fill_viridis_d()`](https://ggplot2.tidyverse.org/reference/scale_viridis.html).

- ...:

  Passed to [`getData()`](https://energyRt.org/reference/getData.md)
  (e.g. `region =` filter).

## Value

A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).
