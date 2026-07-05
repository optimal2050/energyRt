# Visualize a demand object

Two views of a `demand` object:

- `type = "area"` (default):

  **aggregated** demand – slice values are summed to annual totals and
  drawn as an area over the years (stacked by region), with the given
  data years marked as points.

- `type = "line"`:

  **profiles** – the within-year demand shape by region and year. Slices
  with an hour tag (`"..._h07"`) are drawn against the hour of day
  (faceted season x region when a season prefix is present); other
  calendars fall back to the slice sequence.

## Usage

``` r
plot_demand(object, type = c("area", "line"), years = NULL, ...)

# S3 method for class 'demand'
autoplot(object, type = c("area", "line"), years = NULL, ...)
```

## Arguments

- object:

  A `demand` object.

- type:

  `"area"` (annual totals) or `"line"` (slice profiles).

- years:

  Optional integer vector of years. For `"area"` these are the
  interpolation targets (default: range of the given years); for
  `"line"` they filter which given years are shown.

- ...:

  Passed to [`getData()`](https://energyRt.org/reference/getData.md)
  (e.g. `region =` filter).

## Value

A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).
