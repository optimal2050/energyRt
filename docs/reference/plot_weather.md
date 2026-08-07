# Visualize a weather object

Plots the sub-annual weather factor `wval` (a capacity / availability
factor) of a `weather` object, in one of three styles:

- `"heatmap"` (default) — a calendar heatmap (finest timeframe on `y`,
  next on `x`), faceted by region; the value's unit is on the fill
  legend;

- `"line"` — the factor against the finest time level (e.g. hour), one
  line per coarser level (e.g. season), faceted by region;

- `"area"` — the same as `"line"` with filled areas.

The value axis (fill for the heatmap, `y` for line/area) is labelled
with the object's `@unit` (or `"capacity factor"` if unset).

## Usage

``` r
plot_weather(
  object,
  style = c("heatmap", "line", "area"),
  calendar = NULL,
  palette = "D",
  datetime = FALSE,
  angle = 45,
  ...
)

# S3 method for class 'weather'
autoplot(object, style = c("heatmap", "line", "area"), calendar = NULL, ...)
```

## Arguments

- object:

  A `weather` object.

- style:

  One of `"heatmap"` (default), `"line"`, `"area"`.

- calendar:

  A `calendar` object (or format string) giving the slice layout.
  Recommended for a fully structured view. If `NULL`, the layout is
  guessed; when that fails, `"<prefix>_h##"`-style slices (e.g.
  season+hour) are split into a coarse label + hour, otherwise slices
  are shown on a single ordered axis. In every case region and year
  (when present) are drawn as facets.

- palette:

  Viridis color option for the heatmap fill.

- datetime:

  Logical (line/area only). If `TRUE`, place the profile on a real
  datetime axis via
  [`tsl2dtm()`](https://energyRt.org/reference/timeslices.md); if the
  slice type is not yet supported the categorical axis is kept (with a
  warning).

- angle:

  Rotation (degrees) for the x-axis tick labels; overlapping labels are
  dropped so dense sub-annual axes stay legible. Default `45`; `0` =
  flat.

- ...:

  Reserved for future use.

## Value

A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).

## Examples

``` r
if (FALSE) { # \dontrun{
data("calendars", package = "energyRt")
W <- getObject(utopia_modules$electricity$R3$repo, name = "WSOL", drop = TRUE)
autoplot(W, calendar = calendars$utopia_s4h24)                     # heatmap
autoplot(W, style = "line", calendar = calendars$utopia_s4h24)
} # }
```
