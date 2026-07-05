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
  type = c("heatmap", "line", "area"),
  calendar = NULL,
  palette = "D",
  ...
)

# S3 method for class 'weather'
autoplot(object, type = c("heatmap", "line", "area"), calendar = NULL, ...)
```

## Arguments

- object:

  A `weather` object.

- type:

  One of `"heatmap"` (default), `"line"`, `"area"`.

- calendar:

  A `calendar` object (or format string) giving the slice layout.
  Recommended: for season-based calendars the layout cannot be inferred
  from slice names alone. If `NULL`, the layout is guessed and, when
  that fails, slices are shown on an ordered axis.

- palette:

  Viridis color option for the heatmap fill.

- ...:

  Reserved for future use.

## Value

A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).

## Examples

``` r
if (FALSE) { # \dontrun{
data("calendars", package = "energyRt")
W <- getObject(utopia_modules$electricity$reg3$repo, name = "WSOL", drop = TRUE)
plot_weather(W, calendar = calendars$utopia_s4h24)                 # heatmap
plot_weather(W, type = "line", calendar = calendars$utopia_s4h24)
} # }
```
