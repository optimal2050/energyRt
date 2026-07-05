# Map inter-regional trade routes

Draws inter-regional trade routes (`src` → `dst`) as arrows between
region centroids, laid over a region map. Accepts a single `trade`, a
list of them, or a `repository`/`model`/`scenario` (all of whose trade
objects are drawn as one network). Bidirectional links (both `src`→`dst`
and `dst`→`src`, e.g. the `TBD_*` lines) are collapsed to a single
double-headed arrow. A `trade` stores no geometry, so the **map is
supplied by the caller** — an `sf` object with `region`, `x`, `y`
(centroid) columns and polygon `geometry`, such as one of the
`utopia$map` layouts (`squares`, `honeycomb`, `island`, `continent`).

## Usage

``` r
plot_trade_map(
  object,
  map = NULL,
  labels = TRUE,
  route_color = "steelblue",
  ...
)

# S3 method for class 'trade'
autoplot(object, map = NULL, ...)
```

## Arguments

- object:

  A `trade`, a list of `trade` objects, or a `repository`, `model` or
  `scenario` (whose trade objects are all drawn).

- map:

  An `sf`/data.frame with `region`, `x`, `y` and polygon `geometry`
  (e.g. `utopia$map$honeycomb`). Required. Region polygons need `sf`;
  without it, only centroids and routes are drawn.

- labels:

  Logical; label region centroids with their names (default `TRUE`).

- route_color:

  Colour of the route arrows.

- ...:

  Unused.

## Value

A `ggplot` object (or `NULL`, invisibly, if there is nothing to plot).

## Examples

``` r
if (FALSE) { # \dontrun{
TRD <- newTrade("TRD_ELC", commodity = "ELC",
  routes = data.frame(src = c("R1", "R2", "R3"), dst = c("R2", "R7", "R7")))
autoplot(TRD, map = utopia$map$honeycomb)
} # }
```
