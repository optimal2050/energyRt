# Plot trade as a network: capacities and flows between regions

Draws trade objects as a network of regions (nodes) and links, one link
per pair of regions and commodity, its width set by a value: the input
capacities of the trade objects, the solved trade capacity
(`vTradeCap`), or the solved flows (`vTradeIr`). Links are quadratic
curves whose bends are flipped to reduce crossings.

Nodes are placed at region centroids when a `map` (an `sf` object or a
[`geoscales::GeoScale`](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html))
is given or the model carries a geoscale; otherwise they are laid out on
a circle and no geography is needed.

## Usage

``` r
plot_trade_network(
  object,
  map = NULL,
  value = c("input", "capacity", "flow"),
  year = NULL,
  timeslice = "ANNUAL",
  level = NULL,
  curvature = 0.15,
  labels = TRUE,
  node_size = TRUE,
  ...
)
```

## Arguments

- object:

  a `trade`, a list of trades, or a `repository`, `model` or `scenario`
  (all of whose trade objects are drawn). `value = "capacity"` and
  `"flow"` need a solved `scenario`.

- map:

  optional `sf` object with `region` (and `x`, `y`) columns, or a
  [`geoscales::GeoScale`](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html).
  When `NULL`, the object's geoscale is used if it has one, else a
  circular layout.

- value:

  what sets the link width:

  - `"input"`: the trade objects' `@capacity` in `year` – existing links
    (`stock` \> 0) solid, width = stock, with a faint band up to
    `cap.up`; projects (no stock, expandable through `ncap.up`, `cap.up`
    or an `@invcost` row) dashed, width = `cap.up` (else `ncap.up`);

  - `"capacity"`: the solved `vTradeCap` in `year`;

  - `"flow"`: the solved `vTradeIr` in `year`, netted per pair; the
    arrow points in the net direction.

- year:

  the year to draw; default the first year present.

- timeslice:

  for `value = "flow"`: `"ANNUAL"` (default), the year's total as
  `getData(timeframe = "lowest")` returns it (weighted to the full year
  on a sampled calendar); or any timeslice of the calendar – a day, an
  hour – for that snapshot.

- level:

  for a geoscale `map`: the level to draw; routes are lifted to it and
  those inside one region dropped.

- curvature:

  bend of the links as a share of their length (0 = straight).

- labels:

  draw region names.

- node_size:

  scale nodes by the total value of their links.

- ...:

  unused.

## Value

a `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_trade_network(repo)                                  # inputs, circular layout
plot_trade_network(scen, value = "capacity", year = 2050) # solved capacity on the map
plot_trade_network(scen, value = "flow", timeslice = "d180_h18")
} # }
```
