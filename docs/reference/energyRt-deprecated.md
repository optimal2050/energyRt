# Deprecated plotting names

The figure functions that return a ggplot object are named `gg_*`; the
former `plot_*` names forward to them with a one-time warning.
`plot_geoscale()` drew a model's geoscale and has no `gg_` successor in
energyRt: call
[`geoscales::gg_geoscale()`](https://optimal2050.github.io/geoscales/r/reference/gg_geoscale.html)
on `getGeoscale(object)` instead, with `type = "map"`, `"stack"` or
`"icicle"`.

## Usage

``` r
plot_map(object, ...)

plot_heatmap(object, ...)

plot_trade_map(object, ...)

plot_trade_network(object, ...)

plot_share_frontier(object, ...)

plot_geoscale(
  object,
  type = c("map", "stack", "icicle"),
  geoframe = NULL,
  view = "cabinet",
  direction = "down",
  geoscale = NULL,
  ...
)
```

## Arguments

- object, ...:

  Passed to the renamed function.

- type, geoframe, view, direction, geoscale:

  The former `plot_geoscale()` arguments.

## Value

What the renamed function returns.
