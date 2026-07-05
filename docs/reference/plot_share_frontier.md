# Plot feasible share-mix diagram for technology inputs/outputs

Builds the diagonal share-mix chart from a
[`tech_share_frontier`](https://energyRt.org/reference/tech_share_frontier.md)
data.frame. Returns a named list of class `"share_frontier_plots"` of
`ggplot2` objects, one per (direction × group).

## Usage

``` r
plot_share_frontier(df, title = NULL, base_size = 11L)
```

## Arguments

- df:

  data.frame from
  [`tech_share_frontier()`](https://energyRt.org/reference/tech_share_frontier.md).

- title:

  Optional character string prepended to each plot title.

- base_size:

  Integer. Base font size passed to `theme_bw()`.

## Value

A list of class `"share_frontier_plots"`, or `NULL`.
