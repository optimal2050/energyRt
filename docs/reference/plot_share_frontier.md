# Plot feasible share-mix diagram for technology inputs/outputs

Builds the diagonal share-mix chart from a
[`tech_share_frontier`](https://energyRt.org/reference/tech_share_frontier.md)
data.frame. Returns a named list of class `"share_frontier_plots"` of
`ggplot2` objects, one per (direction × group).

## Usage

``` r
plot_share_frontier(object, title = NULL, base_size = 11L)
```

## Arguments

- object:

  data.frame from
  [`tech_share_frontier()`](https://energyRt.org/reference/tech_share_frontier.md).

- title:

  Optional character string prepended to each plot title.

- base_size:

  Integer. Base font size passed to
  [`theme_energyRt()`](https://energyRt.org/reference/theme_energyRt.md).

## Value

A list of class `"share_frontier_plots"`, or `NULL`.

## Examples

``` r
# a co-firing plant whose biomass may cover at most 30% of the fuel group
chp <- newTechnology(
  "CHP",
  input   = data.frame(comm = c("COA", "BIO"), group = "fuel"),
  output  = data.frame(comm = "ELC"),
  group   = data.frame(group = "fuel", unit = "MMBtu"),
  ceff    = data.frame(comm = c("COA", "BIO"), cinp2use = c(0.40, 0.35),
                       grp.share.up = c(1, 0.3)),
  cap2act = 8760
)

# one ggplot per (direction x group)
p <- plot_share_frontier(tech_share_frontier(chp))
names(p)
#> [1] "input:::fuel"
p[[1]]
```
