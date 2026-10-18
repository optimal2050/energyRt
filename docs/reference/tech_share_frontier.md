# Extract feasible share ranges for grouped inputs/outputs

Purely geometric helper — no model solve required. Returns the feasible
share band for each commodity in each constrained input or output group
of the technology.

## Usage

``` r
tech_share_frontier(object)
```

## Arguments

- object:

  A `technology` S4 object.

## Value

A `data.frame` or `NULL` if no constrained groups found.

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

# the feasible share band per commodity, and what it does to efficiency
tech_share_frontier(chp)
#>   tech direction group comm others n_in_group share_lo share_hi share_lo_eff
#> 1  CHP     input  fuel  COA    BIO          2        0      1.0          0.7
#> 2  CHP     input  fuel  BIO    COA          2        0      0.3          0.0
#>   share_hi_eff
#> 1          1.0
#> 2          0.3
```
