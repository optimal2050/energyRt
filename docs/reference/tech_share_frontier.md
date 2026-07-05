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
