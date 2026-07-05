# Operational timeframe of a commodities

Operational timeframe of a commodities

## Usage

``` r
map_comm_timeframe(scen, comm = NULL)
```

## Arguments

- scen:

  scenario object

- comm:

  character vector of commodity names, if not provided, all commodities
  retrieved from the scenario object using the `collect_set_names`
  function.

## Value

a named list mapping each commodity to its timeframe.
