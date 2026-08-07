# Balancing geo-level of commodities

The spatial twin of
[`map_comm_timeframe()`](https://energyRt.org/reference/map_comm_timeframe.md).
A commodity with no `@geolevel` is balanced at the finest level, which
is the flat, single-level behaviour.

## Usage

``` r
map_comm_geolevel(scen, comm = NULL)
```

## Arguments

- scen:

  scenario object

- comm:

  character vector of commodity names, if not provided, all commodities
  retrieved from the scenario object using the `collect_set_names`
  function.

## Value

a named list mapping each commodity to its geo-level.
