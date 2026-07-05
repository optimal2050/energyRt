# Get operational timeframe of processes

Get operational timeframe of processes

## Usage

``` r
get_process_timeframe(scen, process = NULL, comm_timeframe = NULL)
```

## Arguments

- scen:

  scenario object

- process:

  character vector of process names, if not provided, all processes
  retrieved from the scenario object

- comm_timeframe:

  character vector of commodity timeframes, if not provided, will be
  retrieved from the scenario object

## Value

a named list mapping each process to its operational timeframe.
