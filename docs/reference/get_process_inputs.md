# Get inputs of processes

Get inputs of processes

## Usage

``` r
get_process_inputs(scen, process = NULL, classes = NULL)
```

## Arguments

- scen:

  scenario object

- process:

  character vector of process names, if not provided, all processes
  retrieved from the scenario object

- classes:

  character vector of class names to search for

## Value

a named list mapping each process to its input commodities.
