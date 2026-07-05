# Commodities associated with a processes

Commodities associated with a processes

## Usage

``` r
get_process_comm(scen, process = NULL, classes = NULL, return_list = TRUE)
```

## Arguments

- scen:

  scenario object

- process:

  character vector of process names, if not provided,

- classes:

  character vector of class names to search for

- return_list:

  logical, if TRUE, return a list of results, otherwise data.frame with
  columns `process` and `comm`

## Value

a named list, or a data.frame with columns `process` and `comm`.
