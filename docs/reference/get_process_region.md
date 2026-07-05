# Get regions for each process

Get regions for each process

## Usage

``` r
get_process_region(scen, process = NULL, classes = NULL, return_list = TRUE)
```

## Arguments

- scen:

  scenario object

- process:

  character vector of process names, if not provided, all processes in
  the scenario are returned.

- classes:

  character vector of class names to search for

- return_list:

  logical, if TRUE, return a list of results, otherwise data.frame with
  columns `process` and `region`

## Value

a list of named vectors, where each vector contains the regions
associated with a process. The names of the list elements are the
process names, and the values are the regions associated with each
process. If `return_list` is FALSE, the function returns a data frame
with two columns: "process" and "region", where each row represents a
process-region pair. The data frame is unique and sorted by process and
region.
