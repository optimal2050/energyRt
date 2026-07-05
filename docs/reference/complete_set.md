# Complete a data frame with missing set elements

Replaces `NA` values in a data frame column `set_name` with missing
values from `full_set` for each unique combination of other columns.

## Usage

``` r
complete_set(x, set_name, full_set, ...)
```

## Arguments

- x:

  data frame with columns of sets and parameters

- set_name:

  name of the set to complete

- full_set:

  character vector, named list, or data frame with all possible
  combinations of the `set_name` elements and other columns. If
  character vector, it is converted to a data frame with one column. If
  names list, the `set_name` is taken from the names of the list. If
  data frame, all columns matching `x` are considered as a complete set.

- ...:

  additional arguments (currently unused).

## Value

a data frame with the completed set combinations.
