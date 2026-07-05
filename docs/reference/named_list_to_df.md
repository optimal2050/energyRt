# Convert a named list of vectors to a data frame

Convert a named list of vectors to a data frame

## Usage

``` r
named_list_to_df(named_list, col_names = c("name", "value"))
```

## Arguments

- named_list:

  named list of vectors, where each vector represents a set of values
  for a given name

- col_names:

  character vector of column names for the resulting data frame. Default
  is c("name", "value"), where "name" is the name of the list element
  and "value" is the value of the list element.

## Value

data frame with two columns ("name" and "value" by default) where each
row represents a name-value pair from the input list. The function
ensures that the resulting data frame is unique and sorted by name.
