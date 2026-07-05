# Apply function to scenario data

Apply function to scenario data

## Usage

``` r
apply_to_scenario_data(scen, func, ..., classes = NULL, as_list = TRUE)
```

## Arguments

- scen:

  scenario object

- func:

  function to apply to every object of class `classes` in the scenario's
  model data

- ...:

  additional arguments passed to `func`.

- classes:

  character vector of class names to apply the function to

- return_list:

  logical, if TRUE, return a list of results, otherwise return a vector
  of results

## Value

a list or vector of `func` results, one per matching object.
