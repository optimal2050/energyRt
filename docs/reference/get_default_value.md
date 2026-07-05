# Return default value for one, several, or all parameters

Return default value for one, several, or all parameters

## Usage

``` r
get_default_value(
  scen = NULL,
  pname = NULL,
  sname = NULL,
  oname = NULL,
  class = NULL,
  bound = NULL,
  global = is.null(scen),
  one_value = FALSE
)
```

## Arguments

- scen:

  scenario object

- pname:

  character, parameter name, as it appears in the model

- sname:

  character, short name of the parameter, as it appears in classes

- class:

  character, class of the parameter to search for

- bound:

  character, name of the bound to retrieve, `lo` or `up`. If `NULL`, the
  function will return the default value for both upper and lower
  bounds.

- global:

  logical, if `FALSE`, the function will search for adjusted values for
  the parameter in the scenario object. If `TRUE` or no adjustments
  found, the function will return the default value stored in the
  package data.

- one_value:

  logical, if `TRUE`, the function will force returning a single value
  for the parameter, and will throw an error if multiple values are
  found. If `FALSE`, the function will return all found values.

## Value

A default value for one parameter, or a list of default values for
several parameters.
