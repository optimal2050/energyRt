# Update parameter in the scenario by adding data to it

Update parameter in the scenario by adding data to it

## Usage

``` r
update_parameter(scen, param, data, path = NULL)
```

## Arguments

- scen:

  scenario object

- param:

  character, name of the parameter to update

- data:

  data.frame, data to update the parameter with

- path:

  character, path to the parameter on disk, if NULL, the function will
  try to create the path from the path to parameters in
  `scen@modInp@parameters` and the name of the parameter.

## Value

the updated `scenario` object, invisibly.
