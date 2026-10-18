# Update parameter in the scenario by adding data to it

Update parameter in the scenario by adding data to it

## Usage

``` r
update_parameter(object, param, data, path = NULL)
```

## Arguments

- object:

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

## Examples

``` r
# \donttest{
ek   <- topia$modules$electricity$R3
mod  <- newModel("TOPIA", data = ek$repo,
                 calendar = topia$modules$calendars$s4_h24,
                 region = ek$regions,
                 horizon = topia$modules$horizons$base,
                 discount = 0.05)
scen <- interpolate_model(mod, name = "EX")

# write one parameter back after editing it outside the model. The data
# must carry exactly the parameter's own columns -- `getData()` adds the
# scenario name and the variant provenance, which are not part of it.
af <- as.data.frame(getData(scen, "pTechAf", merge = TRUE))
af <- af[, c("tech", "region", "year", "timeslice", "type", "value")]
af$value <- af$value * 0.98

scen <- update_parameter(scen, "pTechAf", af)
# }
```
