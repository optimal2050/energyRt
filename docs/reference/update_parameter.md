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
topia_elc <- topia$modules$electricity
mod  <- newModel("TOPIA", data = topia_elc$repo,
                 calendar = topia$modules$calendars$s4_h24,
                 region = topia_elc$regions,
                 horizon = topia$modules$horizons$BY2025_50_by10,
                 discount = 0.05)
mod <- setGeoscale(mod, topia_elc$geoscale)
scen <- interpolate_model(mod, name = "EX")
#> Cost declared at a coarse geoscale level:
#>   TBD_COA_WEST_CENTRAL@invcost: CENTRAL, WEST
#>   TBD_COA_CENTRAL_EAST@invcost: CENTRAL, EAST
#>   TBD_GAS_EAST_CENTRAL@invcost: CENTRAL, EAST
#> The cost is charged ONCE at that cell rather than at each child region, and is discounted at the children's rate. That is usually what a national cost datum means -- said here because it is a different number from naming the children individually.

# write one parameter back after editing it outside the model. The data
# must carry exactly the parameter's own columns -- `getData()` adds the
# scenario name and the variant provenance, which are not part of it.
af <- as.data.frame(getData(scen, "pTechAf", merge = TRUE))
af <- af[, c("tech", "region", "year", "timeslice", "type", "value")]
af$value <- af$value * 0.98

scen <- update_parameter(scen, "pTechAf", af)
# }
```
