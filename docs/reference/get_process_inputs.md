# Get inputs of processes

Get inputs of processes

## Usage

``` r
get_process_inputs(object, process = NULL, classes = NULL)
```

## Arguments

- object:

  scenario object

- process:

  character vector of process names, if not provided, all processes
  retrieved from the scenario object

- classes:

  character vector of class names to search for

## Value

a named list mapping each process to its input commodities.

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

get_process_inputs(scen, process = c("ECOA", "EGAS"))
#> $ECOA
#> [1] "COA"
#> 
#> $EGAS
#> [1] "GAS"
#> 
# }
```
