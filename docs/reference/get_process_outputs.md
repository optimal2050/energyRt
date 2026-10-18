# Get output commodities for each process

Get output commodities for each process

## Usage

``` r
get_process_outputs(object, process = NULL, classes = NULL)
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

a named list mapping each process to its output commodities.

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

get_process_outputs(scen, process = c("ECOA", "SUP_COA"))
#> $SUP_COA
#> [1] "COA"
#> 
#> $ECOA
#> [1] "ELC"
#> 
# }
```
