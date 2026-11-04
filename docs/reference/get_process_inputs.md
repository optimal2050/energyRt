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

get_process_inputs(scen, process = c("ECOA", "EGAS"))
#> $ECOA
#> [1] "COA"
#> 
#> $EGAS
#> [1] "GAS"
#> 
# }
```
