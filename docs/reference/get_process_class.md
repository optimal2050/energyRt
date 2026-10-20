# Get class of processes

Get class of processes

## Usage

``` r
get_process_class(object, process = NULL, classes = NULL)
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

a named list mapping each process to its class.

## Examples

``` r
# \donttest{
ek   <- topia$modules$electricity$R3
mod  <- newModel("TOPIA", data = ek$repo,
                 calendar = topia$modules$calendars$s4_h24,
                 region = ek$regions,
                 horizon = topia$modules$horizons$base,
                 discount = 0.05)
mod <- setGeoscale(mod, ek$geoscale)
scen <- interpolate_model(mod, name = "EX")
#> Cost declared at a coarse geoscale level:
#>   TBD_COA_WEST_CENTRAL@invcost: CENTRAL, WEST
#>   TBD_GAS_WEST_CENTRAL@invcost: CENTRAL, WEST
#> The cost is charged ONCE at that cell rather than at each child region, and is discounted at the children's rate. That is usually what a national cost datum means -- said here because it is a different number from naming the children individually.

# the class of the processes asked for, or of every one of them
get_process_class(scen, process = c("ECOA", "STG_ELC"))
#> $ECOA
#> [1] "technology"
#> 
#> $STG_ELC
#> [1] "storage"
#> 
get_process_class(scen, classes = "storage")
#> $STG_ELC
#> [1] "storage"
#> 
# }
```
