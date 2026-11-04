# Get operational timeframe of processes

Get operational timeframe of processes

## Usage

``` r
get_process_timeframe(object, process = NULL, comm_timeframe = NULL)
```

## Arguments

- object:

  scenario object

- process:

  character vector of process names, if not provided, all processes
  retrieved from the scenario object

- comm_timeframe:

  character vector of commodity timeframes, if not provided, will be
  retrieved from the scenario object

## Value

a named list mapping each process to its operational timeframe.

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

# the timeframe a process is resolved at: the finest of its commodities
get_process_timeframe(scen, process = c("ECOA", "SUP_COA"))
#> $ECOA
#> [1] "HOUR"
#> 
#> $SUP_COA
#> [1] "ANNUAL"
#> 
# }
```
