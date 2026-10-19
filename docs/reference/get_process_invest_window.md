# Start and end years for process by region

Start and end years for process by region

## Usage

``` r
get_process_invest_window(object, process = NULL, classes = NULL)
```

## Arguments

- object:

  scenario object

- process:

  character vector of process names, if not provided,

- classes:

  character vector of class names to search for

## Value

data frame with columns `process`, `region`, `start`, `end`

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

# the years a process may be invested in, by region
get_process_invest_window(scen, process = c("ECOA", "EWIN"))
#>    process region start   end
#>     <char> <char> <int> <int>
#> 1:    ECOA     C1  2010  2030
#> 2:    ECOA     W1  2010  2030
#> 3:    ECOA     W2  2010  2030
#> 4:    EWIN     C1  2015    NA
#> 5:    EWIN     W1  2015    NA
#> 6:    EWIN     W2  2015    NA
# }
```
