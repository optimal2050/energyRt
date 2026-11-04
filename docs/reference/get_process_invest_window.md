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

# the years a process may be invested in, by region
get_process_invest_window(scen, process = c("ECOA", "EWIN"))
#>     process region start   end
#>      <char> <char> <int> <int>
#>  1:    ECOA     C1  2010  2030
#>  2:    ECOA     C2  2010  2030
#>  3:    ECOA     C3  2010  2030
#>  4:    ECOA     C4  2010  2030
#>  5:    ECOA     C5  2010  2030
#>  6:    ECOA     C6  2010  2030
#>  7:    ECOA     E1  2010  2030
#>  8:    ECOA     E2  2010  2030
#>  9:    ECOA     E3  2010  2030
#> 10:    ECOA     W1  2010  2030
#> 11:    ECOA     W2  2010  2030
#> 12:    EWIN   <NA>  2015  2025
#> 13:    EWIN   <NA>  2015  2040
#> 14:    EWIN   <NA>  2015  2050
#> 15:    EWIN   <NA>  2030  2025
#> 16:    EWIN   <NA>  2030  2040
#> 17:    EWIN   <NA>  2030  2050
#> 18:    EWIN   <NA>  2045  2025
#> 19:    EWIN   <NA>  2045  2040
#> 20:    EWIN   <NA>  2045  2050
#>     process region start   end
#>      <char> <char> <int> <int>
# }
```
