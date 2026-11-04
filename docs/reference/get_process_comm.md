# Commodities associated with a processes

Commodities associated with a processes

## Usage

``` r
get_process_comm(object, process = NULL, classes = NULL, return_list = TRUE)
```

## Arguments

- object:

  scenario object

- process:

  character vector of process names, if not provided,

- classes:

  character vector of class names to search for

- return_list:

  logical, if TRUE, return a list of results, otherwise data.frame with
  columns `process` and `comm`

## Value

a named list, or a data.frame with columns `process` and `comm`.

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

# every commodity a process touches, whichever side it enters on
get_process_comm(scen, process = c("ECOA", "STG_ELC"))
#> $ECOA
#> [1] "COA" "ELC"
#> 
#> $STG_ELC
#> [1] "ELC"
#> 

# as a two-column table instead of a list
head(get_process_comm(scen, classes = "technology", return_list = FALSE), 4)
#> $EBIO
#> [1] "BIO" "ELC"
#> 
#> $ECOA
#> [1] "COA" "ELC"
#> 
#> $EGAS
#> [1] "GAS" "ELC"
#> 
#> $EHYD
#> [1] "HYD" "ELC"
#> 
# }
```
