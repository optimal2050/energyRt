# Get regions for each process

Get regions for each process

## Usage

``` r
get_process_region(object, process = NULL, classes = NULL, return_list = TRUE)
```

## Arguments

- object:

  scenario object

- process:

  character vector of process names, if not provided, all processes in
  the scenario are returned.

- classes:

  character vector of class names to search for

- return_list:

  logical, if TRUE, return a list of results, otherwise data.frame with
  columns `process` and `region`

## Value

a list of named vectors, where each vector contains the regions
associated with a process. The names of the list elements are the
process names, and the values are the regions associated with each
process. If `return_list` is FALSE, the function returns a data frame
with two columns: "process" and "region", where each row represents a
process-region pair. The data frame is unique and sorted by process and
region.

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

get_process_region(scen, process = "ECOA")
#> $ECOA
#> [1] "W1" "W2" "C1"
#> 

# as a (process, region) table
head(get_process_region(scen, return_list = FALSE), 4)
#>    process region
#>     <char> <char>
#> 1: DEM_ELC     C1
#> 2: DEM_ELC     W1
#> 3: DEM_ELC     W2
#> 4:    EBIO     C1
# }
```
