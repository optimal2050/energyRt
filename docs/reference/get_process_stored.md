# Stored commodities of each process

The commodity a process HOLDS, as distinct from what it consumes or
produces. Only `storage` has one. It is `@storage$comm` where given,
otherwise `@commodity` – so a storage that names no roles keeps today's
behaviour.

## Usage

``` r
get_process_stored(object, process = NULL, classes = NULL)
```

## Arguments

- object:

  scenario object

- process:

  optional character vector to restrict to

- classes:

  classes to search; defaults to `"storage"`

## Value

a named list mapping each process to its stored commodity

## Details

Needed because a storage may hold something that is neither its input
nor its output (hydrogen: ELC in, H2 stored, ELC out). Without this the
stored commodity never reaches
[`get_process_comm()`](https://energyRt.org/reference/get_process_comm.md),
and the storage's timeframe would be derived from its ELC flows alone –
putting the level at the wrong resolution.

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

# only `storage` holds a commodity
get_process_stored(scen)
#> $STG_ELC
#> [1] "ELC"
#> 
#> $STG_H2
#> [1] "H2"
#> 
# }
```
