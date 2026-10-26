# Trim numeric/bounds parameters to the domain of the maps that index them

Drops rows of a value parameter that lie outside the union of the
equation- domain maps referencing it (lifespan window x membership). The
maps are the authority on the minimal domain, so any parameter row no
map indexes is dead data: it bloats the written model and (for unfolded
scenarios) may carry a stale wildcard NA that is out-of-domain in the
solver.

## Usage

``` r
trim_parameters(object, verbose = FALSE)
```

## Arguments

- object:

  scenario.

- verbose:

  logical; report per-parameter trim counts.

## Value

scenario with trimmed parameter slots.

## Details

Intended for the unfolded (`fold = FALSE`) pipeline, where wildcard (NA)
dimensions have already been materialised. A parameter is left untouched
when none of its registered maps is available, to avoid clearing data
whose map was not built.

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

# drop the approximation context the scenario no longer needs; the
# parameters themselves are untouched
model_size(scen)
#> model_size: EX
#>   parameters : 200 value, 359 maps, 15 sets
#>   param rows : 18,411
#>   estimate   : ~61,849 variables, ~63,464 constraints (from gating maps)
#>   top parameters by rows:
#>     pTechCinp2use      3,840
#>     pWeather           3,012
#>     pTradeIrVarom      1,552
#>     pTradeIrEff        1,552
#>     pTimesliceAgg      1,188
#>     pDemand            1,152
#>     pExportRowPrice    1,152
#>     pExportRow         1,152
#>     pStorageInpEff     1,152
#>     pStorageOutEff     1,152
#>     pTimesliceWeight   404
#>     pTechAf            384
#>     pTimesliceShare    101
#>     pTechFixom         68
#>     pTechInvcost       68
model_size(trim_parameters(scen))
#> model_size: EX
#>   parameters : 200 value, 359 maps, 15 sets
#>   param rows : 18,394
#>   estimate   : ~61,849 variables, ~63,464 constraints (from gating maps)
#>   top parameters by rows:
#>     pTechCinp2use      3,840
#>     pWeather           3,012
#>     pTradeIrVarom      1,552
#>     pTradeIrEff        1,552
#>     pTimesliceAgg      1,188
#>     pDemand            1,152
#>     pExportRowPrice    1,152
#>     pExportRow         1,152
#>     pStorageInpEff     1,152
#>     pStorageOutEff     1,152
#>     pTimesliceWeight   404
#>     pTechAf            384
#>     pTimesliceShare    101
#>     pTechFixom         65
#>     pTechEac           54
# }
```
