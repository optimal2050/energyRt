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

# drop the approximation context the scenario no longer needs; the
# parameters themselves are untouched
model_size(scen)
#> model_size: EX
#>   parameters : 200 value, 359 maps, 15 sets
#>   param rows : 124,391
#>   estimate   : ~244,839 variables, ~258,466 constraints (from gating maps)
#>   top parameters by rows:
#>     pTechCinp2use      25,728
#>     pStorageInpEff     16,896
#>     pStorageOutEff     16,896
#>     pWeather           15,560
#>     pTechUse2cact      13,056
#>     pTradeIrVarom      8,468
#>     pTradeIrEff        8,468
#>     pDemand            4,224
#>     pExportRowPrice    4,224
#>     pExportRow         4,224
#>     pTimesliceAgg      1,188
#>     pTechFixom         676
#>     pTechInvcost       676
#>     pTimesliceWeight   404
#>     pTechCap2AInp      396
model_size(trim_parameters(scen))
#> model_size: EX
#>   parameters : 200 value, 359 maps, 15 sets
#>   param rows : 123,597
#>   estimate   : ~244,839 variables, ~258,466 constraints (from gating maps)
#>   top parameters by rows:
#>     pTechCinp2use      25,728
#>     pStorageInpEff     16,896
#>     pStorageOutEff     16,896
#>     pWeather           15,560
#>     pTechUse2cact      13,056
#>     pTradeIrVarom      8,468
#>     pTradeIrEff        8,468
#>     pDemand            4,224
#>     pExportRowPrice    4,224
#>     pExportRow         4,224
#>     pTimesliceAgg      1,188
#>     pTechFixom         456
#>     pTimesliceWeight   404
#>     pTechCap2AInp      396
#>     pTechAf            384
# }
```
