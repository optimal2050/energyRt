# Size estimate of an interpolated scenario and the result of folding

Size estimate of an interpolated scenario and the result of folding

## Usage

``` r
model_size(object, top_n = 15L)
```

## Arguments

- object:

  a scenario built by
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md).

- top_n:

  number of largest parameters to list.

## Value

an S3 `model_size` object (see `print.model_size()`).

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

model_size(scen, top_n = 5)
#> model_size: EX
#>   parameters : 189 value, 352 maps, 15 sets
#>   param rows : 18,411
#>   estimate   : ~61,849 variables, ~63,464 constraints (from gating maps)
#>   top parameters by rows:
#>     pTechCinp2use      3,840
#>     pWeather           3,012
#>     pTradeIrVarom      1,552
#>     pTradeIrEff        1,552
#>     pTimesliceAgg      1,188
# }
```
