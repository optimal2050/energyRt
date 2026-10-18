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
scen <- interpolate_model(mod, name = "EX")

model_size(scen, top_n = 5)
#> model_size: EX
#>   parameters : 189 value, 348 maps, 15 sets
#>   param rows : 19,829
#>   estimate   : ~55,216 variables, ~57,581 constraints (from gating maps)
#>   top parameters by rows:
#>     pTechCinp2use      4,608
#>     pWeather           3,012
#>     pTradeIrVarom      1,536
#>     pTradeIrEff        1,536
#>     pTimesliceAgg      1,188
# }
```
