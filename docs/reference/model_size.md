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

model_size(scen, top_n = 5)
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
# }
```
