# UTOPIA base-year capacity stock

Deterministic base-year installed capacity per technology (GW), per
region (replaces the vignette's former `runif` stocks). Expand across
regions with
[`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md).

## Usage

``` r
utopia_stock
```

## Format

A data.frame with columns `tech` and `gw` (base-year capacity, GW).

## See also

[`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md),
[utopia_weather](https://energyRt.org/reference/utopia_weather.md),
[utopia_demand](https://energyRt.org/reference/utopia_demand.md)

## Examples

``` r
utopia_stock
#>   tech gw
#> 1 ECOA  6
#> 2 EGAS  3
#> 3 ENUC  2
#> 4 EHYD  5
#> 5 ESOL  1
#> 6 EWIN  1
```
