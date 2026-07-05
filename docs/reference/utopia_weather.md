# UTOPIA representative capacity-factor profiles

Deterministic solar / wind / hydro capacity factors for the UTOPIA
model, provided for both teaching calendars (`utopia_m12h24`, 288
slices, the default; and `utopia_seasons`, 12). Region-agnostic; expand
to regions with
[`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md).
Built by `data-raw/utopia_data.R` from IDEEA reanalysis profiles (with a
curated fallback when IDEEA is absent); see `attr(.,"source")`.

## Usage

``` r
utopia_weather
```

## Format

A data.frame with columns `calendar` (`utopia_m12h24`/`utopia_seasons`),
`resource` (`WSOL`/`WWIN`/`WHYD`), `slice` (e.g. `m06_h12` or `SUM_DAY`)
and `wval` (capacity factor, 0-1). Attribute `source`.

## See also

[`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md),
[utopia_demand](https://energyRt.org/reference/utopia_demand.md),
[utopia_stock](https://energyRt.org/reference/utopia_stock.md),
[calendars](https://energyRt.org/reference/calendars.md)

## Examples

``` r
head(utopia_weather)
#>       calendar resource   slice       wval
#> 1 utopia_s4h24     WSOL AUT_h00 0.00000000
#> 2 utopia_s4h24     WSOL AUT_h01 0.00000000
#> 3 utopia_s4h24     WSOL AUT_h02 0.00000000
#> 4 utopia_s4h24     WSOL AUT_h03 0.00000000
#> 5 utopia_s4h24     WSOL AUT_h04 0.00000000
#> 6 utopia_s4h24     WSOL AUT_h05 0.00132967
attr(utopia_weather, "source")
#> [1] "IDEEA::ideea_modules$electricity$reg5 (d365_h24 CL01, calendar-aggregated)"
```
