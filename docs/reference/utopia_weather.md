# UTOPIA representative capacity-factor profiles

Deterministic solar / wind / hydro capacity factors for the UTOPIA
model, provided for the three teaching calendars (`s4_h24`, 96
timeslices, the default base case; `m12_h24`, 288; and `utopia_seasons`,
12). Region-agnostic; expand to regions with
[`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md).
Built by `data-raw/utopia_data.R` from IDEEA reanalysis profiles (with a
curated fallback when IDEEA is absent); see `attr(.,"source")`.

## Usage

``` r
utopia_weather
```

## Format

A data.frame with columns `calendar`
(`s4_h24`/`m12_h24`/`utopia_seasons`), `resource`
(`WSOL`/`WWIN`/`WHYD`), `timeslice` (e.g. `SUM_h12`, `m06_h12` or
`SUM_DAY`) and `wval` (capacity factor, 0-1). Attribute `source`.

## See also

[`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md),
[utopia_demand](https://energyRt.org/reference/utopia_demand.md),
[utopia_stock](https://energyRt.org/reference/utopia_stock.md),
[calendars](https://energyRt.org/reference/calendars.md)

## Examples

``` r
head(utopia_weather)
#>   calendar resource timeslice       wval
#> 1   s4_h24     WSOL   FAL_h00 0.00000000
#> 2   s4_h24     WSOL   FAL_h01 0.00000000
#> 3   s4_h24     WSOL   FAL_h02 0.00000000
#> 4   s4_h24     WSOL   FAL_h03 0.00000000
#> 5   s4_h24     WSOL   FAL_h04 0.00000000
#> 6   s4_h24     WSOL   FAL_h05 0.00132967
attr(utopia_weather, "source")
#> [1] "IDEEA::ideea_modules$electricity$reg5 (d365_h24 CL01, calendar-aggregated) [carried over; re-keyed 2026-08]"
```
