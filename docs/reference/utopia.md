# UTOPIA reference maps

Geographic maps for the imaginary country "Utopia" used by the UTOPIA
vignette to lay out regions, neighbours and trade routes.

## Usage

``` r
utopia
```

## Format

A named list. Element `map` is a named list of `sf` polygon layers
(`honeycomb`, `continent`, `island`, `squares`, ...); the vignette uses
`utopia$map$honeycomb` and keeps the first few regions.

## See also

the `utopia` vignette,
[utopia_weather](https://energyRt.org/reference/utopia_weather.md),
[utopia_demand](https://energyRt.org/reference/utopia_demand.md),
[utopia_stock](https://energyRt.org/reference/utopia_stock.md),
[`utopia_profiles()`](https://energyRt.org/reference/utopia_profiles.md)

## Examples

``` r
names(utopia)
#> [1] "map"
names(utopia$map)
#> [1] "squares"   "honeycomb" "island"    "continent"
```
