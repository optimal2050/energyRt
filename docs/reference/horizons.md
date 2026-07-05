# Example planning horizons

A named list of ready-to-use
[horizon](https://energyRt.org/reference/class-horizon.md) objects with
common milestone-year structures. Pass any element to
[`newModel()`](https://energyRt.org/reference/newModel.md) /
`setHorizon()`, or visualize it with
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) / `autoplot()`.

## Usage

``` r
horizons
```

## Format

A named list of `horizon` objects, including:

- Y2020_2060_by_5:

  2020-2060 in 5-year steps (base year 2020).

- Y2020_2060_by_10:

  2020-2060 in 10-year steps.

- Y2020, Y2030, Y2040, Y2050, Y2060, Y2070:

  single-year horizons.

Imported from the IDEEA package; see `data-raw/calendars.R` for the
generating script.

## See also

[`newHorizon()`](https://energyRt.org/reference/newHorizon.md),
[calendars](https://energyRt.org/reference/calendars.md)

## Examples

``` r
names(horizons)
#> [1] "Y2020_2060_by_5"  "Y2020_2060_by_10" "Y2020"            "Y2030"           
#> [5] "Y2040"            "Y2050"            "Y2060"            "Y2070"           
plot(horizons$Y2020_2060_by_5)
```
