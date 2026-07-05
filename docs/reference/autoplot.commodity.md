# Visualize a Commodity object

Plots the commodity's emission factors (`@emis`) as bars — one bar per
emission species, with the y-axis in the emission unit (e.g. `kt/GWh`).
Additional `commodity` objects can be passed via `...` to compare
emission intensities side by side (e.g. `autoplot(COA, OIL, GAS)`).
Commodities with no emission factors produce a message and return `NULL`
invisibly.

## Usage

``` r
# S3 method for class 'commodity'
autoplot(object, ...)
```

## Arguments

- object:

  A `commodity` object.

- ...:

  Optional further `commodity` objects to include in the comparison.

- palette:

  Viridis color option for the emission-species fill scale.

## Value

A `ggplot` object, or `NULL` (invisibly) if there is nothing to plot.

## Examples

``` r
if (FALSE) { # \dontrun{
coa <- newCommodity("COA", emis = data.frame(comm = "CO2", unit = "kt/GWh", emis = 0.33))
autoplot(coa)
} # }
```
