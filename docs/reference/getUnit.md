# Unit of an object's own measure

The one quantity an object measures itself in, as opposed to the table
of parameter units
[`getUnits()`](https://energyRt.org/reference/getUnits.md) reports: the
commodity a flow is counted in for `commodity`, `demand`, `import`,
`export`, `supply` and `trade`, and the value of the series for
`weather`.

## Usage

``` r
getUnit(object, ...)

# S4 method for class 'commodity'
getUnit(object, ...)

# S4 method for class 'demand'
getUnit(object, ...)

# S4 method for class 'import'
getUnit(object, ...)

# S4 method for class 'export'
getUnit(object, ...)

# S4 method for class 'supply'
getUnit(object, ...)

# S4 method for class 'weather'
getUnit(object, ...)

# S4 method for class 'technology'
getUnit(object, ...)

# S4 method for class 'storage'
getUnit(object, ...)

# S4 method for class 'trade'
getUnit(object, ...)

# S4 method for class 'tax'
getUnit(object, ...)

# S4 method for class 'subsidy'
getUnit(object, ...)
```

## Arguments

- object:

  an object with a `@units` slot.

- ...:

  not used.

## Value

A single character value, or `NA_character_` when the object declares no
unit.

## Details

It is stored in `@units` – the `comm` column, or `wval` for `weather` –
and set with the `unit =` argument of the constructors. Up to version
0.91 it was a slot of its own, `@unit`; that slot is gone, and this
accessor replaces every read of it.

## Examples

``` r
getUnit(newCommodity(name = "ELC", desc = "Electricity", unit = "GWh"))
#> [1] "GWh"
getUnit(newCommodity(name = "ELC"))
#> [1] NA
```
