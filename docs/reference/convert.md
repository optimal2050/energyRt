# Convert units

Convert units

Add units to convert function

## Usage

``` r
# S4 method for class 'character'
convert(from, to, x = 1, database = "base", ...)

# S4 method for class 'numeric'
convert(x = 1, from, to, database = "base", ...)

# S4 method for class 'character,character,numeric'
add_to_convert(
  type,
  unit,
  coef,
  alias = "",
  SI_prefixes = FALSE,
  database = "base",
  update = TRUE
)
```

## Arguments

- from:

  character of length one with unit name

- to:

  character of length one with unit name

- x:

  numeric vector with data to convert

- database:

  character name of a database with units (`base` by default, other
  options are not implemented yet).

- ...:

  currently ignored

- type:

  character, type of the unit (one of "Energy", "Power", "Mass", "Time",
  "Length", "Area", "Pressure", "Density", "Volume", "Flow Rates",
  "Currency").

- unit:

  character, the name of the new unit to add to the `database`.

- coef:

  numeric, convert factor to the base unit of this type (see the first
  column of `convert_data[[database]][[type]]`).

- alias:

  character vector, alternative name(s) for the same unit.

- SI_prefixes:

  logical, can be used with `SI` prefixes, FALSE by default.

## Value

numeric vector with converted values

updated `convert_data` in the `.GlobalEnv`, the values will not update
the package data.

## Examples

``` r
convert("MWh", "kWh")
#> [1] 1000
convert("kWh", "MJ")
#> [1] 3.6
convert("kWh/kg", "MJ/t", 1e-3)
#> [1] 3.6
convert("cents/kWh", "USD/MWh")
#> [1] 10
convert(1000, "kWh", "MWh")
#> [1] 1
convert("kWh", "MJ")
#> [1] 3.6
convert(1, "kWh/kg", "MJ/t")
#> [1] 3600
convert(5, "cents/kWh", "USD/MWh")
#> [1] 50
## Not run:
convert_data$base$Currency
#>             USD cents mills       RMB EUR  cr. ₹   cr.₹ crore ₹ crore INR
#> coef          1  0.01 0.001 0.1398601 1.1 121000 121000  121000    121000
#> SI_prefixes   1  0.00 0.000 1.0000000 1.0      0      0       0         0
#>             INR (in cr.)    INR      ₹ crore_INR cr.INR cr. INR   € Euro
#> coef              121000 0.0121 0.0121    121000 121000  121000 1.1  1.1
#> SI_prefixes            0 1.0000 1.0000         0      0       0 1.0  1.0
#>                     JPY Japanese yen         JP¥          円       CNY
#> coef        0.007142857  0.007142857 0.007142857 0.007142857 0.1398601
#> SI_prefixes 1.000000000  1.000000000 1.000000000 1.000000000 1.0000000
#>             Chinese yuan       CN¥        元
#> coef           0.1398601 0.1398601 0.1398601
#> SI_prefixes    1.0000000 1.0000000 1.0000000
add_to_convert("Currency", unit = "JPY", coef = 140)
convert_data$base$Currency
#>             USD cents mills       RMB EUR  cr. ₹   cr.₹ crore ₹ crore INR
#> coef          1  0.01 0.001 0.1398601 1.1 121000 121000  121000    121000
#> SI_prefixes   1  0.00 0.000 1.0000000 1.0      0      0       0         0
#>             INR (in cr.)    INR      ₹ crore_INR cr.INR cr. INR   € Euro JPY
#> coef              121000 0.0121 0.0121    121000 121000  121000 1.1  1.1 140
#> SI_prefixes            0 1.0000 1.0000         0      0       0 1.0  1.0   0
#>             Japanese yen         JP¥          円       CNY Chinese yuan
#> coef         0.007142857 0.007142857 0.007142857 0.1398601    0.1398601
#> SI_prefixes  1.000000000 1.000000000 1.000000000 1.0000000    1.0000000
#>                   CN¥        元
#> coef        0.1398601 0.1398601
#> SI_prefixes 1.0000000 1.0000000
## End(Not run)
```
