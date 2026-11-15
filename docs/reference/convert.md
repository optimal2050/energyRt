# Convert units

A currency may carry a four-digit year – `MEUR2015`, `MUSD2020` –
because money is the one unit whose value moves in time. Converting
between two currency-years needs an exchange rate and a price index, and
the two can be applied in either order: deflating in the source currency
and converting at the target year is not the same as converting at the
source year and deflating in the target currency. The gap between them
IS the change in the real exchange rate over the interval, and
`convert()` warns when it is material.

## Usage

``` r
# S4 method for class 'character'
convert(
  from,
  to,
  x = 1,
  database = "base",
  currency_route = "deflate_then_fx",
  basis = "gdp",
  from_year = NULL,
  to_year = NULL,
  ...
)

# S4 method for class 'numeric'
convert(
  x = 1,
  from,
  to,
  database = "base",
  currency_route = "deflate_then_fx",
  basis = "gdp",
  from_year = NULL,
  to_year = NULL,
  ...
)

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

- currency_route:

  how to combine the exchange rate with the deflators:
  `"deflate_then_fx"` (the default) deflates in the source currency and
  converts at the target year, which is the World Bank and IPCC
  convention; `"fx_then_deflate"` converts at the source year and
  deflates in the target currency; `"both"` returns one value per route,
  named.

- basis:

  which price index deflates, `"gdp"` (the default) or `"cpi"`.
  Technology cost data is often published on a CPI basis.

- from_year, to_year:

  the currency year, as an alternative to writing it into the unit.
  `convert(1, "MEUR", "MUSD", from_year = 2015, to_year = 2020)` is
  `convert(1, "MEUR2015", "MUSD2020")`.

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

- update:

  logical; when the unit already exists with different values, `TRUE`
  (default) replaces them and `FALSE` raises an error instead.

## Value

`convert()`: a numeric vector of converted values, or one value per
route, named, when `currency_route = "both"`.

`add_to_convert()`: invisibly, an updated `convert_data` ASSIGNED IN
`.GlobalEnv`. The package's own data is not changed, and the assignment
is lost with the session.

## Details

A year is read only where the unit is a currency: `MWh2020` is an
unknown unit, not a dated one.

Money with no year on either side consults no price index and never
warns – it is a pure exchange rate, read at
[`currency_base_year()`](https://energyRt.org/reference/currency_base_year.md).
That rate is generated from
[currency_fx](https://energyRt.org/reference/currency_fx.md) at one
stated base year; before version 0.93 it was typed in by hand and
undated, with a euro rate from one year beside a yen rate from another,
so untagged cross-currency answers moved once, at that release. A
conversion within one currency – `convert("EUR/kW", "MEUR/GW")`, a ratio
of prefixes – did not move and consults nothing.

## See also

[currency_fx](https://energyRt.org/reference/currency_fx.md) and
[currency_deflator](https://energyRt.org/reference/currency_deflator.md)
for the data behind a dated conversion,
[`currency_base_year()`](https://energyRt.org/reference/currency_base_year.md)
and
[`add_currency_fx()`](https://energyRt.org/reference/add_currency.md) to
read or extend it, and
[`normalize_model_currency()`](https://energyRt.org/reference/normalize_model_currency.md)
to convert a whole model's costs at once.

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

# money with a year
convert(1, "MUSD2015", "MUSD2020")    # an inflation question
#> [1] 1.082839
convert(1, "MEUR2020", "MUSD2020")    # an exchange-rate question
#> [1] 1.142196
convert(1, "MEUR2015", "MUSD2020", currency_route = "both")
#> deflate_then_fx fx_then_deflate 
#>        1.241012        1.201424 

# the value first, which is the same call with the arguments the other way
convert(1000, "kWh", "MWh")
#> [1] 1
convert(5, "cents/kWh", "USD/MWh")
#> [1] 50
convert(1, "MEUR", "MUSD", from_year = 2015, to_year = 2020)
#> Warning: EUR2015 -> USD2020: the two routes differ by 3.3% (a factor of 1.033). Using deflate_then_fx.
#> The gap IS the change in the real exchange rate over the interval, so neither route is a rounding of the other.
#> Pass currency_route = "both" to see each, or raise options(energyRt.currency_route_tol=) to quieten this.
#> [1] 1.241012

# `coef` is the factor to the type's BASE unit, which is its FIRST column
# -- the joule for Energy, not the GJ or the MWh a modeller thinks in -- so
# a dekatherm (10 therms, 1.055 GJ) is entered as 1.055e9.
convert_data$base$Energy[, 1:4]
#>             J   Wh W*s  Cal
#> coef        1 3600   1 4187
#> SI_prefixes 1    1   0    0
if (FALSE) { # \dontrun{
add_to_convert("Energy", unit = "dekatherm", coef = 1.055e9)
convert(1, "dekatherm", "GJ")   # 1.055
} # }

# NOT the way to add a currency rate: a currency coefficient is generated
# from `currency_fx` at one base year, and overwriting one here would put
# it out of step with every other currency in the table. Use
# `add_currency_fx()`, which is dated and keyed by ISO code.
```
