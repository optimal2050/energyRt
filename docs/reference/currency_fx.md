# Annual exchange rates, local currency per US dollar

One rate per currency and year, the annual average. The column is named
`lcu_per_usd` rather than anything shorter because the direction is the
easiest thing to invert by accident: `convert_data`'s coefficients are
US dollars *per unit*, the reciprocal of what is stored here.

## Usage

``` r
currency_fx
```

## Format

A data frame of one row per currency and year:

- code:

  ISO 4217 currency code

- year:

  calendar year

- lcu_per_usd:

  units of the currency per US dollar, annual average

- source:

  the series the value came from, `"numeraire"` for the US dollar's own
  rows, which are 1 by definition rather than by measurement, or
  `"user"` for a row from
  [`add_currency_fx()`](https://energyRt.org/reference/add_currency.md)

## Source

World Bank World Development Indicators, series `PA.NUS.FCRF` (official
exchange rate, LCU per US\$, period average), which credits IMF
International Financial Statistics as its own source. Licensed CC BY
4.0, <https://datacatalog.worldbank.org/public-licenses>.

## Details

Rates are keyed by currency, not by country. Members of a currency union
report the same rate, so one row serves them all. `USD` is the numeraire
and is 1 by construction. The euro begins in 1999; a year before that
has no euro rate because there was no euro.

`attr(currency_fx, "base_year")` is the year untagged money is read at –
the latest year the table covers for a quorum of currencies, and the
year `convert_data$base$Currency` is generated from.
[`currency_base_year()`](https://energyRt.org/reference/currency_base_year.md)
reads it, or sets it elsewhere.

The table stops at the year it was built from.
[`add_currency_fx()`](https://energyRt.org/reference/add_currency.md)
adds a row, or replaces one, for the current session.

## See also

[currency_codes](https://energyRt.org/reference/currency_codes.md),
[currency_deflator](https://energyRt.org/reference/currency_deflator.md)

## Examples

``` r
attr(currency_fx, "base_year")
#> [1] 2025
subset(currency_fx, code == "EUR" & year >= 2020)
#>      code year lcu_per_usd      source
#> 2351  EUR 2020   0.8755064 PA.NUS.FCRF
#> 2352  EUR 2021   0.8454941 PA.NUS.FCRF
#> 2353  EUR 2022   0.9496238 PA.NUS.FCRF
#> 2354  EUR 2023   0.9248396 PA.NUS.FCRF
#> 2355  EUR 2024   0.9238895 PA.NUS.FCRF
#> 2356  EUR 2025   0.8849690 PA.NUS.FCRF
```
