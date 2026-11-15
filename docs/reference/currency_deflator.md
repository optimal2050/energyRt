# Annual price indices, for moving money between years

A deflator moves a value between points in time within one currency,
which is a different question from an exchange rate and needs different
data. Two bases are carried: `gdp` is the default, being the
economy-wide numeraire and the convention the World Bank and IPCC use
for constant-price comparisons; `cpi` is offered because published
technology cost data is often deflated on that basis instead.

## Usage

``` r
currency_deflator
```

## Format

A data frame of one row per currency, basis and year:

- code:

  ISO 4217 currency code

- area:

  ISO 3166-1 alpha-3 code of the area the index is for

- year:

  calendar year

- basis:

  `"gdp"` or `"cpi"`

- index:

  the price index, in its own base

- source:

  the series the value came from, or `"user"` for a row from
  [`add_currency_deflator()`](https://energyRt.org/reference/add_currency.md)

## Source

World Bank World Development Indicators, series `NY.GDP.DEFL.ZS` (GDP
deflator) and `FP.CPI.TOTL` (consumer price index). Licensed CC BY 4.0,
<https://datacatalog.worldbank.org/public-licenses>.

## Details

The index base year varies by series and does not matter: only the ratio
of two years is ever used.

A deflator is national but a currency need not be, so `area` names the
country the index was taken from. Where no aggregate exists for a
currency union the largest member stands in – the euro is the live case,
having an exchange rate of its own but no euro-area deflator in these
series, so it deflates on Germany. The column is there so that
substitution is visible rather than implied.

## See also

[currency_codes](https://energyRt.org/reference/currency_codes.md),
[currency_fx](https://energyRt.org/reference/currency_fx.md)

## Examples

``` r
subset(currency_deflator, code == "USD" & basis == "gdp" & year >= 2020)
#>       code area year basis    index         source
#> 13942  USD  USA 2020   gdp 100.0000 NY.GDP.DEFL.ZS
#> 13943  USD  USA 2021   gdp 104.5630 NY.GDP.DEFL.ZS
#> 13944  USD  USA 2022   gdp 112.0000 NY.GDP.DEFL.ZS
#> 13945  USD  USA 2023   gdp 116.1443 NY.GDP.DEFL.ZS
#> 13946  USD  USA 2024   gdp 119.0274 NY.GDP.DEFL.ZS
#> 13947  USD  USA 2025   gdp 122.3616 NY.GDP.DEFL.ZS
```
