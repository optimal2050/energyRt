# The year untagged money is read at

Money without a year is read at this year – the latest the shipped
exchange rate table covers for a quorum of currencies, and the year
`convert_data$base$Currency` is generated from. Set it to pin
conversions to a different year.

## Usage

``` r
currency_base_year(year = NULL)
```

## Arguments

- year:

  a four-digit year, or `NULL` to read the current setting.

## Value

The base year, invisibly when setting.

## See also

[currency_fx](https://energyRt.org/reference/currency_fx.md),
[`convert()`](https://energyRt.org/reference/convert.md)

## Examples

``` r
currency_base_year()
#> [1] 2025
```
