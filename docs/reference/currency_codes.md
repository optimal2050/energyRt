# Currency spellings, and the ISO code and scale each denotes

Every currency spelling
[`convert()`](https://energyRt.org/reference/convert.md) accepts,
resolved to an ISO 4217 code and a scale. `scale` is how many of the ISO
unit the spelling denotes, which covers sub-units (`cents` is 0.01 USD)
and super-units (`crore INR` is 1e7 INR) with one mechanism, so an
exchange rate is stored once per currency rather than once per spelling.

## Usage

``` r
currency_codes
```

## Format

A data frame of 25 rows and 4 columns:

- spelling:

  the string a user writes

- code:

  ISO 4217 currency code

- scale:

  how many of the ISO unit the spelling denotes

- si_prefixes:

  may an SI prefix be attached?

## Details

Row order is the column order of `convert_data$base$Currency` and is
load-bearing: unit lookup scans spellings and the first match wins.

`si_prefixes` says whether an SI prefix may be attached. It is `FALSE`
for every scaled spelling, which is what keeps `Mcents` unresolvable.

## See also

[currency_fx](https://energyRt.org/reference/currency_fx.md),
[currency_deflator](https://energyRt.org/reference/currency_deflator.md),
[`convert()`](https://energyRt.org/reference/convert.md)

## Examples

``` r
head(currency_codes, 5)
#>   spelling code scale si_prefixes
#> 1      USD  USD 1.000        TRUE
#> 2    cents  USD 0.010       FALSE
#> 3    mills  USD 0.001       FALSE
#> 4      RMB  CNY 1.000        TRUE
#> 5      EUR  EUR 1.000        TRUE
subset(currency_codes, code == "INR")
#>        spelling code scale si_prefixes
#> 6         cr. ₹  INR 1e+07       FALSE
#> 7          cr.₹  INR 1e+07       FALSE
#> 8       crore ₹  INR 1e+07       FALSE
#> 9     crore INR  INR 1e+07       FALSE
#> 10 INR (in cr.)  INR 1e+07       FALSE
#> 11          INR  INR 1e+00        TRUE
#> 12            ₹  INR 1e+00        TRUE
#> 13    crore_INR  INR 1e+07       FALSE
#> 14       cr.INR  INR 1e+07       FALSE
#> 15      cr. INR  INR 1e+07       FALSE
```
