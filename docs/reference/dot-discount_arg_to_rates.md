# Translate a `discount` argument into a table of `wacc`/`sdr` rates

`discount` is an argument, not a slot column. A bare number (or a table
with a `discount` column) means "one rate for both jobs" and is expanded
into equal `wacc` and `sdr`; a table may instead give `wacc` and `sdr`
explicitly, by region and year. Mixing the two forms in one row, or
giving only one half of the pair, is an error.

## Usage

``` r
.discount_arg_to_rates(x)
```

## Arguments

- x:

  a number, or a data.frame with `region`/`year` and either `discount`
  or both `wacc` and `sdr`.

## Value

a data.frame with `wacc` and `sdr` populated on every row.
