# Drop columns in a data.frame with all NA values

A wrapper with `dplyr` functions to drop columns with no information
(all `NA` values)

## Usage

``` r
drop_na_cols(x, unique = TRUE)
```

## Arguments

- x:

  data.frame

- unique:

  logical, if TRUE (default),
  [`unique()`](https://rdrr.io/r/base/unique.html) function will be
  applied to the result.

## Value

data.frame with dropped columns

## Examples

``` r
x <- data.frame(a = c(1, 2, NA), b = c(NA, NA, NA), c = c(NA, 2, 3))
drop_na_cols(x)
#>    a  c
#> 1  1 NA
#> 2  2  2
#> 3 NA  3
```
