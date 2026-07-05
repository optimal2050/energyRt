# Guess format of time-slices

Guess format of time-slices

## Usage

``` r
tsl_guess_format(tsl)
```

## Arguments

- tsl:

  character vector of time-slice names.

## Value

Character vector with the guessed format of the time-slices

## Examples

``` r
tsl <- c("y2007_d365_h15", NA, "d151_h22", "d001", "m10_h12")
tsl_guess_format(tsl)
#> NULL
tsl_guess_format(tsl[1])
#> [1] "y_d365_h24"
tsl_guess_format(tsl[2])
#> NULL
tsl_guess_format(tsl[3])
#> [1] "d365_h24"
tsl_guess_format(tsl[4])
#> [1] "d365"
tsl_guess_format(tsl[5])
#> [1] "m12_h24"
```
