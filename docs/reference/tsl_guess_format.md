# Guess format of time-timeslices

Guess format of time-timeslices

## Usage

``` r
tsl_guess_format(tsl)
```

## Arguments

- tsl:

  character vector of time-timeslice names.

## Value

Character vector with the guessed format of the time-timeslices

## Examples

``` r
tsl <- c("y2007_d365_h15", NA, "d151_h22", "d001", "m10_h12")
tsl_guess_format(tsl)
#> Error in tsl_guess_format(tsl): could not find function "tsl_guess_format"
tsl_guess_format(tsl[1])
#> Error in tsl_guess_format(tsl[1]): could not find function "tsl_guess_format"
tsl_guess_format(tsl[2])
#> Error in tsl_guess_format(tsl[2]): could not find function "tsl_guess_format"
tsl_guess_format(tsl[3])
#> Error in tsl_guess_format(tsl[3]): could not find function "tsl_guess_format"
tsl_guess_format(tsl[4])
#> Error in tsl_guess_format(tsl[4]): could not find function "tsl_guess_format"
tsl_guess_format(tsl[5])
#> Error in tsl_guess_format(tsl[5]): could not find function "tsl_guess_format"
```
