# Mapping function between time-timeslices and day of the year

Mapping function between time-timeslices and day of the year

Mapping function between time-timeslices and hour

Mapping function between time-timeslices and month

## Usage

``` r
tsl2year(tsl, return.null = TRUE)

tsl2yday(tsl, return.null = TRUE)

tsl2hour(tsl, return.null = TRUE, pattern = "h[0-9]++")

tsl2month(tsl, format = tsl_guess_format(tsl), return.null = TRUE)
```

## Arguments

- tsl:

  character vector with time timeslices

- return.null:

  logical, valid for the cased then all values are NA, then NULL will be
  returned if return.null = TRUE,

- format:

  character, the time timeslices format

## Value

Integer vector of years, the same length as the input vector

Integer vector of days of the year, the same length as the input vector

Integer vector of hours, the same length as the input vector

Integer vector of months, the same length as the input vector

## Functions

- `tsl2year()`: Extract year from time-timeslices

- `tsl2yday()`: Extract the day of the year from time-timeslices

- `tsl2hour()`: Extract hour from time-timeslices

- `tsl2month()`: Extract month from time-timeslices

## Examples

``` r
tsl <- c("y2007_d365_h15", NA, "d151_h22", "d001", "m10_h12")
tsl2year(tsl)
#> Error in tsl2year(tsl): could not find function "tsl2year"
tsl
#> [1] "y2007_d365_h15" NA               "d151_h22"       "d001"          
#> [5] "m10_h12"       
tsl2yday(tsl)
#> Error in tsl2yday(tsl): could not find function "tsl2yday"
tsl
#> [1] "y2007_d365_h15" NA               "d151_h22"       "d001"          
#> [5] "m10_h12"       
tsl2hour(tsl)
#> Error in tsl2hour(tsl): could not find function "tsl2hour"
tsl2month(c("d001_h00", "d151_h22", "d365_h23"))
#> Error in tsl2month(c("d001_h00", "d151_h22", "d365_h23")): could not find function "tsl2month"
tsl2month(c("m01_h12", "m05_h02", "m10_h01"))
#> Error in tsl2month(c("m01_h12", "m05_h02", "m10_h01")): could not find function "tsl2month"
```
