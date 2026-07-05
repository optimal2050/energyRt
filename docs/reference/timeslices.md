# Common formats of time-slices.

This set of functions converts date-time objects to model's time-slices
in a given format, and vice versa, maps time-slices to date-time, and
extracts year, month, day of the year, hour.

## Usage

``` r
tsl_formats

tsl_sets

dtm2tsl(dtm, format = "d365_h24", d366.as.na = grepl("d365", format))

tsl2dtm(
  tsl,
  format = tsl_guess_format(tsl),
  tmz = "UTC",
  year = NULL,
  mday = NULL
)
```

## Format

A character vector with formats:

- d365:

  daily time-slices, 365 a year (leap year's 366th day is disregarded)

- d365_h24:

  time slices with year-day numbers and hours, 8760 in total

- ...:

  etc.

An object of class `list` of length 1.

## Arguments

- dtm:

  vector of timepoints in Date format

- format:

  character, format of the slices

- d366.as.na:

  logical, if

- tsl:

  character vector with time-slices

- tmz:

  time-zone

- year:

  year, used when time-slices don't store year

- mday:

  day of month, for time slices without the information

## Value

Character vector with time-slices names

Vector in Date-Time format

## Examples

``` r
dtm2tsl(lubridate::now())
#> [1] "d186_h09"
dtm2tsl(lubridate::ymd("2020-12-31"))
#> [1] NA
dtm2tsl(lubridate::ymd("2020-12-31"), d366.as.na = FALSE)
#> [1] "d366_h00"
dtm2tsl(lubridate::now(tzone = "UTC"), format = "d365")
#> [1] "d186"
dtm2tsl(lubridate::ymd("2020-12-31"), format = "d365")
#> [1] NA
dtm2tsl(lubridate::ymd("2020-12-31"), format = "d365", d366.as.na = FALSE)
#> [1] "d366"
dtm2tsl(lubridate::ymd("2020-12-31"), format = "d366")
#> [1] "d366"
tsl <- c("y2007_d365_h15", NA, "d151_h22", "d001", "m10_h12")
tsl2dtm(tsl[1])
#> [1] "2007-12-31 15:00:00 UTC"
tsl2dtm(tsl[1:2])
#> Warning:  1 failed to parse.
#> [1] "2007-12-31 15:00:00 UTC" NA                       
tsl2dtm(tsl[2])
#> NULL
tsl2dtm(tsl[3])
#> NULL
tsl2dtm(tsl[4])
#> NULL
tsl2dtm(tsl[3], year = 2010)
#> [1] "2010-05-31 22:00:00 UTC"
tsl2dtm(tsl[4], year = 1900)
#> [1] "1900-01-01 UTC"
tsl2dtm(tsl[3:4], year = 1900)
#> NULL
```
