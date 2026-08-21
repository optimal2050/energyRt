# Duration bands of a full-year battery, precomputed

The
[`storage_duration()`](https://energyRt.org/reference/storage_duration.md)
decomposition of a battery in an hourly, full-year wind-solar model
built on [vre_cf](https://energyRt.org/reference/vre_cf.md) and
`calendars$d365_h24`. Duration bands only say something when the model
can hold energy for weeks, which needs all 8760 timeslices – a solve of
minutes, so it is done once and the result ships.
`data-raw/vre_storage_duration.R` is the model, and the Storage article
shows it as a non-evaluated chunk beside this table.

## Usage

``` r
vre_storage_duration
```

## Format

A data.frame with one row per timeslice and band:

- stg:

  storage name.

- timeslice:

  timeslice, `"d001_h00"` to `"d365_h23"`.

- datetime:

  the timeslice as a `POSIXct`, for plotting.

- duration:

  ordered factor of duration bands, shortest first.

- value:

  energy in that band; the bands sum to `vStorageLevel`.

## Source

Solved with energyRt on julia/HiGHS. See
`data-raw/vre_storage_duration.R`.

## See also

[`storage_duration()`](https://energyRt.org/reference/storage_duration.md),
[vre_cf](https://energyRt.org/reference/vre_cf.md)

## Examples

``` r
# the bands partition the level, so they sum back to it
tapply(vre_storage_duration$value, vre_storage_duration$duration, sum)
#>        <12h      12h-1d       1d-1w      1w-30d        >30d 
#>    3848.461    6035.640   56860.498  173581.134 2141880.391 
```
