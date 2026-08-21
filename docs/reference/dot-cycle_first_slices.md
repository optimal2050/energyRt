# Cycle-first timeslices for one storage

Returns one row per closed cycle: its first timeslice (earliest in
calendar order) and its SHARE of the year, which is the sum of its
members' shares – the cycle's parent share by construction. A year-long
cycle sums to 1; each of 365 daily cycles sums to 1/365; a calendar
filtered to part of a year sums to that `year_fraction`.

## Usage

``` r
.cycle_first_slices(succ, share)
```

## Arguments

- succ:

  data.frame with `timeslice` -\> `timeslicep` (the SUCCESSOR map)

- share:

  named numeric: timeslice -\> share of the year, in calendar order

## Value

data.frame with `timeslice` (the cycle's first) and `share`
