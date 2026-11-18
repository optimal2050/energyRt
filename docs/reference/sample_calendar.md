# Build a calendar from a sample of another's timeslices

Returns a calendar holding only the chosen timeslices of `cal`, with the
weights that make it annualise: a sample covering half the year weighs
each of its slices twice. Used to solve a model, or one of its milestone
years, on part of the calendar rather than all of it.

## Usage

``` r
sample_calendar(cal, slices, counts = NULL, name = "sample")
```

## Arguments

- cal:

  a `calendar` to sample.

- slices:

  the timeslices to keep. Any level: a leaf set, or coarser members with
  every finer slice beneath them.

- counts:

  optional named integer vector of repeat counts per timeslice, for a
  draw with replacement.

- name:

  name for the sampled calendar. A calendar assigned to particular years
  with [`setCalendar()`](https://energyRt.org/reference/setCalendar.md)
  needs one, since the horizon refers to it by name.

## Value

A `calendar` covering part of the year, with `@year_fraction` set to the
surviving `sum(share)`.

## Details

A sample keeps the parent's timeslice NAMES, its timeframe levels and
its `share` per slice. Only membership and `weight` change, so a model
keyed on the parent's labels still matches, and `sum(share * weight)` is
1 per level as in any calendar.

At least two members must survive at every level below the year.
Dropping a level instead re-derives the leaf names – `WIN_h00` becomes
`h00` – after which the model's data matches nothing and the result
solves to zero without complaint. That is refused rather than attempted,
naming the level at fault.

`counts` carries a bootstrap repeat: a slice drawn twice cannot be a
repeated row, since a calendar holds each timeslice once, so the
repetition becomes a scaled `share`.

[`timescales::filter_calendar()`](https://optimal2050.github.io/timescales/r/reference/filter_calendar.html)
is the same operation on a
[`timescales::Calendar`](https://optimal2050.github.io/timescales/r/reference/Calendar.html),
selecting by timeframe and label and recording the parent and the
coverage in the object's metadata. This one works on energyRt's own
calendar class, which is why it exists separately.

## See also

[`calendar_samples()`](https://energyRt.org/reference/calendar_samples.md)
to plan a set of draws,
[`solve_by_sample()`](https://energyRt.org/reference/solve_by_sample.md)
to solve them as replicates, and
[`setCalendar()`](https://energyRt.org/reference/setCalendar.md) to give
one to particular milestone years.

## Examples

``` r
cal <- calendars$s4_h24
# two of the four seasons, with every hour beneath them
smp <- sample_calendar(cal, grep("^(WIN|SUM)", cal@timeslice_share$timeslice,
                                 value = TRUE), name = "half")
smp@year_fraction
#> [1] 0.005479452
# share is the parent's; weight annualises
head(smp@timeslice_share, 3)
#>    timeslice       share weight
#>       <char>       <num>  <num>
#> 1:    ANNUAL 1.000000000    1.0
#> 2:       WIN 0.002739726  182.5
#> 3:       SUM 0.002739726  182.5
```
