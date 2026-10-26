# Build a calendar-sample specification

Splits a calendar into samples for
[`solve_by_sample()`](https://energyRt.org/reference/solve_by_sample.md)
by drawing whole members of one level — seasons, weeks, days — and
keeping every finer timeslice beneath them. The result is a plain table
you can inspect, edit and pass back.

## Usage

``` r
calendar_samples(
  calendar,
  level = NULL,
  sample_size = NULL,
  n = NULL,
  method = c("sequential", "random", "bootstrap"),
  seed = NULL
)
```

## Arguments

- calendar:

  a calendar object.

- level:

  character, the level whose members are drawn; the coarsest non-ANNUAL
  level by default (e.g. `"SEASON"` for `s4_h24`).

- sample_size:

  integer, members of `level` per sample.

- n:

  integer, number of samples.

- method:

  one of `"sequential"`, `"random"`, `"bootstrap"`.

- seed:

  optional integer seed for the random methods.

## Value

a `calendar_samples` data.frame with columns `sample`, `timeslice` and
`count`.

## Details

`method` chooses the traversal:

- `"sequential"` — consecutive blocks of `sample_size` members in
  calendar order, tiling the year. Deterministic and full-coverage; the
  week-by-week or month-by-month crawl.

- `"random"` — `n` disjoint draws of `sample_size` members (the year
  sampled without replacement); coverage may be partial.

- `"bootstrap"` — `n` independent draws of `sample_size` members
  **with** replacement. A member drawn twice cannot be a repeated row (a
  calendar holds each timeslice once), so repeats are carried as a
  `count` and become a scaled share when the sample's calendar is built.

Samples are drawn as whole members of `level`. A hand-built selection
need not be a complete cross: its calendar is irregular and keeps the
full calendar's timeslice names. Each sample must keep at least two
members at every non-ANNUAL level, so `sample_size` of 1 is refused for
the coarsest level.

## See also

[`solve_by_sample()`](https://energyRt.org/reference/solve_by_sample.md)

## Examples

``` r
cal <- topia$modules$calendars$s4_h24

# two sequential blocks of timeslices, one row per (sample, timeslice)
sam <- calendar_samples(cal, n = 2, seed = 1)
sam
#> calendar_samples: 2 sample(s), method = sequential
#>   slices per sample: 48-48 | distinct slices used: 96
head(as.data.frame(sam), 3)
#>   sample timeslice count
#> 1    s01   WIN_h00     1
#> 2    s01   WIN_h01     1
#> 3    s01   WIN_h02     1

# two random samples of two seasons each, drawn without replacement
calendar_samples(cal, level = "SEASON", sample_size = 2, n = 2,
                 method = "random", seed = 42)
#> calendar_samples: 2 sample(s), method = random
#>   slices per sample: 48-48 | distinct slices used: 96
```
