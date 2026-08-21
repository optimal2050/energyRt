# Split a storage level into duration bands

Decomposes `vStorageLevel` into how long the energy actually sits in the
store: the part cycling within a few hours, within a day, across a week,
and the part held for a month or more. The bands partition the level
exactly – they sum back to it at every timeslice – so they can be
stacked in an area chart directly.

## Usage

``` r
storage_duration(
  scen,
  width = c(12L, 24L, 24L * 7L, 24L * 30L),
  labels = NULL,
  cyclic = NA,
  tmz = "UTC",
  stg = NULL
)
```

## Arguments

- scen:

  a solved scenario.

- width:

  integer vector of window widths in HOURS, strictly increasing. The
  default splits at half a day, a day, a week and 30 days, producing
  five bands. Widths at or above the length of the series collapse to
  the series minimum, which is the honest answer rather than an error.

- labels:

  optional character vector naming the bands, from shortest to longest.
  Must be `length(width) + 1`. Defaults to labels derived from `width`,
  e.g. `"<12h"`, `"12h-1d"`, `"1d-1w"`, `"1w-30d"`, `">30d"`.

- cyclic:

  how to pad the ends of the series before the rolling windows. `NA`
  (default) takes it from each storage's `@fullYear`: a store whose
  cycle closes over the year genuinely continues into itself, so the
  head and tail are wrapped. `FALSE` repeats the edge values instead.
  `TRUE`/`FALSE` force one choice for every storage.

- tmz:

  time zone used to build the datetime column. `"UTC"` by default; the
  original IDEEA script hard-coded `"Asia/Kolkata"`, which silently
  moved every model's clock.

- stg:

  optional character vector restricting the storages considered.

## Value

A data frame with one row per storage, commodity, region, year,
timeslice and band: columns `stg`, `comm`, `region`, `year`,
`timeslice`, `datetime`, `duration` (an ordered factor, shortest first)
and `value`. Zero rows if the scenario has no storage level to
decompose.

## Reading the result

A store cycling within its parent timeframe (`@fullYear = FALSE`) cannot
hold energy for longer than that cycle, so its long-duration bands come
out at or near zero. That is a property of the model, not a failure of
the split.

## Examples

``` r
if (FALSE) { # \dontrun{
d <- storage_duration(scen)
library(ggplot2)
ggplot(d, aes(datetime, value, fill = duration)) +
  geom_area() +
  facet_wrap(~region)
} # }
```
