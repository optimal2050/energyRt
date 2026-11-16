# Set or get a model's calendar, for every year or for particular years

`setCalendar()` puts a
[calendar](https://energyRt.org/reference/class-calendar.md) on a
`config`, `model` or `scenario`; `getCalendar()` reads it back. With
`years`, the calendar is given to those milestone years alone, and the
model keeps its own for the rest.

## Usage

``` r
# S4 method for class 'config'
setCalendar(obj, calendar = NULL, years = NULL, ...)

# S4 method for class 'model'
setCalendar(obj, calendar = NULL, years = NULL, ...)

# S4 method for class 'scenario'
setCalendar(obj, calendar = NULL, years = NULL, ...)

# S4 method for class 'config'
getCalendar(obj)

# S4 method for class 'model'
getCalendar(obj)

# S4 method for class 'scenario'
getCalendar(obj)
```

## Arguments

- obj:

  a `config`, `settings`, `model` or `scenario`.

- calendar:

  a `calendar` object, or `NULL` to build one from `...`.

- years:

  milestone years this calendar applies to, or `NULL` (default) to make
  it the model's calendar for every year.

- ...:

  passed to
  [`newCalendar()`](https://energyRt.org/reference/newCalendar.md) when
  `calendar` is not given.

## Value

`setCalendar()` returns the modified object; `getCalendar()` returns a
`calendar`.

## Details

Pass a calendar object — an element of
[calendars](https://energyRt.org/reference/calendars.md), or one from
[`newCalendar()`](https://energyRt.org/reference/newCalendar.md) or
[`calendar_samples()`](https://energyRt.org/reference/calendar_samples.md).
Leaving `calendar` out and giving
[`newCalendar()`](https://energyRt.org/reference/newCalendar.md)'s
arguments instead builds one on the spot, which is all this accepted up
to version 0.92.

With `years`, the calendar is registered in `config@misc$calendars` and
named on those rows of `horizon@intervals`, so a milestone can be solved
on a **sample** of the model calendar: hourly where the investment
decisions land, cheaper in the years that only carry the trajectory
forward.

Such a calendar must be a sample rather than a different calendar — the
same timeframe levels, slices drawn from the model calendar's own
universe, the same `share` for a slice the two share. What may differ is
which slices a year has and how many times each stands for (its
`weight`). The check runs here, where the call that broke it can be
named: dropping a level instead of sampling its members re-derives the
leaf timeslice names and the year would then solve to zero in silence.

## See also

[`getYearCalendars()`](https://energyRt.org/reference/getYearCalendars.md),
[`calendar_samples()`](https://energyRt.org/reference/calendar_samples.md),
[calendars](https://energyRt.org/reference/calendars.md)

## Examples

``` r
# milestones 2030, 2035 and 2040: `period` is a span, so it is given in
# full and the interval widths carve it up
mod <- newModel("demo", calendar = calendars$d365_h24,
                horizon = newHorizon(2030:2040, intervals = c(1, 5, 5),
                                     mid_is_end = TRUE))
getCalendar(mod)@name
#> [1] "d365_h24"

# 2035 on one day per month, the other two years hourly
mod <- setCalendar(mod, calendars$d365_h24_1dpm, years = 2035)
names(getYearCalendars(mod))
#> [1] "2030" "2035" "2040"
mod@config@horizon@intervals$calendar
#> [1] NA              "d365_h24_1dpm" NA             
```
