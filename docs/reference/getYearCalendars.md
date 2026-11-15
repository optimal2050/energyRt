# Calendars by milestone year

The calendar each milestone is solved on. Years that name one of their
own get it; the rest get the model calendar.

## Usage

``` r
getYearCalendars(x)
```

## Arguments

- x:

  a `model`, `scenario` or `config`.

## Value

A named list of `calendar` objects, named by milestone year, or `NULL`
when no milestone names a calendar of its own – which is the
one-calendar-for-every-year case.

## See also

[`setCalendar()`](https://energyRt.org/reference/setCalendar.md),
[`calendar_samples()`](https://energyRt.org/reference/calendar_samples.md)

## Examples

``` r
mod <- newModel("m", calendar = calendars$d365_h24,
                horizon = newHorizon(2030, intervals = 1))
getYearCalendars(mod)   # NULL: one calendar for every year
#> NULL
```
