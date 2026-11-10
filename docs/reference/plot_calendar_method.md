# Visualize a Calendar object

Draws the calendar's time structure as an icicle: one row per timeframe,
`ANNUAL` on top, each rectangle a timeslice sized by its share of the
year. The figure is
[`timescales::gg_calendar()`](https://optimal2050.github.io/timescales/r/reference/gg_calendar.html)
on the calendar's timetable; `autoplot()` returns it,
[`plot()`](https://energyRt.org/reference/draw.md) draws it.

## Usage

``` r
# S3 method for class 'calendar'
autoplot(object, ..., reference = NULL)
```

## Arguments

- object:

  An object of class `calendar`.

- ...:

  Passed to
  [`timescales::gg_calendar()`](https://optimal2050.github.io/timescales/r/reference/gg_calendar.html)
  (`type = "icicle"`): `fill`, `color_pattern`, `palette`, `labels`,
  `max_labels`, `max_segments`, `border`.

- reference:

  Optional full `calendar`. When supplied, `object` is drawn as a subset
  of it: the layout is the reference's, and the timeslices absent from
  `object` are left empty. Use it to see which part of a full calendar a
  sampled calendar covers.

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
cal <- newCalendar(make_timetable(timeslices3), name = "m12h24")
plot(cal)
autoplot(cal, fill = "share")

# which timeslices a reduced calendar covers within the full one
autoplot(calendars$d365_h24_1dpm, reference = calendars$d365_h24)
} # }
```
