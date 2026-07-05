# Heatmap of timeslice-indexed values over a calendar

Lays timeslice-indexed values on a 2-D grid whose axes follow the
calendar structure: the finest timeframe becomes the y-axis, the
next-finest the x-axis, and any coarser level(s) become facets — e.g.
for a `d365_h24` calendar, `x = day-of-year`, `y = hour-of-day`. Useful
for load curves, renewable profiles, prices, and other sub-annual
series.

## Usage

``` r
plot_heatmap(
  x,
  calendar = NULL,
  value = NULL,
  facet = NULL,
  palette = "D",
  name = NULL
)
```

## Arguments

- x:

  A `data.frame` with a `slice` column and a numeric value column, a
  named numeric vector (names are slices), or a `calendar` object (then
  the slice `share` — or `value` column — is shown).

- calendar:

  A `calendar` object giving the layout (matched to `x` by slice), or a
  format string (e.g. `"d365_h24"`). If `NULL`, the format is guessed
  from the slice names with
  [`tsl_guess_format()`](https://energyRt.org/reference/tsl_guess_format.md).

- value:

  Name of the value column in `x` (defaults to the single numeric
  column, or `share` for a calendar).

- facet:

  Optional timeframe level(s) to facet by. `"month"` over a day-of-year
  format splits the year into monthly panels (x = day-of-month).

- palette:

  Viridis color option for the fill scale.

- name:

  Legend title (defaults to the value name).

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
data("calendars", package = "energyRt")
cal <- calendars$d365_h24
prof <- data.frame(slice = cal@timetable$slice,
                   load  = runif(nrow(cal@timetable)))
plot_heatmap(prof, calendar = cal, value = "load")
plot_heatmap(prof, calendar = cal, value = "load", facet = "month")
} # }
```
