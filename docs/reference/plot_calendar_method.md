# Visualize a Calendar object

Draws the calendar's time-structure as stacked rows (one per timeframe,
`ANNUAL` on top), where each rectangle is a time-slice sized by its
share of the year. `autoplot()` is the ggplot2-idiomatic entry point and
returns the same `ggplot` object as
[`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Usage

``` r
# S4 method for class 'calendar'
plot(
  x,
  ...,
  fill = c("order", "share", "weight"),
  color_pattern = c("within", "global"),
  palette = "D",
  labels = TRUE,
  label_by = c("name", "slice", "none"),
  label_color = "auto",
  max_labels = 60L,
  border = NA,
  show_leafs = NULL,
  reference = NULL
)

# S3 method for class 'calendar'
autoplot(
  object,
  ...,
  fill = c("order", "share", "weight"),
  color_pattern = c("within", "global"),
  palette = "D",
  labels = TRUE,
  label_by = c("name", "slice", "none"),
  label_color = "auto",
  max_labels = 60L,
  border = NA,
  show_leafs = NULL,
  reference = NULL
)
```

## Arguments

- x, object:

  An object of class `calendar`.

- ...:

  Passed to `plot_calendar()`.

- fill:

  One of `"order"` (chronology, default), `"share"` (year-share of the
  slice), or `"weight"` — the metric mapped to rectangle fill color.

- color_pattern:

  For `fill = "order"`, how the color gradient is applied: `"within"`
  (default) colors each level over its own slices — a full `h00`→`h23`
  gradient recycled every day, `d001`→`d365` over the year — so each row
  shows its cyclical structure; `"global"` colors by absolute chronology
  (leaf order `1…n`, e.g. `0…8760`). Ignored for `"share"`/`"weight"`.

- palette:

  Viridis color option (e.g. `"D"`, `"C"`, `"magma"`) for the fill
  scale.

- labels:

  Logical; draw labels inside the rectangles (master on/off).

- label_by:

  What to label each rectangle with: `"name"` (default) the individual
  level name (e.g. `HOUR` cells become `h00`…`h23`, `YDAY` cells
  `d001`…`d365`), `"slice"` the full slice path (e.g. `d001_h00`), or
  `"none"`.

- label_color:

  Text color for the labels. `"auto"` (default) contrasts each label
  with its cell — white on the darker part of the gradient, dark on the
  lighter part — so labels stay readable on dark fills. Pass any single
  color (e.g. `"black"`, `"white"`) to use it for all labels.

- max_labels:

  Integer; timeframes with more slices than this are left unlabeled to
  avoid clutter.

- border:

  Rectangle outline color. `NA` (default) draws no outline, so a
  high-resolution row (e.g. 8760 hourly slices) reads as a smooth
  gradient instead of a solid block of borders. Pass e.g. `"grey30"` to
  outline slices on coarse calendars.

- show_leafs:

  Select which slices to draw (`NULL`, default, shows all). Two forms:

  - an unnamed vector filtering the finest (leaf) level — leaf slice
    names (e.g. `"d001_h05"`) or integer leaf indices (e.g. `1:100` for
    the first 100 leaves);

  - a named list filtering per timeframe level, combined with AND, e.g.
    `list(YDAY = "d100", HOUR = 5:10)` — for each level a character
    vector of that level's slice names or integer positions among its
    slices (`HOUR = 5:10` selects the 5th–10th hours). The kept slices
    are packed left-to-right and the x-axis spans their total
    year-share. Colors stay stable (e.g. `h05` keeps its color whether
    or not other hours are shown).

- reference:

  Optional full `calendar`. When supplied, `x` is treated as a *subset*
  of `reference`: the plot lays out `reference`'s full structure but
  fills only the slices present in `x` (matched by slice name), leaving
  the unselected slices empty. Use it to see which part of a full
  calendar a sampled/subset calendar covers.

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
cal <- newCalendar(make_timetable(timeslices3), name = "m12h24")
plot(cal)
autoplot(cal, fill = "share")

# Subset view: show which slices a reduced calendar covers within the full one
autoplot(calendars$d365_h24_subset_1day_per_month,
         reference = calendars$d365_h24)

# Zoom into specific slices: day 100, hours 5-10
autoplot(calendars$d365_h24, show_leafs = list(YDAY = "d100", HOUR = 5:10))
} # }
```
