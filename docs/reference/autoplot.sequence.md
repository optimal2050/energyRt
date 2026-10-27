# Draw what a solve sequence covered

`autoplot()` over
[`sequence_coverage()`](https://energyRt.org/reference/sequence_coverage.md):
the units of a sequence drawn on the scale they were drawn from, so
tiling, overlap and gaps are shapes to be seen rather than properties to
be inferred from a table of runs.

## Usage

``` r
# S3 method for class 'by_sample'
autoplot(
  object,
  type = c("coverage", "wall", "histogram"),
  year = NULL,
  var = "objective",
  by = NULL,
  bins = NULL,
  ...
)

# S3 method for class 'by_region'
autoplot(object, type = c("coverage"), geoscale = NULL, level = NULL, ...)

# S3 method for class 'recursive'
autoplot(object, type = c("coverage"), ...)

# S3 method for class 'guided'
autoplot(object, type = c("coverage"), ...)
```

## Arguments

- object:

  a `by_sample`, `by_region`, `recursive` or `guided` sequence.

- type:

  which drawing to make; see Details.

- year:

  the model year whose weekdays `type = "wall"` is arranged by. Without
  one the days are laid out in plain sequence.

- var:

  for `type = "histogram"`, what to draw the spread of: `"objective"`,
  or the name of a solved variable.

- by:

  for `type = "histogram"`, the columns to group a variable on, one
  histogram each. Defaults to every key column, since any column left
  out would have to be aggregated away.

- bins:

  for `type = "histogram"`, the number of bins. Defaults to the square
  root of the number of samples.

- ...:

  for `type = "histogram"`, passed to
  [`getData()`](https://energyRt.org/reference/getData.md) to narrow
  what is drawn (`year = `, `region = `, ...); otherwise unused.

- geoscale:

  a
  [`geoscales::GeoScale`](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html)
  to draw on, or `NULL` for the sequence model's own.

- level:

  the geoframe to draw. Defaults to the finest frame that contains every
  covered region.

## Value

A `ggplot` object.

## Details

One method per driver, because each samples a different scale.

- `by_sample` draws the calendar. `type = "coverage"` is a tile grid
  over the calendar's two finest timeframes and works for any calendar;
  `type = "wall"` is the month-faceted wall calendar, which needs a
  day-resolution timeframe (`YDAY` or `MDAY`) and rolls each unit up to
  whole days. `type = "histogram"` leaves coverage behind and draws the
  spread of a *result* across the samples: bars weighted by each
  sample's `year_fraction`, a dashed rule at the weighted mean
  [`sample_summary()`](https://energyRt.org/reference/sample_summary.md)
  reports, and the free pass overlaid on the floored one when
  `ncap_floor > 0`. There is no histogram for `by_region`, whose units
  are shards of one system rather than replicates of it.

- `by_region` draws the geoscale as a choropleth, falling back to a tile
  grid when the geoscale carries no geometry or `sf` is not installed.

- `recursive` and `guided` draw the horizon as a Gantt chart, one row
  per step. For `recursive` the look-ahead milestones are filled apart
  from the decided ones: the overlap between steps *is* the look-ahead,
  and seeing it as such is the point.

Elements no unit covered keep the grey `NA` fill, and an element two
units claimed is filled as `"(shared)"` rather than resolved silently.
The subtitle counts both, plus the within-unit multiplicity a bootstrap
draw carries.

## See also

[`sequence_coverage()`](https://energyRt.org/reference/sequence_coverage.md),
[`solve_by_sample()`](https://energyRt.org/reference/solve_by_sample.md),
[`solve_by_region()`](https://energyRt.org/reference/solve_by_region.md),
[`solve_recursive()`](https://energyRt.org/reference/solve_recursive.md)

## Examples

``` r
if (FALSE) { # \dontrun{
bs <- solve_by_sample(mod, samples = 12)
autoplot(bs)                        # which timeslices each sample drew
autoplot(bs, "wall", year = 2020)   # the same, as a wall calendar
autoplot(bs, "histogram")           # the objective's spread
autoplot(bs, "histogram", var = "vTechCap", by = "tech", year = 2050)
autoplot(solve_by_region(mod, group = list(c("R1", "R2"), "R3")))
autoplot(solve_recursive(mod, overlap = 1))
} # }
```
