# Validate interpolated scenario parameters

Runs a set of post-interpolation consistency checks over the numeric /
bounds / map parameters of a scenario and reports any issues. Checks:

- **NA index columns**: no NA in a parameter's `dimSets` id columns.
  When `fold = TRUE`, NA is permitted only in the trimmable dimensions
  (region / timeslice / vintage, which fold encodes as wildcards); when
  `fold = FALSE`, no NA is permitted in any id column.

- **Schema**: data columns match the declared `dimSets` (plus `value`,
  and `type` for bounds).

- **Duplicate keys**: no duplicate id tuples.

- **Map vs parameter (correctness)**: every tuple of a value map is
  covered by its source parameter; a missing value would otherwise be
  silently replaced by the solver default.

- **Parameter vs map (efficiency)**: value parameter rows lie within the
  union of their maps (no orphan / out-of-domain rows). After trimming
  this must hold exactly.

- **Free meals**: every object with flow variables has its governing
  constraints. A storage without its balance chain, or a trade corridor
  without its flow-capacity link, would supply the balance unbounded and
  solve to OPTIMAL with a meaningless objective – structural. Missing
  charge/discharge or availability bounds are advisory, since an open
  (`Inf`) side deliberately produces no equation.

- **Calendar chronology**: the timeslice successor chain (the storage
  balance and ramping order) is derived from the calendar's timetable
  row order; a row order that contradicts the timetable's own timeframe
  columns is structural.

## Usage

``` r
validate_scenario_parameters(
  scen,
  fold = TRUE,
  action = c("warn", "stop", "silent")
)
```

## Arguments

- scen:

  an interpolated scenario.

- fold:

  logical or character; whether (or in which dimensions) the scenario is
  folded, so NA wildcards are allowed there.

- action:

  one of `"warn"` (default), `"stop"`, `"silent"`: how to report
  advisory issues.

## Value

(invisibly) a data frame of findings with columns `parameter`, `check`,
`detail`, `severity`.

## Details

Structural findings always stop; `action` governs the advisory ones.
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
runs this automatically (`validate = TRUE`, action `"warn"`); call it
directly to re-check a scenario or to collect the findings table.
