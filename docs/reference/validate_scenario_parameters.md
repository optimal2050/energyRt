# Validate interpolated scenario parameters

Runs a set of post-interpolation consistency checks over the numeric /
bounds / map parameters of a scenario and reports any issues. Checks:

- **NA index columns**: no NA in a parameter's `dimSets` id columns.
  When `fold = TRUE`, NA is permitted only in the trimmable dimensions
  (region / slice / vintage, which fold encodes as wildcards); when
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

  scenario.

- fold:

  logical; whether the scenario is folded (NA wildcards allowed in
  trimmable dimensions).

- action:

  one of `"warn"` (default), `"stop"`, `"silent"`: how to report issues.

## Value

(invisibly) a data frame of issues with columns `parameter`, `check`,
`detail`.
