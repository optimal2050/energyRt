# Capacity targets from a solved scenario

Harvests standing capacity per process, region and milestone year from a
solved scenario, for use as a guide on a later solve of the same model.
The target counterpart of
[`solution_ledger()`](https://energyRt.org/reference/solution_ledger.md):
where a ledger records decided capacity to be carried forward as sunk
`stock`, a target records indicative capacity to be imposed as a
`cap.lo` bound by
[`apply_targets()`](https://energyRt.org/reference/apply_targets.md).

## Usage

``` r
solution_targets(object, years = NULL, classes = NULL)
```

## Arguments

- object:

  a solved scenario.

- years:

  integer, milestone years to harvest; `NULL` (default) takes every year
  present in the solution.

- classes:

  character, families to harvest; defaults to all (technology, the three
  storage parts, trade).

## Value

a `capacity_targets` tibble: `family`, `proc`, `region`, `year`,
`value`, carrying the scenario's variant `provenance` as an attribute.

## See also

[`apply_targets()`](https://energyRt.org/reference/apply_targets.md),
[`solve_guided()`](https://energyRt.org/reference/solve_guided.md)

## Examples

``` r
if (FALSE) { # \dontrun{
open_project(tempdir())

ek   <- topia$modules$electricity$R3
mod  <- newModel("TOPIA", data = ek$repo,
                 calendar = topia$modules$calendars$s4_h24,
                 region = ek$regions,
                 horizon = topia$modules$horizons$base,
                 discount = 0.05)
mod <- setGeoscale(mod, ek$geoscale)
scen <- solve_model(mod, name = "BASE", solver = solver_options$glpk)

# the capacity the solved run decided, per process, region and year
tg <- solution_targets(scen)
tg

# technologies only
solution_targets(scen, classes = "technology")
} # }
```
