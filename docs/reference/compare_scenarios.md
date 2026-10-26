# Compare solved scenarios (or runs of one scenario)

Builds a results comparison across a named list of solved scenarios, or
across recorded runs of a single scenario (see
[`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md)).
One element is the **base** (default: the first); every other element is
compared against it: objectives and run metadata in an overview table,
and selected solved variables value-by-value through a tolerance-aware
full join at an annual aggregation level.

## Usage

``` r
compare_scenarios(
  object,
  runs = NULL,
  name = NULL,
  base = 1L,
  level = c("annual_region", "annual_total"),
  tol_rel = 1e-06,
  tol_abs = 1e-06,
  inputs = FALSE,
  top_n = 10L
)

# S3 method for class 'scenarios_cmp'
print(x, top = 10L, ...)

# S3 method for class 'scenarios_cmp'
autoplot(
  object,
  type = c("objective", "generation", "capacity", "new_capacity", "fuel", "emissions",
    "cost", "delta"),
  comm = "ELC",
  region = NULL,
  year = NULL,
  position = c("facet", "dodge"),
  variable = NULL,
  top_n = 12,
  ...
)

# S4 method for class 'scenarios_cmp'
plot(x, y, ...)
```

## Arguments

- object:

  a `scenarios_cmp` object.

- runs:

  character, run ids of `scen` to compare (as listed by
  [`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md));
  each is read into a local copy via
  [`read_solution()`](https://energyRt.org/reference/read.md) – the
  caller's scenario keeps its active run.

- name:

  character, variables to compare (overrides the role-driven default).

- base:

  integer position or element name of the baseline (default: the first
  element).

- level:

  aggregation level for value comparisons: `"annual_region"` (sum over
  timeslices) or `"annual_total"` (also over regions; variables with
  role `"interregional"` are skipped there).

- tol_rel, tol_abs:

  numeric comparison tolerances (a value differs when
  `abs(diff) > tol_abs + tol_rel * max(abs(values))`).

- inputs:

  logical, also diff the model inputs of every scenario against the base
  via
  [`compare_inputs()`](https://energyRt.org/reference/compare_inputs.md).

- top_n:

  integer, passed to
  [`getMix()`](https://energyRt.org/reference/getMix.md) for the mix
  charts: keep the `top_n` largest processes per scenario and lump the
  rest into `"Other"`. `NULL`/`Inf` keeps all.

- x:

  a `scenarios_cmp` object (plot delegates to `autoplot`).

- top:

  integer, number of top differences to print.

- ...:

  ignored.

- type:

  chart type: `"objective"` (bars per scenario), one of the
  [`getMix()`](https://energyRt.org/reference/getMix.md) types
  (`"generation"`, `"capacity"`, `"new_capacity"`, `"fuel"`),
  `"emissions"`, `"cost"` (role-driven cost breakdown per scenario), or
  `"delta"` (largest value differences).

- comm, region, year:

  filters forwarded to
  [`getMix()`](https://energyRt.org/reference/getMix.md).

- position:

  `"facet"` (stacked mixes, one panel per scenario) or `"dodge"`
  (scenarios side by side, aggregated over process).

- variable:

  for `type = "delta"`: restrict to one variable.

- y:

  ignored.

## Value

A `scenarios_cmp` object (a list) with components: `overview` (one row
per scenario: run/solver/status/objective and objective differences vs
base), `values` (per variable x scenario: `n_diff`, `max_abs_diff`,
`max_rel_diff`, status), `deltas` (named list per variable of row-level
differences), `top` (the `top_n` largest absolute differences), `inputs`
(list of
[`compare_inputs()`](https://energyRt.org/reference/compare_inputs.md)
results when `inputs = TRUE`), and the carried `scen` list / `base` /
`level` / `tol`. The object holds the full scenario list so `autoplot()`
and [`report()`](https://energyRt.org/reference/report.md) can rebuild
mixes from it. [`print()`](https://energyRt.org/reference/print.md)
gives a compact report; `autoplot()` draws comparison charts;
[`report()`](https://energyRt.org/reference/report.md) renders the
comparative report.

## Details

Variables default to the role-driven selection the scenario report uses:
every solved variable the catalogue declares as a cost (minus
`vObjective` / `vTotalCost`) plus the key quantity variables
(`vTechCap`, `vTechNewCap`, `vTechOut`, `vEmsFuelTot`) that are present.

## See also

[`compare_inputs()`](https://energyRt.org/reference/compare_inputs.md),
[`compare_models()`](https://energyRt.org/reference/compare_models.md),
[`getMix()`](https://energyRt.org/reference/getMix.md) (which also
accepts named scenario lists),
[`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md).

Other comparison:
[`compare_inputs()`](https://energyRt.org/reference/compare_inputs.md),
[`compare_models()`](https://energyRt.org/reference/compare_models.md)

## Examples

``` r
if (FALSE) { # \dontrun{
open_project(tempdir())

ek  <- topia$modules$electricity$R3
mod <- newModel("TOPIA", data = ek$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = ek$regions,
                horizon = topia$modules$horizons$base,
                discount = 0.05)
mod <- setGeoscale(mod, ek$geoscale)

base <- solve_model(mod, name = "BASE", solver = solver_options$glpk)
cap  <- solve_model(add(mod, ek$CO2_CAP), name = "CAP",
                    solver = solver_options$glpk)

# a NAMED list, the first entry as the baseline
cmp <- compare_scenarios(list(BASE = base, CAP = cap))
cmp
ggplot2::autoplot(cmp, type = "generation", comm = "ELC")

# or several runs of ONE scenario folder, by run id
compare_scenarios(base, runs = scenario_runs(base)$run)
} # }
```
