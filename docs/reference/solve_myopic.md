# Solve a model sequentially (myopic foresight)

Solves the model's horizon window by window instead of all at once. Each
window sees only its own milestone years (plus optional look-ahead);
capacity built in earlier windows is carried forward as exogenous —
sunk, investment-cost-free — `stock`, aged by each process's lifetime
and net of its recorded retirements, and cumulative supply reserves are
debited by past use (see
[`solution_ledger()`](https://energyRt.org/reference/solution_ledger.md)
/ [`apply_ledger()`](https://energyRt.org/reference/apply_ledger.md)).

By default (`store = "variants"`) the whole sequence is ONE scenario:
every step is an own-problem variant (`runs/s01-2020/…`), so the steps
share a folder, appear together in
[`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md),
and are switchable with
[`read_solution()`](https://energyRt.org/reference/read.md).
`store = "scenarios"` writes one scenario folder per step instead.

Because carried capacity pays no EAC, per-step objective values are not
comparable to a perfect-foresight objective; the capacity and activity
PATHS are the meaningful comparison, and
[`myopic_objective()`](https://energyRt.org/reference/myopic_objective.md)
recomputes a globally discounted total cost over the decided years.

## Usage

``` r
solve_myopic(
  object,
  name = NULL,
  ...,
  horizon = NULL,
  step = 1L,
  overlap = 0L,
  carry = c("capacity", "budgets"),
  solver = NULL,
  tolerance = 1e-06,
  store = c("variants", "scenarios"),
  overwrite = FALSE,
  keep_scenarios = TRUE,
  on_error = c("stop", "return"),
  verbose = TRUE
)
```

## Arguments

- object:

  a model.

- name:

  character, sequence name (default `"<model>_myopic"`); names the
  scenario (or the per-step scenario prefix).

- ...:

  forwarded to
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
  for every step (settings / calendar / config overrides).

- horizon:

  the full horizon to split (default: the model's).

- step, overlap:

  window shape, see
  [`horizon_windows()`](https://energyRt.org/reference/horizon_windows.md).

- carry:

  what to carry between steps, see
  [`apply_ledger()`](https://energyRt.org/reference/apply_ledger.md).

- solver:

  solver specification (as in
  [`solve_scenario()`](https://energyRt.org/reference/solve_model.md)).

- tolerance:

  carried values below it are treated as zero.

- store:

  `"variants"` (one scenario, steps as own-problem variants) or
  `"scenarios"` (one scenario folder per step).

- overwrite:

  logical; `FALSE` (default) refuses to run when a variant of the same
  label already exists on disk with a different type or different
  parameters. `TRUE` skips that guard and writes into it.

- keep_scenarios:

  logical, keep each step's solved scenario object in the result (needed
  by [`getData()`](https://energyRt.org/reference/getData.md) on the
  result).

- on_error:

  `"stop"` (default) or `"return"` (return the partial result with the
  failed step recorded).

- verbose:

  logical.

## Value

an object of class `myopic`: `steps` (one row per step: step, run id,
decided years, status, objective), `scenarios` (the solved step objects
when kept), `ledger` (the accumulated
[`solution_ledger()`](https://energyRt.org/reference/solution_ledger.md)),
plus the call parameters.
[`getData()`](https://energyRt.org/reference/getData.md) on it stitches
per-step results (each year contributed by the step that decided it).

## See also

[`horizon_windows()`](https://energyRt.org/reference/horizon_windows.md),
[`solution_ledger()`](https://energyRt.org/reference/solution_ledger.md),
[`apply_ledger()`](https://energyRt.org/reference/apply_ledger.md),
[`myopic_objective()`](https://energyRt.org/reference/myopic_objective.md)

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

# one solve per milestone year, each blind to the next, with capacity
# carried forward
res <- solve_myopic(mod, name = "MYOPIC", step = 1L,
                    solver = solver_options$glpk)
myopic_objective(res, sdr = 0.05)
} # }
```
