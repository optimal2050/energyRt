# Import a run's solution and make it stick

Reads the solution of one run into `modOut` and saves it, so the
scenario keeps it after the session ends. This is the step between a
finished solve and a scenario that still has its results tomorrow:
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md)
never saves, and
[`read_solution()`](https://energyRt.org/reference/read.md) alone leaves
the solution in memory.

## Usage

``` r
import_solution(
  object,
  run,
  ...,
  restore_solver = TRUE,
  save = TRUE,
  promote = FALSE,
  cleanup = FALSE,
  verbose = TRUE
)
```

## Arguments

- object:

  a scenario object.

- run:

  character, the run to import (`"<solve>"` or `"<variant>/<solve>"`).

- ...:

  passed to [`read_solution()`](https://energyRt.org/reference/read.md).

- restore_solver:

  logical; take the run's solver identity (name, lang, solver, method,
  formats) from its `solver.csv`. Never the command line.

- save:

  logical; write the result with
  [`save_scenario()`](https://energyRt.org/reference/save_scenario.md).
  `FALSE` returns the scenario with the solution in memory only — which
  is the situation this function exists to end, so it is on by default.

- promote:

  logical; after importing, make this the scenario's own solution with
  [`promote_solution()`](https://energyRt.org/reference/promote_solution.md)
  — copied to `<scenario>/modOut/` with its provenance, after which the
  run tree is scratch. Off by default, since which attempt is final is a
  choice.

- cleanup:

  logical; remove the run's regenerable solver files afterwards. Off by
  default.

- verbose:

  logical.

## Value

the scenario, invisibly, with `modOut` populated and `run` active.

## Details

Pick the run with
[`scenario_solutions()`](https://energyRt.org/reference/scenario_solutions.md),
which shows each attempt's status, objective, solver and backend.
Nothing is chosen for you.

The solver's `output/` is left alone. Importing makes it *removable* —
it is no longer the only copy — but removing it is
[`drop_solver_outputs()`](https://energyRt.org/reference/drop_solver_outputs.md)'s
job, and `cleanup = FALSE` here means this function never does it.

**Why this is a verb rather than two calls.** When
[`read_solution()`](https://energyRt.org/reference/read.md) cannot read
`output/variable_list.csv` it returns the scenario *unchanged*,
discarding the run switch. Saving that would write an empty `modOut/`,
which flips the run to "imported" and would let the solution's only copy
be cleaned up. This refuses to save a read that produced no variables.

## See also

[`scenario_solutions()`](https://energyRt.org/reference/scenario_solutions.md),
[`read_solution()`](https://energyRt.org/reference/read.md),
[`drop_solver_outputs()`](https://energyRt.org/reference/drop_solver_outputs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
open_project("~/projects/mymodel")
scen <- getScenario("BASE")

# read a solve that ran elsewhere (a cluster or cloud run) back in
scenario_runs(scen)
scen <- import_solution(scen, run = "glpk")

# `promote = TRUE` makes it the scenario's active solution
scen <- import_solution(scen, run = "highs", promote = TRUE)
} # }
```
