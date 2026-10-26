# Make a run's solution the scenario's own

Copies an imported run's solution to `<scenario>/modOut/`, beside
`modInp/`, and clears the active run so the scenario reads from there.
After this the `runs/` tree is scratch: it can be dropped without losing
the solution.

## Usage

``` r
promote_solution(object, run = NULL, save = TRUE, verbose = TRUE)
```

## Arguments

- object:

  a scenario object.

- run:

  character, the run to promote (`"<solve>"`). `NULL` (default) takes
  the scenario's active run.

- save:

  logical; write the scenario shell afterwards, so a reload resolves to
  the promoted store. On by default — without it the store exists and
  nothing points at it.

- verbose:

  logical.

## Value

the scenario, invisibly, reading from `<scenario>/modOut/`.

## Details

Until a solution is promoted it lives only inside the run that produced
it, and deleting that run leaves the scenario pointing at a store that
is no longer there — every read then fails. Promotion is what makes the
run tree disposable.

The solve's provenance travels with the store in `modOut.yml` — solver,
objective, stage and timings — because `run.yml` is its only other home
and goes with the run folder. The manifest is written last, so its
presence also marks the store as complete.

Runs are untouched: promoting copies, it does not move. Use
[`drop_scenario_run()`](https://energyRt.org/reference/scenario_runs.md)
or
[`drop_solver_outputs()`](https://energyRt.org/reference/drop_solver_outputs.md)
afterwards to reclaim the space, and
[`scenario_solutions()`](https://energyRt.org/reference/scenario_solutions.md)
to choose which run to promote.

## Variants cannot be promoted

An own-problem variant carries its own `modInp`, settings and manifest
inside `runs/<variant>/`. Its results belong to a different problem from
the scenario's, so lifting only the solution would pair variant results
with base-problem parameters. Promote a base-problem run, or keep the
variant's run folder.

## See also

[`import_solution()`](https://energyRt.org/reference/import_solution.md),
[`scenario_solutions()`](https://energyRt.org/reference/scenario_solutions.md),
[`drop_scenario_run()`](https://energyRt.org/reference/scenario_runs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
scen <- import_solution(scen, "glpk", promote = TRUE)
drop_scenario_run(scen, "glpk", force = TRUE)   # the run is now scratch
} # }
```
