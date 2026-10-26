# What solutions does this scenario have, and which is worth importing?

One row per run, joining what the run record holds with what can be read
back from a run's raw `output/` without importing it. Read-only: it
writes nothing, not even the objectives it recovers.

## Usage

``` r
scenario_solutions(object, peek = TRUE)
```

## Arguments

- object:

  a scenario object.

- peek:

  logical; `FALSE` skips reading `output/` and leaves `solver_status` /
  peeked objectives as `NA`. Faster on a scenario with many large runs.

## Value

a tibble: `run`, `variant`, `solve`, `status`, `solver_status`,
`objective`, `objective_src`, `solver_name`, `lang`, `solver`,
`duration_sec`, `imported`, `has_output`, `output_mb`, `active`.

## Details

`run.yml`'s `objective` is computed from the in-memory solution when the
solve finishes, so it is `NA` for any run whose solution was never
imported — which is exactly the set you need to compare after several
attempts. This recovers it per run from the single `vObjective` file in
`output/`, and `objective_src` says which reading you are looking at:
`"record"` from `run.yml`, `"output"` peeked from the dump, `NA` when
neither is available.

`solver_status` likewise comes from the run's `output/log.csv` and is
the solver's own verdict, independent of `status`, which is what the
driver recorded. The two disagree exactly when a solve finished but was
never read back.

Nothing here ranks the runs or picks one. Objectives are comparable only
within one problem: a `"<variant>/<solve>"` run solves a *different*
problem (its own `modInp`), so compare within a `variant`, not across.

## See also

[`import_solution()`](https://energyRt.org/reference/import_solution.md),
[`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md),
[`scenario_artifacts()`](https://energyRt.org/reference/scenario_artifacts.md)

## Examples

``` r
if (FALSE) { # \dontrun{
scenario_solutions(scen)                      # what is there
import_solution(scen, "julia_highs_barrier")  # make one of them stick
} # }
```
