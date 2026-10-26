# Delete the regenerable part of a solve

Removes the generated model source, the exchange `input/`, solver logs
and — only when the solution has been imported — the solver's raw
`output/`. The run record and the imported solution are kept, so the run
stays listed and readable. **Dry-run by default.**

## Usage

``` r
drop_solver_outputs(object, runs = NULL, dry_run = TRUE, verbose = TRUE)
```

## Arguments

- object:

  a scenario object.

- runs:

  character, run identifiers (`"<solve>"` or `"<variant>/<solve>"`) or
  `NULL` for every run
  [`scenario_artifacts()`](https://energyRt.org/reference/scenario_artifacts.md)
  suggests.

- dry_run:

  logical. `TRUE` (default) lists what would go without removing it.

- verbose:

  logical.

## Value

a tibble of candidates with an `action` column (`would_delete`,
`deleted`, `kept_not_imported`, `nothing_to_do`, `skipped_sealed`) and
`freed_mb`. Returned visibly on a dry run, invisibly otherwise.

## Details

This is what
[`drop_scenario_run()`](https://energyRt.org/reference/scenario_runs.md)
cannot do: that removes the whole run.

A run whose solution was never imported keeps its `output/` whatever is
asked, because that directory is the only copy — see
[`scenario_artifacts()`](https://energyRt.org/reference/scenario_artifacts.md).

Only runs are touched. Rendered reports and cached levcost results are
scenario-level, not part of a run;
[`clear_report_cache()`](https://energyRt.org/reference/clear_report_cache.md)
and
[`clear_levcost_cache()`](https://energyRt.org/reference/clear_levcost_cache.md)
remove those.

## See also

[`scenario_artifacts()`](https://energyRt.org/reference/scenario_artifacts.md),
[`drop_scenario_run()`](https://energyRt.org/reference/scenario_runs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
open_project("~/projects/mymodel")
scen <- getScenario("BASE")

# the solver's scratch files are the bulk of a scenario folder and are not
# needed once the solution has been imported
drop_solver_outputs(scen)                    # dry run: reports what it would do
drop_solver_outputs(scen, dry_run = FALSE)
} # }
```
