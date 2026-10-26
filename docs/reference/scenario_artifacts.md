# Solver artifacts of a stored scenario

Lists what each solve left on disk — whether its solution was imported,
what the scratch costs, and which rows are safe to clean up. Read-only.

## Usage

``` r
scenario_artifacts(object)
```

## Arguments

- object:

  a scenario object.

  Rows for the scenario-level derived tiers are listed too,
  distinguished by `kind`: `"reports"` for rendered reports and
  `"levcost"` for cached levcost results. They sit beside `runs/` rather
  than inside it, and each has its own remover —
  [`clear_report_cache()`](https://energyRt.org/reference/clear_report_cache.md)
  and
  [`clear_levcost_cache()`](https://energyRt.org/reference/clear_levcost_cache.md)
  — so
  [`drop_solver_outputs()`](https://energyRt.org/reference/drop_solver_outputs.md),
  a run cleaner, leaves them alone.

## Value

a tibble: `kind`, `run`, `variant`, `solve`, `status`, `imported`,
`scratch_mb`, `solution_mb`, `active`, `sealed`, `has_record`, `path`,
`suggest`.

## Details

The `scratch_mb` column counts the regenerable part: the generated model
source, the exchange `input/`, solver logs, and `output/` **only when
the solution has been imported** into `modOut/`. Until that has happened
`output/` is the only copy of the solution and is never counted as
scratch, whatever `status` says — `status == "solved"` records that the
solution reached memory, not disk.

`suggest` is a reason string, empty when the row is not a candidate. A
sealed scenario suggests nothing. The **active** run is suggested like
any other — unlike
[`drop_scenario_run()`](https://energyRt.org/reference/scenario_runs.md),
cleaning scratch keeps the record and the solution, so the run stays
listed and readable.

## See also

[`drop_solver_outputs()`](https://energyRt.org/reference/drop_solver_outputs.md),
[`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md),
[`clear_report_cache()`](https://energyRt.org/reference/clear_report_cache.md)

## Examples

``` r
# \donttest{
ek   <- topia$modules$electricity$R3
mod  <- newModel("TOPIA", data = ek$repo,
                 calendar = topia$modules$calendars$s4_h24,
                 region = ek$regions,
                 horizon = topia$modules$horizons$base,
                 discount = 0.05)
mod <- setGeoscale(mod, ek$geoscale)
scen <- interpolate_model(mod, name = "EX")
#> Cost declared at a coarse geoscale level:
#>   TBD_COA_WEST_CENTRAL@invcost: CENTRAL, WEST
#>   TBD_GAS_WEST_CENTRAL@invcost: CENTRAL, WEST
#> The cost is charged ONCE at that cell rather than at each child region, and is discounted at the children's rate. That is usually what a national cost datum means -- said here because it is a different number from naming the children individually.

# what exists on disk for this scenario, and what could be dropped
scenario_artifacts(scen)
#> # A tibble: 0 × 13
#> # ℹ 13 variables: kind <chr>, run <chr>, variant <chr>, solve <chr>,
#> #   status <chr>, imported <lgl>, scratch_mb <dbl>, solution_mb <dbl>,
#> #   active <lgl>, sealed <lgl>, has_record <lgl>, path <chr>, suggest <chr>
# }
```
