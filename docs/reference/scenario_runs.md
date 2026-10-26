# List, inspect, and drop a scenario's runs

A solved scenario's runs live under `<scenario>/runs/` — base-problem
runs directly (`runs/glpk/`), own-problem variants as named folders
holding their solves (`runs/cal-d24/glpk/`) — each with a `run.yml`
provenance record (see
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md)).
The scenario manifest's `default:` field names the default run.

- `scenario_runs()` lists them as a tibble — one row per run, including
  legacy `script/<solver>/` directories from older layouts
  (`status = "legacy"`).

- `scenario_run_info()` returns one run's full `run.yml` record as a
  list.

- `drop_scenario_run()` deletes a run directory. The active run (the one
  currently loaded in the scenario object) is refused unless
  `force = TRUE`.

## Usage

``` r
scenario_runs(object)

scenario_run_info(object, run)

drop_scenario_run(object, run, force = FALSE)
```

## Arguments

- object:

  a scenario object with a non-empty `@path`.

- run:

  character, run identifier: `"<solve>"` for a base-problem run or
  `"<variant>/<solve>"`.

- force:

  logical, allow dropping the active run.

## Value

`scenario_runs()` a tibble (run, variant, solve, status, solver_name,
lang, started, duration_sec, peak_mb, updated, objective, modinp,
active); `scenario_run_info()` a named list; `drop_scenario_run()` the
dropped directory, invisibly.

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

# a freshly interpolated scenario has no runs yet; each solve adds one
scenario_runs(scen)
#> # A tibble: 0 × 13
#> # ℹ 13 variables: run <chr>, variant <chr>, solve <chr>, status <chr>,
#> #   solver_name <chr>, lang <chr>, started <chr>, duration_sec <dbl>,
#> #   peak_mb <dbl>, updated <chr>, objective <dbl>, modinp <chr>, active <lgl>
# }
```
