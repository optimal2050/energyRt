# Read the operation log

`get_profile_dir()` / `set_profile_dir()` locate the interpolation
profile log: four CSV files (`interp_runs.csv`, `interp_stages.csv`,
`interp_maps.csv`, `interp_params.csv`) that every
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
call appends its per-stage / per-map / per-parameter timing and memory
readings to (R heap, per-stage heap peak, OS process RSS and its
process-lifetime peak). When unset, the tables land next to the
operation log if one is set, else under `<project>/logs/` when a project
registry exists; with no sink resolvable nothing is written.

The operation log is an optional, project-wide record of what ran:
`set_log_file()` names a CSV file, and from then on
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md),
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md) (and
through it
[`solve_model()`](https://energyRt.org/reference/solve_model.md)), and
[`solve_recursive()`](https://energyRt.org/reference/solve_recursive.md)
append one line each —
`timestamp, op, object, status, duration_sec, details`. `read_log()`
reads it back as a tibble with parsed timestamps.

Levcost's internal mini-model solves are not logged (their scratch
scenarios are dot-prefixed). Per-run solver detail lives in each run's
`run.yml` (see
[`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md));
the log records the sequence.

## Usage

``` r
set_log_file(path = NULL)

get_log_file()

set_profile_dir(path = NULL)

get_profile_dir()

read_log(file = get_log_file())
```

## Arguments

- path:

  character, the file or directory to use; `NULL` clears the setting and
  restores the default.

- file:

  character, the log file (default: the `log_file` option).

## Value

a tibble, one row per logged operation (empty when the file does not
exist).

## See also

Other options:
[`en_config_path()`](https://energyRt.org/reference/en_config.md),
[`en_config_show()`](https://energyRt.org/reference/en_config_show.md),
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`get_exchange_format()`](https://energyRt.org/reference/exchange_format.md),
[`get_storage_format()`](https://energyRt.org/reference/storage_format.md),
[`isVerbose()`](https://energyRt.org/reference/isVerbose.md),
[`set_option()`](https://energyRt.org/reference/en_option.md),
[`set_path_builder()`](https://energyRt.org/reference/path_builders.md),
[`set_registry_file()`](https://energyRt.org/reference/registry_file.md),
[`set_reports_path()`](https://energyRt.org/reference/reports_path.md),
[`set_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)

## Examples

``` r
if (FALSE) { # \dontrun{
set_log_file("energyRt_log.csv")
scen <- interpolate_model(mod) |> solve_scenario()
read_log()
} # }
```
