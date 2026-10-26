# Override how folder names are derived

The storage layer derives every folder name through a small set of
seams, each of which a user function can replace for the session:

|  |  |  |
|----|----|----|
| kind | signature | names |
| `scenario_dir` | `function(name, model, calendar, horizon)` | the scenario folder basename under [`get_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md) |
| `store_entry` | `function(type, name)` — type `"model"`, `"repository"` or `"dataset"` | a store entry's basename under its root |
| `run_label` | `function(solver)` — the solver spec list | the default solve label when `run =` is not given |
| `slug` | `function(parts)` — character vector | the sanitize-and-join primitive used everywhere else |

A hook must return a single non-empty, filesystem-safe string (no path
separators; letters, digits, `.`/`_`/`-`); an invalid return is an ERROR
naming the contract — never a silent fallback. Custom scenario and store
folder names are fully safe: resolution goes through the registry and
the folder manifests, never through parsing names. The run TREE
(`runs/<solve>` vs `runs/<variant>/<solve>`) is structural and not
hookable. A custom `slug` should keep dashes out of the individual
parts, or folder labels become ambiguous (harmless — nothing parses them
— but stated). Hooks are session-local options, not stored in projects.

## Usage

``` r
set_path_builder(
  scenario_dir = NULL,
  store_entry = NULL,
  run_label = NULL,
  slug = NULL
)

get_path_builder(kind = NULL)
```

## Arguments

- scenario_dir, store_entry, run_label, slug:

  a function per the table (merged into the active set), or `FALSE` to
  remove that hook; `NULL` (default) leaves it unchanged.

- kind:

  character, one hook name to fetch (or `NULL` for the whole list).

## Value

`set_path_builder()` the previous list, invisibly; `get_path_builder()`
the requested function (or `NULL`) / the list.

## See also

Other options:
[`en_config_path()`](https://energyRt.org/reference/en_config.md),
[`en_config_show()`](https://energyRt.org/reference/en_config_show.md),
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`get_exchange_format()`](https://energyRt.org/reference/exchange_format.md),
[`get_storage_format()`](https://energyRt.org/reference/storage_format.md),
[`isVerbose()`](https://energyRt.org/reference/isVerbose.md),
[`set_log_file()`](https://energyRt.org/reference/log.md),
[`set_option()`](https://energyRt.org/reference/en_option.md),
[`set_registry_file()`](https://energyRt.org/reference/registry_file.md),
[`set_reports_path()`](https://energyRt.org/reference/reports_path.md),
[`set_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)

## Examples

``` r
if (FALSE) { # \dontrun{
set_path_builder(scenario_dir = function(name, model, calendar, horizon)
  paste(name, calendar, sep = "."))
set_path_builder(scenario_dir = FALSE)   # back to the default
} # }
```
