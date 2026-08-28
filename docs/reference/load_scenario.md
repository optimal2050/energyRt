# Load scenario (in progress)

The batch companion of `load_scenario()`: resolves scenario NAMES
through the project registry (all registered scenarios by default) and
loads each as a thin on-disk shell. The result — a named list, or a
filled environment — feeds
[`getData()`](https://energyRt.org/reference/getData.md) directly:
`getData(load_scenarios(), name = "vTechOut", merge = TRUE)`.

A registered scenario whose folder is gone is skipped with a warning
(run [`refresh_registry()`](https://energyRt.org/reference/registry.md)
to drop the stale row).

## Usage

``` r
load_scenario(
  path,
  name = NULL,
  env = .scen,
  overwrite = FALSE,
  ignore_errors = FALSE,
  verbose = TRUE
)

load_scenarios(names = NULL, run = NULL, env = NULL, verbose = TRUE)
```

## Arguments

- path:

  character. Path to saved with function `save_scenario` scenario
  directory.

- name:

  character. Name to assign to the loaded scenario object. By default,
  the name is taken from the loaded scenario object.

- env:

  environment to assign the scenarios into (e.g. `.scen`), or `NULL`
  (default) to return them as a named list.

- overwrite:

  logical. Overwrite existing scenario object in the environment.

- ignore_errors:

  logical. Ignore load errors and continue execution. This option is
  useful when some data is missing or corrupted.

- verbose:

  logical.

- names:

  character, scenario names to load; `NULL` (default) = every scenario
  in the registry.

- run:

  character, optional run id to activate in each loaded scenario via
  [`read_solution()`](https://energyRt.org/reference/read.md) (scenarios
  without that run keep their default, with a warning).

## Value

TRUE if scenario is loaded, FALSE if not.

a named list of scenarios, or (with `env=`) the loaded names, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
load_scenario("scenarios/base")
} # }
```
