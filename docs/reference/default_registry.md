# Default registry

The registry holds repositories, models and scenarios.
`set_default_registry()` records which object, in which environment,
energyRt should use; `which_registry()` reports it, and `get_registry()`
returns the object itself, creating it if it does not exist yet.

## Usage

``` r
set_default_registry(obj_name = "registry", env_name = ".scen")

use_registry(obj_name = "registry", env_name = ".scen")

which_registry()

get_registry()
```

## Arguments

- obj_name:

  character, name of the registry object.

- env_name:

  character, name of the environment holding it.

## Value

`which_registry()` a list with `name` and `env`; `get_registry()` the
registry object; `set_default_registry()` the previous value, invisibly.

The current registry object.

## See also

Other options:
[`en_config_path()`](https://energyRt.org/reference/en_config.md),
[`en_config_show()`](https://energyRt.org/reference/en_config_show.md),
[`get_arrow_format()`](https://energyRt.org/reference/arrow_format.md),
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`isVerbose()`](https://energyRt.org/reference/isVerbose.md),
[`set_option()`](https://energyRt.org/reference/en_option.md),
[`set_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)
