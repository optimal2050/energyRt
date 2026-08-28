# Generate a new scenario object

Generate a new scenario object

## Usage

``` r
newScenario(
  name,
  model = NULL,
  path = fp(get_scenarios_path(), name),
  ...,
  env_name = NULL,
  registry = NULL,
  replace = NULL
)
```

## Arguments

- name:

  character. Name of the scenario object, for reference, also used in
  scenario path-functions.

- path:

  character. Path to the scenario folder with the model data and
  scripts.

- ...:

  any model objects or settings to be assigned to the scenario.

- env_name, registry, replace:

  deprecated and ignored — the in-memory registry has been replaced by
  the persisted project registry (see
  [`load_registry()`](https://energyRt.org/reference/registry.md));
  scenarios are registered on
  [`save_scenario()`](https://energyRt.org/reference/save_scenario.md).

## Value

A new scenario object.

## Examples

``` r
# It is suggested to use `interpolate(model)` or `solve(model)` to create a new scenario.
```
