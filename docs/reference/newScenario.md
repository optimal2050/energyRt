# Generate a new scenario object

Generate a new scenario object

## Usage

``` r
newScenario(
  name,
  model = NULL,
  path = fp(get_scenarios_path(), name),
  ...,
  env_name = ".scen",
  registry = get_registry(),
  replace = FALSE
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

- env_name:

  character. Name of the environment to register the scenario. Default
  is ".scen". Used only if registry is provided. (in development)

- registry:

  optional registry object to register the scenario. (in development)

- replace:

  logical. If TRUE, replace the entry of the scenario in the registry if
  the entry already exists. (in development)

## Value

A new scenario object.

## Examples

``` r
# It is suggested to use `interpolate(model)` or `solve(model)` to create a new scenario.
```
