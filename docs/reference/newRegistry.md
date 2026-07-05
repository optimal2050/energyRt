# Create a new registry object.

Create a new registry object to store records of scenarios, models, and
repositories. **\[experimental\]**

## Usage

``` r
newRegistry(
  class = c("scenario", "model", "repository"),
  name = NULL,
  registry_env = ".GlobalEnv",
  store_env = ".scen"
)
```

## Arguments

- class:

  character, type of the classes to be stored in the registry.

- name:

  character, name of the registry object.

- registry_env:

  character, environment to store the registry object.

- store_env:

  character, environment to store the objects.

## Examples

``` r
# The `registry` methods are in development.
```
