# Register an object in the registry.

Register an repository, model, or scenario object in the registry.
**\[experimental\]**

## Usage

``` r
register(
  obj,
  registry,
  name = obj@name,
  project = "",
  path = "",
  memo = "",
  datetime = lubridate::now(tzone = "UTC"),
  user = Sys.info()["user"],
  system = Sys.info()["sysname"],
  ...,
  env = obj@misc$env,
  replace = FALSE
)
```

## Arguments

- obj:

  object to be registered.

- registry:

  registry object to add the entry.

- name:

  character, name of the object.

- project:

  character, optional, the name of the project.

- path:

  character, optional path to the object's 'onDisk' directory.

- memo:

  character, optional short note about the object.

- datetime:

  timestamp, optional, date and time of the registration.

- user:

  character, optional, user who registered the object.

- system:

  character, optional, system where the object is registered.

- ...:

  (reserved for future use).

- env:

  character, environment where the object is stored.

- replace:

  logical, if TRUE, replace the existing entry.

## Examples

``` r
# `registry` methods are in development.
```
