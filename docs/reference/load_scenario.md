# Load scenario (in progress)

Load scenario (in progress)

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
```

## Arguments

- path:

  character. Path to saved with function `save_scenario` scenario
  directory.

- name:

  character. Name to assign to the loaded scenario object. By default,
  the name is taken from the loaded scenario object.

- env:

  environment. Environment to assign the loaded scenario object.

- overwrite:

  logical. Overwrite existing scenario object in the environment.

- ignore_errors:

  logical. Ignore load errors and continue execution. This option is
  useful when some data is missing or corrupted.

- verbose:

  logical. Print messages.

## Value

TRUE if scenario is loaded, FALSE if not.

## Examples

``` r
if (FALSE) { # \dontrun{
load_scenario("scenarios/base")
} # }
```
