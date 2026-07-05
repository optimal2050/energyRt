# Make a name for a scenario directory

A function to automate the creation of a scenario directory name. Used
internally in `solve*()` and `interpolate*()` functions. Also can be
used to amend the name of the scenario directory and explicitly assign
the directory name to save the scenario object.

## Usage

``` r
make_scenario_dirname(
  scen,
  name = scen@name,
  model_name = scen@model@name,
  calendar_name = scen@settings@calendar@name,
  horizon_name = scen@settings@horizon@name,
  prefix = NULL,
  suffix = NULL,
  sep = "_"
)
```

## Arguments

- scen:

  scenario object

- name:

  character, name of the scenario, default is `scen@name`

- model_name:

  character, name of the model, default is `scen@model@name`

- calendar_name:

  character, name of the calendar, default is
  `scen@settings@calendar@name`

- horizon_name:

  character, name of the horizon, default is
  `scen@settings@horizon@name`

- prefix:

  character, prefix to add to the name

- suffix:

  character, suffix to add to the name

- sep:

  character, separator, default is `_`

## Value

character, name of the scenario directory

## Examples

``` r
if (FALSE) { # \dontrun{
make_scenario_dirname(scen_BASE)
make_scenario_dirname(scen_BASE, prefix = "prefix", suffix = "suffix")
} # }
```
