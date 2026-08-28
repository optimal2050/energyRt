# Make a name for a scenario directory (deprecated)

Deprecated: scenario folders are named by the internal
`.scenario_dir_name()` (which slugs the parts and drops duplicates);
this export never was the live implementation and produced different
names. It now delegates to the real one; `prefix`/`suffix` are still
honored. The previous body is archived in
`drafts/make_scenario_dirname-legacy.R`.

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

  character, ignored (kept for backward compatibility)

## Value

character, name of the scenario directory
