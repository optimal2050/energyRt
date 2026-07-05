# An S4 class to represent scenario, an interpolated and/or solved model.

An S4 class to represent scenario, an interpolated and/or solved model.

## Slots

- `name`:

  character. Name of the scenario object, for reference, also used in
  scenario path-functions.

- `desc`:

  character. Description of the scenario object, for own references.

- `model`:

  model. Model object with the model data and configuration settings.

- `settings`:

  settings. Settings object with the scenario-specific settings.
  Initialized with the default settings from the model configuration
  object. Overrule the model config for the scenario-specific
  parameters.

- `modInp`:

  modInp. Model input object with the interpolated model parameters.

- `modOut`:

  modOut. Model output object with the model solution and logs.

- `status`:

  character. Indication of the solution status of the model/scenario.

- `inMemory`:

  logical. Indication if the scenario is stored in memory or on the
  disk.

- `path`:

  character. Path to the scenario folder with the model data and
  scripts.

- `misc`:

  list. Any additional data or information to store in the object, added
  by the user or the functions.

## See also

`interpolate()`, [`solve()`](https://rdrr.io/r/base/solve.html),
[`register()`](https://energyRt.org/reference/register.md),
[`summary()`](https://energyRt.org/reference/summary.md),
[`newScenario()`](https://energyRt.org/reference/newScenario.md)
