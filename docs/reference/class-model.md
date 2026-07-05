# An S4 class to represent model

An S4 class to represent model

## Slots

- `name`:

  character. Name of the model object, for reference, also used in
  scenario path-functions.

- `desc`:

  character. Description of the model object, for own references.

- `data`:

  repository or list. A named list of model objects to interpolate and
  pass to the solver. Use the `repository` class to add objects to the
  model, or a list of model objects directly.

- `config`:

  config. Configuration object with the default model settings.

- `misc`:

  list. Any additional data or information to store in the object.
