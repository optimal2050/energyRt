# An S4 class to store the model objects.

Use `newRepository` to create a new repository object.

## Slots

- `name`:

  character. Name of the repository.

- `desc`:

  character. Description of the repository.

- `data`:

  list. Model objects ("bricks"), e.g., technologies, constraints,
  costs, etc., stored in with their names as keys, or gropped in named
  lists.

- `permit`:

  character. Vector with names of classes permitted to store in the
  repository. There is a default list of permitted classes which can be
  extended or modified. Used in internal functions, it is not common to
  modify this slot.

- `misc`:

  list. Any additional data or information to store in the object.

## See also

Other repository model data:
[`newRepository()`](https://energyRt.org/reference/newRepository.md)
