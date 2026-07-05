# An S4 class to represent scenario settings

Class 'settings' inherits all slots from class 'config' and adds the
following:

## Slots

- `subset`:

  list. Named list of subsets used in the model. The names of the list
  elements are the names of the subsets, the values are the vectors of
  the subset elements. The subsets are used to narrow the dimension of
  the model variables and constraints.

- `yearFraction`:

  numeric. The fraction of a year covered by the calendar, e.g. 1 for
  annual calendar (default), 0.5 for semi-annual, 0.25 for quarterly,
  etc. Currently must be specified manually for subset calendars to
  validate the sum of the shares.

- `solver`:

  list. Named list of solver parameters. The names of the list elements
  are the names of the solver parameters, the values are the solver
  parameters themselves. The solver parameters are used to control the
  optimization process.

- `sourceCode`:

  list. Named list of paths to the source code files. The names of the
  list elements are the names of the source code files, the values are
  the paths to the source code files. The source code files are used to
  store the model and scenario scripts.

## See also

Other class config settings scenario model:
[`class-config`](https://energyRt.org/reference/class-config.md)
