# An S4 class to store results of a solved scenario

The class is a part of the scenario object and stores the results of a
solved scenario. It is not intended to be used as a standalone object.

## Slots

- `sets`:

  list. Named list of sets used in the model.

- `variables`:

  list. Named list of data frames with variables imported from solved
  scenario.

- `stage`:

  character. Indication of the solution status of the model/scenario.

- `solutionLogs`:

  data.frame. Data frame with the model solution logs.

- `misc`:

  list. Any additional information or data to store in the object.
