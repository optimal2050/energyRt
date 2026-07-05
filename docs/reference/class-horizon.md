# An S4 class to represent model/scenario planning horizon with intervals (year-steps)

An S4 class to represent model/scenario planning horizon with intervals
(year-steps)

## Slots

- `name`:

  character. Name of the horizon object. Used to distinguish between
  different horizons in the model or scenario, including the automatic
  creation of the folder name for the model/scenario scripts.

- `desc`:

  character. Description of the horizon object, for own references.

- `period`:

  integer. A planning period defined as a sequence of years (arranged,
  without gaps) of the model planning (e.g. optimization) window. Data
  with years before or after the planning `period` can present in the
  model-objects and will be taken into account during interpolation of
  the model parameters.

- `intervals`:

  data.frame. Data frame with three columns, representing start, middle,
  and the end year of every interval. The first column is the start year
  of the interval, the second column is the middle year of the interval,
  the third column is the end year of the interval.
