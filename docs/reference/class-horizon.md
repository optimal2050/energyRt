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

  data.frame. Data frame with the start, middle, and end year of every
  modelled interval, plus an optional display `label`. `start`, `mid`
  and `end` are integer years; `mid` is the milestone and the model's
  key for the period. `label` is a character display name, unique across
  intervals, filled from `mid` when not given, and rendered fiscally
  (`FY2025-26`) when the calendar carries a non-January `year_start`.
  The label is presentation only and never reaches the solver – see
  [`year_label()`](https://energyRt.org/reference/year_label.md).
