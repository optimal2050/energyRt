# An S4 class to represent sub-annual time resolution structure.

Sub-annual time resolution is represented by nested, named time-frames
and time-slices.

## Slots

- `name`:

  character. Name of the calendar object. Use to distingush between
  different structures and subsets of time-slices. The name is used to
  propose default folder names for the model/scenario scripts to
  separate solutions of the same scenario with different calendar
  objects.

- `desc`:

  character. Description of the calendar object, for own references.

- `timeframes`:

  list. Named list of nested sub-annual levels with vectors of
  individual elements. The top level of the list is the highest level of
  the calendar, e.g., "ANNUAL". The lowest level is the smallest
  time-slice, e.g., "MONTH". "ANNUAL" is the default (hardwired) top
  level of the calendar. All other levels are optional, and create
  nested sub-annual levels of time-slices. The minimum number of
  time-slices in a timeframe is two (except for the top level).

- `year_fraction`:

  numeric. The fraction of a year covered by the calendar, e.g. 1 for
  annual calendar (default), 0.5 for semi-annual, 0.25 for quarterly,
  etc. Currently must be specified manually for subset calendars to
  validate the sum of the shares.

- `timetable`:

  data.frame. Data frame with levels of timeframes in the named columns,
  and number of rows equal to the total number of time-slices on the
  lowest level. Every timeframe is a set of time-slices ("slices") - a
  named fragment of time with a year-share. Timeframes have nested
  structure where every slice serves as a parent for the lower level of
  time-slices (children). The first column is the name of the
  time-slice, the rest of the columns are the names of the timeframes.
  The values are the share of the year covered by the time-slice. The
  sum of the shares in every timeframe should be equal to 1. `weight` is
  an optional column with the weight of the time-slice in the year, used
  for sumpled/subset selection of the time-slices.

- `slice_share`:

  data.frame. Auto-calculated from the `timetable` two column data.frame
  with slices from all levels with their individual share in a year. The
  first column is the name of the time-slice, the second column is the
  share of the year covered by the time-slice.

- `default_timeframe`:

  character. The name of the default level of the time-slices used in
  the model. If not specified, the lowest level of the timeframes is
  used as the default timeframe.

- `timeframe_rank`:

  character. Auto-calculated from the `timetable` and `timeframes` slots
  named character vector with ranks of the timeframes. The rank is used
  to determine the order of the timeframes in the calendar.

- `slices_in_frame`:

  integer. Auto-calculated from the `timetable` Number of time-slices in
  every timeframe.

- `slice_family`:

  data.frame. Auto-calculated from the `timetable` data.frame mapping
  "parent" to "child" slices in two nearest timeframes in the nested
  hierarchy. The first column is the name of the parent time-slice, the
  second column is the name of the child time-slice.

- `slice_ancestry`:

  data.frame. Auto-calculated from the `timetable` data.frame mapping
  "child", "grandchild", etc. slices to the "parent" and "grandparent"
  time-slices in the full hierarchy. The first column is the name of the
  (grand-) child time-slice, the second column is the name of the
  (grand-) parent time-slice.

- `next_in_timeframe`:

  data.frame. Auto-calculated from the `timetable` data.frame mapping
  chronological sequence between time-slices in the same timeframe. The
  first column is the name of the time-slice, the second column is the
  name of the next time-slice in the same timeframe.

- `next_in_year`:

  data.frame. Auto-calculated from the `timetable` data.frame mapping
  chronological sequence between time-slices in the same timeframe
  through the whole year. The first column is the name of the
  time-slice, the second column is the name of the next time-slice in
  the same timeframe.

- `misc`:

  list. Any additional data or information to store in the object.
