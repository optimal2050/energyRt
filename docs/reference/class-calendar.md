# An S4 class to represent sub-annual time resolution structure.

Sub-annual time resolution is represented by nested, named time-frames
and time-timeslices.

## Slots

- `name`:

  character. Name of the calendar object. Use to distingush between
  different structures and subsets of time-timeslices. The name is used
  to propose default folder names for the model/scenario scripts to
  separate solutions of the same scenario with different calendar
  objects.

- `desc`:

  character. Description of the calendar object, for own references.

- `timeframes`:

  list. Named list of nested sub-annual levels with vectors of
  individual elements. The top level of the list is the highest level of
  the calendar, e.g., "ANNUAL". The lowest level is the smallest
  time-timeslice, e.g., "MONTH". "ANNUAL" is the default (hardwired) top
  level of the calendar. All other levels are optional, and create
  nested sub-annual levels of time-timeslices. The minimum number of
  time-timeslices in a timeframe is two (except for the top level).

- `year_fraction`:

  numeric. The fraction of a year covered by the calendar, e.g. 1 for
  annual calendar (default), 0.5 for semi-annual, 0.25 for quarterly,
  etc. Currently must be specified manually for subset calendars to
  validate the sum of the shares.

- `year_start`:

  list. `list(month = , day = )` giving the first day of the model year;
  defaults to January 1. A non-January anchor makes model year `y` span
  `[year_start(y), year_start(y + 1))`, and `y` is the STARTING
  Gregorian year – Indian "FY 2021-22" is model year 2021 – which is the
  convention
  [`timescales::calendar_build()`](https://optimal2050.github.io/timescales/r/reference/calendar_build.html)
  uses. The anchor is carried and reported only: it sets the default
  milestone labels (see
  [`year_label()`](https://energyRt.org/reference/year_label.md)) and
  does NOT yet drive timeslice-to-timestamp alignment or
  `year_fraction`.

- `utc_offset_minutes`:

  integer. Constant offset of local time from UTC, in minutes; defaults
  to 0 (UTC). E.g. 330 for IST (UTC+5:30). Carried for provenance
  alongside `year_start`; Olson time zones and DST are not supported.

- `timetable`:

  data.frame. Data frame with levels of timeframes in the named columns,
  and number of rows equal to the total number of time-timeslices on the
  lowest level. Every timeframe is a set of time-timeslices
  ("timeslices") - a named fragment of time with a year-share.
  Timeframes have nested structure where every timeslice serves as a
  parent for the lower level of time-timeslices (children). The first
  column is the name of the time-timeslice, the rest of the columns are
  the names of the timeframes. The values are the share of the year
  covered by the time-timeslice. The sum of the shares in every
  timeframe should be equal to 1. `weight` is an optional column with
  the weight of the time-timeslice in the year, used for sumpled/subset
  selection of the time-timeslices.

- `timeslice_share`:

  data.frame. Auto-calculated from the `timetable` two column data.frame
  with timeslices from all levels with their individual share in a year.
  The first column is the name of the time-timeslice, the second column
  is the share of the year covered by the time-timeslice.

- `default_timeframe`:

  character. The name of the default level of the time-timeslices used
  in the model. If not specified, the lowest level of the timeframes is
  used as the default timeframe.

- `timeframe_rank`:

  character. Auto-calculated from the `timetable` and `timeframes` slots
  named character vector with ranks of the timeframes. The rank is used
  to determine the order of the timeframes in the calendar.

- `timeslices_in_frame`:

  integer. Auto-calculated from the `timetable` Number of
  time-timeslices in every timeframe.

- `timeslice_family`:

  data.frame. Auto-calculated from the `timetable` data.frame mapping
  "parent" to "child" timeslices in two nearest timeframes in the nested
  hierarchy. The first column is the name of the parent time-timeslice,
  the second column is the name of the child time-timeslice.

- `timeslice_ancestry`:

  data.frame. Auto-calculated from the `timetable` data.frame mapping
  "child", "grandchild", etc. timeslices to the "parent" and
  "grandparent" time-timeslices in the full hierarchy. The first column
  is the name of the (grand-) child time-timeslice, the second column is
  the name of the (grand-) parent time-timeslice.

- `next_in_timeframe`:

  data.frame. Auto-calculated from the `timetable` data.frame mapping
  chronological sequence between time-timeslices in the same timeframe.
  The first column is the name of the time-timeslice, the second column
  is the name of the next time-timeslice in the same timeframe.

- `next_in_year`:

  data.frame. Auto-calculated from the `timetable` data.frame mapping
  chronological sequence between time-timeslices in the same timeframe
  through the whole year. The first column is the name of the
  time-timeslice, the second column is the name of the next
  time-timeslice in the same timeframe.

- `misc`:

  list. Any additional data or information to store in the object.
