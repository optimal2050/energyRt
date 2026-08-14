# Generate a new calendar object from

Calendars are defined by the structure of timeframes and time-timeslices
with shares of time in a year. The structure is represented by a
`timetable` data.frame with levels of timeframes in the named columns,
and names of individual time-timeslices in every timeframe. The number
of rows in `timetable` is equal to the total number of time-timeslices
on the lowest level. Every timeframe is a set of timeslices
("timeslices") - a named fragment of time with a year-share. Timeframes
have nested structure. Currently, every "parent"-timeframe must have the
same number of elements as the "child"-timeframe. (This may change in
the future.)

- ANNUAL:

  character, annual, the top level of timeframes

- TIMEFRAME2:

  character, (optional) first subannual level of timeframes

- TIMEFRAME3:

  character, (optional) second subannual level of timeframes

- ...:

  character, (optional) further subannual levels of timeframes

- timeslice:

  character, name of the timeslices used in sets to refer to the lowest
  level of timeframes. If not specified, will be auto-created with the
  formula: `{TIMESLICE2}_{TIMESLICE3}...`

## Usage

``` r
newCalendar(
  name = "",
  desc = "",
  timetable = NULL,
  year_fraction = 1,
  default_timeframe = NULL,
  misc = list(pTimesliceWeight = NULL),
  ...
)
```

## Arguments

- name:

  character. Name of the calendar object. Use to distingush between
  different structures and subsets of time-timeslices. The name is used
  to propose default folder names for the model/scenario scripts to
  separate solutions of the same scenario with different calendar
  objects.

- desc:

  character. Description of the calendar object, for own references.

- timetable:

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

- year_fraction:

  numeric. The fraction of a year covered by the calendar, e.g. 1 for
  annual calendar (default), 0.5 for semi-annual, 0.25 for quarterly,
  etc. Currently must be specified manually for subset calendars to
  validate the sum of the shares.

- default_timeframe:

  character. The name of the default level of the time-timeslices used
  in the model. If not specified, the lowest level of the timeframes is
  used as the default timeframe.

- misc:

  list. Any additional data or information to store in the object.

- ...:

  ignored

## Value

an object of class `calendar` with the specified structure.

## Examples

``` r
newCalendar()
#> An object of class "calendar"
#> Slot "name":
#> [1] ""
#> 
#> Slot "desc":
#> [1] ""
#> 
#> Slot "timeframes":
#> $ANNUAL
#> [1] "ANNUAL"
#> 
#> 
#> Slot "year_fraction":
#> [1] 1
#> 
#> Slot "timetable":
#>    ANNUAL timeslice share weight
#>    <char>    <char> <num>  <num>
#> 1: ANNUAL    ANNUAL     1      1
#> 
#> Slot "timeslice_share":
#>    timeslice share weight
#>       <char> <num>  <num>
#> 1:    ANNUAL     1      1
#> 
#> Slot "default_timeframe":
#> [1] "ANNUAL"
#> 
#> Slot "timeframe_rank":
#> ANNUAL 
#>      1 
#> 
#> Slot "timeslices_in_frame":
#> ANNUAL 
#>      1 
#> 
#> Slot "timeslice_family":
#> Empty data.table (0 rows and 2 cols): parent,child
#> 
#> Slot "timeslice_ancestry":
#> Empty data.table (0 rows and 2 cols): parent,child
#> 
#> Slot "next_in_timeframe":
#> Empty data.table (0 rows and 2 cols): timeslice,timeslicep
#> 
#> Slot "next_in_year":
#> Empty data.table (0 rows and 2 cols): timeslice,timeslicep
#> 
#> Slot "misc":
#> $pTimesliceWeight
#> NULL
#> 
#> 
```
