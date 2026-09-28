# Example calendars

A named list of ready-to-use
[calendar](https://energyRt.org/reference/class-calendar.md) objects
covering common sub-annual time resolutions. Pass any element to
[`newModel()`](https://energyRt.org/reference/newModel.md) /
`setCalendar()`, inspect it with
[`plot()`](https://energyRt.org/reference/draw.md) / `autoplot()`, or
use it as a template for
[`newCalendar()`](https://energyRt.org/reference/newCalendar.md).

## Usage

``` r
calendars
```

## Format

A named list of `calendar` objects:

- annual:

  Annual resolution (1 timeslice).

- s4_hp3:

  Four seasons x three hour types `DAY/NIGHT/PEAK`, day-proportional
  seasons and a uniform 12/8/4 split (12 timeslices).

- m12, m12a:

  Monthly resolution, day-proportional shares – `m01..m12` and
  `JAN..DEC` labels respectively (12 timeslices).

- q4:

  Calendar quarters `Q1..Q4`, day-proportional (4 timeslices).

- s4:

  Meteorological seasons `WIN/SPR/SUM/FAL` in calendar order,
  day-proportional 90/92/92/91 shares (4 timeslices).

- s4_h24:

  Seasons x 24 hours (96 timeslices) – the TOPIA base calendar.

- m12_h24:

  Months x 24 hours (288 timeslices) – the higher-resolution TOPIA
  option.

- wd7_h24:

  Weekday (`MON..SUN`) x 24 hours (168 timeslices).

- w52_h24:

  Week (`w01..w52`) x 24 hours (1248 timeslices).

- d365:

  Daily resolution, 365 days.

- d365_h24:

  Full hourly year: 365 days x 24 hours (8760 timeslices).

- s4_h24_subset_2seasons:

  SAMPLED: `s4_h24` filtered to WIN+SUM; `year_fraction` ~ 0.499.

- m12_h24_subset_4months:

  SAMPLED: `m12_h24` filtered to m01/m04/m07/m10; `year_fraction` ~
  0.337.

- m12_subset_q1:

  SAMPLED: `m12` filtered to Jan-Mar; `year_fraction` = 90/365.

- d365_h24_1dpm:

  SAMPLED: one day per month at hourly resolution (288 timeslices,
  `year_fraction` ~ 12/365).

- d365_h24_1dps:

  SAMPLED: one day per season at hourly resolution (d015/d105/d196/d288,
  96 timeslices, `year_fraction` ~ 4/365).

The mainstream designs (`m12` .. `w52_h24` and the first three sampled
entries) are generated from the `timescales` catalog at DATA-BUILD time
only – timescales is not a runtime dependency. Sampled calendars carry
`year_fraction < 1` and solve partial years natively: their timetables
are row subsets of the parent's (never rebuilt with
[`make_timetable()`](https://energyRt.org/reference/calendar.md), which
would renormalise the shares) with the surviving `sum(share)` passed as
`year_fraction`. The hourly `d365_h24*` entries come from IDEEA. See
`data-raw/calendars.R` for the generating script.

## Details

Only GENERIC calendars are shipped. Calendars belonging to a particular
model travel with it: the TOPIA teaching calendar is
`topia$modules$calendars$topia_seasons`, and the unit kits' symmetric
calendars are `topia$modules$unit$calendars`.

## See also

[`newCalendar()`](https://energyRt.org/reference/newCalendar.md),
[`make_timetable()`](https://energyRt.org/reference/calendar.md),
[horizons](https://energyRt.org/reference/horizons.md)

## Examples

``` r
names(calendars)
#>  [1] "annual"                 "d365"                   "d365_h24"              
#>  [4] "m12"                    "m12a"                   "q4"                    
#>  [7] "s4"                     "s4_h24"                 "m12_h24"               
#> [10] "wd7_h24"                "w52_h24"                "s4_h24_subset_2seasons"
#> [13] "m12_h24_subset_4months" "m12_subset_q1"          "d365_h24_1dps"         
#> [16] "d365_h24_1dpm"          "s4_hp3"                
plot(calendars$s4_hp3)
```
