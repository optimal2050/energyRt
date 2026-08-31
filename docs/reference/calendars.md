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

- season_dn:

  Four seasons x day/night (8 timeslices).

- utopia_seasons:

  UTOPIA: 4 seasons x 3 dayparts (DAY/NIGHT/PEAK) with representative
  shares (12 timeslices).

- unit_s4, unit_s4h4:

  Perfectly symmetric unit calendars for the hand-computable
  `utopia$modules$unit` kits.

- m12, m12a:

  Monthly resolution, day-proportional shares – `m01..m12` and
  `JAN..DEC` labels respectively (12 timeslices).

- q4:

  Calendar quarters `Q1..Q4`, day-proportional (4 timeslices).

- s4:

  Meteorological seasons `WIN/SPR/SUM/FAL` in calendar order,
  day-proportional 90/92/92/91 shares (4 timeslices).

- s4_h24:

  Seasons x 24 hours (96 timeslices) – the UTOPIA base calendar.

- m12_h24:

  Months x 24 hours (288 timeslices) – the higher-resolution UTOPIA
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

- d365_h24_subset_1day_per_month:

  SAMPLED: one day per month at hourly resolution (288 timeslices,
  `year_fraction` ~ 12/365).

The mainstream designs (`m12` .. `w52_h24` and the first three sampled
entries) are generated from the `timescales` catalog at DATA-BUILD time
only – timescales is not a runtime dependency. Sampled calendars carry
`year_fraction < 1` and solve partial years natively: their timetables
are row subsets of the parent's (never rebuilt with
[`make_timetable()`](https://energyRt.org/reference/calendar.md), which
would renormalise the shares) with the surviving `sum(share)` passed as
`year_fraction`. The hourly `d365_h24*` entries come from IDEEA. See
`data-raw/calendars.R` for the generating script.

## See also

[`newCalendar()`](https://energyRt.org/reference/newCalendar.md),
[`make_timetable()`](https://energyRt.org/reference/calendar.md),
[horizons](https://energyRt.org/reference/horizons.md)

## Examples

``` r
names(calendars)
#>  [1] "season_dn"                      "d365"                          
#>  [3] "annual"                         "utopia_seasons"                
#>  [5] "unit_s4"                        "unit_s4h4"                     
#>  [7] "d365_h24"                       "d365_h24_subset_1day_per_month"
#>  [9] "m12"                            "m12a"                          
#> [11] "q4"                             "s4"                            
#> [13] "s4_h24"                         "m12_h24"                       
#> [15] "wd7_h24"                        "w52_h24"                       
#> [17] "s4_h24_subset_2seasons"         "m12_h24_subset_4months"        
#> [19] "m12_subset_q1"                 
plot(calendars$season_dn)
```
