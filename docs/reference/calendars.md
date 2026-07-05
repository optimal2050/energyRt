# Example calendars

A named list of ready-to-use
[calendar](https://energyRt.org/reference/class-calendar.md) objects
covering common sub-annual time resolutions. Pass any element to
[`newModel()`](https://energyRt.org/reference/newModel.md) /
`setCalendar()`, inspect it with
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) / `autoplot()`,
or use it as a template for
[`newCalendar()`](https://energyRt.org/reference/newCalendar.md).

## Usage

``` r
calendars
```

## Format

A named list of `calendar` objects, including:

- season_dn:

  Four seasons x day/night (8 slices).

- d365:

  Daily resolution, 365 days.

- utopia_annual:

  UTOPIA: annual resolution (1 slice).

- utopia_seasons:

  UTOPIA: 4 seasons x 3 dayparts (DAY/NIGHT/PEAK) with representative
  shares (12 slices) – the default UTOPIA resolution.

- utopia_m12h24:

  UTOPIA: 12 months x 24 hours (288 slices).

- d365_h24:

  Full hourly year: 365 days x 24 hours (8760 slices).

- d365_h24_subset_1day_per_month:

  Representative subset: one day per month at hourly resolution (288
  slices, `year_fraction` ~ 12/365).

The `utopia_*` calendars are built from energyRt's own constructors; the
hourly `d365_h24*` calendars are imported from the IDEEA package. See
`data-raw/calendars.R` for the generating script.

## See also

[`newCalendar()`](https://energyRt.org/reference/newCalendar.md),
[`make_timetable()`](https://energyRt.org/reference/calendar.md),
[horizons](https://energyRt.org/reference/horizons.md)

## Examples

``` r
names(calendars)
#> [1] "season_dn"                      "d365"                          
#> [3] "utopia_annual"                  "utopia_seasons"                
#> [5] "utopia_s4h24"                   "utopia_m12h24"                 
#> [7] "d365_h24"                       "d365_h24_subset_1day_per_month"
plot(calendars$season_dn)
```
