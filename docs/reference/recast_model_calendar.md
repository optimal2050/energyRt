# Recast a model onto another calendar

Moves a model's time-varying data from the calendar it was written
against onto `calendar`, so the model can be built and solved at another
time resolution. The time mirror of
[`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md)
and
[`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md).

## Usage

``` r
recast_model_calendar(
  object,
  calendar,
  rules = NULL,
  year = NULL,
  verbose = isVerbose()
)
```

## Arguments

- object:

  a [model](https://energyRt.org/reference/class-model.md).

- calendar:

  the target
  [calendar](https://energyRt.org/reference/class-calendar.md); coarser
  or finer than the model's own.

- rules:

  optional named character vector of
  [timescales::CALENDAR_RULES](https://optimal2050.github.io/timescales/r/reference/CALENDAR_RULES.html)
  keyed by value column, overriding the rate default for those columns
  (e.g. `c(demand = "sum")`).

- year:

  model year used to materialise both calendars on a shared datetime
  grid. `NULL` (default) takes the model's first horizon milestone. It
  matters only where the calendars align to real dates (days of the
  year, weekdays), and then only across a leap year.

- verbose:

  print per-table messages.

## Value

the recast [model](https://energyRt.org/reference/class-model.md),
carrying `calendar`.

## Details

Every timeslice-indexed value in energyRt is a rate: the equations
multiply it by the timeslice weight to obtain an annual quantity.
Aggregating therefore takes the weighted mean, which conserves the
weighted annual total exactly, and disaggregating copies the rate into
each child timeslice. `rules` overrides this per column where a value is
not a rate (a declared quantity to be summed, a standard deviation).

Rows are recast by their own timeframe, not by their table: only rows at
the source calendar's leaf timeframe move. A row declared at `ANNUAL`,
or at any timeframe `calendar` also has, is left exactly as it is.

A `@timeframe` naming a timeframe `calendar` does not have falls back to
the finest timeframe that survives, and is reported – otherwise the
model references a timeframe its calendar cannot resolve.

## Comparability

A recast model is not a smaller answer to the same question. Collapsing
timeslices removes the variation inside them, which is what a model
prices: peaks flatten, variable resources become firmer than they are,
and storage loses the cycle it was built to serve. Its dispatch, costs
and objective are its own. Use it to run, plot and teach the shape of a
solution, not to report the source model's numbers.

## See also

Other interpolation:
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md),
[`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md),
[`with_solver_log()`](https://energyRt.org/reference/with_solver_log.md)

## Examples

``` r
mod <- newModel("TOPIA", data = topia$modules$electricity$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = topia$modules$electricity$regions,
                horizon = topia$modules$horizons$BY2025,
                discount = 0.05)

# 96 hourly timeslices collapse to 4 seasons: the same model, small enough
# to solve in a second
light <- recast_model_calendar(mod, calendar = calendars$s4)
#> recast onto calendar 's4' (96 -> 4 timeslices): 0 table(s), 0 -> 0 row(s); 6 timeframe declaration(s) -> SEASON: HYD@timeframe (HOUR); H2@timeframe (HOUR); ELC@timeframe (HOUR); WSOL@timeframe (HOUR); WWIN@timeframe (HOUR); WHYD@timeframe (HOUR).
#> Dispatch, costs and the objective are the recast model's own, not the source model's.
getCalendar(light)
#> An object of class "calendar"
#> Slot "name":
#> [1] "s4"
#> 
#> Slot "desc":
#> [1] "SEASON calendar (4 timeslices; complete, regular) (timescales 0.7.2)"
#> 
#> Slot "timeframes":
#> $ANNUAL
#> [1] "ANNUAL"
#> 
#> $SEASON
#> [1] "WIN" "SPR" "SUM" "FAL"
#> 
#> 
#> Slot "year_fraction":
#> [1] 1
#> 
#> Slot "year_start":
#> $month
#> [1] 1
#> 
#> $day
#> [1] 1
#> 
#> 
#> Slot "utc_offset_minutes":
#> [1] 0
#> 
#> Slot "timetable":
#>    ANNUAL SEASON timeslice     share weight
#>    <char> <char>    <char>     <num>  <num>
#> 1: ANNUAL    WIN       WIN 0.2465753      1
#> 2: ANNUAL    SPR       SPR 0.2520548      1
#> 3: ANNUAL    SUM       SUM 0.2520548      1
#> 4: ANNUAL    FAL       FAL 0.2493151      1
#> 
#> Slot "timeslice_share":
#>    timeslice     share weight
#>       <char>     <num>  <num>
#> 1:    ANNUAL 1.0000000      1
#> 2:       WIN 0.2465753      1
#> 3:       SPR 0.2520548      1
#> 4:       SUM 0.2520548      1
#> 5:       FAL 0.2493151      1
#> 
#> Slot "default_timeframe":
#> [1] "SEASON"
#> 
#> Slot "timeframe_rank":
#> ANNUAL SEASON 
#>      1      2 
#> 
#> Slot "timeslices_in_frame":
#> ANNUAL SEASON 
#>      1      4 
#> 
#> Slot "timeslice_family":
#>    parent  child
#>    <char> <char>
#> 1: ANNUAL    WIN
#> 2: ANNUAL    SPR
#> 3: ANNUAL    SUM
#> 4: ANNUAL    FAL
#> 
#> Slot "timeslice_ancestry":
#>    parent  child
#>    <char> <char>
#> 1: ANNUAL    WIN
#> 2: ANNUAL    SPR
#> 3: ANNUAL    SUM
#> 4: ANNUAL    FAL
#> 
#> Slot "next_in_timeframe":
#>    timeslice timeslicep
#>       <char>     <char>
#> 1:       WIN        SPR
#> 2:       SPR        SUM
#> 3:       SUM        FAL
#> 4:       FAL        WIN
#> 
#> Slot "next_in_year":
#>    timeslice timeslicep
#>       <char>     <char>
#> 1:       WIN        SPR
#> 2:       SPR        SUM
#> 3:       SUM        FAL
#> 4:       FAL        WIN
#> 
#> Slot "misc":
#> $pTimesliceWeight
#> NULL
#> 
#> 

# the hourly demand profile, and the four seasonal rates it becomes
dem <- getObject(light, name = "DEM_ELC", drop = TRUE)
head(dem@demand)
#>   cluster region year timeslice   demand
#> 1    <NA>    R01 2025  d001_h00 1.314840
#> 2    <NA>    R01 2025  d001_h01 1.309818
#> 3    <NA>    R01 2025  d001_h02 1.299658
#> 4    <NA>    R01 2025  d001_h03 1.322146
#> 5    <NA>    R01 2025  d001_h04 1.330137
#> 6    <NA>    R01 2025  d001_h05 1.338699
```
