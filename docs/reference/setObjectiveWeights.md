# Weight the objective by milestone year and by region

Sets how much each `(region, year)` cell of total system cost counts in
the objective. Two halves, declared separately and multiplied: `years`
writes `horizon@intervals$weight`, `regions` writes `config@objweight`,
and interpolation combines them into `pObjWeight[region, year]`.

## Usage

``` r
setObjectiveWeights(obj, ...)

# S4 method for class 'config'
setObjectiveWeights(obj, years = NULL, regions = NULL, reset = FALSE, ...)

# S4 method for class 'model'
setObjectiveWeights(obj, years = NULL, regions = NULL, reset = FALSE, ...)

# S4 method for class 'scenario'
setObjectiveWeights(obj, years = NULL, regions = NULL, reset = FALSE, ...)
```

## Arguments

- obj:

  a `model`, `config` or `scenario`.

- ...:

  unused, for method consistency.

- years:

  the year half: a named numeric over milestone years
  (`c("2035" = 0.5, "2045" = 0.5)`), an unnamed numeric of one weight
  per milestone, or `NULL` to leave it alone. `NA` or 1 means the
  milestone counts in full.

- regions:

  the region half: a named numeric over regions (`c(R1 = 1, R2 = 0.6)`),
  a `data.frame(region, year, weight)` for weights that differ by year
  as well, or `NULL` to leave it alone.

- reset:

  logical; `TRUE` clears the half (or halves) named by a `NULL` argument
  instead of leaving it alone. `setObjectiveWeights(mod, reset = TRUE)`
  removes every weight.

## Value

the object, with the weights written.

## Details

The usual reason to weight a year is uneven time resolution. A milestone
solved on a SAMPLE of the model calendar still carries full-year cost at
full period weight –
[`sample_calendar()`](https://energyRt.org/reference/sample_calendar.md)
annualises every sample – so a cheaply resolved interim year counts in
the objective exactly as much as a fully resolved one, and several of
them can dominate the sum. A weight below 1 says the estimate deserves
less trust.

The usual reason to weight a region is that the study does not value
every region's costs equally: differences in income level or purchasing
power, a region modelled only as context, or a priority stated
deliberately.

The weight scales the WHOLE cost cell – capital, fuel, variable cost,
levies, penalties. That is what makes it safe: when every cost term in a
cell scales by one factor, no trade-off inside the cell changes, so the
cell's dispatch is what it would have been unweighted and only its share
of the cross-cell investment decision moves. Weighting operating cost
alone would shift the merit order inside the down-weighted cell toward
fuel-burning plant. What the weight does change is capital: a
down-weighted year's capital is charged less, which nudges investment
earlier.

Weights are not interpolated. A cell no declaration names counts in
full.

After a solve,
[`objective_weights()`](https://energyRt.org/reference/objective_weights.md)
reports the weighted objective beside the same sum recomputed without
the weights, so the two can be compared without solving twice.

## See also

[`objective_weights()`](https://energyRt.org/reference/objective_weights.md)
to read them back after a solve,
[`sample_calendar()`](https://energyRt.org/reference/sample_calendar.md)
and [`setCalendar()`](https://energyRt.org/reference/setCalendar.md) for
the uneven-resolution case the year half exists for.

## Examples

``` r
mod <- newModel("w", horizon = newHorizon(2030:2049, intervals = 5),
                region = c("R1", "R2"))
mid <- getHorizon(mod)@intervals$mid
# the interim milestones are solved on a sample, so trust them half
w <- stats::setNames(rep(0.5, length(mid) - 2L), mid[-c(1, length(mid))])
mod <- setObjectiveWeights(mod, years = w)
#> Error: `years` is unnamed, so it must give one weight per milestone (2) or a single weight for all; got 0.
#>   Name it by year instead to weight some milestones only.
getHorizon(mod)@intervals[, c("mid", "weight")]
#> Error in `[.data.table`(getHorizon(mod)@intervals, , c("mid", "weight")): column not found: [weight]
# and R2's costs count for 60%
mod <- setObjectiveWeights(mod, regions = c(R2 = 0.6))
mod@config@objweight
#>   region year weight
#> 1     R2   NA    0.6
```
