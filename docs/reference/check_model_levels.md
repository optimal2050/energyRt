# Check declared resolutions against the commodities they serve

The rule
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
enforces, reported instead of raised. Six classes – `demand`, `supply`,
`import`, `export`, `trade`, `storage` – have no aggregation path
between a flow and its commodity, so a region or a timeslice declared at
a different level than the one the commodity balances at is a cell that
is written and never read: the term silently evaluates to zero.
Interpolation stops at the first call that finds any; this collects
every one, on a model that has not been interpolated yet.

## Usage

``` r
check_model_levels(object)
```

## Arguments

- object:

  a [model](https://energyRt.org/reference/class-model.md) or a
  `scenario`.

## Value

A `data.frame` with one row per object, commodity and axis, holding the
declared and the expected members; no rows when every declaration
matches. Printed as a short verdict.

## Details

Also reported, which the assertion cannot see from inside interpolation:
a commodity naming a `@geoframe` the model's geoscale does not have, or
a `@timeframe` its calendar does not have.

The coarse direction stays exempt exactly as at interpolation – for a
commodity the object only consumes or carries as `@aux`, and only where
the class has an aggregation path.

## See also

[`check_geoscale_regions()`](https://energyRt.org/reference/check_geoscale_regions.md),
[`check_feasibility()`](https://energyRt.org/reference/check_feasibility.md),
[`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md)

Other validation:
[`verify_aggregation()`](https://energyRt.org/reference/verify_aggregation.md)

## Examples

``` r
repo <- newRepository(
  "mini",
  newCommodity("ELC", unit = "MWh"),
  newDemand("DEM", commodity = "ELC",
            demand = data.frame(region = "R1", year = 2020, demand = 10))
)
mod <- newModel("MINI", data = repo, calendar = calendars$annual,
                region = "R1", horizon = horizons$Y2020_2060_by_5)
check_model_levels(mod)
#> Levels: every declaration matches its commodity.
```
