# Restrict a model to a region subset

The spatial mirror of interpolating on a sampled calendar, usable up
front: declares only `region`, drops trade routes that cross the subset
boundary (with a message), and – when `boundary_prices` supplies a price
for a dropped route – replaces it with an import/export stub at the kept
endpoint. When the model carries a geoscale, it is filtered in step
(recording coverage; see
[`geoscales::geoscale_coverage()`](https://optimal2050.github.io/geoscales/r/reference/geoscale_coverage.html)).

## Usage

``` r
subset_model_regions(
  object,
  region,
  boundary_prices = NULL,
  verbose = isVerbose()
)
```

## Arguments

- object:

  a [model](https://energyRt.org/reference/class-model.md).

- region:

  character vector of regions to keep (must be a subset of the model's
  declared regions).

- boundary_prices:

  optional `data.frame` pricing the boundary stubs of dropped routes,
  with columns `src`, `dst`, `price`, and optionally `trade` (object
  name; `NA` = any), `year`, `timeslice` (both `NA` = all), and `cap.up`
  (explicit stub bound overriding the derivation from the route's
  `ava.*` rows). Routes without a price row are dropped without a stub.

- verbose:

  print per-object messages.

## Value

the restricted [model](https://energyRt.org/reference/class-model.md).

## Details

The same behaviour is available without this verb by passing a
[`geoscales::filter_geoscale()`](https://optimal2050.github.io/geoscales/r/reference/filter_geoscale.html)
subset to
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md).

## See also

Other interpolation:
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md),
[`with_solver_log()`](https://energyRt.org/reference/with_solver_log.md)

## Examples

``` r
repo <- newRepository(
  "mini",
  newCommodity("COA", unit = "MMBtu"),
  newCommodity("ELC", unit = "MWh", timeframe = "ANNUAL"),
  newSupply("SUP_COA", commodity = "COA", supply = data.frame(cost = 1)),
  newTechnology("ECOA", input = list(comm = "COA"), output = list(comm = "ELC"),
                cap2act = 1, invcost = list(invcost = 100),
                olife = list(olife = 30)),
  newDemand("DEM", commodity = "ELC",
            demand = data.frame(region = c("W1", "W2"), year = 2020, demand = 10))
)
mod <- newModel("MINI", data = repo, calendar = calendars$annual,
                region = c("W1", "W2"), horizon = horizons$Y2020_2060_by_5)

# keep one region; demand, capacity and routes outside it are dropped
sub <- subset_model_regions(mod, "W1")
get_region(sub)
#> [1] "W1"
```
