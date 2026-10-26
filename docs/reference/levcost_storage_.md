# Levelized cost of storage

Called through [`levcost()`](https://energyRt.org/reference/levcost.md)
on a `storage` object. See `vignette("levcost")` for the metric; the
header of this file for the algebra.

## Usage

``` r
levcost_storage_(
  object,
  comm = NULL,
  cycles = 365,
  price = NULL,
  repo = NULL,
  fuel_costs = NULL,
  discount = 0.05,
  base_year = NULL,
  horizon = NULL,
  region = NULL,
  solver = solver_options$glpk,
  method = c("auto", "analytic", "solve"),
  verbose = TRUE,
  ...
)
```

## Arguments

- object:

  A `storage` object.

- comm:

  Commodity to price the output in. Defaults to the discharged
  commodity.

- cycles:

  Full charge/discharge cycles per year. A store's levelized cost is
  inversely proportional to how hard it is worked, and no slot carries
  that, so it is an argument. `365` is one cycle a day.

- price:

  Price of the charging commodity, per unit. Taken from `fuel_costs`,
  then from a matching `supply` in `repo`, then `0`.

- repo, fuel_costs, discount, base_year, horizon, region, solver,
  method, verbose:

  As in [`levcost()`](https://energyRt.org/reference/levcost.md) for a
  technology.

- ...:

  Ignored, for signature compatibility with the technology method.

## Value

A `levcost` object – the same shape the technology method returns.
