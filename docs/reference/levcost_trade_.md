# Levelized cost of transmission

Called through [`levcost()`](https://energyRt.org/reference/levcost.md)
on a `trade` object. The metric is cost per unit ARRIVING in the
receiving region, summed over both endpoints' capital and fixed charges
– see the header of this file for why both endpoints pay.

## Usage

``` r
levcost_trade_(
  object,
  comm = NULL,
  route = NULL,
  utilisation = NULL,
  price = 0,
  repo = NULL,
  fuel_costs = NULL,
  discount = 0.05,
  base_year = NULL,
  horizon = NULL,
  solver = solver_options$glpk,
  method = c("auto", "analytic", "solve"),
  verbose = TRUE,
  ...
)
```

## Arguments

- object:

  A `trade` object.

- comm:

  Label for the priced commodity. Defaults to `@commodity`.

- route:

  The direction to price, as `c(src, dst)`. Defaults to the first row of
  `@routes`. Capacity is shared by both directions, so the full corridor
  capex is charged to the direction being priced.

- utilisation:

  Fraction of the corridor's capacity actually used, over the year.
  Defaults to `@trade$af.up` for the priced direction, then `1`.

- price:

  Price of the commodity in the sending region, per unit sent. Defaults
  to `0`, which makes the result the pure wheeling cost – the
  conventional reading of a transmission tariff, and deliberately unlike
  [`levcost_storage_()`](https://energyRt.org/reference/levcost_storage_.md),
  where the charging price is taken from the container because
  round-trip loss is intrinsic to what a store costs. Set it (or
  `fuel_costs`) to get the delivered cost instead.

- repo:

  Accepted for signature compatibility with the container path and
  otherwise unused: a corridor moves its own commodity rather than
  consuming a different one, so there is nothing in a repository for it
  to be priced against. Pass `price` or `fuel_costs` to charge for the
  energy sent.

- fuel_costs, discount, base_year, horizon, solver, method, verbose:

  As in [`levcost()`](https://energyRt.org/reference/levcost.md) for a
  technology.

- ...:

  Ignored, for signature compatibility with the technology method.

## Value

A `levcost` object – the same shape the technology method returns.

## Details

`@varom$varom` is priced, on the quantity SENT – it becomes
`pTradeIrVarom`, which `eqExportIrCost` charges at `src`.
`@varom$markup` is not priced: it is a transfer the importer pays and
the exporter receives, so it cancels system-wide and leaves the
objective unchanged.
