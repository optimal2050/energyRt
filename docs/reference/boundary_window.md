# A generic boundary trade window

Builds a `boundary_prices` table for
[`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md)
/
[`solve_by_region()`](https://energyRt.org/reference/solve_by_region.md)
sized from the model's own demand: each severed route is replaced by a
stepped import or export curve whose total quantity is `share` of the
region's demand in that timeslice.

The quantity is sized from the model's own demand; the price is taken
from `price` and is not estimated from the model. Inspect and edit the
returned data.frame before use, or supply your own.

## Usage

``` r
boundary_window(
  object,
  regions,
  share = 0.1,
  nsteps = 3,
  price = NULL,
  spread = 0.5,
  routes = NULL
)
```

## Arguments

- object:

  a model object.

- regions:

  character, the regions being solved (the sample). Routes are priced at
  whichever endpoint is inside this set.

- share:

  numeric in `(0, 1]`, the fraction of a region's demand the boundary
  may supply or absorb. `0.1` by default.

- nsteps:

  integer, steps in the price curve; `1` gives a flat price.

- price:

  numeric, the reference price. With `nsteps > 1` the curve spans
  `price * (1 +/- spread)`, rising for imports and falling for exports.

- spread:

  numeric, the half-width of the curve as a fraction of `price`.

- routes:

  optional `data.frame(src, dst)` limiting which routes get a window; by
  default every route leaving the sample.

## Value

a `data.frame` with the `boundary_prices` columns — `src`, `dst`,
`price`, `cap.up`, and the curve columns `nsteps`, `price_lo`,
`price_hi`.

## Details

The bound is applied **per timeslice** (`share * demand[r, y, s]`),
which is where `pImportRowUp` lives. Summed over the year that is
`share` of annual demand, so the familiar "trade is at most 10% of
consumption" reading holds without needing an annual budget — which
could not be expressed correctly anyway (a per-region reserve does not
exist, and a constraint RHS averages across regions).

## See also

[`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md),
[`solve_by_region()`](https://energyRt.org/reference/solve_by_region.md),
[`asImportCurve()`](https://energyRt.org/reference/supply-curve.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ek  <- topia$modules$electricity$R3
mod <- newModel("TOPIA", data = ek$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = ek$regions, horizon = topia$modules$horizons$base)
mod <- setGeoscale(mod, ek$geoscale)

# the priced import/export stubs a sub-territory solve needs at its edge:
# a share of the region's own demand, in `nsteps` price steps
boundary_window(mod, regions = c("W1", "W2"), price = 60, share = 0.1,
                nsteps = 3)
} # }
```
