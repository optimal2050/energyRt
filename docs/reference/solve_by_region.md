# Solve a model region by region

Solves a multi-region model as a series of sub-territory problems — one
per region by default, or one per group. Each run is stored as an
own-problem variant of a single scenario, so the whole set lives in one
folder and is listed by
[`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md).

The intended use is supply-curve evaluation, where each region is priced
against the rest of the system rather than co-optimised with it.

## Usage

``` r
solve_by_region(
  object,
  name = NULL,
  ...,
  regions = NULL,
  group = NULL,
  trade = c("none", "user", "auto"),
  boundary_prices = NULL,
  share = 0.1,
  nsteps = 3,
  price = NULL,
  spread = 0.5,
  solver = NULL,
  on_error = c("stop", "continue"),
  overwrite = FALSE,
  keep_scenarios = TRUE,
  verbose = TRUE
)
```

## Arguments

- object:

  a model object.

- name:

  character, the sequence name (one scenario folder).

- ...:

  passed to
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
  for every region.

- regions:

  character, the regions to solve; defaults to every region of the
  model, one at a time.

- group:

  a list of character vectors to solve groups of regions together
  instead of one at a time; overrides `regions`.

- trade:

  one of `"none"`, `"user"`, `"auto"` — see Details.

- boundary_prices:

  a `boundary_prices` data.frame for `trade = "user"`.

- share, nsteps, price, spread:

  passed to
  [`boundary_window()`](https://energyRt.org/reference/boundary_window.md)
  for `trade = "auto"`.

- solver:

  a solver option list; the scenario's default when `NULL`.

- on_error:

  `"stop"` (default) or `"continue"` — with `"continue"` a region that
  fails to solve is recorded and the run carries on, so one bad region
  does not lose the rest.

- overwrite:

  logical; `FALSE` (default) refuses to run when a variant of the same
  label already exists on disk with a different type or different
  parameters. `TRUE` skips that guard and writes into it.

- keep_scenarios:

  logical, keep each region's scenario object.

- verbose:

  logical.

## Value

a `by_region` object: `runs` (a tibble of what ran, with `boundary`
counting the boundary objects each region faces – a k-step price curve
contributes k), `scenarios`, `additive`, and `failed`.

## Details

`trade` decides what replaces the routes that leave a region:

- `"none"` (default) — routes are dropped and the region is solved in
  autarky. Because regional quantities are extensive and nothing
  crosses, the per-region objectives **add up** to the full model's.

- `"user"` — `boundary_prices` is passed through to
  [`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md).

- `"auto"` —
  [`boundary_window()`](https://energyRt.org/reference/boundary_window.md)
  builds a stepped import/export curve sized at `share` of the region's
  own demand in each timeslice. A reference `price` is required: the
  window can size the quantity from the model but cannot invent a price.

With `trade != "none"` each region faces an exterior the others cannot
see, so the objectives no longer sum to anything meaningful — the
result's `additive` field records this.

## See also

[`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md),
[`boundary_window()`](https://energyRt.org/reference/boundary_window.md),
[`solve_guided()`](https://energyRt.org/reference/solve_guided.md)

## Examples

``` r
if (FALSE) { # \dontrun{
open_project(tempdir())

ek  <- topia$modules$electricity$R3
mod <- newModel("TOPIA", data = ek$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = ek$regions,
                horizon = topia$modules$horizons$base,
                discount = 0.05)
mod <- setGeoscale(mod, ek$geoscale)

# one solve per region, each seeing priced boundary flows to its neighbours
res <- solve_by_region(mod, name = "BY_REGION", trade = "auto", price = 60,
                       solver = solver_options$glpk)
res$summary
} # }
```
