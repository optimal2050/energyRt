# Impose capacity targets on a model

Writes capacity targets from
[`solution_targets()`](https://energyRt.org/reference/solution_targets.md)
into a model's process `@capacity` slots as bounds, so a later solve is
steered toward a guiding run's answer without being pinned to it. The
target counterpart of
[`apply_ledger()`](https://energyRt.org/reference/apply_ledger.md).

## Usage

``` r
apply_targets(
  object,
  targets,
  horizon,
  slack = 0.2,
  mode = c("lo", "fx"),
  basis = c("cap", "ncap"),
  digits = 6,
  tolerance = 1e-06,
  verbose = FALSE
)
```

## Arguments

- object:

  a model object.

- targets:

  a `capacity_targets` tibble from
  [`solution_targets()`](https://energyRt.org/reference/solution_targets.md).

- horizon:

  the horizon of the solve the targets are being written for; used to
  validate the target years.

- slack:

  numeric in `[0, 1]`, the fraction by which a target is relaxed: the
  bound is `(1 - slack) * value`. `slack = 1` disables the guidance.

- mode:

  `"lo"` (default) writes a lower bound, `"fx"` fixes the capacity
  outright.

- basis:

  `"cap"` (default) bounds the standing fleet; `"ncap"` bounds the
  new-build rate and divides by the period length.

- digits:

  integer, decimal places kept when rounding a target down.

- tolerance:

  numeric, targets below this are dropped rather than written as a zero
  bound.

- verbose:

  logical.

## Value

the model, with `@capacity` bound columns written.

## Details

Targets are written at exactly the years present in `targets` — they are
NOT densified across the horizon, because a bound is forward-filled to
the end of the horizon by `inter.forth` interpolation and a fabricated
intermediate value would bind years the guiding run never examined. A
target set whose last year precedes the horizon's last milestone warns
for the same reason.

Every imposed value is rounded down, so a target cannot exceed what
another constraint permits and turn a guide into an infeasibility.

## See also

[`solution_targets()`](https://energyRt.org/reference/solution_targets.md),
[`solve_guided()`](https://energyRt.org/reference/solve_guided.md),
[`apply_ledger()`](https://energyRt.org/reference/apply_ledger.md)

## Examples

``` r
if (FALSE) { # \dontrun{
open_project(tempdir())

ek   <- topia$modules$electricity$R3
mod  <- newModel("TOPIA", data = ek$repo,
                 calendar = topia$modules$calendars$s4_h24,
                 region = ek$regions,
                 horizon = topia$modules$horizons$base,
                 discount = 0.05)
mod <- setGeoscale(mod, ek$geoscale)
scen <- solve_model(mod, name = "BASE", solver = solver_options$glpk)

# steer a second solve toward the first without pinning it
tg   <- solution_targets(scen)
mod2 <- apply_targets(mod, tg, horizon = getHorizon(mod), slack = 0.2)
scen2 <- solve_model(mod2, name = "STEERED", solver = solver_options$glpk)
} # }
```
