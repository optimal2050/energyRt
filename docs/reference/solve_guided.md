# Solve a model with guided perfect foresight

Reaches a full-horizon solution by solving cheap endpoint problems first
and using their capacity as targets for the expensive full solve. Unlike
[`solve_myopic()`](https://energyRt.org/reference/solve_myopic.md),
every subproblem sees the horizon's last milestone, so the transition is
disciplined by where it must arrive rather than by local optimality —
and the final stage is one ordinary full-horizon solve, whose objective
is therefore directly comparable to a true perfect-foresight one.

## Usage

``` r
solve_guided(
  object,
  name = NULL,
  ...,
  horizon = NULL,
  stages = c("endpoint", "pairs", "final"),
  slack = 0.2,
  basis = c("cap", "ncap"),
  mode = c("lo", "fx"),
  calendar_guide = NULL,
  direction = c("backward", "forward"),
  solver = NULL,
  on_infeasible = c("relax", "stop"),
  overwrite = FALSE,
  keep_scenarios = TRUE,
  verbose = TRUE
)
```

## Arguments

- object:

  a model object.

- name:

  character, the sequence name (a scenario folder).

- ...:

  passed to
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
  for every stage.

- horizon:

  a horizon; defaults to the model's own. At least two milestone years
  are required.

- stages:

  character, which stages to run, in order. Defaults to
  `c("endpoint", "pairs", "final")`; add `"base"` and/or `"transition"`.

- slack:

  numeric in `[0, 1]`, how far below a target the solver may go (the
  bound is `(1 - slack) * target`). `0.2` by default.

- basis:

  `"cap"` (default) or `"ncap"` — see Details.

- mode:

  `"lo"` (default) writes lower bounds; `"fx"` fixes capacity.

- calendar_guide:

  an optional reduced
  [`newCalendar()`](https://energyRt.org/reference/newCalendar.md) used
  for the guide stages (`base`, `endpoint`, `transition`, `pairs`) but
  never for `final`.

- direction:

  `"backward"` (default) or `"forward"`, for the `pairs` stage.

- solver:

  a solver option list; the scenario's default when `NULL`.

- on_infeasible:

  `"relax"` (default) retries a stage with progressively looser guidance
  before giving up; `"stop"` fails immediately.

- overwrite:

  logical; `FALSE` (default) refuses to run when a variant of the same
  label already exists on disk with a different type or different
  parameters. `TRUE` skips that guard and writes into it.

- keep_scenarios:

  logical, keep each stage's scenario object in the result.

- verbose:

  logical.

## Value

a `guided` object: `stages` (a tibble of what ran), `scenarios`,
`targets`, `ledger`, and `final` — the full-horizon solved scenario.

## Details

Stages, each stored as an own-problem variant of one scenario:

- `"base"` — solve the base year alone and write the result back as
  `stock`, so the past enters later stages as sunk capacity rather than
  being re-optimised.

- `"endpoint"` — solve `{base, last}`. Whatever survives from the base
  year stands; everything else is built at the last milestone. This is
  the end-state fleet. The base-year row is kept so discounting stays
  anchored there.

- `"transition"` — optionally carry the endpoint target across every
  milestone on a reduced `calendar_guide`, cheaply.

- `"pairs"` — refine each interior milestone against the endpoint, via
  [`guided_windows()`](https://energyRt.org/reference/guided_windows.md).

- `"final"` — the deliverable: the full horizon with every accumulated
  target imposed as a bound.

Targets bound the STANDING FLEET (`cap.lo`) by default. `basis = "ncap"`
bounds the build rate instead and divides by the period length, because
`ncap.*` bounds are multiplied by `pPeriodLen` in the equations while
the solved variable is already an annual rate.

## See also

[`solve_myopic()`](https://energyRt.org/reference/solve_myopic.md),
[`apply_targets()`](https://energyRt.org/reference/apply_targets.md),
[`guided_gap()`](https://energyRt.org/reference/guided_gap.md)

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

# the endpoint first, then each neighbouring pair, then the full horizon
# with the guidance as bounds
res <- solve_guided(mod, name = "GUIDED", slack = 0.2,
                    solver = solver_options$glpk)
sum(getData(res$final, "vObjective", merge = TRUE)$value)
} # }
```
