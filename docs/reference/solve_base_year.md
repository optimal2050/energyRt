# Solve the base year of a marked horizon

Solves the base year on its own, as an own-problem variant, and returns
the ledger of what it decided. This is the solve the projection does not
contain: with `base_year` marked, the projection's LP has no base-year
variables at all.

## Usage

``` r
solve_base_year(
  object,
  name = NULL,
  solver = NULL,
  optimize_retirement = FALSE,
  variant = "by",
  save = TRUE,
  ...,
  verbose = TRUE
)
```

## Arguments

- object:

  a model, or an interpolated scenario, whose horizon marks a
  `base_year`.

- name:

  scenario name; defaults to the object's own.

- solver:

  solver options, as
  [`solve_scenario()`](https://energyRt.org/reference/solve_model.md)
  takes.

- optimize_retirement:

  allow the base year to retire stock endogenously.

- variant:

  the variant label for the base-year run.

- save:

  store the solved scenario, so a projection can refer to it by name
  later. `TRUE` by default: a base-year solution nothing can reopen
  cannot be carried into a scenario built in another session.

- ...:

  passed to
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
  and
  [`solve_scenario()`](https://energyRt.org/reference/solve_model.md).

- verbose:

  logical.

## Value

A list with `scenario`, `ledger`, `horizon` and `base_year`.

## Details

Endogenous retirement is switched off for this solve unless
`optimize_retirement = TRUE`. A past year's retirements are observed,
not chosen, and a base year left free to retire its own fleet mis-dates
every vintage carried forward.

What crosses to the projection is **only what this solve decided** — the
ledger's `newcap`. The model's own `@stock`, which is where observed
history lives and which already carries its vintages, is left alone.

The LP is the base year alone. Milestones before it are history and are
solved here no more than they are in the projection; their data still
anchors interpolation.

## See also

[`solution_ledger()`](https://energyRt.org/reference/solution_ledger.md),
[`newHorizon()`](https://energyRt.org/reference/newHorizon.md),
[`horizon_windows()`](https://energyRt.org/reference/horizon_windows.md)

## Examples

``` r
if (FALSE) { # \dontrun{
h <- newHorizon(intervals = iv, base_year = 2025, name = "h25")
mod <- setHorizon(mod, h)
by <- solve_base_year(mod, solver = solver_options$glpk)
by$ledger
} # }
```
