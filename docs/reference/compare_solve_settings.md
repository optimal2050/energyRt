# Compare interpolation settings AND solvers for a model

Interpolates `mod` under each setting combination (via
[`compare_interp_settings()`](https://energyRt.org/reference/compare_interp_settings.md))
and then solves every build with each supplied solver option, tabulating
the objective, solve time and status. Because the solution must not
depend on storage settings, the objective should be identical across
`fold` / `sparse` / `prune` for a given solver – the print method flags
any per-solver disagreement.

## Usage

``` r
compare_solve_settings(
  mod,
  settings = NULL,
  solvers,
  ...,
  name = "cmp",
  verbose = FALSE,
  tmp.dir = NULL
)
```

## Arguments

- mod:

  a `model`.

- settings:

  a named list of `interp_mod()` setting combinations (see
  [`compare_interp_settings()`](https://energyRt.org/reference/compare_interp_settings.md));
  `NULL` uses the default grid.

- solvers:

  a list of solver-option objects, e.g.
  `list(solver_options$glpk, solver_options$julia_highs)`. Names are
  taken from the list names when present, otherwise from each option's
  `$name`.

- ...:

  forwarded to every `interp_mod()` build (e.g.
  `horizon = newHorizon(period = 2024)`).

- name:

  base scenario name.

- verbose:

  forwarded to `interp_mod()`.

- tmp.dir:

  solver working directory (passed to
  [`solve_scen()`](https://energyRt.org/reference/solve_mod.md)); `NULL`
  lets the solver pick one.

## Value

an object of class `solve_settings_cmp`: `summary` (one row per setting
x solver: ok, seconds, objective, error), `interp` (the interp-size
table), plus `top` / `details` from the interpolation comparison.

## See also

[`compare_interp_settings()`](https://energyRt.org/reference/compare_interp_settings.md),
[`solve_scen()`](https://energyRt.org/reference/solve_mod.md),
[`model_size()`](https://energyRt.org/reference/model_size.md)

## Examples

``` r
if (FALSE) { # \dontrun{
compare_solve_settings(mod,
  solvers = list(solver_options$glpk),
  horizon = newHorizon(period = 2024))
} # }
```
