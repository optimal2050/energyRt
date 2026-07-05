# Compare interpolation settings (size & build time) for a model

Interpolates `mod` in memory under each of several setting combinations
and tabulates how big the resulting model is: total value-parameter
rows, the parameter / map / set counts, the variable & constraint
estimate (from
[`model_size()`](https://energyRt.org/reference/model_size.md)), the
in-memory size, and the build time. A quick way to see how `fold` /
`sparse` / `prune` trade off model size and speed before committing to a
solve.

## Usage

``` r
compare_interp_settings(
  mod,
  settings = NULL,
  ...,
  name = "cmp",
  verbose = FALSE,
  top_n = 12L,
  keep_scen = FALSE
)
```

## Arguments

- mod:

  a `model`.

- settings:

  a *named* list of setting combinations; each element is a list of
  arguments forwarded to
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md),
  e.g. `list(fold = TRUE, sparse = TRUE, prune = TRUE)`. When `NULL` a
  default grid (dense / sparse / sparse+prune / fold+sparse+prune /
  fold-all) is used.

- ...:

  arguments forwarded to EVERY `interp_mod()` call, e.g.
  `horizon = newHorizon(period = 2024)`.

- name:

  base scenario name; each build is `"<name>_<setting>"`.

- verbose:

  forwarded to
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
  (per-build progress).

- top_n:

  number of largest parameters to keep per build.

- keep_scen:

  if `TRUE`, the interpolated scenarios are returned in `$scen` (handy
  to feed a later solve step). Off by default to save memory.

## Value

an object of class `interp_settings_cmp`: a list with

- `summary` — one row per setting (ok, seconds, mb, param_rows, counts,
  variable/constraint estimates, error),

- `top` — wide data.frame of the largest parameters' row counts per
  setting,

- `details` — named list of per-build
  [`model_size()`](https://energyRt.org/reference/model_size.md)
  objects,

- `scen` — named list of scenarios when `keep_scen = TRUE`,

- `settings` — the settings grid used.

## Details

All builds use `ondisk = FALSE` (so sizes are measured in memory and are
directly comparable) and `overwrite = TRUE`.

## See also

[`model_size()`](https://energyRt.org/reference/model_size.md),
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)

## Examples

``` r
if (FALSE) { # \dontrun{
cmp <- compare_interp_settings(mod, horizon = newHorizon(period = 2024))
cmp                       # prints the comparison table
# custom grid:
compare_interp_settings(mod,
  settings = list(
    none    = list(fold = FALSE, sparse = FALSE, prune = FALSE),
    all     = list(fold = c("region","slice","year","comm","tech","stg","trade"),
                   sparse = TRUE, prune = TRUE)),
  horizon = newHorizon(period = 2024))
} # }
```
