# Interpolate a model into a solver-ready scenario

Builds an interpolated
[scenario](https://energyRt.org/reference/class-scenario.md) from a
[model](https://energyRt.org/reference/class-model.md) via the mapping
pipeline: collects sets from the model objects, builds the membership /
calendar / lifespan / value / constraint / cost mappings, extracts and
interpolates the numeric parameters over the milestone years, and
(optionally) folds, prunes and validates the result. The returned
scenario is ready for
[`solve_model()`](https://energyRt.org/reference/solve_model.md) /
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md).

## Usage

``` r
interpolate_model(
  mod,
  name = NULL,
  ...,
  desc = NULL,
  ondisk = FALSE,
  overwrite = FALSE,
  fold = FALSE,
  sparse = TRUE,
  prune = TRUE,
  validate = TRUE,
  code = NULL,
  verbose = getOption("energyRt.verbose", FALSE)
)
```

## Arguments

- mod:

  a [model](https://energyRt.org/reference/class-model.md) object, or a
  [scenario](https://energyRt.org/reference/class-scenario.md) (its
  `@model` is re-interpolated).

- name:

  character scenario name. If `NULL`, a default `scen_<model>` is used
  (with a warning).

- ...:

  additional energyRt objects folded into the model BEFORE the pipeline
  runs: `settings`, `config`, `calendar`, `horizon`, a whole
  `repository`, or individual model "bricks" (`technology`, `commodity`,
  `storage`, ...). This is how a scenario overrides or extends the model
  (e.g. pass a sampled `calendar` to interpolate on a reduced time
  resolution).

- desc:

  character scenario description.

- ondisk:

  logical; store each parameter's data in the on-disk parameter store
  rather than the in-memory `@data` slot. `FALSE` (default) keeps data
  in memory, which the solver writers read directly; `TRUE` suits very
  large models (data is materialised back to memory at solve time).

- overwrite:

  logical; overwrite an existing on-disk scenario of the same name.

- fold:

  logical or character; whole-column "fold" of trimmable dimensions to
  NA wildcards to shrink the data. `TRUE` folds `region` + `slice`;
  `FALSE` (default) folds nothing; a character vector selects dims among
  `region`, `slice`, `year`, `comm`, `tech`, `stg`, `trade`. A folded
  scenario is expanded to solver-ready form at solve time.

- sparse:

  logical; the storage knob. `TRUE` drops `value == defVal` rows (and
  folds); `FALSE` materialises the default over each parameter's full
  domain (and unfolds).

- prune:

  logical; drop interpolated rows that fall outside the equation-domain
  maps (no effect on the solution, smaller data).

- validate:

  logical; run post-interpolation consistency checks (schema, duplicate
  keys, map/parameter coverage).

- code:

  optional named list overriding solver source-code blocks (`GLPK`,
  `GAMS`, `JuMP`, `PYOMOConcrete`, ...), each either a script-file path
  or a character vector of lines. Lets a model-script version be
  supplied at interpolation time without rebuilding `sysdata` (handy to
  A/B templates).

- verbose:

  logical; print per-step progress.

## Value

an interpolated
[scenario](https://energyRt.org/reference/class-scenario.md) object.

## See also

[`solve_model()`](https://energyRt.org/reference/solve_model.md),
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md), the
`interpolate` S4 method.
