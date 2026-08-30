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
  kvl = FALSE,
  boundary_prices = NULL,
  .prefilter = FALSE,
  verbose = isVerbose()
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
  NA wildcards to shrink the data. `TRUE` folds `region` + `timeslice`;
  `FALSE` (default) folds nothing; a character vector selects dims among
  `region`, `timeslice`, `year`, `comm`, `tech`, `stg`, `trade`. A
  folded scenario is expanded to solver-ready form at solve time.

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

- boundary_prices:

  optional `data.frame` pricing import/export stubs for trade routes
  dropped by a SPATIAL SAMPLE (a
  [`geoscales::filter_geoscale()`](https://optimal2050.github.io/geoscales/r/reference/filter_geoscale.html)
  subset passed via `...`); see
  [`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md)
  for the columns. Ignored (with a warning) when no sampled geoscale is
  supplied.

- .prefilter:

  EXPERIMENTAL, default `FALSE`. Restrict each model object's data to
  the timeslices and regions the SCENARIO declares before interpolating
  it, rather than interpolating everything and discarding the excess
  afterwards.

  On the sampled-calendar recipe – a full-year model with a subset
  calendar handed to this function – the default order expands all 8,760
  timeslices in `ob2mi` and then keeps 96. The parameter filters that
  follow are cheap (0.05s on a 5-node model); the cost is the
  interpolation they cannot undo.

  Off by default because it is not obviously safe: anything deriving a
  relation from the full declared grid rather than from the calendar
  would see a narrower input. Compare objectives before relying on it.

- verbose:

  logical; print per-step progress. This also governs the
  variant-expansion report – how many process objects were expanded and
  how many constraints were generated for them. The generated
  constraints themselves are retrievable with
  `getObject(scen, class = "constraint")`; each carries a readable
  `desc` and, in `misc$.variant_source`, the object it was derived from.

## Value

an interpolated
[scenario](https://energyRt.org/reference/class-scenario.md) object.

## See also

[`solve_model()`](https://energyRt.org/reference/solve_model.md),
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md), the
`interpolate` S4 method.

Other interpolation:
[`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md),
[`with_solver_log()`](https://energyRt.org/reference/with_solver_log.md)
