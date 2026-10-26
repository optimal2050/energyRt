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
  object,
  name = NULL,
  ...,
  base = NULL,
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
  parent_levels = getOption("energyRt.parent_levels", FALSE),
  fx_equ = getOption("energyRt.fx_equ", TRUE),
  on_missing_input = getOption("energyRt.on_missing_input", "stop"),
  verbose = isVerbose(),
  .reuse = NULL
)

# S4 method for class 'model'
interpolate(object, ...)
```

## Arguments

- object:

  as `mod`, for the method dispatched on a scenario.

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

- base:

  optional interpolated
  [scenario](https://energyRt.org/reference/class-scenario.md) to build
  on. The objects in `...` must be new to the model: constraints, costs,
  taxes and subsidies, new processes and commodities, or a `calendar` /
  geoscale that samples the base's. The result is the base plus what
  they add: the base's tables are reused, nothing is written into its
  store, and the result equals a full interpolation of the model with
  the base's additions and the new ones. The base's `fold`, `sparse` and
  `prune` settings apply. Otherwise (another model, a replaced object,
  settings or horizon objects) a message says why and the model is
  interpolated in full. `object` may be the base itself.

  New processes, commodities and other model objects are built on the
  base too: the per-object tables of the objects the base already holds
  are reused, the new objects' tables are computed, and every map is
  rebuilt.

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
  NA wildcards to shrink the data. `TRUE` folds `region` + `timeslice` +
  `year`; `FALSE` (default) folds nothing; a character vector selects
  dims among `region`, `timeslice`, `year`, `comm`, `tech`, `stg`,
  `trade`. The wildcards are substituted by an artificial set member in
  the written model files only; the scenario object keeps them and
  [`getData()`](https://energyRt.org/reference/getData.md) expands them.

  A dimension folds only where the value is uniform across its WHOLE
  set, so folding never changes the solution — it changes how the same
  numbers are stored. `year` matters most in weather-heavy models: one
  weather series repeated across milestone years is usually the largest
  parameter, and `region`/`timeslice` alone do not touch it. Measured on
  a 4-region `d365_h24` model over 7 milestones, adding `year` took
  `pWeather` from 245,259 to 35,037 rows (-85.7%) with a bit-identical
  objective on GLPK and Pyomo/HiGHS. A model whose weather genuinely
  differs by year simply does not fold.

- sparse:

  logical; the storage knob, and the user-facing switch between
  `drop_default_values()` and `expand_default_values()`. `TRUE` drops
  `value == defVal` rows; `FALSE` materialises the default over each
  parameter's full domain (the form GAMS needs) and then trims rows no
  map indexes. Folding applies to either.

- prune:

  logical; apply the per-parameter `misc$prune$value` filter via
  `drop_default_values()` - rows whose interpolated value equals that
  parameter's flagged value are dropped (lossless: an absent tuple reads
  as the default, which equals the dropped value). One parameter is
  flagged today (`pWeather`). Superseded by `sparse`'s `value =` form;
  retained for compatibility.

- validate:

  logical; run
  [`validate_scenario_parameters()`](https://energyRt.org/reference/validate_scenario_parameters.md)
  after interpolation (schema, duplicate keys, map/parameter coverage,
  missing governing constraints, calendar chronology).

- code:

  optional named list overriding solver source-code blocks (`GLPK`,
  `GAMS`, `JuMP`, `PYOMOConcrete`, ...), each either a script-file path
  or a character vector of lines. Lets a model-script version be
  supplied at interpolation time without rebuilding `sysdata` (handy to
  A/B templates).

- kvl:

  logical or `NULL`; build the Kirchhoff voltage-law cycle constraints
  for meshed trade networks. `NULL` (default) engages them when the
  network has cycles and reactances to use.

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

- parent_levels:

  logical or character; give a variable rows at every timeslice / region
  level COARSER than the one it occupies, each defined by a roll-up of
  its finer rows, so a constraint can be written at a level the variable
  does not natively carry (`summand@timeframe` / `@geoframe`). `FALSE`
  (default) builds none; `TRUE` builds them for every SUPPORTED
  variable; a character vector selects among those. Naming an
  unsupported variable is an error listing what is supported – a parent
  row with no equation to define it would be a free variable.

  `vOutTot` / `vInpTot` carry their parent rows unconditionally and are
  not affected: `eqOutTot` / `eqInpTot` are written against them.

  Defaults to `getOption("energyRt.parent_levels", FALSE)`, so a whole
  run or test suite can be switched in one place.

- fx_equ:

  logical; write a bound whose two sides coincide (`.fx`, or
  `.lo == .up`, with the same weather factors) as one equality row
  (`eq<Family>Fx`) instead of a pair of inequalities (`eq<Family>Lo` +
  `eq<Family>Up`). Applies to every bound family: technology
  availability (`af`, `afs`, `afc`), shares, capacity, new capacity and
  retirement of technologies, storages and trade, storage availability,
  charge, discharge, duration and part ratios, supply availability and
  reserve, trade flows and availability, import and export rows. The
  solution is the same; the model has fewer rows before presolve (-25%
  on an hourly full-year VRE model). `FALSE` writes the pairs. Defaults
  to `getOption("energyRt.fx_equ", TRUE)`.

- on_missing_input:

  `"stop"` or `"warn"`: what to do when a process with existing capacity
  (stock, or a lower / fixed capacity bound) is declared in a region
  where one of its input commodities has no source. `"stop"` names the
  cells; `"warn"` drops them, as is always done for processes that could
  only be built new. Defaults to
  `getOption("energyRt.on_missing_input", "stop")`.

- verbose:

  logical; print per-step progress. This also governs the
  variant-expansion report – how many process objects were expanded and
  how many constraints were generated for them. The generated
  constraints themselves are retrievable with
  `getObject(scen, class = "constraint")`; each carries a readable
  `desc` and, in `misc$.variant_source`, the object it was derived from.

- .reuse:

  internal: an interpolated scenario whose per-object tables are reused
  for the objects it holds (set by `base`).

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

## Examples

``` r
# \donttest{
ek  <- topia$modules$electricity$R3
mod <- newModel("TOPIA", data = ek$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = ek$regions,
                horizon = topia$modules$horizons$base,
                discount = 0.05)
mod <- setGeoscale(mod, ek$geoscale)

# build the solver-facing sets and parameters; nothing is solved yet
scen <- interpolate_model(mod, name = "BASE")
#> Cost declared at a coarse geoscale level:
#>   TBD_COA_WEST_CENTRAL@invcost: CENTRAL, WEST
#>   TBD_GAS_WEST_CENTRAL@invcost: CENTRAL, WEST
#> The cost is charged ONCE at that cell rather than at each child region, and is discounted at the children's rate. That is usually what a national cost datum means -- said here because it is a different number from naming the children individually.
scen
#> Name:  BASE 
#> Model:
#> Name:  TOPIA 

# `fold = TRUE` substitutes constant parameters into the equations, which
# shrinks what the backend has to read
model_size(interpolate_model(mod, name = "FOLDED", fold = TRUE))
#> Cost declared at a coarse geoscale level:
#>   TBD_COA_WEST_CENTRAL@invcost: CENTRAL, WEST
#>   TBD_GAS_WEST_CENTRAL@invcost: CENTRAL, WEST
#> The cost is charged ONCE at that cell rather than at each child region, and is discounted at the children's rate. That is usually what a national cost datum means -- said here because it is a different number from naming the children individually.
#> model_size: FOLDED
#>   parameters : 200 value, 359 maps, 15 sets
#>   param rows : 2,111  (fold step: 8,741 -> 2,111, saved 6,630 = 76%)
#>   folded dims: region=20  timeslice=11  year=19
#>   estimate   : ~61,849 variables, ~63,464 constraints (from gating maps)
#>   top parameters by rows:
#>     pWeather           753
#>     pDemand            384
#>     pTimesliceAgg      297
#>     pExportRow         288
#>     pTimesliceShare    101
#>     pTechStock         50
#>     pTechFixom         28
#>     pTechInvcost       28
#>     pTechEac           20
#>     pTechCinp2use      16
#>     pDiscountFactor    15
#>     pTechStockSurv     12
#>     pTradeIrVarom      8
#>     pTradeIrEff        8
#>     pSupCost           7
# }
```
