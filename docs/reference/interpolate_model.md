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
  base_year_scenario = base,
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
  parent_levels = options::opt("parent_levels"),
  fx_equ = options::opt("fx_equ"),
  on_missing_input = options::opt("on_missing_input"),
  check = FALSE,
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

- base_year_scenario:

  where the base year's own solution comes from, when the horizon marks
  one (see
  [`newHorizon()`](https://energyRt.org/reference/newHorizon.md)).
  Defaults to `base`, so a projection built on a base takes that base's
  base-year solution for free. May also be a solved
  [scenario](https://energyRt.org/reference/class-scenario.md), the list
  [`solve_base_year()`](https://energyRt.org/reference/solve_base_year.md)
  returns, a scenario name, or `"<scenario>/<run>"`; `NULL` or `FALSE`
  carries nothing. What crosses is only what that solve DECIDED — its
  new capacity, written as sunk `stock` at its true vintage. The model's
  own `@stock` is left alone. With no source the projection still
  excludes the base year, and starts from the model's declared `@stock`;
  a message says so.

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

  Defaults to the `energyRt.parent_levels` option, so a whole run or
  test suite can be switched in one place.

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
  only be built new. Defaults to the `energyRt.on_missing_input` option.

- check:

  run
  [`check_feasibility()`](https://energyRt.org/reference/check_feasibility.md)
  on the interpolated scenario and STOP when it finds a contradiction,
  rather than returning a scenario that can only solve to INFEASIBLE.
  Off by default: the check costs a pass over the parameter tables, and
  a model known to be sound does not need it.

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
[`recast_model_calendar()`](https://energyRt.org/reference/recast_model_calendar.md),
[`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md),
[`with_solver_log()`](https://energyRt.org/reference/with_solver_log.md)

## Examples

``` r
# \donttest{
topia_elc <- topia$modules$electricity
mod <- newModel("TOPIA", data = topia_elc$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = topia_elc$regions,
                horizon = topia$modules$horizons$BY2025_50_by10,
                discount = 0.05)
mod <- setGeoscale(mod, topia_elc$geoscale)

# build the solver-facing sets and parameters; nothing is solved yet
scen <- interpolate_model(mod, name = "BASE")
#> 
#> ── interpolate_model: BASE ─────────────────────────────────────────────────────
#> storage: in-memory
#> sparse: TRUE
#> prune: TRUE
#> fold: none
#> validate: TRUE
#> base year 2025: not optimised, and no base-year solution was given -- the projection starts from the model's own @stock (base_year_scenario = carries one forward)
#> ℹ checking declarations
#> Warning: 8760 timeslice(s) named by 4 object(s) are outside the sampled calendar "s4_h24"; their rows are dropped.
#> Cost declared at a coarse geoscale level:
#>   TBD_COA_Z4_Z3@invcost: Z3, Z4
#>   TBD_COA_Z4_Z5@invcost: Z4, Z5
#>   TBD_GAS_Z2_Z3@invcost: Z2, Z3
#> The cost is charged ONCE at that cell rather than at each child region, and is discounted at the children's rate. That is usually what a national cost datum means -- said here because it is a different number from naming the children individually.
#> ✔ checking declarations (0.2s)
#> ℹ expanding vintage/cluster variants
#> Expanded 8 process objects into 33 variants (vintage/cluster).
#> ✔ expanding vintage/cluster variants (0.1s)
#> ℹ materializing commodity clusters
#> Materialized 3 commodity cluster(s): LANDW_CLHI, LANDW_CLLO, LANDW_CLMD
#> ✔ materializing commodity clusters (0.0s)
#> ℹ materializing weather clusters
#> Materialized 5 weather cluster(s): WSOL_CLFL, WSOL_CLTH, WWIN_CLHI, WWIN_CLMD, WWIN_CLLO
#> ✔ materializing weather clusters (0.1s)
#> ℹ materializing weather transforms
#> ✔ materializing weather transforms (0.4s)
#> ℹ collecting sets: object names
#> ✔ collecting sets: object names (0.1s)
#> ℹ collecting sets: process classes
#> ✔ collecting sets: process classes (0.0s)
#> ℹ collecting sets: timeframes
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ collecting sets: timeframes (0.1s)
#> ℹ collecting sets: process inputs/outputs
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ collecting sets: process inputs/outputs (0.0s)
#> ℹ collecting sets: membership comm sets
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ collecting sets: membership comm sets (0.0s)
#> ℹ collecting sets: process regions
#> ✔ collecting sets: process regions (0.0s)
#> ℹ building maps: membership + closure
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ building maps: membership + closure (0.4s)
#> ℹ collecting sets: invest/stock windows
#> ✔ collecting sets: invest/stock windows (0.2s)
#> ℹ settings parameters (discount, period length, timeslice share)
#> ✔ settings parameters (discount, period length, timeslice share) (0.1s)
#> ℹ building maps: calendar + lifespan
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ building maps: calendar + lifespan (0.3s)
#> ℹ ob2mi: interpolating 81 model objects
#> ✔ ob2mi: interpolating 81 model objects (1.1s)
#> ℹ interpolating parameters over milestone years
#> ✔ interpolating parameters over milestone years (3.0s)
#> ℹ calendar-filter: restricting parameters to declared timeslices
#> ✔ calendar-filter: restricting parameters to declared timeslices (0.1s)
#> ℹ computing equivalent annual costs (EAC)
#> ✔ computing equivalent annual costs (EAC) (0.4s)
#> ℹ building maps: value
#> ✔ building maps: value (0.2s)
#> ℹ building maps: filter
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ building maps: filter (1.1s)
#> ℹ building maps: constraint
#> Error: waf: the weather series has no value at the timeslices of the bound it multiplies (ESOL_VIN2015_CLFL/WSOL_CLFL, ESOL_VIN2030_CLFL/WSOL_CLFL, ESOL_VIN2045_CLFL/WSOL_CLFL, ESOL_VIN2015_CLTH/WSOL_CLTH, ESOL_VIN2030_CLTH/WSOL_CLTH). Give the weather the technology's timeframe.
scen
#> Error: object 'scen' not found

# `fold = TRUE` substitutes constant parameters into the equations, which
# shrinks what the backend has to read
model_size(interpolate_model(mod, name = "FOLDED", fold = TRUE))
#> 
#> ── interpolate_model: FOLDED ───────────────────────────────────────────────────
#> storage: in-memory
#> sparse: TRUE
#> prune: TRUE
#> fold: region, timeslice, year
#> validate: TRUE
#> base year 2025: not optimised, and no base-year solution was given -- the projection starts from the model's own @stock (base_year_scenario = carries one forward)
#> ℹ checking declarations
#> Warning: 8760 timeslice(s) named by 4 object(s) are outside the sampled calendar "s4_h24"; their rows are dropped.
#> Cost declared at a coarse geoscale level:
#>   TBD_COA_Z4_Z3@invcost: Z3, Z4
#>   TBD_COA_Z4_Z5@invcost: Z4, Z5
#>   TBD_GAS_Z2_Z3@invcost: Z2, Z3
#> The cost is charged ONCE at that cell rather than at each child region, and is discounted at the children's rate. That is usually what a national cost datum means -- said here because it is a different number from naming the children individually.
#> ✔ checking declarations (0.2s)
#> ℹ expanding vintage/cluster variants
#> Expanded 8 process objects into 33 variants (vintage/cluster).
#> ✔ expanding vintage/cluster variants (0.1s)
#> ℹ materializing commodity clusters
#> Materialized 3 commodity cluster(s): LANDW_CLHI, LANDW_CLLO, LANDW_CLMD
#> ✔ materializing commodity clusters (0.0s)
#> ℹ materializing weather clusters
#> Materialized 5 weather cluster(s): WSOL_CLFL, WSOL_CLTH, WWIN_CLHI, WWIN_CLMD, WWIN_CLLO
#> ✔ materializing weather clusters (0.1s)
#> ℹ materializing weather transforms
#> ✔ materializing weather transforms (0.4s)
#> ℹ collecting sets: object names
#> ✔ collecting sets: object names (0.1s)
#> ℹ collecting sets: process classes
#> ✔ collecting sets: process classes (0.0s)
#> ℹ collecting sets: timeframes
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ collecting sets: timeframes (0.1s)
#> ℹ collecting sets: process inputs/outputs
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ collecting sets: process inputs/outputs (0.0s)
#> ℹ collecting sets: membership comm sets
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ collecting sets: membership comm sets (0.0s)
#> ℹ collecting sets: process regions
#> ✔ collecting sets: process regions (0.0s)
#> ℹ building maps: membership + closure
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ building maps: membership + closure (0.3s)
#> ℹ collecting sets: invest/stock windows
#> ✔ collecting sets: invest/stock windows (0.2s)
#> ℹ settings parameters (discount, period length, timeslice share)
#> ✔ settings parameters (discount, period length, timeslice share) (0.2s)
#> ℹ building maps: calendar + lifespan
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ building maps: calendar + lifespan (0.3s)
#> ℹ ob2mi: interpolating 81 model objects
#> ✔ ob2mi: interpolating 81 model objects (1.1s)
#> ℹ interpolating parameters over milestone years
#> ✔ interpolating parameters over milestone years (3.3s)
#> ℹ calendar-filter: restricting parameters to declared timeslices
#> ✔ calendar-filter: restricting parameters to declared timeslices (0.1s)
#> ℹ computing equivalent annual costs (EAC)
#> ✔ computing equivalent annual costs (EAC) (0.1s)
#> ℹ building maps: value
#> ✔ building maps: value (0.2s)
#> ℹ building maps: filter
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> Processes with no input commodity (output is bounded, not fuelled): ESOL_VIN2015_CLFL, ESOL_VIN2015_CLTH, ESOL_VIN2030_CLFL, ESOL_VIN2030_CLTH, ESOL_VIN2045_CLFL, ESOL_VIN2045_CLTH, EWIN_VIN2015_CLHI, EWIN_VIN2015_CLLO, EWIN_VIN2015_CLMD, EWIN_VIN2030_CLHI, EWIN_VIN2030_CLLO, EWIN_VIN2030_CLMD, EWIN_VIN2045_CLHI, EWIN_VIN2045_CLLO, EWIN_VIN2045_CLMD
#> ✔ building maps: filter (1.1s)
#> ℹ building maps: constraint
#> Error: waf: the weather series has no value at the timeslices of the bound it multiplies (ESOL_VIN2015_CLFL/WSOL_CLFL, ESOL_VIN2030_CLFL/WSOL_CLFL, ESOL_VIN2045_CLFL/WSOL_CLFL, ESOL_VIN2015_CLTH/WSOL_CLTH, ESOL_VIN2030_CLTH/WSOL_CLTH). Give the weather the technology's timeframe.
# }
```
