# Changelog

## energyRt 0.90.0 (pre-release)

The first release since `0.50-beta`. It consolidates the 0.60, 0.70 and
0.80 development cycles, none of which were released, and is the
**pre-release for 1.0**: the modelling API is the one 1.0 will ship, and
the work left is testing, debugging and visualisation rather than new
structure.

Upgrading from 0.50 is not a drop-in. The time dimension, the `storage`
class, the variant system and the geography layer all changed shape —
read *Breaking changes* first. The change-by-change log for the whole
cycle (450 entries) is in
[dev/NEWS-0.6-0.9-detail.md](https://energyRt.org/news/dev/NEWS-0.6-0.9-detail.md).

### License

- energyRt is relicensed from AGPL-3 to **Apache-2.0**. Releases up to
  and including v0.89 remain available under AGPL-3.

### Breaking changes

- **The time dimension is `timeslice`** everywhere (was `slice`),
  matching TIMES and OSeMOSYS, and a **`calendar` is required** wherever
  a timeslice layout is needed. Timeslice label conversion moved to the
  sibling **timescales** package, so
  [`tsl2hour()`](https://optimal2050.github.io/timescales/r/reference/timeslice_conversions.html),
  `tsl2year()`, `tsl_guess_format()` and `tsl_formats` are gone.
  `calendars` ships generic designs (`m12`, `q4`, `d365_h24`, sampled
  variants); model-specific ones live with their models.

- **A storage is three parts.** Charger, reservoir and discharger each
  have their own capacity variable, costs and availability:
  `@input`/`@output`/ `@storage` declare the roles,
  `vStorageOutCap`/`vStorageInpCap`/ `vStorageStgCap` size them,
  `@duration` and `@inp2out`/`@inp2stg` tie them together, and
  `vStorageStore` is now `vStorageLevel`. Charge and discharge **rates
  are bounded by the converters**: `inp.af.up`/`out.af.up` default to 1,
  so a flow per timeslice cannot exceed `capacity * cap2act * share`.

- **Vintages and clusters are one mechanism.** `@vintage` holds build
  years (with the investment window merged in from
  `@start`/`@end`/`@olife`) and `@cluster` holds parallel parts; both
  must be **declared** before a label may be used in a data slot, and
  interpolation expands them into ordinary processes named
  `EWIN_VIN2030_CL01`. Read coordinates from
  [`getVariants()`](https://energyRt.org/reference/getVariants.md),
  never by parsing a name.

- **Geography is multi-level.** `@geolevel` is `@geoframe` on
  commodities and summands, a model carries a `Geoscale` (from the
  sibling **geoscales** package), and a commodity or process may sit at
  a coarser level than the flows it touches.

- **Taxes and subsidies are indexed by the instrument**, not by the
  commodity, and can be levied on a process. `newConstraintS()` is
  removed — it failed for every input it was given; declare a
  `constraint` or a levy instead.

- **Auxiliary flows sit at the aux commodity’s timeframe**: an annual
  emission of an hourly plant is one variable and one equation per year,
  not 8760. The storage aux couplings are part-prefixed (`inp.cap2ainp`,
  `stg.ncap2aout`, …); the bare names silently applied to the discharger
  only.

- **The API is verb-first and prefixed `en`**: scenario management reads
  [`save_scenario()`](https://energyRt.org/reference/save_scenario.md) /
  [`load_scenario()`](https://energyRt.org/reference/load_scenario.md) /
  [`drop_scenario_run()`](https://energyRt.org/reference/scenario_runs.md),
  the registry class is `en_registry`, and 46 machinery helpers are no
  longer exported. Object names are validated at construction. The
  0.5-era deprecation layer is **gone**: `storage@charge`/`@inflow` and
  the `slice` column name are refused with an error naming the new
  spelling, and the un-prefixed solver path environment variables and
  `options(energyRt.verbose)` are no longer read.

- **The Julia and Python backends exchange Arrow files**, which needs
  `Arrow.jl` and `pyarrow`; `arrow_format` split into `storage_format`
  and `exchange_format`. A run’s solver files are written into
  `runs/<solve>/`.

- **The teaching model is TOPIA** (was UTOPIA), its regions are
  `W1`/`W2`, `C1`–`C6`, `E1`–`E3`, and its four standalone datasets are
  folded into `topia`. The test tier `nightly` is renamed `full`.

- **The first argument is `object`** wherever a function takes an
  energyRt object: 66 functions that said `obj`, `mod` or `scen` now say
  `object` ([`getData()`](https://energyRt.org/reference/getData.md),
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md),
  [`solve_model()`](https://energyRt.org/reference/solve_model.md) and
  the solve drivers, the scenario-store verbs, `get_process_*()`, …).
  Positional calls are unaffected; a call naming the argument
  (`getData(scen = s)`) must be updated. `x` stays where the argument is
  data rather than a model object, and generics defined elsewhere
  ([`summary()`](https://rdrr.io/r/base/summary.html),
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
  [`update()`](https://energyRt.org/reference/update.md)) keep their
  formals.

- `storage@cap2stg` is refused rather than renamed: it became
  `@duration` in 0.80, and the compatibility shim is gone. The error
  names the replacement. `storage@charge` and `@inflow` are refused the
  same way, and the error says why it is not a pure rename:
  `@startLevel` is added once per cycle at the first timeslice, where
  `@charge` added its value at every timeslice given. For a
  weather-driven inflow profile, feed the storage from a `supply`.

- `newSub()` is removed —
  [`newSubsidy()`](https://energyRt.org/reference/newSubsidy.md) is the
  constructor. They were the same function, and
  [`newTax()`](https://energyRt.org/reference/newTax.md) never had a
  short alias.

### New features

#### Model building

- **Resource tiers in one object.** A `commodity` or a `weather` object
  declaring `@cluster` becomes one object per tier at interpolation,
  each with its own balance, so graded land or a graded wind resource
  cannot pool. A consumer picks its tier by cluster name; an unclustered
  object is untouched.
- **Weather transforms**: one shipped series feeds many derived ones,
  and a weather object may be declared at a coarser region than the
  processes reading it — one stored profile for a whole country.
- **Transmission**:
  [`newACLine()`](https://energyRt.org/reference/newACLine.md) and
  [`newDCLink()`](https://energyRt.org/reference/newDCLink.md), an
  opt-in Kirchhoff voltage law, quadratic losses approximated by
  capacity tranches
  ([`lossTranches()`](https://energyRt.org/reference/lossTranches.md)),
  and `@trade$af` to rate a route’s flow.
- **Supply curves**:
  [`asSupplyCurve()`](https://energyRt.org/reference/supply-curve.md),
  [`asImportCurve()`](https://energyRt.org/reference/supply-curve.md),
  [`asExportCurve()`](https://energyRt.org/reference/supply-curve.md)
  turn a single price into a stepped curve with its own roster.
- `commodity@property` carries physical properties — heating values,
  densities, carbon content — as a tidy table rather than scattered
  coefficients.
- Discount rates may be declared at any geoscale level, and a WACC per
  technology is separate from the social discount rate, with `payback`
  as the cost-recovery life on every backend.
- Process definitions are portable:
  [`process_to_spec()`](https://energyRt.org/reference/process_to_spec.md)
  /
  [`process_from_spec()`](https://energyRt.org/reference/process_from_spec.md)
  (YAML/JSON), an interactive
  [`process_designer()`](https://energyRt.org/reference/process_designer.md),
  and Excel round-trip with
  [`read_technology_xlsx()`](https://energyRt.org/reference/read_technology_xlsx.md)
  /
  [`write_technology_xlsx()`](https://energyRt.org/reference/write_technology_xlsx.md).

#### Time and space

- Calendars gained `year_start` and `utc_offset_minutes`, milestone
  years can carry display labels, and sampled calendars (one
  representative day per month, `d365_h24_1dps`) are first-class — a
  full model solved on a subset of slices.
- **Region aggregation with clustering**:
  `aggregate_model_regions(clusters = )` partitions a corridor family
  into `k` groups,
  [`get_process_groups()`](https://energyRt.org/reference/get_process_groups.md)
  reports what may be merged,
  [`process_cluster_sweep()`](https://energyRt.org/reference/process_cluster_sweep.md)
  sweeps the admissible range and
  [`model_clusters()`](https://energyRt.org/reference/model_clusters.md)
  reports what an aggregation did.
- A filtered geoscale passed to
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
  solves a sub-territory, and
  [`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md)
  warns when sampling splits a cell a process spans.
- **`@geoframe` on the region-carrying classes** (`weather`, `supply`,
  `demand`, `technology`, `storage`, `import`, `export`, `tax`,
  `subsidy`, `trade`) declares the geoscale level an object’s regions
  are expressed at. Optional: left unset the level is derived from the
  region codes, as before. Declared, it scopes an object that names no
  regions of its own to that level’s regions, and every code it does
  name must be a member of the level — `geoframe = "nation"` beside
  `region = "R1"` is now an error instead of quietly meaning something
  else. It adds that one default and those checks, nothing more: an
  object may still be coarser only than the commodities it consumes,
  never than those it produces or stores, enforced where it always was.

#### Solving

- **Solve drivers**:
  [`solve_myopic()`](https://energyRt.org/reference/solve_myopic.md)
  (window by window, with a solution ledger),
  [`solve_by_sample()`](https://energyRt.org/reference/solve_by_sample.md),
  [`solve_by_region()`](https://energyRt.org/reference/solve_by_region.md)
  and [`solve_guided()`](https://energyRt.org/reference/solve_guided.md)
  (target-seeking), each with its primitives exported.
- **GPU and first-order LP**: presets for cuOpt (`julia_cuopt`,
  `mm_cuopt_wsl`) and HiGHS PDLP/HiPDLP, plus `lang = "mm"`, which hands
  the matrix to **multimod** and solves it outside the templates.
- `solver_options$pyomo_mps` writes the problem as MPS, and
  [`read_solution()`](https://energyRt.org/reference/read.md) reads a
  solution produced outside energyRt.
- [`en_check_solve()`](https://energyRt.org/reference/en_check.md)
  solves a tiny model on every configured backend and reports what
  actually works;
  [`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md)
  /
  [`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md)
  /
  [`en_install_julia_pkgs()`](https://energyRt.org/reference/en_install_julia_pkgs.md)
  set the toolchains up.
- [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
  is about **6x faster** on large models, folds `year` as well as
  `region` and `timeslice`, and logs its per-stage timings. The
  technology group and share maps are built once per run rather than
  once per map, and the object loop no longer slows down with the number
  of objects: a model of 21,000 objects interpolates in about 20 minutes
  instead of 48.
- The `mm` route keeps folded numeric parameters folded when the
  installed multimod looks wildcards up itself, instead of expanding
  them on import.
- Remote solving: the NEOS server for GAMS/CPLEX without a local
  licence, and GPU execution in the cloud.
- `interpolate_model(fx_equ = TRUE)` writes a fixed bound (`.fx`, or
  `.lo == .up`) as one equality row, `eq<Family>Fx`, instead of a lower
  and an upper row, in every bound family (availability, shares,
  capacities, storage, supply, trade, import/export rows): about 25%
  fewer rows on hourly VRE models, same solution. `fx_equ = FALSE` keeps
  the pairs.

#### Results, storage and sharing

- **A project registry** keyed on path records every model, scenario and
  dataset, with a lifecycle (`seal_*()` / `unseal_*()`,
  [`mark_delete()`](https://energyRt.org/reference/seal.md) /
  [`delete_marked()`](https://energyRt.org/reference/seal.md)) and
  [`summary()`](https://rdrr.io/r/base/summary.html) reporting counts
  and staleness. Objects are reachable by name: `getScenario("base")`.
- **Runs and artifacts**:
  [`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md),
  [`scenario_solutions()`](https://energyRt.org/reference/scenario_solutions.md)
  and
  [`scenario_artifacts()`](https://energyRt.org/reference/scenario_artifacts.md)
  say what each solve produced and left on disk;
  [`promote_solution()`](https://energyRt.org/reference/promote_solution.md)
  and
  [`import_solution()`](https://energyRt.org/reference/import_solution.md)
  choose which one counts;
  [`drop_solver_outputs()`](https://energyRt.org/reference/drop_solver_outputs.md)
  and
  [`drop_scenario_run()`](https://energyRt.org/reference/scenario_runs.md)
  clean up.
- Results live in Arrow stores, so a solution larger than memory stays
  readable, and `getData(run = )` reads several runs in one call.
- [`prepare_for_sharing()`](https://energyRt.org/reference/prepare_for_sharing.md)
  writes a cleaned, trimmed copy of a stored scenario and
  [`strip_user_info()`](https://energyRt.org/reference/strip_user_info.md)
  removes the machine it was made on.

#### Validation

- [`validate_scenario_parameters()`](https://energyRt.org/reference/validate_scenario_parameters.md)
  separates **structural** findings (the model would solve to a
  meaningless answer) from advisory ones, and names them: missing
  flow-to-capacity links (`free_meal`), constraint tuples with no data
  behind them, coefficient ranges that will hurt a solver, and variant
  provenance that disagrees with the process names.

- [`verify_solution()`](https://energyRt.org/reference/verify_solution.md)
  re-derives the model’s own identities from the results, with check
  groups (`checks = "all"`), and
  [`verify_checks()`](https://energyRt.org/reference/verify_checks.md)
  lists them.

- [`getVariants()`](https://energyRt.org/reference/getVariants.md) also
  takes a `model`, listing the names vintages and clusters will expand
  to before interpolating.

- [`report()`](https://energyRt.org/reference/report.md) on a technology
  or storage takes spatial and solved context: `geoscale =` adds a map
  of where the process is declared, `model =` prices it in that model’s
  calendar, horizon and discount, and `scenario =` adds what the solve
  realized – capacity built per region, and the ex-post levelized cost
  beside the ex-ante one, per variant. `scenario` implies its model,
  which implies its geoscale; a model that is not the scenario’s is
  refused.

- [`levcost()`](https://energyRt.org/reference/levcost.md) on a solved
  scenario accepts a **base** process name and prices every variant of
  it, returning the same `levcost_variants` result the declared-object
  path gives (`by_variant =` included). A vector of names works too;
  variants the solver never built are reported and skipped.

- [`audit_coefficients()`](https://energyRt.org/reference/audit_coefficients.md)
  reports the coefficient range of a written model, per equation.

- [`compare_scenarios()`](https://energyRt.org/reference/compare_scenarios.md),
  [`compare_models()`](https://energyRt.org/reference/compare_models.md)
  and
  [`compare_inputs()`](https://energyRt.org/reference/compare_inputs.md)
  diff solved results, declarations and interpolated inputs.

#### Reporting and visualisation

- **[`report()`](https://energyRt.org/reference/report.md) for every
  object**: 14 element classes plus model and scenario containers,
  rendered to HTML, PDF, Word or TeX, cached, with user-defined process
  groups, branding and a `report_*` helper family for custom templates.
- **Levelized cost**:
  [`levcost()`](https://energyRt.org/reference/levcost.md) prices
  technologies, storages (LCOS) and trade routes (LCOT) analytically —
  no solver — per vintage, cluster and region, with results cached and
  plotted by
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
- **[`draw()`](https://energyRt.org/reference/draw.md)** sketches a
  process as a diagram, including a storage’s three parts and its ratio
  couplings.
- [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  covers objects, calendars, horizons, scenarios and comparisons, with
  `by = "vintage"` / `"cluster"` fan-outs;
  [`plot_map()`](https://energyRt.org/reference/plot_map.md),
  [`plot_trade_map()`](https://energyRt.org/reference/plot_trade_map.md)
  and
  [`plot_trade_network()`](https://energyRt.org/reference/plot_trade_network.md)
  map any solved variable carrying a region.

### Deprecations

- None pending. The 0.5-era deprecation layer and its
  `?energyRt-deprecated` page are removed, and the renames in this cycle
  are hard breaks: the error names the new spelling.

### Bug fixes

Around 130 individual fixes are listed in the detailed log; most changed
an answer rather than only the code. The classes worth knowing about:

- **A fixed weather factor fixed nothing.** `waf.fx` (and the storage
  and supply equivalents) entered only the upper bound unless the
  process also declared `af.lo`, so VRE output could fall below the
  weather and curtailment never reached `vBalance`. It now binds from
  both sides.

- **`wafs` and `wafc`.** `wafs` multiplies `afs`: without `afs` it is
  now an error, and its weather must have a value at each `afs`
  timeslice (a weather at another timeframe read as 0 and shut the
  technology down). `wafc` acts like `waf` at the commodity’s timeframe:
  with no `afc` it implies `afc = 1` on the sides it sets (it was
  silently ignored).

- **Parameters that never reached the solver.** Several slots were
  declared but not wired (`pTradeIrEff`, per-part storage `fixom`/`eac`,
  aux couplings on an unpriced part), so a cost or a limit was silently
  dropped and the model solved without it.

- **Equations instantiated where no data stood behind them**, which made
  the solver read its own default — a bound of 0 shutting a process
  down, or an unbounded flow where the user expected a rating.

- **Multi-level geography**: emissions of plants in sub-regions are
  counted, coarse supply keeps its cost, a repeated region code at two
  adjacent levels no longer poisons its parent, and a geoscale whose
  levels do not nest is refused rather than silently flattened.

- **Levies and constraints**: a levy on a timeslice-indexed base is
  weighted, taxes survive folding, and a summand’s `timeframe` /
  `geoframe` is resolved per member instead of from the pooled index.

- **Levelized cost**: priced variants no longer return `NA`, a vintage
  outside the priced horizon is handled, a trade corridor is priced at
  what the model actually pays, and a repository is priced process by
  process.

- **Reporting the truth**: an infeasible or unbounded model is a result
  rather than an error,
  [`verify_solution()`](https://energyRt.org/reference/verify_solution.md)
  stops double-counting coarse regions, and a non-interactive run no
  longer stops at a debugger prompt.

- **`get_process_*(process = )` is honoured.**
  [`get_process_inputs()`](https://energyRt.org/reference/get_process_inputs.md),
  `_outputs()`, `_aux()`, `_region()`, `_invest_window()` and
  `_timeframe()` accepted the argument and ignored it, returning every
  process in the model;
  [`get_process_comm()`](https://energyRt.org/reference/get_process_comm.md)
  inherited it. The returned shape was right, so the answer looked
  right.

- **A weather series whose data omits `region` is stored where it is
  declared.** `pWeather` took its regions from the data column while the
  two weather maps took theirs from `@region`, so a region-agnostic
  profile declared at a coarse region was materialised at every region
  of every geoscale level. No answer moves — every backend reads the
  series through `mWeatherRegionAt` — but the parameter no longer claims
  it where nothing declared it.

### Documentation

- A rebuilt [pkgdown site](https://energyRt.org) with tutorial articles
  for installation, backends, model bricks, units, time and space
  resolution, storage, vintages and clusters, the three-part TOPIA
  walk-through, workflow, scenario management, reports and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
- The testing strategy — tiers `check < fast < cross < full`, the
  `@covers` coverage matrix, tracked goldens — is documented in
  `tests/README.md`, `tests/TESTING.md` and the *Testing strategy*
  article.
- [`?add`](https://energyRt.org/reference/add.md),
  [`?read`](https://energyRt.org/reference/read.md),
  [`?interpolate`](https://energyRt.org/reference/interpolate_model.md),
  [`?setCalendar`](https://energyRt.org/reference/setGeoscale.md),
  [`?setHorizon`](https://energyRt.org/reference/newModel.md),
  [`?getHorizon`](https://energyRt.org/reference/newModel.md) and
  [`?add_to_convert`](https://energyRt.org/reference/convert.md) open a
  help page. The generics were exported with only their methods
  documented, so the bare names resolved to nothing.
- **Every public help page has an example.** 82 topics had none; they
  now build what they use, on the shipped TOPIA kits, so each runs as
  pasted. Examples needing a solver, a project store, pandoc or a python
  environment are marked `\dontrun{}` but were executed before being
  written in.

## energyRt 0.50.9-dev

### Breaking changes

- Time-slice weighting is rewritten: variables with a `slice` dimension
  are no longer weighted, for consistency between sampled and
  non-sampled runs. Slice weights can vary across model years.
- Variables with a `year` dimension are no longer weighted to interval
  lengths, except cumulative variables (`vSupReserveCum`) and capacity
  variables, which represent the state at the end of the period.
- New-capacity variables (`vTechNewCap`) are given per year — apply
  `pPeriodLen` to get total new capacity by the end of each period.
- System costs are regrouped by type (capital, fixed O&M, variable O&M,
  supply, taxes, subsidies) and by process type, and the Total Costs
  equation rewritten to match.

### Bug fixes

- `optimizeRetirment = TRUE` no longer retires new technologies at the
  same time as their installation.
- [`draw()`](https://energyRt.org/reference/draw.md) on a trade no
  longer repeats arrows.
- [`newCosts()`](https://energyRt.org/reference/newCosts.md) is
  debugged, with an example in the Topia tutorial.
- [`tsl2hour()`](https://optimal2050.github.io/timescales/r/reference/timeslice_conversions.html)
  identifies n-digit hours; it previously worked for two only.

## energyRt 0.50.7-dev

### Bug fixes

- Several stability issues in
  [`draw()`](https://energyRt.org/reference/draw.md).

### Documentation

- A “Hello World” example in the tutorial, a new logo design, and the
  first draft of the CRAN-style manual. Clean-up for CRAN in progress;
  the version may be unstable.

## energyRt 0.50.6-dev

### New features

- [`draw()`](https://energyRt.org/reference/draw.md) is drafted for
  every process class — `technology`, `export`, `import`, `supply`,
  `demand`, `trade`, `storage`.

### Documentation

- Docs completed for the main classes, with examples.

## energyRt 0.50.5-dev

### New features

- [`draw()`](https://energyRt.org/reference/draw.md) is rewritten on the
  `grid` package and is now a generic, with methods for `technology`,
  `export` and `import`.

### Bug fixes

- Several interface-level bugs introduced in 0.50.4-dev during clean-up.

## energyRt 0.50.4-dev

### Documentation

- Class documentation ~70% complete; website reshaped with new (empty)
  articles. Untested — the version may hold surprises.

## energyRt 0.50.3-dev

### New features

- Functions to document classes from `classes.yaml`.

### Documentation

- A `NEWS.md` file to track changes; `technology-class` and
  [`newTechnology()`](https://energyRt.org/reference/technology.md)
  documented. Development version in preparation for CRAN submission.
