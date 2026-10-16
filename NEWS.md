# energyRt 0.90.0 (pre-release)

The first release since `0.50-beta`. It consolidates the 0.60, 0.70 and 0.80
development cycles, none of which were released, and is the **pre-release for
1.0**: the modelling API is the one 1.0 will ship, and the work left is testing,
debugging and visualisation rather than new structure.

Upgrading from 0.50 is not a drop-in. The time dimension, the `storage` class,
the variant system and the geography layer all changed shape — read *Breaking
changes* first. The change-by-change log for the whole cycle (450 entries) is in
[dev/NEWS-0.6-0.9-detail.md](dev/NEWS-0.6-0.9-detail.md).

## License

* energyRt is relicensed from AGPL-3 to **Apache-2.0**. Releases up to and
  including v0.89 remain available under AGPL-3.

## Breaking changes

* **The time dimension is `timeslice`** everywhere (was `slice`), matching
  TIMES and OSeMOSYS, and a **`calendar` is required** wherever a timeslice
  layout is needed. Timeslice label conversion moved to the sibling
  **timescales** package, so `tsl2hour()`, `tsl2year()`, `tsl_guess_format()`
  and `tsl_formats` are gone. `calendars` ships generic designs (`m12`, `q4`,
  `d365_h24`, sampled variants); model-specific ones live with their models.

* **A storage is three parts.** Charger, reservoir and discharger each have
  their own capacity variable, costs and availability: `@input`/`@output`/
  `@storage` declare the roles, `vStorageOutCap`/`vStorageInpCap`/
  `vStorageStgCap` size them, `@duration` and `@inp2out`/`@inp2stg` tie them
  together, and `vStorageStore` is now `vStorageLevel`. Charge and discharge
  **rates are bounded by the converters**: `inp.af.up`/`out.af.up` default to 1,
  so a flow per timeslice cannot exceed `capacity * cap2act * share`.

* **Vintages and clusters are one mechanism.** `@vintage` holds build years
  (with the investment window merged in from `@start`/`@end`/`@olife`) and
  `@cluster` holds parallel parts; both must be **declared** before a label may
  be used in a data slot, and interpolation expands them into ordinary processes
  named `EWIN_VIN2030_CL01`. Read coordinates from `getVariants()`, never by
  parsing a name.

* **Geography is multi-level.** `@geolevel` is `@geoframe` on commodities and
  summands, a model carries a `Geoscale` (from the sibling **geoscales**
  package), and a commodity or process may sit at a coarser level than the
  flows it touches.

* **Taxes and subsidies are indexed by the instrument**, not by the commodity,
  and can be levied on a process. `newConstraintS()` is removed — it failed for
  every input it was given; declare a `constraint` or a levy instead.

* **Auxiliary flows sit at the aux commodity's timeframe**: an annual emission
  of an hourly plant is one variable and one equation per year, not 8760. The
  storage aux couplings are part-prefixed (`inp.cap2ainp`, `stg.ncap2aout`, …);
  the bare names silently applied to the discharger only.

* **The API is verb-first and prefixed `en`**: scenario management reads
  `save_scenario()` / `load_scenario()` / `drop_scenario_run()`, the registry
  class is `en_registry`, and 46 machinery helpers are no longer exported.
  Object names are validated at construction. The 0.5-era deprecation layer is
  removed, bar four shims that still accept the old spelling and warn once per
  session: `storage@charge`, the `slice` column name, the un-prefixed solver
  path environment variables, and `options(energyRt.verbose)`.

* **The Julia and Python backends exchange Arrow files**, which needs
  `Arrow.jl` and `pyarrow`; `arrow_format` split into `storage_format` and
  `exchange_format`. A run's solver files are written into `runs/<solve>/`.

* **The teaching model is TOPIA** (was UTOPIA), its regions are `W1`/`W2`,
  `C1`–`C6`, `E1`–`E3`, and its four standalone datasets are folded into
  `topia`. The test tier `nightly` is renamed `full`.

* `storage@cap2stg` is refused rather than renamed: it became `@duration` in
  0.80, and the compatibility shim is gone. The error names the replacement.

* `newSub()` is removed — `newSubsidy()` is the constructor. They were the same
  function, and `newTax()` never had a short alias.

## New features

### Model building

* **Resource tiers in one object.** A `commodity` or a `weather` object
  declaring `@cluster` becomes one object per tier at interpolation, each with
  its own balance, so graded land or a graded wind resource cannot pool. A
  consumer picks its tier by cluster name; an unclustered object is untouched.
* **Weather transforms**: one shipped series feeds many derived ones, and a
  weather object may be declared at a coarser region than the processes reading
  it — one stored profile for a whole country.
* **Transmission**: `newACLine()` and `newDCLink()`, an opt-in Kirchhoff
  voltage law, quadratic losses approximated by capacity tranches
  (`lossTranches()`), and `@trade$af` to rate a route's flow.
* **Supply curves**: `asSupplyCurve()`, `asImportCurve()`, `asExportCurve()`
  turn a single price into a stepped curve with its own roster.
* `commodity@property` carries physical properties — heating values,
  densities, carbon content — as a tidy table rather than scattered
  coefficients.
* Discount rates may be declared at any geoscale level, and a WACC per
  technology is separate from the social discount rate, with `payback` as the
  cost-recovery life on every backend.
* Process definitions are portable: `process_to_spec()` / `process_from_spec()`
  (YAML/JSON), an interactive `process_designer()`, and Excel round-trip with
  `read_technology_xlsx()` / `write_technology_xlsx()`.

### Time and space

* Calendars gained `year_start` and `utc_offset_minutes`, milestone years can
  carry display labels, and sampled calendars (one representative day per
  month, `d365_h24_1dps`) are first-class — a full model solved on a subset of
  slices.
* **Region aggregation with clustering**: `aggregate_model_regions(clusters = )`
  partitions a corridor family into `k` groups, `get_process_groups()` reports
  what may be merged, `process_cluster_sweep()` sweeps the admissible range and
  `model_clusters()` reports what an aggregation did.
* A filtered geoscale passed to `interpolate_model()` solves a sub-territory,
  and `subset_model_regions()` warns when sampling splits a cell a process
  spans.

### Solving

* **Solve drivers**: `solve_myopic()` (window by window, with a solution
  ledger), `solve_by_sample()`, `solve_by_region()` and `solve_guided()`
  (target-seeking), each with its primitives exported.
* **GPU and first-order LP**: presets for cuOpt (`julia_cuopt`,
  `mm_cuopt_wsl`) and HiGHS PDLP/HiPDLP, plus `lang = "mm"`, which hands the
  matrix to **multimod** and solves it outside the templates.
* `solver_options$pyomo_mps` writes the problem as MPS, and `read_solution()`
  reads a solution produced outside energyRt.
* `en_check_solve()` solves a tiny model on every configured backend and
  reports what actually works; `en_check_packages()` / `en_install_deps()` /
  `en_install_julia_pkgs()` set the toolchains up.
* `interpolate_model()` is about **6x faster** on large models, folds `year` as
  well as `region` and `timeslice`, and logs its per-stage timings.
* Remote solving: the NEOS server for GAMS/CPLEX without a local licence, and
  GPU execution in the cloud.

### Results, storage and sharing

* **A project registry** keyed on path records every model, scenario and
  dataset, with a lifecycle (`seal_*()` / `unseal_*()`, `mark_delete()` / `delete_marked()`) and
  `summary()` reporting counts and staleness. Objects are reachable by name:
  `getScenario("base")`.
* **Runs and artifacts**: `scenario_runs()`, `scenario_solutions()` and
  `scenario_artifacts()` say what each solve produced and left on disk;
  `promote_solution()` and `import_solution()` choose which one counts;
  `drop_solver_outputs()` and `drop_scenario_run()` clean up.
* Results live in Arrow stores, so a solution larger than memory stays
  readable, and `getData(run = )` reads several runs in one call.
* `prepare_for_sharing()` writes a cleaned, trimmed copy of a stored scenario
  and `strip_user_info()` removes the machine it was made on.

### Validation

* `validate_scenario_parameters()` separates **structural** findings (the model
  would solve to a meaningless answer) from advisory ones, and names them:
  missing flow-to-capacity links (`free_meal`), constraint tuples with no data
  behind them, coefficient ranges that will hurt a solver, and variant
  provenance that disagrees with the process names.
* `verify_solution()` re-derives the model's own identities from the results,
  with check groups (`checks = "all"`), and `verify_checks()` lists them.
* `getVariants()` also takes a `model`, listing the names vintages and clusters
  will expand to before interpolating.
* `levcost()` on a solved scenario accepts a **base** process name and prices
  every variant of it, returning the same `levcost_variants` result the
  declared-object path gives (`by_variant = ` included). A vector of names
  works too; variants the solver never built are reported and skipped.
* `audit_coefficients()` reports the coefficient range of a written model, per
  equation.
* `compare_scenarios()`, `compare_models()` and `compare_inputs()` diff solved
  results, declarations and interpolated inputs.

### Reporting and visualisation

* **`report()` for every object**: 14 element classes plus model and scenario
  containers, rendered to HTML, PDF, Word or TeX, cached, with user-defined
  process groups, branding and a `report_*` helper family for custom
  templates.
* **Levelized cost**: `levcost()` prices technologies, storages (LCOS) and
  trade routes (LCOT) analytically — no solver — per vintage, cluster and
  region, with results cached and plotted by `autoplot()`.
* **`draw()`** sketches a process as a diagram, including a storage's three
  parts and its ratio couplings.
* `autoplot()` covers objects, calendars, horizons, scenarios and comparisons,
  with `by = "vintage"` / `"cluster"` fan-outs; `plot_map()`,
  `plot_trade_map()` and `plot_trade_network()` map any solved variable
  carrying a region.

## Deprecations

* None pending. The 0.5-era deprecation layer and its `?energyRt-deprecated`
  page are removed, and the renames in this cycle are hard breaks: the error
  names the new spelling.

## Bug fixes

Around 130 individual fixes are listed in the detailed log; most changed an
answer rather than only the code. The classes worth knowing about:

* **Parameters that never reached the solver.** Several slots were declared but
  not wired (`pTradeIrEff`, per-part storage `fixom`/`eac`, aux couplings on an
  unpriced part), so a cost or a limit was silently dropped and the model
  solved without it.
* **Equations instantiated where no data stood behind them**, which made the
  solver read its own default — a bound of 0 shutting a process down, or an
  unbounded flow where the user expected a rating.
* **Multi-level geography**: emissions of plants in sub-regions are counted,
  coarse supply keeps its cost, a repeated region code at two adjacent levels
  no longer poisons its parent, and a geoscale whose levels do not nest is
  refused rather than silently flattened.
* **Levies and constraints**: a levy on a timeslice-indexed base is weighted,
  taxes survive folding, and a summand's `timeframe` / `geoframe` is resolved
  per member instead of from the pooled index.
* **Levelized cost**: priced variants no longer return `NA`, a vintage outside
  the priced horizon is handled, a trade corridor is priced at what the model
  actually pays, and a repository is priced process by process.
* **Reporting the truth**: an infeasible or unbounded model is a result rather
  than an error, `verify_solution()` stops double-counting coarse regions, and
  a non-interactive run no longer stops at a debugger prompt.

## Documentation

* A rebuilt [pkgdown site](https://energyRt.org) with tutorial articles for
  installation, backends, model bricks, units, time and space resolution,
  storage, vintages and clusters, the three-part TOPIA walk-through, workflow,
  scenario management, reports and `autoplot()`.
* The testing strategy — tiers `check < fast < cross < full`, the `@covers`
  coverage matrix, tracked goldens — is documented in `tests/README.md`,
  `tests/TESTING.md` and the *Testing strategy* article.
* `?add`, `?read`, `?interpolate`, `?setCalendar`, `?setHorizon`, `?getHorizon`
  and `?add_to_convert` open a help page. The generics were exported with only
  their methods documented, so the bare names resolved to nothing.

# energyRt 0.50.9-dev

## Breaking changes

* Time-slice weighting is rewritten: variables with a `slice` dimension are no
  longer weighted, for consistency between sampled and non-sampled runs. Slice
  weights can vary across model years.
* Variables with a `year` dimension are no longer weighted to interval lengths,
  except cumulative variables (`vSupReserveCum`) and capacity variables, which
  represent the state at the end of the period.
* New-capacity variables (`vTechNewCap`) are given per year — apply `pPeriodLen`
  to get total new capacity by the end of each period.
* System costs are regrouped by type (capital, fixed O&M, variable O&M, supply,
  taxes, subsidies) and by process type, and the Total Costs equation rewritten
  to match.

## Bug fixes

* `optimizeRetirment = TRUE` no longer retires new technologies at the same time
  as their installation.
* `draw()` on a trade no longer repeats arrows.
* `newCosts()` is debugged, with an example in the Topia tutorial.
* `tsl2hour()` identifies n-digit hours; it previously worked for two only.

# energyRt 0.50.7-dev

## Bug fixes

* Several stability issues in `draw()`.

## Documentation

* A "Hello World" example in the tutorial, a new logo design, and the first
  draft of the CRAN-style manual. Clean-up for CRAN in progress; the version may
  be unstable.

# energyRt 0.50.6-dev

## New features

* `draw()` is drafted for every process class — `technology`, `export`,
  `import`, `supply`, `demand`, `trade`, `storage`.

## Documentation

* Docs completed for the main classes, with examples.

# energyRt 0.50.5-dev

## New features

* `draw()` is rewritten on the `grid` package and is now a generic, with methods
  for `technology`, `export` and `import`.

## Bug fixes

* Several interface-level bugs introduced in 0.50.4-dev during clean-up.

# energyRt 0.50.4-dev

## Documentation

* Class documentation ~70% complete; website reshaped with new (empty) articles.
  Untested — the version may hold surprises.

# energyRt 0.50.3-dev

## New features

* Functions to document classes from `classes.yaml`.

## Documentation

* A `NEWS.md` file to track changes; `technology-class` and `newTechnology()`
  documented. Development version in preparation for CRAN submission.
