# energyRt 0.90.0 (pre-release)

The first release since `0.50-beta`, consolidating the unreleased 0.60-0.80
cycles. The modelling API is the one planned for 1.0; testing and debugging
continue. Upgrading from 0.50 is not a drop-in: see *Breaking changes*.

## License

* The per-object timeslice maps (`mTechTimeslice` and its siblings,
  `mCommTimeslice`, `mWeatherTimeslice`), the technology activity and output
  domains (`mvTechAct`, `mvTechOut`), `meqTechActSng` and the
  availability-factor maps (`meqTechAfLo`, `meqTechAfUp`, `meqTechAfFx`,
  where the bound is one number per technology, region and year) are
  defined by a rule, the join of the small tables they are made of, and
  their tables are dropped at the end of `interpolate_model()`. Readers see the same tuples;
  the multimod route enumerates the rule itself and the saved scenario no
  longer carries the tables. The output and single-output domains keep
  their tables in a model with coarse processes.
  `options(energyRt.map_rules = FALSE)` keeps every table;
  `options(energyRt.map_rules_verify = TRUE)` checks each rule against the
  table it replaces.
  The availability-factor maps are decided per technology, region and year
  and recorded as rules without building their tables, in a sparse,
  unfolded interpolation; a full-year one-country model builds them about
  3.5 times faster, with a quarter less peak memory.
* Relicensed from AGPL-3 to Apache-2.0. Releases up to v0.89 remain AGPL-3.

## New features

### Model building

* Three-part storage: charger, reservoir and discharger with their own
  capacities, costs and availability, coupled by `@duration`, `@inp2out` and
  `@inp2stg`.
* Vintages (`@vintage`) and clusters (`@cluster`) expand a process into
  variants at interpolation; `getVariants()` lists them.
* Resource tiers: a `commodity` or `weather` object with `@cluster` gets one
  balance per tier.
* Weather transforms derive many series from one, and a weather object may be
  declared at a coarser region than the processes reading it.
* Transmission: `newACLine()`, `newDCLink()`, an optional Kirchhoff voltage
  law, loss tranches (`lossTranches()`), route ratings (`@trade$af`), route
  ratings over a parent timeslice (`@trade$afs`) and weather factors of the
  ratings (`@weather`, read at the route's source region).
* Supply, import and export curves: `asSupplyCurve()`, `asImportCurve()`,
  `asExportCurve()`.
* Taxes and subsidies are indexed by instrument and can be levied on processes.
  `cap` and `ncap` charge technologies and trades (a trade in each region the
  levy is placed in); a storage's capacity is levied by part
  with `stg.cap`/`stg.ncap` (reservoir), `inp.cap`/`inp.ncap` (charger) and
  `out.cap`/`out.ncap` (discharger).
* `commodity@property` for physical properties; discount rates at any geoscale
  level; a WACC per technology with `payback` as its cost-recovery life.
* Portable process definitions: `process_to_spec()` / `process_from_spec()`
  (YAML/JSON), `process_designer()`, `read_technology_xlsx()` /
  `write_technology_xlsx()`.

### Time and space

* Calendars from **timescales**, including irregular calendars (months of
  28-31 days), sampled calendars (representative days) and the new
  `m12_d365_h24` and `m12_d365_h24_1dpm`.
* Multi-level geography from **geoscales**: commodities and processes may sit
  at a coarser `@geoframe` than the flows they touch, with auxiliary flows held
  in the finer cells beneath.
* Region aggregation with clustering: `aggregate_model_regions(clusters = )`,
  `get_process_groups()`, `process_cluster_sweep()`, `model_clusters()`.
* Sub-territory solves from a filtered geoscale, and `subset_model_regions()`.
* Auxiliary flows sit at the auxiliary commodity's timeframe: an annual
  emission is one variable per year, not one per hour.

### Solving

* Solve drivers: `solve_myopic()`, `solve_by_sample()`, `solve_by_region()`
  and `solve_guided()`.
* First-order LP solvers (cuOpt, HiGHS PDLP), and `lang = "mm"`, which builds
  the problem matrix with **multimod**. Remote solving on the NEOS server and
  on cloud GPUs.
* Matrix presets (`backend = "mm"`) are solved by `multimod::solve_mps()`:
  HiGHS in the R session or through Julia, cuOpt on a local GPU through WSL
  (`mm_cuopt_wsl`) or on a rented GPU (`mm_cuopt_cloud`, `pyomo_cuopt_cloud`).
* On the `lang = "mm"` route the activity and output definitions
  (`eqTechAfFx`, `eqTechActSng`) and the per-process totals (`eqTechOutTot`,
  `eqTechInpTot`, the storage, supply, import, export and demand totals) are
  substituted while the matrix is built: their rows and the columns they
  define are not written, which reduces a full-year model several-fold. The preset field `substitute` names the
  equations; `character()` builds every row. With a multimod that composes
  roll-ups, the commodity output and input totals (`eqOutTot`, `eqInpTot`)
  are substituted too.
* The matrix route removes `model.mps` (and its compressed copy) once the
  solution is decoded; the index tables and the solution file stay. The
  preset field `keep_mps = TRUE` keeps it.
  A preset's `mm_solver` and `where` choose the solver and the place. A
  Pyomo- or JuMP-written matrix carries index tables (from Pyomo's symbol map,
  from JuMP's containers) and is decoded by multimod like its own;
  `julia_mps` writes the JuMP matrix and `julia_cuopt_cloud` solves it on a
  rented GPU.
* `interpolate_model()` is about 6x faster on large models;
  `fx_equ = TRUE` writes fixed bounds as single equality rows (about 25% fewer
  rows on hourly VRE models).
* MPS output (`solver_options$pyomo_mps`), `read_solution()` for solutions
  produced elsewhere, and `en_check_solve()`, `en_check_packages()`,
  `en_install_deps()`, `en_install_julia_pkgs()` to check and set up the
  toolchains.

* `interpolate_model(model, ..., base = REF)` builds a scenario on an
  interpolated base when the objects added are constraints, cost terms,
  taxes or subsidies: only what they add is computed, the base's tables are
  reused and its store is never written to, and the result equals a full
  interpolation. Anything else is interpolated in full, with a message saying
  why. The saved scenario names its base in `scenario.yml`.
* A scenario keeps the model as given in `@model`: the variants, tiers and
  transform clones interpolation builds are rebuilt when a reader needs them.
  A saved scenario references its sealed model instead of copying it, also
  for models with vintages, clusters or a spatial sample. `getObject()` on a
  scenario returns the objects as defined.
* `save_dataset(rehash = FALSE)` and `save_dataset(overwrite = TRUE)` stop
  when another store entry references the version they would rewrite, and
  name the dependents.
* `base = REF` also takes new processes, commodities and other model objects:
  the per-object tables of the objects REF holds are reused and every map is
  rebuilt.
* `base = REF` takes a sample of REF's calendar or regions (a calendar of
  some of its timeslices, a filtered geoscale with `boundary_prices`): REF's
  tables are reused filtered to the sample. Saved, such a scenario references
  the sealed REF's tables through a row filter.

### Results and project management

* A project registry keyed on path, with a lifecycle (`seal_*()`,
  `mark_delete()`, ...); objects are reachable by name (`getScenario("base")`).
* Runs and artifacts: `scenario_runs()`, `scenario_solutions()`,
  `promote_solution()`, `import_solution()`, `drop_scenario_run()`.
* Results in Arrow stores, readable when larger than memory;
  `getData(run = )` reads several runs at once.
* `prepare_for_sharing()` and `strip_user_info()`.
* The dataset store keeps versions by content under one name
  (`datasets/<name>/<hash>/`): saving other content under a name adds a
  version instead of overwriting, `load_dataset(name, hash = )` pins one,
  and the registry lists every version. A references table beside the
  registry records which scenario points at which version.
* `save_scenario()` keeps a scenario's large interpolated tables (numeric
  parameters, bounds and maps from `energyRt.param_ref_min_rows` rows, 1e6
  by default; weather and demand series from `energyRt.ondisk_min_rows`) in
  its own store and records their hashes in `scenario.yml`. A scenario built
  with `interpolate_model(base = )` on a saved and sealed base references
  every table identical to the base's instead of storing a copy, and keeps
  the rest. `load_scenario()` resolves the references; the lazy readers
  follow them; a writer that changes a table drops its reference.
  `unseal_scenario()` refuses while scenarios reference the base's tables
  (`force = TRUE` to unseal anyway). References into the dataset store held
  by scenarios saved earlier still load; a weather or demand table whose
  version is gone is recomputed from the model.
* `save_model()` and `save_scenario()` take `mode = "ref"` (default,
  `options(energyRt.save_mode)`) or `"full"`. `"ref"` references a repository
  or model only when its exact content is a SEALED version, and copies it in
  otherwise; `"full"` copies everything in. `save_scenario()` no longer
  stores an unsealed model in the model store to reference it. Changed: a
  scenario whose model is stored but not sealed now carries its own copy.
* A scenario whose model is a sealed version plus repositories added by
  `interpolate_model()` references the sealed version and keeps only the
  added repositories (`model: added` in its manifest); `load_scenario()`
  adds them back.
* `list_versions()` lists the versions of models, repositories and datasets with a
  status (`current`, `in use`, `marked`, `unused`), when they were sealed,
  how many entries reference them and their size (`details = TRUE` for
  more). `mark_delete(version = )` marks superseded versions (sealed ones of
  models and repositories; a hash, or `"unused"`), and `delete_marked()`
  deletes them when still unreferenced.
  `delete_marked()` also keeps an entry while any of its sealed versions has
  a dependent.
* `list_dependents()` replaces `store_dependents()`: the verified dependents
  recorded on sealed versions (with `recursive = TRUE`, transitively) plus
  the references found in manifests; `prune = TRUE` drops stale records.
* Unsealed saves of models and repositories record a save id instead of a
  content hash (`hashed: false` in the manifest), so saving a large model no
  longer serializes it for hashing. The content hash is computed when a
  decision needs it: saving onto a sealed entry, comparing with the sealed
  versions of a name, and sealing (which now loads and hashes the payload).
  An identical re-save of an unsealed entry is written again.
  `options(energyRt.lazy_hash = FALSE)` hashes every save.
* A model whose repository holds unchanged objects of one sealed repository
  version (taken with `getObject()` from a loaded sealed repository)
  references that version with the object names and the hash of the
  selected objects, verified by `load_model()`. Sealing a repository records
  each object's hash for this.
* Sealing a model or repository freezes its content as a version
  (`<entry>/<hash>/`): unsealed saves update the entry in place and leave the
  sealed versions untouched, and references and `load_*(hash = )` reach a
  sealed version exactly. A sealed version records the models and scenarios
  that reference it, and `unseal_model()` / `unseal_repository()` list them
  and refuse while they exist unless `force = TRUE`.
* Referenced tables read back into memory are dropped before the scenario
  shell (`scen.RData`) is written. A shell above
  `options(energyRt.shell_max_mb = 256)` warns and names its largest slots.
* The `mm` route no longer copies the scenario's tables into the multimod
  model: multimod reads them from the scenario's store or the dataset store
  as the matrix is built (`multimod_from_energyRt(inMemory = FALSE)`), a
  table held in memory is attached rather than dropped, and the memo size
  is the preset's `data_cache_gb` (32 by default). Once the matrix is
  written, the model, the memo and the scenario's tables that are persisted
  on disk or in the store are released, so a solve, local or in the cloud,
  waits with the memory free.

### Validation and comparison

* `validate_scenario_parameters()` (structural versus advisory findings),
  `verify_solution()` with check groups, `audit_coefficients()`.
* `compare_scenarios()`, `compare_models()`, `compare_inputs()`.

### Reporting and visualisation

* `report()` for every class and for models and scenarios (HTML, PDF, Word,
  TeX), with spatial and solved context (`geoscale =`, `model =`,
  `scenario =`).
* `levcost()`: analytic levelized cost of technologies, storage (LCOS) and
  trade (LCOT) per vintage, cluster and region; ex-post per variant on a solved
  scenario.
* `draw()` process diagrams; `autoplot()` for objects, calendars, horizons,
  scenarios and comparisons; `plot_map()`, `plot_trade_map()`,
  `plot_trade_network()`.
* TOPIA, the teaching model (was UTOPIA), rebuilt around concentrated resources
  traded between zones.

## Breaking changes

* `slice` is `timeslice`, and a `calendar` is required; the `tsl2*()` helpers
  moved to timescales.
* Storage: `vStorageStore` is `vStorageLevel`; `@cap2stg`, `@charge` and
  `@inflow` are refused (use `@duration`, `@startLevel` or a `supply`); flows
  are bounded by converter capacity (`inp.af.up` / `out.af.up` default to 1).
* `@start`, `@end` and `@olife` are merged into `@vintage`; vintage and cluster
  labels must be declared.
* `@geolevel` is `@geoframe`, and a model carries a `Geoscale`.
* Taxes and subsidies are indexed by instrument; `newConstraintS()` and
  `newSub()` are removed.
* Storage auxiliary couplings are part-prefixed (`inp.cap2ainp`, ...).
* The first argument is `object` in 66 functions; the scenario API is
  verb-first (`save_scenario()`, `load_scenario()`, ...) with `en_registry`;
  46 helpers are no longer exported; the 0.5-era deprecations are removed.
* The Julia and Python backends exchange Arrow files (`Arrow.jl`, `pyarrow`);
  `arrow_format` is split into `storage_format` and `exchange_format`.
* UTOPIA is TOPIA (regions `W1`/`W2`, `C1`-`C6`, `E1`-`E3`); the test tier
  `nightly` is `full`.
* The cloud presets are renamed: `mmcloud` is `mm_cuopt_cloud`,
  `mmcloud_crossover` is `mm_cuopt_cloud_crossover`, `mmcloud_pyomo` is
  `pyomo_cuopt_cloud`, `mmcloud_pyomo_crossover` is
  `pyomo_cuopt_cloud_crossover`; a solve with a former name stops and names
  its replacement. `mm_engine` is replaced by `mm_solver` and `where`;
  settings stored by earlier versions are still read.
* `get_hf_token()` and `set_hf_token()` are removed: use
  `mmcloud::get_hf_token()` and `mmcloud::set_hf_token()`.

## Bug fixes

* About 130 fixes, most of which change results: unwired parameters now reach
  the solver, equations are no longer instantiated without data, `waf.fx` binds
  from both sides, `aeff$cout2ainp` / `cinp2aout` bind, sub-region emissions
  are counted, coarse supply keeps its cost, and `get_process_*(process = )` is
  honoured.
* A tax or subsidy without `@region` charged the coarse regions of a geoscale
  too, i.e. the roll-up of their sub-regions a second time. A commodity rate
  now applies at the commodity's own regions and a process rate at the regions
  the process is declared in; a region off that level is an error.
* `levcost()` of a storage sized a charger with no `inp2out` or `inp2stg` like
  the discharger, and a discharger with no `duration` at one hour of storage.
  Each is now sized by its own flow, as in the solved model.
* Auxiliary flows (`aeff`) of a process were lost when it gave the coefficient
  for all timeslices and another process gave the same coefficient for named
  timeslices.
* Capacity taxes and subsidies on a storage charged nothing unless it had
  discharger data. They are now levied by part (see above); a levy that
  selects storages and sets only `cap`/`ncap` is an error.
* A bound declared per region on one object and for all regions on another
  lost the second object's bound equation (e.g. a fixed storage duration).
* Content hashes of models with a geoscale are stable between R sessions
  (the S7 class object was hashed), so such models match their sealed
  versions in a new session.

## Documentation

* A rebuilt [pkgdown site](https://energyRt.org) with tutorial articles, and a
  runnable example on every public help page.
* The testing strategy (tiers, the `@covers` matrix, goldens) is documented in
  `tests/README.md` and `tests/TESTING.md`.

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
