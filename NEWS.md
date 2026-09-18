# energyRt (development version)

* On-disk stores write ~1M-row row groups instead of one per 32k-row record
  batch. Stored data.frames are smaller (1.3x on model-shaped data, 2.2x on
  a sorted hourly series) and scan faster; existing stores are unaffected
  until rewritten.

* `multimod_cloud` runs persist the COMPLETE job handle in `cloud_job.yml`
  (via `mmcloud::cloud_handle_write()`), including the output-bucket
  reference, so a finished job's solution is fetchable from any later
  session.


## Breaking changes

* Decoded `vTradeIr` names its endpoint columns `src` and `dst` on every
  backend. Pyomo and JuMP emitted `region`/`regionp`; GAMS and GLPK already
  used `src`/`dst`. Code reading `regionp` from a trade result needs updating.

* `scenario_artifacts()` gains a `kind` column and adds scenario-level rows for
  the derived tiers (`"reports"`, `"levcost"`) beside the `"run"` rows;
  `drop_solver_outputs()` acts on `kind == "run"` only.
* `prepare_for_sharing()` drops rendered reports unless `keep_reports = TRUE`:
  they re-render, and a PDF or DOCX can carry a path or user name where
  byte-level scrubbing is not safe.
* `delete_marked()` skips an entry that something still references, reporting
  `skipped_referenced` beside the existing `skipped_sealed`; `ignore_refs = TRUE`
  deletes it anyway, and an interactive session names the dependents and asks.
* `save_scenario(embed_model = NULL)`, the default, saves the model to the
  model store and references it instead of embedding a copy in every scenario;
  a name already holding different content is left alone and the model embedded.
* Storage aux capacity couplings are part-prefixed: `cap2ainp` / `cap2aout` /
  `ncap2ainp` / `ncap2aout` on a storage `@aeff` are now `out.cap2ainp` etc.
  The bare names error with a rename hint; technology `@aeff` keeps them.
* A run's solver files are written directly into `runs/<solve>/`, beside
  `run.yml`, instead of a `solver/` subfolder, and the per-solve metadata
  file is `solver.csv`. Existing scenarios still open.
* The Julia and Python backends exchange data as Arrow files. `data.RData` and
  `input/data.db` are opt-in through `solver_options$julia_highs_rdata` and
  `$pyomo_cbc_sqlite`; the `*_arrow` presets are retired.
* Arrow exchange needs `Arrow.jl` (Julia) and `pyarrow` (Python). Both are
  installed and checked by `en_install_julia_pkgs()` / `en_check_pyomo()`;
  `CSV` and `SQLite` are dropped from the Julia install.
* `arrow_format` splits into `storage_format` and `exchange_format`, likewise
  `*_compression` and `*_compression_level`. Storage keeps zstd-15; exchange
  defaults to `lz4`.
* 46 machinery helpers are no longer exported (314 → 268 exports) — the
  interpolation/mapping engine, the NEOS plumbing below `neos_ping()` and
  `neos_list_solvers()`, and small utilities. They remain as `energyRt:::`.
* `print.levcost()`, `print.levcost_list()`, `print.levcost_variants()` and
  `print.share_frontier_plots()` are registered as S3 methods; `print(x)` is
  unchanged, the direct `print.levcost(x)` call form is gone.
* The `ert` prefix is retired for `en`: registry class tag `en_registry`,
  report CSS `.en-*`, LaTeX colours `en_blue` / `en_gray`; custom report
  templates written against the old names need updating.
* Store folders are named by the object, not its hash (`models/UTOPIA/`), and
  updated in place. Old hash-named folders keep loading; `rehash = FALSE` keeps
  a recorded hash through a change you declare insignificant.
* Object names are validated at construction — letters, digits and underscore,
  starting with a letter. Scenario folders join their parts with dashes
  (`BASE-UTOPIA-s4_h24`).
* `problem.RData` is retired; `scen.RData` is the base problem's one home.
  Legacy files are read and folded in by `upgrade_scenario_layout()`.
* A storage's availability columns take the part prefixes: `@af$cinp.*` →
  `inp.af.*`, `cout.*` → `out.af.*`; `@weather$wcinp.*` → `inp.waf.*`,
  `wcout.*` → `out.waf.*`. Old names error and name their replacement.
* `commodity@geolevel` and `summand@geolevel` are now `@geoframe`, as are the
  `geolevel =` arguments of `newCommodity()`, `newSummand()` and `getData()`;
  `map_comm_geolevel()` is now `map_comm_geoframe()`.
* The variable-catalogue role `flow` is now `interregional`.
* Scenario-management operations are verb-first with no aliases:
  `load/save/add_to/find/refresh_registry()`, `drop_scenario_run()`,
  `upgrade_scenario_layout()`, `apply_ledger()`.
* `modInp@set` is removed (superseded by `@sets`); `@gams.equation` is now
  `@user_constraints` and `@costs.equation` is `@user_costs`. Scenarios saved
  with the old slots migrate on load.
* `interpolate_model()` errors on a weather profile named but not declared, and
  on a `geff` row naming an input group no commodity belongs to. Both used to
  be dropped silently.
* The UTOPIA world reuses the shared calendars: `utopia_annual`, `utopia_s4h24`
  and `utopia_m12h24` are retired for `annual`, `s4_h24` and `m12_h24`;
  `utopia_seasons` stays, relabelled `AUT` → `FAL`. Objectives shift slightly.
* `report_tbl()` enforces a 200-row cap in PDF/Word when no `max_rows` is given.
* The sampled daily calendar `calendars$d365_h24_subset_1day_per_month` is
  renamed `d365_h24_1dpm` (solver working paths embed the calendar name);
  existing scenario folders keep their names.

## New features

* `interpolate_model()` is about 6x faster on large models (a 41-node
  vintaged multi-year interpolation dropped from ~16 to under 3 minutes) and
  runs `data.table` on all cores for the duration of the call
  (`options(energyRt.threads = )` to override). Interpolated content is
  unchanged.
* Every `interpolate_model()` call appends its per-stage / per-map /
  per-parameter timing and memory readings (R heap, per-stage heap peak,
  process RSS and peak via `ps`) to a profile log — `interp_runs.csv`,
  `interp_stages.csv`, `interp_maps.csv`, `interp_params.csv` — under
  `set_profile_dir()`, else next to the operation log, else `<project>/logs/`;
  with no sink resolvable nothing is written.
* On-disk interpolation writes each parameter's store once at the end of the
  object loop instead of rewriting it on every append, removing the quadratic
  cost that made `ondisk = TRUE` slower than in-memory on large models.
* `clear_report_cache()` removes rendered reports — an object's own `reports/`
  folder, or the project tier for in-memory objects — listing them by default
  and deleting only with `dry_run = FALSE`. A sealed owner refuses.
* `store_dependents()` lists what references a stored model, repository or
  dataset instead of embedding a copy of it — the entries that would break if
  it were deleted, and whether each is pinned to a superseded version.
* New solver presets for GPU-accelerated LP. `solver_options$julia_cuopt`,
  `julia_cuopt_concurrent`, `julia_cuopt_pdlp` and `julia_cuopt_crossover`
  solve via NVIDIA cuOpt through `cuOpt.jl`. **Linux only** -- cuOpt ships no
  Windows build, and `libcuopt.so` must be on `LD_LIBRARY_PATH` before Julia
  starts. `julia_cuopt_pdlp` uses a first-order method with no factorization,
  so GPU memory grows linearly with the number of non-zeros; add crossover
  (`julia_cuopt_crossover`) when duals are needed, as a first-order solution
  alone carries a ~1e-4 duality gap.
* `solver_options$julia_highs_pdlp` and `julia_highs_hipdlp` select the HiGHS
  first-order solvers. GPU execution requires a `libhighs` built with
  `-DCUPDLP_GPU=ON`; a stock build runs them on the CPU. Neither returns a
  basic solution or supports crossover — use the simplex or interior-point
  presets when duals are needed.
* The JuMP backend records the solver's `termination status` in
  `output/log.csv` and skips result export when no primal solution exists,
  instead of failing while reading variable values.
* New sampled calendar `calendars$d365_h24_1dps`: one representative day
  per season at hourly resolution (96 timeslices).
* `solver_options$pyomo_mps` writes the problem as an MPS file with
  energyRt's own variable and constraint names and stops before solving —
  for handing a model to an external solver (a GPU LP solver, a remote
  machine) whose solution comes back later.
* `read_solution()` reads a run solved outside energyRt: a solution decoded
  into the run's `output/` is picked up with the exchange format recorded by
  the run itself, and the run record's status and objective are updated. A
  run with no primal solution reports the recorded termination status
  instead of a missing-file error.

* Weather transforms: one shipped data stream can serve many derived series.
  A weather object carries named functions in `misc$transform`; a
  technology/storage/supply weather link selects one in its `transform`
  column. `materialize_weather()` inspects a derived series and
  `register_weather_transform()` adds session-wide transforms. Region
  aggregation refuses transform-bearing weather.
* `scenario_artifacts()` lists what each solve left on disk — whether its
  solution was imported, what the solver scratch costs, and which runs are
  candidates for clean-up.
* `drop_solver_outputs()` removes the regenerable part of a solve — model
  source, exchange input, solver logs, and the raw output once the solution has
  been imported — keeping the run record and the solution. Dry-run by default.
* `strip_user_info()` removes the machine a scenario was made on — stored
  absolute paths, the solver command line, and, with the default
  `scope = "share"`, the host and user of every run; `scope = "store"` keeps it.
* `prepare_for_sharing()` writes a cleaned, trimmed copy of a stored scenario
  and reports anything that will not travel with it.
* Folding works with GAMS: a dense build (`sparse = FALSE`) may be folded, and
  the GAMS writer substitutes the artificial member like the other backends.
* `validate_scenario_parameters()` is exported and also checks that every
  variable has its governing constraints, that availability bounds are
  present, and that a calendar's row order agrees with its timeframe columns.
* Storage aux flows can couple every part's capacity: `inp.cap2a*` /
  `inp.ncap2a*` (charger) and `stg.cap2a*` / `stg.ncap2a*` (reservoir) join
  the `out.*` couplings on all four backends.
* `draw()` of a storage shows the charger, reservoir and discharger as boxes
  with their own parameters, the ratio triangle (`inp2stg`, `duration`,
  `inp2out`) as labelled links, and `fullYear` in the header.
* A storage gains `@inp2stg`, the charging C-rate (charging per unit of
  storing capacity, 1/hours); with `@duration` and `@inp2out` any two ratios
  fix the third, and `newStorage()` refuses a contradictory fixed triple.
* `newImport()` and `newExport()` take `region =`, like every other process
  class; `import` and `export` objects gain the matching `@region` slot.
* `solve_by_sample()` solves a model on samples of its calendar — consecutive
  blocks tiling the year, disjoint random draws, or bootstrap draws — storing
  each as a variant of one scenario.
* Calendar samples are replicates, not pieces: each is annualised and estimates
  the whole year, so `sample_summary()` reduces them to quantiles and their
  objectives must not be summed.
* `calendar_samples()` builds the sample specification by drawing whole members
  of a calendar level (seasons, weeks, days); the table can be edited and
  passed back.
* `aggregate_model_regions()` builds a coarser model by mapping regions onto
  a geoframe of a `geoscales::Geoscale`: extensive quantities sum, intensive
  ones take a size-weighted mean, intra-region trade corridors drop.
* `solve_by_region()` solves a multi-region model one region or group at a
  time, each run a variant of one scenario; with `trade = "none"` the
  objectives add up to the full model's (the result's `additive` field says so).
* A severed trade route can be replaced by a stepped price curve instead of a
  flat price: `boundary_prices` gains `nsteps`, `price_lo`, `price_hi` and
  `imp.lo`/`exp.lo` columns.
* `boundary_window()` builds a `boundary_prices` table whose quantity is
  `share` of each region's demand per timeslice; `price` is required.
* `solve_guided()` solves a too-large model in stages (endpoint problems seed
  capacity targets for one final full solve); `guided_gap()` reports the gap
  to perfect foresight; primitives `solution_targets()`, `apply_targets()`.
* Every energyRt object has a `report()` method: 14 element classes render
  datasheets from per-class templates; `report(mod, name = )` finds an element
  of any class (`class = ` narrows); `misc$report` sets the default template.
* `calendars` ships the mainstream timescales designs — `m12`, `m12a`, `q4`,
  `s4`, `s4_h24`, `m12_h24`, `wd7_h24`, `w52_h24` — plus three sampled
  calendars whose `year_fraction < 1` solves partial years natively.
* The registry gained `newRegistry()` (`load_registry()` no longer invents a
  missing registry), a `variant` column, and `getScenario()` / `getObject()`
  methods fetching by `type/name` with `run =` selecting the run.
* `levcost(x, by_variant = )` replaces `levcost_by_variant()`, taking `TRUE`,
  `"npv"` or `"components"`, either while computing or on a result in hand.
* Store entries have a lifecycle: `seal_*()` / `unseal_*()` freeze an entry,
  `mark_delete(x, importance =)` queues it and `delete_marked()` (dry-run by
  default) removes marks up to a threshold.
* `set_path_builder()` overrides how folder names are derived — `scenario_dir`,
  `store_entry`, `run_label`, or the `slug` primitive. See `?path_builders`.
* `utopia_profile()` generates deterministic synthetic shapes on any calendar —
  step staircase, sine, cosine or hexagonal trapezoid — with per-region phase or
  amplitude variation.
* The "unit model": `utopia$modules$unit` kits (`U1`, `U3`) where every input is
  1 on the symmetric `unit_s4` calendar, so each variant's objective is a small
  hand-checkable integer.
* UTOPIA add-on modules in every `electricity` kit — `GAS_CURVE` (3-step supply
  curve), `EWIN_SITES` (two wind site grades), `ENUC_VINT` (two nuclear
  vintages) — replace their base counterpart via `add(mod, ., overwrite = TRUE)`.
* `solve_myopic()` solves a horizon window by window. The primitives are
  composable: `horizon_windows()`, `solution_ledger()`, `apply_ledger()`.
* A comparison layer: `compare_scenarios()` (scenarios or recorded runs, with
  `print()`, `autoplot()` and `report()` methods), `compare_models()`
  (declaration diff) and `compare_inputs()` (interpolated-input diff).
* `report(scen, template = "full")` renders a page-broken document with
  branding, time and geography pages, result choropleths and stacked bars.
  `report(mod/scen, template = "summary")` renders one-page glimpses.
* Container reports split by role: the model report documents assumptions and
  data, the scenario report the results of a solved scenario. Both render to
  HTML, PDF and Word.
* Model and scenario reports follow user-defined process groups
  (`report(groups = list(Coal = "_coal_"))` or `misc$report_groups`), else a
  sample of up to 12 processes; `groups = "topology"` groups by structure.
* Faceted autoplots cap at 16 panels (a caption says how many were dropped);
  report figures size their height by the facet layout
  (`report_fig_height()`).
* Report figures carry pandoc captions instead of in-plot titles: new
  `report_fig()` emitter (strips `title`/`subtitle`, auto height, caption)
  and `report_img(caption = )`.
* Supply charts: `autoplot(supply, style = "bar")` draws availability and
  cost by region (curve steps stacked in order); `style = "regions"` draws
  region bars, unlimited availability as a translucent full-height bar.
* Levelized-cost comparison charts: `report(model)` and `report(scenario)`
  with `levcost = TRUE` add overall and per-group comparisons; a process
  datasheet reported from a container compares it with its structural peers.
* Model reports summarise weather factors (mean/min/max per factor and region)
  and draw calendar heatmaps per factor family (6 best + 6 last clusters) and
  per demand; `autoplot(demand, style = "heatmap")` draws the latter directly.
* Report branding: `misc$logos`, `misc$figure` and scenario `misc$badges`, with
  `report(logos = , figure = , badges = )` overrides. New helpers
  `report_img_row()` and `report_pagebreak()`.
* The report-template helpers are exported as the `report_*` family for use in
  custom templates.
* `report_templates()` lists shipped templates; resolution is class-scoped, so
  containers and processes can share template names.
* `report()` works inside a knitr chunk — the nested render no longer collides
  with the outer document's chunk labels.
* `report_tbl()` gains `max_rows` and `scroll`: scrollable tables in HTML,
  a capped table with a "... K more rows" footer in PDF/Word.
* `getMix(top_n = )` keeps the N largest processes and lumps the rest into
  `"Other"`, mass-preserving. `autoplot()` defaults to `top_n = 12`.
* `plot_map()` maps any solved variable carrying a region dimension via `name =`
  and facets by year. New `plot_geoscale()` draws a geoscale as a membership
  map, a layered cabinet stack, or an icicle.
* `levcost()` prices `storage` (LCOS) and `trade` (LCOT) as well as
  `technology`, closed-form and solver engines held together by a parity test.
  Containers price all three; `classes =` narrows.
* `asSupplyCurve()`, `asImportCurve()` and `asExportCurve()` turn a single price
  per `(region, year, timeslice)` into a stepped curve.
* Transmission losses can be quadratic, approximated by capacity tranches.
* `newACLine()` and `newDCLink()`, with an opt-in Kirchhoff voltage law.
* `@trade$af` rates a route's flow relative to its capacity, alongside the
  absolute `ava.lo/up/fx`.
* A filtered geoscale passed to `interpolate_model()` produces a sub-territory
  model — the spatial mirror of calendar sampling.
* `"TOTAL"` in a `vintage` or `cluster` column works on flow bounds and
  availability factors, not only `@capacity`.
* `vTradeIr` and the `*RetiredNewCap` variables can be used in custom
  constraints.
* Scenario storage: a persisted per-project registry, one folder per solve under
  `runs/<variant>/<solve>/`, a content-addressed model store, a shared
  repository store, and own-problem variants via `solve_scen(variant = )`.
* Registered objects are accessible by name — `getScenario("base")`,
  `getModel()`, `getRepository()`, `getDataset()`, `load_scenarios()`;
  `getData()` takes names and environments; `open_project(path)` anchors a session.
* A dataset store completes the storage tiers: `save_dataset()` /
  `load_dataset()` keep a large table, a geoscale map or a recorded generating
  call in a content-addressed folder; the other savers gain `embed_datasets =`.
* Reports render into the object's own folder; an unchanged report is not
  re-rendered (`force = TRUE` overrides).
* `levcost()` results are cached on disk, keyed by object content and
  assumptions; `report()` shares the cache. `clear_levcost_cache()` empties it.
* `object_hash()` works for any energyRt object.
* An optional operation log: `set_log_file()` makes `interpolate_model()`,
  `solve_scenario()` and `solve_myopic()` append one CSV line each; `read_log()`
  reads it back. Off by default.
* `add()` dispatches on a loaded registry, as a shorthand for
  `add_to_registry()`.
* New `get_weather()`, the weather-side counterpart to `get_region()`.
* `summary()` on a model reports its regions and its objects by class.
* `run.yml` records a solve's memory footprint (`mem_mb`/`peak_mb`) and
  `saved`/`updated` stamps, surfaced by `scenario_runs()`.

## Deprecations

* The UTOPIA datasets are now elements of one list — `utopia$weather`,
  `$demand`, `$stock`, `$modules`, alongside `$map` and `$geo`. The four
  standalone datasets still work and are removed in v0.90.
* All deprecated names warn with the version that removes them ("won't be
  available starting energyRt v0.90") and are collected under one help page,
  `?energyRt-deprecated`.
* `find_registry()` is now `find_in_registry()` — it filters rows inside a
  loaded registry, next to `get_registry_file()`.
* `read_procspec()` is now `read_process_spec()`.
* `registry_exists()` / `registry.exists()` now report whether the project has a
  registry file at all; the `name` argument is accepted and ignored.
* `solve_mod()` / `solve_scen()` are deprecated aliases of `solve_model()` /
  `solve_scenario()`, which hold the implementations.
* `make_scenario_dirname()` is replaced by `set_path_builder(scenario_dir = )`,
  since folder naming is automatic.
* The mosox back-end experiment moved to `drafts/`; it was not functional.

## Bug fixes

* A solution reconstructed from a solver `.sol` file wrote `year` (and
  `yearp`, `yeare`, `yearn`, `year2`) as text, where every other route
  writes integers. Output from the two routes could not be joined on the
  year, in nearly every table, and the mismatch was silent.
* Value-map domains (`mTechVarom` and siblings) treat a `NA` key as a
  per-row wildcard: a technology's year-unkeyed cost row is no longer
  dropped from the map when another technology keys the same cost by
  year, which silently removed the cost from the model.

* `save_scenario()` no longer errors on a scenario interpolated with
  `ondisk = TRUE` and never solved (`@modOut` is `NULL` there).

* `interpolate_model(ondisk = TRUE)` writes each parameter store under
  `modInp/parameters/<name>`, the location `load_scenario()` rebuilds paths
  to; previously the tables landed flat under `modInp/<name>` and were
  unreachable after a reload. Existing flat stores still load (the rebase
  falls back to the flat location).

* Multi-year models understated capital charges by the milestone length:
  annuities (from `eac` or `invcost`) were charged on the annual build rate
  instead of each vintage's standing capacity — one fifth of their value on
  5-year milestones. Fixed in all backends; single-year and overnight models
  are unchanged. Re-solve multi-year scenarios solved with earlier versions.
  See `dev/multiyear-capital-charge-bug.md`.

* On a sampled calendar, ANNUAL-timeframe quantities are now full-year
  magnitude: annual caps and emission totals bind at face value, and every
  commodity gains totals at each coarser timeslice level and, with a geoscale
  attached, each coarser region level. Full calendars are unchanged. A
  serialized calendar built under the old convention is refused at
  interpolation — rebuild it with `newCalendar()`. See
  `dev/annualized-annual-convention.md`.

* Solution CSVs written by the GLPK backend carry 10 significant digits
  (was 6 decimal places).

* `fold = TRUE` could silently drop capital cost from the objective: a
  parameter whose folded dimension was only *partially* wildcard kept a raw
  `NA`, which is not a set member, so lookups missed and took the parameter's
  default. Partial columns are now materialised to explicit members before the
  write, and `apply_fold_artificial()` refuses to write a surviving raw `NA`.
  See `dev/fold-partial-wildcard-bug.md`.

* `interpolate_model()` rehydrates a model whose large slots live in the
  model store (`obj2mem()`) instead of silently interpolating the empty
  placeholders — a store-loaded model produced a plausible-looking scenario
  with no demand and no weather.

* `verify_solution()`'s `objective` check no longer errors out (and is no
  longer reported as skipped) when a parameter and its gating map disagree
  on a key column's type, as `pDiscountFactor` and `mvTotalCost` do on `year`.
* `save_model()`, `model_hash()` and the other store hashes accept objects
  serialized before a slot was added to their class; a model holding a
  pre-`inp2stg` `storage` used to fail to save.
* `subset_model_regions()` (and so `solve_by_region()`) prunes `@weather`
  references whose object is dropped with the regions outside the sample;
  narrowing a clustered-resource model to one region used to fail validation.
* A solved or written folded scenario no longer carries the artificial set
  members (`ANYREGION`, `0`); `getData()` on such a scenario unfolds every
  folded dimension, including `year`.
* Folding no longer depends on the order of the folded dimensions, and a
  parameter folded on both `tech` and `region` reads back correctly.
  `getData()` and `verify_solution()` unfold every foldable dimension.
* Folding `trade` no longer empties a trade parameter whose route endpoints
  are wildcards.
* A user constraint's `rhs` is interpolated over years per region (and any
  other key), not as one column across regions.
* User-constraint and user-cost parameters (`pCns*`, `pCosts*`) are never
  folded.
* `write_script()` substitutes the folded wildcard with the artificial set
  member, as `solve_scenario()` already did; a written folded scenario no
  longer silently takes parameter defaults.
* `autoplot()` of a `demand` draws its `"line"` and `"heatmap"` styles with the
  same profile engine as `weather`; the old line view errored on demands with
  years and faceted one row per day on daily calendars.
* Profile lines and areas (`demand`, `weather`) colour the coarse time level
  with a viridis gradient, continuous when the level is numeric.
* `plot_process_windows()` no longer draws a process backwards: a process not
  investable within the horizon shows its exogenous stock instead, and a
  missing `olife` runs the operating tail to the end of the horizon.
* A zero discount rate with no operational life is refused instead of silently
  charging nothing for capital. Supplying `@invcost$eac` directly is
  unaffected.
* The solver exchange checks its longest prospective file path before writing
  and stops with the directory and symbol at fault; exchange directories are
  created recursively.
* A process declared in a region where a commodity it consumes is unavailable
  is dropped there instead of producing from nothing; the dropped cells are
  reported and listed in `scenario@misc$region_gaps`.
* A `supply` scoped by the `region` column of its data, rather than by
  `@region`, is no longer available in every region of the model.
* A `weather` object scoped the same way no longer zeroes availability in
  regions it was never declared for.
* An interpolation that failed part-way left bulk parameter writing on, and
  every later solve in the session silently returned objective 0; the
  `en.bulk_param_write` option is now restored on every exit path.
* `verify_solution()$ok` no longer reports `TRUE` when every check was skipped;
  new `n_ran` / `n_skipped` fields, and `print()` says "NOTHING VERIFIED".
* `validate_scenario_parameters()` issues carry a severity: a populated map
  whose source parameter is empty is structural and errors whatever `action`
  says; the issue count is always announced with `message()`.
* `interpolate_model()` asserts no parameter is left unmaterialised after the
  final flush, naming the offenders.
* `read_solution()` errors when none of the declared variables produced an
  output file, naming the extension and the active `import_format`.
* `add()` accepts a repository holding more than one object.
* `report(levcost = TRUE, by_variant = )` no longer drops the levelised-cost
  section: `FALSE` reports the display instance alone, the default keeps the
  per-variant tables.
* A user `constraint` whose `for.each` years all fall outside the solved
  horizon is dropped with a warning instead of generating unusable solver code.
* A `supply` at a commodity's coarse `@geoframe` level was silently costless;
  it is now priced, and rest-of-world import/export at a coarse level are
  refused loudly.
* `newCommodity()` without `timeframe =` no longer crashes interpolation; the
  empty slot resolves to the calendar's finest timeframe.
* `add(mod, x, overwrite = TRUE)` replaces an object of the same class and name
  instead of appending a duplicate next to it; without `overwrite` the collision
  is an error.
* Calendar chronology follows the timetable's row order; mid-level timeslices
  are no longer ordered alphabetically.
* On-disk parameter stores no longer default to CSV; they follow
  `storage_format`. Existing CSV stores keep loading.
* A parameter write-back could make its store unreadable by writing CSV beside
  `.arrow` files; the store's recorded format is now authoritative.
* An atomic (single-column) slot was always written as CSV, mixing codecs inside
  a store.
* `import_format = "parquet"` returned an empty scenario silently. Pyomo honours
  parquet on both legs; Julia refuses it at write time.
* Tables with no rows are no longer written.
* `obj2mem()` is quiet by default and reports a progress bar; `verbose` is
  passed down the recursion.
* `subset_model_regions()` reports one summary line — regions kept, objects and
  routes dropped — instead of a message per item; `verbose = TRUE` lists them.
* Interpolation no longer re-deduplicates a parameter's whole table on every
  object that writes to it. Set `options(en.bulk_param_write = FALSE)` for the
  previous path.
* The process/commodity level check no longer refuses models whose objects span
  many regions.
* Conflicting-bounds detection names the parameter again instead of dying on
  "comparison of these types is not implemented".
* Parameter writes no longer re-coerce every year and character column.
* Variant expansion respects `verbose`; because `verbose` defaults to off, the
  generated-constraint and variant-expansion messages no longer appear by
  default. When shown, constraints are counted by family rather than listed.
* `get_region()` returns a model's declared regions.
* `load_scenario()` on a saved-but-never-solved scenario no longer warns about
  rebasing on-disk paths.
* `getData()` no longer returns an empty frame for a scenario whose solve was
  not proven optimal: a stored incumbent is served with a warning naming the
  stage.
* `solve_scenario(transient = TRUE)` deletes the throwaway solver directory.
* `levcost()`'s mini-models solve in a scratch dir instead of creating scenario
  folders that `refresh_registry()` indexed as real scenarios.
* `inp.eac` / `stg.eac` give a storage part its own capacity — they priced the
  charging or storing part without bounding it, so it came out free.
* `inp.fixom` / `stg.fixom` reach the objective; a storage priced only on its
  charger or reservoir paid no fixed O&M.
* `eqTechPhaseOut` / `eqStoragePhaseOut` are gated on `mTechNew` /
  `mStorageNew`; a phaseout window past the investment window no longer
  crashes Pyomo. Objectives unchanged.
* `scenario@status$solved` is set when the solution is read.
* Per-part `inp.` / `stg.` `wacc` and `payback` are honoured; the annuity always
  read the `out.*` columns.
* `technology@af$rampup` / `$rampdown` reach the solver on all four backends;
  `cap2act` is no longer applied twice and the Up/Down equations are no longer
  orientation-swapped.
* Per-column `config@defVal` / `config@interpolation` overrides are read at
  interpolation instead of being copied onto the scenario and ignored.
* The `costs` class is wired end to end; user cost terms reach the objective
  via `eqTotalUserCosts`.
* An unknown summand field in `newConstraint()` is an error instead of being
  dropped — it quietly turned a per-technology cap into a global one.
* A storage flow into a coarser-timeframe commodity reaches the balance; the
  totals summed only at identical timeslices.
* A one-sided `inp2out` or `duration` range no longer resurrects the binding
  default of 1 on its open side, which made `inp2out.lo = 2` infeasible.
* A supply restricted to one region no longer leaks into every other region.
* Multi-level regions were inert — the hierarchy was read under the old
  `parent_level` / `child_level` column names.
* A cost declared at a coarser geoscale level never reached the objective.
* `add()` on a model worked exactly once.
* A scalar `mult` on a custom constraint summand was silently discarded.
* Exogenous stock could phase out and retire at the same time; `capacity$stock`
  reads as the fleet still standing.
* Trade phase-out is reported, not only computed; GAMS declares retirement
  non-negative.
* A moved scenario folder loads: stored paths are rebased onto the folder being
  read.

## Documentation

* New article *Reports and levelized costs* tours the reporting layer and the
  report/levcost caches.
* One `trade` object is one shared throughput budget — `eqTradeCapFlow` sums
  every route against its single capacity, so a network in one object is not a
  set of independently rated lines.
* Trade costs are a rate per endpoint region: capacity is region-free, but
  `invcost`, `fixom`, `retcost` and `eac` are region-indexed and each named
  region pays its own rate on the whole capacity.

# energyRt 0.80 (development) — the time dimension is now `timeslice`

## Breaking changes

* Stack-wide rename `slice` → `timeslice`, matching the TIMES/OSeMOSYS
  vocabulary and reading unambiguously next to `region`. It covers the set/index
  family in all four backends, every `mSlice*`/`pSlice*` symbol, the
  `ANYSLICE` wildcard, the solution CSV column, and the S4 `calendar` slots. A
  shim accepts the old `slice` name in user data with a once-per-session
  warning.
* `getData(timeframe =)` defaults to `"highest"` (native resolution), not
  `"lowest"`. An 8760-slice series used to come back as a single annual number
  with nothing to signal it. Code relying on annual totals must pass
  `timeframe = "lowest"`.
* `storage` role slots declare, parameter slots parameterise. `@input`,
  `@output` and `@storage` keep `comm`, `unit` and `cap2act`; every capacity and
  cost lives in `@capacity`, `@invcost` or `@fixom` under a part prefix —
  `out.` (discharger, power), `inp.` (charger, power), `stg.` (reservoir,
  energy). Old spellings are errors naming the column to write.
* Output-side parameters are renamed to agree with the variables:
  `pStorageCap` → `pStorageOutCap`, `pStorageInvcost` → `pStorageOutInvcost`,
  and so on for all ten families.
* `vStorageCap` / `vStorageNewCap` are now `vStorageOutCap` /
  `vStorageOutNewCap`; the old names resolve to nothing rather than returning
  power where energy is meant.
* `vStorageStore` is now `vStorageLevel`, with `eqStorageStore` →
  `eqStorageLevel` and the maps following. Numbers are unchanged.
* `storage@cap2stg` is now `@duration` (`pStorageCap2stg` → `pStorageDuration`);
  `cap2stg =` warns once per session. `ncap2stg` in `@aeff` is unrelated and
  unchanged.
* `storage@charge` is now `@startLevel` and the value is **annual**, added once
  per cycle at the cycle's first timeslice. It carries no `timeslice` column;
  one on a deprecated spelling is dropped with a warning.
* `eqStorageClear` is now `eqStorageOutLevel`; the dead `eqStorageClean`
  placeholder is removed.
* `plot_demand()`, `plot_weather()` and `plot_process_windows()` are no longer
  exported — all three are reachable through `autoplot()`.
* Pyomo-Abstract is retired to `drafts/`; asking for it raises an error naming a
  Concrete option. No shipped solver option ever selected it, and it had fallen
  behind three refactors.
* `@sharedThroughput` is removed, having been added and withdrawn in the same
  cycle. It never bound for a single-commodity storage and compared incommensurable
  units for a multi-commodity one. Archived with the measurements in
  `drafts/storage-shared-throughput.R`.
* `data/utopia_modules.rda` was regenerated: bundled datasets serialise S4
  objects with the class definition of their time, so objects saved before the
  storage renames must be rebuilt from `data-raw/`.

## New features

* A storage names a commodity per role: `@input$comm` fills the store,
  `@storage$comm` is what it holds, `@output$comm` is what it releases. One
  shape covers a battery, a hydrogen store and a reservoir; `@seff$inpeff` /
  `$outeff` become cross-commodity conversion factors. The fused hydrogen
  storage reproduces the three-object decomposition exactly in 149 variables /
  151 constraints instead of 251 / 257.
* A storage's energy capacity is its own variable, `vStorageStgCap` (with
  `vStorageStgNewCap`), measured in the commodity's unit, with its own stock,
  bounds and costs on `@storage`. `@duration` becomes a bound on
  `(storage, region, year)` — `.fx` ties energy to power, `.lo`/`.up` let the
  model choose.
* A storage sizes its charger separately: `vStorageInpCap`, with `@inp2out`
  linking the two ratings the way `@duration` links energy to power. The default
  `[1, 1]` keeps the two sides symmetric.
* `@output` accepts the same capacity and cost columns as the other two parts
  and folds them into `@capacity`/`@invcost`/`@fixom` at construction; supplying
  both forms is an error.
* `@input$cap2act` / `@output$cap2act` make a storage capacity a **rate**. It
  defaults to 8760, so a capacity reads as commodity per hour on any calendar;
  the flow bounds previously meant "per timeslice", so refining an hourly model
  to 4-hourly silently quartered every store. The storing side deliberately has
  no `cap2act`.
* The charger and reservoir gained the parameters only the discharger had
  (`ret.*`, `wacc`, `payback`, `retcost`). These are declared and inert —
  storage retirement has no equation in any backend.
* `storage_duration(scen, width = )` splits a storage level into duration bands
  by nested floors, so the bands partition the level with no remainder and sum
  back exactly. Verified against a brute-force definition.
* `@invcost$payback` works on GAMS, Julia and Pyomo-Concrete, not just GLPK.
  `eqXCap` keeps `pXOlife` everywhere — only the cost window moves.
  Pyomo-Abstract still refuses it.
* `commodity@property`: a tidy table of physical properties — heating values,
  density, molar mass, composition — with uncertainty columns. It is reference
  data; no solver template sees it. `commodity_properties()` lists recognised
  names, `commodity_property()` reads one value.
* `newCommodity()` gains `image =` and `icon =`; `object_image()` resolves
  either. `report()` defaults `image_file` from the object's own `misc$image`.
* `levcost()` computes analytically — no solver required. New
  `method = c("auto", "analytic", "solve")`; `"auto"` prices analytically
  whenever the technology qualifies and falls back with a message naming the
  reason. A vintaged or clustered technology is priced per cell directly.
* Group shares report every corner solution: the analytic result evaluates all
  vertices of the input and output share polytopes and returns them in
  `$frontier_vertices`.
* Per-vintage levelized costs in reports: a "Levelized Cost by Vintage" section,
  a Vintages table and per-vintage cost tables. Detail figures show one instance
  (newest vintage, first cluster), labelled in the section header.
* `report_generic.Rmd` is rebuilt on the vehicle two-column layout with one
  sizing system — fractions of the text width shared by HTML and LaTeX, ggplots
  rasterised at 150 dpi through one helper. Word output renders through a
  Word-safe branch.
* `autoplot()` on objects draws points for given data and lines for the
  interpolated series, with `interpolate = TRUE` and a new
  `show_defaults = FALSE` that draws mapped-but-unset parameters at their
  defaults.
* `autoplot.weather()` / `plot_weather()` gain `region` — pass a count or names
  to keep a legible subset of a 41-region model.
* `techspec` reads and writes JSON as well as YAML; the process designer gained
  a "Save JSON" button.
* `storage@input`, `@output` and `@storage` take a `unit` column, carried for
  reporting and `convert()`. It never reaches the solver.
* New data `vre_cf` — 8760 hourly capacity factors for one wind and one solar
  resource from MERRA-2 via `merra2ools`, keyed in the `d365_h24` vocabulary —
  and `vre_storage_duration`, the precomputed decomposition of a battery in a
  full-year hourly model.
* New `theme_energyRt()`, and `plot()` delegates to `autoplot()` for every class
  that has one.

## Bug fixes

* `storage@fullYear` had no effect — every storage cycled within its parent
  timeframe. `mStorageFullYear` was built and declared in all four backends but
  referenced by no equation, so multi-day and seasonal storage were not
  representable. Results change for any model with a calendar three or more
  levels deep; set `fullYear = FALSE` to keep the old behaviour.
* A storage requesting `fullYear = TRUE` on a calendar with no year-wide
  successor map now falls back to the parent-timeframe cycle with a warning
  rather than dropping out of the balance entirely — which left its level
  unconstrained by history.
* Each `@seff` coefficient reaches its own role's commodity. `ob2mi()` assigned
  into a shared frame, so whichever parameter went first stamped `comm` and the
  other two inherited it — an EV's kilometres-per-kWh was filed under
  electricity and the motor ran at efficiency 1.
* `mStorageInpTot` / `mStorageOutTot` followed the level's commodity, so a
  hydrogen store never appeared in the `ELC` balance and sat unused at a
  valid-looking optimum. Each total now follows its own flow.
* The stored commodity never reached `mCommReg`, so `mvStorageLevel` emptied and
  the storage lost its level variable altogether — built, solved and reported,
  storing nothing.
* `vStorageInp` / `vStorageOut` were declared over `mvStorageLevel` and reported
  zero flow for any storage whose level commodity differed from what it
  exchanged.
* `storage_duration()` returned zero rows with no message on month-based
  calendars — it called `tsl2dtm()` without `mday`. It now stops with the format
  it read when a level genuinely cannot be dated.
* `tsl2dtm()` died with `object 'dtm' not found` for formats it has no branch
  for; it returns `NULL` instead.
* `draw()` on a storage drew no arrows unless `@seff` was populated — the
  commodity frame was cross-joined with a slot whose prototype has zero rows.
  Arrows now come from the roles.
* `draw()` printed one label per region: a technology's `@ceff` holds a row per
  region, so a 41-node model drew 36 stacked labels on one arrow. Values collapse
  to a single number or a `min-max` range.
* `draw()` showed no duration label once `@duration` became a bound.
* `autoplot()` on process objects reported "No year-indexed data to plot" for
  virtually every object: `year = NULL` was passed into `getData()`'s `...`,
  where a NULL selector matched nothing. NULL/empty filters are now ignored.
* The parameter registry behind `getData(interpolate = TRUE)` was silently
  empty — it read `.modInp@parameters`, but the baked-in `.modInp` is a plain
  YAML list, so every lookup fell back to generic interpolation.
* `getData(object, interpolate = TRUE)` crashed on any empty data-frame slot;
  demand plots dropped region-NA rows on aggregation.
* `print()` on a `getUnits()` result and on a commodity was dead code: `@export`
  alone does not register an S3 method on an S4 generic.
* Empty columns are dropped throughout the report templates, and NA cells render
  as a dash (`na.string=` was never a `kable` argument).
* PDF soundness: correct LaTeX escaping (the old `fixed = TRUE` patterns never
  matched `$ ^ { }`), forward-slashed image paths in HTML, single kable
  escaping, and aligned HTML/LaTeX column fractions.
* `report()` passes only the params a template declares, so custom templates
  keep rendering as `report()` grows new ones.
* On the `levcost()` solve path the annual capacity factor collapsed from
  `@weather` never reached the solver, so weather-driven technologies were
  priced at full availability.
* `newStorage(commodity = , storage = list(invcost = ))` silently dropped the
  `invcost`: the commodity shorthand replaced a supplied part frame instead of
  adding to it.
* `cap2stg` could never work — the formal default was a bare `duration = 1`, so
  the deprecation path always fired.
* `prod()` over several weather factors is verified on GLPK and Julia/HiGHS; the
  dead `write_jump()` guard refusing multi-factor models is retired.

## Documentation

* New article **Units**: where each unit is declared, how `convert()` moves
  between dimensions, how `commodity@property` lets a conversion cross them, and
  why money needs a year attached.
* The **Storage** article is restyled to the modelling convention and extended
  to the slots it previously skipped, with a slot map of the whole object.
* **Model bricks** corrected: `cap2act = 8.76` was documented as 8.76 GWh per GW
  per year, wrong by a factor of 1000. The text now states the unit basis
  explicitly.
* Slot documentation for `storage@fullYear` rewritten — the `TRUE` and `FALSE`
  branches were described with the same sentence. `technology@fullYear`,
  documented as ignored, in fact governs ramping.
* A coarse calendar remains inadequate for storage even with rate-based
  capacities: on slices longer than the discharge duration the power bound goes
  slack and the cycle count collapses. A store whose cycle is shorter than one
  timeslice is understated, not approximated.

# energyRt 0.74.0.9000-dev

## Breaking changes

* Package options are prefixed `en.` — `options(verbose = )` becomes
  `options(en.verbose = )`, and so on. The old names collided with base R. The
  documented API is the `get_*()` / `set_*()` functions, which are unchanged.
* Environment variables are prefixed `ENERGYRT_`. The old unprefixed names work
  for one release and warn once; `NEOS_EMAIL` deliberately keeps its bare name.
* `en.verbose` is a level (`0`, `1`, `2`, …) tested with `isVerbose(level)`; the
  separate `energyRt.verbose` option is deprecated for one release.
* Dead model code removed: the LEC family (`eqLECActivity`, `meqLECActivity`,
  `mLECRegion`, `pLECLoACT`) and `mvTradeCost` / `mvTradeRowCost`. An audit of
  all 237 mapping parameters found nothing could ever fill them. Objectives are
  unchanged; every written `.dat` loses five meaningless lines.

## New features

* `en_config_write()` / `en_config_read()` persist settings to a config file,
  applied at load without overriding an option or environment variable already
  set. It writes to `tools::R_user_dir("energyRt", "config")` — CRAN policy does
  not permit writing in the user's home filespace — and backs up legacy files.
* `en_config_show()` prints every option with its value and where that value
  came from. The first thing to run when a solver is not found.
* `set_solver_path()` / `get_solver_path()`, the generic form of the six
  per-solver setters, which are now thin wrappers.
* `?energyRt-options` documents all seventeen options, generated from the
  declarations so it cannot drift.
* `en.debug` gained `isDebug()` and now gates internal consistency warnings that
  used to be silent — or, in one case, called `browser()`.

## Bug fixes

* An unservable demand warns instead of stopping interpolation, so an incomplete
  model can be inspected. Restore the old behaviour with
  `options(en.model_checks_stop = TRUE)`.
* `get_scenarios_path()` was defined twice, the second silently shadowing the
  first.
* `.call_solver()` restored the working directory only from its error handlers,
  so a non-error early return left the session inside the solver run folder.
* `interpolate_model(ondisk = TRUE)` referenced `mi_path` before assignment.
* The default solver reported `lang = "glpk"` while `solver_options$glpk`
  reported `"GLPK"`, so the two compared unequal.
* Two test scripts changed the default solver without restoring it.

# energyRt 0.70.5.9000-dev

## Breaking changes

* A class's main data slot is now named after the class: `demand@dem` →
  `@demand` (and its value column), `supply@availability` → `@supply`,
  `import@imp` → `@import`, `export@exp` → `@export`. `dem =`, `imp =` and
  `exp =` still partial-match; `availability =` fails loudly.
* The `sub` class is now `subsidy`, with `@sub` → `@subsidy` and
  `newSubsidy()` as the constructor (`newSub()` remains an alias). The modInp
  set dimension and the `pSub*` parameters are unchanged.
* `geo_map()` is now `plot_map()`; `geo_*` is `geoscales`' prefix.
* The first argument of every `plot_*()` function and of `draw()` is `object`;
  `years` is now `year`, matching `getData()` and `getMix()`.
* `type` always means the quantity shown; the chart shape is now `style`.
* Nothing in this group changes the generated solver models — every GAMS/GLPK
  file written before and after is byte-identical.

## New features

* A commodity can be balanced at a coarser geoscale level than the model's
  regions — `newCommodity("STEEL", geolevel = "nation")` — the spatial twin of
  `commodity@timeframe`. It asserts free unlimited transport within that level,
  so it suits an integrated market and never a network-constrained carrier.
  **GLPK and GAMS only**; Julia and Pyomo raise rather than solve a different
  problem.
* A model can carry a `geoscales::Geoscale` describing how its regions nest,
  what they weigh and where they are. Attach with `newModel(geoscale = )` or
  `setGeoscale()`, read with `getGeoscale()`. `geoscales` is a Suggests —
  storing, printing, interpolating and solving all work without it.
* `plot_trade_map()` accepts a `Geoscale` as its `map`, falls back to the
  model's own, and can draw at a coarser `level`.
* Aggregation across regions is delegated to `geoscales::geo_recast()` with the
  rules read off the variable catalogue. `vTradeIr` is netted, not summed.
* `utopia_geoscale()` builds a geoscale for the UTOPIA model
  (`nation → zone → region`), with geometry from any of the four `utopia$map`
  layouts. The hierarchy ships as the plain table `utopia$geo`.
* `levcost()` prices vintages and clusters separately, returning a
  `levcost_variants` object that `autoplot()` compares directly. Each cell is
  priced in its own region so the variants cannot serve each other's demand. New
  `run = c("single", "sequential")` and `max_failures`.
* `getCalendar()` gained methods for `config`, `model` and `scenario` — it was
  declared as a generic with none.

## Bug fixes

* `levcost()` on a technology declaring `@vintage` or `@cluster` was silently
  wrong: the expanded cells competed for one unit of demand and the extraction
  summed across them.
* `plot_trade_map()` drew routes with `geom_segment()` on raw `x`/`y` beside a
  `geom_sf()` layer, which works only for a map with no CRS. Routes, centroids
  and labels are now `sf` layers whenever the map is projected.
* `tech_from_spec()` read `@input$combustion` back as character, so any
  technology setting it wrote a valid techspec that then failed to load.
* `size()` regained its `@export`: a helper inserted between its roxygen block
  and the function had silently taken over the block.
* `autoplot()` on models and repositories fails with energyRt's own message when
  ggplot2 is absent.

# energyRt 0.70.4.9000-dev

## Breaking changes

* `scenario@modOut@variables$vTechCap` returns a `variable` object, not a
  data.frame. `$`, `[[`, `dim()`, `names()` and `as.data.frame()` work on it;
  `merge()`, `rbind()` and `colnames<-` do not. Use `getData()` or
  `get_variable()`.
* The on-disk scenario layout changed — a variable's data moved to
  `variables/<name>/data/`. Directories carry a `layout` file and
  `load_scenario()` refuses an older one rather than reading it as empty.
* `vTechRetiredNewCap` read from a GDX names its second year column `yearp`,
  matching every other engine; the `src`/`dst` renaming for `vTradeIr` applies
  to all engines.
* `vDummyImportCost` / `vDummyExportCost` are the solver's own values; the R
  recomputation that overwrote them after every solve is removed.
* `parameter@misc$nValues` is gone. It truncated a parameter's data in the GAMS,
  Pyomo and Julia writers while GLPK never truncated, so a stale count made
  those engines emit less data for the same scenario.

## New features

* Model variables are S4 objects. `scenario@modOut@variables` is a list of
  `variable` objects as `modInp@parameters` is a list of `parameter` objects,
  both extending a shared virtual class `modelData`.
* A `variable` knows its dimensions, output columns, gating map, positivity,
  `role`, `unit` kind and origin. The specification is composed at build time
  from the GAMS source, the GLPK template and `data-raw/variables.yml`, and
  validated for completeness.
* `modOut` pre-populates every declared variable, so one the solver skipped
  still reports its column names.
* The "never sum this over slices" rule and the sign of each series in a
  generation mix now come from the specification instead of being hard-coded.

## Bug fixes

* `summary()` on a solved scenario never reported dummy import/export costs — it
  read a variable name that has never existed. Dummy flows mask infeasibility.
* `model_structure` attached each variable's dimensions and gating map to the
  wrong variable: names came from one generated table and dims from another.
* `model_size()` counted a gating map's rows once however many variables it
  gated.
* `.get_data_slot()` read a parameter's data slot directly, so every call site
  saw zero rows for an on-disk parameter.
* `update_parameter()` built an on-disk path missing its `parameters` segment.

# energyRt 0.70.3.9000-dev

## Breaking changes

* `config@discount` holds `region`, `year`, `wacc` and `sdr` — there is no
  `discount` column. `pDiscount` is replaced by `pWacc` and `pSdr`;
  `pDiscountFactor` keeps its name and is built from `sdr`.
* `pTechEac`, `pStorageEac` and `pTradeEac` read `@invcost$eac` instead of
  `@invcost$invcost`.
* `levcost()` annuitises at the `wacc`; `report()` prints both rates under their
  own names.
* `trade@capacityVariable` is removed — a trade's capacity is always a decision
  variable. For a fixed transfer limit with no investment, drop
  `@capacity`/`@invcost` and set `trade = data.frame(src =, dst =, ava.up = )`.
* `@start`, `@end` and `@olife` are no longer slots; they are columns of the new
  `@vintage`. The constructors still accept them as arguments, but saved objects
  from earlier versions must be rebuilt.
* Passing an unrecognised slot name to a constructor errors instead of being
  silently dropped.

## New features

* A model has two rates: `wacc` annuitises investment, `sdr` discounts the cost
  stream, with no fallback between them. `discount` survives as an argument —
  `newModel(discount = 0.05)` sets both.
* Technologies, storages and trades can carry their own `@invcost$wacc`. There
  is deliberately no per-process `sdr`.
* `@invcost$eac` is honoured: supply the annuity directly and it is used
  verbatim.
* New `@invcost$payback`, the cost-recovery period — the investment is repaid
  over `payback` years while capacity operates for its full `@vintage$olife`.
  **GLPK only** in this version; the other writers refuse a model that sets it.
* Process classes can describe a group of similar processes evolving over time
  (vintage) or across space/type (cluster). Add a `vintage` and/or `cluster`
  column to the data slots and the object is replicated into one ordinary
  process per cell before the model is built — equations and set dimensions are
  unchanged.
* New `@vintage` slot on `technology`, `storage` and `trade`, and `@cluster` on
  the first two. Variant names suffix the base name (`ECOA_VIN2030_CLnorth`),
  controlled by `config@variant_prefix`.
* `getData()` gains `variants = TRUE`, attaching `base`, `class`, `vintage` and
  `cluster` columns. New `getVariants()` and `variantSummary()` report the
  expansion.
* A `vintage = "TOTAL"` or `cluster = "TOTAL"` row in `@capacity` bounds the sum
  over that process's variants, generating a single group constraint.
* `storage@cap2stg` is promoted from a scalar to a data.frame.

## Bug fixes

* A per-region group capacity bound was applied to each region's variants
  separately instead of to their sum.
* `getData()` returned no rows for parameters selected with a wildcard
  `slice = NA`.
* `check_name()` had inverted guards, so some invalid names passed and some
  valid ones were rejected.
* `pTradeIrEff` declared `slot: teff`, but `teff` is a column of `@trade`, so
  the parameter was never populated.

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
* `newCosts()` is debugged, with an example in the Utopia tutorial.
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
