# Features

This is the full inventory, organised around the three independent axes
the front page introduces — the bricks a system is described with, the
time and space it is resolved on, and the route it is solved by —
followed by what happens after a solve. Functions named here are
exported; items marked *(experimental)* work but may change shape before
1.0. What *changed* in a release is in the [release
notes](https://energyrt.org/news/).

## Bricks: describing a system

**Component classes.** `commodity`, `supply`, `demand`, `technology`,
`storage`, `trade`, `weather`, `constraint`, `tax`, `subsidy`, `costs`,
plus `import` / `export` for flows across the model boundary. Each is
built by its `newX()` constructor, carries its data in slots, and is
edited with [`update()`](https://energyRt.org/reference/update.md),
which derives a variant instead of re-typing the object.

- **Technologies** convert any number of input commodities into any
  number of outputs, with conversion given per commodity (`ceff`), per
  group of commodities (`@group`), or as process activity (`cap2act`);
  availability by timeslice (`af`), by summed slices (`afs`) and per
  commodity (`afc`); ramping limits; capacity and retirement bounds;
  investment, fixed and variable costs.
- **Auxiliary flows** (`@aux` / `@aeff`) couple a process’s capacity,
  new capacity, retirement, activity or any of its flows to a further
  commodity — land per MW, water per MWh, an emission per unit of fuel.
  They sit at the auxiliary commodity’s own timeframe, so an annual
  emission of an hourly plant is one variable per year.
- **Storage** is three parts — charger, reservoir, discharger — each
  with its own capacity variable (`vStorageInpCap`, `vStorageStgCap`,
  `vStorageOutCap`), costs and availability. `@duration` sets reservoir
  hours per unit of discharge power, `@inp2out` the charger/discharger
  ratio and `@inp2stg` the charging C-rate; `@seff` holds charge,
  discharge and self-discharge efficiency; `@startLevel` the initial
  state. A storage may store one commodity and charge or discharge
  another.
- **Trade** carries a commodity between regions on declared routes, with
  efficiency, capacity, costs, availability and lifetime.
  [`newACLine()`](https://energyRt.org/reference/newACLine.md) and
  [`newDCLink()`](https://energyRt.org/reference/newDCLink.md) wrap the
  two common electrical cases;
  [`lossTranches()`](https://energyRt.org/reference/lossTranches.md)
  approximates quadratic losses by capacity tranches; an opt-in
  Kirchhoff voltage law constrains loop flows.
- **Weather** supplies an exogenous time series that multiplies a
  process’s availability — capacity factors, inflows, a charging window.
  One series can serve many processes, be declared at a coarser region
  than its users, and derive further series through registered
  transforms
  ([`register_weather_transform()`](https://energyRt.org/reference/register_weather_transform.md)).
- **Policy objects.** A `constraint` writes an arbitrary linear
  restriction over declared summands; `tax` and `subsidy` levy on a
  commodity total or on a named process; `costs` adds a user-defined
  cost term to the objective.
- **Commodity properties.** `@property` carries physical data (heating
  values, densities, carbon content) as a tidy table; `@emis` emission
  factors; `@agg` aggregation into reporting groups; `@limtype` the
  balance direction.
- **Units** are declared on flows and capacities, with `cap2act` making
  the capacity-to-activity conversion explicit, and
  [`getUnits()`](https://energyRt.org/reference/getUnits.md) reporting
  what a model carries.
- **Portable definitions.**
  [`process_to_spec()`](https://energyRt.org/reference/process_to_spec.md)
  /
  [`process_from_spec()`](https://energyRt.org/reference/process_from_spec.md)
  write and read a process as YAML or JSON;
  [`process_spec_issues()`](https://energyRt.org/reference/process_spec_issues.md)
  lints one;
  [`process_designer()`](https://energyRt.org/reference/process_designer.md)
  is an interactive Shiny editor;
  [`read_technology_xlsx()`](https://energyRt.org/reference/read_technology_xlsx.md)
  /
  [`write_technology_xlsx()`](https://energyRt.org/reference/write_technology_xlsx.md)
  round-trip through Excel.

### Variants and graded resources

- **Two axes on one object.** `@vintage` declares build years with their
  investment window and operational life; `@cluster` declares parallel
  members that exist simultaneously — site grades, fleet types,
  tranches. Interpolation expands them into ordinary processes
  (`EWIN_VIN2030_CL01`), so every equation stays as it is.
- **Per-member data.** Every parameter slot carries `vintage` and
  `cluster` columns; a row leaving them `NA` broadcasts to all members.
  A roster must be declared before a label may be used, which turns a
  typo into an error rather than a phantom member built from defaults.
- **Region-scoped members.** `@cluster$region` scopes a member, so a
  tier set of a different size in each region is one object.
- **Family-level bounds.** `cap.share.fx` and `act.share.lo/up/fx` bound
  how a family splits between its members; `"TOTAL"` in a selector
  column bounds the family as a whole.
- **Graded resources.** A `commodity` declaring `@cluster` becomes one
  commodity per tier, each with its own balance, so grades cannot pool;
  a `weather` object declaring `@cluster` becomes one series per tier.
  Consumers resolve their tier by cluster name; unclustered objects are
  untouched.
- **Supply curves.**
  [`asSupplyCurve()`](https://energyRt.org/reference/supply-curve.md),
  [`asImportCurve()`](https://energyRt.org/reference/supply-curve.md)
  and
  [`asExportCurve()`](https://energyRt.org/reference/supply-curve.md)
  turn a single price into a stepped curve with its own roster.
- **Reading a family back.**
  [`getVariants()`](https://energyRt.org/reference/getVariants.md) (on a
  scenario or, before interpolating, on a model),
  `getData(variants = TRUE)` and
  [`variantSummary()`](https://energyRt.org/reference/variantSummary.md)
  — the provenance table is authoritative, so names are never parsed.

## Axis one: time

- **User-defined nested calendars** from
  [timescales](https://github.com/optimal2050/timescales): the modeller
  states the hierarchy (`ANNUAL > SEASON > DAY > HOUR`, or whatever the
  question needs), the timeslices and their durations. There is no fixed
  8,760-slice ceiling and no built-in assumption about what a “slice”
  is.
- **Mixed timeframes in one model.** Each commodity balances at its own
  timeframe, so an annual fuel market, a seasonal reservoir and an
  hourly power balance coexist; a process may be declared coarser than
  the commodities it touches, and the equations aggregate.
- **Shipped designs.** `calendars` carries generic layouts — `m12`,
  `q4`, `s4_h24`, `d365_h24` and sampled variants such as
  `d365_h24_1dps` (one representative day per month) and
  `s4_h24_subset_2seasons`.
- **Sampled calendars.** A model declared on a full calendar can be
  interpolated and solved on a subset of its slices, with annual
  quantities kept on a full-year basis — the usual way to iterate
  quickly and confirm at full resolution before reporting.
- **Horizons** with uneven milestone years, interval weights and display
  labels
  ([`newHorizon()`](https://energyRt.org/reference/newHorizon.md)), and
  a calendar anchored to a `year_start` and a UTC offset.
- **Conversions and layout** come from the sibling package:
  [`make_timetable()`](https://energyRt.org/reference/calendar.md),
  [`tsl_sets()`](https://energyRt.org/reference/timeslices.md),
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  on a calendar, and the timescales API for label conversion.

## Axis two: space

- **Nested geography** from
  [geoscales](https://github.com/optimal2050/geoscales): a `Geoscale`
  describes how regions nest (nation \> zone \> node), attached with
  [`setGeoscale()`](https://energyRt.org/reference/setGeoscale.md) or
  `newModel(geoscale = )`.
- **Multi-level balances.** A commodity declares the level it balances
  on, so a national fuel market and nodal electricity live in one model
  without a trade route for every pair; coarse balances roll up.
- **Coarse processes.** A technology or storage may sit at a level
  coarser than a commodity it uses, with its flows aggregated to the
  level where the balance is written.
- **Region aggregation.**
  [`aggregate_model_regions()`](https://energyRt.org/reference/aggregate_model_regions.md)
  merges regions, including `clusters =` to partition a corridor family
  into `k` groups;
  [`get_process_groups()`](https://energyRt.org/reference/get_process_groups.md)
  reports what may be merged,
  [`process_cluster_sweep()`](https://energyRt.org/reference/process_cluster_sweep.md)
  sweeps the admissible range of `k`, and
  [`model_clusters()`](https://energyRt.org/reference/model_clusters.md)
  reports what an aggregation did.
- **Sub-territory solves.** A filtered geoscale handed to
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
  solves part of a model’s territory;
  [`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md)
  samples regions and warns when a sample splits a cell a process spans.
- **Maps.**
  [`plot_geoscale()`](https://energyRt.org/reference/plot_geoscale.md),
  [`plot_map()`](https://energyRt.org/reference/plot_map.md) for any
  solved variable carrying a region,
  [`plot_trade_map()`](https://energyRt.org/reference/plot_trade_map.md)
  and
  [`plot_trade_network()`](https://energyRt.org/reference/plot_trade_network.md)
  for flows.

## Axis three: the solve route

Four modelling languages, one direct-matrix route, and remote execution.
The model definition does not change between them.

| Route | What it writes | Notes |
|----|----|----|
| GLPK / MathProg | `.mod` + `.dat`, solved by `glpsol` | The transparent default. The `glpsol` R package bundles the binary on every platform and is used first when installed, so a first model needs no system solver |
| Julia / JuMP | JuMP code for HiGHS, CBC, GLPK, CPLEX, cuOpt | The usual choice for mid-size and large models |
| Python / Pyomo | Pyomo for HiGHS, CBC, GLPK, CPLEX; MPS output | For Python-based solver stacks |
| GAMS | GAMS for CPLEX, CBC and others | For institutional GAMS workflows |
| multimod *(experimental)* | A language-neutral AST → LaTeX, backend code, or a direct LP/MPS matrix | `lang = "mm"` skips the symbolic model build |

- **Presets, not flags.** `solver_options` carries named presets per
  backend — barrier, simplex, PDLP, crossover on or off, tolerances —
  including the first-order CPU preset `julia_highs_pdlp` and the GPU
  presets `julia_cuopt` and `mm_cuopt_wsl`.
- **GPU solving.** NVIDIA cuOpt runs through JuMP on Linux, or on
  Windows through WSL (`mm_cuopt_wsl`), and on cloud hardware
  (`mm_cuopt_cloud`) through the experimental route below. See [Solver
  backends](https://energyRt.org/articles/backends.md) for setup and the
  validation required before a first-order solution is trusted.
- **Remote execution.** [NEOS](https://neos-server.org/) runs supported
  solver jobs without a local licence; the *(experimental)* cloud
  presets (`mm_cuopt_cloud`, `pyomo_cuopt_cloud`) ship the problem to
  cloud hardware through `mmcloud`, stream progress and retrieve the
  solution.
- **Solutions from elsewhere.**
  [`read_solution()`](https://energyRt.org/reference/read.md) decodes a
  solution produced outside energyRt, so a model written here can be
  solved anywhere and read back.
- **Toolchain checks.**
  [`en_setup()`](https://energyRt.org/reference/en_setup.md),
  [`en_check_dependencies()`](https://energyRt.org/reference/en_check.md),
  [`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md),
  [`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md),
  [`en_install_julia_pkgs()`](https://energyRt.org/reference/en_install_julia_pkgs.md),
  and [`en_check_solve()`](https://energyRt.org/reference/en_check.md),
  which solves a tiny model on every configured backend and reports what
  actually works.

## Generating the model

- [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
  resolves declarations into solver-ready parameters: interpolating over
  years, expanding variants, materialising tiers and weather transforms,
  building sets and mappings, and checking the result.
- **The model it writes** has about 185 equation blocks over 93
  variables (as written in `glpk/energyRt.mod`), extendable with
  [`newConstraint()`](https://energyRt.org/reference/newConstraint.md).
- **Sparse or dense**, folded or explicit: `sparse =` drops rows equal
  to a default, `fold =` collapses dimensions that do not vary (region,
  timeslice, year), both losslessly.
- **On-disk interpolation** writes parameters to an Arrow store, so a
  model too large for memory still builds.
- **Performance and transparency.** The release notes record
  interpolation about six times faster than the 0.8 series on a 41-node
  hourly case, and every call appends per-stage and per-map timings to a
  log.
- **Model inspection.**
  [`model_size()`](https://energyRt.org/reference/model_size.md) /
  [`size()`](https://energyRt.org/reference/size.md),
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`find_in_model()`](https://energyRt.org/reference/find_in_model.md),
  [`findData()`](https://energyRt.org/reference/findData.md),
  [`findDuplicates()`](https://energyRt.org/reference/findDuplicates.md),
  [`audit_coefficients()`](https://energyRt.org/reference/audit_coefficients.md)
  for the written coefficient ranges, and
  [`compare_inputs()`](https://energyRt.org/reference/compare_inputs.md)
  to diff two interpolated models.

## Solve strategies

- [`solve_model()`](https://energyRt.org/reference/solve_model.md) — the
  whole pipeline, or the verbs separately
  ([`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md),
  [`write_script()`](https://energyRt.org/reference/write.md),
  [`read_solution()`](https://energyRt.org/reference/read.md)).
- [`solve_recursive()`](https://energyRt.org/reference/solve_recursive.md)
  — milestone by milestone with limited foresight, on an exported
  solution ledger
  ([`solution_ledger()`](https://energyRt.org/reference/solution_ledger.md),
  [`apply_ledger()`](https://energyRt.org/reference/apply_ledger.md),
  [`horizon_windows()`](https://energyRt.org/reference/horizon_windows.md),
  [`recursive_objective()`](https://energyRt.org/reference/recursive_objective.md)).
- [`solve_by_sample()`](https://energyRt.org/reference/solve_by_sample.md)
  — one solve per sampled calendar
  ([`calendar_samples()`](https://energyRt.org/reference/calendar_samples.md),
  [`sample_summary()`](https://energyRt.org/reference/sample_summary.md)).
- [`solve_by_region()`](https://energyRt.org/reference/solve_by_region.md)
  — regional decomposition with boundary windows.
- [`solve_guided()`](https://energyRt.org/reference/solve_guided.md) —
  iterate toward a target
  ([`solution_targets()`](https://energyRt.org/reference/solution_targets.md),
  [`apply_targets()`](https://energyRt.org/reference/apply_targets.md),
  [`guided_gap()`](https://energyRt.org/reference/guided_gap.md)).
- Every driver records its runs under the scenario, with the parameters
  it was called with.

## Scenarios, storage and provenance

- **A project registry** keyed on path records models, repositories,
  scenarios and datasets:
  [`newRegistry()`](https://energyRt.org/reference/registry.md),
  [`load_registry()`](https://energyRt.org/reference/registry.md),
  [`refresh_registry()`](https://energyRt.org/reference/registry.md),
  [`add_to_registry()`](https://energyRt.org/reference/registry.md), and
  [`summary()`](https://rdrr.io/r/base/summary.html) for counts and
  staleness. Objects are reachable by name — `getScenario("base")`,
  [`getObject()`](https://energyRt.org/reference/getObject.md) — so a
  study with thousands of solved runs stays addressable.
- **Lifecycle.**
  [`seal_model()`](https://energyRt.org/reference/seal.md) /
  `unseal_*()` freeze an entry as a version; only sealed versions are
  referenced.
  [`list_versions()`](https://energyRt.org/reference/list_versions.md)
  lists the versions of models, repositories and datasets with their
  status; [`mark_delete()`](https://energyRt.org/reference/seal.md) and
  [`delete_marked()`](https://energyRt.org/reference/seal.md) stage
  removals and refuse to break a reference;
  [`list_dependents()`](https://energyRt.org/reference/list_dependents.md)
  says what points at an object.
- **Scenarios built on a base.** `interpolate_model(..., base = REF)`
  reuses an interpolated scenario’s tables for a scenario that adds
  constraints, costs, levies, processes or commodities, or samples REF’s
  calendar or regions; the result equals a full interpolation. On a
  sealed REF, the saved scenario references REF’s tables instead of
  copying them.
- **Runs and artefacts.**
  [`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md),
  [`scenario_solutions()`](https://energyRt.org/reference/scenario_solutions.md),
  [`scenario_artifacts()`](https://energyRt.org/reference/scenario_artifacts.md)
  report what each solve produced and left on disk;
  [`promote_solution()`](https://energyRt.org/reference/promote_solution.md)
  and
  [`import_solution()`](https://energyRt.org/reference/import_solution.md)
  choose which solution counts;
  [`drop_solver_outputs()`](https://energyRt.org/reference/drop_solver_outputs.md)
  and
  [`drop_scenario_run()`](https://energyRt.org/reference/scenario_runs.md)
  clean up;
  [`upgrade_scenario_layout()`](https://energyRt.org/reference/upgrade_scenario_layout.md)
  migrates older folders.
- **Arrow-backed results.** Solutions and inputs live in Parquet/Arrow
  stores with lazy reading, so results larger than memory stay usable;
  [`isInMemory()`](https://energyRt.org/reference/isInMemory.md) /
  [`isOnDisk()`](https://energyRt.org/reference/isOnDisk.md) /
  [`obj2mem()`](https://energyRt.org/reference/obj2mem.md) move between
  the two.
- **Hashes and reproducibility.**
  [`model_hash()`](https://energyRt.org/reference/model_hash.md),
  [`repository_hash()`](https://energyRt.org/reference/repository_hash.md),
  [`dataset_hash()`](https://energyRt.org/reference/dataset_store.md),
  [`object_hash()`](https://energyRt.org/reference/model_hash.md); an
  optional operation log
  ([`set_log_file()`](https://energyRt.org/reference/log.md)); `run.yml`
  recording a solve’s memory footprint and timings.
- **Sharing.**
  [`prepare_for_sharing()`](https://energyRt.org/reference/prepare_for_sharing.md)
  writes a cleaned, trimmed copy of a stored scenario;
  [`strip_user_info()`](https://energyRt.org/reference/strip_user_info.md)
  removes the machine it was built on.

## Results and analysis

- [`getData()`](https://energyRt.org/reference/getData.md) returns tidy
  data for any parameter or variable, with `merge = TRUE` for set
  labels, `variants = TRUE` for provenance, `interpolate = TRUE` for the
  interpolated series, `run =` to read several runs at once, and
  `timeframe =` to aggregate.
- [`get_variable()`](https://energyRt.org/reference/get_variable.md),
  [`getMix()`](https://energyRt.org/reference/getMix.md) (with `top_n`
  lumping),
  [`get_weather()`](https://energyRt.org/reference/get_weather.md),
  [`read_sequence()`](https://energyRt.org/reference/read_sequence.md)
  for a driver’s sequence of solves.
- **Levelized cost.**
  [`levcost()`](https://energyRt.org/reference/levcost.md) prices a
  technology, a storage (LCOS) or a trade route (LCOT) analytically — no
  solver — per vintage, cluster and region, from either a declared
  object or a solved scenario; results are cached on disk.
  [`tech_share_frontier()`](https://energyRt.org/reference/tech_share_frontier.md)
  and
  [`plot_share_frontier()`](https://energyRt.org/reference/plot_share_frontier.md)
  show how the merit order shifts with a share constraint.
- **Comparison.**
  [`compare_scenarios()`](https://energyRt.org/reference/compare_scenarios.md)
  (solved results, or runs of one scenario),
  [`compare_models()`](https://energyRt.org/reference/compare_models.md)
  (declarations) and
  [`compare_inputs()`](https://energyRt.org/reference/compare_inputs.md)
  (interpolated inputs), each with an
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  method.
- **Decomposition drivers** sum by region and average by sample, so a
  multi-run study aggregates correctly.

## Validation and diagnostics

- [`validate_scenario_parameters()`](https://energyRt.org/reference/validate_scenario_parameters.md)
  separates **structural** findings — the model would solve to a
  meaningless answer — from advisory ones, and names them: a populated
  flow with no capacity link (`free_meal`), constraint tuples with no
  data behind them, coefficient ranges that will hurt a solver,
  duplicate keys, and variant provenance that disagrees with the process
  names.
- [`verify_solution()`](https://energyRt.org/reference/verify_solution.md)
  re-derives the model’s own identities from the results — balances,
  capacity limits, storage dynamics, emissions, costs — with check
  groups (`checks = "all"`) and
  [`verify_checks()`](https://energyRt.org/reference/verify_checks.md)
  listing them.
- [`audit_coefficients()`](https://energyRt.org/reference/audit_coefficients.md)
  reports coefficient ranges per equation of a written model, which is
  where a scaling problem shows up before the solver struggles.
- Structural refusals at build time: an undeclared weather link, a
  geoscale whose levels do not nest, a commodity finer than the process
  producing it, a variant name colliding with an existing object, a tier
  a consumer asks for but the resource does not declare.
- [`en_check_dependencies()`](https://energyRt.org/reference/en_check.md)
  /
  [`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md)
  for the environment, and
  [`trim_parameters()`](https://energyRt.org/reference/trim_parameters.md)
  /
  [`update_parameter()`](https://energyRt.org/reference/update_parameter.md)
  for surgery on an interpolated model.

## Visualisation and reporting

- **Diagrams.** [`draw()`](https://energyRt.org/reference/draw.md)
  sketches a process’s flow structure — inputs, outputs, auxiliary
  flows, groups, and a storage’s three parts with their ratio couplings.
- **[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)**
  on objects (declared data as points, interpolated as lines),
  calendars, horizons, levelized costs, comparisons, and solved
  scenarios (`type = "generation"`, `"capacity"`, `"new_capacity"`,
  `"fuel"`, `"storage"`, with `by = "vintage"` / `"cluster"` fan-outs,
  faceting and `top_n`).
- **Maps** — see the space axis above — and
  [`plot_heatmap()`](https://energyRt.org/reference/plot_heatmap.md) for
  a timeslice × region view.
- **Reports.** [`report()`](https://energyRt.org/reference/report.md)
  renders any energyRt object — fourteen element classes plus the model,
  repository and scenario containers — to HTML, PDF, Word or TeX.
  Reports are cached, follow user-defined process groups, carry branding
  (`misc$logos`, `misc$badges`), and are built from an exported
  `report_*` helper family so a template can be replaced.
  [`report_templates()`](https://energyRt.org/reference/report_templates.md)
  lists what ships.
- [`theme_energyRt()`](https://energyRt.org/reference/theme_energyRt.md),
  [`object_image()`](https://energyRt.org/reference/object_image.md) and
  [`design()`](https://energyRt.org/reference/design.md) for consistent
  styling.

## Interoperability

- **multimod** turns an interpolated scenario into a language-neutral
  model —
  [`multimod_from_energyRt()`](https://energyRt.org/reference/multimod_from_energyRt.md)
  — from which it renders LaTeX, emits code for other modelling
  languages, or assembles the LP matrix directly without the symbolic
  model-build step that can dominate time and memory on large problems.
  [`multimod_import_data()`](https://energyRt.org/reference/multimod_import_data.md)
  and
  [`multimod_unmatched()`](https://energyRt.org/reference/multimod_unmatched.md)
  bring results and diagnostics back.
- **OSeMOSYS / TIMES conventions** are followed where they exist — the
  `timeslice` dimension, the parameter naming style — so a model is
  legible to users of those frameworks.
- **The sibling stack**: `timescales` (time), `geoscales` (space),
  `nestedscales` (the nested-set core these two are built on), `glpsol`
  (the bundled GLPK binary) and `mmcloud` (remote execution).
- Tidy in, tidy out: everything crosses the boundary as a data frame, so
  the usual tidyverse and arrow tooling applies.

## Learning and examples

- **TOPIA**, the packaged teaching model: a multi-region power system
  built up step by step over three articles (build, run scenarios,
  multi-level regions), with scenario levers (CO2 cap, carbon tax,
  renewable share, no new nuclear, early retirement) and add-on modules.
- **Unit kits** (`topia$modules$unit`): minimal models whose every input
  is an integer, so the objective can be computed by hand — the fastest
  way to check a backend or a change.
- **Articles** for installation, backends, model bricks, units, time and
  space resolution, storage, vintages and clusters, workflow, scenario
  management, reports and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).
- **A production example.**
  [IDEEA](https://ideea-model.github.io/IDEEA/) applies the same
  machinery to India’s power system at full resolution.

## Engineering

- **A tiered test suite** — `check < fast < cross < full` — with a
  coverage matrix tying tests to the model entities they exercise
  (`@covers`), tracked goldens for interpolated inputs and solved
  results, cross-backend equivalence tests, and a backend probe that
  skips what the machine cannot solve.
- **Deterministic builds.** `data-raw/` drivers are idempotent; bundled
  datasets are regenerated rather than hand-edited.
- **Apache-2.0 licensed**, with the sibling packages under the same
  terms.
