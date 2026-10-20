# Changelog

## energyRt 0.90.0 (pre-release)

The first release since `0.50-beta`, consolidating the unreleased
0.60-0.80 cycles. The modelling API is the one planned for 1.0; testing
and debugging continue. Upgrading from 0.50 is not a drop-in: see
*Breaking changes*.

### License

- Relicensed from AGPL-3 to Apache-2.0. Releases up to v0.89 remain
  AGPL-3.

### New features

#### Model building

- Three-part storage: charger, reservoir and discharger with their own
  capacities, costs and availability, coupled by `@duration`, `@inp2out`
  and `@inp2stg`.
- Vintages (`@vintage`) and clusters (`@cluster`) expand a process into
  variants at interpolation;
  [`getVariants()`](https://energyRt.org/reference/getVariants.md) lists
  them.
- Resource tiers: a `commodity` or `weather` object with `@cluster` gets
  one balance per tier.
- Weather transforms derive many series from one, and a weather object
  may be declared at a coarser region than the processes reading it.
- Transmission:
  [`newACLine()`](https://energyRt.org/reference/newACLine.md),
  [`newDCLink()`](https://energyRt.org/reference/newDCLink.md), an
  optional Kirchhoff voltage law, loss tranches
  ([`lossTranches()`](https://energyRt.org/reference/lossTranches.md))
  and route ratings (`@trade$af`).
- Supply, import and export curves:
  [`asSupplyCurve()`](https://energyRt.org/reference/supply-curve.md),
  [`asImportCurve()`](https://energyRt.org/reference/supply-curve.md),
  [`asExportCurve()`](https://energyRt.org/reference/supply-curve.md).
- Taxes and subsidies are indexed by instrument and can be levied on
  processes.
- `commodity@property` for physical properties; discount rates at any
  geoscale level; a WACC per technology with `payback` as its
  cost-recovery life.
- Portable process definitions:
  [`process_to_spec()`](https://energyRt.org/reference/process_to_spec.md)
  /
  [`process_from_spec()`](https://energyRt.org/reference/process_from_spec.md)
  (YAML/JSON),
  [`process_designer()`](https://energyRt.org/reference/process_designer.md),
  [`read_technology_xlsx()`](https://energyRt.org/reference/read_technology_xlsx.md)
  /
  [`write_technology_xlsx()`](https://energyRt.org/reference/write_technology_xlsx.md).

#### Time and space

- Calendars from **timescales**, including irregular calendars (months
  of 28-31 days), sampled calendars (representative days) and the new
  `m12_d365_h24` and `m12_d365_h24_1dpm`.
- Multi-level geography from **geoscales**: commodities and processes
  may sit at a coarser `@geoframe` than the flows they touch, with
  auxiliary flows held in the finer cells beneath.
- Region aggregation with clustering:
  `aggregate_model_regions(clusters = )`,
  [`get_process_groups()`](https://energyRt.org/reference/get_process_groups.md),
  [`process_cluster_sweep()`](https://energyRt.org/reference/process_cluster_sweep.md),
  [`model_clusters()`](https://energyRt.org/reference/model_clusters.md).
- Sub-territory solves from a filtered geoscale, and
  [`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md).
- Auxiliary flows sit at the auxiliary commodity’s timeframe: an annual
  emission is one variable per year, not one per hour.

#### Solving

- Solve drivers:
  [`solve_myopic()`](https://energyRt.org/reference/solve_myopic.md),
  [`solve_by_sample()`](https://energyRt.org/reference/solve_by_sample.md),
  [`solve_by_region()`](https://energyRt.org/reference/solve_by_region.md)
  and
  [`solve_guided()`](https://energyRt.org/reference/solve_guided.md).
- First-order LP solvers (cuOpt, HiGHS PDLP), and `lang = "mm"`, which
  builds the problem matrix with **multimod**. Remote solving on the
  NEOS server and on cloud GPUs.
- [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
  is about 6x faster on large models; `fx_equ = TRUE` writes fixed
  bounds as single equality rows (about 25% fewer rows on hourly VRE
  models).
- MPS output (`solver_options$pyomo_mps`),
  [`read_solution()`](https://energyRt.org/reference/read.md) for
  solutions produced elsewhere, and
  [`en_check_solve()`](https://energyRt.org/reference/en_check.md),
  [`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md),
  [`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md),
  [`en_install_julia_pkgs()`](https://energyRt.org/reference/en_install_julia_pkgs.md)
  to check and set up the toolchains.

#### Results and project management

- A project registry keyed on path, with a lifecycle (`seal_*()`,
  [`mark_delete()`](https://energyRt.org/reference/seal.md), …); objects
  are reachable by name (`getScenario("base")`).
- Runs and artifacts:
  [`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md),
  [`scenario_solutions()`](https://energyRt.org/reference/scenario_solutions.md),
  [`promote_solution()`](https://energyRt.org/reference/promote_solution.md),
  [`import_solution()`](https://energyRt.org/reference/import_solution.md),
  [`drop_scenario_run()`](https://energyRt.org/reference/scenario_runs.md).
- Results in Arrow stores, readable when larger than memory;
  `getData(run = )` reads several runs at once.
- [`prepare_for_sharing()`](https://energyRt.org/reference/prepare_for_sharing.md)
  and
  [`strip_user_info()`](https://energyRt.org/reference/strip_user_info.md).

#### Validation and comparison

- [`validate_scenario_parameters()`](https://energyRt.org/reference/validate_scenario_parameters.md)
  (structural versus advisory findings),
  [`verify_solution()`](https://energyRt.org/reference/verify_solution.md)
  with check groups,
  [`audit_coefficients()`](https://energyRt.org/reference/audit_coefficients.md).
- [`compare_scenarios()`](https://energyRt.org/reference/compare_scenarios.md),
  [`compare_models()`](https://energyRt.org/reference/compare_models.md),
  [`compare_inputs()`](https://energyRt.org/reference/compare_inputs.md).

#### Reporting and visualisation

- [`report()`](https://energyRt.org/reference/report.md) for every class
  and for models and scenarios (HTML, PDF, Word, TeX), with spatial and
  solved context (`geoscale =`, `model =`, `scenario =`).
- [`levcost()`](https://energyRt.org/reference/levcost.md): analytic
  levelized cost of technologies, storage (LCOS) and trade (LCOT) per
  vintage, cluster and region; ex-post per variant on a solved scenario.
- [`draw()`](https://energyRt.org/reference/draw.md) process diagrams;
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  for objects, calendars, horizons, scenarios and comparisons;
  [`plot_map()`](https://energyRt.org/reference/plot_map.md),
  [`plot_trade_map()`](https://energyRt.org/reference/plot_trade_map.md),
  [`plot_trade_network()`](https://energyRt.org/reference/plot_trade_network.md).
- TOPIA, the teaching model (was UTOPIA), rebuilt around concentrated
  resources traded between zones.

### Breaking changes

- `slice` is `timeslice`, and a `calendar` is required; the `tsl2*()`
  helpers moved to timescales.
- Storage: `vStorageStore` is `vStorageLevel`; `@cap2stg`, `@charge` and
  `@inflow` are refused (use `@duration`, `@startLevel` or a `supply`);
  flows are bounded by converter capacity (`inp.af.up` / `out.af.up`
  default to 1).
- `@start`, `@end` and `@olife` are merged into `@vintage`; vintage and
  cluster labels must be declared.
- `@geolevel` is `@geoframe`, and a model carries a `Geoscale`.
- Taxes and subsidies are indexed by instrument; `newConstraintS()` and
  `newSub()` are removed.
- Storage auxiliary couplings are part-prefixed (`inp.cap2ainp`, …).
- The first argument is `object` in 66 functions; the scenario API is
  verb-first
  ([`save_scenario()`](https://energyRt.org/reference/save_scenario.md),
  [`load_scenario()`](https://energyRt.org/reference/load_scenario.md),
  …) with `en_registry`; 46 helpers are no longer exported; the 0.5-era
  deprecations are removed.
- The Julia and Python backends exchange Arrow files (`Arrow.jl`,
  `pyarrow`); `arrow_format` is split into `storage_format` and
  `exchange_format`.
- UTOPIA is TOPIA (regions `W1`/`W2`, `C1`-`C6`, `E1`-`E3`); the test
  tier `nightly` is `full`.

### Bug fixes

- About 130 fixes, most of which change results: unwired parameters now
  reach the solver, equations are no longer instantiated without data,
  `waf.fx` binds from both sides, `aeff$cout2ainp` / `cinp2aout` bind,
  sub-region emissions are counted, coarse supply keeps its cost, and
  `get_process_*(process = )` is honoured.

### Documentation

- A rebuilt [pkgdown site](https://energyRt.org) with tutorial articles,
  and a runnable example on every public help page.
- The testing strategy (tiers, the `@covers` matrix, goldens) is
  documented in `tests/README.md` and `tests/TESTING.md`.

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
