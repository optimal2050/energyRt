# Development Status and Roadmap

**Version 0.90.x (development).** The current line modernizes the
interpolation pipeline, scenario storage, and analysis tools on the way
to v1.0. The **v0.50** release (*“half-way-there”*) is frozen and
receives fixes only.

Status legend: **\[✓\]** done · **\[~\]** in progress · **\[ \]**
planned.

------------------------------------------------------------------------

## Current status (September 2026)

- **v0.50** — frozen reference release (fixes only)  
- **v0.90.x** — active development. A breaking release: the first under
  **Apache-2.0** (earlier releases remain AGPL-3), and the one that
  removes the deprecation layer the 0.8x series had been warning about.
  See [NEWS](https://energyRt.org/news/index.md) before upgrading.

## What’s implemented

- \[✓\] Four solver backends — GLPK/MathProg, Julia/JuMP, Python/Pyomo,
  GAMS — from one model object, plus remote execution: the **NEOS**
  server for GAMS and CPLEX without a local licence, and
  `multimod_cloud` for GPU solves of models too large for a laptop.
  `solver_options` carries presets for simplex, barrier, interior-point
  and first-order (PDLP, cuOpt) methods.  
- \[✓\] Refactored interpolation pipeline
  ([`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md),
  spec-driven recipe engine)  
- \[✓\] Scenario storage (Arrow) and analysis
  ([`levcost()`](https://energyRt.org/reference/levcost.md),
  [`report()`](https://energyRt.org/reference/report.md),
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)),
  including recorded runs, variants and myopic sequences  
- \[✓\] **Nested time.** Calendars from a single annual slice to 8760
  hours, from the shipped `calendars` catalog (generated with
  **[timescales](https://github.com/optimal2050/timescales)**) or
  [`newCalendar()`](https://energyRt.org/reference/newCalendar.md).
  Commodities and processes declare the timeframe they balance on, so
  annual fuel accounting and hourly dispatch coexist in one model;
  sampled calendars solve a representative subset and annualise it. (see
  [Time resolution](https://energyRt.org/articles/time-resolution.md))  
- \[✓\] **Nested regions.** Hierarchical regions via
  **[geoscales](https://github.com/optimal2050/geoscales)**: a commodity
  balanced at a coarser level with `newCommodity(geoframe = )` rolls up
  automatically, so point-to-point `trade` is no longer the only way to
  move a commodity between regions. (see [Space
  resolution](https://energyRt.org/articles/space-resolution.md))  
- \[✓\] Dependency setup & checks
  ([`en_setup()`](https://energyRt.org/reference/en_setup.md),
  [`en_check_dependencies()`](https://energyRt.org/reference/en_check.md),
  [`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md))  
- \[✓\] **Testing system** — ~150 test files on tiers
  (`ENERGYRT_TEST_TIER`), cross-backend golden benchmarks, `@covers`
  tags feeding a parameter-level coverage matrix, and
  [`verify_solution()`](https://energyRt.org/reference/verify_solution.md)
  identity checks on solved scenarios  
- \[~\] multimod-based equation rendering (GAMS → AST → LaTeX; proof of
  concept)

------------------------------------------------------------------------

## Roadmap / next steps

### Model structure & scope

- \[~\] **Finish nested regions & time.** The balance layer is in
  (above). Still open: declaring a *process* at a coarse level and
  resolving it DOWN to its commodities, rest-of-world import/export at a
  coarse level, and parallel (cross-cutting) groupings of the same
  regions.  

### Model API

\[~\] **Refactor
[`newConstraint()`](https://energyRt.org/reference/newConstraint.md).**
User-defined constraints currently compile to GAMS-dialect text; move
them to a language-neutral representation so every backend renders from
a single definition (aligned with the multimod AST).  

**Add `newVariable()`.** There is no user-facing constructor for
decision variables yet — the variable catalogue is fixed. Add a runtime
`newVariable()` paralleling
[`newConstraint()`](https://energyRt.org/reference/newConstraint.md) so
users can extend the model with new variables.  

### Validation

\[~\] **Validate results against inputs.** Input-side checks now refuse
conflicting parameter rows at construction and stop an interpolation
that would multiply a series. The output side is thinner: extend
[`verify_solution()`](https://energyRt.org/reference/verify_solution.md)
so declared bounds and values can be traced into the solution, and so
variables can be checked against each other (capacity vs flows, new
capacity vs EAC).  

**Unit coherence.**
[`getUnits()`](https://energyRt.org/reference/getUnits.md) already knows
each parameter’s unit formula; report chains where units are undeclared,
and flag coefficients whose units cannot compose.  

### Base-code simplification

\[~\] Reduce duplication across the parallel **technology / storage /
trade** entity systems (the triplicated capacity / EAC / fixed-O&M
skeletons) toward a more unified, parameterized process form.  

\[~\] Finish the interpolation recipe/registry engine — route the
remaining inline mapping construction through the engine.  

Retire obsolete aggregate variables once all backends use
up-aggregation.  

### Renaming, cleaning & optimization

\[✓\] Retire the deprecation layer (v0.90) — the 0.8x aliases are gone
and the names they forwarded to are the only spellings.  

Consistent naming, dead-code removal, resolution of outstanding in-code
TODOs, and namespace/import hygiene.  

Hand the timeslice ↔︎ datetime helpers to **timescales**, which owns the
time dimension; they are unexported internals in energyRt until it
provides them.  

### Quality & tooling

**Continuous integration.** The test suite exists and is tiered, but
there are no CI workflows yet: the full suite is an overnight run
locally. Add scheduled runs and per-PR checks on the cheap tiers.  

**Documentation & solver hardening.** Document the remaining classes and
arguments, fill in missing articles, and harden the GAMS writer and the
NEOS remote backend.  

### Integration with multimod

- \[~\] **Single-source model.** Drive both the equation documentation
  and the executable model code (LaTeX / GLPK / JuMP / Pyomo) from one
  definition via [multimod](https://github.com/optimal2050/multimod)’s
  read → AST → write chain, replacing the manual cross-engine mirroring
  of the model templates.  

### v1.0 & CRAN

**CRAN submission.** Resolve the off-CRAN optional dependencies, clear
the `R CMD check` warnings (documentation of classes and arguments),
tidy the package namespace and `DESCRIPTION`, and guard solver-dependent
examples and tests.  

------------------------------------------------------------------------

## Milestones

- **v0.50** — frozen *“half-way-there”* reference release (fixes only)  
- **v0.90.x** — current development: Apache-2.0, deprecation layer
  removed, nested regions & time, remote and GPU solving  
- **v1.0** — CRAN release; stabilized model, classes, and API  
- **v1.0+** — multimod integration; completing nested regions & time
  (coarse-level processes, cross-cutting groupings)

------------------------------------------------------------------------

## Contributing & support

Contributions, issues, and ideas are welcome. energyRt is **Apache-2.0**
from v0.90; contributions are accepted under the same terms.

- GitHub: <https://github.com/optimal2050/energyRt>  
- Issues: <https://github.com/optimal2050/energyRt/issues>  
- Website: <https://energyRt.org>
