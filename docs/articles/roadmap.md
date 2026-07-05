# Development Status and Roadmap

**Version 0.60.x (development).** The current line modernizes the
interpolation pipeline, scenario storage, and analysis tools on the way
to v1.0. The **v0.50** release (*“half-way-there”*) is frozen and
receives fixes only.

Status legend: **\[✓\]** done · **\[~\]** in progress · **\[ \]**
planned.

------------------------------------------------------------------------

## Current status (July 2026)

- **v0.50** — frozen reference release (fixes only)  
- **v0.60.x** — active development: `interp_mod()` pipeline, scenario
  storage, analysis tools

## What’s implemented

- \[✓\] Four solver backends — GLPK, Julia/JuMP, Python/Pyomo, GAMS  
- \[✓\] Refactored interpolation pipeline (`interp_mod()`, spec-driven
  recipe engine)  
- \[✓\] Scenario storage (Arrow/SQLite) and analysis
  ([`levcost()`](https://energyRt.org/reference/levcost.md),
  [`report()`](https://energyRt.org/reference/report.md),
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html))  
- \[✓\] Dependency setup & checks
  ([`en_setup()`](https://energyRt.org/reference/en_setup.md),
  [`en_check_dependencies()`](https://energyRt.org/reference/en_check.md),
  [`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md))  
- \[~\] multimod-based equation rendering (GAMS → AST → LaTeX; proof of
  concept)

------------------------------------------------------------------------

## Roadmap / next steps

### Model structure & scope

**Nested regions & time.** Regions are a flat set of labels today, and
interregional exchange is modelled as point-to-point `trade`. Add
hierarchical, nested regions via
**[geoscales](https://github.com/optimal2050/geoscales)** and nested
timeframes via
**[timescales](https://github.com/optimal2050/timescales)** — the
spatial and temporal companion packages.  

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

### Base-code simplification

\[~\] Reduce duplication across the parallel **technology / storage /
trade** entity systems (the triplicated capacity / EAC / fixed-O&M
skeletons) toward a more unified, parameterized process form.  

\[~\] Finish the `interp_mod()` recipe/registry engine — route the
remaining inline mapping construction through the engine and retire the
legacy
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md).  

Retire obsolete aggregate variables once all backends use
up-aggregation.  

### Renaming, cleaning & optimization

Consistent naming, dead-code removal, resolution of outstanding in-code
TODOs, and namespace/import hygiene.  

### Quality & tooling

**Testing & CI.** Expand the `testthat` suite, add cross-backend
objective-consistency checks, and set up continuous integration.  

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
- **v0.60.x** — current development: interpolation pipeline, scenario
  storage, analysis  
- **v1.0** — CRAN release; stabilized model, classes, and API  
- **v1.0+** — multimod integration; nested regions & time (geoscales /
  timescales)

------------------------------------------------------------------------

## Contributing & support

Contributions, issues, and ideas are welcome.

- GitHub: <https://github.com/optimal2050/energyRt>  
- Issues: <https://github.com/optimal2050/energyRt/issues>  
- Website: <https://energyRt.org>
