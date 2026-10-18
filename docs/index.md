## energyRt ![energyRt hex logo](reference/figures/logo.png)

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License: Apache
2.0](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

> **Energy-system planning, end to end in R.**

**energyRt** is an open-source, R-native framework and model generator
for least-cost energy-system planning. Describe resources, technologies,
storage, trade, and demand as reusable R objects; energyRt turns them
into a capacity-expansion and dispatch model, runs the solver, and
returns tidy results for analysis and reporting. Built-in validation
tools can then test model structure and solution identities.

The package supplies the modeling machinery rather than a fixed dataset
or scenario. It is intended for analysts, researchers, and educators who
want to build transparent models at the scale and resolution their
question requires without leaving the R ecosystem.

### Why energyRt

- **Model the system, not the algebra.** Work with energy-system
  components; energyRt generates sets, mappings, equations, solver
  files, and result tables.
- **Match resolution to the question.** Define the timeframes and
  timeslices your model needs in a nested
  [timescales](https://github.com/optimal2050/timescales) calendar, and
  balance different commodities at different levels of a regional
  hierarchy.
- **Keep one reproducible workflow.** Prepare data, compose scenarios,
  solve, validate, compare, plot, and report in R. Large scenarios can
  use Arrow-backed storage and lazy result loading.
- **Choose how and where to solve.** The same model can target
  GLPK/MathProg, Julia/JuMP, Python/Pyomo, or GAMS, with experimental
  direct-matrix and remote routes for larger problems.

### Time and space are yours to define

Resolution is a modelling choice, not a fixed property of the framework.
The calendar below nests `ANNUAL > SEASON > HOUR`; the geoscale nests
`nation > zone > region`. Both are declared by the modeller, and each
commodity balances at the level its data supports.

|  |  |
|----|----|
| ![A nested calendar: one annual slice over four seasons over 24 hours, sized by their share of the year](reference/figures/readme-calendar.png) | ![A three-level geoscale: regions nested in zones nested in one nation](reference/figures/readme-geoscale.png) |
| *`autoplot(calendars`$`s4_h24)</code> — 4 seasons x 24 hours, nested
under an annual balance.</em></td>
<td><em><code>plot_geoscale(topia`$`geoscales$continent)` — regions grouped into zones, coloured by zone.* |  |

**Changing a dimension does not mean rewriting the model.** A model is
declared once at full resolution; a coarser or sampled calendar is
passed at solve time, and the geography is attached, or filtered to a
sub-territory, the same way. Three arguments decide the resolution of a
run:

``` r

newModel(..., calendar = calendars$s4_h24)   # declared: 4 seasons x 24 hours
solve_model(mod, name = "sampled",           # solved on a subset of its slices
            calendar = calendars$s4_h24_subset_2seasons)
setGeoscale(mod, topia$geoscales$continent)  # nation > zone > region
```

In a small gas-and-demand model on that calendar, sampling two of the
four seasons cuts 101 timeslices to 51 and lands within 0.2% of the full
solve (1759.7 against 1756.8) — the usual reason to sample while
iterating and confirm at full resolution before reporting.

### What’s new in v0.90

Version 0.90 is the pre-release for v1.0 and the first release since
`v0.50-beta`, consolidating three unreleased development cycles. It
brings nested time and geography into the core model, re-models storage
as three priced parts, makes vintages and clusters one mechanism
(including graded resources that cannot pool), reworks interpolation and
scenario storage, adds structural validation and solution verification,
expands analysis and reporting
([`levcost()`](https://energyRt.org/reference/levcost.md),
[`report()`](https://energyRt.org/reference/report.md),
[`draw()`](https://energyRt.org/reference/draw.md), `autoplot()`), and
adopts the Apache-2.0 license.

What remains before v1.0 is testing, debugging and visualisation rather
than new structure. Upgrading from 0.50 is not a drop-in — see the
[release notes](https://energyrt.org/news/) and
[Upgrading](#upgrading-from-earlier-versions) first.

## Quickstart

This complete model has one fuel, one power plant, and one demand. On
Windows, GLPK is available with Rtools.

``` r

library(energyRt)

GAS <- newCommodity("GAS", timeframe = "ANNUAL")
ELC <- newCommodity("ELC", timeframe = "ANNUAL")

SUP_GAS <- newSupply("SUP_GAS", commodity = "GAS",
  supply = data.frame(cost = 6.0))                    # MEUR/PJ

EGAS <- newTechnology("EGAS",
  input   = list(comm = "GAS"),
  output  = list(comm = "ELC"),
  ceff    = data.frame(comm = "GAS", cinp2use = 0.55),
  invcost = list(invcost = 900),                     # MEUR/GW
  fixom   = 25,
  cap2act = 31.536,
  olife   = 25L)

DEM_ELC <- newDemand("DEM_ELC", commodity = "ELC",
  demand = data.frame(demand = 50))                  # PJ/year

mod <- newModel("HELLO",
  data = newRepository("parts", GAS, ELC, SUP_GAS, EGAS, DEM_ELC),
  region = "R1",
  discount = 0.05,
  horizon = newHorizon(
    period = 2025:2040,
    intervals = c(1, 5, 10),
    mid_is_end = TRUE
  ))

scen <- solve_model(mod, name = "BASE", solver = solver_options$glpk)

getData(scen, "vObjective", merge = TRUE)  # discounted system cost
getData(scen, "vTechCap", merge = TRUE)    # installed capacity
```

[`solve_model()`](https://energyRt.org/reference/solve_model.md)
interpolates and solves in one call. To inspect or modify the
interpolated scenario, use the composable stages instead:

``` r

scen <- mod |>
  interpolate(name = "BASE") |>
  solve()
```

The [Get started](https://energyrt.org/articles/energyRt.html) vignette
explains the model and workflow step by step.

## Core capabilities

- **Reference energy systems** built from commodities, supply, demand,
  technologies, storage, weather, trade, taxes, subsidies, and custom
  constraints.
- **Capacity expansion and dispatch** across multiple planning periods,
  with investment, retirement, operating, resource, emissions, and
  policy constraints.
- **Nested time** using
  [timescales](https://github.com/optimal2050/timescales). The modeler
  defines the calendar hierarchy, timeslices, and their duration:
  energyRt imposes no fixed 8,760-slice ceiling. Full-resolution,
  representative, and sampled calendars can all mix different balancing
  timeframes in one model.
- **Nested geography** using
  [geoscales](https://github.com/optimal2050/geoscales), so national,
  zonal, and nodal balances can coexist and results can be mapped or
  aggregated.
- **One object, many processes.** A technology declares build
  **vintages** and parallel **clusters** — site grades, fleet types,
  tranches — and expands into ordinary processes, each with its own
  costs, efficiency and limits. Tiered commodities and tiered weather
  series give a graded resource its own balance and profile per grade,
  so the best sites run out before the next grade starts.
- **Storage as three parts.** Charger, reservoir and discharger are
  sized and priced separately, tied by duration and C-rate, with flows
  bounded by the converter rating.
- **Reusable scenarios** with recorded runs, a project registry, myopic,
  sampled, regional and guided solve workflows, and Arrow-backed
  persistence for results larger than memory.
- **Checks that catch silent nonsense.** Structural validation refuses a
  model that would solve to a meaningless answer — a flow with no
  capacity link, a constraint with no data behind it — and
  [`verify_solution()`](https://energyRt.org/reference/verify_solution.md)
  re-derives the model’s own identities from the results.
- **Analysis and communication** through tidy
  [`getData()`](https://energyRt.org/reference/getData.md) results,
  scenario and input comparison, levelized costs for technologies,
  storage and trade without a solver,
  [`draw()`](https://energyRt.org/reference/draw.md) diagrams,
  `autoplot()`, maps, and HTML/PDF/Word reports for any object.
- **A path from learning to application:** the packaged TOPIA teaching
  model develops a multi-region power system step by step, while
  [IDEEA](https://ideea-model.github.io/IDEEA/) demonstrates a
  production-scale application to India’s power system.

## A tour of the API

Each block below is the call shape, not a complete script — the
Quickstart above is the runnable one, and every topic has an article
behind it.

### Processes that extend rather than multiply

A technology converts any number of inputs into any number of outputs,
with auxiliary flows coupling its capacity or its flows to anything else
— land per MW, water or an emission per unit of fuel.
[`update()`](https://energyRt.org/reference/update.md) derives a variant
instead of re-typing the object.

``` r

EGAS <- newTechnology("EGAS",
  input   = data.frame(comm = "GAS", unit = "PJ"),
  output  = data.frame(comm = "ELC", unit = "GWh"),
  aux     = data.frame(acomm = "CO2", unit = "kt"),
  aeff    = data.frame(acomm = "CO2", comm = "GAS", cinp2aout = 56),
  af      = data.frame(af.up = 0.85),
  invcost = data.frame(invcost = 900),
  cap2act = 8.76)

EGAS_CCS <- update(EGAS, name = "EGAS_CCS",
  aeff = data.frame(acomm = "CO2", comm = "GAS", cinp2aout = 5.6))
```

### Supply, curves, trade and the model boundary

A flat price becomes a stepped supply curve in one call; corridors carry
a commodity between regions, with electrical wrappers and optional
loop-flow physics; imports and exports price what crosses the model
boundary.

``` r

SUP_GAS  <- newSupply("SUP_GAS", commodity = "GAS",
                      supply = data.frame(ava.up = 400))
SUP_GAS  <- asSupplyCurve(SUP_GAS, price = c(4, 6, 9))   # three priced steps

AC_R1_R2 <- newACLine("AC_R1_R2", commodity = "ELC",
                      from = "R1", to = "R2", reactance = 0.1)
IMP_ELC  <- newImport("IMP_ELC", commodity = "ELC",
                      import = data.frame(price = 5000))
```

### Variants: similar processes as one family

Build **vintages** and parallel **clusters** live on one object and
expand into ordinary processes, so a fleet or a graded resource is
declared once and analysed together — including its levelized cost,
member by member, without a solver.

``` r

EWIN <- newTechnology("EWIN",
  output  = data.frame(comm = "ELC", unit = "GWh"),
  cluster = data.frame(cluster = c("01", "02", "03"), order = 1:3),
  capacity = data.frame(cluster = c("01", "02", "03"), cap.up = c(8, 16, 40)),
  invcost  = data.frame(cluster = c("01", "02", "03"), invcost = c(600, 900, 1800)),
  vintage  = data.frame(olife = 25L), cap2act = 8.76)

getVariants(mod)                      # the names before interpolating
levcost(EWIN, by_variant = TRUE)      # USD/MWh per member
```

### Policy instruments and constraints

Taxes and subsidies levy on a commodity total or on named processes.
Beyond the ~150 equation families energyRt generates from your data,
[`newConstraint()`](https://energyRt.org/reference/newConstraint.md)
writes an arbitrary linear restriction over declared summands — the two
below are TOPIA’s own carbon-price and no-new-nuclear levers.

``` r

CT_CO2 <- newTax(name = "CT_CO2", comm = "CO2",
  tax = data.frame(year = c(2030, 2050), bal = c(0.02, 0.08)))

NO_NEW_NUC <- newConstraint(name = "NO_NEW_NUC", eq = "<=",
  for.each = data.frame(year = years, tech = "ENUC"),
  term1 = list(variable = "vTechNewCap"),
  rhs = 0, defVal = 0)
```

### Solve strategies

The same model can be solved with perfect foresight or with any of four
alternatives, each recorded as a run of the scenario.

``` r

base   <- solve_model(mod, name = "base")                    # intertemporal
myopic <- solve_myopic(mod, name = "myopic", step = 1L)      # window by window
sampled <- solve_by_sample(mod, name = "sampled")            # sampled calendars
by_reg <- solve_by_region(mod, name = "by_region")           # region by region
guided <- solve_guided(mod, name = "guided")                 # toward a target
```

### Model size control

Resolution is the first lever — a coarser timeframe or geoframe for a
commodity, a sampled calendar, a subset of regions. Folding and sparse
storage are the second: they drop what does not vary and what equals a
default, losslessly.

``` r

scen <- interpolate_model(mod, fold = TRUE, sparse = TRUE)
model_size(scen)              # rows per parameter, largest first
scen <- trim_parameters(scen) # drop rows no mapping indexes
```

### Scenarios, tracking and queries

A project registry keyed on path records every model, scenario and
dataset, so a study with thousands of runs stays addressable by name.
Content hashes say whether two things are really the same, and results
are queried and compared without leaving R.

``` r

reg  <- load_registry()
scen <- getScenario("base")                     # by name, from the registry
scenario_runs(scen)                             # every solve of this scenario
model_hash(mod); object_hash(EGAS)              # content, not file names

getData(scen, name = "vTechCap", merge = TRUE, variants = TRUE)
compare_scenarios(list(base = base, carbon_tax = tax_run))
```

### Visualisation and auto-reporting

``` r

draw(EGAS)                                            # the process diagram
autoplot(scen, type = "generation", comm = "ELC", by = "cluster")
plot_map(scen, name = "vTechNewCap")                  # any variable with a region
report(scen, template = "full", format = "html")      # a full scenario datasheet
```

## One model, many representations and solve routes

The model definition is independent of the mathematical-programming
runtime. Choose a representation and solve route without rewriting the
energy-system model.

| Route | What it does | Good fit |
|----|----|----|
| [GLPK / MathProg](https://www.gnu.org/software/glpk/) | Writes MathProg and solves with GLPK | First models, teaching, and transparent local runs |
| [Julia / JuMP](https://jump.dev/) | Writes JuMP for HiGHS, CBC, GLPK, CPLEX, or cuOpt | Larger local models and a choice of algorithms |
| [Python / Pyomo](https://www.pyomo.org/) | Writes Pyomo for HiGHS, CBC, GLPK, or CPLEX | Python-based solver environments |
| [GAMS](https://www.gams.com/) | Writes GAMS for solvers such as CPLEX or CBC | Existing institutional GAMS workflows |
| [multimod](https://github.com/optimal2050/multimod) *(experimental)* | Converts through a common AST to LaTeX, backend code, or an LP/MPS matrix | Translation, mathematical documentation, inspection, and direct LP assembly |

**multimod is more than another solver backend.** It parses energyRt’s
model into a language-neutral abstract syntax tree, from which it can
render the mathematical formulation in LaTeX, emit code for other
modeling languages, or skip generated model code and assemble the LP
matrix directly. The direct route writes MPS and avoids the symbolic
model-build step that can dominate time and memory on large linear
problems.

The same separation extends to remote execution.
[NEOS](https://neos-server.org/) can run supported solver jobs remotely.
The experimental [mmcloud](https://github.com/optimal2050/mmcloud) route
sends model files to cloud hardware, runs the solver there, streams its
progress, and retrieves the solution. Together, multimod and mmcloud let
energyRt assemble a large LP locally and solve it on cloud GPUs when it
exceeds the practical capacity of a workstation.

See [Solver backends](https://energyrt.org/articles/backends.html) for
setup, presets, limitations, and validation guidance.

## Installation

Install the development release from GitHub:

``` r

pak::pkg_install("optimal2050/energyRt")
```

`pak` installs energyRt’s R dependencies, including the optimal2050
packages listed in `DESCRIPTION`. You also need at least one solver
runtime. On Windows, Rtools includes GLPK; on other platforms, install
GLPK separately or configure Julia/JuMP, Python/Pyomo, or GAMS.

After installation, check the local setup:

``` r

library(energyRt)
en_setup()
en_check_dependencies()
```

See the [installation guide](https://energyrt.org/articles/install.html)
for platform-specific instructions.

## Learn more

- [Get started](https://energyrt.org/articles/energyRt.html) — the core
  idea and first model.
- [Model bricks](https://energyrt.org/articles/model-bricks.html) — the
  objects used to describe a system.
- [Time resolution](https://energyrt.org/articles/time-resolution.html),
  [timescales](https://github.com/optimal2050/timescales), and [space
  resolution](https://energyrt.org/articles/space-resolution.html) —
  user-defined nested calendars and regional hierarchies.
- [Storage](https://energyrt.org/articles/storage.html) and [Vintages &
  clusters](https://energyrt.org/articles/variants.html) — the two
  classes with the most structure, worked through end to end.
- [Units](https://energyrt.org/articles/units.html) — stating units, and
  the capacity-to-activity conversion.
- [TOPIA I](https://energyrt.org/articles/topia-build.html),
  [II](https://energyrt.org/articles/topia-use.html) and
  [III](https://energyrt.org/articles/region-aggregation.html) — build a
  complete teaching model, run scenarios on it, then aggregate its
  regions.
- [Workflow](https://energyrt.org/articles/workflow.html), [scenario
  management](https://energyrt.org/articles/scenario-management.html),
  and [reports](https://energyrt.org/articles/reports.html) — move from
  a model to a reproducible study.
- [Roadmap](https://energyrt.org/articles/roadmap.html) — current status
  and priorities on the way to v1.0.

## Upgrading from earlier versions

v0.90 removes deprecated names and compatibility shims from the 0.8
series and changes several serialized model and scenario structures.
Rebuild saved inputs from their source definitions and review the
[release notes](https://energyrt.org/news/) before comparing results.

To reproduce a pre-2026 project without migrating it, install the frozen
legacy release:

``` r

pak::pkg_install("optimal2050/energyRt@v0.50-beta")
```

energyRt v0.90 is under active development. Please report bugs and
proposed improvements in the [issue
tracker](https://github.com/optimal2050/energyRt/issues). The package
website is <https://energyrt.org>.
