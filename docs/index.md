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

### What’s new in v0.90

Version 0.90 is a major, breaking modernization on the way to v1.0. It
brings nested time and geography into the core model, reworks
interpolation and scenario storage, expands validation and analysis
([`levcost()`](https://energyRt.org/reference/levcost.md),
[`report()`](https://energyRt.org/reference/report.md), and
`autoplot()`), and adopts the Apache-2.0 license. The 0.8-series
deprecation layer has been removed; see
[Upgrading](#upgrading-from-earlier-versions) before moving an existing
model.

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
- **Reusable scenarios** with model variants, recorded runs, myopic and
  guided solve workflows, regional decomposition, and Arrow-backed
  persistence.
- **Analysis and communication** through tidy
  [`getData()`](https://energyRt.org/reference/getData.md) results,
  comparison helpers, `autoplot()`, levelized-cost analysis, technology
  diagrams, and HTML/PDF reports.
- **A path from learning to application:** the packaged TOPIA teaching
  model develops a multi-region power system step by step, while
  [IDEEA](https://ideea-model.github.io/IDEEA/) demonstrates a
  production-scale application to India’s power system.

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
- [TOPIA I](https://energyrt.org/articles/topia-build.html) and [TOPIA
  II](https://energyrt.org/articles/topia-use.html) — build and use a
  complete teaching model.
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
