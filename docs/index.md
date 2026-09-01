## energyRt ![energyRt hex logo](reference/figures/logo.png)

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License: AGPL
v3](https://img.shields.io/badge/License-AGPL_v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)

> 📣 **Energy System Modeling with R** — a post-conference online series
> following useR! 2026 · four live sessions, Fridays 7–28 Aug 2026,
> 12:00 UTC. [Details & registration »](https://energyRt.org/use-R-2026)

**energyRt** (*energy* system modeling *R-t*oolbox /ˈɛnərdʒi ɑrt/) is a
macro-language for energy system modeling in R. You describe an energy
system — its fuels, technologies, resources and demands — as R objects;
**energyRt** compiles them into a full capacity-expansion & dispatch
optimization model, solves it, and returns tidy results ready for
`dplyr`/`ggplot2` analysis. The technical layer (sets, equations, solver
files, results parsing) is generated for you, so you concentrate on the
system you are modeling, not on the code that optimizes it.

### One model, four backends

The energyRt optimization model (~100 predefined equations, extendable
with
[`newConstraint()`](https://energyRt.org/reference/newConstraint.md)) is
implemented in four mathematical-programming languages. The *same* model
object solves on any of them, with consistent results:

| Backend | Language | License |
|----|----|----|
| [GLPK / MathProg](https://www.gnu.org/software/glpk/) | (bundled with Rtools on Windows) | open source |
| [Julia / JuMP](https://jump.dev/) | Julia + HiGHS | open source |
| [Python / Pyomo](http://www.pyomo.org/) | Python + CBC/HiGHS | open source |
| [GAMS](http://www.gams.com/) | GAMS | commercial |

Start on zero-setup GLPK; switch backends later without touching your
model.

### Quickstart

A complete model — one fuel, one power plant, one demand — built, solved
and read in ~20 lines (GLPK, no external setup needed on Windows with
Rtools):

``` r

library(energyRt)

GAS <- newCommodity("GAS", timeframe = "ANNUAL")
ELC <- newCommodity("ELC", timeframe = "ANNUAL")

SUP_GAS <- newSupply("SUP_GAS", commodity = "GAS",
                     supply = data.frame(cost = 6.0))         # fuel price, MEUR/PJ

EGAS <- newTechnology("EGAS",
  input   = list(comm = "GAS"), output = list(comm = "ELC"),
  ceff    = data.frame(comm = "GAS", cinp2use = 0.55),        # 55% efficient
  invcost = list(invcost = 900),                              # MEUR/GW
  fixom   = 25, cap2act = 31.536, olife = 25L)

DEM_ELC <- newDemand("DEM_ELC", commodity = "ELC",
                     demand = data.frame(demand = 50))         # 50 PJ a year

mod <- newModel("HELLO",
  data    = newRepository("parts", GAS, ELC, SUP_GAS, EGAS, DEM_ELC),
  region  = "R1", discount = 0.05,
  horizon = newHorizon(period = 2025:2040, intervals = c(1, 5, 10),
                       mid_is_end = TRUE))

scen <- solve_model(mod, name = "BASE", solver = solver_options$glpk)

getData(scen, "vObjective", merge = TRUE)   # total discounted system cost
getData(scen, "vTechCap",   merge = TRUE)   # capacity the model built, GW
```

[`solve_model()`](https://energyRt.org/reference/solve_model.md)
interpolates and solves in one call — `solve(mod, ...)` is the same
thing. When you want the stages separately, each verb takes a scenario
and returns one, so they compose:

``` r

scen <- mod |> interpolate(name = "BASE") |> solve()
```

[`solve()`](https://rdrr.io/r/base/solve.html) runs the whole solver
stage — writes the script, runs the solver, waits, and reads the
solution back — so the scenario it returns is ready to query.

The [Get started](https://energyrt.org/articles/energyRt.html) vignette
walks through this example and the ideas behind it.

### What’s in the box

- **Model bricks** — commodities, technologies (with efficiencies, fuel
  blends & shares, auxiliary flows like emissions or cooling water),
  supply, demand, storage, weather, trade;
  [`draw()`](https://energyRt.org/reference/draw.md) sketches any
  technology as a diagram. → [Model
  bricks](https://energyrt.org/articles/model-bricks.html)
- **UTOPIA teaching model** — a complete multi-region electricity model
  built step by step, shipped as the `utopia_modules` data kit with
  ready scenario levers (CO₂ cap, carbon tax, renewable share, nuclear
  moratorium). → [UTOPIA I: building the
  model](https://energyrt.org/articles/utopia-build.html) · [UTOPIA II:
  running scenarios](https://energyrt.org/articles/utopia-use.html)
- **Levelized cost** —
  [`levcost()`](https://energyRt.org/reference/levcost.md) prices a
  technology *a-priori* (screening, textbook LCOE) or *ex-post* from a
  solved scenario, with `autoplot()` cost breakdowns.
- **Reports** — [`report()`](https://energyRt.org/reference/report.md)
  renders an HTML/PDF datasheet for a technology or a solved process,
  `levcost` included.
- **Scenario workflow** — tidy result extraction with
  [`getData()`](https://energyRt.org/reference/getData.md), scenario
  editing & re-solving, Arrow-backed
  [`save_scenario()`](https://energyRt.org/reference/save_scenario.md)/[`load_scenario()`](https://energyRt.org/reference/load_scenario.md)
  for larger-than-memory results. →
  [Workflow](https://energyrt.org/articles/workflow.html) ·
  [Plotting](https://energyrt.org/articles/autoplot.html)

### Learn more

- [Get started](https://energyrt.org/articles/energyRt.html) — the core
  idea in ten minutes.
- [Tutorials](https://energyrt.org/articles/) — installation, solver
  backends, model bricks, UTOPIA, workflow, plotting.
- **useR! workshop** — a hands-on training course built on energyRt and
  the UTOPIA model (Quarto book, in preparation).
- [IDEEA](https://ideea-model.github.io/IDEEA/) — an open multi-region
  model of India’s power system, built with energyRt: a production-scale
  application.

## Installation

``` r

pak::pkg_install("optimal2050/energyRt")
# or
remotes::install_github("optimal2050/energyRt")
```

To reproduce **pre-2026 models**, install the frozen legacy release
instead: `pak::pkg_install("optimal2050/energyRt@v0.50")`.

You will need at least one solver backend. On Windows, **GLPK ships with
Rtools** — no extra setup. For Julia/JuMP, Python/Pyomo, or GAMS see the
[installation article](https://energyrt.org/articles/install.html) and
the [solver backends
article](https://energyrt.org/articles/backends.html).

Geoscale features
([`utopia_geoscale()`](https://energyRt.org/reference/utopia_geoscale.md),
[`plot_geoscale()`](https://energyRt.org/reference/plot_geoscale.md),
multi-level regions) need the sibling package, which is not on CRAN:

``` r

pak::pkg_install("optimal2050/geoscales")
```

### Start here

1.  Install R and, on Windows, **Rtools** — it bundles `glpsol`, so GLPK
    is ready with no further setup and is the right backend for every
    small model.
2.  Install energyRt (above), then
    [`library(energyRt); en_setup()`](https://energyRt.org) and
    [`en_check_dependencies()`](https://energyRt.org/reference/en_check.md)
    to see what is green.
3.  Run the **Quickstart** above — one fuel, one plant, one demand,
    solved and read in ~20 lines.
4.  [Get started](https://energyrt.org/articles/energyRt.html) — the
    same model, explained.
5.  [Model bricks](https://energyrt.org/articles/model-bricks.html) →
    [Units](https://energyrt.org/articles/units.html) → [Time
    resolution](https://energyrt.org/articles/time-resolution.html) —
    what the objects are and how they fit together.
6.  [UTOPIA I: building the
    model](https://energyrt.org/articles/utopia-build.html) → [UTOPIA
    II: running
    scenarios](https://energyrt.org/articles/utopia-use.html) — a full
    multi-region model and its scenario levers.
7.  [Workflow](https://energyrt.org/articles/workflow.html) and
    [Scenario
    management](https://energyrt.org/articles/scenario-management.html)
    — saving models, recorded runs, variants.
8.  Add a second backend only when models grow: **julia/HiGHS** for
    mid-size, hourly, full-year and sampled runs.

### Development status

The current development line (**v0.89.x**) modernizes the interpolation
pipeline, scenario storage, and analysis tools (`levcost`, `report`,
`autoplot`) on the way to **v1.0**. The **v0.50** release
(*“half-way-there”*) is frozen and remains available for pre-2026
modeling projects; its model code, classes and methods will receive
fixes only.

The package website: <https://energyrt.org>
