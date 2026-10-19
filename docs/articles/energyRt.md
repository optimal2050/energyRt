# Get started with energyRt

## Model systems, not implementations

> **A comprehensive, modular energy system modeling framework with an R
> interface.**

An energy system is a network of flows. Resources are extracted or
harvested, converted by technologies, moved, stored, and consumed as
useful energy. Planning that system means deciding what to build, when
to build it, and how to operate it while meeting demand and respecting
physical and policy constraints.

**energyRt** is a framework and macro-language for stating that problem
in R. Modelers describe the system with reusable components—commodities,
technologies, supply, demand, storage, and trade—rather than writing the
optimization equations for every application. energyRt generates the
sets, mappings, equations, and solver files, runs the model, and returns
results ready for analysis in R.

The package provides **modeling machinery rather than a fixed model,
dataset, or scenario**. The same components can support a small teaching
example, a national planning model, or a detailed multi-sector study.

## How it works: bricks and axes

An energyRt model combines reusable bricks with independent structural
axes:

- **Bricks** describe commodities and processes. They are inspectable R
  objects that can be drawn, updated, reused, and combined in
  repositories.
- **Time** comes from a modeler-defined nested
  [timescales](https://github.com/optimal2050/timescales) calendar.
  Annual, seasonal, hourly, representative, and sampled resolutions are
  choices, not package limits. A full 8,760-hour year is one possible
  calendar; it is not a fixed requirement or ceiling.
- **Space** comes from a nested
  [geoscales](https://github.com/optimal2050/geoscales) hierarchy.
  National, zonal, and nodal balances can coexist, with each commodity
  and process placed where its data belongs.
- **Solve routes** are interchangeable. The same model can write
  MathProg, JuMP, Pyomo, or GAMS, assemble a direct LP matrix, and solve
  locally or remotely.

Time, space, horizon, and solver are inputs to the model workflow rather
than assumptions embedded in every process. Define the system once, then
adapt those axes to the question.

The central object types are:

| Brick | Constructor | Role |
|----|----|----|
| commodity | [`newCommodity()`](https://energyRt.org/reference/newCommodity.md) | an energy carrier or accounting flow |
| technology | [`newTechnology()`](https://energyRt.org/reference/technology.md) | converts inputs into outputs |
| supply | [`newSupply()`](https://energyRt.org/reference/newSupply.md) | provides a resource at an availability and cost |
| demand | [`newDemand()`](https://energyRt.org/reference/newDemand.md) | defines consumption that must be met |
| storage | [`newStorage()`](https://energyRt.org/reference/storage.md) | moves a commodity between timeslices |
| trade | [`newTrade()`](https://energyRt.org/reference/newTrade.md) | moves a commodity between regions or the rest of the world |
| weather | [`newWeather()`](https://energyRt.org/reference/newWeather.md) | supplies exogenous profiles such as capacity factors |
| repository | [`newRepository()`](https://energyRt.org/reference/newRepository.md) | collects reusable bricks |
| model | [`newModel()`](https://energyRt.org/reference/newModel.md) | binds a repository to time, space, and a horizon |
| scenario | [`interpolate()`](https://energyRt.org/reference/interpolate_model.md) / [`solve()`](https://rdrr.io/r/base/solve.html) | materializes and solves one model configuration |

A technology connects `input -> use -> activity -> output`; costs,
capacity, availability, emissions, and auxiliary flows attach to that
chain. The [model bricks
article](https://energyrt.org/articles/model-bricks.html) develops the
complete object system.

## A five-minute model

The first model has one fuel, one power plant, and one electricity
demand. The units and timeframe are explicit: annual energy is measured
in PJ, capacity in GW, and costs in MEUR.

``` r

GAS <- newCommodity(
  name = "GAS",
  desc = "Natural gas",
  timeframe = "ANNUAL",
  unit = "PJ"
)
ELC <- newCommodity(
  name = "ELC",
  desc = "Electricity",
  timeframe = "ANNUAL",
  unit = "PJ"
)

gas_supply <- data.frame(
  cost = 6.0
)
SUP_GAS <- newSupply(
  name = "SUP_GAS",
  desc = "Natural gas supply",
  commodity = "GAS",
  unit = "PJ",
  supply = gas_supply
)

gas_efficiency <- data.frame(
  comm = "GAS",
  cinp2use = 0.55
)
gas_investment <- data.frame(
  invcost = 900
)
EGAS <- newTechnology(
  name = "EGAS",
  desc = "Combined-cycle gas power plant",
  input = list(comm = "GAS"),
  output = list(comm = "ELC"),
  ceff = gas_efficiency,
  invcost = gas_investment,
  fixom = 25,
  cap2act = 31.536,
  olife = 25L
)

electricity_demand <- data.frame(
  demand = 50
)
DEM_ELC <- newDemand(
  name = "DEM_ELC",
  desc = "Annual electricity demand",
  commodity = "ELC",
  unit = "PJ",
  demand = electricity_demand
)

draw(object = EGAS)
```

![](energyRt_files/figure-html/hello-build-1.png)

Collect the bricks, define a planning horizon, bind the model to one
region, and solve it with the bundled GLPK solver:

``` r

HELLO_REPOSITORY <- newRepository(
  name = "HELLO_REPOSITORY",
  GAS,
  ELC,
  SUP_GAS,
  EGAS,
  DEM_ELC,
  desc = "Components of the introductory electricity model"
)

HELLO_HORIZON <- newHorizon(
  name = "HELLO_HORIZON",
  desc = "Planning horizon from 2025 through 2040",
  period = 2025:2040,
  intervals = c(1, 5, 10),
  mid_is_end = TRUE
)

HELLO <- newModel(
  name = "HELLO",
  desc = "Single-region gas-to-electricity model",
  data = HELLO_REPOSITORY,
  region = "R1",
  discount = 0.05,
  horizon = HELLO_HORIZON
)

BASE <- solve_model(
  object = HELLO,
  name = "BASE",
  solver = solver_options$glpk,
  echo = FALSE
)
```

[`solve_model()`](https://energyRt.org/reference/solve_model.md)
interpolates and solves in one call. The composable form exposes the
intermediate scenario when it needs to be inspected or modified:

``` r

BASE_STAGED <- HELLO |>
  interpolate(name = "BASE_STAGED") |>
  solve()
```

Results return as tables at the same level of detail as the model:

``` r

base_objective <- getData(
  object = BASE,
  name = "vObjective",
  merge = TRUE
)
base_capacity <- getData(
  object = BASE,
  name = "vTechCap",
  merge = TRUE
)

base_objective
#> # A tibble: 1 × 3
#>   scenario name       value
#>   <chr>    <chr>      <dbl>
#> 1 BASE     vObjective 6677.
base_capacity
#> # A tibble: 3 × 6
#>   scenario name     tech  region  year value
#>   <chr>    <chr>    <chr> <chr>  <int> <dbl>
#> 1 BASE     vTechCap EGAS  R1      2025  1.59
#> 2 BASE     vTechCap EGAS  R1      2030  1.59
#> 3 BASE     vTechCap EGAS  R1      2040  1.59
```

The model built 1.59 GW of gas capacity—enough to supply 50 PJ per year
under the declared conversion efficiency. The solved scenario also
contains every underlying flow, cost, and balance.

## Change an axis, not the model

Changing the solver does not change the bricks or model definition. For
example, the same `HELLO` object can be sent to Julia and HiGHS:

``` r

BASE_HIGHS <- solve_model(
  object = HELLO,
  name = "BASE_HIGHS",
  solver = solver_options$julia_highs
)
```

The same principle applies to time and space. A model can use a compact
sampled calendar while it is being developed, then move to a full hourly
calendar or a different user-defined hierarchy. A sub-territory or
aggregated geography can be solved from the same system description.
Those choices change the model instance, not the meaning of every
technology.

See [Time
resolution](https://energyrt.org/articles/time-resolution.html) and
[Space resolution](https://energyrt.org/articles/space-resolution.html)
for worked examples.

## Modeling languages and solve routes

energyRt separates the system definition from the software used to
compile and solve it:

| Route | What it does | Good fit |
|----|----|----|
| GLPK / MathProg | writes transparent MathProg and solves with bundled `glpsol` | first models, teaching, reproducible local runs |
| Julia / JuMP | writes JuMP for HiGHS, CBC, GLPK, CPLEX, or cuOpt | larger local models and solver choice |
| Python / Pyomo | writes Pyomo for HiGHS, CBC, GLPK, or CPLEX | Python-based solver environments |
| GAMS | writes GAMS for institutional solver stacks | established GAMS workflows |
| [multimod](https://github.com/optimal2050/multimod) *(experimental)* | converts the model to a language-neutral representation, assembles LP/MPS matrices, solves with HiGHS, or renders the formulation as LaTeX | large linear problems, inspection, and mathematical documentation |
| [mmcloud](https://github.com/optimal2050/mmcloud) *(experimental)* | sends large matrix problems to cloud infrastructure and solves them with GPU-capable backends | models that exceed local memory or compute capacity |

[NEOS](https://neos-server.org/) provides another remote route for
supported solvers without a local license. The [solver backends
article](https://energyrt.org/articles/backends.html) covers
installation, presets, and operational limits.

## From one solve to a study

The first solve uses only a fraction of the framework:

- **Build and reuse.**
  [`update()`](https://energyRt.org/reference/update.md) derives
  variants without duplicating complete formulations; repositories
  organize components into model libraries.
- **Represent detail where it matters.** Nested timeframes and
  geographies let different commodities and processes balance at
  different levels.
- **Scale the formulation.** Sparse interpolation materializes only the
  tuples used by a scenario; vintages, clusters, graded resources, and
  custom constraints extend the model without changing its workflow.
- **Manage many runs.** Named projects and Arrow-backed scenario folders
  keep results addressable and allow larger-than-memory analysis.
- **Inspect and validate.**
  [`draw()`](https://energyRt.org/reference/draw.md),
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
  [`report()`](https://energyRt.org/reference/report.md), and
  [`verify_solution()`](https://energyRt.org/reference/verify_solution.md)
  expose both declared objects and solved behavior.
- **Analyze in R.** Results flow directly into `dplyr`, `ggplot2`, and
  the rest of the R ecosystem without a separate export-import step.

TOPIA, the teaching model shipped with energyRt, connects these ideas in
a multi-region electricity system with nested geography and time,
storage, trade, capacity expansion, dispatch, and policy scenarios.

## Where next

1.  [Model bricks](https://energyrt.org/articles/model-bricks.html)
    explains the object system in depth.
2.  [TOPIA I](https://energyrt.org/articles/topia-build.html) builds a
    complete multi-region electricity model brick by brick.
3.  [TOPIA II](https://energyrt.org/articles/topia-use.html) solves it
    and adds policy scenarios.
4.  [Time
    resolution](https://energyrt.org/articles/time-resolution.html) and
    [Space
    resolution](https://energyrt.org/articles/space-resolution.html)
    develop the two structural axes.
5.  [Solver backends](https://energyrt.org/articles/backends.html)
    covers local, direct-matrix, GPU, and cloud routes.
6.  [Workflow](https://energyrt.org/articles/workflow.html) moves from
    one solve to reproducible scenario studies.
7.  [Features](https://energyrt.org/articles/features.html) is the full
    capability inventory.
