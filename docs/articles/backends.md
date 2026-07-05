# Solver backends and solver_options

energyRt formulates the optimization model **once** and then writes it
out for one of several mathematical-programming **backends**, runs the
solver, and reads the solution back. You pick the backend and the
underlying solver with a single **`solver_options` preset**; the same
model and scenario code works with all of them, which makes
cross-checking results between backends easy.

``` r

library(energyRt)
```

## The backends at a glance

| Backend | `lang` | Solvers | License | Notes |
|----|----|----|----|----|
| **GLPK / MathProg** | `GLPK` | GLPK | open-source | Ships as the `glpsol` executable; easiest to install, slow on large models. |
| **Python / Pyomo** | `PYOMO` | CBC, GLPK, CPLEX | open-source (CBC/GLPK) | Convenient with a conda environment. |
| **Julia / JuMP** | `JuMP` | HiGHS, Cbc, GLPK, CPLEX | open-source (HiGHS/Cbc/GLPK) | HiGHS barrier is fast; recommended for large models. |
| **GAMS** | `GAMS` | CPLEX, CBC | proprietary | Needs a GAMS license; supports GDX I/O and fine solver tuning. |
| **NEOS** (remote) | `PYOMO` / `GAMS` | CPLEX, CBC, … | free service | Solve on the [NEOS server](https://neos-server.org) — no local commercial solver needed. |

See the **Installation and Settings** article for how to install each
runtime and the library layer
([`en_check_dependencies()`](https://energyRt.org/reference/en_check.md),
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md)).

## Choosing a backend

`solver_options` is a named list of ready-made presets — each one pins a
backend, a solver, the data-exchange format, and any solver-specific
tuning:

``` r

names(solver_options)
solver_options$glpk
solver_options$julia_highs_barrier
```

Use a preset in one of two ways:

``` r

# 1. Set it as the session default, then solve normally
set_default_solver(solver_options$julia_highs)
get_default_solver()                       # inspect the current default
scen <- solve_mod(model)                   # uses the default

# 2. Pass it explicitly to a single solve
scen <- solve_mod(model,  solver = solver_options$glpk)
scen <- solve_scen(scen,  solver = solver_options$gams_gdx_cplex)
```

[`solve_mod()`](https://energyRt.org/reference/solve_mod.md)
interpolates a `model` and solves it;
[`solve_scen()`](https://energyRt.org/reference/solve_mod.md) solves an
already-interpolated scenario. With `solver = NULL` the scenario’s own
settings or `get_default_solver()` are used.

## The backends in detail

### GLPK / MathProg

The GNU Linear Programming Kit. energyRt writes the model in
GMPL/MathProg and calls the `glpsol` executable — the lowest-friction
option and a good default for small and medium models.

``` r

solve_mod(model, solver = solver_options$glpk)
```

### Python / Pyomo

energyRt writes a Pyomo `ConcreteModel` (data exchanged as SQLite by
default, or Arrow/feather with the `*_arrow` presets) and solves it with
the configured solver. CBC is the open-source default; CPLEX and GLPK
are also available.

``` r

solve_mod(model, solver = solver_options$pyomo_cbc)
solve_mod(model, solver = solver_options$pyomo_cplex_barrier)   # CPLEX barrier
```

### Julia / JuMP

energyRt writes a JuMP model and solves it in Julia. **HiGHS**
(open-source) is fast and the recommended choice for large models;
`*_barrier`, `*_simplex` and `*_parallel` presets select the HiGHS
algorithm. Cbc, GLPK and CPLEX are also available.

``` r

solve_mod(model, solver = solver_options$julia_highs)           # default (auto)
solve_mod(model, solver = solver_options$julia_highs_barrier)   # interior point
solve_mod(model, solver = solver_options$julia_highs_parallel)  # parallel simplex
```

### GAMS

If you have a GAMS license, energyRt can write and run a GAMS model. The
`gams_gdx_*` presets exchange data as GDX (fast, native); the
`gams_csv_*` presets use text/CSV. The `*_barrier` and `*_parallel`
presets inject a tuned `cplex.opt` (LP method, threads, crossover, …).

``` r

solve_mod(model, solver = solver_options$gams_gdx_cplex)
solve_mod(model, solver = solver_options$gams_gdx_cplex_barrier)
```

### NEOS (remote solve)

The [NEOS Server](https://neos-server.org) runs commercial solvers
(CPLEX, …) for free over the internet. The model is **built locally**
but the **solve is dispatched to NEOS**, so you can use CPLEX without a
local license. Two families are provided — via Pyomo (`neos_pyomo_*`)
and via GAMS (`neos_gams_*`, sent as inlined text so no local GAMS
install is needed).

NEOS requires an email address, set once via the `NEOS_EMAIL`
environment variable or the helper:

``` r

set_neos_email("you@example.org")
solve_mod(model, solver = solver_options$neos_gams_cplex)
solve_mod(model, solver = solver_options$neos_pyomo_cplex_barrier)
```

The low-level NEOS interface (submit/poll/fetch a job, list solvers, …)
is also exported — see [`?neos`](https://energyRt.org/reference/neos.md)
and the `neos_*` functions.

## All preset `solver_options`

The presets shipped with the package (generated from the installed
data):

| preset                   | backend       | solver | remote | data exchange |
|:-------------------------|:--------------|:-------|:-------|:--------------|
| glpk                     | GLPK/MathProg | —      |        | default       |
| pyomo_cbc                | Python/Pyomo  | cbc    |        | SQLite        |
| pyomo_cbc_arrow          | Python/Pyomo  | cbc    |        | feather       |
| pyomo_cplex              | Python/Pyomo  | cplex  |        | SQLite        |
| pyomo_cplex_barrier      | Python/Pyomo  | cplex  |        | SQLite        |
| pyomo_glpk               | Python/Pyomo  | glpk   |        | SQLite        |
| neos_pyomo_cplex         | Python/Pyomo  | —      |        | SQLite        |
| neos_pyomo_cplex_barrier | Python/Pyomo  | —      |        | SQLite        |
| neos_pyomo_cbc           | Python/Pyomo  | —      |        | SQLite        |
| julia_cbc                | Julia/JuMP    | Cbc    |        | default       |
| julia_cplex              | Julia/JuMP    | CPLEX  |        | default       |
| julia_cplex_barrier      | Julia/JuMP    | CPLEX  |        | default       |
| julia_highs              | Julia/JuMP    | HiGHS  |        | default       |
| julia_highs_arrow        | Julia/JuMP    | HiGHS  |        | feather       |
| julia_highs_barrier      | Julia/JuMP    | HiGHS  |        | default       |
| julia_glpk               | Julia/JuMP    | GLPK   |        | default       |
| julia_highs_simplex      | Julia/JuMP    | HiGHS  |        | default       |
| julia_highs_parallel     | Julia/JuMP    | HiGHS  |        | default       |
| gams_cplex               | GAMS          | CPLEX  |        | default       |
| gams_gdx_cplex           | GAMS          | CPLEX  |        | GDX           |
| gams_gdx_cplex_barrier   | GAMS          | CPLEX  |        | GDX           |
| gams_gdx_cplex_parallel  | GAMS          | CPLEX  |        | GDX           |
| gams_cbc                 | GAMS          | CBC    |        | default       |
| gams_gdx_cbc             | GAMS          | CBC    |        | GDX           |
| neos_gams_cplex          | GAMS          | CPLEX  | NEOS   | default       |
| neos_gams_cplex_barrier  | GAMS          | CPLEX  | NEOS   | default       |
| neos_gams_cbc            | GAMS          | CBC    | NEOS   | default       |

## Anatomy of a preset

Each preset is a plain list, so you can copy one and tweak it. Common
fields:

| Field | Meaning |
|----|----|
| `name` | preset name (informational) |
| `lang` | backend: `"GLPK"`, `"PYOMO"`, `"JuMP"`, `"GAMS"` |
| `solver` | underlying solver (e.g. `"HiGHS"`, `"cbc"`, `"CPLEX"`) |
| `export_format` / `import_format` | data exchange: `SQLite`, `feather` (Arrow), `GDX`, CSV/text |
| `backend` | `"neos"` for a remote NEOS solve |
| `neos_solver`, `neos_category` | remote solver and NEOS problem category |
| `inc3`, `inc4`, … | backend-specific code blocks injected into the model template (e.g. a `cplex.opt` for LP-method tuning) |

``` r

# start from a preset and customise
my_solver <- solver_options$julia_highs
my_solver$name <- "julia_highs_custom"
# ... adjust an inc* block to set HiGHS attributes ...
solve_mod(model, solver = my_solver)
```

## See also

- **Installation and Settings** — installing the runtimes and library
  layer.
- **Model bricks** — assembling commodities, processes and structures
  into a model.
- [`?solve_mod`](https://energyRt.org/reference/solve_mod.md),
  [`?solve_scen`](https://energyRt.org/reference/solve_mod.md),
  `?set_default_solver`,
  [`?set_neos_email`](https://energyRt.org/reference/neos_email.md).
