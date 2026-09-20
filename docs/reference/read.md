# Read a solved run's solution into a scenario

Reads the variables a solver wrote for one run and returns the scenario
with its `@modOut` populated. This is the import step:
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md)
leaves the raw output on disk, and nothing else reads it.

## Usage

``` r
read_solution(obj, run = NULL, ..., ondisk = !isInMemory(obj))

# S4 method for class 'scenario'
read(obj, run = NULL, ..., ondisk = !isInMemory(obj))
```

## Arguments

- obj:

  scenario object.

- run:

  character, optional run to read: `"<solve>"` for a base-problem run or
  `"<variant>/<solve>"` — see
  [`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md).
  The scenario's active run switches to it. Default `NULL` reads the
  active run (or, for a freshly loaded scenario, the manifest's
  `default:` run).

- ...:

  optional `solver.dir` (an external solver directory, replacing the run
  resolution; `tmp.dir` is the deprecated alias)

- ondisk:

  logical. `TRUE` writes each variable into the run's `modOut/` store as
  it is read, instead of returning the whole solution in memory; the
  returned `@modOut` is then an on-disk object read lazily. Defaults to
  `!isInMemory(obj)`. Needs a run folder, so an external `solver.dir`
  falls back to memory. The scenario shell still has to be saved:
  [`save_scenario()`](https://energyRt.org/reference/save_scenario.md)
  records the store, and skips rewriting it.

## Value

The function returns the scenario object with populated modOut slot from
the solved model directory.

## Details

Without `run`, it reads the active run — or, for a freshly loaded
scenario, the `default:` run named in `scenario.yml`. Naming a variant
run switches the whole in-memory *problem*, settings and parameters
together, so [`getData()`](https://energyRt.org/reference/getData.md)
stays consistent with the run you are looking at.

Reading alone does not persist anything:
[`save_scenario()`](https://energyRt.org/reference/save_scenario.md)
writes the `modOut/` store, and
[`import_solution()`](https://energyRt.org/reference/import_solution.md)
does both in one call.
[`scenario_solutions()`](https://energyRt.org/reference/scenario_solutions.md)
lists what each run holds before you choose.

## Why the imported store, and not just `output/`

The two are different things, and `modOut/` is not a compressed copy of
`output/`. The solver's dump is untyped text in the backend's own
naming; the import normalises the backend away (GDX, CSV, Arrow and
Parquet all land in one store), synthesises the set aliases no solver
writes (`src`, `dst`, `regionp`, `yearp`, `acomm`, `commp`,
`timeslicep`), types every dimension against its declared set members
rather than the values that happen to occur, keeps an empty typed
skeleton for variables the solver skipped so their columns stay known,
and computes two variables no solver ever wrote — `vTechEmsFuel`
(emission factors against `vTechInp`) and `vUserCosts` (each cost object
against the variable it is defined on).

Re-deriving that on demand would also need `@modInp`, since the set
members come from the problem — and a variant swap may have replaced it.

## The solver's `output/` is never modified

Reading a solution only ever reads `output/`, whatever `ondisk` is. It
is the solver's raw dump and, until the solution has been imported, the
only copy of it —
[`drop_solver_outputs()`](https://energyRt.org/reference/drop_solver_outputs.md)
is the one verb that removes it, it refuses while the run is not
imported, and it is dry-run by default.

## See also

[`import_solution()`](https://energyRt.org/reference/import_solution.md)
to read and save in one step;
[`scenario_solutions()`](https://energyRt.org/reference/scenario_solutions.md)
to see what each run holds;
[`solve_scenario()`](https://energyRt.org/reference/solve_model.md),
[`save_scenario()`](https://energyRt.org/reference/save_scenario.md).

## Examples

``` r
if (FALSE) { # \dontrun{
scen <- read_solution(scen)                    # the active run
scen <- read_solution(scen, run = "low/glpk")  # a variant's run
} # }
```
