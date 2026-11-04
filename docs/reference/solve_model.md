# Solve a model or an interpolated scenario

`solve_model()` interpolates a model with
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
and solves it (an already-interpolated scenario passed to it is solved
as-is; an un-interpolated one is interpolated first). `solve_scenario()`
solves a scenario that was already interpolated with
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
and errors otherwise. Both reuse the write / run / read framework
(`.executeScenario()`); all solver backends (GLPK/GMPL, GAMS, Pyomo,
JuMP) work unchanged. The transitional names `solve_mod()` /
`solve_scen()` are deprecated aliases.

## Usage

``` r
solve_model(
  object,
  name = NULL,
  solver = NULL,
  ondisk = FALSE,
  fold = FALSE,
  ...
)

solve_scenario(
  object,
  name = object@name,
  solver = NULL,
  solver.dir = NULL,
  transient = FALSE,
  force = FALSE,
  kvl = NULL,
  variant = NULL,
  wait = TRUE,
  ...,
  tmp.dir = NULL,
  tmp.del = NULL
)

# S4 method for class 'model,character'
solve(a, b, ...)
```

## Arguments

- object:

  a model object (`solve_model()`) or an interpolated scenario object
  (`solve_scenario()`).

- name:

  character name of the scenario to create / return.

- solver:

  a character or list with solver settings. When `NULL`, the scenario's
  own solver settings or
  [`get_default_solver()`](https://energyRt.org/reference/default_solver.md)
  are used.

- ondisk, fold:

  passed to
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md).
  Defaults (`FALSE`/`FALSE`) keep parameters in memory and unfolded,
  matching the shape the writers expect.

- ...:

  for `solve_model()`, arguments are routed to
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
  (settings / calendar / horizon / model data) or to the solver run
  (`solver.dir`, `transient`, `force`, `read.solution`, `wait`, `echo`,
  `run`, `run.conflict`, `n.threads`, ...). For `solve_scenario()`,
  arguments are passed to `.executeScenario()`. Set `echo = FALSE` for a
  quiet run: it silences both the progress messages and the solver's own
  console output.

  `run` doubles as the run label: `TRUE`/`FALSE` keeps its historical
  meaning (run the solver or only write the script), while a character
  value names the run — the solve lands in `<scenario>/runs/<run>/` (or
  `runs/<variant>/<run>/` for an own-problem variant) with a `run.yml`
  provenance record (default label: the solver directory name, e.g.
  `"glpk"`). `run.conflict` controls an existing recorded run under the
  same label: `"overwrite"` (default), `"suffix"` (append `-2`, `-3`,
  ...), `"error"`. See
  [`scenario_runs()`](https://energyRt.org/reference/scenario_runs.md)
  to list runs and
  [`read_solution()`](https://energyRt.org/reference/read.md) to switch
  between them.

- solver.dir:

  character, an EXTERNAL solver working directory. When set, the solve
  runs there verbatim and records no run; the default (`NULL`) solves
  into the scenario's own run directory `runs/[<variant>/]<label>/`.

- transient:

  logical, use a throwaway timestamp directory deleted after the run (no
  run record).

- force:

  logical, re-solve a scenario already solved to optimal.

- kvl:

  logical or `NULL`; what the caller expects of Kirchhoff's voltage law.
  It GUARDS rather than builds – the cycle constraints are baked in at
  interpolation time, so this is the last chance to notice that a solve
  is about to differ from what was asked for. `NULL` reports what the
  scenario actually carries.

- variant:

  character (`solve_scenario()` only): declare the scenario's
  interpolated problem an OWN-PROBLEM VARIANT of this name. Its solves
  land in `runs/<variant>/<solve>/`, and
  [`save_scenario()`](https://energyRt.org/reference/save_scenario.md)
  stores the variant's problem (`modInp`, settings snapshot,
  `variant.yml`) under `runs/<variant>/` — once, shared by all its
  solves — leaving the scenario-level (base) problem untouched. Switch
  between problems with
  [`read_solution()`](https://energyRt.org/reference/read.md)
  (`run = "<variant>/<solve>"` vs `run = "<solve>"`).

- wait:

  logical. `TRUE` (default) blocks until the solver exits. `FALSE`
  launches it detached and returns at once, leaving the run record at
  `status: "running"` for the solver's process to close; collect the
  result later with
  [`import_solution()`](https://energyRt.org/reference/import_solution.md)
  or [`read_solution()`](https://energyRt.org/reference/read.md).
  Detached solving works for the backends that launch a solver process –
  GLPK, GAMS, Pyomo and JuMP – and is refused together with
  `read.solution = TRUE`, `transient = TRUE` or an external
  `solver.dir`, none of which can be honoured once the call returns
  before the solve does.

- tmp.dir, tmp.del:

  deprecated aliases of `solver.dir` / `transient`.

- a, b:

  the [`solve()`](https://rdrr.io/r/base/solve.html) S4 signature: `a`
  the model, `b` the scenario name.

## Value

a scenario object with the solution.

## See also

[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md),
[`read_solution()`](https://energyRt.org/reference/read.md)

## Examples

``` r
if (FALSE) { # \dontrun{
open_project(tempdir())

topia_elc <- topia$modules$electricity
mod  <- newModel("TOPIA", data = topia_elc$repo,
                 calendar = topia$modules$calendars$s4_h24,
                 region = topia_elc$regions,
                 horizon = topia$modules$horizons$BY2025_50_by10,
                 discount = 0.05)
mod <- setGeoscale(mod, topia_elc$geoscale)
scen <- solve_model(mod, name = "BASE", solver = solver_options$glpk)

sum(getData(scen, "vObjective", merge = TRUE)$value)

# or in two steps, which is what you want when the inputs need inspecting
# before the solver sees them
scen <- interpolate_model(mod, name = "BASE2")
scen <- solve_scenario(scen, solver = solver_options$glpk)
} # }
```
