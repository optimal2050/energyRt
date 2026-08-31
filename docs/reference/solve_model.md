# Solve a model or an interpolated scenario

`solve_model()` interpolates a model with
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
and solves it (an already-interpolated scenario passed to it is solved
as-is; an un-interpolated one is interpolated first). `solve_scenario()`
solves a scenario that was already interpolated with
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
and errors otherwise. Both reuse the write / run / read framework
(`.executeScenario()`); all solver backends (GLPK/GMPL, GAMS, Pyomo,
JuMP) work unchanged. The transitional names
[`solve_mod()`](https://energyRt.org/reference/energyRt-deprecated.md) /
[`solve_scen()`](https://energyRt.org/reference/energyRt-deprecated.md)
are deprecated aliases.

## Usage

``` r
solve_model(obj, name = NULL, solver = NULL, ondisk = FALSE, fold = FALSE, ...)

solve_scenario(
  obj,
  name = obj@name,
  solver = NULL,
  solver.dir = NULL,
  transient = FALSE,
  force = FALSE,
  kvl = NULL,
  variant = NULL,
  ...,
  tmp.dir = NULL,
  tmp.del = NULL
)
```

## Arguments

- obj:

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
  into the scenario's own run directory
  `runs/[<variant>/]<label>/solver/`.

- transient:

  logical, use a throwaway timestamp directory deleted after the run (no
  run record).

- force:

  logical, re-solve a scenario already solved to optimal.

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

- tmp.dir, tmp.del:

  deprecated aliases of `solver.dir` / `transient`.

## Value

a scenario object with the solution.

## See also

[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md),
[`read_solution()`](https://energyRt.org/reference/read.md)
