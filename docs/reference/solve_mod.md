# Solve a model or an interpolated scenario built with the new pipeline

`solve_mod()` interpolates a model with
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
and solves it. `solve_scen()` solves a scenario that was already
interpolated with
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md).
Both reuse the existing write / run / read framework
(`.executeScenario()`), so any solver backend supported by the legacy
[`solve_model()`](https://energyRt.org/reference/solve_model.md)
(GLPK/GMPL, GAMS, Pyomo, JuMP) works unchanged.

## Usage

``` r
solve_mod(obj, name = NULL, solver = NULL, ondisk = FALSE, fold = FALSE, ...)

solve_scen(
  obj,
  name = obj@name,
  solver = NULL,
  tmp.dir = NULL,
  tmp.del = FALSE,
  force = FALSE,
  ...
)
```

## Arguments

- obj:

  a model object (`solve_mod()`) or an interpolated scenario object
  (`solve_scen()`).

- name:

  character name of the scenario to create / return.

- solver:

  a character or list with solver settings. When `NULL`, the scenario's
  own solver settings or `get_default_solver()` are used.

- ondisk, fold:

  passed to
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md).
  Defaults (`FALSE`/`FALSE`) keep parameters in memory and unfolded,
  matching the shape the writers expect.

- ...:

  for `solve_mod()`, arguments are routed to
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
  (settings / calendar / horizon / model data) or to the solver run
  (`tmp.dir`, `tmp.del`, `force`, `read.solution`, `wait`, `echo`,
  `run`, `n.threads`, ...). For `solve_scen()`, arguments are passed to
  `.executeScenario()`. Set `echo = FALSE` for a quiet run: it silences
  both the progress messages and the solver's own console output.

- tmp.dir:

  character path to the solver working directory.

- tmp.del:

  logical, delete the working directory after the run.

- force:

  logical, re-solve a scenario already solved to optimal.

## Value

a scenario object with the solution.

## See also

[`solve_model()`](https://energyRt.org/reference/solve_model.md),
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md),
[`read_solution()`](https://energyRt.org/reference/read.md)
