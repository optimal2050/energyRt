# Write scenario object as a Python, Julia, GAMS, or MathProg script with data files to a directory

Write scenario object as a Python, Julia, GAMS, or MathProg script with
data files to a directory

## Usage

``` r
write_script(object, solver.dir = NULL, solver = NULL, ...)

write_sc(x, solver.dir = NULL, solver = NULL, ...)
```

## Arguments

- object:

  scenario object, must be interpolated

- solver.dir:

  character, an external directory to write into; default (`NULL`)
  writes into the run's `solver/` directory under the scenario
  (`tmp.dir` is the deprecated alias).

- solver:

  list of character with solver specification.

- ...:

  additional solver parameters

- x:

  scenario object, as `scen` (the `write_sc()` spelling).

## See also

[`solve()`](https://rdrr.io/r/base/solve.html) to run the script, solve
the scenario. [read_solution](https://energyRt.org/reference/read.md) to
read model solution.

## Examples

``` r
if (FALSE) { # \dontrun{
open_project(tempdir())

ek   <- topia$modules$electricity$R3
mod  <- newModel("TOPIA", data = ek$repo,
                 calendar = topia$modules$calendars$s4_h24,
                 region = ek$regions,
                 horizon = topia$modules$horizons$base,
                 discount = 0.05)
mod <- setGeoscale(mod, ek$geoscale)
scen <- interpolate_model(mod, name = "BASE")

# write the backend's model and data files without solving, to read them
# or to run the solver by hand
write_script(scen, solver = solver_options$glpk, solver.dir = tempdir())
} # }
```
