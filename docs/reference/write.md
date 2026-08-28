# Write scenario object as a Python, Julia, GAMS, or MathProg script with data files to a directory

Write scenario object as a Python, Julia, GAMS, or MathProg script with
data files to a directory

## Usage

``` r
write_script(scen, solver.dir = NULL, solver = NULL, ...)

write_sc(x, solver.dir = NULL, solver = NULL, ...)

write.sc(x, solver.dir = NULL, solver = NULL, ...)
```

## Arguments

- scen:

  scenario object, must be interpolated

- solver.dir:

  character, an external directory to write into; default (`NULL`)
  writes into the run's `solver/` directory under the scenario
  (`tmp.dir` is the deprecated alias).

- solver:

  list of character with solver specification.

- ...:

  additional solver parameters

## See also

[`solve()`](https://rdrr.io/r/base/solve.html) to run the script, solve
the scenario. [read_solution](https://energyRt.org/reference/read.md) to
read model solution.
