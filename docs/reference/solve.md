# Functions and methods to solve model and scenario objects

The function interpolates model, writes the script in a directory, runs
the external software to solve the model, reads the solution results,
and returns a scenario object with the solution.

## Usage

``` r
solve_model()

# S4 method for class 'model,character'
solve(a, b, ...)

solve_scenario()

# S4 method for class 'scenario,character'
solve(a, b, ...)
```

## Arguments

- ...:

- obj:

  model or scenario object

- name:

  character name of scenario to return

- solver:

  a character or list with solver settings

- tmp.dir:

  character path to temporary directory

- tmp.del:

  logical delete temporary directory after the run

## Value

When the first argument is a model object, the function

## See also

[`read_solution()`](https://energyRt.org/reference/read.md)
