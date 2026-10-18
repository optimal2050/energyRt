# Get one solved variable from a scenario

Get one solved variable from a scenario

## Usage

``` r
get_variable(object, name, data = TRUE)
```

## Arguments

- object:

  a scenario.

- name:

  character, the variable's name.

- data:

  logical, return the values (default) or the `variable` object.

## Value

a data.frame, a `variable`, or NULL if there is no such variable.

## See also

Other variable:
[`class-variable`](https://energyRt.org/reference/class-variable.md)

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
scen <- solve_model(mod, name = "BASE", solver = solver_options$glpk)

# the solved values
get_variable(scen, "vTechCap")

# the `variable` object itself, with its sets and units
get_variable(scen, "vTechCap", data = FALSE)
} # }
```
