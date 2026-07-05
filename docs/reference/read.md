# Read solution

The function and method read outputs of solved model/scenario and return
the scenario object populated with variables data.

## Usage

``` r
read_solution(obj, ...)

# S4 method for class 'scenario'
read(obj, ...)
```

## Arguments

- obj:

  scenario object

- ...:

  optional tmp.dir (if missing in the scenario object or to replace the
  saved path)

## Value

The function returns the scenario object with populated modOut slot from
the solved model directory.

## See also

[`solve()`](https://rdrr.io/r/base/solve.html) to run the script, solve
the scenario. [`write_sc()`](https://energyRt.org/reference/write.md) to
write model inputs.

## Examples

``` r
if (FALSE) { # \dontrun{
scen <- read(scen)
} # }
```
