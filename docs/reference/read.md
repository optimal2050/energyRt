# Read solution

The function and method read outputs of solved model/scenario and return
the scenario object populated with variables data.

## Usage

``` r
read_solution(obj, run = NULL, ...)

# S4 method for class 'scenario'
read(obj, run = NULL, ...)
```

## Arguments

- obj:

  scenario object

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
