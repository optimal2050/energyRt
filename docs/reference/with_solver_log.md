# Send the solver's progress log to a file

Adds a `log_file` attribute to a JuMP/HiGHS solver option set, so the
solver writes its iteration log where it can be watched and kept.

## Usage

``` r
with_solver_log(opts, file = "highs.log")
```

## Arguments

- opts:

  a solver option set, e.g. `solver_options$julia_highs_barrier`.

- file:

  log file name. A bare name is written into the run's `solver/`
  directory; an absolute path is used as given.

## Value

`opts` with the attribute appended to its `inc3` block.

## Details

The log lands in the run's `solver/` directory, alongside the generated
model, so it can be followed while the solve runs:

    tail -f <scenario>/runs/<solve>/solver/highs.log

Without it, HiGHS logs to stdout, which a piped R session buffers – the
output appears only when the process ends, and nothing is retained.

## See also

`solver_options` (the shipped solver option sets)

Other interpolation:
[`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md),
[`subset_model_regions()`](https://energyRt.org/reference/subset_model_regions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# barrier, with a followable log
opt <- with_solver_log(solver_options$julia_highs_barrier)
scen <- write_script(scen, solver = opt)
} # }
```
