# Coefficient-range audit of a written model

Writes the scenario for GLPK (or reads an already written run directory
/ free-MPS file), has `glpsol` re-emit the constraint matrix as free
MPS, and summarises the non-zero coefficients per equation family: the
range of `|a_ij|`, the widest row and the variable families at each end
of it. Wide rows come from unit choices (a capacity in MW against an
hourly activity share, `1 / pTimesliceShare` on a ramp), and the table
shows which equations carry them before a solver has to scale them away.

## Usage

``` r
audit_coefficients(x, solver.dir = NULL, ratio = 1e+06, keep = FALSE)
```

## Arguments

- x:

  an interpolated `scenario`, a directory holding `energyRt.mod` +
  `energyRt.dat`, or the path of a free-MPS file.

- solver.dir:

  directory to write the scenario into (default: a temporary directory).
  Ignored when `x` is a path.

- ratio:

  a row whose `max |a_ij| / min |a_ij|` exceeds this is counted as wide.

- keep:

  keep the non-zero triplets in `$entries` (row, col, value).

## Value

an object of class `coefficient_audit` with `$summary` (matrix size and
global range), `$families` (one row per equation family), `$variables`
(one row per variable family), `$decades` (count of coefficients per
power of ten) and `$source` (the MPS file read).

## Examples

``` r
if (FALSE) { # \dontrun{
scen <- interpolate_model(mod, name = "audit")
au <- audit_coefficients(scen)
au
au$families[order(-au$families$ratio_max), ]
} # }
```
