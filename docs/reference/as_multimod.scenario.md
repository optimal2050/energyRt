# Build a multimod model from an interpolated scenario

Converts an energyRt scenario into a multimod model: the equations come
from energyRt's own GAMS model code (shipped as `.modelCode$GAMS`), the
data from the scenario's `@modInp`, and any
[`newConstraint()`](https://energyRt.org/reference/newConstraint.md) /
[`newCosts()`](https://energyRt.org/reference/newCosts.md) objects from
the GAMS string IR interpolation compiled onto
`@modInp@user_constraints`.

## Usage

``` r
# S3 method for class 'scenario'
as_multimod(x, ...)
```

## Arguments

- x:

  An interpolated energyRt scenario.

- ...:

  Passed to
  [`multimod_from_energyRt()`](https://energyRt.org/reference/multimod_from_energyRt.md):
  `gms`, `prefer`, `inMemory`, `verbose`.

## Value

A multimod model.

## Details

The model can then be rendered to any backend multimod supports, or
assembled straight into a matrix and solved - see
[`multimod::model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.html)
and
[`multimod::solve_highs()`](https://optimal2050.github.io/multimod/r/reference/solve_highs.html).

## See also

[`multimod_from_energyRt()`](https://energyRt.org/reference/multimod_from_energyRt.md)

## Examples

``` r
if (FALSE) { # \dontrun{
scen <- interpolate_model(mod, name = "BASE")
m    <- multimod::as_multimod(scen)
lp   <- multimod::model_to_lp(m)
} # }
```
