# Build a multimod model from an interpolated energyRt scenario

Structure is read from energyRt's GAMS template; data comes from the
scenario. This is the entry point for the direct-matrix / MPS route -
everything downstream
([`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.html),
[`write_mps()`](https://optimal2050.github.io/multimod/r/reference/write_mps.html),
[`solve_highs()`](https://optimal2050.github.io/multimod/r/reference/solve_highs.html))
takes the model this returns.

## Usage

``` r
multimod_from_energyRt(
  object,
  gms = NULL,
  prefer = c("spec", "hint"),
  inMemory = TRUE,
  verbose = TRUE
)
```

## Arguments

- object:

  An interpolated energyRt scenario.

- gms:

  The GAMS model source. `NULL` (the default) reads the code energyRt
  ships in `.modelCode$GAMS`, which needs no source checkout and matches
  the installed energyRt version. Pass a path to read a working tree
  instead, or a character vector of GAMS source directly.

- prefer:

  Passed to `fill_variable_domains()`.

- inMemory:

  Load the scenario's data into memory. Must be `TRUE` for a folded
  scenario (the lazy path cannot expand wildcards) - see
  [`multimod_import_data()`](https://energyRt.org/reference/multimod_import_data.md).

- verbose:

  Logical; report progress.

## Value

A multimod model with data attached, ready for
[`model_to_lp()`](https://optimal2050.github.io/multimod/r/reference/model_to_lp.html).

## Examples

``` r
if (FALSE) { # \dontrun{
scen <- energyRt::interpolate_model(mod, name = "BASE")
m <- multimod_from_energyRt(scen)
lp <- model_to_lp(m)
write_mps(m, "model.mps", lp = lp)
} # }
```
