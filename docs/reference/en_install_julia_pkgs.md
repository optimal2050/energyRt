# Install Julia packages

Install Julia packages

## Usage

``` r
en_install_julia_pkgs(pkgs = NULL, update = FALSE)
```

## Arguments

- pkgs:

  A character vector of Julia packages to install. The default is
  `c("JuMP", "HiGHS", "GLPK", "Cbc", "Arrow", "RData", "DataFrames")`:
  the solvers the shipped `julia_*` presets name, plus `Arrow` for the
  default data exchange (see
  [`get_exchange_format()`](https://energyRt.org/reference/exchange_format.md)),
  `RData` for the `julia_highs_rdata` preset, and `DataFrames`, which
  the generated code uses. The `julia_cplex*` and `julia_cuopt*` presets
  need `CPLEX` / `cuOpt`, which are licensed or GPU-specific and so are
  not installed by default – add them to `pkgs` yourself. `Dates` is a
  Julia standard library and needs no install.

- update:

  logical; also run `Pkg.update()` after adding the packages.

## Value

NULL if the completion is successful. The verification of the
installation is done by the user or by the function
[`en_check_julia()`](https://energyRt.org/reference/en_check.md).

## Examples

``` r
if (FALSE) { # \dontrun{
en_install_julia_pkgs()
} # }
```
