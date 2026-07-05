# Install Julia packages

Install Julia packages

## Usage

``` r
en_install_julia_pkgs(pkgs = NULL, update = FALSE)
```

## Arguments

- pkgs:

  A character vector of Julia packages to install. The default is
  `c("JuMP", "HiGHS", "Cbc", "Clp", "RData", "RCall", "CodecBzip2", "Gadfly", "DataFrames", "CSV", "SQLite", "Dates")`.
  If you have pre-installed CPLEX or Gurobi, you can add them to the
  list.

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
