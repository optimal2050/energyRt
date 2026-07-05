# Detect external solver software and dependencies

Read-only detectors that report whether each external backend energyRt
can use is installed and runnable. `en_check_dependencies()` runs all of
them and prints a status table; the individual `en_check_*()` functions
probe a single backend and return a one-row tibble (`component`,
`required`, `found`, `version`, `path`, `note`). None of these functions
install anything — see
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md).

Each detector honours the path configured via the corresponding
`set_*_path()` (e.g.
[`set_julia_path()`](https://energyRt.org/reference/solver.md)) and
falls back to the system `PATH`.

## Usage

``` r
en_check_glpk()

en_check_julia()

en_check_python()

en_check_pyomo()

en_check_gams()

en_check_gdx()

en_check_julia_pkgs(pkgs = c("JuMP", "HiGHS"))

en_check_dependencies(solver_pkgs = FALSE)
```

## Arguments

- pkgs:

  character vector of Julia package names to verify.

- solver_pkgs:

  verify Julia solver packages too (slower; starts Julia).

## Value

A one-row (`en_check_*`) or multi-row (`en_check_dependencies`) tibble,
returned invisibly for `en_check_dependencies()` which also prints a
table.

## See also

Other solver:
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md),
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md),
[`en_install_python_deps()`](https://energyRt.org/reference/en_install_python_deps.md),
[`en_setup()`](https://energyRt.org/reference/en_setup.md),
[`get_neos_email()`](https://energyRt.org/reference/neos_email.md),
[`neos`](https://energyRt.org/reference/neos.md),
[`neos_build_gams_text_job()`](https://energyRt.org/reference/neos_build_gams_text_job.md),
[`neos_build_gams_xml()`](https://energyRt.org/reference/neos_build_gams_xml.md),
[`neos_gams_inline()`](https://energyRt.org/reference/neos_gams_inline.md),
[`neos_job`](https://energyRt.org/reference/neos_job.md)

## Examples

``` r
if (FALSE) { # \dontrun{
en_check_dependencies()
en_check_julia()
} # }
```
