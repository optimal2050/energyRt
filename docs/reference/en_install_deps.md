# Install the energyRt dependency library layer

Orchestrator that auto-installs the *safe* library layer: optional R
packages, Julia solver packages (via
[`en_install_julia_pkgs()`](https://energyRt.org/reference/en_install_julia_pkgs.md))
when Julia is present, and Python/Pyomo packages (via
[`en_install_python_deps()`](https://energyRt.org/reference/en_install_python_deps.md))
when Python or conda is present. System software that is missing (Julia,
Python, conda, GAMS) is *not* auto-installed — the function reports it
and points to the platform-specific instructions printed by
[`en_check_dependencies()`](https://energyRt.org/reference/en_check.md).

## Usage

``` r
en_install_deps(julia = TRUE, python = TRUE, r = TRUE, recheck = TRUE)
```

## Arguments

- julia, python, r:

  logical. Whether to install each layer.

- recheck:

  logical. Run
  [`en_check_dependencies()`](https://energyRt.org/reference/en_check.md)
  before and after.

## Value

NULL, invisibly.

## See also

Other solver: [`en_check`](https://energyRt.org/reference/en_check.md),
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md),
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
en_install_deps()
} # }
```
