# Set up energyRt after installation

A one-call "am I ready?" entry point to run *after* energyRt is
installed. It reports the operating system, prints the system libraries
to install on Linux (report only — it never runs `sudo`), and prints the
status tables from
[`en_check_dependencies()`](https://energyRt.org/reference/en_check.md)
(solver backends) and
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md)
(R packages, training extras, and external tools). It installs nothing
itself: use the `install_energyRt()` bootstrap (see the Installation
article) to install energyRt and its dependencies, and
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md)
to set up the solver-backend library layer.

## Usage

``` r
en_setup(ref = "optimal2050/energyRt")
```

## Arguments

- ref:

  character. Package reference used to query system requirements on
  Linux (default `"optimal2050/energyRt"`).

## Value

A list, invisibly, with elements `os` (character), `deps` (the tibble
from
[`en_check_dependencies()`](https://energyRt.org/reference/en_check.md))
and `packages` (the tibble from
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md)).

## See also

Other solver: [`en_check`](https://energyRt.org/reference/en_check.md),
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md),
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md),
[`en_install_python_deps()`](https://energyRt.org/reference/en_install_python_deps.md),
[`get_neos_email()`](https://energyRt.org/reference/neos_email.md),
[`neos`](https://energyRt.org/reference/neos.md),
[`neos_build_gams_text_job()`](https://energyRt.org/reference/neos_build_gams_text_job.md),
[`neos_build_gams_xml()`](https://energyRt.org/reference/neos_build_gams_xml.md),
[`neos_gams_inline()`](https://energyRt.org/reference/neos_gams_inline.md),
[`neos_job`](https://energyRt.org/reference/neos_job.md)

## Examples

``` r
if (FALSE) { # \dontrun{
en_setup()
} # }
```
