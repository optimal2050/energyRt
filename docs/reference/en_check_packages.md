# Check R package and external-tool dependencies

Companion to
[`en_check_dependencies()`](https://energyRt.org/reference/en_check.md)
(which covers the solver *backends*). `en_check_packages()` reports
whether energyRt's own R package dependencies and the extra packages
used in the training course are installed, plus the external tools some
of them need: a LaTeX engine for PDF reports (via `tinytex`) and
MuseScore for the `gm` music output. Like
[`en_check_dependencies()`](https://energyRt.org/reference/en_check.md)
it prints a status table and returns the underlying tibble invisibly.

## Usage

``` r
en_check_packages(
  extras = c("ggplot2", "patchwork", "knitr", "rmarkdown", "tinytex", "sf", "gm"),
  external = TRUE
)
```

## Arguments

- extras:

  character vector of optional / training R packages to check.

- external:

  logical; also probe external tools (LaTeX engine, MuseScore).

## Value

A tibble (`component`, `required`, `found`, `version`, `path`, `note`),
invisibly.

## See also

Other solver: [`en_check`](https://energyRt.org/reference/en_check.md),
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md),
[`en_install_python_deps()`](https://energyRt.org/reference/en_install_python_deps.md),
[`en_mosox_run()`](https://energyRt.org/reference/en_mosox_run.md),
[`en_setup()`](https://energyRt.org/reference/en_setup.md),
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`get_mosox_path()`](https://energyRt.org/reference/get_mosox_path.md),
[`get_neos_email()`](https://energyRt.org/reference/neos_email.md),
[`mosox_run()`](https://energyRt.org/reference/mosox_run.md),
[`neos`](https://energyRt.org/reference/neos.md),
[`neos_build_gams_text_job()`](https://energyRt.org/reference/neos_build_gams_text_job.md),
[`neos_build_gams_xml()`](https://energyRt.org/reference/neos_build_gams_xml.md),
[`neos_gams_inline()`](https://energyRt.org/reference/neos_gams_inline.md),
[`neos_job`](https://energyRt.org/reference/neos_job.md),
[`set_mosox_path()`](https://energyRt.org/reference/set_mosox_path.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)

## Examples

``` r
if (FALSE) { # \dontrun{
en_check_packages()
} # }
```
