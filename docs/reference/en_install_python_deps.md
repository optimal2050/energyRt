# Install Python/Pyomo dependencies

Installs the Python library layer energyRt needs for the Pyomo backend.
When a conda/mamba executable is available (the recommended route) it
creates or reuses a named environment and installs `pyomo`, a solver
(`coincbc`), and the result-IO helpers from conda-forge. Otherwise it
falls back to `pip` into the configured Python (see
[`set_python_path()`](https://energyRt.org/reference/solver.md)); note
that `pip` cannot supply the CBC solver binary, which must then be
installed separately.

This installs *libraries only*. It does not install Python or conda
themselves — see
[`en_check_dependencies()`](https://energyRt.org/reference/en_check.md)
for guidance on the system layer.

## Usage

``` r
en_install_python_deps(
  env = "energyRt",
  packages = c("pyomo", "pandas", "pyarrow"),
  solver = "coincbc",
  channel = "conda-forge",
  use_conda = NULL
)
```

## Arguments

- env:

  character. Name of the conda environment to create/use.

- packages:

  character vector of Python packages to install.

- solver:

  character. Conda solver package to install (e.g. `"coincbc"`).

- channel:

  character. Conda channel.

- use_conda:

  logical or NULL. Force conda (`TRUE`) or pip (`FALSE`); `NULL`
  auto-detects (conda if found, else pip).

## Value

NULL, invisibly. Verify with
[`en_check_pyomo()`](https://energyRt.org/reference/en_check.md).

## See also

Other solver: [`en_check`](https://energyRt.org/reference/en_check.md),
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md),
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md),
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
en_install_python_deps()                 # conda env "energyRt" with pyomo + cbc
en_install_python_deps(use_conda = FALSE) # pip into current python
} # }
```
