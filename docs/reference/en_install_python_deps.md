# Install Python/Pyomo dependencies (deprecated)

Superseded by
[`multimod::mm_setup_python()`](https://optimal2050.github.io/multimod/reference/mm_setup_python.html),
which owns the one conda environment shared by energyRt and multimod.
This forwards to it and will be removed.

## Usage

``` r
en_install_python_deps(
  env = "multimod",
  packages = NULL,
  solver = NULL,
  channel = NULL,
  use_conda = NULL
)
```

## Arguments

- env:

  character. Forwarded as `env_name`.

- packages:

  character vector of Python packages to install.

- solver, channel, use_conda:

  Ignored, with a warning when set.

## Value

NULL, invisibly. Verify with
[`en_check_pyomo()`](https://energyRt.org/reference/en_check.md).

## Details

`solver`, `channel` and `use_conda` are no longer honoured: the
replacement installs with `pip` inside a conda environment, so it cannot
supply the CBC binary. Install a shell solver separately if you still
need one; `highspy` is part of the default package set.

## See also

Other solver: [`en_check`](https://energyRt.org/reference/en_check.md),
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md),
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md),
[`en_setup()`](https://energyRt.org/reference/en_setup.md),
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`get_neos_email()`](https://energyRt.org/reference/neos_email.md),
[`neos`](https://energyRt.org/reference/neos.md),
[`neos_build_gams_text_job()`](https://energyRt.org/reference/neos_build_gams_text_job.md),
[`neos_build_gams_xml()`](https://energyRt.org/reference/neos_build_gams_xml.md),
[`neos_gams_inline()`](https://energyRt.org/reference/neos_gams_inline.md),
[`neos_job`](https://energyRt.org/reference/neos_job.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)
