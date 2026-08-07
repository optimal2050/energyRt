# Inline a GAMS model's `$include` files into one self-contained string

Recursively splices the contents of every `$include "file"` /
`$batinclude` directive in `main` (resolved against `base_dir`) into a
single GAMS source string. Use it to bundle a *text-mode* energyRt GAMS
scenario (model plus the `$include`d text data files) for submission to
NEOS with **no input GDX** and no local GAMS install.

## Usage

``` r
neos_gams_inline(main, base_dir = dirname(main), flatten = TRUE)
```

## Arguments

- main:

  path to the top GAMS file (e.g. `energyRt.gms`).

- base_dir:

  directory includes are resolved against (default: dir of `main`).

- flatten:

  logical; strip the `input/` and `output/` path prefixes so the job
  runs in NEOS's flat workspace (default `TRUE`).

## Value

a single character string: the fully inlined GAMS source.

## See also

Other solver: [`en_check`](https://energyRt.org/reference/en_check.md),
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md),
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
[`neos_job`](https://energyRt.org/reference/neos_job.md),
[`set_mosox_path()`](https://energyRt.org/reference/set_mosox_path.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)
