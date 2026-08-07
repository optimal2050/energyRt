# Run mosox on a GMPL model or data file

Run mosox on a GMPL model or data file

## Usage

``` r
en_mosox_run(
  model,
  data = NULL,
  output = NULL,
  command = c("compile", "solve"),
  extra_args = character(),
  check = TRUE
)
```

## Arguments

- model:

  character. Path to the GMPL model file.

- data:

  character. Optional path to the GMPL data file.

- output:

  character. Optional output path for compile or solve output.

- command:

  character. Either "compile" or "solve".

- extra_args:

  character. Additional CLI arguments to pass to mosox.

- check:

  logical. If TRUE, stop on a non-zero exit status.

## Value

A list with the execution result.

## See also

Other solver: [`en_check`](https://energyRt.org/reference/en_check.md),
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md),
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md),
[`en_install_python_deps()`](https://energyRt.org/reference/en_install_python_deps.md),
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
