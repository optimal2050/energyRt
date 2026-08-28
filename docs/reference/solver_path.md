# Set or get the path to a solver toolchain

`set_solver_path()` and `get_solver_path()` are the generic form of the
per-backend accessors
([`set_gams_path()`](https://energyRt.org/reference/solver.md),
[`set_julia_path()`](https://energyRt.org/reference/solver.md), ...),
which are thin wrappers around them.

Setting a path is optional. When it is unset, each backend falls back to
its own discovery: `glpk` searches the session `PATH` and well-known
install locations (on Windows `glpsol` is found in Rtools, which bundles
GLPK), while `gams`, `python` and `julia` simply invoke the bare command
and let the OS resolve it on `PATH`. Set a path only to override that,
for instance to pick between several installed GAMS versions.

## Usage

``` r
set_solver_path(backend, path = NULL)

get_solver_path(backend)
```

## Arguments

- backend:

  character, one of `"gams"`, `"gdxlib"`, `"glpk"`, `"python"`,
  `"julia"`.

- path:

  character, path to the directory containing the executable or library,
  or `NULL` to clear it. The directory must exist.

## Value

`get_solver_path()` the configured path with a trailing `/`, or `NULL`
if unset; `set_solver_path()` the path, invisibly.

## See also

Other options:
[`en_config_path()`](https://energyRt.org/reference/en_config.md),
[`en_config_show()`](https://energyRt.org/reference/en_config_show.md),
[`get_arrow_format()`](https://energyRt.org/reference/arrow_format.md),
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`isVerbose()`](https://energyRt.org/reference/isVerbose.md),
[`set_log_file()`](https://energyRt.org/reference/log.md),
[`set_option()`](https://energyRt.org/reference/en_option.md),
[`set_registry_file()`](https://energyRt.org/reference/registry_file.md),
[`set_reports_path()`](https://energyRt.org/reference/reports_path.md),
[`set_scenarios_path()`](https://energyRt.org/reference/scenarios_path.md)

Other solver: [`en_check`](https://energyRt.org/reference/en_check.md),
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md),
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md),
[`en_install_python_deps()`](https://energyRt.org/reference/en_install_python_deps.md),
[`en_setup()`](https://energyRt.org/reference/en_setup.md),
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`get_neos_email()`](https://energyRt.org/reference/neos_email.md),
[`neos`](https://energyRt.org/reference/neos.md),
[`neos_build_gams_text_job()`](https://energyRt.org/reference/neos_build_gams_text_job.md),
[`neos_build_gams_xml()`](https://energyRt.org/reference/neos_build_gams_xml.md),
[`neos_gams_inline()`](https://energyRt.org/reference/neos_gams_inline.md),
[`neos_job`](https://energyRt.org/reference/neos_job.md)

## Examples

``` r
if (FALSE) { # \dontrun{
set_solver_path("gams", "C:/GAMS/win64/48.1")
get_solver_path("gams")
} # }
```
