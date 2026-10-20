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

`en_check_solve()` is the one exception to "read-only": it SOLVES a tiny
model (objective 8) on each configured backend, which is the only way to
tell a backend that is installed from one that can produce a number. A
backend that is not configured is not probed at all.

## Usage

``` r
en_check_glpk()

en_check_julia()

en_check_python()

en_check_pyomo()

en_check_gams()

en_check_gdx()

en_check_julia_pkgs(pkgs = c("JuMP", "HiGHS", "Arrow"))

en_check_solve(backends = NULL, timeout = 180)

en_check_dependencies(solver_pkgs = FALSE, solve = FALSE)
```

## Arguments

- pkgs:

  character vector of Julia package names to verify.

- backends:

  character vector naming the backends to probe: any of `"glpk"`,
  `"julia"`, `"pyomo"`, `"mm"`, `"gams"`. `NULL` (default) probes every
  one whose toolchain is configured EXCEPT `"gams"`, which is
  proprietary and must be asked for by name.

- timeout:

  seconds allowed per backend before the probe gives up.

- solver_pkgs:

  verify Julia solver packages too (slower; starts Julia).

- solve:

  also run `en_check_solve()`, which SOLVES a tiny model on each
  configured backend. `FALSE` by default: the plain dependency check
  starts no solver.

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
[`get_default_solver()`](https://energyRt.org/reference/default_solver.md),
[`get_neos_email()`](https://energyRt.org/reference/neos_email.md),
[`neos`](https://energyRt.org/reference/neos.md),
[`neos_build_gams_text_job()`](https://energyRt.org/reference/neos_build_gams_text_job.md),
[`neos_build_gams_xml()`](https://energyRt.org/reference/neos_build_gams_xml.md),
[`neos_gams_inline()`](https://energyRt.org/reference/neos_gams_inline.md),
[`neos_job`](https://energyRt.org/reference/neos_job.md),
[`set_solver_path()`](https://energyRt.org/reference/solver_path.md)

## Examples

``` r
if (FALSE) { # \dontrun{
en_check_dependencies()
en_check_julia()
en_check_solve()                      # solves on every configured backend
en_check_solve(backends = "gams")     # proprietary: ask for it by name
} # }
```
