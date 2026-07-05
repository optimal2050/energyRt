# NEOS Server client (query the remote solver service)

Read-only queries against the NEOS Server XML-RPC API
(<https://neos-server.org>). These let you check connectivity and
discover which solver/input-method combinations are available before
wiring NEOS in as a solver backend. No account or API key is required.

## Usage

``` r
neos_ping(timeout = 30)

neos_list_categories(timeout = 30)

neos_list_solvers(category, timeout = 30)

neos_get_template(category, solver, inputMethod = "GAMS", timeout = 30)
```

## Arguments

- timeout:

  request timeout in seconds.

- category:

  NEOS solver category abbreviation, e.g. `"milp"`, `"lp"`, `"nco"` (see
  `neos_list_categories()`).

- solver:

  NEOS solver name, e.g. `"CPLEX"`, `"Gurobi"` (see
  `neos_list_solvers()`).

- inputMethod:

  input format, e.g. `"GAMS"`, `"MPS"`, `"AMPL"`.

## Value

- `neos_ping()`: a status string (invisibly), `TRUE` if the server is
  alive.

- `neos_list_categories()`: a named character vector (abbrev -\> full
  name).

- `neos_list_solvers()`: a character vector of `solver:inputMethod`
  strings.

- `neos_get_template()`: the XML job template as a single string.

## Details

NEOS provides commercial solvers free of charge **for academic /
non-commercial use only**, submitted jobs are **public and stored** (do
not send confidential data), and jobs are limited to roughly 3 GB RAM /
8 h. Please cite NEOS in publications.

## See also

Other solver: [`en_check`](https://energyRt.org/reference/en_check.md),
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md),
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md),
[`en_install_python_deps()`](https://energyRt.org/reference/en_install_python_deps.md),
[`en_setup()`](https://energyRt.org/reference/en_setup.md),
[`get_neos_email()`](https://energyRt.org/reference/neos_email.md),
[`neos_build_gams_text_job()`](https://energyRt.org/reference/neos_build_gams_text_job.md),
[`neos_build_gams_xml()`](https://energyRt.org/reference/neos_build_gams_xml.md),
[`neos_gams_inline()`](https://energyRt.org/reference/neos_gams_inline.md),
[`neos_job`](https://energyRt.org/reference/neos_job.md)

## Examples

``` r
if (FALSE) { # \dontrun{
neos_ping()
head(neos_list_categories())
neos_list_solvers("milp")
cat(neos_get_template("milp", "CPLEX", "GAMS"))
} # }
```
