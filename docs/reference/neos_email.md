# NEOS submission email

Get / set the email address NEOS requires for job submission.
`set_neos_email()` stores it in the energyRt option **and** exports it
to the `NEOS_EMAIL` environment variable, so it is picked up by BOTH
backends: the GAMS/NEOS backend (R-side,
[`neos_submit_job()`](https://energyRt.org/reference/neos_job.md)) and
the Pyomo/NEOS backend (which reads `NEOS_EMAIL` from inside the python
subprocess). `get_neos_email()` returns the option, falling back to the
`NEOS_EMAIL` environment variable, or `NULL`.

## Usage

``` r
get_neos_email()

set_neos_email(email = NULL)
```

## Arguments

- email:

  a valid email address (character), or `NULL` to clear.

## Value

`get_neos_email()` the email or `NULL`; `set_neos_email()` the email,
invisibly.

## See also

Other solver: [`en_check`](https://energyRt.org/reference/en_check.md),
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md),
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md),
[`en_install_python_deps()`](https://energyRt.org/reference/en_install_python_deps.md),
[`en_setup()`](https://energyRt.org/reference/en_setup.md),
[`neos`](https://energyRt.org/reference/neos.md),
[`neos_build_gams_text_job()`](https://energyRt.org/reference/neos_build_gams_text_job.md),
[`neos_build_gams_xml()`](https://energyRt.org/reference/neos_build_gams_xml.md),
[`neos_gams_inline()`](https://energyRt.org/reference/neos_gams_inline.md),
[`neos_job`](https://energyRt.org/reference/neos_job.md)

## Examples

``` r
if (FALSE) { # \dontrun{
set_neos_email("you@example.com")
get_neos_email()
} # }
```
