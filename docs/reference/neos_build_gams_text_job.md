# Build a NEOS GAMS job from a text-mode scenario directory (no input GDX)

Convenience wrapper: inline the model + text data of a *written* GAMS
scenario directory and assemble a job document with an **empty**
`<gdx>`. The scenario must have been written WITHOUT a gdx (i.e.
[`set_gdxlib_path()`](https://energyRt.org/reference/solver.md) unset),
so its data lives in text `$include`d `.gms` files. This is the "text
data" path: no gdx encoding and no local GAMS/gdx library needed to
submit.

## Usage

``` r
neos_build_gams_text_job(
  dir,
  email,
  main = "energyRt.gms",
  solver = "CPLEX",
  category = "milp",
  options = "",
  parameters = "",
  wantgdx = "yes",
  wantlst = "yes",
  comments = "energyRt (text data)"
)
```

## Arguments

- dir:

  the scenario's GAMS working directory (holds `energyRt.gms`).

- email:

  character. A valid email (required by NEOS).

- main:

  name of the top GAMS file in `dir`.

- category, solver:

  NEOS category/solver (default `milp`/`CPLEX`).

- options, parameters:

  character. GAMS options / double-dash parameters.

- comments:

  character. Free-text comment stored with the job.

## Value

an XML job document string for
[`neos_submit_job()`](https://energyRt.org/reference/neos_job.md).

## See also

[`neos_build_gams_xml()`](https://energyRt.org/reference/neos_build_gams_xml.md)
for the GDX-input path.

Other solver: [`en_check`](https://energyRt.org/reference/en_check.md),
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md),
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md),
[`en_install_python_deps()`](https://energyRt.org/reference/en_install_python_deps.md),
[`en_setup()`](https://energyRt.org/reference/en_setup.md),
[`get_neos_email()`](https://energyRt.org/reference/neos_email.md),
[`neos`](https://energyRt.org/reference/neos.md),
[`neos_build_gams_xml()`](https://energyRt.org/reference/neos_build_gams_xml.md),
[`neos_gams_inline()`](https://energyRt.org/reference/neos_gams_inline.md),
[`neos_job`](https://energyRt.org/reference/neos_job.md)
