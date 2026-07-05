# Assemble a NEOS GAMS job document

Assemble a NEOS GAMS job document

## Usage

``` r
neos_build_gams_xml(
  model,
  email,
  options = "",
  parameters = "",
  gdx = "",
  wantgdx = "yes",
  wantlst = "yes",
  wantlog = "",
  comments = "energyRt",
  category = "milp",
  solver = "CPLEX"
)
```

## Arguments

- model:

  character. The GAMS model source (contents of a `.gms` file).

- email:

  character. A valid email (required by NEOS).

- options, parameters:

  character. GAMS options / double-dash parameters.

- gdx:

  character. Base64-of-gzip of an *input* GDX file; emitted as
  `<gdx><base64>...</base64></gdx>`. Empty for no input gdx.

- wantgdx, wantlst, wantlog:

  character. Non-empty to request GDX / listing / log output back.
  Default requests GDX + listing.

- comments:

  character. Free-text comment stored with the job.

- category, solver:

  NEOS category/solver (default `milp`/`CPLEX`).

## Value

A single XML string suitable for
[`neos_submit_job()`](https://energyRt.org/reference/neos_job.md).

## See also

Other solver: [`en_check`](https://energyRt.org/reference/en_check.md),
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md),
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md),
[`en_install_python_deps()`](https://energyRt.org/reference/en_install_python_deps.md),
[`en_setup()`](https://energyRt.org/reference/en_setup.md),
[`get_neos_email()`](https://energyRt.org/reference/neos_email.md),
[`neos`](https://energyRt.org/reference/neos.md),
[`neos_build_gams_text_job()`](https://energyRt.org/reference/neos_build_gams_text_job.md),
[`neos_gams_inline()`](https://energyRt.org/reference/neos_gams_inline.md),
[`neos_job`](https://energyRt.org/reference/neos_job.md)
