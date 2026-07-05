# Submit and manage a NEOS job

Low-level wrappers over the NEOS job API. `neos_submit_job()` submits a
job document and returns the `(job, password)` handle; the others use
that handle to poll status and retrieve results. **Submitting a job
sends the model to a public service** — see the notes in
[neos](https://energyRt.org/reference/neos.md).

## Usage

``` r
neos_submit_job(xml, user = NULL, password = NULL, timeout = 600)

neos_job_status(job, pw, timeout = 30)

neos_completion_code(job, pw, timeout = 30)

neos_final_results(job, pw, timeout = 120)

neos_get_output_file(job, pw, fileName, timeout = 120)

neos_wait(job, pw, poll = 5, max_wait = 600, verbose = TRUE)
```

## Arguments

- xml:

  character. A job document (see
  [`neos_build_gams_xml()`](https://energyRt.org/reference/neos_build_gams_xml.md)).

- user, password:

  NEOS account credentials for `authenticatedSubmitJob`; if `NULL`, an
  anonymous `submitJob` is used.

- timeout:

  per-request timeout in seconds.

- job:

  integer job number, and `pw` its password, from `neos_submit_job()`.

- fileName:

  name of an output file to fetch (e.g. `"soln.gdx"`).

- poll, max_wait:

  polling interval / overall wait cap in seconds.

- verbose:

  print status while waiting.

## Value

- `neos_submit_job()`: list(`job`, `password`).

- `neos_job_status()` / `neos_completion_code()`: a status string.

- `neos_final_results()`: combined solver output as text.

- `neos_get_output_file()`: raw bytes (e.g. GDX) — write with
  [`writeBin()`](https://rdrr.io/r/base/readBin.html).

- `neos_wait()`: final status (invisibly) once `"Done"`.

## See also

Other solver: [`en_check`](https://energyRt.org/reference/en_check.md),
[`en_check_packages()`](https://energyRt.org/reference/en_check_packages.md),
[`en_install_deps()`](https://energyRt.org/reference/en_install_deps.md),
[`en_install_python_deps()`](https://energyRt.org/reference/en_install_python_deps.md),
[`en_setup()`](https://energyRt.org/reference/en_setup.md),
[`get_neos_email()`](https://energyRt.org/reference/neos_email.md),
[`neos`](https://energyRt.org/reference/neos.md),
[`neos_build_gams_text_job()`](https://energyRt.org/reference/neos_build_gams_text_job.md),
[`neos_build_gams_xml()`](https://energyRt.org/reference/neos_build_gams_xml.md),
[`neos_gams_inline()`](https://energyRt.org/reference/neos_gams_inline.md)
