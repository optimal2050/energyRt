# Prepare a stored scenario for sharing

Copies a scenario to a new directory, drops the regenerable solver
scratch, and removes the machine it was made on — in one call. The
original is left untouched.

## Usage

``` r
prepare_for_sharing(
  object,
  path,
  scope = c("share", "store"),
  keep_runs = FALSE,
  keep_reports = FALSE,
  keep_solver = FALSE,
  verbose = TRUE
)
```

## Arguments

- object:

  a scenario object, or the path to a stored scenario.

- path:

  target directory. Required.

- scope:

  `"share"` (default) or `"store"`, passed to
  [`strip_user_info()`](https://energyRt.org/reference/strip_user_info.md).

- keep_runs:

  logical, keep the solver scratch. `FALSE` by default.

- keep_reports:

  logical, keep rendered reports. `FALSE` by default: they re-render
  from the object and the template, and are the one artifact that can
  carry an absolute path or a user name inside a binary (PDF, DOCX)
  where scrubbing is not reliable — so they are removed, not cleaned.

- keep_solver:

  logical, keep the solver settings.

- verbose:

  logical.

## Value

invisibly, a list: `path`, `mb_before`, `mb_after`, `freed_mb`,
`remaining` (files still carrying the home directory or user name) and
`unresolved` (references that will not travel).

## Details

Runs
[`drop_solver_outputs()`](https://energyRt.org/reference/drop_solver_outputs.md)
then
[`strip_user_info()`](https://energyRt.org/reference/strip_user_info.md)
on the copy, and reports anything it could not make portable rather than
producing a folder that fails on the recipient's machine.

Two things it does **not** do, deliberately. It will not write in place:
the point is an artifact beside the working original. And it does not
re-save through `save_scenario(path = )`, which for an already-stored
scenario writes only the shell and leaves the data behind.

A scenario that *references* a model or dataset store rather than
embedding it cannot be made self-contained by copying alone; those
references are listed in `unresolved` and the recipient will need the
store too.

## See also

[`strip_user_info()`](https://energyRt.org/reference/strip_user_info.md),
[`drop_solver_outputs()`](https://energyRt.org/reference/drop_solver_outputs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
open_project("~/projects/mymodel")
scen <- getScenario("BASE")

# a copy with absolute paths, user names and solver scratch removed
prepare_for_sharing(scen, path = file.path(tempdir(), "share"))

# keep the runs and reports, for a colleague who will read them
prepare_for_sharing(scen, path = file.path(tempdir(), "share"),
                    keep_runs = TRUE, keep_reports = TRUE)
} # }
```
