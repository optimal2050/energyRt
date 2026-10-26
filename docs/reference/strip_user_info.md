# Remove machine and user information from a stored scenario

Clears the absolute paths, solver command line and — with
`scope = "share"` — the host/user provenance from a scenario, so the
folder can move between machines or leave the team.

## Usage

``` r
strip_user_info(
  object,
  path = NULL,
  scope = c("share", "store"),
  keep_solver = FALSE,
  confirm = FALSE,
  verbose = TRUE
)
```

## Arguments

- object:

  a scenario object.

- path:

  target directory for a cleaned copy, or `NULL` to edit in place.

- scope:

  `"share"` (default) or `"store"` — see Details.

- keep_solver:

  logical, keep the solver settings and any user-authored template code.
  `FALSE` by default.

- confirm:

  logical, required to edit in place.

- verbose:

  logical.

## Value

invisibly, a list with `path`, `scope`, `run_yml` (run and solution
records scrubbed), `solver_csv`, and `remaining` — files that still
contain the home directory or user name after the pass, which should be
empty.

## Details

`scope = "store"` removes only what breaks cross-machine use: the stored
absolute paths and the solver command line, which is the one path the
load-time rebase does not reconstruct and which every backend
regenerates when empty. Provenance is kept, because on a shared drive
"who ran this" is the point.

`scope = "share"` (default) additionally clears `hostname` and `user`
from every `run.yml`, the save-history `logfile.csv`, and user-authored
constraint code.

Structural bookkeeping is never touched — in particular `@misc$onDisk`,
which is not user-identifying and whose loss would silently return every
result as zero rows.

**Rendered reports are reported, not cleaned.** A report can embed an
absolute path or a user name inside a PDF or DOCX, where byte-level
scrubbing would corrupt the file; such files show up in `remaining`.
Remove them with
[`clear_report_cache()`](https://energyRt.org/reference/clear_report_cache.md)
— they re-render — or let
[`prepare_for_sharing()`](https://energyRt.org/reference/prepare_for_sharing.md)
drop them, which it does by default.

Passing `path` writes a cleaned copy and leaves the original working.
`path = NULL` edits in place, which is irreversible, so it is a dry run
unless `confirm = TRUE`.

## See also

[`prepare_for_sharing()`](https://energyRt.org/reference/prepare_for_sharing.md),
[`scenario_artifacts()`](https://energyRt.org/reference/scenario_artifacts.md)

## Examples

``` r
if (FALSE) { # \dontrun{
open_project("~/projects/mymodel")
scen <- getScenario("BASE")

# in place, on the scenario's own folder -- `confirm = TRUE` is required
# because it rewrites what is on disk
strip_user_info(scen, confirm = TRUE)
} # }
```
