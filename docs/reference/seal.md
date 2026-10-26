# Seal and unseal store entries; mark them for deletion

Store entries update IN PLACE, so finished work can be locked:
`seal_model("TOPIA")` freezes the store entry — re-saving identical
content stays a silent no-op, but CHANGED content errors until
`unseal_model()`. A scenario has two parts, sealed separately: its
INPUTS (model, settings, interpolated parameters) and its OUTPUTS
(runs). `seal_scenario(x)` seals the inputs and leaves the outputs open:
the scenario is never re-saved – scenarios built on it
(`interpolate_model(base = )`) may reference its tables – but it can
still be solved under a new run label, and
[`save_scenario()`](https://energyRt.org/reference/save_scenario.md)
then writes that run alone; runs can be dropped.
`seal_scenario(x, outputs = TRUE)` makes it an archive: no new run, no
dropped run. A sealed entry also can never trigger the
reference-mismatch warning — its content cannot drift.

The other end of the lifecycle: `mark_delete(x, importance = )` queues
an entry for deletion (HIGHER importance = more valuable = deleted
later), `unmark_delete(x)` clears the mark, and `delete_marked()` —
dry-run by default — actually removes marked entries up to an importance
threshold. Sealed entries are never deleted, even when marked.

All lifecycle state lives in the entry's manifest, so it survives moves
and syncs, and
[`refresh_registry()`](https://energyRt.org/reference/registry.md) never
disturbs it.

Scans the four stores for entries queued by `mark_delete()` and removes
those at or below `max_importance`. **Dry-run by default**: it only
lists what would go. Sealed entries are never deleted, even when marked
(reported as `skipped_sealed`), and neither are entries that something
still references (`skipped_referenced`). After a real deletion the
registry is refreshed.

## Usage

``` r
seal_model(x, verbose = TRUE)

unseal_model(x, force = FALSE, verbose = TRUE)

seal_repository(x, verbose = TRUE)

unseal_repository(x, force = FALSE, verbose = TRUE)

seal_dataset(x, verbose = TRUE)

unseal_dataset(x, verbose = TRUE)

seal_scenario(x, inputs = TRUE, outputs = FALSE, verbose = TRUE)

unseal_scenario(
  x,
  inputs = TRUE,
  outputs = TRUE,
  force = FALSE,
  verbose = TRUE
)

mark_delete(x, importance = 0, type = NULL, verbose = TRUE, version = NULL)

unmark_delete(x, type = NULL, verbose = TRUE, version = NULL)

delete_marked(
  types = NULL,
  max_importance = 0,
  dry_run = TRUE,
  ignore_refs = FALSE,
  verbose = TRUE
)
```

## Arguments

- x:

  the object (model/repository/scenario), or its registered name. For
  `mark_delete()`/`unmark_delete()` on a bare name, give `type =`.

- verbose:

  logical.

- force:

  logical; `unseal_model()` / `unseal_repository()` refuse while the
  current sealed version has dependents (see
  [`list_dependents()`](https://energyRt.org/reference/list_dependents.md)),
  listing them; `TRUE` releases the name anyway. The sealed version
  stays, so the dependents keep loading it. `unseal_scenario()` refuses
  while scenarios built on it (`interpolate_model(base = )`) reference
  its tables; `TRUE` unseals anyway, and a later change to it changes
  them.

- inputs, outputs:

  logical; the parts of a scenario to seal (or, for `unseal_scenario()`,
  to unseal). `TRUE` acts on the part, `FALSE` leaves it as it is. The
  inputs are the model, settings and interpolated parameters; the
  outputs are its runs. Outputs are sealed only with the inputs, and
  unsealing the inputs opens the outputs too.

- importance:

  numeric; HIGHER = more valuable = deleted later. `delete_marked()`
  removes only marks with importance at or below its `max_importance`.

- type:

  character, the entry type when `x` is a bare name.

- version:

  for `mark_delete()` / `unmark_delete()` on a model or repository: mark
  (or unmark) superseded sealed versions instead of the whole entry – a
  hash or prefix, several, or `"unused"` for every version nothing
  references (see
  [`list_versions()`](https://energyRt.org/reference/list_versions.md)).
  `delete_marked()` deletes marked versions that are still unreferenced.

- types:

  character, which stores to sweep (default: all four).

- max_importance:

  numeric threshold; only marks at or below it are deleted (default 0 —
  the least valuable tier).

- dry_run:

  logical; `FALSE` actually deletes.

- ignore_refs:

  logical; `TRUE` deletes referenced entries too. Named apart from
  `force` because
  [`drop_scenario_run()`](https://energyRt.org/reference/scenario_runs.md)'s
  `force` means something else (the active run).

## Value

a tibble of the marked entries (type, name, importance, sealed,
dependents, action, path), invisibly when deleting.

## Details

Since
[`save_scenario()`](https://energyRt.org/reference/save_scenario.md)
references the model store rather than embedding a copy, deleting an
entry can break the entries that point at it — and the breakage would
surface only later, at load time. So a marked entry with dependents is
skipped:
[`list_dependents()`](https://energyRt.org/reference/list_dependents.md)
lists them, and `ignore_refs = TRUE` deletes anyway.

In an **interactive** session a referenced entry is offered rather than
silently skipped: the dependents are named and the deletion confirmed
one entry at a time. A non-interactive session is never prompted — it
takes the refusal and `ignore_refs`, so a scripted sweep cannot hang.

## Examples

``` r
if (FALSE) { # \dontrun{
open_project(tempdir())

save_repository(newRepository("my_repo", newCommodity("ELC", unit = "MWh")))

# sealing and marking address the STORED version, by name: a sealed one
# refuses overwrites and deletion
seal_repository("my_repo")
unseal_repository("my_repo")

# marking is the opposite -- flag now, sweep later
mark_delete("my_repo", type = "repository", importance = 0)
delete_marked(dry_run = TRUE)      # review what would go
delete_marked(dry_run = FALSE)
unmark_delete("my_repo", type = "repository")
} # }
```
