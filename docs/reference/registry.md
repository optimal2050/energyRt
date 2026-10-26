# Project registry of models, scenarios, and runs

The registry is a per-project CSV file (see
[`get_registry_file()`](https://energyRt.org/reference/registry_file.md))
indexing the saved models, scenarios, and runs: one row each, with type,
name, content hash (models), path, parent (runs), timestamps, and a
memo. It is a rebuildable index — the on-disk manifests are the source
of truth, and `refresh_registry()` reconstructs the registry by
rescanning the `scenarios/` and `models/` stores.

- `newRegistry()` constructs a new, empty registry. Creating one is
  explicit: `load_registry()` does NOT invent a registry for you.

- `load_registry()` reads the registry file into a tibble, and errors
  when the file does not exist — naming `newRegistry()`. (Object stores
  create their own registry on first write, so
  [`save_model()`](https://energyRt.org/reference/model_store.md) and
  friends work in a fresh project without any ceremony.)

- `save_registry()` writes the tibble back.

- `add_to_registry()` inserts or updates one row (keyed by `type` +
  `name` + `parent`) and returns the updated tibble; it does not write
  the file.

- `find_in_registry()` filters by any combination of `type`, `name`,
  `hash`, and `parent` (exact matches; `NULL` = no filter).

- `refresh_registry()` rescans the stores under `root` and rebuilds rows
  from what is actually on disk, preserving `created`/`memo` of
  surviving entries; with `write = TRUE` (default) the result is saved.

## Usage

``` r
newRegistry()

load_registry(file = get_registry_file())

save_registry(reg, file = get_registry_file())

add_to_registry(
  reg,
  type,
  name,
  path,
  hash = "",
  model = "",
  model_hash = "",
  parent = "",
  variant = "",
  memo = "",
  sealed = FALSE,
  sealed_at = "",
  marked_delete = FALSE,
  delete_importance = NA_real_,
  marked_at = "",
  status = ""
)

# S3 method for class 'en_registry'
add(obj, ...)

# S3 method for class 'en_registry'
summary(object, ...)

find_in_registry(
  reg,
  type = NULL,
  name = NULL,
  hash = NULL,
  parent = NULL,
  variant = NULL
)

refresh_registry(
  root = ".",
  file = get_registry_file(),
  write = TRUE,
  lazy = FALSE
)
```

## Arguments

- file:

  character, path to the registry CSV (default
  [`get_registry_file()`](https://energyRt.org/reference/registry_file.md)).

- reg:

  a registry tibble as returned by `load_registry()`.

- type:

  character: `"model"`, `"repository"`, `"scenario"`, or `"run"`.

- name:

  character, object name (for runs: `"<variant>/<solve>"`).

- path:

  character, object directory, relative to the registry file's
  directory.

- hash:

  character, model content hash (from
  [`model_hash()`](https://energyRt.org/reference/model_hash.md),
  arriving with the model store); `""` for scenario/run rows.

- model_hash:

  character, for scenario rows: hash of the referenced or embedded
  model.

- parent:

  character, for run rows: the scenario name.

- variant:

  character, for run rows: the variant the run belongs to.

- memo:

  character, optional free-form note.

- sealed, sealed_at, marked_delete, delete_importance, marked_at:

  Lifecycle state, mirrored from the entry's own manifest — which stays
  the source of truth. Cached in the registry so the CSV answers
  "sealed?" and "marked for deletion?" on its own. Set by
  [`seal_model()`](https://energyRt.org/reference/seal.md) and
  [`mark_delete()`](https://energyRt.org/reference/seal.md) rather than
  passed by hand.

- status:

  character, for run rows: the solve status from `run.yml`.

- obj:

  the object a registry row is derived from.

- ...:

  unused.

- object:

  a loaded registry.

- root:

  character, project root to rescan (the directory holding the
  `scenarios/` and `models/` stores).

- write:

  logical, save the refreshed registry to `file`.

- lazy:

  logical. `FALSE` (default) always rescans — a full scan is around 0.3
  s for ~80 manifests, too cheap to risk serving a stale table. `TRUE`
  skips the scan when the registry is already up to date with the
  stores, for callers that refresh repeatedly. The check compares the
  CSV's own modification time, which changes without the content
  changing (a `git checkout`, a file copy, cloud sync, saving from
  Excel), so a registry copied in from elsewhere can read as fresh.

## Value

`load_registry()`, `add_to_registry()`, `find_in_registry()`, and
`refresh_registry()` return the registry tibble; `save_registry()`
returns `file` invisibly.

## Details

[`add()`](https://energyRt.org/reference/add.md) dispatches on a loaded
registry, so `add(reg, "scenario", "BASE", path = "scenarios/BASE")` is
`add_to_registry()` under the generic shared with models and
repositories.

[`summary()`](https://rdrr.io/r/base/summary.html) reports the registry
rather than listing it: counts by type, how many entries are sealed or
queued for deletion, how the runs ended, and — because the lifecycle
columns are a cache of each entry's manifest — a warning when the stores
have moved on since the file was written.

## Examples

``` r
reg <- newRegistry()
reg <- add_to_registry(reg, "scenario", "BASE", path = "scenarios/BASE")
find_in_registry(reg, type = "scenario")
#> # A tibble: 1 × 18
#>   type     name  hash  model model_hash path      parent variant created updated
#>   <chr>    <chr> <chr> <chr> <chr>      <chr>     <chr>  <chr>   <chr>   <chr>  
#> 1 scenario BASE  ""    ""    ""         scenario… ""     ""      2026-1… 2026-1…
#> # ℹ 8 more variables: energyRt_version <chr>, sealed <lgl>, sealed_at <chr>,
#> #   marked_delete <lgl>, delete_importance <dbl>, marked_at <chr>,
#> #   status <chr>, memo <chr>
```
