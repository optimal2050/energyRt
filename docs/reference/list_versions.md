# Versions held in the model, repository and dataset stores

One row per version: the current content of an entry and each sealed
version (each version, for a dataset), with its status, when it was
sealed, how many entries reference it and its size.

Status: `current` — the name's current content (an unsealed payload or
the current sealed version); `in use` — a superseded sealed version that
models or scenarios still reference; `marked` — a superseded version
marked for deletion
([`mark_delete()`](https://energyRt.org/reference/seal.md) with
`version =`); `unused` — a superseded version nothing references and
nobody marked. Versions are deleted only when marked, by
[`delete_marked()`](https://energyRt.org/reference/seal.md).

## Usage

``` r
list_versions(
  x = NULL,
  type = c("model", "repository", "dataset"),
  details = FALSE
)
```

## Arguments

- x:

  optional: a model, repository or dataset, or its stored name. All
  entries of `type` when `NULL`.

- type:

  any of `"model"`, `"repository"`, `"dataset"` (default: all three).

- details:

  logical; `TRUE` adds `updated`, `importance`, the dependents' names,
  the full `hash` and the `path`.

## Value

a tibble: `status`, `name`, `type`, `version` (8 characters of the
content hash; `(unsealed)` for a payload saved under a save id),
`sealed_at`, `refs`, `size_mb`, and with `details` the columns above.

## See also

[`list_dependents()`](https://energyRt.org/reference/list_dependents.md),
[`mark_delete()`](https://energyRt.org/reference/seal.md),
[`delete_marked()`](https://energyRt.org/reference/seal.md)

## Examples

``` r
if (FALSE) { # \dontrun{
list_versions()                       # every model and repository
list_versions("TOPIA", type = "model", details = TRUE)
} # }
```
