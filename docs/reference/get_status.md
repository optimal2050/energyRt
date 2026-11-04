# Seal, mark, runs, storage and lineage of a saved object

What state a saved object is in: whether it is sealed, whether it is
queued for deletion, what runs it holds, what it costs on disk, and what
it shares with other scenarios. Reads the manifests, so the answer does
not depend on the registry being up to date.

## Usage

``` r
get_status(x = NULL, type = NULL, sizes = NULL, lineage = TRUE, ...)

# S3 method for class '`NULL`'
get_status(x = NULL, type = NULL, sizes = NULL, lineage = TRUE, ...)

# S3 method for class 'scenario'
get_status(x, type = NULL, sizes = NULL, lineage = TRUE, ...)

# S3 method for class 'model'
get_status(x, type = NULL, sizes = NULL, lineage = TRUE, ...)

# S3 method for class 'repository'
get_status(x, type = NULL, sizes = NULL, lineage = TRUE, ...)

# S3 method for class 'character'
get_status(x, type = NULL, sizes = NULL, lineage = TRUE, ...)

# Default S3 method
get_status(x, type = NULL, sizes = NULL, lineage = TRUE, ...)

# S3 method for class 'en_status'
print(x, ...)
```

## Arguments

- x:

  a saved object, a name, or `NULL` for every saved object.

- type:

  `"model"`, `"repository"`, `"dataset"` or `"scenario"`, when `x` is a
  name that needs disambiguating. A dataset is always named this way:
  [`load_dataset()`](https://energyRt.org/reference/dataset_store.md)
  returns an unclassed payload, so there is nothing to dispatch on.

- sizes:

  measure folder sizes with `dir_size()`. Defaults to `TRUE` for one
  object and `FALSE` for a whole project, where walking a large store
  would dominate the call.

- lineage:

  resolve `used_by` and check for broken references. Needs the store
  walk; with `FALSE`, `used_by` is `NA` and nothing is checked.

- ...:

  unused.

## Value

A tibble of class `en_status`, one row per object.

## Details

With an object, a name, or nothing:

- `get_status(scen)`, `get_status(mod)`, `get_status(repo)` — that
  object.

- `get_status("REF_sub")`, `get_status("merra_cf", type = "dataset")` —
  the named entry, resolved in its store. A bare name present in more
  than one store is refused rather than guessed; name the `type`.

- `get_status()` — every saved model, repository, dataset and scenario.

A **run** carries no seal and no deletion mark — those verbs act on the
four entry kinds, not on a run — so runs appear as the `runs` and
`solved` counts of the scenario that holds them.

## Lineage

A derived scenario records its origin twice, and the two can disagree.
`base` is the name from `base:` in the manifest: **provenance**, written
by `interpolate_model(base = )`. `shares` counts the tables it actually
borrows from another scenario: the **coupling**, and the only one of the
two that blocks
[`unseal_scenario()`](https://energyRt.org/reference/seal.md) or
deletion.

`used_by` is the other direction, for every kind of entry and not only
scenarios – the scenarios built on a model, the models holding a
repository, the scenarios borrowing a scenario's tables. It counts the
same edges
[`list_dependents()`](https://energyRt.org/reference/list_dependents.md)
walks, so a non-zero `used_by` is what a refusal will cite.

`base` set with `shares == 0` is therefore the ordinary safe case — the
base was not sealed when the derivation was built, or every table fell
under `getOption("energyRt.param_ref_min_rows")` and was copied.
`lineage_warn` is for the cases that are not safe: a borrowed table
whose source scenario is gone, or a recorded base that no longer exists.

## See also

[`seal_scenario()`](https://energyRt.org/reference/seal.md),
[`mark_delete()`](https://energyRt.org/reference/seal.md),
[`delete_marked()`](https://energyRt.org/reference/seal.md),
[`list_dependents()`](https://energyRt.org/reference/list_dependents.md),
[`load_registry()`](https://energyRt.org/reference/registry.md)

## Examples

``` r
if (FALSE) { # \dontrun{
get_status()                              # the whole project
get_status(scen)
get_status("merra_cf", type = "dataset")

st <- get_status()
st[st$marked, c("type", "name", "importance")]   # what delete_marked() sees
st[nzchar(st$lineage_warn), ]                    # broken references
} # }
```
