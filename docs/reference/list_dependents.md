# What depends on a stored model, repository or dataset

Lists the store entries that *reference* `x` rather than embedding a
copy: those that would break if it were deleted. For models and
repositories the dependents recorded on their sealed versions are
verified (does the dependent still exist, does it still reference that
version) and, with `recursive = TRUE`, followed through models to the
scenarios using them; references found in the manifests are added (the
only record for datasets, and for references saved before versions
recorded their dependents). This is the check
[`unseal_model()`](https://energyRt.org/reference/seal.md) /
[`unseal_repository()`](https://energyRt.org/reference/seal.md) make
before releasing a name.

## Usage

``` r
list_dependents(
  x,
  type = NULL,
  version = NULL,
  recursive = FALSE,
  prune = FALSE,
  details = FALSE
)
```

## Arguments

- x:

  an energyRt object, or the name of a store entry.

- type:

  the entry type when `x` is a bare name: `"model"`, `"repository"`,
  `"dataset"` or `"scenario"`.

- version:

  optional hash (or prefix) of one version.

- recursive:

  logical; follow dependents that are themselves sealed versions
  (repository -\> models -\> scenarios).

- prune:

  logical; remove the records of `x`'s own versions whose dependent no
  longer exists or no longer references them.

- details:

  logical; `TRUE` returns every column and also the stale records
  (`exists` / `references` FALSE).

## Value

a tibble: `level` (1 = references `x` directly), the dependent's `type`,
`name` and `path`, the `version` it references and whether that is `x`'s
`current` one; with `details` also `of_type`, `of_name`, `hash` (the
dependent's own), `exists`, `references` and `via`.

## See also

[`list_versions()`](https://energyRt.org/reference/list_versions.md),
[`delete_marked()`](https://energyRt.org/reference/seal.md),
[`mark_delete()`](https://energyRt.org/reference/seal.md)

## Examples

``` r
if (FALSE) { # \dontrun{
list_dependents("TOPIA", type = "model")            # which scenarios use it
list_dependents("my_repo", type = "repository", recursive = TRUE)
} # }
```
