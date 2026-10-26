# Save a repository to / load a repository from the repository store

`save_repository()` writes a repository into the content-addressed store
`<repositories_path>/<name>/` (see
[`get_repositories_path()`](https://energyRt.org/reference/registry_file.md)):
a `repository.yml` manifest, the thinned `repo.RData`, and parquet
stores for the large data slots. Saving identical content again is a
no-op. The repository is registered in the project registry unless
`registry = FALSE`. A stored repository can then be REFERENCED by models
instead of embedded — see
[`save_model()`](https://energyRt.org/reference/model_store.md)'s
`embed_repos`.

`load_repository()` resolves a stored repository by `name` (optionally
`"name@hash8"`, or with `hash =`) via the registry first, then by
scanning the store; `path` bypasses resolution.

## Usage

``` r
save_repository(
  repo,
  path = NULL,
  embed_datasets = NULL,
  registry = TRUE,
  format = get_storage_format(),
  overwrite = FALSE,
  rehash = TRUE,
  verbose = TRUE
)

load_repository(name, hash = NULL, path = NULL, env = NULL, verbose = TRUE)
```

## Arguments

- repo:

  a repository object.

- path:

  character, explicit store directory (bypasses the registry).

- embed_datasets:

  `NULL` (default), `TRUE`, or `FALSE` — whether the big data tables
  (weather/demand) are embedded in this entry or stored as references
  into the dataset store (see
  [`save_dataset()`](https://energyRt.org/reference/dataset_store.md)).
  `NULL` references whatever identical content is already stored and
  embeds the rest; `TRUE` always embeds; `FALSE` requires a store hit
  and errors otherwise. Mirrors `embed_repos` / `embed_model` one level
  down.

- registry:

  logical, add/refresh the registry row on save.

- format:

  storage format for the data slots (as in
  [`save_scenario()`](https://energyRt.org/reference/save_scenario.md)).

- overwrite:

  logical, rewrite the store entry even when the hash matches.

- rehash:

  logical; `FALSE` keeps the DESTINATION entry's recorded hash while
  writing the changed content — for changes the user declares
  insignificant (a description, a memo). The manifest marks the
  exception (`hash_kept: true`); references then verify against the
  retained hash. With `rehash = FALSE` the hash is a user-managed
  version tag, not a content proof.

- verbose:

  logical.

- name:

  character, repository name, `"name@hash8"`, or a store directory path.

- hash:

  character, full or short content hash to pin a version.

- env:

  environment to assign the repository into by name, or `NULL` (default)
  to return it.

## Value

`save_repository()` the repository, invisibly (with `misc$hash` /
`misc$path` recording the store entry); `load_repository()` the
repository object (or `invisible(TRUE)` when assigned into `env`).

## Examples

``` r
if (FALSE) { # \dontrun{
open_project(tempdir())

repo <- newRepository("my_repo", newCommodity("ELC", unit = "MWh"))
save_repository(repo)

load_repository("my_repo")
} # }
```
