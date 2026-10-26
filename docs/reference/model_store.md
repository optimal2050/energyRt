# Save a model to / load a model from the model store

`save_model()` writes a model into the content-addressed store
`<models_path>/<name>/` (see
[`get_models_path()`](https://energyRt.org/reference/registry_file.md)):
a `model.yml` manifest, the thinned `mod.RData`, and parquet stores for
the large data slots. Saving the identical content again is a no-op. The
model is registered in the project registry (see
[`load_registry()`](https://energyRt.org/reference/registry.md)) unless
`registry = FALSE`.

`load_model()` resolves a stored model by `name` (optionally
`"name@hash8"`, or with `hash =`) via the registry first, then by
scanning the store directory; `path` bypasses resolution and loads that
directory.

## Usage

``` r
save_model(
  object,
  path = NULL,
  embed_repos = NULL,
  embed_datasets = NULL,
  mode = NULL,
  registry = TRUE,
  format = get_storage_format(),
  overwrite = FALSE,
  rehash = TRUE,
  verbose = TRUE
)

load_model(name, hash = NULL, path = NULL, env = NULL, verbose = TRUE)
```

## Arguments

- object:

  a model object.

- path:

  character, explicit store directory (bypasses the registry).

- embed_repos:

  `NULL` (default: by `mode`), `TRUE`, or `FALSE`. Controls whether the
  model's repositories are embedded in the store entry or referenced
  from the repository store (see
  [`save_repository()`](https://energyRt.org/reference/repo_store.md)).
  `NULL`: reference a repository whose identical content is a sealed
  version, embed otherwise. `TRUE`: always embed. `FALSE`: require a
  sealed version for every repository. The model hash is always taken
  over the FULL model, so it is unaffected by this choice;
  `load_model()` resolves references back.

- embed_datasets:

  the same choice one level further down, for the big data tables
  (weather/demand) of embedded repositories and the geoscale map on
  `@config` — see
  [`save_dataset()`](https://energyRt.org/reference/dataset_store.md).
  `NULL` (default) references whatever identical content is already in
  the dataset store; `TRUE` always embeds; `FALSE` requires a store hit.

- mode:

  `"ref"` or `"full"`; defaults to
  `getOption("energyRt.save_mode", "ref")`. `"ref"` references a
  repository whose exact content is a SEALED version in the repository
  store and copies the others in; `"full"` copies every repository and
  dataset in (a self-contained entry). An explicit `embed_repos` /
  `embed_datasets` overrides the mode for its level.

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

  character, model name, `"name@hash8"`, or a store directory path.

- hash:

  character, full or short content hash to pin a version.

- env:

  environment to assign the model into by name, or `NULL` (default) to
  return it.

## Value

`save_model()` the model, invisibly (with `misc$hash` / `misc$path`
recording the store entry); `load_model()` the model object (or
`invisible(TRUE)` when assigned into `env`).

## Examples

``` r
if (FALSE) { # \dontrun{
open_project(tempdir())

ek  <- topia$modules$electricity$R3
mod <- newModel("TOPIA", data = ek$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = ek$regions,
                horizon = topia$modules$horizons$base)
mod <- setGeoscale(mod, ek$geoscale)

# writes models/<name>/ and registers this version
save_model(mod)

# read it back by name (the newest version)
mod2 <- load_model("TOPIA")

# the registry lists every stored version, each addressable by its hash
load_registry()
} # }
```
