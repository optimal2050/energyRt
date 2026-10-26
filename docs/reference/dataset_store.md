# Content hash of a dataset payload

The dataset analogue of
[`model_hash()`](https://energyRt.org/reference/model_hash.md) /
[`repository_hash()`](https://energyRt.org/reference/repository_hash.md):
a stable content hash of the payload with volatile attributes stripped.
Works for data.frames (weather/demand tables),
[`geoscales::GeoScale`](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html)
maps, and any other R object.

`save_dataset()` writes a payload — a data.frame (a weather or demand
series), a
[`geoscales::GeoScale`](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html)
map, or any R object — into the store `<datasets_path>/<name>/` (see
[`get_datasets_path()`](https://energyRt.org/reference/registry_file.md)).
An entry holds its versions by content: the payload of each version
lives under `<name>/<hash>/` and the entry's `dataset.yml` names the
current version and lists every version present. Saving identical
content again is a no-op; saving other content under the same name adds
a version and makes it current, nothing is overwritten; saving content
already stored as an older version makes that version current again.
`load_dataset()` takes the current version by name, or a pinned one by
`hash` (full or a prefix, also as `name@hash`). An entry written before
versions, with its payload beside the manifest, reads as its single
version. A stored dataset can then be REFERENCED by repositories,
models, and scenarios instead of embedded — see the `embed_datasets`
argument of
[`save_repository()`](https://energyRt.org/reference/repo_store.md),
[`save_model()`](https://energyRt.org/reference/model_store.md) and
[`save_scenario()`](https://energyRt.org/reference/save_scenario.md):
the table is stored once, however many model or scenario versions use
it.

A FUNCTIONAL dataset records a generating call instead of typed-in data:
pass `fun = "pkg::fun"` and `args = list(...)` (no `x`). By default
(`materialize = TRUE`) the call is evaluated once at save time and the
result is stored as a snapshot: the dataset's identity is the CONTENT
hash of that result, loading uses the snapshot without the generating
package, and re-evaluation (`load_dataset(evaluate = TRUE)` on a
functional entry) can verify the recorded `result_hash` — a mismatch
warns (package drift is worth seeing), or errors with `strict = TRUE`.
`materialize = FALSE` stores no snapshot and hashes the normalized call
(`hash_of: call` in the manifest); loading then requires the package.

`load_dataset()` resolves by `name` (optionally `"name@hash8"`, or with
`hash =`) via the registry first, then by scanning the store; `path`
bypasses resolution. It returns the bare payload.

## Usage

``` r
dataset_hash(x)

save_dataset(
  x,
  name,
  fun = NULL,
  args = NULL,
  materialize = TRUE,
  path = NULL,
  registry = TRUE,
  format = get_storage_format(),
  overwrite = FALSE,
  rehash = TRUE,
  memo = "",
  hash = NULL,
  verbose = TRUE
)

load_dataset(
  name,
  hash = NULL,
  path = NULL,
  evaluate = FALSE,
  strict = FALSE,
  verbose = TRUE
)
```

## Arguments

- x:

  the dataset payload.

- name:

  character, dataset name, `"name@hash8"`, or a store directory path.

- fun:

  character, a namespaced generator `"pkg::fun"` (functional datasets
  only).

- args:

  list, yaml-serializable arguments of `fun`.

- materialize:

  logical, evaluate `fun` at save time and store the result snapshot
  (default). `FALSE` records the call only.

- path:

  character, explicit store directory.

- registry:

  logical, add/refresh the registry row on save.

- format:

  storage format for tables (as in
  [`save_scenario()`](https://energyRt.org/reference/save_scenario.md)).

- overwrite:

  logical, rewrite the store entry even when the hash matches.

- rehash:

  logical; `FALSE` keeps the DESTINATION entry's recorded hash while
  writing the changed content — for changes the user declares
  insignificant (a description, a memo). The manifest marks the
  exception (`hash_kept: true`); references then verify against the
  retained hash. With `rehash = FALSE` the hash is a user-managed
  version tag, not a content proof. A version referenced by another
  store entry is never rewritten: the save stops and names the
  dependents.

- memo:

  character, optional registry note.

- hash:

  character, full or short content hash to pin a version.

- verbose:

  logical.

- evaluate:

  logical, for a functional entry: re-evaluate the recorded call
  (verifying `result_hash`) instead of only reading the snapshot. Falls
  back to the snapshot with a message when the package is missing.

- strict:

  logical, error (instead of warn) when a re-evaluated functional
  dataset no longer matches its recorded `result_hash`.

## Value

character, the full hash (the store uses its first 8 characters).

`save_dataset()` returns `list(name, hash, kind, path)`, invisibly.
`load_dataset()` returns the payload.

## Examples

``` r
if (FALSE) { # \dontrun{
open_project(tempdir())

# a table a model refers to, stored once and referenced by hash
save_dataset(topia$weather, "topia_weather")
dataset_hash(topia$weather)
load_dataset("topia_weather")

# or store the CALL that makes it -- `fun` is a namespaced name, and the
# payload is left out entirely -- and materialize it on demand
save_dataset(name = "profiles", fun = "energyRt::topia_profiles",
             args = list(regions = c("W1", "W2")), materialize = FALSE)
load_dataset("profiles", evaluate = TRUE)
} # }
```
