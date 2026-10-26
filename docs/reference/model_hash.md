# Content hash of a model

A stable content hash
([`rlang::hash`](https://rlang.r-lib.org/reference/hash.html), xxHash)
of the model with volatile bookkeeping stripped: every `@misc` list
(paths, on-disk markers, timestamps) and the `inMemory` flag are
removed, recursively, before hashing. Two models built identically hash
identically; renaming a folder, saving, or loading does not change the
hash — changing any modeled content does.

Note: a model whose data slots were thinned to disk (e.g. inside an
on-disk scenario) hashes differently from its in-memory original,
because the data is genuinely absent from the object. Hash models
before/without thinning —
[`save_model()`](https://energyRt.org/reference/model_store.md) does.

The generalization of `model_hash()` to any S4 object of the package
(technology, storage, trade, scenario, calendar, horizon, weather, ...):
[`rlang::hash`](https://rlang.r-lib.org/reference/hash.html) of the
object with volatile bookkeeping stripped, so two identically-built
objects hash identically regardless of `@misc` contents, paths, or the
`inMemory` flag. Models and repositories delegate to their dedicated
hashes. Used as the object-identity half of the report and levcost cache
keys.

The thinning caveat of `model_hash()` applies: an object whose data
slots live on disk hashes differently from its in-memory original.

## Usage

``` r
model_hash(object)

object_hash(x)
```

## Arguments

- object:

  a model object.

- x:

  an S4 object (model and repository delegate to `model_hash()` /
  [`repository_hash()`](https://energyRt.org/reference/repository_hash.md)).

## Value

character, the full hash. The store uses its first 8 characters in
directory names (`<name>@<hash8>`).

character, the full hash.

## Examples

``` r
if (FALSE) { # \dontrun{
model_hash(mod)
} # }
```
