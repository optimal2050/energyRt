# Content hash of a repository

The repository analogue of
[`model_hash()`](https://energyRt.org/reference/model_hash.md): a stable
content hash of the repository with volatile bookkeeping stripped (every
`@misc`, the `inMemory` flag, data.table's session-specific attributes).
Identical content hashes identically; on-disk state and session history
do not matter. As with models, a repository whose data slots were
thinned to disk hashes differently from its in-memory original — hash
before thinning
([`save_repository()`](https://energyRt.org/reference/repo_store.md)
does).

## Usage

``` r
repository_hash(repo)
```

## Arguments

- repo:

  a repository object.

## Value

character, the full hash (the store uses its first 8 characters).

## Examples

``` r
repo <- newRepository("r", newCommodity("ELC", unit = "MWh"))
repository_hash(repo)
#> [1] "3427382a1e34f6efa1863716f8d93af5"

# the hash follows the content: adding an object changes it
repository_hash(add(repo, newCommodity("COA", unit = "MMBtu")))
#> [1] "965548fbd3dfb3f41be2532550171ce4"
```
