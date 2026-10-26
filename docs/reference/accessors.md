# Access registered scenarios, models, repositories, and datasets by name

The short, name-first accessors over the project stores (see the
scenario-management article). `getScenario("base")` resolves the name
through the registry (falling back to a scan of the scenarios store),
loads the thin on-disk shell, and CACHES it in the session bank `.scen`
— a second call is free, and a scenario re-saved on disk is reloaded
automatically (the cache is stamped with the folder manifest's `updated`
field). [`getData()`](https://energyRt.org/reference/getData.md) accepts
names directly and uses this cache:
`getData(c("base", "policy"), name = "vTechOut", merge = TRUE)`.

`getModel()`, `getRepository()` and `getDataset()` are the typed
companions — thin wrappers over
[`load_model()`](https://energyRt.org/reference/model_store.md),
[`load_repository()`](https://energyRt.org/reference/repo_store.md) and
[`load_dataset()`](https://energyRt.org/reference/dataset_store.md) name
resolution (uncached).

`open_project(path)` anchors the whole session on one project folder:
the registry file, the scenarios/models/repositories/datasets store
roots, and the temporary tiers for derived artifacts (`reports/`,
`levcosts/`). Without it, everything resolves relative to the working
directory (the option defaults).

## Usage

``` r
getScenario(name, ...)

# S3 method for class 'character'
getScenario(
  name,
  registry = NULL,
  run = NULL,
  refresh = FALSE,
  verbose = FALSE,
  model = NULL,
  ...
)

# S3 method for class 'en_registry'
getScenario(
  name,
  scenario = NULL,
  run = NULL,
  refresh = FALSE,
  verbose = FALSE,
  ...
)

getModel(name, hash = NULL, verbose = FALSE)

getRepository(name, hash = NULL, verbose = FALSE)

getDataset(name, hash = NULL, verbose = FALSE)

open_project(path = ".", verbose = TRUE)
```

## Arguments

- name:

  character, the object's registered name. Scenarios accept a name only;
  models/repositories/datasets also accept `"name@hash8"`.

- ...:

  passed to the dispatched accessor.

- registry:

  an `en_registry` tibble to resolve against (default:
  [`load_registry()`](https://energyRt.org/reference/registry.md)).

- run:

  character, optional run id activated in the returned scenario via
  [`read_solution()`](https://energyRt.org/reference/read.md) (the
  cached copy keeps the default run).

- refresh:

  logical, reload from disk even when the cached copy is current.

- verbose:

  logical.

- model:

  character, the model a scenario belongs to. A scenario's name is a
  label and may be shared — a 48-country pass can save every scenario as
  `"BY25A_F4"`, distinguished only by the model — in which case the name
  alone is refused and this picks one. See the `model` column of
  [`load_registry()`](https://energyRt.org/reference/registry.md).

- scenario:

  character, the scenario name to fetch from a registry.

- hash:

  character, content hash to pin a stored version.

- path:

  character, project root directory (must exist).

## Value

the requested object; `open_project()` returns the previous option
values, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
open_project("~/projects/mymodel")
base <- getScenario("base")
getData(c("base", "policy"), name = "vTechOut", merge = TRUE)
} # }
```
