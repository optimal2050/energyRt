# Retrieve model objects from a repository, model or scenario

`getObject()` returns the model building-block objects held in a
container – commodities, technologies, supplies, storages, and so on –
selected by **class**, **name**, **description**, **region** and/or any
object **slot**. It is the object-level counterpart of
[`getData()`](https://energyRt.org/reference/getData.md), which returns
those objects' parameter data.

## Usage

``` r
getObject(x, ...)

# Default S3 method
getObject(x, ...)

# S3 method for class 'repository'
getObject(
  x,
  class = NULL,
  name = NULL,
  desc = NULL,
  region = NULL,
  ...,
  regex = FALSE,
  ignore.case = TRUE,
  region_agnostic = TRUE,
  drop = FALSE
)

# S3 method for class 'model'
getObject(
  x,
  class = NULL,
  name = NULL,
  desc = NULL,
  region = NULL,
  ...,
  regex = FALSE,
  ignore.case = TRUE,
  region_agnostic = TRUE,
  drop = FALSE
)

# S3 method for class 'scenario'
getObject(
  x,
  class = NULL,
  name = NULL,
  desc = NULL,
  region = NULL,
  ...,
  regex = FALSE,
  ignore.case = TRUE,
  region_agnostic = TRUE,
  drop = FALSE
)
```

## Arguments

- x:

  a `repository`, `model` or `scenario`.

- ...:

  additional per-slot filters forwarded to the matching engine, e.g.
  `timeframe = "HOUR"`. Character filters are exact by default; a
  data.frame slot is filtered by named columns. Filtering by a slot
  implicitly restricts results to classes that have it.

- class:

  character vector of object classes to keep (e.g. `"technology"`,
  `c("supply", "import")`); `NULL` (default) keeps all classes.

- name:

  character, object name(s) to match against `@name`.

- desc:

  character, description pattern(s) to match against `@desc`.

- region:

  character, keep objects belonging to any of these regions.
  Region-agnostic objects (e.g. commodities, which carry no region) are
  kept for every region unless `region_agnostic = FALSE`.

- regex:

  logical; treat `name`, `desc` and character `...` filters as regular
  expressions instead of exact matches (default `FALSE`).

- ignore.case:

  logical; case-insensitive matching for `regex = TRUE` (default
  `TRUE`).

- region_agnostic:

  logical; whether objects with no region information satisfy a `region`
  filter (default `TRUE`).

- drop:

  logical; if `TRUE` and exactly one object matches, return that object
  itself instead of a one-element list (default `FALSE`).

## Value

A named list of model objects keyed by `@name` (empty list if none
match); or a single object when `drop = TRUE` and exactly one matches.

## Details

Class, name, description and any other slot are matched by the same
engine that powers the internal object accessors; the **region** filter
is applied with
[`get_region()`](https://energyRt.org/reference/get_region.md), which
reads regions uniformly from `@region` slots and from the
`region`/`src`/`dst` columns of data.frame slots, so it works for every
class (including `import`/`export`/`trade`, which store region
structurally). A scenario is unwrapped to its model automatically.

## See also

[`getData()`](https://energyRt.org/reference/getData.md),
[`get_region()`](https://energyRt.org/reference/get_region.md),
[`find_in_model()`](https://energyRt.org/reference/find_in_model.md)

## Examples

``` r
if (FALSE) { # \dontrun{
repo <- utopia_modules$electricity$reg3$repo
getObject(repo, class = "technology")                 # all technologies
getObject(repo, class = c("supply", "commodity"))     # two classes
getObject(repo, region = "R1")                         # everything in R1
getObject(repo, name = "ECOA", drop = TRUE)            # the ECOA object
getObject(repo, desc = "coal", regex = TRUE)           # by description
getObject(repo, class = "technology", timeframe = "HOUR")  # slot filter
} # }
```
