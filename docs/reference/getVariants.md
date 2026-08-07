# Process variants of a solved (or interpolated) scenario

Lists the vintage/cluster variants a scenario was built from, i.e. the
provenance table `scenario@modInp@sets$variant`: which base object each
variant came from, of which class, and which `(vintage, cluster)` cell
it represents.

Variant names are an implementation detail of the solver-facing sets;
use this table (or `getData(..., variants = TRUE)`) to analyse results
by vintage or cluster rather than parsing the names.

## Usage

``` r
getVariants(scen, class = NULL)
```

## Arguments

- scen:

  a `scenario` object.

- class:

  optional character, keep only variants of these process classes (e.g.
  `"technology"`, `"storage"`).

## Value

A data.frame with columns `name`, `class`, `base`, `vintage`, `cluster`,
or `NULL` when the model has no variants.

## See also

Other technology process:
[`newTechnology()`](https://energyRt.org/reference/technology.md),
[`read_technology_xlsx()`](https://energyRt.org/reference/read_technology_xlsx.md),
[`variantSummary()`](https://energyRt.org/reference/variantSummary.md),
[`write_technology_xlsx()`](https://energyRt.org/reference/write_technology_xlsx.md)
