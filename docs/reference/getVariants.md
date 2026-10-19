# Process variants of a solved (or interpolated) scenario

Lists the vintage/cluster variants a scenario was built from, i.e. the
provenance table `scenario@modInp@sets$variant`: which base object each
variant came from, of which class, and which `(vintage, cluster)` cell
it represents.

Variant names are an implementation detail of the solver-facing sets;
use this table (or `getData(..., variants = TRUE)`) to analyse results
by vintage or cluster rather than parsing the names.

A `model` is accepted as well, which answers the same question *before*
interpolating: the names a constraint, levy or report will have to refer
to. The expansion runs on a throwaway copy, so the model passed in is
unchanged.

## Usage

``` r
getVariants(object, class = NULL)
```

## Arguments

- object:

  a `scenario` object, or a `model` to expand on the fly.

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

## Examples

``` r
ek  <- topia$modules$electricity$R3
mod <- newModel("TOPIA", data = ek$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = ek$regions,
                horizon = topia$modules$horizons$base,
                discount = 0.05)
mod <- setGeoscale(mod, ek$geoscale)

# the base kit has no variants
getVariants(mod)
#> NULL

# add the clustered-wind and vintaged-nuclear modules and ask again: this is
# the roster of names the solver-facing sets will carry
mod <- add(mod, ek$EWIN_SITES, ek$ENUC_VINT, overwrite = TRUE)
getVariants(mod)
#> Technology 'ENUC': `stock` rows without a `vintage` are assigned to the earliest vintage.
#>            name      class base vintage cluster
#> 1 ENUC_VINv2025 technology ENUC   v2025    <NA>
#> 2 ENUC_VINv2040 technology ENUC   v2040    <NA>
#> 3   EWIN_CLGOOD technology EWIN    <NA>    GOOD
#> 4   EWIN_CLPOOR technology EWIN    <NA>    POOR
getVariants(mod, class = "technology")
#> Technology 'ENUC': `stock` rows without a `vintage` are assigned to the earliest vintage.
#>            name      class base vintage cluster
#> 1 ENUC_VINv2025 technology ENUC   v2025    <NA>
#> 2 ENUC_VINv2040 technology ENUC   v2040    <NA>
#> 3   EWIN_CLGOOD technology EWIN    <NA>    GOOD
#> 4   EWIN_CLPOOR technology EWIN    <NA>    POOR
```
