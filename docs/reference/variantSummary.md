# Summarise a result variable across technology variants

Aggregates a solved variable over the variants of each base technology
and reports the spread across them: total, mean, standard deviation, min
and max, plus the number of variants contributing. This is the "analyse
by cluster and vintage" view – e.g. the spread of capacity factors
across wind resource clusters, or of capacity across vintages.

## Usage

``` r
variantSummary(object, name, by = character(), weight = NULL, ...)
```

## Arguments

- object:

  a solved `scenario`.

- name:

  character, name of the variable or parameter (e.g. `"vTechCap"`).

- by:

  character vector, variant dimensions to keep as grouping columns: any
  of `"vintage"` and `"cluster"`. Empty (default) summarises across all
  variants of a base technology.

- weight:

  optional character, name of a variable to use as weights for the mean
  (e.g. `"vTechCap"` to capacity-weight a per-unit quantity). When
  `NULL` the mean is unweighted.

- ...:

  further filters passed to
  [`getData()`](https://energyRt.org/reference/getData.md) (e.g.
  `year = 2050`).

## Value

A data.frame with the grouping columns plus `n`, `total`, `mean`, `sd`,
`min` and `max`.

## See also

Other technology process:
[`getVariants()`](https://energyRt.org/reference/getVariants.md),
[`newTechnology()`](https://energyRt.org/reference/technology.md),
[`read_technology_xlsx()`](https://energyRt.org/reference/read_technology_xlsx.md),
[`write_technology_xlsx()`](https://energyRt.org/reference/write_technology_xlsx.md)

## Examples

``` r
if (FALSE) { # \dontrun{
open_project(tempdir())

ek  <- topia$modules$electricity$R3
mod <- newModel("TOPIA", data = ek$repo,
                calendar = topia$modules$calendars$s4_h24,
                region = ek$regions,
                horizon = topia$modules$horizons$base,
                discount = 0.05)

# split wind into site grades, solve, then read the result BY GRADE rather
# than by variant name
mod  <- add(mod, ek$EWIN_SITES, overwrite = TRUE)
scen <- solve_model(mod, name = "SITES", solver = solver_options$glpk)

variantSummary(scen, "vTechCap", by = "cluster")
variantSummary(scen, "vTechOut", by = c("cluster", "region"))
} # }
```
