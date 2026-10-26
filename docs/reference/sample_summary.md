# Summarise an ensemble across samples

Median and quantiles of a variable across the samples of a
[`solve_by_sample()`](https://energyRt.org/reference/solve_by_sample.md)
ensemble — the input for range plots of capacity.

Each sample estimates the whole year, so these are quantiles of repeated
estimates, not of a partitioned total.

`mean` is weighted by each sample's `year_fraction`, because samples
need not cover equal shares of the year — `method = "sequential"` folds
a trailing singleton block into its predecessor, so one sample can be
larger than the rest. The quantiles are **unweighted** order statistics
of the replicates; a message says so whenever the coverage varies.

## Usage

``` r
sample_summary(x, name, probs = c(0.05, 0.5, 0.95), by = NULL, ...)
```

## Arguments

- x:

  a `by_sample` result.

- name:

  character, the variable to summarise (e.g. `"vTechCap"`).

- probs:

  numeric, quantiles to report.

- by:

  character, columns to group by; defaults to every key column the
  variable carries apart from `sample` and `value`.

- ...:

  passed to [`getData()`](https://energyRt.org/reference/getData.md).

## Value

a data.frame with the grouping columns, `n`, `mean` (coverage-weighted)
and one column per requested quantile (unweighted).

## See also

[`solve_by_sample()`](https://energyRt.org/reference/solve_by_sample.md)

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
mod <- setGeoscale(mod, ek$geoscale)
res <- solve_by_sample(mod, name = "BY_SAMPLE", level = "SEASON",
                       sample_size = 2, n = 2, method = "random", seed = 1,
                       solver = solver_options$glpk)

sample_summary(res, "vTechCap", by = "tech")

# a wider band, by region as well
sample_summary(res, "vTechNewCap", probs = c(0.1, 0.5, 0.9),
               by = c("tech", "region"))
} # }
```
