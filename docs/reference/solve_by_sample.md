# Solve a model on samples of its calendar

Solves a model repeatedly on samples of its calendar — consecutive
blocks tiling the year, disjoint random draws, or bootstrap draws —
storing each run as an own-problem variant of one scenario. The set is
then combined into distributions rather than a single point answer.

## Usage

``` r
solve_by_sample(
  object,
  name = NULL,
  ...,
  samples = NULL,
  level = NULL,
  sample_size = NULL,
  n = NULL,
  method = c("sequential", "random", "bootstrap"),
  seed = NULL,
  calendar = NULL,
  solver = NULL,
  base = NULL,
  ncap_floor = 0,
  ncap_stat = c("mean", "median"),
  on_error = c("stop", "continue"),
  overwrite = FALSE,
  keep_scenarios = TRUE,
  verbose = TRUE
)
```

## Arguments

- object:

  a model object.

- name:

  character, the sequence name (one scenario folder).

- ...:

  passed to
  [`interpolate_model()`](https://energyRt.org/reference/interpolate_model.md)
  for every sample.

- samples:

  a `calendar_samples` table, a `data.frame(sample, timeslice)`, or a
  calendar carrying a `sample` column. When `NULL`, built by
  [`calendar_samples()`](https://energyRt.org/reference/calendar_samples.md)
  from `sample_size` / `n` / `method`.

- level, sample_size, n, method, seed:

  passed to
  [`calendar_samples()`](https://energyRt.org/reference/calendar_samples.md)
  when `samples` is `NULL`.

- calendar:

  the calendar to sample; the model's own when `NULL`.

- solver:

  a solver option list; the scenario's default when `NULL`.

- ncap_floor:

  numeric in `[0, 1]`. `0` (default) leaves every sample free to choose
  its own capacity, and the ensemble is a distribution over different
  investment plans. Above `0` the samples are solved a second time
  against a floor of `ncap_floor` times the ensemble's own new capacity,
  so the spread becomes period variation at one plan rather than
  variation between plans. `0.9` lets a sample trim a tenth; `1` holds
  the consensus exactly. It is a floor, not a fix: a sample may still
  build ABOVE it, and that is the signal worth reading – it marks a
  period the consensus plan cannot serve.

- ncap_stat:

  `"mean"` (default) or `"median"`, the statistic the consensus is taken
  with. Used only when `ncap_floor > 0`.

- on_error:

  `"stop"` (default) or `"continue"` — with `"continue"` a sample that
  fails to solve is recorded and the ensemble carries on.

- overwrite:

  logical; `FALSE` (default) refuses to run when a variant of the same
  label already exists on disk with a different type or different
  parameters. `TRUE` skips that guard and writes into it.

- keep_scenarios:

  logical, keep each sample's scenario object.

- verbose:

  logical.

## Value

a `by_sample` object: `runs` (a tibble of what ran), `scenarios`,
`samples` and `method`.

## Details

Each sample is annualised (sub-annual slices carry
`pTimesliceWeight = 1/year_fraction`), so every sample estimates the
**whole year**: the runs are replicates and their spread is the sampling
uncertainty. They are not pieces of a year and must not be summed —
unlike
[`solve_by_region()`](https://energyRt.org/reference/solve_by_region.md),
whose disjoint samples do add up.

A sample must keep at least two members at every non-ANNUAL level of the
calendar. A one-season (or one-week, or one-day) window collapses its
outer level and is refused, because the only way to build it would
rename the leaf timeslices and silently detach the model's data.

## See also

[`calendar_samples()`](https://energyRt.org/reference/calendar_samples.md),
[`sample_summary()`](https://energyRt.org/reference/sample_summary.md),
[`solve_by_region()`](https://energyRt.org/reference/solve_by_region.md)

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

# two random two-season samples of the calendar, each solved as a full year
res <- solve_by_sample(mod, name = "BY_SAMPLE", level = "SEASON",
                       sample_size = 2, n = 2, method = "random", seed = 1,
                       solver = solver_options$glpk)

# the spread across samples is the estimate's uncertainty, not a partition
sample_summary(res, "vTechCap", by = "tech")
} # }
```
