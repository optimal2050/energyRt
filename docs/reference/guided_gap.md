# Cost of the guidance

The relative gap between a guided solve's objective and a true
perfect-foresight one. Because the guided result's final stage is a
single full-horizon solve with extra bounds, the two objectives are
directly comparable and the gap is non-negative by construction: bounds
can only raise the optimum. A gap of zero means the guidance never
bound.

## Usage

``` r
guided_gap(x, pf)
```

## Arguments

- x:

  a `guided` result with a `final` scenario.

- pf:

  a solved perfect-foresight scenario of the same model, or its
  objective as a number.

## Value

a list with `guided`, `pf` and `gap` (the relative difference).

## See also

[`solve_guided()`](https://energyRt.org/reference/solve_guided.md)

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

# what the guidance cost, against a true perfect-foresight run
pf  <- solve_model(mod, name = "PF", solver = solver_options$glpk)
res <- solve_guided(mod, name = "GUIDED", solver = solver_options$glpk)

guided_gap(res, pf)        # 0 means the bounds never bound
guided_gap(res, sum(getData(pf, "vObjective", merge = TRUE)$value))
} # }
```
