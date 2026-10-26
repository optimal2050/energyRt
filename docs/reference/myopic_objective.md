# Globally discounted total cost of a myopic sequence

Per-step objectives are discounted to each step's own base year and
therefore not additive. This recomputes one comparable number: the sum
of each decided year's (undiscounted) `vTotalCost`, weighted by its
period length and discounted to the sequence's first milestone at rate
`sdr`. Note the documented under-count relative to perfect foresight:
capacity carried between steps enters as investment-cost-free stock, so
its annuities after its building step are not in `vTotalCost`.

## Usage

``` r
myopic_objective(x, sdr = 0)
```

## Arguments

- x:

  a `myopic` result.

- sdr:

  numeric social discount rate (default 0).

## Value

numeric scalar.

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
res <- solve_myopic(mod, name = "MYOPIC", solver = solver_options$glpk)

# per-step objectives are discounted to their own base years, so they do
# not add up; this is the comparable number
myopic_objective(res)              # undiscounted
myopic_objective(res, sdr = 0.05)  # discounted to the first milestone
} # }
```
