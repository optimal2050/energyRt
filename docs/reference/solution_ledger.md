# Extract the carry-forward ledger from a solved scenario

The extraction half of the sequential-solving seam: reads WHAT a solved
scenario decided in its (decided) milestone years — new capacity by
vintage (with the solving horizon's period length and the process
lifetime frozen in), retirements of that capacity, endogenous stock
retirements, the terminal exogenous-stock level, and cumulative supply
use — into a `carry_ledger`. Pure read: nothing is modified. Ledgers of
consecutive steps accumulate;
[`apply_ledger()`](https://energyRt.org/reference/apply_ledger.md) turns
the accumulated ledger into the next step's model.

## Usage

``` r
solution_ledger(object, years = NULL, ledger = NULL)
```

## Arguments

- object:

  a solved scenario.

- years:

  integer, the DECIDED milestone years to extract (default: all
  milestones of the scenario's horizon). With look-ahead windows,
  restrict to the decided years so look-ahead decisions are discarded.

- ledger:

  an existing `carry_ledger` to append to (default: empty).

## Value

a `carry_ledger` object.

## See also

[`apply_ledger()`](https://energyRt.org/reference/apply_ledger.md),
[`solve_myopic()`](https://energyRt.org/reference/solve_myopic.md)

## Examples

``` r
if (FALSE) { # \dontrun{
open_project(tempdir())

ek   <- topia$modules$electricity$R3
mod  <- newModel("TOPIA", data = ek$repo,
                 calendar = topia$modules$calendars$s4_h24,
                 region = ek$regions,
                 horizon = topia$modules$horizons$base,
                 discount = 0.05)
mod <- setGeoscale(mod, ek$geoscale)
scen <- solve_model(mod, name = "BASE", solver = solver_options$glpk)

# what the step decided, as the input to the next one
led <- solution_ledger(scen)
names(led)

# the accumulated ledger is what `solve_myopic()` writes into the next
# step's model
led$steps
} # }
```
