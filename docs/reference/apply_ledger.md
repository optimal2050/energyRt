# Apply a carry ledger to a model

The application half of the sequential-solving seam: returns a COPY of
the (pristine) model whose technology / storage / trade objects carry
the ledger's decided past as dense exogenous `stock` schedules over the
target horizon's milestones, and whose supply objects have their
cumulative reserves debited by past use. Carried capacity stands while
`year - vintage < olife` (mirroring the model's own capacity equation)
and enters as `stock` — investment-cost-free, i.e. sunk.

## Usage

``` r
apply_ledger(
  mod,
  ledger,
  horizon,
  carry = c("capacity", "budgets"),
  tolerance = 1e-06,
  verbose = FALSE
)
```

## Arguments

- mod:

  the pristine model (never a previously-applied copy: each application
  applies the FULL ledger from scratch).

- ledger:

  a `carry_ledger` from
  [`solution_ledger()`](https://energyRt.org/reference/solution_ledger.md).

- horizon:

  the horizon of the NEXT step (its milestone `mid` years receive the
  dense stock rows).

- carry:

  character, what to carry: `"capacity"` (with its retirements) and/or
  `"budgets"` (supply reserves debited).

- tolerance:

  numeric, carried values below it are written as zero.

- verbose:

  logical.

## Value

a model object.

## See also

[`solution_ledger()`](https://energyRt.org/reference/solution_ledger.md),
[`solve_myopic()`](https://energyRt.org/reference/solve_myopic.md)
