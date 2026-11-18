# The objective, weighted and unweighted

Recomputes `eqObjective` from a solved scenario, with and without
`pObjWeight`, so a weighted run can be read beside the net present value
it would have reported unweighted. No second solve: both sums are
decoded from `vTotalCost`, which the solution carries per
`(region, year)` cell.

## Usage

``` r
objective_weights(scen)
```

## Arguments

- scen:

  a solved `scenario`.

## Value

an object of class `objective_weights` (a list with a `print` method):
`objective` as the solver reported it, the recomputed `weighted` and
`unweighted` sums, their `ratio`, and `cells`, one row per
`(region, year)` with its cost, period length, discount factor and
weight.

## Details

`weighted` is the identity the solver optimised,
`sum(vTotalCost * pPeriodLen * pDiscountFactor * pObjWeight)`, and
should equal the reported `vObjective` – a mismatch is a defect, and
[`verify_solution()`](https://energyRt.org/reference/verify_solution.md)
checks it as one. `unweighted` is the same sum with the weight omitted:
the net present value of the system as costed, which is the number to
report when the weights were a device for uneven resolution rather than
a statement about what the costs are worth.

The two differ only in how cells are added up. Every cost in the
solution is in real money either way – the weight lives in the
summation, not in any variable – so nothing else needs reinterpreting.
energyRt exports no duals, so there are no weighted prices to divide
out.

## See also

[`setObjectiveWeights()`](https://energyRt.org/reference/setObjectiveWeights.md)
to declare the weights,
[`verify_solution()`](https://energyRt.org/reference/verify_solution.md)
for the full set of post-solve identities.
