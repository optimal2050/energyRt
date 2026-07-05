# An S4 class to add costs to objective function

**\[experimental\]**

## Slots

- `name`:

  character. Name of the cost object, used in sets.

- `desc`:

  character. Description of the cost object for own references.

- `variable`:

  character. Name of the variable included in the costs-constraint.

- `subset`:

  data.frame. Named list or data frame with set-values for each
  dimension of the variable. This slot subsets the variable to the
  specified set values.

- `mult`:

  data.frame. Named list or data frame with numeric values for the
  variable included in the costs-constraint. A constant or a data frame
  with the same dimensions as the subseted variable.

- `misc`:

  list. Additional information.

## See also

Other class constraint policy:
[`class-constraint`](https://energyRt.org/reference/class-constraint.md),
[`newConstraint()`](https://energyRt.org/reference/newConstraint.md),
[`newCosts()`](https://energyRt.org/reference/newCosts.md),
[`newSubsidy()`](https://energyRt.org/reference/newSubsidy.md),
[`newTax()`](https://energyRt.org/reference/newTax.md),
[`subsidy-class`](https://energyRt.org/reference/class-subsidy.md),
[`tax-class`](https://energyRt.org/reference/class-tax.md)
