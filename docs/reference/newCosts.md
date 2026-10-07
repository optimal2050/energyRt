# Create new costs object

Costs object is used to define additional costs to add to the model's
objective function.

## Usage

``` r
newCosts(name, variable, desc = "", mult = NULL, subset = NULL, misc = NULL)
```

## Arguments

- name:

  `get_slot_doc("costs", "name")`

- variable:

  `get_slot_doc("costs", "variable")`

- desc:

  `get_slot_doc("costs", "desc")`

- mult:

  data.frame. Named list or data frame with numeric values for the
  variable included in the costs-constraint. A constant or a data frame
  with the same dimensions as the subseted variable.

- subset:

  data.frame. Named list or data frame with set-values for each
  dimension of the variable. This slot subsets the variable to the
  specified set values.

- misc:

  list. Additional information.

## Value

costs object with given specifications.

## See also

Other class constraint policy:
[`class-constraint`](https://energyRt.org/reference/class-constraint.md),
[`class-costs`](https://energyRt.org/reference/class-costs.md),
[`newConstraint()`](https://energyRt.org/reference/newConstraint.md),
[`newSubsidy()`](https://energyRt.org/reference/newSubsidy.md),
[`newTax()`](https://energyRt.org/reference/newTax.md),
[`subsidy-class`](https://energyRt.org/reference/class-subsidy.md),
[`tax-class`](https://energyRt.org/reference/class-tax.md)
