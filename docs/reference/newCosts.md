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

  `get_slot_doc("costs", "mult")`

- subset:

  `get_slot_doc("costs", "subset")`

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
