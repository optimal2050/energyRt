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

## Examples

``` r
# a per-unit fee on the activity of two technologies, stated as a rate table
newCosts("ACTFEE", variable = "vTechAct",
         mult = data.frame(tech = c("ECOA", "EGAS"), value = c(2, 3)),
         desc = "Activity fee")
#> An object of class "costs"
#> Slot "name":
#> [1] "ACTFEE"
#> 
#> Slot "desc":
#> [1] "Activity fee"
#> 
#> Slot "variable":
#> [1] "vTechAct"
#> 
#> Slot "subset":
#> data frame with 0 columns and 0 rows
#> 
#> Slot "mult":
#>   tech value
#> 1 ECOA     2
#> 2 EGAS     3
#> 
#> Slot "defVal":
#> [1] 0
#> 
#> Slot "misc":
#> list()
#> 

# one rate for a subset of processes: `mult` scalar, `subset` picks the members
newCosts("CAPFEE", variable = "vTechNewCap", mult = 1.5,
         subset = list(tech = "ECOA"))
#> An object of class "costs"
#> Slot "name":
#> [1] "CAPFEE"
#> 
#> Slot "desc":
#> [1] ""
#> 
#> Slot "variable":
#> [1] "vTechNewCap"
#> 
#> Slot "subset":
#>   tech
#> 1 ECOA
#> 
#> Slot "mult":
#> data frame with 0 columns and 0 rows
#> 
#> Slot "defVal":
#> [1] 1.5
#> 
#> Slot "misc":
#> list()
#> 
```
