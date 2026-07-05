# Create constraint object to add custom constraints to the model.

The function creates a new constraint object that can be used to add
custom constraints to the model.

## Usage

``` r
newConstraint(
  name,
  desc = "",
  ...,
  eq = "==",
  for.each = NULL,
  rhs = data.frame(),
  defVal = NULL,
  interpolation = "inter",
  replace_zerros = 1e-20
)

isConstraint(object)

newConstraintS(
  name,
  type,
  eq = "==",
  rhs = 0,
  for.sum = list(),
  for.each = list(),
  defVal = 0,
  rule = NULL,
  comm = NULL,
  cout = TRUE,
  cinp = TRUE,
  aout = TRUE,
  ainp = TRUE
)
```

## Arguments

- name:

  character. Name of the constraint object, used in sets.

- desc:

  character. Description of the constraint.

- ...:

  named or unnamed list(s) of left-hand side (LHS) linear terms
  (summands) to define the constraint. Every summand is defined as a
  list with the following elements:

  - `variable` - name of the variable in the summand.

  - `mult` - multiplier for the variable in the summand.

  - `for.sum` - list of sets for which the summand is defined. The
    summands can be passed as named or unnamed lists. They will be added
    to the `lhs` slot of the constraint object as linear terms of
    mulipliers and variables.

- eq:

  Type of the relation ('==' default, '\<=', '\>=')

- for.each:

  list or data.frame with sets that define the dimension of the
  constraint.

- rhs:

  a numeric value, list or data frame with sets and numeric values for
  each constraint. Note: zero values will be replaced with
  `replace_zerros` to avoid dropping them by the interpolation
  algorithms.

- defVal:

  numeric. The default value for the rhs. It is recommended to set the
  default value for the rhs of every constraint to avoid unexpected
  behavior. If not specified, the default value is 0, and the warning is
  issued.

- interpolation:

  character. Interpolation rule for the constraint. Recognized values,
  any combination of "back", "inter", "forth", e.g., "back.inter" or
  "forth.inter", indicating the direction of interpolation. The default
  value is "inter", meaning that the interpolation is done for years
  between the specified values. The "back" and "forth" values induce
  backward and forward interpolation of the `rhs` values, respectively.

- replace_zerros:

  numeric value to replace zero values in `rhs` and `defVal`. Default is
  `1e-20`.

- object:

  any R object

## Value

Object of class `constraint`.

TRUE if the object inherits class `constraint`, FALSE otherwise.

## Details

Custom constraints extend the functionality of the model by adding
user-defined constraints to the optimization problem. If the predefined
constraints are not sufficient to describe the problem, custom
constraints can be used to add linear equality or inequality constraints
to define additional relationships between the variables. In many cases
this can be done without writing constraints in the GAMS, Julia/JuMP,
Python/Pyomo, or GLPK-MathProg languages by using the `constrant` class
and the `newConstraint` function. To define a custom constraint with the
`newConstraint` function, the user needs to specify the name of the
constraint, the type of the relation (equality, less than or equal,
greater than or equal), the left-hand side (LHS) terms of the statement,
and the right-hand side (RHS) value. The dimension of the constraint is
set by the `for.each` parameter. The 'lhs' terms are defined as a list
of linear terms (summands). Each summand consists of a variable, a
multiplier, and a set of sets for which the summand is defined.

## Functions

- `isConstraint()`: Check if an object is a constraint.

## See also

Other class constraint policy:
[`class-constraint`](https://energyRt.org/reference/class-constraint.md),
[`class-costs`](https://energyRt.org/reference/class-costs.md),
[`newCosts()`](https://energyRt.org/reference/newCosts.md),
[`newSubsidy()`](https://energyRt.org/reference/newSubsidy.md),
[`newTax()`](https://energyRt.org/reference/newTax.md),
[`subsidy-class`](https://energyRt.org/reference/class-subsidy.md),
[`tax-class`](https://energyRt.org/reference/class-tax.md)

Other class constraint:
[`class-summand`](https://energyRt.org/reference/class-summand.md)

## Examples

``` r
isConstraint(1)
#> [1] FALSE
isConstraint(newConstraint(""))
#> Warning: It is advisable to define 'defVal' parameter.
#> [1] TRUE
```
