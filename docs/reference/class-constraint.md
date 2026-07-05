# An S4 class to represent a custom constraint.

Class `constraint` is used to define custom constraints in the
optimization problem. **\[experimental\]**

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

## Slots

- `name`:

  character. Name of the constraint object, used in sets.

- `desc`:

  character. Description of the constraint.

- `eq`:

  character. Type of the relation ('==' default, '\<=', '\>=').

- `for.each`:

  list. List with sets for combination of which the constraint is
  created.

- `rhs`:

  data.frame. Named list or data frame with numeric values for each
  constraint. The dimensions of the data frame should match the
  dimensions of the sets in the `for.each` slot.

- `defVal`:

  numeric. The default value for the rhs. It is recommended to set the
  default value for the rhs of every constraint to avoid unexpected
  behavior. If not specified, the default value is 0, and the warning is
  issued.

- `interpolation`:

  character. Interpolation rule for the constraint. Recognized values,
  any combination of "back", "inter", "forth", e.g., "back.inter" or
  "forth.inter", indicating the direction of interpolation. The default
  value is "inter", meaning that the interpolation is done for years
  between the specified values. The "back" and "forth" values induce
  backward and forward interpolation of the `rhs` values, respectively.

- `lhs`:

  list. List of summands for the left-hand-side of the equation. This
  slot is created automatically from all named of unnamed lists passed
  to the `newConstraint` function, except for the named arguments.

- `misc`:

  list. Any additional information or parameters to store in the
  constraint object.

## See also

Other class constraint policy:
[`class-costs`](https://energyRt.org/reference/class-costs.md),
[`newConstraint()`](https://energyRt.org/reference/newConstraint.md),
[`newCosts()`](https://energyRt.org/reference/newCosts.md),
[`newSubsidy()`](https://energyRt.org/reference/newSubsidy.md),
[`newTax()`](https://energyRt.org/reference/newTax.md),
[`subsidy-class`](https://energyRt.org/reference/class-subsidy.md),
[`tax-class`](https://energyRt.org/reference/class-tax.md)
