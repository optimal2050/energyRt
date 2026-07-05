# An S4 class to represent a commodity tax

Taxes are used to represent the financial levy imposed on production,
consumption, or balance of a commodity.

## Slots

- `name`:

  character. Name of the tax object, used in sets.

- `desc`:

  character. Description of the tax object.

- `comm`:

  character. Name of the taxed commodity.

- `region`:

  character. Region where the tax is applied.

- `defVal`:

  numeric. Default value of the tax for not specified sets, 0 if not
  specified.

- `tax`:

  data.frame. Tax values.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  slice

  :   character. Time slice to apply the parameter, NA for every slice.

  inp

  :   numeric. Input tax, e.g., per unit of commodity consumed by all
      processes.

  out

  :   numeric. Output tax, e.g., per unit of commodity produced by all
      processes.

  bal

  :   numeric. Balance tax, e.g., per unit of commodity balance
      (production - consumption).

- `misc`:

  list. Additional information.

## See also

Other class constraint policy:
[`class-constraint`](https://energyRt.org/reference/class-constraint.md),
[`class-costs`](https://energyRt.org/reference/class-costs.md),
[`newConstraint()`](https://energyRt.org/reference/newConstraint.md),
[`newCosts()`](https://energyRt.org/reference/newCosts.md),
[`newSubsidy()`](https://energyRt.org/reference/newSubsidy.md),
[`newTax()`](https://energyRt.org/reference/newTax.md),
[`subsidy-class`](https://energyRt.org/reference/class-subsidy.md)
