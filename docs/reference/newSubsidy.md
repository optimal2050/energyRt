# Create a new subsidy object

Subsidies are used to represent the financial support provided to
production, consumption, or balance of a commodity.

## Usage

``` r
newSub(
  name,
  desc = "",
  comm = "",
  region = character(),
  defVal = 0,
  sub = data.frame(),
  misc = list(),
  ...
)
```

## Arguments

- name:

  character. Name of the subsidy object, used in sets.

- desc:

  character. Description of the subsidy object.

- comm:

  character. Name of the subsidized commodity.

- region:

  character. Region where the subsidy is applied.

- defVal:

  numeric. Default value of the subsidy.

- sub:

  data.frame. Subsidy values.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  slice

  :   character. Time slice to apply the parameter, NA for every slice.

  inp

  :   numeric. Input subsidy, e.g., per unit of commodity consumed by
      all processes.

  out

  :   numeric. Output subsidy, e.g., per unit of commodity produced by
      all processes.

  bal

  :   numeric. Balance subsidy, e.g., per unit of commodity balance
      (production - consumption).

- misc:

  list. Any additional information or data to store in the subsidy
  object.

## Value

An object of class `sub`

## See also

Other class constraint policy:
[`class-constraint`](https://energyRt.org/reference/class-constraint.md),
[`class-costs`](https://energyRt.org/reference/class-costs.md),
[`newConstraint()`](https://energyRt.org/reference/newConstraint.md),
[`newCosts()`](https://energyRt.org/reference/newCosts.md),
[`newTax()`](https://energyRt.org/reference/newTax.md),
[`subsidy-class`](https://energyRt.org/reference/class-subsidy.md),
[`tax-class`](https://energyRt.org/reference/class-tax.md)

## Examples

``` r
SUB_BIO <- newSub(
 name = "SUB_BIO", # used in sets
 desc = "Biofuel consumption subsidy", # for own reference
 comm = "BIO", # must match the commodity name in the model
 region = "R1", # region where the subsidy is applied
 defVal = 0, # default value
 sub = data.frame(
    # region = "R1",
    year = 2025:2030,
    inp = 0.9 # subsidy rate
   )
 )
```
