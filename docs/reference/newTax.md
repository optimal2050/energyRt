# Create a new tax object

Taxes are used to represent the financial levy imposed on production,
consumption, or balance of a commodity.

## Usage

``` r
newTax(
  name,
  desc = "",
  comm = "",
  region = character(),
  defVal = 0,
  tax = data.frame(),
  misc = list(),
  ...
)
```

## Arguments

- name:

  character. Name of the tax object, used in sets.

- desc:

  character. Description of the tax object.

- comm:

  character. Name of the taxed commodity.

- region:

  character. Region where the tax is applied.

- defVal:

  numeric. Default value of the tax for not specified sets, 0 if not
  specified.

- tax:

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

- misc:

  list. Additional information.

## Value

An object of class `tax`

## See also

Other class constraint policy:
[`class-constraint`](https://energyRt.org/reference/class-constraint.md),
[`class-costs`](https://energyRt.org/reference/class-costs.md),
[`newConstraint()`](https://energyRt.org/reference/newConstraint.md),
[`newCosts()`](https://energyRt.org/reference/newCosts.md),
[`newSubsidy()`](https://energyRt.org/reference/newSubsidy.md),
[`subsidy-class`](https://energyRt.org/reference/class-subsidy.md),
[`tax-class`](https://energyRt.org/reference/class-tax.md)

## Examples

``` r
CO2TAX <- newTax(
 name = "CO2TAX",
 desc = "Tax on net CO2 emissions",
 comm = "CO2",
 region = "R1",
 defVal = 0,
 tax = data.frame(
 # region = "R1", # not required when @region is set
 year = c(2030, 2040, 2050),
 bal =  c(10, 50, 200) # $10, $50, $200 per ton, will be interpolated
 # out = ... use to tax output commodity
 # inp = ... use to tax input commodity
   ),
 misc = list(
  source = "https://www.example.com/tax"
  )
 )
```
