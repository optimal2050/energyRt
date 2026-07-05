# Create new demand object

Create new demand object

Update data in a demand object

## Usage

``` r
newDemand(
  name = "",
  desc = character(),
  commodity = character(),
  unit = character(),
  dem = data.frame(),
  region = character(),
  misc = list(),
  ...
)

# S4 method for class 'demand'
update(object, ...)
```

## Arguments

- name:

  character. Name of the demand.

- desc:

  character. Optional description of the demand for reference.

- commodity:

  character. Name of the commodity for which the demand will be
  specified.

- unit:

  character. Optional unit of the commodity.

- dem:

  data.frame. Specification of the demand.

  region

  :   character. Name of region for the demand value. NA for every
      region.

  year

  :   integer. Year of the demand. NA for every year.

  slice

  :   character. Name of the slice for the demand value. NA for every
      slice.

  dem

  :   numeric. Value of the demand.

- region:

  character. Optional name of region to narrow the specification of the
  demand in the case of used NAs. Error will be returned if specified
  regions in `@dem` are not mensioned in the `@region` slot (if the slot
  is not empty).

- misc:

  list. Optional list of additional information.

- object:

  demand object

## Value

demand object with given specifications.

## Examples

``` r
DSTEEL <- newDemand(
 name = "DSTEEL",
 desc = "Steel demand",
 commodity = "STEEL",
 unit = "Mt",
 dem = data.frame(
    region = "UTOPIA", # NA for every region
    year = c(2020, 2030, 2050),
    slice = "ANNUAL",
    dem = c(100, 200, 300)
 ),
 region = "UTOPIA", # optional, to narrow the specification of the demand
 )
 class(DSTEEL)
#> [1] "demand"
#> attr(,"package")
#> [1] "energyRt"
 draw(DSTEEL)

```
