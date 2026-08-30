# Create new export object

Import object to represent commodity import from the Rest of the World
(RoW).

## Usage

``` r
newImport(
  name,
  desc = "",
  commodity = "",
  unit = NULL,
  reserve = data.frame(),
  import = data.frame(),
  cluster = data.frame(),
  misc = list(),
  ...
)
```

## Arguments

- name:

  character. Name of the import object, used in sets.

- desc:

  character. Description of the import object.

- commodity:

  character. Name of the imported commodity.

- unit:

  character. Unit of the imported commodity.

- reserve:

  data.frame. Cumulative limit over the whole model horizon, summed
  across ALL regions, years and timeslices. A data.frame (rather than
  the bare number it was before 0.85) so it can carry a `cluster` column
  and be split across price steps; without that every step would inherit
  the FULL limit and the model would quietly hold `nsteps` times the
  resource. A plain number is still accepted by the constructor and read
  as `res.up`. There is deliberately no `region` column: adding one
  would turn this into a per-region cap and LOSE the all-region total,
  which is what it means today.

  cluster

  :   character. Price step this row applies to, NA for every step.

  res.lo

  :   numeric. Lower bound on the cumulative volume.

  res.up

  :   numeric. Upper bound on the cumulative volume.

  res.fx

  :   numeric. Fixed cumulative volume. Overrides `res.lo` and `res.up`.

- import:

  data.frame. Import parameters.

  cluster

  :   character. Price step this row applies to, NA for every step. See
      the `cluster` slot.

  region

  :   character. Region name to apply the parameter; use NA to apply to
      all regions.

  year

  :   integer. Year to apply the parameter; use NA to apply to all
      years.

  timeslice

  :   character. Time timeslice to apply the parameter; use NA to apply
      to all timeslices.

  imp.lo

  :   numeric. Lower bound on the import volume.

  imp.up

  :   numeric. Upper bound on the import volume.

  imp.fx

  :   numeric. Fixed import volume, ignored if NA. This parameter
      overrides `imp.lo` and `imp.up`.

  price

  :   numeric. Price paid per unit imported.

- misc:

  list. Additional information.

## Value

import object with given specifications.

## Details

Constructor for import object.

Import object adds an "external" source of commodity to the model. The
RoW is not modeled explicitly as a region, `export` and `import` objects
define and control the exchange with the RoW. The operation is similar
to the `demand` object, but the two ideas distinguishes between internal
and external final consumption. This exchange can be exogenously defined
(`imp.fx`) or optimized by the model within the given limits (`imp.lo`,
`imp.up`). The `price` column is used to define the price of the
imported commodity. "Reserve" sets the total amount that can be imported
over the model horizon.

## Examples

``` r
IMPOIL <- newImport(
  name = "IMPOIL", # used in sets
  desc = "Oil import to the model to the RoW", # for own reference
  commodity = "OIL", # must match the commodity name in the model
  unit = "Mtoe", # for own reference
  import = data.frame(
    region = rep(c("R1", "R2"), each = 2), # import region(s)
    year = rep(c(2020, 2050)), # import years
    price = 600, # import price in MUSD/Mtoe (USD/t),
    imp.up = rep(c(1e4, 1e6), each = 2), # upper bound for import in each year
    imp.lo = rep(c(1e4, 1e5), each = 2) # lower bound for import in each year
  )
)
draw(IMPOIL)

```
