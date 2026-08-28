# Create new export object

Export object represent commodity export to the Rest of the World (RoW).

## Usage

``` r
newExport(
  name,
  desc = "",
  commodity = "",
  unit = NULL,
  reserve = data.frame(),
  export = data.frame(),
  cluster = data.frame(),
  misc = list(),
  ...
)
```

## Arguments

- name:

  character. Name of the export object, used in sets.

- desc:

  character. Description of the export object.

- commodity:

  character. Name of the exported commodity.

- unit:

  character. Unit of the exported commodity.

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

- export:

  data.frame. Export parameters.

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

  exp.lo

  :   numeric. Export lower bound.

  exp.up

  :   numeric. Export upper bound.

  exp.fx

  :   numeric. Fixed export volume, ignored if NA. This parameter
      overrides `exp.lo` and `exp.up`.

  price

  :   numeric. Price received per unit exported. Export revenue is a
      NEGATIVE cost – the minus sits inside `eqExportRowCost` – so a
      higher price is a better outcome for the objective.

- misc:

  list. Additional information.

## Value

export object with given specifications.

## Details

`export` is a type of process that adds an "external" source to a
commodity to the model. The Rest of the World (RoW) is not modeled
explicitly, `export` and `import` objects define and control the
exchange with the RoW. The operation of the export object is similar to
the `demand` objects, the two different classes are used to distinguish
domestic and external sources of final consumption. The export is
controlled by the `exp` data frame, which specifies bounds and fixed
values for the export of the export flow. The `exp.fx` column is used to
specify fixed values of the export flow, making the export flow
exogenous. The `exp.lo` and `exp.up` columns are used to specify lower
and upper bounds of the export flow, making the export flow endogenous.
The `price` column is used to specify the exogenous price for the export
commodity. The `reserve` slot is used to set limits on the total export
over the model horizon.

## Examples

``` r
EXPOIL <- newExport(
  name = "EXPOIL", # used in sets
  desc = "Oil export from the model to RoW", # for own reference
  commodity = "OIL", # must match the commodity name in the model
  unit = "Mtoe", # for own reference
  export = data.frame(
    region = rep(c("R1", "R2"), each = 2), # export region(s)
    year = rep(c(2020, 2050)), # export years
    price = 500, # export price in MUSD/Mtoe (USD/t),
    exp.up = rep(c(1e3, 1e4), each = 2), # upper bound for export in each year
    exp.lo = rep(c(5e2, 0), each = 2) # lower bound for export in each year
  )
)
draw(EXPOIL)
```
