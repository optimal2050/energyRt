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
  region = character(),
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

- cluster:

  data.frame. Declaration of the object's parallel sub-objects. For
  export a cluster is a PRICE STEP of a stepped curve: its own share of
  the quantity, at its own price. A single price cannot express a curve
  – real resource grades get dearer as they are exhausted, and a real
  export market pays less as more volume is pushed into it. This slot
  declares WHAT the steps are; the per-step values live in the `cluster`
  column of the other slots. Build both with
  [`asExportCurve()`](https://energyRt.org/reference/supply-curve.md),
  which also computes the prices and checks the direction of the curve.
  Optional. When empty, step labels are harvested from the `cluster`
  columns of the other slots. When populated it is AUTHORITATIVE.
  LIMITATION – there is deliberately no `cap.share.fx` column. A
  capacity share ties the variants' capacities to fixed proportions, and
  this class has no capacity variable for the tie to act on. The column
  is absent rather than present-and-ignored, so the mistake is rejected
  at construction instead of silently doing nothing. Bound the split
  with `act.share.lo/up/fx`, which acts on the quantity this class does
  have, or fix it directly in the per-step bounds. The share columns are
  different levers, not substitutes. `cap.share.fx` fixes the ratio of
  the variants' CAPACITIES in every year: loss tranches need it, because
  each tranche efficiency is calibrated to its place in the capacity
  stack and the flow must stay free to fill the cheapest first.
  `act.share.lo/up/fx` bounds a variant share of the family THROUGHPUT
  instead, which is what stops a clustered family collapsing to its
  cheapest member once regional borders are aggregated away. Both are
  optional; a variant with neither sizes and runs freely.

  cluster

  :   character. Step label, used in the expanded object name.

  desc

  :   character. Description of the step.

  share

  :   numeric. The step's fraction of the object's quantity.
      DESCRIPTIVE, not enforced: unlike `technology` or `trade` this
      class has no capacity variable to tie, so
      [`asExportCurve()`](https://energyRt.org/reference/supply-curve.md)
      writes the split straight into the quantity bounds and records
      here what it did.

  order

  :   integer. Fill order, 1 = the cheapest step (or, for export, the
      best paid). Without it labels sort alphabetically and "S10" would
      precede "S2".

- region:

  character. Regions where the export process exists. Empty (or NA)
  means every region of the model. A region named in any other slot must
  be one of these; the other slots place VALUES and never change where
  the process exists.

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
