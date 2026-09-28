# An S4 class to represent commodity import from the rest of the world.

Use `newImport` to create a new `import` object.

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

## Slots

- `name`:

  character. Name of the import object, used in sets.

- `desc`:

  character. Description of the import object.

- `commodity`:

  character. Name of the imported commodity.

- `unit`:

  character. Unit of the imported commodity.

- `reserve`:

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

- `cluster`:

  data.frame. Declaration of the object's parallel sub-objects. For
  import a cluster is a PRICE STEP of a stepped curve: its own share of
  the quantity, at its own price. A single price cannot express a curve
  – real resource grades get dearer as they are exhausted, and a real
  export market pays less as more volume is pushed into it. This slot
  declares WHAT the steps are; the per-step values live in the `cluster`
  column of the other slots. Build both with
  [`asImportCurve()`](https://energyRt.org/reference/supply-curve.md),
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
      [`asImportCurve()`](https://energyRt.org/reference/supply-curve.md)
      writes the split straight into the quantity bounds and records
      here what it did.

  order

  :   integer. Fill order, 1 = the cheapest step (or, for export, the
      best paid). Without it labels sort alphabetically and "S10" would
      precede "S2".

- `import`:

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

- `region`:

  character. Regions where the import process exists. Empty (or NA)
  means every region of the model. A region named in any other slot must
  be one of these; the other slots place VALUES and never change where
  the process exists.

- `misc`:

  list. Additional information.
