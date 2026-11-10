# An S4 class to represent a supply of a commodity

An S4 class to represent a supply of a commodity

## Value

supply object with given specifications.

## Slots

- `name`:

  character. Name of the supply object, used in sets.

- `desc`:

  character. Description of the supply object.

- `units`:

  data.frame. Key units of the object, for reference and for carrying
  its units with it. An object moved between models is only meaningful
  where the units agree, and nothing else in the object records them:
  the numbers in `cost`/`invcost` and the capacity and activity columns
  are bare. A declared `currency` that disagrees with the model's own is
  an error rather than a silent conversion; leave a column empty to
  claim nothing.

  comm

  : character. The commodity this object is counted in, e.g. "GWh" or
    "PJ". Set by the `unit =` argument of the constructor. Leave it
    empty to claim nothing: a unit is reported and cross-checked, never
    converted.

  activity

  : character. Unit of the activity variable.

  currency

  : character. Currency of the cost columns.

- `commodity`:

  character. The supplied commodity short name.

- `weather`:

  data.frame. Weather factors to apply to the supply.

  cluster

  : character. Price step this row applies to, NA for every step. See
    the `cluster` slot.

  weather

  : character. Name of the weather factor to apply. Must match the
    weather factor names in a `weather` class in the model.

  unit

  : character. Optional. The unit this process EXPECTS from the series
    named in `weather`, e.g. "degC". Per row, because one process may
    read a temperature from one series and a wind speed from another.
    Where this and the weather object's own `@units$wval` are both
    declared and differ, that is an error: nothing in the model converts
    between weather units, so a coefficient fitted to one measure and
    applied to another is wrong in a way no output reveals. Empty on
    either side claims nothing and is not checked.

  transform

  : character. Optional name of a function in the weather object's
    `@misc$transform`: the factor reads the series that function derives
    from the raw one, materialised at interpolation. NA reads the raw
    series.

  wava.lo

  : numeric. Coefficient that links the weather factor with the lower
    bound of the availability factor `ava.lo`.

  wava.up

  : numeric. Coefficient that links the weather factor with the upper
    bound of the availability factor `ava.up`.

  wava.fx

  : numeric. Coefficient that links the weather factor with the fixed
    value of the availability factor `ava.fx`. This parameter overrides
    `wava.lo` and `wava.up`.

- `reserve`:

  data.frame. Total available resource. Applicable to exhaustible
  resources. Set for each region. If not set, the resource is considered
  infinite.

  cluster

  : character. Price step this row applies to, NA for every step. See
    the `cluster` slot.

  region

  : character. Region name to apply the parameter. Use NA to apply to
    all regions.

  res.lo

  : numeric. Lower bound of the total available resource.

  res.up

  : numeric. Upper bound of the total available resource.

  res.fx

  : numeric. Fixed value of the total available resource. This parameter
    overrides `res.lo` and `res.up`.

- `cluster`:

  data.frame. Declaration of the object's parallel sub-supplies. For
  supply a cluster is a PRICE STEP – a resource grade: its own share of
  the availability and reserve, at its own cost. A single `cost` cannot
  express a supply curve, where cheap grades are exhausted first and the
  next ones cost more. This slot declares WHAT the grades are; the
  per-grade values live in the `cluster` column of `supply`, `reserve`
  and `weather`. Build both with
  [`asSupplyCurve()`](https://energyRt.org/reference/supply-curve.md),
  which also computes the costs and checks that the curve rises.
  REQUIRED whenever any other slot carries a `cluster` label, and
  AUTHORITATIVE: a label used in a slot but not declared here is a typo
  and an error. Labels are never harvested from the data slots – a
  misspelling would otherwise become a variant with defaults for
  everything it does not carry (no cost, no bound). LIMITATION – there
  is deliberately no `cap.share.fx` column. A capacity share ties the
  variants' capacities to fixed proportions, and this class has no
  capacity variable for the tie to act on. The column is absent rather
  than present-and-ignored, so the mistake is rejected at construction
  instead of silently doing nothing. Bound the split with
  `act.share.lo/up/fx`, which acts on the quantity this class does have,
  or fix it directly in the per-step bounds. The share columns are
  different levers, not substitutes. `cap.share.fx` fixes the ratio of
  the variants' CAPACITIES in every year: loss tranches need it, because
  each tranche efficiency is calibrated to its place in the capacity
  stack and the flow must stay free to fill the cheapest first.
  `act.share.lo/up/fx` bounds a variant share of the family THROUGHPUT
  instead, which is what stops a clustered family collapsing to its
  cheapest member once regional borders are aggregated away. Both are
  optional; a variant with neither sizes and runs freely.

  cluster

  : character. Grade label, used in the expanded object name.

  desc

  : character. Description of the grade.

  region

  : character. Region(s) the grade exists in, NA for everywhere.
    Intersected with the object's own `region` slot.

  act.share.lo

  : numeric. Lower bound on the share of the family's throughput this
    variant carries.

  act.share.up

  : numeric. Upper bound on the share of the family's throughput this
    variant carries.

  act.share.fx

  : numeric. Fixed share of the family's throughput this variant
    carries.

  order

  : integer. Fill order, 1 = the cheapest grade. Without it labels sort
    alphabetically and "S10" would precede "S2".

- `supply`:

  data.frame. Availability of the resource in physical units. Unlike the
  `af`/`afs` availability *factors* of technologies, `ava.*` is an
  absolute bound on the supplied quantity per timeslice: supply
  processes have no capacity variable, so nothing multiplies it (except
  weather factors, see `weather`). Rows here also define where and when
  the supply exists in the model.

  cluster

  : character. Price step this row applies to, NA for every step. See
    the `cluster` slot.

  region

  : character. Region name to apply the parameter. Use NA to apply to
    all regions.

  year

  : integer. Year to apply the parameter. Use NA to apply to all years.

  timeslice

  : character. Time timeslice to apply the parameter. Use NA to apply to
    all timeslices.

  ava.lo

  : numeric. Lower bound on the supplied quantity, in physical commodity
    units per timeslice.

  ava.up

  : numeric. Upper bound on the supplied quantity, in physical commodity
    units per timeslice.

  ava.fx

  : numeric. Fixed value of the supplied quantity, in physical commodity
    units per timeslice. This parameter overrides `ava.lo` and `ava.up`.

  cost

  : numeric. Cost of the resource extraction, if not set, the resource
    is considered free.

- `geoframe`:

  character. Optional. The geoscale level this object's regions are
  expressed at; unset (the default) derives it from the region codes.
  See `technology@geoframe` for the full rule, including the consistency
  checks it turns on. A supply DELIVERS its commodity, so its level may
  not be coarser than the commodity's balance level.

- `region`:

  character. Regions where the supply process exists. Must include all
  regions used in other slots. `availability` and `reserve` slots also
  limit possible regions.

- `defVal`:

  data.frame. Optional per-object override of a parameter's DEFAULT,
  keyed by the short column name the value is written under –
  `defVal$af.up <- 1` on a technology, `defVal$wval <- 3` on a weather
  series. Empty by default, and one column per override. A parameter
  name also works and is resolved (`pTechAf.up`, or `pTechAf` for both
  bounds). A key that names no column or parameter of this class is an
  error, not a silent no-op. This is the last of three layers: the
  catalogue (`data-raw/parameters.yml`), then `config@defVal`, then
  this; a value equal to what the layer before it says records nothing.

- `interpolation`:

  data.frame. Optional per-object override of a parameter's
  INTERPOLATION RULE, keyed exactly as `defVal` above:
  `interpolation$af.up <- "inter"`. Any accepted spelling of a rule
  works and is normalised, so `bwd.mid.fwd` is `back.inter.forth`. The
  override applies to THIS object's rows only: every other object of the
  class keeps the rule the catalogue or `config` gives it.

- `misc`:

  list. List of additional parameters that are not used in the model but
  can be used for reference or user-defined functions. For example,
  links to the source of the supply data, or other metadata.
