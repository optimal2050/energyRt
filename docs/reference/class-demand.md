# An S4 class to declare a demand in the model

An S4 class to declare a demand in the model

## Slots

- `name`:

  character. Name of the demand.

- `desc`:

  character. Optional description of the demand for reference.

- `commodity`:

  character. Name of the commodity for which the demand will be
  specified.

- `units`:

  data.frame. Units this object is measured in, for reference and for
  carrying them with it: an object moved between models is only
  meaningful where the units agree, and nothing else in the object
  records them. Up to version 0.91 the object's own measure was a slot
  of its own, `@unit`.

  comm

  : character. The commodity this object is counted in, e.g. "GWh" or
    "PJ". Set by the `unit =` argument of the constructor. Leave it
    empty to claim nothing: a unit is reported and cross-checked, never
    converted.

  currency

  : character. Currency of the cost columns. A currency that disagrees
    with the model's own is an error rather than a silent conversion.

- `demand`:

  data.frame. Specification of the demand.

  region

  : character. Name of region for the demand value. NA for every region.

  year

  : integer. Year of the demand. NA for every year.

  timeslice

  : character. Name of the timeslice for the demand value. NA for every
    timeslice.

  demand

  : numeric. Value of the demand.

  cluster

  : character. Cluster label of the row (see `@cluster`), NA for the
    whole demand.

- `cluster`:

  data.frame. Declaration of the demand's parallel parts. REQUIRED
  whenever `demand` carries a `cluster` label, and authoritative: labels
  are never harvested from the data slot. For demand a cluster is a
  SUB-REGIONAL SHARE: the object carries one total, declared at a coarse
  region, and each cluster takes a fixed fraction of it in one finer
  region. Variant expansion mints one `dem` set member per cluster and
  `pDemand` is indexed by `dem`, so `eqDemInp` sums the parts back – the
  coarse figure is the AGGREGATE of its children and stays the number
  you edit. LIMITATION – only `dem.share.fx`, deliberately. A `lo`/`up`
  split would need the LP to CHOOSE where load sits inside the region,
  and nothing indexed by `dem` is a variable for such a bound to act on:
  `pDemand` sits on the right-hand side of an equality. There is no
  `cap.share.fx` either (a demand has no capacity) and no `act.share.*`
  (same reason – no variable per demand object). Shares are fractions of
  THIS object's total and must sum to 1, so one demand object is one
  coarse region. Several zones are several demand objects, the same way
  one [`lossTranches()`](https://energyRt.org/reference/lossTranches.md)
  object is one line. Note the commodity's own `@geoframe` is a
  different question: a commodity balanced at the coarse level cannot
  read region-level demand at all, and declaring both is refused. Split
  the demand when the commodity stays at the finer level.

  cluster

  : character. Cluster label, referenced by the `cluster` column of
    `@demand`.

  desc

  : character. Optional description of the share.

  region

  : character. The finer region this share lands in. Required – it is
    where the share of the demand goes.

  dem.share.fx

  : numeric. This cluster's fixed fraction of the object's total demand.
    Must be positive, and the shares of one object must sum to 1.

  order

  : integer. Optional ordering for reporting. Without it labels sort
    alphabetically and "R10" would precede "R2".

- `geoframe`:

  character. Optional. The geoscale level this object's regions are
  expressed at; unset (the default) derives it from the region codes.
  See `technology@geoframe` for the full rule, including the consistency
  checks it turns on. A demand CONSUMES, so it may sit at a coarser
  level than the commodity it takes.

- `region`:

  character. Optional name of region to narrow the specification of the
  demand in the case of used NAs. Error will be returned if specified
  regions in `@demand` are not mensioned in the `@region` slot (if the
  slot is not empty).

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

  list. Optional list of additional information.
