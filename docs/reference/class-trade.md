# An S4 class to represent inter-regional trade

An S4 class to represent inter-regional trade

## Details

Trade objects are used to represent inter-regional exchange in the
model. Without trade, every region is isolated and can only use its own
resources. The class defines trade routes, efficiency, costs, and other
parameters related to the process. Number of routes per trade object is
not limited. One trade object can have a part or entire trade network of
the model. However, it has a distinct name and all the routs will be
optimized together. Create separate trade objects to optimize different
parts of the trade network (aka transmission lines).

## Slots

- `name`:

  character. Name of the trade object, used in sets.

- `desc`:

  character. Description of the trade object.

- `commodity`:

  character. The traded commodity short name.

- `routes`:

  data.frame. Source and destination regions. For bivariate trade define
  both directions in separate rows.

  from

  :   character. Source region.

  to

  :   character. Destination region.

- `trade`:

  data.frame. Technical parameters of trade per directed route. Two
  kinds of flow bound live here and they are not interchangeable:
  `ava.*` is ABSOLUTE, in physical commodity units per timeslice, and
  does not scale with capacity; `af.*` is RELATIVE, a fraction of the
  object's own capacity (`cap2act` x `vTradeCap` x the timeslice share),
  and is the direct analogue of `technology@af`. Use `af.*` to rate
  individual lines of a multi-route object, where the absolute bound
  would have to be a number correct for exactly one capacity – and
  capacity is the decision.

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for every vintage. See the `vintage` slot.

  cluster

  :   character. Cluster (loss tranche) label selecting the tranche this
      row applies to, NA for every tranche. See the `cluster` slot.

  src

  :   character. Source region of the flow, NA for every source region
      on the route.

  dst

  :   character. Destination region of the flow, NA for every
      destination region on the route.

  year

  :   integer. Year to apply the parameter, NA for every year.

  timeslice

  :   character. Time timeslice to apply the parameter, NA for every
      timeslice.

  ava.lo

  :   numeric. Lower bound on the traded flow from `src` to `dst`, in
      physical commodity units per timeslice.

  ava.up

  :   numeric. Upper bound on the traded flow from `src` to `dst`, in
      physical commodity units per timeslice.

  ava.fx

  :   numeric. Fixed value of the traded flow from `src` to `dst`, in
      physical commodity units per timeslice. This parameter overrides
      `ava.lo` and `ava.up`.

  af.lo

  :   numeric. Lower bound on the flow from `src` to `dst` as a FRACTION
      of the object's capacity: `af.lo x cap2act x vTradeCap x share`.
      Forces a minimum loading on that direction.

  af.up

  :   numeric. Upper bound on the flow from `src` to `dst` as a FRACTION
      of the object's capacity: `af.up x cap2act x vTradeCap x share`.
      This is how a single trade object carrying several routes gives
      each line its own rating – `af.up = 0.5` on one leg limits it to
      half the corridor whatever the corridor turns out to be. Note that
      `af.up` of 1 is already implied by `eqTradeCapFlow` and cannot
      bind.

  af.fx

  :   numeric. Fixed loading of the direction `src` -\> `dst` as a
      fraction of capacity. Overrides `af.lo` and `af.up`.

  teff

  :   numeric. Trade efficiency: the fraction of the flow sent from
      `src` that is delivered to `dst` (1 = lossless).

  reactance

  :   numeric. Series reactance `x` of the line, in the model's own
      impedance units. Carried for AC lines and used by nothing unless
      the model is interpolated with `kvl = TRUE`, when it becomes the
      coefficient of Kirchhoff's voltage law. Presence of a finite
      `reactance` is what marks a route as a PASSIVE AC branch: a
      controllable DC link has none, because its flow is chosen rather
      than set by impedance. Must be SYMMETRIC across the two directed
      rows of a line – the voltage law constrains the net flow – and a
      line under KVL must be declared in both directions. Where several
      physical circuits have been merged into one route, the value is
      the EQUIVALENT reactance, `1/x_eq = sum(1/x_i)`, not any one
      circuit's.

  resistance

  :   numeric. Series resistance `r` of the line, same units and same
      symmetry requirement as `reactance`. Recorded for round-tripping
      with power-system models (PyPSA's `Line` carries both). It does
      not drive losses by itself – losses are the per-route `teff` – but
      it is the input
      [`lossTranches()`](https://energyRt.org/reference/lossTranches.md)
      turns into a piecewise-linear loss curve: pass it with the rating
      it was measured against, since `loss_full = r * F` and a
      resistance alone does not determine a loss fraction.

- `aux`:

  data.frame. Auxiliary commodity of trade.

  acomm

  :   character. Name of the auxiliary commodity (used in sets).

  unit

  :   character. Unit of the auxiliary commodity.

- `aeff`:

  data.frame. Auxiliary commodity efficiency parameters.

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for every vintage. See the `vintage` slot.

  cluster

  :   character. Cluster (loss tranche) label selecting the tranche this
      row applies to, NA for every tranche. See the `cluster` slot.

  acomm

  :   character. Name of the auxiliary commodity (used in sets).

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  timeslice

  :   character. Time timeslice to apply the parameter, NA for every
      timeslice.

  trade2ainp

  :   numeric. Trade-to-auxiliary-input-commodity coefficient
      (multiplier).

  trade2aout

  :   numeric. Trade-to-auxiliary-output-commodity coefficient
      (multiplier).

- `invcost`:

  data.frame. Investment cost of the trade capacity, as a RATE PER
  ENDPOINT REGION per unit of capacity. Trade capacity is region-free –
  one number per corridor – but its costs are region-indexed, and each
  region named in `region` pays its own rate on the whole capacity. A
  corridor whose two endpoints each pay 100 therefore costs 200 in
  total. Leaving `region` unset applies the rate at every endpoint of
  the route; energyRt reports that rather than correcting it, because
  the same rate is what a partial-region study would see – solve a
  subset of the endpoints and it bears only their share, which is the
  intended behaviour. Name the regions to vary the rate between them.
  `fixom`, `retcost` and `eac` follow the same convention.

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for every vintage. See the `vintage` slot.

  cluster

  :   character. Cluster (loss tranche) label selecting the tranche this
      row applies to, NA for every tranche. See the `cluster` slot.

  region

  :   character. Endpoint region bearing this rate. NA applies the rate
      at every endpoint of the route; name the regions to vary it
      between them. Must be one of the model's own regions – a coarser
      geoscale level is accepted by validation but never reaches the
      objective.

  year

  :   integer. Year to apply the parameter, NA for every year.

  invcost

  :   numeric. Investment cost.

  wacc

  :   numeric. Weighted average cost of capital used to annuitise
      `invcost` for this corridor. Overrides the model-wide `wacc` (see
      the model `discount` argument). The social discount rate is never
      used here.

  payback

  :   numeric. Cost-recovery period in years. Where given it replaces
      `olife` in the annuity AND in the years over which the annuity is
      charged, so the investment is repaid over `payback` years while
      the capacity keeps operating for its full operational life. Must
      be positive and not exceed `olife`. Unset (or 0) means recover
      over `olife`. Implemented for the GLPK solver only.

  eac

  :   numeric. Equivalent annual cost, supplied directly instead of
      being computed from `invcost`, `wacc` and the lifetime. Where
      given it wins; where absent the annuity is computed. Mutually
      exclusive with `invcost` per row.

  retcost

  :   numeric. Costs of early retirement of the trade capacity, default
      is 0.

- `fixom`:

  data.frame. Fixed operation and maintenance costs, per unit of
  standing capacity per year. Like the other trade cost slots this is a
  RATE PER ENDPOINT REGION, named in its `region` column – see the note
  on `invcost`.

- `varom`:

  data.frame. Costs charged on the traded flow, PER ROUTE. Unlike the
  capacity costs above – which are a rate per endpoint region – these
  are indexed by `(src, dst, year, timeslice)`, so they can differ by
  direction. The two columns mean different things:

  `varom` – variable operation and maintenance, a REAL resource cost.
  Charged where the flow ORIGINATES (`src`) and added to total system
  cost, exactly as `technology@varom` is. Use it for wheeling fees,
  transit taxes or any real charge per unit shipped. Becomes
  `pTradeIrCost`.

  `markup` – a border price or bilateral charge: a TRANSFER. The
  importing region pays it and the exporting region receives it, so it
  moves cost between regions and leaves the objective unchanged – the
  same distinction as a tax versus a cap. Becomes `pTradeIrMarkup`.

  Before 0.85 both columns were summed into a single term that cancelled
  between `eqImportIrCost` and `eqExportIrCost`, so `varom` could not
  express an operating cost at all; it now enters the objective once,
  positively, at `src`. (A separate parameter `pTradeVarom` was once
  declared in the GLPK and GAMS templates and read by no equation; it
  has been removed and was never this slot.)

- `capacity`:

  data.frame. Capacity parameters of the trade object: `stock` (the
  legacy corridor still standing at each milestone), the `cap.*` bounds
  on total capacity, the `ncap.*` bounds on the build RATE and the
  `ret.*` bounds on the early retirement rate. Region-free: a route IS a
  pair of regions, so a per-region capacity would be ambiguous.

- `vintage`:

  data.frame. Investment window and operational life of the trade
  object, one row per vintage. Replaces the former `start`, `end` and
  `olife` slots. A vintage is a separately investable variant that keeps
  the characteristics of its build year for its whole life, so several
  vintages of one corridor mean several capacities on the same
  `(src, dst)` route, with their flows summed in the commodity balance.
  `region` is present for a uniform shape across process classes but is
  unused for trade: its scope comes from the route endpoints. `cluster`
  selects a loss tranche declared in the `cluster` slot.

  vintage

  :   character. Vintage label, normally the build year as a string. NA
      for an un-vintaged trade.

  region

  :   character. Unused for trade (no `region` slot); present for
      consistency with the other classes.

  cluster

  :   character. Cluster (loss tranche) this row applies to, NA for
      every tranche. See the `cluster` slot.

  start

  :   integer. The first year the trade object is available for
      investment. NA means unbounded (up to `end`).

  end

  :   integer. The last year the trade object is available for
      investment. NA means unbounded (from `start` on).

  olife

  :   integer. Operational life of the trade object in years.

- `cap2act`:

  numeric. Capacity to activity ratio.

- `optimizeRetirement`:

  logical. Incidates if the retirement of the trade object should be
  optimized. Also requires the same parameter in the `model` or
  `scenario` class to be set to TRUE to be effective.

- `misc`:

  list. Additional information.
