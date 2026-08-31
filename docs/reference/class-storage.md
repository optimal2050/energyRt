# An S4 class to represent storage type of technological process.

Storage type of technological processes with accumulating capacity of a
commodity.

## Details

Storage can be used in combination with other processes, such as
technologies, supply, or demand to represent complex technological
chains, demand or supply technologies with time-shift. Operation of
storage includes accumulation, storing, and release of the stored
commodity. The storing cycle operates on the ordered time-timeslices of
the commodity timeframe. The cycle is looped either on an annual basis
(last time-timeslice of a year follows the first time timeslice of the
same year) or within the parent time-frame (for example, when commodity
time-frame is "HOUR" and the parent time-frame is "DAY" then the storage
cycle will be a calendar day).

## Slots

- `name`:

  character. Name of the storage (used in sets).

- `desc`:

  character. Description of the storage.

- `aux`:

  data.frame. Auxiliary commodities.

  acomm

  :   character. Name of the auxiliary commodity (used in sets).

  unit

  :   character. Unit of the auxiliary commodity.

- `region`:

  character. Region where the storage technology exists or can be
  installed.

- `cluster`:

  data.frame. Declaration of the storage clusters – parallel
  sub-processes of the same storage with their own capacity,
  availability and costs. The motivating cases are site-constrained
  storage (pumped-hydro head classes, CAES caverns) and duration classes
  via a per-cluster `duration`. Optional: when empty, cluster labels are
  harvested from the other slots; when populated it is authoritative and
  an undeclared label raises an error.

  cluster

  :   character. Cluster label. Must match the `cluster` column values
      used in the other slots.

  desc

  :   character. Human-readable description of the cluster, used in
      reports.

  region

  :   character. Region the cluster exists in, NA for every region.

  order

  :   integer. Optional display/ranking order; controls the order
      variants are created and reported in.

- `vintage`:

  data.frame. Investment window and operational life of the storage, one
  row per (vintage, region, cluster). Replaces the former `start`, `end`
  and `olife` slots. A vintage is a separately investable variant that
  keeps the characteristics of its build year for its whole life – for
  storage typically a falling capex and a rising round-trip efficiency.

  vintage

  :   character. Vintage label, normally the build year as a string. NA
      for an un-vintaged storage.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  cluster

  :   character. Cluster label, NA for every cluster.

  start

  :   integer. The first year the storage can be installed. NA means
      unbounded (up to `end`).

  end

  :   integer. The last year the storage can be installed. NA means
      unbounded (from `start` on).

  olife

  :   integer. Operational life of the storage in years, applicable to
      new investment only.

- `capacity`:

  data.frame. Capacity parameters of the storage technology.

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for every vintage. See the `vintage` slot.

  cluster

  :   character. Cluster label selecting the variant this row applies
      to, NA for every cluster. See the `cluster` slot.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  out.stock

  :   numeric. Existing (exogenous) capacity of the discharging part –
      the power the store can deliver, in power units.

  out.cap.lo

  :   numeric. Lower bound of the discharging part – the power the store
      can deliver capacity.

  out.cap.up

  :   numeric. Upper bound of the discharging part – the power the store
      can deliver capacity.

  out.cap.fx

  :   numeric. Fixed value of the discharging part – the power the store
      can deliver capacity. Overrides `out.cap.lo` and `out.cap.up`.

  out.ncap.lo

  :   numeric. Lower bound of the discharging part – the power the store
      can deliver new capacity.

  out.ncap.up

  :   numeric. Upper bound of the discharging part – the power the store
      can deliver new capacity.

  out.ncap.fx

  :   numeric. Fixed value of the discharging part – the power the store
      can deliver new capacity. Overrides `out.ncap.lo` and
      `out.ncap.up`.

  out.ret.lo

  :   numeric. Lower bound of the discharging part – the power the store
      can deliver capacity retirement.

  out.ret.up

  :   numeric. Upper bound of the discharging part – the power the store
      can deliver capacity retirement.

  out.ret.fx

  :   numeric. Fixed value of the discharging part – the power the store
      can deliver capacity retirement. Overrides `out.ret.lo` and
      `out.ret.up`.

  inp.stock

  :   numeric. Existing (exogenous) capacity of the charging part – the
      power the store can absorb, in power units.

  inp.cap.lo

  :   numeric. Lower bound of the charging part – the power the store
      can absorb capacity.

  inp.cap.up

  :   numeric. Upper bound of the charging part – the power the store
      can absorb capacity.

  inp.cap.fx

  :   numeric. Fixed value of the charging part – the power the store
      can absorb capacity. Overrides `inp.cap.lo` and `inp.cap.up`.

  inp.ncap.lo

  :   numeric. Lower bound of the charging part – the power the store
      can absorb new capacity.

  inp.ncap.up

  :   numeric. Upper bound of the charging part – the power the store
      can absorb new capacity.

  inp.ncap.fx

  :   numeric. Fixed value of the charging part – the power the store
      can absorb new capacity. Overrides `inp.ncap.lo` and
      `inp.ncap.up`.

  inp.ret.lo

  :   numeric. Lower bound of the charging part – the power the store
      can absorb capacity retirement.

  inp.ret.up

  :   numeric. Upper bound of the charging part – the power the store
      can absorb capacity retirement.

  inp.ret.fx

  :   numeric. Fixed value of the charging part – the power the store
      can absorb capacity retirement. Overrides `inp.ret.lo` and
      `inp.ret.up`.

  stg.stock

  :   numeric. Existing (exogenous) capacity of the reservoir itself –
      the energy the store can hold, in energy units.

  stg.cap.lo

  :   numeric. Lower bound of the reservoir itself – the energy the
      store can hold capacity.

  stg.cap.up

  :   numeric. Upper bound of the reservoir itself – the energy the
      store can hold capacity.

  stg.cap.fx

  :   numeric. Fixed value of the reservoir itself – the energy the
      store can hold capacity. Overrides `stg.cap.lo` and `stg.cap.up`.

  stg.ncap.lo

  :   numeric. Lower bound of the reservoir itself – the energy the
      store can hold new capacity.

  stg.ncap.up

  :   numeric. Upper bound of the reservoir itself – the energy the
      store can hold new capacity.

  stg.ncap.fx

  :   numeric. Fixed value of the reservoir itself – the energy the
      store can hold new capacity. Overrides `stg.ncap.lo` and
      `stg.ncap.up`.

  stg.ret.lo

  :   numeric. Lower bound of the reservoir itself – the energy the
      store can hold capacity retirement.

  stg.ret.up

  :   numeric. Upper bound of the reservoir itself – the energy the
      store can hold capacity retirement.

  stg.ret.fx

  :   numeric. Fixed value of the reservoir itself – the energy the
      store can hold capacity retirement. Overrides `stg.ret.lo` and
      `stg.ret.up`.

- `input`:

  data.frame. The commodity that FILLS the store – the "charger" side.
  Named by `comm`, with an optional `unit`. Left empty it takes whatever
  `newStorage(commodity = )` supplied.

  comm

  :   character. Commodity consumed to fill the store.

  unit

  :   character. Unit of `comm` on this side, exactly as on
      `technology@input`/ `@output`. Descriptive only: it is carried for
      reporting and `convert()` and never reaches the solver. It is the
      unit of the COMMODITY (e.g. MWh), not of the capacity – capacity
      units follow from `cap2act`, and the storing side has no `cap2act`
      because a reservoir is an amount rather than a rate.

  cap2act

  :   numeric. Capacity to ANNUAL flow, exactly as `technology@cap2act`.
      The flow bound is `inp.af.up * cap2act * cap * pTimesliceShare`,
      so `cap` is a RATE and means the same physical thing on any
      calendar. Defaults to 8760 (hours in a year), which makes `cap`
      read as commodity per HOUR; an hourly full-year model is unchanged
      because 8760 \* (1/8760) = 1. Assumes commodity unit = capacity
      unit x hour (GW with GWh); GW with TWh wants 8.76. The STORING
      side has no cap2act – energy is energy at any resolution.

- `output`:

  data.frame. The commodity the store RELEASES – the "discharger" side.
  Named by `comm`, with an optional `unit`. Left empty it takes whatever
  `newStorage(commodity = )` supplied.

  comm

  :   character. Commodity produced when the store discharges.

  unit

  :   character. Unit of `comm` on this side, exactly as on
      `technology@input`/ `@output`. Descriptive only: it is carried for
      reporting and `convert()` and never reaches the solver. It is the
      unit of the COMMODITY (e.g. MWh), not of the capacity – capacity
      units follow from `cap2act`, and the storing side has no `cap2act`
      because a reservoir is an amount rather than a rate.

  cap2act

  :   numeric. Capacity to ANNUAL flow, exactly as `technology@cap2act`.
      The flow bound is `inp.af.up * cap2act * cap * pTimesliceShare`,
      so `cap` is a RATE and means the same physical thing on any
      calendar. Defaults to 8760 (hours in a year), which makes `cap`
      read as commodity per HOUR; an hourly full-year model is unchanged
      because 8760 \* (1/8760) = 1. Assumes commodity unit = capacity
      unit x hour (GW with GWh); GW with TWh wants 8.76. The STORING
      side has no cap2act – energy is energy at any resolution.

- `storage`:

  data.frame. The commodity the store HOLDS – what `vStorageLevel` is
  measured in. Named by `comm`, with an optional `unit`. Left empty it
  takes whatever `newStorage(commodity = )` supplied. It may differ from
  BOTH flows: a hydrogen store consumes and produces electricity while
  holding hydrogen. There is no `commodity` slot –
  `newStorage(commodity = )` is a shorthand folded into these three at
  construction, so an object never carries two answers to what it
  consumes. Beyond `comm`, this slot carries the storing side's OWN
  capacity and economics, measured in ENERGY (e.g. MWh) rather than
  power. Supplying any of them materialises `vStorageStgCap`, so the
  store's energy can be sized, bounded and priced independently of its
  inverter; supplying none of them (a bare `comm`) leaves the storage
  with a single power capacity and the pre-v0.84 model, with `@duration`
  inlined into the availability bounds.

  comm

  :   character. Commodity the store HOLDS – what `vStorageLevel` is
      measured in.

  unit

  :   character. Unit of `comm`. Descriptive only, carried for reporting
      and `convert()` and never reaching the solver. There is no
      `cap2act` on this side – energy is energy at any resolution.

- `startLevel`:

  data.frame. Energy added to the storage level ONCE PER CYCLE, at the
  first timeslice of the cycle. There is deliberately no `timeslice`
  column: the slice is derived from the calendar and `fullYear`, so it
  cannot be left unset and broadcast to every timeslice the way the old
  `charge` slot could. Which cycle depends on `fullYear`: once a year
  when TRUE, once per parent timeframe when FALSE. The value is ANNUAL.
  When the cycle is shorter than a year each cycle receives its own
  SHARE of it – 365 daily cycles get 1/365 each – so the annual
  endowment is the same however the cycle closes. A calendar covering
  part of a year endows that fraction, consistently. It is FREE to the
  model, by design and unavoidably – a store that ends a cycle below
  where it started has consumed an endowment nobody paid for. Being
  additive, the level at the first timeslice is `startLevel` PLUS
  whatever carried over from the previous cycle, i.e. at least
  `startLevel` rather than exactly it; the model may end the cycle empty
  to make it exact. Renamed from `charge` (via `inflow`) in v0.80; both
  are still accepted with a warning, and any `timeslice` column they
  carried is dropped. NOTE hydro inflow does NOT belong here – use a
  weather-driven `supply` (with `ava.up`, so spilling is free) feeding
  the storage.

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for every vintage. See the `vintage` slot.

  cluster

  :   character. Cluster label selecting the variant this row applies
      to, NA for every cluster. See the `cluster` slot.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  startLevel

  :   numeric. Energy added to the level at the first timeslice of each
      cycle.

- `seff`:

  data.frame. Storage efficiency parameters.

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for every vintage. See the `vintage` slot.

  cluster

  :   character. Cluster label selecting the variant this row applies
      to, NA for every cluster. See the `cluster` slot.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  timeslice

  :   character. Time timeslice to apply the parameter, NA for every
      timeslice.

  stgeff

  :   numeric. Storage decay annual rate.

  inpeff

  :   numeric. Input efficiency rate.

  outeff

  :   numeric. Output efficiency rate.

- `af`:

  data.frame. Availability factor parameters. Unlike a technology's `af`
  (which bounds activity, a flow), the storage `af.*` columns bound the
  stored LEVEL – a stock – as a fraction of the storing capacity, i.e. a
  state-of-charge range. They are not capacity factors. The flow-side
  bounds are the `cinp.*`/`cout.*` columns, which bound charge/discharge
  relative to the charger/discharger capacity per timeslice (those are
  the storage analogue of a technology's `af`).

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for every vintage. See the `vintage` slot.

  cluster

  :   character. Cluster label selecting the variant this row applies
      to, NA for every cluster. See the `cluster` slot.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  timeslice

  :   character. Time timeslice to apply the parameter, NA for every
      timeslice.

  af.lo

  :   numeric. Lower bound of the stored level as a fraction of storing
      capacity (minimum state of charge).

  af.up

  :   numeric. Upper bound of the stored level as a fraction of storing
      capacity (maximum state of charge).

  af.fx

  :   numeric. Fixed value of the stored-level fraction. This parameter
      overrides `af.lo` and `af.up`.

  inp.af.lo

  :   numeric. Lower bound of the charging (input) flow relative to
      charger capacity per timeslice.

  inp.af.up

  :   numeric. Upper bound of the charging (input) flow relative to
      charger capacity per timeslice.

  inp.af.fx

  :   numeric. Fixed value of the charging (input) flow relative to
      charger capacity per timeslice. This parameter overrides
      `inp.af.lo` and `inp.af.up`.

  out.af.lo

  :   numeric. Lower bound of the discharging (output) flow relative to
      discharger capacity per timeslice.

  out.af.up

  :   numeric. Upper bound of the discharging (output) flow relative to
      discharger capacity per timeslice.

  out.af.fx

  :   numeric. Fixed value of the discharging (output) flow relative to
      discharger capacity per timeslice. This parameter overrides
      `out.af.lo` and `out.af.up`.

- `aeff`:

  data.frame. Auxiliary commodities efficiency parameters.

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for every vintage. See the `vintage` slot.

  cluster

  :   character. Cluster label selecting the variant this row applies
      to, NA for every cluster. See the `cluster` slot.

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

  stg2ainp

  :   numeric. Storaage-level-to-auxiliary-input-commodity coefficient
      (multiplier).

  cinp2ainp

  :   numeric. Input-commodity-to-auxiliary-input-commodity coefficient
      (multiplier).

  cout2ainp

  :   numeric. Output-commodity-to-auxiliary-input-commodity coefficient
      (multiplier).

  stg2aout

  :   numeric. Storage-level-to-auxiliary-output-commodity coefficient
      (multiplier).

  cinp2aout

  :   numeric. Input-commodity-to-auxiliary-output-commodity coefficient
      (multiplier).

  cout2aout

  :   numeric. Output-commodity-to-auxiliary-output-commodity
      coefficient (multiplier).

  cap2ainp

  :   numeric. Capacity-to-auxiliary-input-commodity coefficient
      (multiplier).

  cap2aout

  :   numeric. Capacity-to-auxiliary-output-commodity coefficient
      (multiplier).

  ncap2ainp

  :   numeric. New-capacity-to-auxiliary-input-commodity coefficient
      (multiplier).

  ncap2aout

  :   numeric. New-capacity-to-auxiliary-output-commodity coefficient
      (multiplier).

  pho2ainp

  :   numeric. Aux commodity CONSUMED when capacity reaches the END OF
      ITS LIFE (demolition energy, labour). Multiplies the per-year
      phase-out FLOW, so the charge lands ONCE – in the milestone where
      the capacity disappears – not every year it stood. NO
      `pTimesliceShare` is applied and the aux balance is PER TIMESLICE,
      so a value given with `timeslice = NA` applies in EVERY slice and
      the annual total comes out multiplied by the slice count – 8760x
      on an hourly calendar. Give a per-slice value, or name a single
      slice.

  pho2aout

  :   numeric. Aux commodity RELEASED when capacity reaches the END OF
      ITS LIFE (demolition waste, recovered material). Fires even when
      `optimizeRetirement` is FALSE, which is the usual case. NO
      `pTimesliceShare` is applied and the aux balance is PER TIMESLICE,
      so a value given with `timeslice = NA` applies in EVERY slice and
      the annual total comes out multiplied by the slice count – 8760x
      on an hourly calendar. Give a per-slice value, or name a single
      slice.

  ret2ainp

  :   numeric. Aux commodity CONSUMED when capacity is retired EARLY.
      Separate from `pho2ainp` because the two differ physically:
      scrapping an intact plant is not the same job as demolishing a
      worn-out one. NO `pTimesliceShare` is applied and the aux balance
      is PER TIMESLICE, so a value given with `timeslice = NA` applies
      in EVERY slice and the annual total comes out multiplied by the
      slice count – 8760x on an hourly calendar. Give a per-slice value,
      or name a single slice.

  ret2aout

  :   numeric. Aux commodity RELEASED when capacity is retired EARLY
      (scrap). Usually LARGER than `pho2aout`: a plant retired before
      its time is still largely intact, so more material is recoverable.
      NO `pTimesliceShare` is applied and the aux balance is PER
      TIMESLICE, so a value given with `timeslice = NA` applies in EVERY
      slice and the annual total comes out multiplied by the slice count
      – 8760x on an hourly calendar. Give a per-slice value, or name a
      single slice.

  ncap2stg

  :   numeric. New-capacity-to-storage-level coefficient (multiplier).

- `fixom`:

  data.frame. Fixed operation and maintenance cost.

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for every vintage. See the `vintage` slot.

  cluster

  :   character. Cluster label selecting the variant this row applies
      to, NA for every cluster. See the `cluster` slot.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  out.fixom

  :   numeric. Fixed operation and maintenance cost per unit of
      installed capacity of the discharging part – the power the store
      can deliver.

  inp.fixom

  :   numeric. Fixed operation and maintenance cost per unit of
      installed capacity of the charging part – the power the store can
      absorb.

  stg.fixom

  :   numeric. Fixed operation and maintenance cost per unit of
      installed capacity of the reservoir itself – the energy the store
      can hold.

- `varom`:

  data.frame. Variable operation and maintenance cost.

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for every vintage. See the `vintage` slot.

  cluster

  :   character. Cluster label selecting the variant this row applies
      to, NA for every cluster. See the `cluster` slot.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  timeslice

  :   character. Time timeslice to apply the parameter, NA for every
      timeslice.

  inpcost

  :   numeric. Costs associated with the input commodity.

  outcost

  :   numeric. Costs associated with the output commodity.

  stgcost

  :   numeric. Costs associated with the storage level.

- `invcost`:

  data.frame. Investment cost.

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for every vintage. See the `vintage` slot.

  cluster

  :   character. Cluster label selecting the variant this row applies
      to, NA for every cluster. See the `cluster` slot.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  out.invcost

  :   numeric. Investment cost per unit of new capacity of the
      discharging part – the power the store can deliver.

  out.wacc

  :   numeric. Cost of capital used to annuitise the discharging part –
      the power the store can deliver investment (overrides `pWacc`).

  out.payback

  :   numeric. Cost-recovery period of the discharging part – the power
      the store can deliver, in years (overrides the operational life in
      the EAC charge).

  out.eac

  :   numeric. Equivalent annual cost of the discharging part – the
      power the store can deliver, supplied directly instead of being
      annuitised from `invcost`.

  out.retcost

  :   numeric. Cost of retiring the discharging part – the power the
      store can deliver early.

  inp.invcost

  :   numeric. Investment cost per unit of new capacity of the charging
      part – the power the store can absorb.

  inp.wacc

  :   numeric. Cost of capital used to annuitise the charging part – the
      power the store can absorb investment (overrides `pWacc`).

  inp.payback

  :   numeric. Cost-recovery period of the charging part – the power the
      store can absorb, in years (overrides the operational life in the
      EAC charge).

  inp.eac

  :   numeric. Equivalent annual cost of the charging part – the power
      the store can absorb, supplied directly instead of being
      annuitised from `invcost`.

  inp.retcost

  :   numeric. Cost of retiring the charging part – the power the store
      can absorb early.

  stg.invcost

  :   numeric. Investment cost per unit of new capacity of the reservoir
      itself – the energy the store can hold.

  stg.wacc

  :   numeric. Cost of capital used to annuitise the reservoir itself –
      the energy the store can hold investment (overrides `pWacc`).

  stg.payback

  :   numeric. Cost-recovery period of the reservoir itself – the energy
      the store can hold, in years (overrides the operational life in
      the EAC charge).

  stg.eac

  :   numeric. Equivalent annual cost of the reservoir itself – the
      energy the store can hold, supplied directly instead of being
      annuitised from `invcost`.

  stg.retcost

  :   numeric. Cost of retiring the reservoir itself – the energy the
      store can hold early.

- `fullYear`:

  logical. Controls where the charge/discharge cycle closes. If TRUE
  (default), the storage operates across parent timeframes through the
  whole year: the preceding time-timeslice of the first time-timeslice
  of a group is the last time-timeslice of the PREVIOUS group, and only
  the last time-timeslice of the year wraps round to the first. A
  battery on an hourly calendar nested under days can therefore carry
  energy from one day into the next, and seasonal storage is
  representable. If FALSE, the cycle is closed within each parent
  timeframe: the preceding time-timeslice of the first time-timeslice of
  a group is the LAST time-timeslice of the SAME group, so every group
  is an independent loop with no energy carried between them.

- `duration`:

  data.frame. How long the store can run at its rated output: the ratio
  of storing capacity to (dis)charging capacity. `duration = 6` on an
  hourly calendar is a 6-hour store. Renamed from `cap2stg` in v0.80,
  which is still accepted with a warning. It is a BOUND on (storage,
  region, year): `duration.fx` ties energy to power,
  `duration.lo`/`duration.up` let the model choose the ratio – which
  only bites when the storing side is priced, via `@storage$invcost`.
  The bare `duration` column is the scalar shorthand and is normalised
  to `duration.fx` at construction. A ONE-SIDED range opens the other
  side (an `up` alone does not inherit the default `lo` of 1). Saying
  nothing leaves the slot empty and the parameter default of `[1, 1]`
  ties energy to power at one hour, which is the pre-v0.84 behaviour.

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for every vintage.

  cluster

  :   character. Cluster label selecting the variant this row applies
      to, NA for every cluster.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  duration

  :   numeric. Capacity-to-storage ratio (storage duration).

  duration.lo

  :   numeric. Lower bound on the energy-to-power ratio, in hours.

  duration.up

  :   numeric. Upper bound on the energy-to-power ratio, in hours.

  duration.fx

  :   numeric. Fixed energy-to-power ratio, in hours. The scalar
      shorthand `duration = 6` normalises to this.

- `inp2out`:

  data.frame. The charge-to-discharge capacity ratio, dimensionless: how
  big the charger is relative to the discharger. Same bound semantics as
  `duration` – `inp2out.fx` ties the two (one bidirectional inverter),
  `inp2out.lo`/`.up` let the model size them apart, and the bare
  `inp2out` column is the scalar shorthand, normalised to `.fx` at
  construction. A one-sided range opens the other side. Saying nothing
  leaves the slot empty and the parameter default of `[1, 1]` keeps the
  two sides symmetric, which is what a storage without a separate
  charger has always been. It bites only when the charging side is
  priced or bounded via `@input`; otherwise there is no charging
  capacity variable to constrain.

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for all.

  cluster

  :   character. Cluster label selecting the variant this row applies
      to, NA for all.

  region

  :   character. Region the row applies to, NA for all.

  year

  :   integer. Year the row applies to, NA for all.

  inp2out

  :   numeric. Scalar shorthand, normalised to `inp2out.fx`.

  inp2out.lo

  :   numeric. Lower bound on the charge-to-discharge capacity ratio.

  inp2out.up

  :   numeric. Upper bound on the charge-to-discharge capacity ratio.

  inp2out.fx

  :   numeric. Fixed charge-to-discharge capacity ratio.

- `weather`:

  data.frame. Weather factors multipliers.

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for every vintage. See the `vintage` slot.

  cluster

  :   character. Cluster label selecting the variant this row applies
      to, NA for every cluster. See the `cluster` slot.

  weather

  :   character. Name of the weather factor to apply.

  waf.lo

  :   numeric. Coefficient that links the weather factor with the lower
      bound of the availability factor.

  waf.up

  :   numeric. Coefficient that links the weather factor with the upper
      bound of the availability factor.

  waf.fx

  :   numeric. Coefficient that links the weather factor with the fixed
      value of the availability factor. This parameter overrides
      `waf.lo` and `waf.up`.

  inp.waf.lo

  :   numeric. Coefficient that links the weather factor with the lower
      bound of the input commodity availability factor.

  inp.waf.up

  :   numeric. Coefficient that links the weather factor with the upper
      bound of the input commodity availability factor.

  inp.waf.fx

  :   numeric. Coefficient that links the weather factor with the fixed
      value of the input commodity availability factor. This parameter
      overrides `inp.waf.lo` and `inp.waf.up`.

  out.waf.lo

  :   numeric. Coefficient that links the weather factor with the lower
      bound of the output commodity availability factor.

  out.waf.up

  :   numeric. Coefficient that links the weather factor with the upper
      bound of the output commodity availability factor.

  out.waf.fx

  :   numeric. Coefficient that links the weather factor with the fixed
      value of the output commodity availability factor. This parameter
      overrides `out.waf.lo` and `out.waf.up`.

- `optimizeRetirement`:

  logical. Incidates if the retirement of the storage should be
  optimized. Also requires the same parameter in the `model` or
  `scenario` class to be set to TRUE to be effective.

- `misc`:

  list. List of additional parameters that are not used in the model but
  can be used for reference or user-defined functions. For example,
  links to the source of the storage data, or other metadata.
