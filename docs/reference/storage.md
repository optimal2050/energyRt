# Create new storage object

Storage type of technological processes with accumulating capacity of a
commodity.

## Usage

``` r
newStorage(
  name = "",
  desc = "",
  commodity = character(),
  aux = data.frame(),
  region = character(),
  start = data.frame(),
  end = data.frame(),
  olife = data.frame(),
  input = data.frame(),
  output = data.frame(),
  storage = data.frame(),
  startLevel = data.frame(),
  seff = data.frame(),
  aeff = data.frame(),
  af = data.frame(),
  fixom = data.frame(),
  varom = data.frame(),
  invcost = data.frame(),
  capacity = data.frame(),
  cluster = data.frame(),
  vintage = data.frame(),
  duration = NULL,
  inp2out = NULL,
  fullYear = TRUE,
  weather = data.frame(),
  optimizeRetirement = FALSE,
  misc = list(),
  ...
)
```

## Arguments

- name:

  character. Name of the storage (used in sets).

- desc:

  character. Description of the storage.

- commodity:

  convenience shorthand: the commodity used for whichever of `input`,
  `output` and `storage` do not name their own. A battery is
  `commodity = "ELC"` and nothing else; a hydrogen store is
  `commodity = "H2"` with `input`/`output` naming `"ELC"`. There is no
  `@commodity` slot – this is folded into the three roles at
  construction, so the object never carries two answers.

- aux:

  data.frame. Auxiliary commodities.

  acomm

  :   character. Name of the auxiliary commodity (used in sets).

  unit

  :   character. Unit of the auxiliary commodity.

- region:

  character. Region where the storage technology exists or can be
  installed.

- start:

  deprecated, use the `start` column of `vintage`.

- end:

  deprecated, use the `end` column of `vintage`.

- olife:

  deprecated, use the `olife` column of `vintage`.

- input:

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
      The flow bound is `cinp.up * cap2act * cap * pTimesliceShare`, so
      `cap` is a RATE and means the same physical thing on any calendar.
      Defaults to 8760 (hours in a year), which makes `cap` read as
      commodity per HOUR; an hourly full-year model is unchanged because
      8760 \* (1/8760) = 1. Assumes commodity unit = capacity unit x
      hour (GW with GWh); GW with TWh wants 8.76. The STORING side has
      no cap2act – energy is energy at any resolution.

- output:

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
      The flow bound is `cinp.up * cap2act * cap * pTimesliceShare`, so
      `cap` is a RATE and means the same physical thing on any calendar.
      Defaults to 8760 (hours in a year), which makes `cap` read as
      commodity per HOUR; an hourly full-year model is unchanged because
      8760 \* (1/8760) = 1. Assumes commodity unit = capacity unit x
      hour (GW with GWh); GW with TWh wants 8.76. The STORING side has
      no cap2act – energy is energy at any resolution.

- storage:

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

  :   character. Commodity held in the store.

  unit

  :   character. Unit of `comm` on this side, exactly as on
      `technology@input`/ `@output`. Descriptive only: it is carried for
      reporting and `convert()` and never reaches the solver. It is the
      unit of the COMMODITY (e.g. MWh), not of the capacity – capacity
      units follow from `cap2act`, and the storing side has no `cap2act`
      because a reservoir is an amount rather than a rate.

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

  stock

  :   numeric. Pre-existing energy capacity, in commodity units. The
      energy analogue of `capacity$stock`, which is power.

  cap.lo

  :   numeric. Lower bound on total energy capacity.

  cap.up

  :   numeric. Upper bound on total energy capacity.

  cap.fx

  :   numeric. Fixed total energy capacity.

  ncap.lo

  :   numeric. Lower bound on new energy capacity added in the period.

  ncap.up

  :   numeric. Upper bound on new energy capacity added in the period.

  ncap.fx

  :   numeric. Fixed new energy capacity added in the period.

  invcost

  :   numeric. Overnight investment cost per unit of ENERGY capacity
      (currency/MWh), annuitised on the storage's own `@vintage` – one
      lifetime and one wacc for the object, two capital costs on
      different bases. This is what previously had to be hand-multiplied
      into the per-MW `@invcost`.

  fixom

  :   numeric. Fixed O&M cost per unit of energy capacity per year.

- startLevel:

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
  where it started has consumed an endowment nobody paid for. PyPSA's
  `state_of_charge_initial` has the same property. Being additive, the
  level at the first timeslice is `startLevel` PLUS whatever carried
  over from the previous cycle, i.e. at least `startLevel` rather than
  exactly it; the model may end the cycle empty to make it exact.
  Renamed from `charge` (via `inflow`) in v0.80; both are still accepted
  with a warning, and any `timeslice` column they carried is dropped.
  NOTE hydro inflow does NOT belong here – use a weather-driven `supply`
  (with `ava.up`, so spilling is free) feeding the storage.

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

- seff:

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

- aeff:

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

  ncap2stg

  :   numeric. New-capacity-to-storage-level coefficient (multiplier).

- af:

  data.frame. Availability factor parameters.

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

  :   numeric. Lower bound of the availability factor.

  af.up

  :   numeric. Upper bound of the availability factor.

  af.fx

  :   numeric. Fixed value of the availability factor. This parameter
      overrides `af.lo` and `af.up`.

  cinp.lo

  :   numeric. Lower bound of the input commodity availability factor.

  cinp.up

  :   numeric. Upper bound of the input commodity availability factor.

  cinp.fx

  :   numeric. Fixed value of the input commodity availability factor.
      This parameter overrides `cinp.lo` and `cinp.up`.

  cout.lo

  :   numeric. Lower bound of the output commodity availability factor.

  cout.up

  :   numeric. Upper bound of the output commodity availability factor.

  cout.fx

  :   numeric. Fixed value of the output commodity availability factor.
      This parameter overrides `cout.lo` and `cout.up`.

- fixom:

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

  fixom

  :   numeric. Fixed operation and maintenance cost for the specified
      sets.

- varom:

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

- invcost:

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

  invcost

  :   numeric. Overnight investment cost for the specified region and
      year.

  wacc

  :   numeric. Weighted average cost of capital used to annuitise
      `invcost` for this storage. Overrides the model-wide `wacc` (see
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

  :   numeric. Costs of early retirement of the storage, default is 0.

- capacity:

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

  cap

  :   numeric. Capacity of the storage technology.

  cap.lo

  :   numeric. Lower bound of the storage capacity.

  cap.up

  :   numeric. Upper bound of the storage capacity.

  cap.fx

  :   numeric. Fixed value of the storage capacity. This parameter
      overrides `cap.lo` and `cap.up`.

  ncap.lo

  :   numeric. Lower bound of the new storage capacity.

  ncap.up

  :   numeric. Upper bound of the new storage capacity.

  ncap.fx

  :   numeric. Fixed value of the new storage capacity. This parameter
      overrides `ncap.lo` and `ncap.up`.

  ret.lo

  :   numeric. Lower bound of the storage capacity retirement.

  ret.up

  :   numeric. Upper bound of the storage capacity retirement.

  ret.fx

  :   numeric. Fixed value of the storage capacity retirement. This
      parameter overrides `ret.lo` and `ret.up`.

- cluster:

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

- vintage:

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

- duration:

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
  nothing leaves the slot empty and the parameter default of 1, 1 ties
  energy to power at one hour, which is the pre-v0.84 behaviour.

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

- inp2out:

  data.frame. The charge-to-discharge capacity ratio, dimensionless: how
  big the charger is relative to the discharger. Same bound semantics as
  `duration` – `inp2out.fx` ties the two (one bidirectional inverter),
  `inp2out.lo`/`.up` let the model size them apart, and the bare
  `inp2out` column is the scalar shorthand, normalised to `.fx` at
  construction. A one-sided range opens the other side. Saying nothing
  leaves the slot empty and the parameter default of 1, 1 keeps the two
  sides symmetric, which is what a storage without a separate charger
  has always been. It bites only when the charging side is priced or
  bounded via `@input`; otherwise there is no charging capacity variable
  to constrain.

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

- fullYear:

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

- weather:

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

  wcinp.lo

  :   numeric. Coefficient that links the weather factor with the lower
      bound of the input commodity availability factor.

  wcinp.up

  :   numeric. Coefficient that links the weather factor with the upper
      bound of the input commodity availability factor.

  wcinp.fx

  :   numeric. Coefficient that links the weather factor with the fixed
      value of the input commodity availability factor. This parameter
      overrides `wcinp.lo` and `wcinp.up`.

  wcout.lo

  :   numeric. Coefficient that links the weather factor with the lower
      bound of the output commodity availability factor.

  wcout.up

  :   numeric. Coefficient that links the weather factor with the upper
      bound of the output commodity availability factor.

  wcout.fx

  :   numeric. Coefficient that links the weather factor with the fixed
      value of the output commodity availability factor. This parameter
      overrides `wcout.lo` and `wcout.up`.

- optimizeRetirement:

  logical. Incidates if the retirement of the storage should be
  optimized. Also requires the same parameter in the `model` or
  `scenario` class to be set to TRUE to be effective.

- misc:

  list. List of additional parameters that are not used in the model but
  can be used for reference or user-defined functions. For example,
  links to the source of the storage data, or other metadata.

## Value

storage object

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

## Examples

``` r
STG1 <- newStorage(
  name = "STG1",
  desc = "Storage description",
  commodity = "electricity",
  region = "R1",
  start = data.frame(region = "R1", start = 0),
  end = data.frame(region = "R1", end = 1),
  olife = data.frame(region = "R1", olife = 20),
  startLevel = data.frame(
    # region = "R1",
    year = 2020,
    startLevel = 0.1
  ),
  seff = data.frame(
    # region = "R1",
    # year = 2020,
    # timeslice = "HOUR",
    stgeff = 0.999,
    inpeff = 0.9,
    outeff = 0.9
  ),
  aeff = data.frame(
    acomm = "electricity",
    region = "R1",
    year = 2020,
    # timeslice = "HOUR",
    stg2ainp = 0.9,
    cinp2ainp = 0.1,
    cout2ainp = 0.2,
    stg2aout = 0.9,
    cinp2aout = 0.9,
    cout2aout = 0.9,
    cap2ainp = 0.9,
    cap2aout = 0.9,
    ncap2ainp = 0.9,
    ncap2aout = 0.9,
    ncap2stg = 0.9
  ),
  af = data.frame(
    region = "R1", year = 2020, timeslice = "HOUR",
    af.lo = 0.9, af.up = 0.9, af.fx = 0.9, cinp.up = 0.9,
    cinp.fx = 0.9, cinp.lo = 0.9, cout.up = 0.9,
    cout.fx = 0.9, cout.lo = 0.9
  ),
  fixom = data.frame(region = "R1", year = 2020, fixom = 0.9),
  varom = data.frame(
    region = "R1", year = 2020, timeslice = "HOUR",
    inpcost = 0.9, outcost = 0.9, stgcost = 0.9
  ),
  invcost = data.frame(
    region = "R1", year = 2020, invcost = 0.9,
    wacc = 0.09, payback = 10, retcost = 0.9
  ),
  capacity = data.frame(
    region = "R1", year = 2020, stock = 0.9,
    cap.lo = 0.9, cap.up = 0.9, cap.fx = 0.9, ncap.lo = 0.9,
    ncap.up = 0.9, ncap.fx = 0.9, ret.lo = 0.9, ret.up = 0.9,
    ret.fx = 0.9
  ),
  duration = 1,
  fullYear = TRUE,
  weather = data.frame(
    weather = "sunny",
    waf.lo = 0.9,
    waf.up = 0.9,
    waf.fx = 0.9, wcinp.lo = 0.9,
    wcinp.fx = 0.9, wcinp.up = 0.9, wcout.lo = 0.9, wcout.fx = 0.9,
    wcout.up = 0.9
  ),
  optimizeRetirement = FALSE,
  misc = list()
  )
```
