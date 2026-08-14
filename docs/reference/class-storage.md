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

- `commodity`:

  character. Name of the stored commodity.

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
  via a per-cluster `cap2stg`. Optional: when empty, cluster labels are
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

- `charge`:

  data.frame. Pre-charged level at the beginning of the operational
  cycle.

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

  :   character. Time timeslice for which the charged level will be
      specified.

  charge

  :   numeric. Pre-charged or targeted level at the specified timeslice.

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

  fixom

  :   numeric. Fixed operation and maintenance cost for the specified
      sets.

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

- `fullYear`:

  logical. If TRUE (default), the storage technology operates between
  parent timeframes through the year. The last time-timeslice in the
  timeframe is used as a preciding time-timeslice for the first
  time-timeslice in the the same group of time-timeslices within the
  parent timeframe. if FALSE, the storage charge and discchare cycle is
  limited to the parent timeframe. The last time-timeslice in the
  timeframe is used as a preciding time-timeslice for the first
  time-timeslice in the the same group of time-timeslices within the
  parent timeframe.

- `cap2stg`:

  data.frame. Charging and discharging capacity to the storing capacity
  inverse ratio, i.e. the storage duration. A data.frame rather than a
  scalar so it can differ by cluster – 1h / 4h / 8h duration classes of
  the same storage. A bare scalar (`cap2stg = 4`) is still accepted.

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

  cap2stg

  :   numeric. Capacity-to-storage ratio (storage duration).

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

- `optimizeRetirement`:

  logical. Incidates if the retirement of the storage should be
  optimized. Also requires the same parameter in the `model` or
  `scenario` class to be set to TRUE to be effective.

- `misc`:

  list. List of additional parameters that are not used in the model but
  can be used for reference or user-defined functions. For example,
  links to the source of the storage data, or other metadata.
