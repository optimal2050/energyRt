# An S4 class to represent storage type of technological process.

Storage type of technological processes with accumulating capacity of a
commodity.

## Details

Storage can be used in combination with other processes, such as
technologies, supply, or demand to represent complex technological
chains, demand or supply technologies with time-shift. Operation of
storage includes accumulation, storing, and release of the stored
commodity. The storing cycle operates on the ordered time-slices of the
commodity timeframe. The cycle is looped either on an annual basis (last
time-slice of a year follows the first time slice of the same year) or
within the parent time-frame (for example, when commodity time-frame is
"HOUR" and the parent time-frame is "DAY" then the storage cycle will be
a calendar day).

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

- `start`:

  data.frame. Start year when the storage is available for installation.

  region

  :   character. Regions where the storage is available for investment.

  start

  :   integer. The first year when the storage is available for
      investment.

- `end`:

  data.frame. Last year when the storage is available for investment.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  end

  :   integer. The last year when the storage is available for
      investment.

- `olife`:

  data.frame. Operational life of the storage technology, applicable to
  the new investment only, the operational life (retirement) of
  preexiting capacity is described in the `stock` slot.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  olife

  :   integer. Operational life of the storage technology in years.

- `capacity`:

  data.frame. Capacity parameters of the storage technology.

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

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  slice

  :   character. Time slice for which the charged level will be
      specified.

  charge

  :   numeric. Pre-charged or targeted level at the specified slice.

- `seff`:

  data.frame. Storage efficiency parameters.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  slice

  :   character. Time slice to apply the parameter, NA for every slice.

  stgeff

  :   numeric. Storage decay annual rate.

  inpeff

  :   numeric. Input efficiency rate.

  outeff

  :   numeric. Output efficiency rate.

- `af`:

  data.frame. Availability factor parameters.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  slice

  :   character. Time slice to apply the parameter, NA for every slice.

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

  acomm

  :   character. Name of the auxiliary commodity (used in sets).

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  slice

  :   character. Time slice to apply the parameter, NA for every slice.

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

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  slice

  :   character. Time slice to apply the parameter, NA for every slice.

  inpcost

  :   numeric. Costs associated with the input commodity.

  outcost

  :   numeric. Costs associated with the output commodity.

  stgcost

  :   numeric. Costs associated with the storage level.

- `invcost`:

  data.frame. Investment cost.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  invcost

  :   numeric. Overnight investment cost for the specified region and
      year.

  wacc

  :   numeric. Weighted average cost of capital. If not supplied, the
      discount from the model or scenario is used. (currently ignored)

- `fullYear`:

  logical. If TRUE (default), the storage technology operates between
  parent timeframes through the year. The last time-slice in the
  timeframe is used as a preciding time-slice for the first time-slice
  in the the same group of time-slices within the parent timeframe. if
  FALSE, the storage charge and discchare cycle is limited to the parent
  timeframe. The last time-slice in the timeframe is used as a preciding
  time-slice for the first time-slice in the the same group of
  time-slices within the parent timeframe.

- `cap2stg`:

  numeric. Charging and discharging capacity to the storing capacity
  inverse ratio. Can be used to define the storage duration.

- `weather`:

  data.frame. Weather factors multipliers.

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
