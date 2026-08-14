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

  data.frame. Technical parameters of trade.

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for every vintage. See the `vintage` slot.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  timeslice

  :   character. Time timeslice to apply the parameter, NA for every
      timeslice.

  trade

  :   numeric. Trade volume.

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

  data.frame. Investment cost of the trade capacity (per unit of
  capacity).

  vintage

  :   character. Vintage label selecting the variant this row applies
      to, NA for every vintage. See the `vintage` slot.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

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

  data.frame. (not implemented!) Fixed operation and maintenance costs.

- `varom`:

  data.frame. (not implemented!) Variable operation and maintenance
  costs.

- `capacity`:

  data.frame. (not implemented!) Capacity parameters of the trade
  object.

- `vintage`:

  data.frame. Investment window and operational life of the trade
  object, one row per vintage. Replaces the former `start`, `end` and
  `olife` slots. A vintage is a separately investable variant that keeps
  the characteristics of its build year for its whole life, so several
  vintages of one corridor mean several capacities on the same
  `(src, dst)` route, with their flows summed in the commodity balance.
  `region` and `cluster` are present for a uniform shape across process
  classes but are unused for trade: its scope comes from the route
  endpoints, and `routes` already provides the multiplicity a cluster
  dimension would add.

  vintage

  :   character. Vintage label, normally the build year as a string. NA
      for an un-vintaged trade.

  region

  :   character. Unused for trade (no `region` slot); present for
      consistency with the other classes.

  cluster

  :   character. Unused for trade (no cluster dimension); present for
      consistency with the other classes.

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
