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

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  slice

  :   character. Time slice to apply the parameter, NA for every slice.

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

  acomm

  :   character. Name of the auxiliary commodity (used in sets).

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  slice

  :   character. Time slice to apply the parameter, NA for every slice.

  trade2ainp

  :   numeric. Trade-to-auxiliary-input-commodity coefficient
      (multiplier).

  trade2aout

  :   numeric. Trade-to-auxiliary-output-commodity coefficient
      (multiplier).

- `invcost`:

  data.frame. Investment cost, used when capacityVariable is TRUE.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  invcost

  :   numeric. Investment cost.

- `fixom`:

  data.frame. (not implemented!) Fixed operation and maintenance costs.

- `varom`:

  data.frame. (not implemented!) Variable operation and maintenance
  costs.

- `olife`:

  numeric. Operational life of the trade object.

- `start`:

  data.frame. Start year when the trade-type of process is available for
  investment.

  region

  :   character. Regions where the trade-type of process is available
      for investment.

  start

  :   integer. The first year when the trade-type of process is
      available for investment.

- `end`:

  data.frame. End year when the trade-type of process is available for
  investment.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  end

  :   integer. The last year when the trade-type of process is available
      for investment.

- `capacity`:

  data.frame. (not implemented!) Capacity parameters of the trade
  object.

- `capacityVariable`:

  logical. If TRUE, the capacity variable of the trade object is
  optimized. If FALSE, the capacity is defined by availability
  parameters (`ava.*`) in the trade-flow units.

- `cap2act`:

  numeric. Capacity to activity ratio.

- `optimizeRetirement`:

  logical. Incidates if the retirement of the trade object should be
  optimized. Also requires the same parameter in the `model` or
  `scenario` class to be set to TRUE to be effective.

- `misc`:

  list. Additional information.
