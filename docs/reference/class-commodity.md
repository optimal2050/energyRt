# An S4 class to represent a commodity

A commodity is a good or service that is produced and consumed in the
model. The commodity class is used to store information about the
commodity. All processes in the model operate on commodities, i.e. they
either generate, produce, consume, transform, store, or transport
commodities. The creation of a commodity object is done with the
`newCommodity` function.

## Slots

- `name`:

  character. Name of the commodity.

- `desc`:

  character. Optional description of the commodity for reference.

- `limtype`:

  factor or character. The limit type of the commodity in balance
  equation, "LO", "UP", or "FX". "LO" by default, meaning that the level
  of commodity in the model is restricted with the lower bound, excess
  is allowed. "UP" means that the level of commodity cannot exceed the
  upper bound. "FX" means that total commudity supply and demand are
  equal, no excess or deficit is allowed.

- `timeframe`:

  character. The default time-frame this commodity operates in the
  model. The lowest timeframe in the model is used by default.

- `unit`:

  character. The main unit of the commodity used in the model.

- `emis`:

  data.frame. Emissions factors related to the commodity consumption (if
  "combustion" parameter of a technology which consumes the commodity is
  \> 0).

  comm

  :   character. Name of the emitted commodity.

  unit

  :   character. Unit of the emission factor.

  emis

  :   numeric. Emission factor, emissions released per unit of the
      consumed commodity.

- `agg`:

  data.frame. Used to define an aggregation of several commodities into
  the `name` commodity.

  comm

  :   character. Name of a commodity being aggregated.

  unit

  :   character. Unit of the commodity being aggregated.

  agg

  :   numeric. weight of the commodity in the aggregation, must be set
      for all aggregated commodities.

- `misc`:

  list. List of additional parameters that are not used in the model but
  can be used for reference or user-defined functions. For example,
  links to the source of the commodity data, or other metadata.
