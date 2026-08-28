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

- `geoframe`:

  character. The geoscale level at which this commodity is balanced –
  the spatial counterpart of `timeframe`. The finest level in the model
  is used by default, reproducing the flat, single-level behaviour.
  Naming a coarser level (e.g. balancing steel nationally while
  electricity stays state-level) asserts free, unlimited transport of
  the commodity within that level, so it suits goods with a genuinely
  integrated market and never a network-constrained carrier such as
  electricity. Requires a geoscale on the model config; GLPK and GAMS
  only.

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

- `property`:

  data.frame. Physical properties of the commodity – heating values,
  density, molar mass, composition. Reference data: it is never written
  to the model input, but it gives `convert()` the physics needed to
  move between measures of the same commodity (energy, mass, volume,
  amount). A property whose unit is a ratio of two dimensions, such as
  `lhv` in "GJ/t", acts as a conversion edge between them. Only
  properties of the commodity itself belong here – anything whose
  numerator is a *different* commodity, such as a CO2 emission factor,
  belongs in `emis`. See
  [`commodity_properties()`](https://energyRt.org/reference/commodity_properties.md)
  for the recognised names.

  property

  :   character. Name of the property, e.g. "lhv", "hhv", "density",
      "molar_mass", or "frac_C" for an element mass fraction.
      Unrecognised names are kept with a warning.

  value

  :   numeric. The value of the property, in `unit`. Always the
      deterministic point estimate – the mean, mode or best guess;
      `min`, `max` and `sd` describe uncertainty around it and are never
      read by `convert()`.

  min

  :   numeric. Lower bound of the value, for sensitivity analysis.
      Optional.

  max

  :   numeric. Upper bound of the value, for sensitivity analysis.
      Optional.

  sd

  :   numeric. Standard deviation of the value, for sensitivity
      analysis. Optional.

  dist

  :   character. Distribution of the value for sensitivity analysis, one
      of "point", "uniform", "triangular", "pert", "normal", or
      "lognormal". Optional; "point" or NA means the value is treated as
      certain. "uniform" reads `min` and `max`; "triangular" and "pert"
      read `min`, `value` (the mode) and `max`; "normal" and "lognormal"
      read `value` and `sd`.

  unit

  :   character. Unit of the property, e.g. "GJ/t", "t/m3", "g/mol",
      "kg/kg". Applies to `value`, `min`, `max` and `sd` alike.

  comment

  :   character. Optional note on the source of the value or the
      conditions it refers to, such as "bulk, as stockpiled" or "gas, 0
      C, 1 atm".

- `misc`:

  list. List of additional parameters that are not used in the model but
  can be used for reference or user-defined functions. For example,
  links to the source of the commodity data, or other metadata. Two
  names are conventional: `misc$image`, a path or URL to an illustration
  used in reports, and `misc$icon`, a path or URL to a small icon. Both
  can be set with the `image` and `icon` arguments of `newCommodity`.
