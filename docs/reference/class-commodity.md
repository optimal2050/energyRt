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
  electricity. The coarse balance is the plain SUM of its children:
  regional quantities are extensive, so no weighting is applied.
  Requires a geoscale on the model config whose geoframes NEST –
  adjacent levels must refine one another, or a region would be counted
  into two parents. Supported by all back-ends.

- `unit`:

  character. The main unit of the commodity used in the model.

- `cluster`:

  data.frame. Declaration of the commodity's parallel grades. For a
  commodity a cluster is a RESOURCE TIER: the same substance in grades
  that must NOT pool, so tier-3 land cannot serve a tier-1 plant. Tiers
  exist because the commodity balance is keyed on
  `(comm, region, year, timeslice)` and carries no cluster – every
  producer of one commodity name writes into the same cell. Declaring
  tiers here expands the object into one commodity per tier (`LAND` tier
  `T3` -\> `LAND_CLT3`), each with its own balance, and a reference from
  a clustered process resolves BY CLUSTER NAME. A commodity with no
  clusters is shared as before, which is why `ELC` needs no flag. Tiers
  may differ in DATA, not only in name: `emis`, `agg` and `property`
  carry a `cluster` column, so coal grades can differ in carbon content
  and heat value. A row with no cluster is shared by every tier, as
  elsewhere. Declared labels are authoritative – a label used in a data
  slot but absent from this declaration is a typo and an error. The
  declaration is REQUIRED whenever a data slot carries a label: labels
  are never harvested. There is deliberately no `region` column: a
  commodity has no `region` slot (only `geoframe`), and which tiers
  exist in a region follows from the processes that reference them.

  cluster

  :   character. Tier label, used in the expanded commodity name.

  desc

  :   character. Description of the tier.

  order

  :   integer. Reporting order, 1 = the best tier. Without it labels
      sort alphabetically and "T10" would precede "T2".

- `emis`:

  data.frame. Emissions factors related to the commodity consumption (if
  "combustion" parameter of a technology which consumes the commodity is
  \> 0).

  cluster

  :   character. Tier this row applies to, NA for every tier. See the
      `cluster` slot: grades of one commodity differ in exactly this
      kind of data (carbon content, heat value), so a row may be shared
      or per tier.

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

  cluster

  :   character. Tier this row applies to, NA for every tier. See the
      `cluster` slot: grades of one commodity differ in exactly this
      kind of data (carbon content, heat value), so a row may be shared
      or per tier.

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
  to the model input, but it gives
  [`convert()`](https://energyRt.org/reference/convert.md) the physics
  needed to move between measures of the same commodity (energy, mass,
  volume, amount). A property whose unit is a ratio of two dimensions,
  such as `lhv` in "GJ/t", acts as a conversion edge between them. Only
  properties of the commodity itself belong here – anything whose
  numerator is a *different* commodity, such as a CO2 emission factor,
  belongs in `emis`. See
  [`commodity_properties()`](https://energyRt.org/reference/commodity_properties.md)
  for the recognised names.

  cluster

  :   character. Tier this row applies to, NA for every tier. See the
      `cluster` slot: grades of one commodity differ in exactly this
      kind of data (carbon content, heat value), so a row may be shared
      or per tier.

  property

  :   character. Name of the property, e.g. "lhv", "hhv", "density",
      "molar_mass", or "frac_C" for an element mass fraction.
      Unrecognised names are kept with a warning.

  value

  :   numeric. The value of the property, in `unit`. Always the
      deterministic point estimate – the mean, mode or best guess;
      `min`, `max` and `sd` describe uncertainty around it and are never
      read by [`convert()`](https://energyRt.org/reference/convert.md).

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
