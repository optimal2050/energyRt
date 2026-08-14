# An S4 class to represent default model configuration.

Config class is used to represent the default model configuration. It is
stored in the model object and is used to initialize the scenario
settings.

## Slots

- `name`:

  character. Name of the configuration object for own references, also
  can be used in functions to distinguish between different model or
  scenario instances.

- `desc`:

  character. Description of the configuration object for own references.

- `region`:

  character. Coma separated string of all region names in the model. All
  regions used in the model-objects should be listed here.

- `calendar`:

  calendar. Calendar object with the model time parameters.

- `geoscale`:

  ANY. Optional
  [`geoscales::Geoscale`](https://optimal2050.github.io/geoscales/r/reference/Geoscale.html)
  object describing the model's regions – their nesting into coarser
  levels, weights, and (optionally) geometry. Purely additional
  information about the labels in `region`, which remains authoritative;
  the geoscale is used for plotting, reporting and subsetting and never
  changes the optimisation model. `NULL` when unset. Typed `ANY` because
  `geoscales` is an optional (Suggests) dependency.

- `horizon`:

  horizon. Horizon object with the model time parameters. The horizon
  defines the planning period and intervals of the model.

- `discount`:

  data.frame. The model's two rates, which can be assigned by region and
  year. `wacc` annuitises investment into the equivalent annual cost;
  `sdr` discounts the stream of system costs in the objective. They are
  independent – there is no fallback from one to the other. As an
  ARGUMENT (`newModel(discount = )`, `update(cfg, discount = )`) this
  slot also accepts the shorthand `discount`, a single rate that plays
  both roles: `discount = 0.05` is equivalent to
  `data.frame(wacc = 0.05, sdr = 0.05)`. Per row you supply either
  `discount`, or both `wacc` and `sdr` – a partial pair or a mix of the
  two forms is an error. There is no `discount` column: the shorthand is
  expanded on entry, so the stored table always has both rates.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  wacc

  :   numeric. Weighted average cost of capital, used to annuitise
      investment costs into `pTechEac` / `pStorageEac` / `pTradeEac`. A
      process may override it with its own `@invcost$wacc`. Default is
      0.

  sdr

  :   numeric. Social discount rate, used to build the cumulative
      discount factor applied to total system costs in the objective. A
      property of the model, never of an individual process. Default is
      0.

- `discountFirstYear`:

  logical. If TRUE, the discounting starts from the beginning of the
  year. If FALSE, the discounting starts from the end of the first year,
  i.e., the first year is not discounted.

- `optimizeRetirement`:

  logical. Incidates if the retirement of capacities of the model
  objects should be optimized. Also requires the same parameter in the
  classes with capacitu, such as `technology`, `storage`, `trade` to be
  set to TRUE to be effective for a specific object.

- `defVal`:

  data.frame. Default values of model parameters. The data frame with
  the default values for every parameter in the model, used to fill the
  missing values in the model objects. The values are used in the
  interpolation step. The data is stored in `energyRt::.defVal` object
  and can be overwritten by the user and supplied to the model as a
  parameter.

- `interpolation`:

  data.frame. Default interpolation rules for every parameter in the
  model. The data frame with the default interpolation rules is stored
  in `energyRt::.defInt` object and can be overwritten by the user and
  supplied to the model as a parameter.

- `debug`:

  data.frame. Artificial (dummy or sluck) variables to debug model
  infeasibility. Can be specified by commodities, regions, years, and
  timeslices.

- `variant_prefix`:

  character. Named character vector giving the prefixes inserted into
  the names of expanded technology variants, keyed by variant dimension:
  `c(vintage = "_VIN", cluster = "_CL")`. A technology `WIND` with
  vintage 2030 and cluster "01" then becomes the set member
  `WIND_VIN2030_CL01`. Number the clusters rather than labelling them
  ("01", not "best"): the prefix already says what the axis is, so a
  label that repeats it reads as `WIND_VIN2030_CLCL01`. Model-wide,
  because these become solver set-member names; the scenario settings
  inherit the value and must agree with it. Each prefix must be
  non-empty, match `^[[:alnum:]_]+$`, and the two must differ. A partial
  value such as `c(cluster = "_RC")` is merged over the defaults.

- `misc`:

  list. Any additional data or information to store in the object.

## See also

Other class config settings scenario model:
[`class-settings`](https://energyRt.org/reference/class-settings.md)
