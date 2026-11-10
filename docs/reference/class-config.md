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

- `units`:

  data.frame. Optional. The model's generic units: what everything in it
  is expressed in, and the declared fallback for an object that states
  nothing of its own. The columns are the ones an object's `@units`
  uses, so the two levels can be compared directly. Every column is
  optional and an empty one claims nothing, which is how every model
  written before this slot behaves. Nothing here converts a value: a
  unit is declared, reported and cross-checked only.

  comm

  : character. The model's generic unit for a commodity flow, e.g.
    "GWh".

  capacity

  : character. The model's generic unit of capacity, e.g. "GW".

  activity

  : character. The model's generic unit of activity.

  use

  : character. The model's generic unit of use; falls back to `activity`
    when empty.

  currency

  : character. The currency every cost in the model is stated in, and
    the unit of the objective. Declared, it is checked against each
    object's own `@units$currency`: an object naming a different
    currency is an error, not a conversion, because there is no exchange
    rate in the model to convert with. The point is objects moving
    between models – a technology priced in `MUSD` added to a model
    counting `bnEUR` is a mistake that no number in the output reveals.
    It was the `@currency` slot up to version 0.91; `currency =` is
    still the argument that sets it.

- `region`:

  character. Coma separated string of all region names in the model. All
  regions used in the model-objects should be listed here.

- `calendar`:

  calendar. Calendar object with the model time parameters.

- `geoscale`:

  ANY. Optional
  [`geoscales::GeoScale`](https://optimal2050.github.io/geoscales/r/reference/GeoScale.html)
  object describing the model's regions – their nesting into coarser
  levels, weights, and (optionally) geometry. `region` remains
  authoritative: it is always the finest level, and the geoscale adds
  the structure above it. Attaching one is inert on its own – it widens
  the model's region set with the coarser codes and enables level-aware
  plotting, reporting and subsetting – but it is also what makes
  `commodity@geoframe` possible, and a commodity balanced at a coarser
  level DOES change the optimisation model. `NULL` when unset. Typed
  `ANY` because `geoscales` is an optional (Suggests) dependency.

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

  : character. Region name to apply the parameter, NA for every region.

  year

  : integer. Year to apply the parameter, NA for every year.

  wacc

  : numeric. Weighted average cost of capital, used to annuitise
    investment costs into `pTechEac` / `pStorageOutEac` / `pTradeEac`. A
    process may override it with its own `@invcost$wacc`. Default is 0.

  sdr

  : numeric. Social discount rate, used to build the cumulative discount
    factor applied to total system costs in the objective. A property of
    the model, never of an individual process. Default is 0.

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

  data.frame. Model-wide OVERRIDES of parameter default values, applied
  at the interpolation step. Empty unless set: a parameter's own default
  is declared once, in the catalogue (`data-raw/parameters.yml`), and a
  column here replaces it for the whole model. One column per override,
  named after the PARAMETER – `pDemand` for a single value, `pTechAf.lo`
  / `pTechAf.up` for a bound. A column naming no parameter is an error,
  not a silent no-op. Up to version 0.91 this table was pre-filled with
  every column of the model and keyed by column name; a table saved then
  must be cleared or re-keyed.

- `interpolation`:

  data.frame. Model-wide OVERRIDES of parameter interpolation rules,
  keyed exactly as `defVal` and subject to the same rules. Empty unless
  set; each parameter's own rule is declared in the catalogue
  (`data-raw/parameters.yml`).

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
