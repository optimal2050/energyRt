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

- `horizon`:

  horizon. Horizon object with the model time parameters. The horizon
  defines the planning period and intervals of the model.

- `discount`:

  data.frame. Discount rates, can be assigned by region and year.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  discount

  :   numeric. Discount rate. Default is 0.05. The discount rate is used
      to calculate the present value of future costs and benefits.

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
  slices.

- `misc`:

  list. Any additional data or information to store in the object.

## See also

Other class config settings scenario model:
[`class-settings`](https://energyRt.org/reference/class-settings.md)
