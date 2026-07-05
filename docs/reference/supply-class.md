# An S4 class to represent a supply of a commodity

An S4 class to represent a supply of a commodity

## Value

supply object with given specifications.

## Slots

- `name`:

  character. Name of the supply object, used in sets.

- `desc`:

  character. Description of the supply object.

- `commodity`:

  character. The supplied commodity short name.

- `unit`:

  character. The main unit of the commodity used in the model.

- `weather`:

  data.frame. Weather factors to apply to the supply.

  weather

  :   character. Name of the weather factor to apply. Must match the
      weather factor names in a `weather` class in the model.

  wava.lo

  :   numeric. Coefficient that links the weather factor with the lower
      bound of the availability factor `ava.lo`.

  wava.up

  :   numeric. Coefficient that links the weather factor with the upper
      bound of the availability factor `ava.up`.

  wava.fx

  :   numeric. Coefficient that links the weather factor with the fixed
      value of the availability factor `ava.fx`. This parameter
      overrides `wava.lo` and `wava.up`.

- `reserve`:

  data.frame. Total available resource. Applicable to exhaustible
  resources. Set for each region. If not set, the resource is considered
  infinite.

  region

  :   character. Region name to apply the parameter. Use NA to apply to
      all regions.

  res.lo

  :   numeric. Lower bound of the total available resource.

  res.up

  :   numeric. Upper bound of the total available resource.

  res.fx

  :   numeric. Fixed value of the total available resource. This
      parameter overrides `res.lo` and `res.up`.

- `availability`:

  data.frame. Availability of the resource in physical units.

  region

  :   character. Region name to apply the parameter. Use NA to apply to
      all regions.

  year

  :   integer. Year to apply the parameter. Use NA to apply to all
      years.

  slice

  :   character. Time slice to apply the parameter. Use NA to apply to
      all slices.

  ava.lo

  :   numeric. Lower bound of the availability factor.

  ava.up

  :   numeric. Upper bound of the availability factor.

  ava.fx

  :   numeric. Fixed value of the availability factor. This parameter
      overrides `ava.lo` and `ava.up`.

  cost

  :   numeric. Cost of the resource extraction, if not set, the resource
      is considered free.

- `region`:

  character. Regions where the supply process exists. Must include all
  regions used in other slots. `availability` and `reserve` slots also
  limit possible regions.

- `misc`:

  list. List of additional parameters that are not used in the model but
  can be used for reference or user-defined functions. For example,
  links to the source of the supply data, or other metadata.
