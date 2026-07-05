# An S4 class to declare a demand in the model

An S4 class to declare a demand in the model

## Slots

- `name`:

  character. Name of the demand.

- `desc`:

  character. Optional description of the demand for reference.

- `commodity`:

  character. Name of the commodity for which the demand will be
  specified.

- `unit`:

  character. Optional unit of the commodity.

- `dem`:

  data.frame. Specification of the demand.

  region

  :   character. Name of region for the demand value. NA for every
      region.

  year

  :   integer. Year of the demand. NA for every year.

  slice

  :   character. Name of the slice for the demand value. NA for every
      slice.

  dem

  :   numeric. Value of the demand.

- `region`:

  character. Optional name of region to narrow the specification of the
  demand in the case of used NAs. Error will be returned if specified
  regions in `@dem` are not mensioned in the `@region` slot (if the slot
  is not empty).

- `misc`:

  list. Optional list of additional information.
