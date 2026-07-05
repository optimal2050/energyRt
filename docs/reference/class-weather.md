# S4 class to represent weather factors

`weather` is a data-carrying class with exogenous shocks used to
influence operation of processes in the model.

## Details

Weather factors are separated from the model parameters and can be added
or replaced for different scenarios. !!!Additional details...

## Slots

- `name`:

  character. Name of the weather factor, used in sets.

- `desc`:

  character. Description of the weather factor.

- `unit`:

  character. Unit of the weather factor.

- `region`:

  character. Region where the weather factor is applied.

- `timeframe`:

  character. Timeframe of the weather factor.

- `defVal`:

  numeric. Default value of the weather factor, 0 by default.

- `weather`:

  data.frame. Weather factor values.

  region

  :   character. Region name to apply the parameter, NA for every
      region.

  year

  :   integer. Year to apply the parameter, NA for every year.

  slice

  :   character. Time slice to apply the parameter, NA for every slice.

  wval

  :   numeric. Weather factor value.

- `misc`:

  list. Additional information.
