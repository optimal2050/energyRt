# An S4 class to specify the model set or parameter

Class `parameter` is used to represent the model set or parameter. All
parameters and sets used in the model are populated with data from the
model repository on the interpolation stage and stored as a named list
in `model@modInp@data` slot. The class and related methods and functions
are not intended for direct use by the user.

## Slots

- `name`:

  character. Name of the parameter as it appears in GAMS, Julia/JuMP,
  Python/Pyomo, etc.

- `desc`:

  character. Description of the parameter for reference.

- `type`:

  factor. Type of the parameter, e.g., "set", "map", "numpar", or
  "bounds". "set" is a set of elements, "map" is a mapping between sets,
  "numpar" is a numeric parameter, "bounds" is a parameter with lower
  and upper bounds.

- `dimSets`:

  character. A vector of sets used to define the dimension of the
  parameter.

- `defVal`:

  numeric. Default value of numeric parameters. The default value is
  used to fill the missing values in the model objects. The values are
  used in the interpolation step and/or passed to the solver software.

- `data`:

  data.frame. Data frame with the parameter values.

- `interpolation`:

  character. Interpolation rule for numeric parameters across years.
  Recognized values are any combination of "back", "inter", "forth",
  e.g., "back.inter" or "forth.inter", indicating the direction of
  interpolation.

- `inClass`:

  character. The class of the parameter, e.g., "technology", "storage",
  "trade", "supply", "demand", "export", etc.

- `misc`:

  list. Any additional information or data to store in the object.
