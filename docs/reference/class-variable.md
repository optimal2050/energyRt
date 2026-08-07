# An S4 class to specify a model variable

The counterpart of
[class-parameter](https://energyRt.org/reference/class-parameter.md): a
`parameter` carries the model's input data, a `variable` its solved
output. Every variable declared by the model has one of these,
pre-populated from the baked specification when a `modOut` is created
and filled with solved values by
[`read_solution()`](https://energyRt.org/reference/read.md) – exactly as
`modInp@parameters` is populated from `data-raw/modInp.yml`.

The specification itself is composed at build time in
`data-raw/DATASET.R`: dimensions, gating map and description come from
the GAMS source via gams2x, the emitted column names and the
positive/free declaration are parsed out of `glpk/energyRt.mod`, and
`role` / `unit` / `origin` come from the `data-raw/variables.yml`
overlay.

The class and its methods are not intended for direct use; read values
with [`getData()`](https://energyRt.org/reference/getData.md).

## Slots

- `name`:

  character, the variable's name in the model.

- `desc`:

  character, one-line description.

- `dimSets`:

  character, the variable's dimensions **as the model declares them**,
  duplicates preserved (`vTradeIr` is
  `trade, comm, region, region, year, slice`). Byte-identical to
  `.variable_set`, which
  [`newCosts()`](https://energyRt.org/reference/newCosts.md) and
  [`newConstraint()`](https://energyRt.org/reference/newConstraint.md)
  consume positionally against the GAMS declaration.

- `colNames`:

  character, the column names the variable is actually written out with.
  Differs from `dimSets` only where a dimension repeats and the writers
  disambiguate it – `src`/`dst` for `vTradeIr`, `yearp` for
  `vTechRetiredNewCap`.

- `gate`:

  character, the name of the map that gates the variable's domain (the
  `$`-condition), or `""` when it is unconditional.

- `role`:

  character, what the variable is: `source`, `sink`, `flow`, `activity`,
  `balance`, `stock`, `capacity` or `cost`. A `stock` is a level at a
  point in time and is never summed over slices.

- `unit`:

  character, the KIND of quantity (`commodity`, `activity`, `capacity`,
  `currency`), not a unit string – real units are per-object here and
  are resolved against the model.

- `positive`:

  logical, whether the model declares the variable non-negative.

- `origin`:

  character, `solver` or `computed` (built in R by `read.R`, so never
  expected in solver output).

- `data`:

  data.frame, the solved values.

- `misc`:

  list, `path` / `inMemory` / `onDisk` when stored on disk.

## See also

Other variable:
[`get_variable()`](https://energyRt.org/reference/get_variable.md)
