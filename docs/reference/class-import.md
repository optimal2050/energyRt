# An S4 class to represent commodity import from the rest of the world.

Use `newImport` to create a new `import` object.

## Details

Constructor for import object.

Import object adds an "external" source of commodity to the model. The
RoW is not modeled explicitly as a region, `export` and `import` objects
define and control the exchange with the RoW. The operation is similar
to the `demand` object, but the two ideas distinguishes between internal
and external final consumption. This exchange can be exogenously defined
(`imp.fx`) or optimized by the model within the given limits (`imp.lo`,
`imp.up`). The `price` column is used to define the price of the
imported commodity. "Reserve" sets the total amount that can be imported
over the model horizon.

## Slots

- `name`:

  character. Name of the import object, used in sets.

- `desc`:

  character. Description of the import object.

- `commodity`:

  character. Name of the imported commodity.

- `unit`:

  character. Unit of the imported commodity.

- `reserve`:

  numeric. Total accumulated limit through the model horizon.

- `imp`:

  data.frame. Import parameters.

  region

  :   character. Region name to apply the parameter; use NA to apply to
      all regions.

  year

  :   integer. Year to apply the parameter; use NA to apply to all
      years.

  slice

  :   character. Time slice to apply the parameter; use NA to apply to
      all slices.

  imp.lo

  :   numeric. Lower bound on the import volume.

  imp.up

  :   numeric. Upper bound on the import volume.

  imp.fx

  :   numeric. Fixed import volume, ignored if NA. This parameter
      overrides `imp.lo` and `imp.up`.

- `misc`:

  list. Additional information.
