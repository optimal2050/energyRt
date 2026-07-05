# Build the auxiliary-commodity membership maps (input / output direction)

For each object of `classes`, splits its declared auxiliary commodities
(`aeff$acomm`) into the input-direction map (`inp_name`) and the
output-direction map (`out_name`) according to whether any `*2ainp` /
`*2aout` conversion factor is supplied for that commodity. Mirrors the
direction split in the legacy technology / storage `.obj2modInp` methods
(obj2modInp.R L840-856 / L2084-2096).

## Usage

``` r
.build_aux_membership(scen, classes, key, fmp, inp_name, out_name)
```

## Arguments

- scen:

  scenario object.

- classes:

  object class(es) carrying an `aeff` slot.

- key:

  key column name (e.g. "tech", "stg").

- fmp:

  function mapping a parameter name to its on-disk path.

- inp_name:

  input-direction membership map name.

- out_name:

  output-direction membership map name.

## Value

updated scenario object.
